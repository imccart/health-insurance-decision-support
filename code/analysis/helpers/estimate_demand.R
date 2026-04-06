# estimate_demand.R — Pure R nested logit estimator
#
# Replaces Julia estimate_demand_v3.jl. Vectorized per-cell computation
# using matrix operations and rowsum() for grouped sums.
#
# Replicates Julia's logic exactly:
#   - V_0 = beta'X_0 (NOT normalized to 0)
#   - Weights normalized globally to mean 1
#   - LBFGS via optim() with analytical gradient
#   - Cell-by-cell accumulation (never builds pooled matrix)
#
# Usage:
#   source("code/analysis/helpers/estimate_demand.R")
#   result <- estimate_demand(cell_dir, spec_path, out_path,
#                             filter_assisted = 0)

# =========================================================================
# Load one cell CSV into a vectorized structure
# =========================================================================
#
# Returns list with:
#   X          — matrix (n_ins_rows x K): insured rows only
#   X_0        — matrix (n_hh x K): uninsured row per HH
#   X_ch       — matrix (n_hh x K): chosen row per HH
#   V_0_only   — not stored; computed as X_0 %*% beta
#   n_hh       — integer
#   hh_id      — integer vector mapping each insured row to its HH index (1..n_hh)
#   chose_ins  — logical vector (n_hh)
#   wt         — numeric vector (n_hh)

load_one_cell <- function(path, covars, filter_assisted = -1L) {
  needed <- unique(c("household_number", "plan_name", "choice", "ipweight",
                     if (filter_assisted >= 0) "assisted",
                     covars))

  df <- data.table::fread(path, select = needed, data.table = TRUE)

  if (filter_assisted >= 0) {
    df <- df[assisted == filter_assisted]
    if (nrow(df) == 0) return(NULL)
  }

  # Sort by household_number, plan_name
  data.table::setorder(df, household_number, plan_name)

  K <- length(covars)
  n_rows <- nrow(df)

  # Build full X matrix
  X_full <- matrix(0, nrow = n_rows, ncol = K)
  for (k in seq_along(covars)) {
    col_name <- covars[k]
    if (col_name %in% names(df)) {
      vals <- df[[col_name]]
      vals[is.na(vals)] <- 0
      X_full[, k] <- as.numeric(vals)
    }
  }

  plan_nm <- as.character(df$plan_name)
  ch <- as.integer(df$choice)
  hh_num <- df$household_number
  ipw <- as.numeric(df$ipweight)

  is_unins <- plan_nm == "Uninsured"
  is_ins <- !is_unins

  # Identify valid HH: must have insured rows, uninsured row, and a chosen row
  # Use data.table for fast grouped checks
  df[, row_idx := .I]
  df[, is_unins := (plan_name == "Uninsured")]

  hh_summary <- df[, .(
    has_ins   = any(!is_unins),
    has_unins = any(is_unins),
    has_choice = any(choice == 1L),
    unins_idx = row_idx[is_unins][1],
    chosen_idx = row_idx[choice == 1L][1],
    chose_insured = (plan_name[choice == 1L][1] != "Uninsured"),
    weight = ipweight[1]
  ), by = household_number]

  valid_hh <- hh_summary[has_ins == TRUE & has_unins == TRUE & has_choice == TRUE]
  n_hh <- nrow(valid_hh)
  if (n_hh == 0) return(NULL)

  # Map valid HH to sequential indices
  valid_hh[, hh_idx := .I]
  valid_hh_set <- valid_hh$household_number

  # Build insured-row subset with HH index mapping
  ins_mask <- is_ins & (hh_num %in% valid_hh_set)
  X_ins <- X_full[ins_mask, , drop = FALSE]

  # Map each insured row to its HH index
  ins_hh_nums <- hh_num[ins_mask]
  hh_lookup <- valid_hh$hh_idx
  names(hh_lookup) <- as.character(valid_hh$household_number)
  hh_id <- as.integer(hh_lookup[as.character(ins_hh_nums)])

  # X_0: uninsured row for each valid HH
  X_0 <- X_full[valid_hh$unins_idx, , drop = FALSE]

  # X_ch: chosen row for each valid HH
  X_ch <- X_full[valid_hh$chosen_idx, , drop = FALSE]

  list(
    X_ins     = X_ins,             # (n_ins_rows x K)
    X_0       = X_0,               # (n_hh x K)
    X_ch      = X_ch,              # (n_hh x K)
    hh_id     = hh_id,             # integer vector, length = n_ins_rows
    n_hh      = n_hh,
    chose_ins = valid_hh$chose_insured,  # logical (n_hh)
    wt        = valid_hh$weight          # numeric (n_hh)
  )
}


# =========================================================================
# Load all cells from directory
# =========================================================================

load_all_cells <- function(cell_dir, covars, filter_assisted = -1L) {
  csv_files <- sort(list.files(cell_dir, pattern = "^cell_.*_data\\.csv$",
                               full.names = TRUE))
  cells <- vector("list", length(csv_files))
  total_hh <- 0L
  n_loaded <- 0L

  for (i in seq_along(csv_files)) {
    cell <- load_one_cell(csv_files[i], covars, filter_assisted)
    if (!is.null(cell)) {
      n_loaded <- n_loaded + 1L
      cells[[n_loaded]] <- cell
      total_hh <- total_hh + cell$n_hh
    }
    if (i %% 20 == 0) cat("    Loaded", i, "/", length(csv_files), "\n")
  }

  cells <- cells[seq_len(n_loaded)]
  cat("  Loaded", n_loaded, "cells,", total_hh, "HH\n")
  list(cells = cells, total_hh = total_hh)
}


# =========================================================================
# Normalize weights globally to mean 1
# =========================================================================

normalize_weights <- function(cells) {
  total_w <- 0
  total_n <- 0L
  for (ci in seq_along(cells)) {
    total_w <- total_w + sum(cells[[ci]]$wt)
    total_n <- total_n + cells[[ci]]$n_hh
  }
  global_mean <- total_w / total_n

  for (ci in seq_along(cells)) {
    cells[[ci]]$wt <- cells[[ci]]$wt / global_mean
  }
  cat("  Weights normalized: global mean was", round(global_mean, 4), "\n")
  cells
}


# =========================================================================
# NLL + gradient for one cell (VECTORIZED)
# =========================================================================
#
# No per-HH R loop. Uses rowsum() for grouped sums over insured rows.

cell_negll_grad <- function(beta, lambda, cell, compute_grad = TRUE) {
  K <- length(beta)
  n_hh <- cell$n_hh
  hh_id <- cell$hh_id  # maps each insured row to HH index

  # V for insured rows and uninsured rows
  V_ins <- as.numeric(cell$X_ins %*% beta)   # length = n_ins_rows
  V_0   <- as.numeric(cell$X_0 %*% beta)     # length = n_hh
  V_ch  <- as.numeric(cell$X_ch %*% beta)    # length = n_hh

  # --- Logsumexp within each HH's insured nest ---
  V_scaled <- V_ins / lambda

  # Max per HH (for numerical stability)
  max_per_hh <- as.numeric(rowsum(V_scaled, hh_id, reorder = FALSE))
  # rowsum gives SUM; we need MAX. Use a trick: tapply is slow,
  # so compute differently. Actually use a grouped max.
  # For speed: compute in C-style with .Internal... or just use tapply
  max_per_hh <- vapply(split(V_scaled, hh_id), max, numeric(1))

  # exp(V_scaled - max) per insured row
  exp_vs <- exp(V_scaled - max_per_hh[hh_id])

  # D = sum of exp per HH
  D_per_hh <- as.numeric(rowsum(exp_vs, hh_id, reorder = FALSE))

  # Inclusive value I = max + log(D) per HH
  I_val <- max_per_hh + log(D_per_hh)

  # Within-nest shares s_j = exp_vs / D
  s_j <- exp_vs / D_per_hh[hh_id]

  # log denominator per HH: log(exp(lambda*I) + exp(V_0))
  lI <- lambda * I_val
  mx_d <- pmax(lI, V_0)
  log_denom <- mx_d + log(exp(lI - mx_d) + exp(V_0 - mx_d))

  # P(insured) per HH
  P_ins <- exp(lI - log_denom)

  # --- Log-likelihood per HH ---
  # If chose insured: ll = V_ch/lambda + (lambda-1)*I - log_denom
  # If chose uninsured: ll = V_0 - log_denom
  ll_ins <- V_ch / lambda + (lambda - 1) * I_val - log_denom
  ll_unins <- V_0 - log_denom
  ll_h <- ifelse(cell$chose_ins, ll_ins, ll_unins)
  negll <- -sum(cell$wt * ll_h)

  if (!compute_grad) return(list(negll = negll, grad = NULL))

  # --- Gradient (vectorized) ---
  # x_bar per HH: weighted average of X_ins within each HH (weights = s_j)
  # x_bar[h,] = sum_j s_j[j] * X_ins[j,]   for j in HH h
  sj_X <- s_j * cell$X_ins  # (n_ins_rows x K), each row scaled by s_j
  x_bar <- rowsum(sj_X, hh_id, reorder = FALSE)  # (n_hh x K)

  # V_bar per HH: sum_j s_j * V_ins[j]
  V_bar <- as.numeric(rowsum(s_j * V_ins, hh_id, reorder = FALSE))

  # Gradient w.r.t. beta (K-vector, summed over HH)
  # Insured choosers: g_beta = (X_ch - x_bar)/lambda + (1 - P_ins)*(x_bar - X_0)
  # Uninsured choosers: g_beta = -P_ins * (x_bar - X_0)
  diff_xbar_x0 <- x_bar - cell$X_0  # (n_hh x K)

  g_beta_ins <- (cell$X_ch - x_bar) / lambda + (1 - P_ins) * diff_xbar_x0
  g_beta_unins <- -P_ins * diff_xbar_x0

  chose_ins_mat <- cell$chose_ins  # logical (n_hh)
  wt <- cell$wt

  # Combine: g_beta per HH
  g_beta_h <- ifelse(chose_ins_mat, 1, 0) * g_beta_ins +
              ifelse(!chose_ins_mat, 1, 0) * g_beta_unins  # (n_hh x K)

  grad_beta <- -as.numeric(crossprod(wt, g_beta_h))  # (K)

  # Gradient w.r.t. lambda (scalar, summed over HH)
  IV_ratio <- I_val - V_bar / lambda
  g_lam_ins <- -V_ch / lambda^2 + I_val -
    (lambda - 1) * V_bar / lambda^2 - P_ins * IV_ratio
  g_lam_unins <- -P_ins * IV_ratio

  g_lam_h <- ifelse(cell$chose_ins, g_lam_ins, g_lam_unins)
  grad_lambda <- -sum(wt * g_lam_h)

  grad <- c(grad_beta, grad_lambda)

  list(negll = negll, grad = grad)
}


# =========================================================================
# Accumulate NLL + gradient across all cells
# =========================================================================

accumulate <- function(theta, cells, compute_grad = TRUE) {
  K <- length(theta) - 1
  beta <- theta[1:K]
  lambda <- theta[K + 1]

  total_negll <- 0
  total_grad <- if (compute_grad) numeric(K + 1) else NULL

  for (ci in seq_along(cells)) {
    res <- cell_negll_grad(beta, lambda, cells[[ci]], compute_grad)
    total_negll <- total_negll + res$negll
    if (compute_grad) total_grad <- total_grad + res$grad
  }

  list(negll = total_negll, grad = total_grad)
}


# =========================================================================
# BFGS-BHHH optimizer (pure R, no optim())
# =========================================================================
# Port of Julia's bfgs_bhhh(). Uses BHHH initialization (outer product of
# per-HH gradients) and halving line search. See optimizer.md for details.
#
# This is the ONLY optimizer that works for this problem.
# optim() L-BFGS-B, BFGS, and nlminb all fail (see optimizer.md line 9).

# NLL + gradient + per-HH gradient matrix for one cell (for BHHH init)
cell_negll_gradi <- function(beta, lambda, cell) {
  K <- length(beta)
  n_hh <- cell$n_hh
  hh_id <- cell$hh_id

  V_ins <- as.numeric(cell$X_ins %*% beta)
  V_0   <- as.numeric(cell$X_0 %*% beta)
  V_ch  <- as.numeric(cell$X_ch %*% beta)

  V_scaled <- V_ins / lambda
  max_per_hh <- vapply(split(V_scaled, hh_id), max, numeric(1))
  exp_vs <- exp(V_scaled - max_per_hh[hh_id])
  D_per_hh <- as.numeric(rowsum(exp_vs, hh_id, reorder = FALSE))
  I_val <- max_per_hh + log(D_per_hh)
  s_j <- exp_vs / D_per_hh[hh_id]

  sj_X <- s_j * cell$X_ins
  x_bar <- rowsum(sj_X, hh_id, reorder = FALSE)
  V_bar <- as.numeric(rowsum(s_j * V_ins, hh_id, reorder = FALSE))

  lI <- lambda * I_val
  mx_d <- pmax(lI, V_0)
  log_denom <- mx_d + log(exp(lI - mx_d) + exp(V_0 - mx_d))
  P_ins <- exp(lI - log_denom)

  ll_ins <- V_ch / lambda + (lambda - 1) * I_val - log_denom
  ll_unins <- V_0 - log_denom
  ll_h <- ifelse(cell$chose_ins, ll_ins, ll_unins)
  negll <- -sum(cell$wt * ll_h)

  diff_xbar_x0 <- x_bar - cell$X_0
  g_beta_ins <- (cell$X_ch - x_bar) / lambda + (1 - P_ins) * diff_xbar_x0
  g_beta_unins <- -P_ins * diff_xbar_x0
  g_beta_h <- ifelse(cell$chose_ins, 1, 0) * g_beta_ins +
              ifelse(!cell$chose_ins, 1, 0) * g_beta_unins

  IV_ratio <- I_val - V_bar / lambda
  g_lam_ins <- -V_ch / lambda^2 + I_val -
    (lambda - 1) * V_bar / lambda^2 - P_ins * IV_ratio
  g_lam_unins <- -P_ins * IV_ratio
  g_lam_h <- ifelse(cell$chose_ins, g_lam_ins, g_lam_unins)

  # Per-HH gradient matrix (n_hh x K+1), weighted
  gradi <- cbind(g_beta_h, g_lam_h) * (-cell$wt)

  grad <- colSums(gradi)

  list(negll = negll, grad = grad, gradi = gradi)
}

bfgs_bhhh <- function(theta_start, cells, max_iter = 500, ftol = 1e-8,
                      stptol = 1e-10, print_every = 5) {
  K <- length(theta_start) - 1
  theta <- theta_start

  # Initial eval with per-HH gradi for BHHH (accumulate crossprod per cell)
  cat("  BFGS-BHHH: computing initial Hessian...\n"); flush.console()
  negll <- 0; g <- numeric(K + 1)
  bhhh <- matrix(0, K + 1, K + 1)
  for (ci in seq_along(cells)) {
    res <- cell_negll_gradi(theta[1:K], theta[K + 1], cells[[ci]])
    negll <- negll + res$negll
    g <- g + res$grad
    bhhh <- bhhh + crossprod(res$gradi)
    if (ci %% 20 == 0) { cat("    BHHH cell", ci, "/", length(cells), "\n"); flush.console() }
  }

  Hm1 <- tryCatch(solve(bhhh), error = function(e) {
    cat("  BHHH singular, using identity\n")
    diag(K + 1)
  })
  rm(bhhh)

  cat(sprintf("  Init: negLL = %.2f  lambda = %.4f  beta1 = %.6f\n",
              negll, theta[K + 1], theta[1]))
  flush.console()

  for (iter in seq_len(max_iter)) {
    old_negll <- negll
    old_g <- g

    # Search direction
    d <- as.numeric(-Hm1 %*% g)

    # Halving line search
    step <- 2
    negll_try <- Inf
    repeat {
      step <- step / 2
      if (step < stptol) break
      theta_try <- theta + step * d
      if (theta_try[K + 1] <= 0.001 || theta_try[K + 1] >= 5.0) next
      acc <- accumulate(theta_try, cells, compute_grad = FALSE)
      negll_try <- acc$negll
      if (negll_try <= old_negll) break
    }

    if (step < stptol) {
      cat(sprintf("  Iter %3d: step too small\n", iter))
      break
    }

    # Accept step
    theta <- theta + step * d
    acc <- accumulate(theta, cells, compute_grad = TRUE)
    negll <- acc$negll
    g <- acc$grad

    # BFGS Hessian update
    incr <- step * d
    y <- g - old_g
    sy <- sum(incr * y)
    if (abs(sy) > 1e-20) {
      Hy <- as.numeric(Hm1 %*% y)
      yHy <- sum(y * Hy)
      Hm1 <- Hm1 + (sy + yHy) / sy^2 * outer(incr, incr) -
        (outer(Hy, incr) + outer(incr, Hy)) / sy
    }

    chi2 <- -sum(d * old_g)

    if (iter %% print_every == 0 || iter <= 3) {
      cat(sprintf("  Iter %3d: negLL = %.2f  step = %.4f  lambda = %.4f  beta1 = %.6f  chi2 = %.2f\n",
                  iter, negll, step, theta[K + 1], theta[1], chi2))
      flush.console()
    }

    if (abs(negll - old_negll) < ftol) { cat("  Converged (ftol)\n"); break }
    if (abs(chi2) < 1e-6) { cat("  Converged (chi2)\n"); break }
  }

  theta
}


# =========================================================================
# Main estimation function
# =========================================================================

estimate_demand <- function(cell_dir, spec_path, out_path,
                            filter_assisted = -1L, temp_dir = NULL) {

  cat("=== Demand estimation (R) ===\n")
  cat("  BFGS-BHHH optimizer (pure R, no optim)\n")
  cat("  V_0 = beta'X_0 (NOT 0)\n")
  cat("  CELL_DIR =", cell_dir, "\n")
  cat("  SPEC =", spec_path, "\n")
  cat("  OUTPUT =", out_path, "\n")
  cat("  FILTER_ASSISTED =", filter_assisted, "\n")

  # Load spec
  spec_df <- read.csv(spec_path, stringsAsFactors = FALSE)
  covars <- spec_df$term
  K <- length(covars)
  cat("  Covariates:", K, "terms\n")

  # Load cells
  loaded <- load_all_cells(cell_dir, covars, filter_assisted)
  cells <- loaded$cells
  rm(loaded)

  # Normalize weights
  cells <- normalize_weights(cells)

  # Run BFGS-BHHH from zeros + lambda=1
  cat("\n  Starting BFGS-BHHH from zeros + lambda=1...\n")
  flush.console()

  theta_opt <- bfgs_bhhh(c(rep(0, K), 1.0), cells)
  negll_final <- accumulate(theta_opt, cells, compute_grad = FALSE)$negll

  cat(sprintf("\n  Done: negLL = %.2f  lambda = %.4f\n",
              negll_final, theta_opt[K + 1]))

  # Print coefficients
  cat("\n  Coefficients:\n")
  for (k in seq_along(covars)) {
    cat(sprintf("    %-25s = %12.6f\n", covars[k], theta_opt[k]))
  }
  cat(sprintf("    %-25s = %12.6f\n", "lambda", theta_opt[K + 1]))

  # Save
  coefs <- data.frame(term = c(covars, "lambda"),
                      estimate = theta_opt)
  write.csv(coefs, out_path, row.names = FALSE)
  cat("  ->", out_path, "\n")

  invisible(coefs)
}
