# estimate_demand.R — Pure R nested logit estimator
#
# Two-part nested logit with an expected inclusive value over channel states
# in the enrollment margin:
#   - V_0 = beta'X_0 (NOT normalized to 0)
#   - Plan choice within the insured nest uses the full utility at the
#     household's realized channel.
#   - The enrollment (insured vs outside) margin uses Ibar, the expectation of
#     the inclusive value over three channel states weighted by the first-stage
#     channel probabilities (build3's multinomial; weights, not covariates):
#       V^0 = base utility (ext_exclude terms zeroed)
#       V^N = base + b_assisted_av*av + b_assisted_premium*premium
#       V^A = base + b_broker_av*av + b_broker_premium*premium
#                  + b_commission_broker*comm_pmpm
#       I^c = logsumexp(V^c/lambda);  Ibar = p_none I^0 + p_nav I^N + p_agent I^A
#       P(enroll) = exp(lambda Ibar) / (exp(lambda Ibar) + exp(V_0))
#     The channel is a circumstance drawn at predictable rates, not a choice,
#     so Ibar averages the state inclusive values (no logsumexp over states).
#     With p = (1,0,0) this collapses to the exclusion model; with no channel
#     terms in the spec the states coincide and this is the ordinary nested
#     logit (the ext_exclude path still applies when cells carry no
#     probabilities).
#   - Weights normalized globally to mean 1
#   - BFGS-BHHH with analytical gradient
#   - Cell-by-cell accumulation (never builds pooled matrix)
#
# Usage:
#   source("code/analysis/helpers/estimate_demand.R")
#   result <- estimate_demand(cell_dir, spec_path, out_path,
#                             ext_exclude = c("assisted_av", ...))

# Channel-term roles in the state utilities, matched by name against the spec
CH_TERMS <- c(nav_av = "assisted_av",  nav_pr = "assisted_premium",
              brk_av = "broker_av",    brk_pr = "broker_premium",
              comm   = "commission_broker")

# =========================================================================
# Load one cell CSV into a vectorized structure
# =========================================================================
#
# Returns list with:
#   X_ins      — matrix (n_ins_rows x K): insured rows only
#   X_0        — matrix (n_hh x K): uninsured row per HH
#   X_ch       — matrix (n_hh x K): chosen row per HH
#   hh_id      — integer vector mapping each insured row to its HH index
#   chose_ins  — logical (n_hh); wt — numeric (n_hh)
#   av_ins / prem_ins / comm_ins — raw state-utility ingredients (insured rows)
#   p0 / pN / pA — first-stage channel probabilities per HH (NA when the cells
#                  were built without them)

load_one_cell <- function(path, covars, filter_assisted = -1L) {
  raw_cols <- c("av", "premium", "comm_pmpm",
                "p_none_hat", "p_nav_hat", "p_agent_hat")
  needed <- unique(c("household_number", "plan_id", "choice", "hh_weight",
                     if (filter_assisted >= 0) "assisted",
                     covars, raw_cols))

  # Intersect with actual columns to avoid fread crash on missing names;
  # missing covariates are filled with 0 in the X matrix below
  header <- names(data.table::fread(path, nrows = 0L))
  available <- intersect(needed, header)
  df <- data.table::fread(path, select = available, data.table = TRUE)

  if (filter_assisted >= 0) {
    df <- df[assisted == filter_assisted]
    if (nrow(df) == 0) return(NULL)
  }

  data.table::setorder(df, household_number, plan_id)

  K <- length(covars)
  n_rows <- nrow(df)

  X_full <- matrix(0, nrow = n_rows, ncol = K)
  for (k in seq_along(covars)) {
    col_name <- covars[k]
    if (col_name %in% names(df)) {
      vals <- df[[col_name]]
      vals[is.na(vals)] <- 0
      X_full[, k] <- as.numeric(vals)
    }
  }
  raw <- sapply(c("av", "premium", "comm_pmpm"), function(cn) {
    v <- if (cn %in% names(df)) as.numeric(df[[cn]]) else numeric(n_rows)
    v[is.na(v)] <- 0
    v
  })
  p_cols <- c("p_none_hat", "p_nav_hat", "p_agent_hat")
  for (cn in setdiff(p_cols, names(df))) df[, (cn) := NA_real_]

  plan_nm <- as.character(df$plan_id)
  hh_num <- df$household_number

  is_unins <- plan_nm == "Uninsured"
  is_ins <- !is_unins

  df[, row_idx := .I]
  df[, is_unins := (plan_id == "Uninsured")]

  hh_summary <- df[, .(
    has_ins   = any(!is_unins),
    has_unins = any(is_unins),
    has_choice = any(choice == 1L),
    unins_idx = row_idx[is_unins][1],
    chosen_idx = row_idx[choice == 1L][1],
    chose_insured = (plan_id[choice == 1L][1] != "Uninsured"),
    weight = hh_weight[1],
    p_none = p_none_hat[1], p_nav = p_nav_hat[1], p_agent = p_agent_hat[1]
  ), by = household_number]

  valid_hh <- hh_summary[has_ins == TRUE & has_unins == TRUE & has_choice == TRUE]
  n_hh <- nrow(valid_hh)
  if (n_hh == 0) return(NULL)

  valid_hh[, hh_idx := .I]
  valid_hh_set <- valid_hh$household_number

  ins_mask <- is_ins & (hh_num %in% valid_hh_set)
  X_ins <- X_full[ins_mask, , drop = FALSE]

  ins_hh_nums <- hh_num[ins_mask]
  hh_lookup <- valid_hh$hh_idx
  names(hh_lookup) <- as.character(valid_hh$household_number)
  hh_id <- as.integer(hh_lookup[as.character(ins_hh_nums)])

  X_0  <- X_full[valid_hh$unins_idx, , drop = FALSE]
  X_ch <- X_full[valid_hh$chosen_idx, , drop = FALSE]

  list(
    X_ins     = X_ins,
    X_0       = X_0,
    X_ch      = X_ch,
    hh_id     = hh_id,
    n_hh      = n_hh,
    chose_ins = valid_hh$chose_insured,
    wt        = valid_hh$weight,
    av_ins    = raw[ins_mask, "av"],
    prem_ins  = raw[ins_mask, "premium"],
    comm_ins  = raw[ins_mask, "comm_pmpm"],
    p0        = valid_hh$p_none,
    pN        = valid_hh$p_nav,
    pA        = valid_hh$p_agent
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
# Per-cell setup: exclusion indices, channel-term indices, state usage
# =========================================================================
# Shared by estimate_demand and s5_se.R so the likelihood and the sandwich
# always run the same model. use_states = TRUE requires the first-stage
# probabilities on every cell.

prepare_cells <- function(cells, covars, ext_exclude) {
  excl_idx <- match(ext_exclude, covars)
  excl_idx <- excl_idx[!is.na(excl_idx)]
  ch_idx <- match(CH_TERMS, covars)
  names(ch_idx) <- names(CH_TERMS)

  has_channel <- !anyNA(ch_idx)
  has_p <- all(vapply(cells, function(cl)
    !anyNA(cl$p0) && !anyNA(cl$pN) && !anyNA(cl$pA), logical(1)))
  use_states <- has_channel && length(excl_idx) > 0
  if (use_states && !has_p)
    stop("Spec has channel terms but the cells carry no channel probabilities; ",
         "rerun build3_data-prep.R (first stage) and rebuild the cells.")

  for (ci in seq_along(cells)) {
    cells[[ci]]$excl_idx <- excl_idx
    cells[[ci]]$ch_idx <- ch_idx
    cells[[ci]]$use_states <- use_states
  }
  cat("  Enrollment margin:",
      if (use_states) "expected IV over channel states"
      else if (length(excl_idx) > 0) "base IV (excluded terms zeroed)"
      else "ordinary nested logit", "\n")
  cells
}


# =========================================================================
# NLL + gradient for one cell (VECTORIZED)
# =========================================================================
#
# Per household h:
#   P_ins        = exp(lambda Ibar) / (exp(lambda Ibar) + exp(V_0))
#   ll (insured) = V_ch/lambda - I_full + lambda Ibar - log_denom
#   ll (outside) = V_0 - log_denom
# Gradients (x_bar_c = within-nest mean of the STATE design matrix under the
# state-c shares; xbar_bar and Vbar_bar the probability-weighted averages; in
# the single-state path xbar_bar is the base-share mean with excluded columns
# zeroed):
#   beta, insured:  (X_ch - x_bar_f)/lambda + (1 - P_ins)(xbar_bar - X_0)
#   beta, outside:  -P_ins (xbar_bar - X_0)
#   lambda, insured: -V_ch/lambda^2 + V_bar_f/lambda^2
#                    + (1 - P_ins)(Ibar - Vbar_bar/lambda)
#   lambda, outside: -P_ins (Ibar - Vbar_bar/lambda)

hh_max <- function(v, hh_id, n_hh) {
  m <- rep(-Inf, n_hh)
  for (idx in seq_along(v)) {
    hi <- hh_id[idx]
    if (v[idx] > m[hi]) m[hi] <- v[idx]
  }
  m
}

# logsumexp of V/lambda by household; returns inclusive value + shares
nest_iv <- function(V, lambda, hh_id, n_hh) {
  Vs <- V / lambda
  mx <- hh_max(Vs, hh_id, n_hh)
  ev <- exp(Vs - mx[hh_id])
  D  <- as.numeric(rowsum(ev, hh_id, reorder = FALSE))
  list(I = mx + log(D), s = ev / D[hh_id])
}

cell_ll_pieces <- function(beta, lambda, cell) {
  n_hh <- cell$n_hh
  hh_id <- cell$hh_id
  excl <- cell$excl_idx
  ch <- cell$ch_idx

  V_ins <- as.numeric(cell$X_ins %*% beta)
  V_0   <- as.numeric(cell$X_0 %*% beta)
  V_ch  <- as.numeric(cell$X_ch %*% beta)
  V_base <- if (length(excl) > 0)
    V_ins - as.numeric(cell$X_ins[, excl, drop = FALSE] %*% beta[excl]) else V_ins

  # Full-utility nest (realized channel): plan choice within the nest
  full <- nest_iv(V_ins, lambda, hh_id, n_hh)

  if (isTRUE(cell$use_states)) {
    add_N <- beta[ch["nav_av"]] * cell$av_ins + beta[ch["nav_pr"]] * cell$prem_ins
    add_A <- beta[ch["brk_av"]] * cell$av_ins + beta[ch["brk_pr"]] * cell$prem_ins +
             beta[ch["comm"]] * cell$comm_ins
    st0 <- nest_iv(V_base, lambda, hh_id, n_hh)
    stN <- nest_iv(V_base + add_N, lambda, hh_id, n_hh)
    stA <- nest_iv(V_base + add_A, lambda, hh_id, n_hh)
    Ibar <- cell$p0 * st0$I + cell$pN * stN$I + cell$pA * stA$I
    s_0 <- st0$s; s_N <- stN$s; s_A <- stA$s
  } else if (length(excl) > 0) {
    stb <- nest_iv(V_base, lambda, hh_id, n_hh)
    Ibar <- stb$I
    s_0 <- stb$s; s_N <- NULL; s_A <- NULL
    add_N <- NULL; add_A <- NULL
  } else {
    Ibar <- full$I
    s_0 <- full$s; s_N <- NULL; s_A <- NULL
    add_N <- NULL; add_A <- NULL
  }

  lI <- lambda * Ibar
  mx_d <- pmax(lI, V_0)
  log_denom <- mx_d + log(exp(lI - mx_d) + exp(V_0 - mx_d))
  P_ins <- exp(lI - log_denom)

  ll_ins <- V_ch / lambda - full$I + lI - log_denom
  ll_unins <- V_0 - log_denom
  ll_h <- ifelse(cell$chose_ins, ll_ins, ll_unins)

  list(V_ins = V_ins, V_base = V_base, V_ch = V_ch,
       add_N = add_N, add_A = add_A,
       s_f = full$s, s_0 = s_0, s_N = s_N, s_A = s_A,
       I_full = full$I, Ibar = Ibar, P_ins = P_ins, ll_h = ll_h)
}

cell_grad_pieces <- function(beta, lambda, cell, pc) {
  hh_id <- cell$hh_id
  excl <- cell$excl_idx
  ch <- cell$ch_idx
  P_ins <- pc$P_ins

  x_bar_f <- rowsum(pc$s_f * cell$X_ins, hh_id, reorder = FALSE)
  V_bar_f <- as.numeric(rowsum(pc$s_f * pc$V_ins, hh_id, reorder = FALSE))

  if (isTRUE(cell$use_states)) {
    # State design-matrix means: channel columns carry the state's own values
    xb0 <- rowsum(pc$s_0 * cell$X_ins, hh_id, reorder = FALSE)
    xb0[, excl] <- 0
    xbN <- rowsum(pc$s_N * cell$X_ins, hh_id, reorder = FALSE)
    xbN[, excl] <- 0
    xbN[, ch["nav_av"]] <- rowsum(pc$s_N * cell$av_ins, hh_id, reorder = FALSE)
    xbN[, ch["nav_pr"]] <- rowsum(pc$s_N * cell$prem_ins, hh_id, reorder = FALSE)
    xbA <- rowsum(pc$s_A * cell$X_ins, hh_id, reorder = FALSE)
    xbA[, excl] <- 0
    xbA[, ch["brk_av"]] <- rowsum(pc$s_A * cell$av_ins, hh_id, reorder = FALSE)
    xbA[, ch["brk_pr"]] <- rowsum(pc$s_A * cell$prem_ins, hh_id, reorder = FALSE)
    xbA[, ch["comm"]]   <- rowsum(pc$s_A * cell$comm_ins, hh_id, reorder = FALSE)
    xbar_bar <- cell$p0 * xb0 + cell$pN * xbN + cell$pA * xbA

    Vb0 <- as.numeric(rowsum(pc$s_0 * pc$V_base, hh_id, reorder = FALSE))
    VbN <- as.numeric(rowsum(pc$s_N * (pc$V_base + pc$add_N), hh_id, reorder = FALSE))
    VbA <- as.numeric(rowsum(pc$s_A * (pc$V_base + pc$add_A), hh_id, reorder = FALSE))
    Vbar_bar <- cell$p0 * Vb0 + cell$pN * VbN + cell$pA * VbA
  } else if (length(excl) > 0) {
    xbar_bar <- rowsum(pc$s_0 * cell$X_ins, hh_id, reorder = FALSE)
    xbar_bar[, excl] <- 0
    Vbar_bar <- as.numeric(rowsum(pc$s_0 * pc$V_base, hh_id, reorder = FALSE))
  } else {
    xbar_bar <- x_bar_f
    Vbar_bar <- V_bar_f
  }

  diff_xbar_x0 <- xbar_bar - cell$X_0
  g_beta_ins <- (cell$X_ch - x_bar_f) / lambda + (1 - P_ins) * diff_xbar_x0
  g_beta_unins <- -P_ins * diff_xbar_x0
  g_beta_h <- ifelse(cell$chose_ins, 1, 0) * g_beta_ins +
              ifelse(!cell$chose_ins, 1, 0) * g_beta_unins

  IV_ratio <- pc$Ibar - Vbar_bar / lambda
  g_lam_ins <- -pc$V_ch / lambda^2 + V_bar_f / lambda^2 + (1 - P_ins) * IV_ratio
  g_lam_unins <- -P_ins * IV_ratio
  g_lam_h <- ifelse(cell$chose_ins, g_lam_ins, g_lam_unins)

  list(g_beta_h = g_beta_h, g_lam_h = g_lam_h)
}

cell_negll_grad <- function(beta, lambda, cell, compute_grad = TRUE) {
  pc <- cell_ll_pieces(beta, lambda, cell)
  negll <- -sum(cell$wt * pc$ll_h)
  if (!compute_grad) return(list(negll = negll, grad = NULL))

  gp <- cell_grad_pieces(beta, lambda, cell, pc)
  grad_beta <- -as.numeric(crossprod(cell$wt, gp$g_beta_h))
  grad_lambda <- -sum(cell$wt * gp$g_lam_h)
  list(negll = negll, grad = c(grad_beta, grad_lambda))
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
# Uses BHHH initialization (outer product of per-HH gradients) and halving
# line search. See optimizer.md for details.
#
# This is the ONLY optimizer that works for this problem.
# optim() L-BFGS-B, BFGS, and nlminb all fail (see optimizer.md line 9).

# NLL + gradient + per-HH gradient matrix for one cell (for BHHH init)
cell_negll_gradi <- function(beta, lambda, cell) {
  pc <- cell_ll_pieces(beta, lambda, cell)
  negll <- -sum(cell$wt * pc$ll_h)
  gp <- cell_grad_pieces(beta, lambda, cell, pc)

  # Per-HH gradient matrix (n_hh x K+1), weighted
  gradi <- cbind(gp$g_beta_h, gp$g_lam_h) * (-cell$wt)
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

  if (any(!is.finite(bhhh))) {
    cat("  BHHH contains non-finite values, using identity\n")
    Hm1 <- diag(K + 1)
  } else Hm1 <- tryCatch(solve(bhhh), error = function(e) {
    cat("  BHHH singular, using identity\n")
    diag(K + 1)
  })
  rm(bhhh)
  gc(verbose = FALSE)

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

    # BFGS Hessian update — only when curvature condition holds (sy > 0).
    # Negative sy would produce a non-PD Hessian inverse → next iter takes a
    # non-descent direction → β/λ explode.
    incr <- step * d
    y <- g - old_g
    sy <- sum(incr * y)
    if (sy > 1e-10) {
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

    # Prevent memory accumulation from temporaries
    if (iter %% 5 == 0) gc(verbose = FALSE)
  }

  theta
}


# =========================================================================
# Main estimation function
# =========================================================================

estimate_demand <- function(cell_dir, spec_path, out_path,
                            filter_assisted = -1L, temp_dir = NULL,
                            ext_exclude = character()) {

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

  # Normalize weights, set state/exclusion structure
  cells <- normalize_weights(cells)
  cells <- prepare_cells(cells, covars, ext_exclude)

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

  invisible(coefs)
}
