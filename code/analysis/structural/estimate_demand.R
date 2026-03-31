# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-28
## Description:   Nested logit demand estimation in R.
##                Faithful port of Julia estimate_demand_v3.jl.
##                Two-stage: (1) BFGS-BHHH on β with λ=1 fixed,
##                           (2) L-BFGS-B joint optimization.
##                Streams cells from .rds files — one cell in memory at a time.
##                Vectorized via rowsum() and matrix ops.
##
##                Key design:
##                  V_0 = β'X_0 (NOT normalized to 0)
##                  Weights normalized to mean 1 globally

# =========================================================================
# Covariates (shared between Phase 1 cell building and estimation)
# =========================================================================

STRUCTURAL_COVARS <- c(
  "premium", "penalty_own", "premium_sq",
  "silver", "bronze", "hh_size_prem",
  "any_0to17_prem", "FPL_250to400_prem", "FPL_400plus_prem",
  "any_black_prem", "any_hispanic_prem", "hmo", "hsa",
  "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
  "Anthem_silver", "BS_silver", "Kaiser_silver", "HN_silver",
  "Anthem_bronze", "BS_bronze", "Kaiser_bronze", "HN_bronze",
  "assisted_silver", "assisted_bronze", "assisted_gold", "assisted_plat",
  "commission_broker", "v_hat_commission"
)


# =========================================================================
# Convert a choice data.frame into compact cell structure for estimation
# =========================================================================

build_cell_struct <- function(df, covars = STRUCTURAL_COVARS) {

  df <- as.data.table(df)
  setorder(df, household_number, plan_name)

  K <- length(covars)
  n <- nrow(df)

  X <- matrix(0, nrow = n, ncol = K)
  for (k in seq_along(covars)) {
    v <- covars[k]
    if (v %in% names(df)) {
      col <- df[[v]]
      col[is.na(col)] <- 0
      X[, k] <- as.numeric(col)
    }
  }

  hh_ids <- df$household_number
  plan_names <- df$plan_name
  choices <- df$choice
  weights <- df$ipweight

  breaks <- c(0L, which(diff(hh_ids) != 0), n)
  n_groups <- length(breaks) - 1L

  unins_row <- integer(n_groups)
  chosen_row <- integer(n_groups)
  chose_insured <- logical(n_groups)
  wt <- numeric(n_groups)
  valid <- logical(n_groups)
  ins_rows_list <- vector("list", n_groups)

  for (g in seq_len(n_groups)) {
    rows <- (breaks[g] + 1L):breaks[g + 1L]
    is_unins <- plan_names[rows] == "Uninsured"
    is_ins <- !is_unins
    is_chosen <- choices[rows] == 1L

    if (!any(is_ins) || !any(is_unins) || !any(is_chosen)) {
      valid[g] <- FALSE
      next
    }

    ins_rows_list[[g]] <- rows[is_ins]
    unins_row[g] <- rows[is_unins][1]
    chosen_row[g] <- rows[is_chosen][1]
    chose_insured[g] <- plan_names[chosen_row[g]] != "Uninsured"
    wt[g] <- weights[rows[is_ins][1]]
    valid[g] <- TRUE
  }

  keep <- which(valid)
  if (length(keep) == 0) return(NULL)

  n_hh <- length(keep)
  ins_rows_list <- ins_rows_list[keep]
  ins_rows_all <- unlist(ins_rows_list)
  group_id <- rep(seq_len(n_hh), lengths(ins_rows_list))

  list(
    X = X,
    ins_rows = ins_rows_all,
    group_id = group_id,
    unins_row = unins_row[keep],
    chosen_row = chosen_row[keep],
    chose_insured = chose_insured[keep],
    weight = wt[keep],
    n_hh = n_hh
  )
}


# =========================================================================
# NLL + gradient for one cell (fully vectorized)
#   compute_gradi = TRUE returns per-HH gradient matrix (for BHHH init)
# =========================================================================

cell_negll_grad <- function(beta, lambda, cell, compute_gradi = FALSE) {

  X <- cell$X
  K <- length(beta)
  n_hh <- cell$n_hh
  ins_rows <- cell$ins_rows
  gid <- cell$group_id
  w <- cell$weight
  ci <- as.numeric(cell$chose_insured)

  V_all <- as.numeric(X %*% beta)

  # --- Insured nest ---
  V_ins <- V_all[ins_rows]
  V_scaled <- pmin(pmax(V_ins / lambda, -500), 500)

  max_vs_per_hh <- as.numeric(tapply(V_scaled, gid, max))
  max_vs <- max_vs_per_hh[gid]

  exp_vs <- exp(V_scaled - max_vs)
  D_per_hh <- pmax(as.numeric(rowsum(cbind(exp_vs), gid)), 1e-300)
  D <- D_per_hh[gid]

  I_val <- max_vs_per_hh + log(D_per_hh)
  s_jg <- exp_vs / D
  s_jg[!is.finite(s_jg)] <- 0

  X_ins <- X[ins_rows, , drop = FALSE]
  x_bar <- rowsum(X_ins * s_jg, gid)
  V_bar <- as.numeric(rowsum(cbind(s_jg * V_ins), gid))

  # --- Outside option ---
  V_0 <- V_all[cell$unins_row]
  X_0 <- X[cell$unins_row, , drop = FALSE]

  # --- Nest probability ---
  lI <- lambda * I_val
  mx_d <- pmax(lI, V_0)
  log_denom <- mx_d + log(exp(pmin(lI - mx_d, 500)) + exp(pmin(V_0 - mx_d, 500)))
  P_ins <- pmin(pmax(exp(lI - log_denom), 1e-15), 1 - 1e-15)

  # --- Log-likelihood ---
  V_ch <- V_all[cell$chosen_row]
  ll_ins <- V_ch / lambda + (lambda - 1) * I_val - log_denom
  ll_out <- V_0 - log_denom
  ll_h <- ci * ll_ins + (1 - ci) * ll_out
  negll <- -sum(w * ll_h)

  # --- Gradient w.r.t. β (per-HH matrix) ---
  X_ch <- X[cell$chosen_row, , drop = FALSE]
  diff_xbar_x0 <- x_bar - X_0
  g_beta_mat <- (ci / lambda) * (X_ch - x_bar) + (ci - P_ins) * diff_xbar_x0
  grad_beta <- -colSums(w * g_beta_mat)

  # --- Gradient w.r.t. λ ---
  I_minus_Vbar_lam <- I_val - V_bar / lambda
  g_lam_vec <- ci * (-V_ch / lambda^2 + I_val - (lambda - 1) * V_bar / lambda^2) -
    P_ins * I_minus_Vbar_lam
  grad_lambda <- -sum(w * g_lam_vec)

  # Guard against NaN
  if (!is.finite(negll)) negll <- 1e20
  grad <- c(grad_beta, grad_lambda)
  grad[!is.finite(grad)] <- 0

  # Per-HH gradient matrix (for BHHH)
  gradi <- NULL
  if (compute_gradi) {
    gradi <- cbind(-w * g_beta_mat, -w * g_lam_vec)
  }

  list(negll = negll, grad = grad, gradi = gradi)
}


# =========================================================================
# Streaming accumulate: NLL + gradient (one cell at a time)
# =========================================================================

accumulate_streaming <- function(theta, rds_files, weight_scale) {
  K <- length(theta) - 1L
  beta <- theta[1:K]
  lambda <- theta[K + 1L]

  total_negll <- 0
  total_grad <- numeric(K + 1L)

  for (i in seq_along(rds_files)) {
    cell <- readRDS(rds_files[i])
    cell$weight <- cell$weight / weight_scale
    res <- cell_negll_grad(beta, lambda, cell)
    total_negll <- total_negll + res$negll
    total_grad <- total_grad + res$grad
    if (i %% 20 == 0) gc(verbose = FALSE)
  }

  list(negll = total_negll, grad = total_grad)
}


# =========================================================================
# Streaming accumulate with BHHH: NLL + gradient + BHHH matrix
# =========================================================================

accumulate_streaming_bhhh <- function(theta, rds_files, weight_scale) {
  K <- length(theta) - 1L
  beta <- theta[1:K]
  lambda <- theta[K + 1L]

  total_negll <- 0
  total_grad <- numeric(K + 1L)
  bhhh <- matrix(0, K, K)

  for (i in seq_along(rds_files)) {
    cell <- readRDS(rds_files[i])
    cell$weight <- cell$weight / weight_scale
    res <- cell_negll_grad(beta, lambda, cell, compute_gradi = TRUE)
    total_negll <- total_negll + res$negll
    total_grad <- total_grad + res$grad
    gi_beta <- res$gradi[, 1:K, drop = FALSE]
    bhhh <- bhhh + crossprod(gi_beta)
    if (i %% 20 == 0) gc(verbose = FALSE)
  }

  list(negll = total_negll, grad = total_grad, bhhh = bhhh)
}


# =========================================================================
# Stage 1: BFGS-BHHH on β with λ fixed (matches Julia optimize_beta)
# =========================================================================

optimize_beta_streaming <- function(rds_files, weight_scale, K,
                                     beta_init = NULL, lambda_fixed = 1.0,
                                     max_iter = 2000, tol = 1e-6,
                                     stptol = 1e-10) {

  beta <- if (is.null(beta_init)) rep(0, K) else beta_init
  theta <- c(beta, lambda_fixed)

  # Initial evaluation with BHHH
  cat("  Computing BHHH initialization...\n")
  res <- accumulate_streaming_bhhh(theta, rds_files, weight_scale)
  negll <- res$negll
  grad_beta <- res$grad[1:K]

  Hm1 <- tryCatch(solve(res$bhhh), error = function(e) {
    warning("BHHH singular, using scaled identity")
    diag(1e-4, K)
  })

  cat(sprintf("  Init: negLL = %.2f  beta_1 = %.6f\n", negll, beta[1]))

  for (iter in seq_len(max_iter)) {
    old_negll <- negll
    old_grad <- grad_beta

    d <- -as.numeric(Hm1 %*% grad_beta)

    # Halving line search
    step <- 2.0
    repeat {
      step <- step / 2
      if (step < stptol) break
      theta_try <- c(beta + step * d, lambda_fixed)
      res_try <- accumulate_streaming(theta_try, rds_files, weight_scale)
      if (res_try$negll <= old_negll) break
    }

    if (step < stptol) {
      cat(sprintf("    beta iter %3d: step too small\n", iter))
      break
    }

    beta <- beta + step * d
    theta <- c(beta, lambda_fixed)
    res <- accumulate_streaming(theta, rds_files, weight_scale)
    negll <- res$negll
    grad_beta <- res$grad[1:K]

    # BFGS inverse Hessian update
    incr <- step * d
    y <- grad_beta - old_grad
    sy <- sum(incr * y)
    if (abs(sy) > 1e-20) {
      yHy <- as.numeric(crossprod(y, Hm1 %*% y))
      Hm1 <- Hm1 + (sy + yHy) / sy^2 * tcrossprod(incr) -
        (Hm1 %*% tcrossprod(y, incr) + tcrossprod(incr, y) %*% Hm1) / sy
    }

    if (iter %% 10 == 0 || iter <= 3) {
      cat(sprintf("    beta iter %3d: negLL = %.2f  step = %.4f  beta_1 = %.6f\n",
                  iter, negll, step, beta[1]))
    }

    if (abs(negll - old_negll) < tol) {
      cat(sprintf("    beta converged at iter %d\n", iter))
      break
    }
  }

  list(beta = beta, negll = negll)
}


# =========================================================================
# Main estimation (two-stage, matching Julia main())
# =========================================================================

estimate_structural_demand <- function(cell_dir, out_dir = "results",
                                       start = NULL, max_iter = 2000) {

  K <- length(STRUCTURAL_COVARS)

  rds_files <- sort(list.files(cell_dir, pattern = "\\.rds$", full.names = TRUE))
  if (length(rds_files) == 0) stop("No .rds cell files found in ", cell_dir)

  cat("=== Structural demand estimation ===\n")
  cat("  V_0 = beta'X_0 (NOT 0)\n")
  cat("  Two-stage: BFGS-BHHH (beta|lambda=1) then L-BFGS-B (joint)\n")
  cat("  Streaming cells from .rds (one at a time)\n")
  cat("  Covariates:", K, ", Cells:", length(rds_files), "\n\n")

  # Compute global weight mean
  cat("  Computing weight normalization...\n")
  total_w <- 0
  total_hh <- 0L
  for (f in rds_files) {
    cell <- readRDS(f)
    total_w <- total_w + sum(cell$weight)
    total_hh <- total_hh + cell$n_hh
  }
  rm(cell); gc(verbose = FALSE)
  weight_scale <- total_w / total_hh
  cat("  HH:", total_hh, " Weight scale:", round(weight_scale, 4), "\n")

  t0 <- proc.time()

  if (!is.null(start)) {
    # Warm start: skip Stage 1
    theta_start <- start
    cat("\n  Warm start from provided coefficients — skipping Stage 1\n")
  } else {
    # Stage 1: BFGS-BHHH on β with λ=1 fixed
    cat("\n  Stage 1: MNL starting values (beta|lambda=1)...\n")
    stage1 <- optimize_beta_streaming(rds_files, weight_scale, K)
    theta_start <- c(stage1$beta, 1.0)
    cat(sprintf("  Stage 1 done: negLL = %.2f\n", stage1$negll))
    rm(stage1); gc(verbose = FALSE)
  }

  # Stage 2: L-BFGS-B joint optimization
  cat("\n  Stage 2: L-BFGS-B joint optimization...\n")

  cache <- new.env(parent = emptyenv())
  cache$theta <- NULL
  cache$result <- NULL
  cache$n_eval <- 0L

  eval_cached <- function(theta) {
    theta[K + 1L] <- max(min(theta[K + 1L], 5.0), 0.001)
    if (!identical(theta, cache$theta)) {
      cache$theta <- theta
      cache$result <- accumulate_streaming(theta, rds_files, weight_scale)
      cache$n_eval <- cache$n_eval + 1L
      if (cache$n_eval %% 50 == 0) {
        cat(sprintf("  eval %d: negLL = %.2f  lambda = %.4f\n",
                    cache$n_eval, cache$result$negll, theta[K + 1L]))
      }
    }
    cache$result
  }
  fn <- function(theta) eval_cached(theta)$negll
  gr <- function(theta) eval_cached(theta)$grad

  result <- optim(
    par = theta_start,
    fn = fn,
    gr = gr,
    method = "L-BFGS-B",
    lower = c(rep(-Inf, K), 0.001),
    upper = c(rep(Inf, K), 5.0),
    control = list(maxit = max_iter, factr = 1e-10, pgtol = 1e-8,
                   trace = 0)
  )

  elapsed <- (proc.time() - t0)[3]
  cat(sprintf("\n  Done in %.1f min\n", elapsed / 60))
  cat(sprintf("  negLL = %.2f  convergence = %d  lambda = %.4f\n",
              result$value, result$convergence, result$par[K + 1L]))

  # Save coefficients
  coefs <- data.frame(
    term = c(STRUCTURAL_COVARS, "lambda"),
    estimate = result$par
  )
  out_path <- file.path(out_dir, "choice_coefficients_structural.csv")
  write.csv(coefs, out_path, row.names = FALSE)

  cat("\n  Coefficients:\n")
  for (i in seq_len(nrow(coefs))) {
    cat(sprintf("    %-25s = %12.6f\n", coefs$term[i], coefs$estimate[i]))
  }

  beta_p <- coefs$estimate[coefs$term == "premium"]
  beta_c <- coefs$estimate[coefs$term == "commission_broker"]
  if (abs(beta_p) > 1e-10) {
    cat(sprintf("\n  beta_commission / |beta_premium| = %.4f\n", beta_c / abs(beta_p)))
  }

  cat("  ->", out_path, "\n")
  coefs
}
