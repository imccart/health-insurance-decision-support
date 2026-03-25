# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Date Edited:   2026-03-24
## Description:   Structural demand estimation (pure R).
##                Phase 1: build cell CSVs from parquet partitions.
##                Phase 2: BFGS-BHHH nested logit optimizer.
##                See docs/optimizer.md for algorithm details and
##                docs/estimate_demand_v3_reference.jl for the original Julia version.

# Dependencies (arrow for read_parquet, not in 0-setup.R) -----------------
library(arrow)

# Tuning parameters -------------------------------------------------------

SAMPLE_FRAC   <- 0.20
CELL_DIR      <- "data/output/choice_cells"
PARTITION_DIR <- "data/output/hh_choice_partitions"

COVARIATES <- c(
  "premium", "silver", "bronze", "hh_size_prem",
  "any_0to17_prem", "FPL_250to400_prem", "FPL_400plus_prem",
  "any_black_prem", "any_hispanic_prem", "hmo", "hsa",
  "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
  "commission_broker"
)

# =========================================================================
# PHASE 1: Build cell CSVs (R)
# =========================================================================

plan_choice <- read_csv("data/output/plan_choice.csv", show_col_types = FALSE)

partition_files <- list.files(PARTITION_DIR, pattern = "^hh_\\d+_\\d+\\.parquet$",
                              full.names = FALSE)
cells <- tibble(file = partition_files) %>%
  mutate(
    region = as.integer(str_extract(file, "(?<=hh_)\\d+")),
    year   = as.integer(str_extract(file, "(?<=_)\\d{4}"))
  ) %>%
  arrange(region, year)

cat("Region-year cells:", nrow(cells), "\n")

set.seed(20260224)
cell_seeds <- sample.int(1e7, nrow(cells))

if (!dir.exists(CELL_DIR)) dir.create(CELL_DIR, recursive = TRUE)

cat("\nPhase 1: Building cell CSVs...\n")
n_built <- 0L
n_skip  <- 0L

for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]

  set.seed(cell_seeds[i])
  hhs <- tryCatch(
    read_parquet(file.path(PARTITION_DIR, paste0("hh_", r, "_", y, ".parquet"))),
    error = function(e) NULL
  )
  if (is.null(hhs) || nrow(hhs) == 0) { n_skip <- n_skip + 1L; next }

  plans <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans) == 0) { n_skip <- n_skip + 1L; next }

  cd <- build_choice_data(plans, hhs, SAMPLE_FRAC)
  rm(hhs, plans)

  if (!is.null(cd)) {
    cd$region <- r
    cd$year <- y
    write_csv(cd, file.path(CELL_DIR, paste0("cell_", r, "_", y, "_data.csv")))
    n_built <- n_built + 1L
  } else {
    n_skip <- n_skip + 1L
  }
  rm(cd)

  if (i %% 20 == 0) {
    gc(verbose = FALSE)
    cat("  Cell", i, "of", nrow(cells), "\n")
  }
}

rm(plan_choice)
gc(verbose = FALSE)
cat("  Built:", n_built, "  Skipped:", n_skip, "\n")


# =========================================================================
# PHASE 2: BFGS-BHHH nested logit estimation (R)
# =========================================================================

cat("\nPhase 2: R demand estimation...\n")
cat("  V_0 = beta'X_0 (NOT 0)\n")
cat("  Weights normalized to mean 1 globally\n")
cat("  BFGS with BHHH initialization\n")
cat("  Pooled model: all HH, commission_broker = comm_pmpm * assisted\n")

# -------------------------------------------------------------------------
# Load cell CSVs into compact list structure
# -------------------------------------------------------------------------

load_cells <- function(cell_dir, covars) {
  csv_files <- sort(list.files(cell_dir, pattern = "^cell_.*_data\\.csv$",
                               full.names = TRUE))
  cells <- list()
  total_hh <- 0L

  for (idx in seq_along(csv_files)) {
    df <- read_csv(csv_files[idx], show_col_types = FALSE)
    df <- df %>% arrange(household_number, plan_name)
    n <- nrow(df)
    K <- length(covars)

    # Build X matrix (all rows including uninsured)
    X <- matrix(0, nrow = n, ncol = K)
    for (k in seq_along(covars)) {
      if (covars[k] %in% names(df)) {
        vals <- df[[covars[k]]]
        vals[is.na(vals)] <- 0
        X[, k] <- vals
      }
    }

    # Build HH group indices
    choice <- df$choice
    plan_name <- df$plan_name
    hh_num <- df$household_number
    ipw <- df$ipweight

    groups <- list()
    i <- 1L
    while (i <= n) {
      hh <- hh_num[i]
      j <- i
      while (j <= n && hh_num[j] == hh) j <- j + 1L

      ins_rows <- integer(0)
      unins_row <- NA_integer_
      chosen_row <- NA_integer_

      for (r in i:(j - 1L)) {
        if (plan_name[r] == "Uninsured") {
          unins_row <- r
        } else {
          ins_rows <- c(ins_rows, r)
        }
        if (choice[r] == 1) chosen_row <- r
      }

      if (length(ins_rows) > 0 && !is.na(unins_row) && !is.na(chosen_row)) {
        groups[[length(groups) + 1L]] <- list(
          ins_rows = ins_rows,
          unins_row = unins_row,
          chosen_row = chosen_row,
          chose_insured = (chosen_row != unins_row),
          weight = ipw[i]
        )
      }
      i <- j
    }

    if (length(groups) > 0) {
      cells[[length(cells) + 1L]] <- list(X = X, groups = groups, n_hh = length(groups))
      total_hh <- total_hh + length(groups)
    }

    if (idx %% 20 == 0) cat("    Loaded", idx, "/", length(csv_files), "\n")
  }

  cat("  Loaded", length(cells), "cells,", total_hh, "HH\n")
  list(cells = cells, total_hh = total_hh)
}

# -------------------------------------------------------------------------
# Normalize weights globally to mean 1 (matching mlogit)
# -------------------------------------------------------------------------

normalize_weights <- function(cells) {
  total_w <- 0
  total_n <- 0L
  for (cell in cells) {
    for (g in cell$groups) {
      total_w <- total_w + g$weight
      total_n <- total_n + 1L
    }
  }
  global_mean <- total_w / total_n

  for (ci in seq_along(cells)) {
    for (gi in seq_along(cells[[ci]]$groups)) {
      cells[[ci]]$groups[[gi]]$weight <- cells[[ci]]$groups[[gi]]$weight / global_mean
    }
  }
  cat("  Weights normalized: global mean was", round(global_mean, 4), "\n")
  cells
}

# -------------------------------------------------------------------------
# NLL + gradient for one cell (V_0 = beta'X_0, NOT 0)
# Returns list(negll, grad, gradi)
# -------------------------------------------------------------------------

cell_negll_grad <- function(beta, lambda, cell, compute_gradi = FALSE) {
  X <- cell$X
  K <- length(beta)
  n_hh <- cell$n_hh

  # V = X %*% beta for ALL rows
  V <- as.numeric(X %*% beta)

  negll <- 0
  grad <- numeric(K + 1L)
  gradi <- if (compute_gradi) matrix(0, nrow = n_hh, ncol = K + 1L) else NULL

  for (h in seq_along(cell$groups)) {
    g <- cell$groups[[h]]
    ins_rows <- g$ins_rows
    w <- g$weight

    # Insured nest: logsumexp of V_ins/lambda
    v_scaled <- V[ins_rows] / lambda
    max_vs <- max(v_scaled)
    D <- sum(exp(v_scaled - max_vs))
    I_val <- max_vs + log(D)  # inclusive value

    # Within-nest shares and x_bar, V_bar
    s_j <- exp(v_scaled - max_vs) / D
    V_bar <- sum(s_j * V[ins_rows])
    x_bar <- as.numeric(crossprod(s_j, X[ins_rows, , drop = FALSE]))

    # V_0 = beta'X_0 (NOT zero!)
    V_0 <- V[g$unins_row]
    X_0 <- X[g$unins_row, ]

    # log denominator: log(exp(lambda*I) + exp(V_0))
    lI <- lambda * I_val
    mx_d <- max(lI, V_0)
    log_denom <- mx_d + log(exp(lI - mx_d) + exp(V_0 - mx_d))

    # P(insured)
    P_ins <- exp(lI - log_denom)

    # Log-likelihood
    if (g$chose_insured) {
      V_ch <- V[g$chosen_row]
      ll_h <- V_ch / lambda + (lambda - 1) * I_val - log_denom
    } else {
      ll_h <- V_0 - log_denom
    }
    negll <- negll - w * ll_h

    # Gradient w.r.t. beta
    if (g$chose_insured) {
      X_ch <- X[g$chosen_row, ]
      g_beta <- (X_ch - x_bar) / lambda + (1 - P_ins) * (x_bar - X_0)
      # Gradient w.r.t. lambda
      g_lam <- -V_ch / lambda^2 + I_val - (lambda - 1) * V_bar / lambda^2 -
        P_ins * (I_val - V_bar / lambda)
    } else {
      g_beta <- -P_ins * (x_bar - X_0)
      g_lam <- -P_ins * (I_val - V_bar / lambda)
    }

    grad[1:K] <- grad[1:K] - w * g_beta
    grad[K + 1L] <- grad[K + 1L] - w * g_lam

    if (compute_gradi) {
      gradi[h, 1:K] <- -w * g_beta
      gradi[h, K + 1L] <- -w * g_lam
    }
  }

  list(negll = negll, grad = grad, gradi = gradi)
}

# -------------------------------------------------------------------------
# Accumulate NLL + gradient across cells
# -------------------------------------------------------------------------

accumulate <- function(theta, cells, compute_gradi = FALSE) {
  K <- length(theta) - 1L
  beta <- theta[1:K]
  lambda <- theta[K + 1L]

  total_negll <- 0
  total_grad <- numeric(K + 1L)
  all_gradi <- if (compute_gradi) list() else NULL

  for (cell in cells) {
    res <- cell_negll_grad(beta, lambda, cell, compute_gradi = compute_gradi)
    total_negll <- total_negll + res$negll
    total_grad <- total_grad + res$grad
    if (compute_gradi) all_gradi[[length(all_gradi) + 1L]] <- res$gradi
  }

  list(negll = total_negll, grad = total_grad, all_gradi = all_gradi)
}

# -------------------------------------------------------------------------
# Optimize beta with lambda fixed (BFGS-BHHH on beta only)
# -------------------------------------------------------------------------

optimize_beta <- function(cells, K, beta_init, lambda_fixed,
                          max_iter = 2000L, tol = 1e-6, stptol = 1e-10,
                          verbose = TRUE) {
  beta <- beta_init

  # Initial evaluation with gradi for BHHH
  theta <- c(beta, lambda_fixed)
  res <- accumulate(theta, cells, compute_gradi = TRUE)
  negll <- res$negll
  grad_beta <- res$grad[1:K]

  # BHHH on beta dimensions only
  bhhh <- matrix(0, K, K)
  for (gradi in res$all_gradi) {
    gi_beta <- gradi[, 1:K, drop = FALSE]
    bhhh <- bhhh + crossprod(gi_beta)
  }
  rm(res)
  gc(verbose = FALSE)

  Hm1 <- tryCatch(solve(bhhh), error = function(e) {
    warning("BHHH singular, using scaled identity")
    diag(K) * 1e-4
  })

  for (iter in seq_len(max_iter)) {
    old_negll <- negll
    old_grad <- grad_beta

    d <- -as.numeric(Hm1 %*% grad_beta)

    # Halving line search
    step <- 2
    while (TRUE) {
      step <- step / 2
      if (step < stptol) break
      theta_try <- c(beta + step * d, lambda_fixed)
      res_try <- accumulate(theta_try, cells, compute_gradi = FALSE)
      if (res_try$negll <= old_negll) break
    }

    if (step < stptol) {
      if (verbose) cat(sprintf("    beta iter %3d: step too small\n", iter))
      break
    }

    # Accept step
    beta <- beta + step * d
    theta <- c(beta, lambda_fixed)
    res <- accumulate(theta, cells, compute_gradi = FALSE)
    negll <- res$negll
    grad_beta <- res$grad[1:K]

    # BFGS Hessian update
    incr <- step * d
    y <- grad_beta - old_grad
    sy <- sum(incr * y)
    if (abs(sy) > 1e-20) {
      Hy <- as.numeric(Hm1 %*% y)
      yHy <- sum(y * Hy)
      Hm1 <- Hm1 + outer(incr, incr) * (sy + yHy) / sy^2 -
        (outer(Hy, incr) + outer(incr, Hy)) / sy
    }

    if (verbose && (iter %% 10 == 0 || iter <= 3)) {
      cat(sprintf("    beta iter %3d: negLL = %.2f  step = %.4f  beta1 = %.6f\n",
                  iter, negll, step, beta[1]))
    }

    if (abs(negll - old_negll) < tol) {
      if (verbose) cat(sprintf("    beta converged at iter %d\n", iter))
      break
    }
  }

  list(beta = beta, negll = negll)
}

# -------------------------------------------------------------------------
# Optimize lambda with beta fixed (golden section search)
# -------------------------------------------------------------------------

optimize_lambda <- function(cells, beta, lambda_lo = 0.01, lambda_hi = 1.0,
                            tol = 1e-4) {
  phi <- (sqrt(5) - 1) / 2

  a <- lambda_lo; b <- lambda_hi
  cc <- b - phi * (b - a)
  dd <- a + phi * (b - a)

  fc <- accumulate(c(beta, cc), cells, compute_gradi = FALSE)$negll
  fd <- accumulate(c(beta, dd), cells, compute_gradi = FALSE)$negll

  while ((b - a) > tol) {
    if (fc < fd) {
      b <- dd; dd <- cc; fd <- fc
      cc <- b - phi * (b - a)
      fc <- accumulate(c(beta, cc), cells, compute_gradi = FALSE)$negll
    } else {
      a <- cc; cc <- dd; fc <- fd
      dd <- a + phi * (b - a)
      fd <- accumulate(c(beta, dd), cells, compute_gradi = FALSE)$negll
    }
  }

  lambda_opt <- (a + b) / 2
  negll_opt <- accumulate(c(beta, lambda_opt), cells, compute_gradi = FALSE)$negll
  list(lambda = lambda_opt, negll = negll_opt)
}

# -------------------------------------------------------------------------
# Joint BFGS-BHHH (all parameters including lambda)
# -------------------------------------------------------------------------

bfgs_bhhh <- function(start, cells, max_iter = 500L, tol = 1e-6,
                      ftol = 1e-8, stptol = 1e-10, print_every = 5L) {
  K <- length(start) - 1L
  theta <- start

  # Initial evaluation with per-HH gradi for BHHH
  res <- accumulate(theta, cells, compute_gradi = TRUE)
  negll <- res$negll
  g <- res$grad

  # BHHH: Hm1 = inv(sum gradi'gradi)
  bhhh <- matrix(0, K + 1L, K + 1L)
  for (gradi in res$all_gradi) {
    bhhh <- bhhh + crossprod(gradi)
  }
  rm(res)
  gc(verbose = FALSE)

  Hm1 <- tryCatch(solve(bhhh), error = function(e) {
    warning("BHHH singular, using identity")
    diag(K + 1L)
  })

  cat(sprintf("  Init: negLL = %.2f  lambda = %.4f  beta1 = %.6f\n",
              negll, theta[K + 1L], theta[1]))

  for (iter in seq_len(max_iter)) {
    old_negll <- negll
    old_g <- g

    # Search direction (descent for minimization)
    d <- -as.numeric(Hm1 %*% g)

    # Halving line search
    step <- 2
    while (TRUE) {
      step <- step / 2
      if (step < stptol) break
      theta_try <- theta + step * d
      if (theta_try[K + 1L] <= 0.001 || theta_try[K + 1L] >= 5.0) next
      res_try <- accumulate(theta_try, cells, compute_gradi = FALSE)
      if (res_try$negll <= old_negll) break
    }

    if (step < stptol) {
      cat(sprintf("  Iter %3d: step too small\n", iter))
      break
    }

    # Accept step
    theta <- theta + step * d
    res <- accumulate(theta, cells, compute_gradi = FALSE)
    negll <- res$negll
    g <- res$grad

    # BFGS Hessian update
    incr <- step * d
    y <- g - old_g
    sy <- sum(incr * y)
    if (abs(sy) > 1e-20) {
      Hy <- as.numeric(Hm1 %*% y)
      yHy <- sum(y * Hy)
      Hm1 <- Hm1 + outer(incr, incr) * (sy + yHy) / sy^2 -
        (outer(Hy, incr) + outer(incr, Hy)) / sy
    }

    chi2 <- -sum(d * old_g)

    if (iter %% print_every == 0 || iter <= 3) {
      cat(sprintf("  Iter %3d: negLL = %.2f  step = %.4f  lambda = %.4f  beta1 = %.6f  chi2 = %.2f\n",
                  iter, negll, step, theta[K + 1L], theta[1], chi2))
    }

    if (abs(negll - old_negll) < ftol) { cat("  Converged (ftol)\n"); break }
    if (abs(chi2) < tol) { cat("  Converged (chi2)\n"); break }
  }

  list(theta = theta, negll = negll)
}

# -------------------------------------------------------------------------
# Run estimation
# -------------------------------------------------------------------------

K <- length(COVARIATES)

cell_data <- load_cells(CELL_DIR, COVARIATES)
cell_list <- normalize_weights(cell_data$cells)
rm(cell_data)
gc(verbose = FALSE)

# Step 1: MNL starting values (beta from zeros, lambda=1 fixed)
cat("\n  MNL starting values (lambda=1 fixed)...\n")
mnl_res <- optimize_beta(cell_list, K, rep(0, K), 1.0, verbose = TRUE)

# Step 2: Coordinate descent — alternate beta|lambda and lambda|beta
cat("\n  Coordinate descent: beta|lambda then lambda|beta\n")
beta <- mnl_res$beta
lambda <- 1.0
negll <- mnl_res$negll

for (outer in 1:20) {
  old_negll <- negll

  # Optimize lambda with beta fixed
  lam_res <- optimize_lambda(cell_list, beta)
  lambda <- lam_res$lambda
  cat(sprintf("    Outer %d: lambda search -> lambda = %.4f  negLL = %.2f\n",
              outer, lambda, lam_res$negll))

  # Optimize beta with lambda fixed
  beta_res <- optimize_beta(cell_list, K, beta, lambda, verbose = FALSE)
  beta <- beta_res$beta
  negll <- beta_res$negll
  cat(sprintf("    Outer %d: beta optim  -> negLL = %.2f  beta1 = %.6f\n",
              outer, negll, beta[1]))

  if (abs(negll - old_negll) < 1.0) {
    cat(sprintf("  Coordinate descent converged at outer iter %d\n", outer))
    break
  }
}

# Step 3: Joint BFGS-BHHH from coordinate descent solution
cat("\n  Joint BFGS-BHHH refinement...\n")
theta_start <- c(beta, lambda)
joint_res <- bfgs_bhhh(theta_start, cell_list)
theta_opt <- joint_res$theta
negll_opt <- joint_res$negll

beta_opt <- theta_opt[1:K]
lambda_opt <- theta_opt[K + 1L]

cat(sprintf("\n  Final: negLL = %.2f  lambda = %.4f\n", negll_opt, lambda_opt))

# Clean up cell data
rm(cell_list)
gc(verbose = FALSE)


# =========================================================================
# PHASE 3: Save and report results
# =========================================================================

cat("\nPhase 3: Saving coefficient estimates...\n")

coefs_structural <- tibble(
  term = c(COVARIATES, "lambda"),
  estimate = theta_opt
)

write_csv(coefs_structural, "data/output/choice_coefficients_structural.csv")

cat("  Pooled model:", nrow(coefs_structural), "terms\n")
for (i in seq_len(nrow(coefs_structural))) {
  cat(sprintf("    %-25s = %12.6f\n", coefs_structural$term[i], coefs_structural$estimate[i]))
}

# Headline: commission-premium ratio
beta_p <- theta_opt[which(COVARIATES == "premium")]
beta_c <- theta_opt[which(COVARIATES == "commission_broker")]
if (abs(beta_p) > 1e-10) {
  cat(sprintf("\n  beta_commission / |beta_premium| = %.4f\n", beta_c / abs(beta_p)))
  cat(sprintf("  Interpretation: $1 commission ~ $%.2f premium equivalent for assisted HH\n",
              beta_c / abs(beta_p)))
}

cat("  -> data/output/choice_coefficients_structural.csv\n")
cat("\nStructural demand estimation complete.\n")
