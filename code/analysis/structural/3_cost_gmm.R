# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-31
## Description:   GMM estimation of cost-side parameters (risk score regression
##                and claims regression) with demand held fixed. Three moment
##                conditions:
##                  M1: Risk score moments (rate filing data)
##                  M2: Claims moments (rate filing data)
##                  M3: FOC moments (evaluated directly, not inverted)
##                Two-step feasible GMM. Produces cost parameters consistent
##                with both rate filing data and the pricing FOC.

# Setup (idempotent — safe to re-source) -----------------------------------
source("code/0-setup.R")
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/supply.R")
source("code/analysis/helpers/ra.R")

# =========================================================================
# LOAD DATA
# =========================================================================

cat("Loading data for cost-side GMM...\n")

# --- Rate filing data (moments 1-2) ---
rsdata <- read_csv("data/output/rate_filing_rsdata.csv", show_col_types = FALSE)
plan_demo <- read_csv(file.path(Sys.getenv("TEMP_DIR"), "plan_demographics.csv"), show_col_types = FALSE)
rsdata <- rsdata %>%
  left_join(plan_demo, by = c("plan_name", "year"))

rsdata <- rsdata %>%
  filter(!is.na(log_risk_score), is.finite(log_risk_score),
         !is.na(log_cost), is.finite(log_cost),
         EXP_MM > 0,
         !is.na(share_18to34), !is.na(share_35to54), !is.na(share_hispanic))

cat("  Rate filing observations:", nrow(rsdata), "\n")

# --- FOC inputs per cell (moment 3) ---
foc_files <- list.files(file.path(Sys.getenv("TEMP_DIR"), "foc_inputs"),
                         pattern = "^foc_.*\\.rds$", full.names = TRUE)
if (length(foc_files) == 0) stop("No FOC input files found — run 2_pricing.R first")

foc_cells <- lapply(foc_files, readRDS)
cat("  FOC cells loaded:", length(foc_cells), "\n")

# Filter to cells with valid Omega (non-NA markup)
foc_cells <- Filter(function(fc) {
  !any(is.na(fc$Omega)) && !any(is.na(fc$shares)) && length(fc$plan_names) >= 2
}, foc_cells)
cat("  FOC cells with valid Omega:", length(foc_cells), "\n")

# --- Supply results (for plan characteristics) ---
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE)

# --- Demographics for FOC risk score prediction ---
plan_demo_yr <- read_csv(file.path(Sys.getenv("TEMP_DIR"), "plan_demographics.csv"), show_col_types = FALSE)

rm(plan_demo)

# =========================================================================
# PREPARE MOMENT DATA
# =========================================================================

cat("Preparing moment data...\n")

# --- M1-M2 data matrices (rate filings) ---
w_rf <- sqrt(rsdata$EXP_MM)  # WLS weights

# M1 regressors: risk score equation
X_rs <- as.matrix(rsdata %>% select(Silver, Gold, Platinum,
                                      share_18to34, share_35to54, share_hispanic))
y_rs <- rsdata$log_risk_score

# M2 regressors: claims equation (exogenous only; log_rs is endogenous)
X_cl_exog <- as.matrix(rsdata %>% select(HMO, trend, Anthem, Blue_Shield, Health_Net))
y_cl <- rsdata$log_cost

# M1 instruments: intercept + regressors
Z_rs <- cbind(1, X_rs)  # 7 columns

# M2 instruments: intercept + exogenous regressors (exclude endogenous log_rs)
Z_cl <- cbind(1, X_cl_exog)  # 6 columns

# --- M3: Precompute FOC cell data ---
# For each cell, we need plan characteristics to predict MC(alpha, gamma).
# Build once, store as list.

MH_LOOKUP <- c("0.6" = 1.00, "0.7" = 1.03, "0.8" = 1.08, "0.9" = 1.15)

for (k in seq_along(foc_cells)) {
  fc <- foc_cells[[k]]
  pn <- fc$plan_names
  r <- fc$region; y <- fc$year

  # Plan characteristics from supply results (metal is now correct for all plans)
  sr_cell <- supply_results %>%
    filter(region == r, year == y, plan_name %in% pn)
  plan_metal <- setNames(sr_cell$metal[match(pn, sr_cell$plan_name)], pn)

  foc_cells[[k]]$Silver <- as.integer(plan_metal == "Silver")
  foc_cells[[k]]$Gold <- as.integer(plan_metal == "Gold")
  foc_cells[[k]]$Platinum <- as.integer(plan_metal == "Platinum")
  foc_cells[[k]]$HMO <- as.integer(grepl("^KA", pn))
  foc_cells[[k]]$trend <- y - 2014L
  foc_cells[[k]]$Anthem <- as.integer(grepl("^ANT", pn))
  foc_cells[[k]]$Blue_Shield <- as.integer(grepl("^BS", pn))
  foc_cells[[k]]$Health_Net <- as.integer(grepl("^HN", pn))

  # Demographics (plan-year level)
  demo <- plan_demo_yr %>% filter(year == y)
  foc_cells[[k]]$share_18to34 <- sapply(pn, function(p) {
    v <- demo$share_18to34[demo$plan_name == p]
    if (length(v) == 0) return(mean(demo$share_18to34, na.rm = TRUE))
    v[1]
  })
  foc_cells[[k]]$share_35to54 <- sapply(pn, function(p) {
    v <- demo$share_35to54[demo$plan_name == p]
    if (length(v) == 0) return(mean(demo$share_35to54, na.rm = TRUE))
    v[1]
  })
  foc_cells[[k]]$share_hispanic <- sapply(pn, function(p) {
    v <- demo$share_hispanic[demo$plan_name == p]
    if (length(v) == 0) return(mean(demo$share_hispanic, na.rm = TRUE))
    v[1]
  })
}

rm(plan_demo_yr, supply_results)

# Count total FOC equations (one per plan per cell)
n_foc_total <- sum(sapply(foc_cells, function(fc) length(fc$plan_names)))
cat("  Total FOC equations:", n_foc_total, "\n")

# M3 instruments: plan characteristics (same for each plan within the FOC)
# We'll compute Z_foc * eps_foc inside compute_g_bar by accumulating across cells
N_Z_FOC <- 9L  # intercept, Silver, Gold, Platinum, HMO, trend, Anthem, BS, HN

N_MOMENTS <- ncol(Z_rs) + ncol(Z_cl) + N_Z_FOC
cat("  Total moment conditions:", N_MOMENTS, "(M1:", ncol(Z_rs),
    " M2:", ncol(Z_cl), " M3:", N_Z_FOC, ")\n")

# =========================================================================
# PARAMETER LAYOUT
# =========================================================================

N_ALPHA <- 7L  # intercept, Silver, Gold, Platinum, share_18to34, share_35to54, share_hispanic
N_GAMMA <- 7L  # intercept, log_risk_score, HMO, trend, Anthem, Blue_Shield, Health_Net

alpha_names <- c("(Intercept)", "Silver", "Gold", "Platinum",
                 "share_18to34", "share_35to54", "share_hispanic")
gamma_names <- c("(Intercept)", "log_risk_score", "HMO", "trend",
                 "Anthem", "Blue_Shield", "Health_Net")

# Starting values from OLS
rs_coefs_start <- read_csv(file.path(Sys.getenv("TEMP_DIR"), "ra_rs_coefs.csv"), show_col_types = FALSE)
cl_coefs_start <- read_csv(file.path(Sys.getenv("TEMP_DIR"), "ra_claims_coefs.csv"), show_col_types = FALSE)

alpha0 <- unname(setNames(rs_coefs_start$estimate, rs_coefs_start$term)[alpha_names])
gamma0_raw <- setNames(cl_coefs_start$estimate, cl_coefs_start$term)[gamma_names]
gamma0_raw[is.na(gamma0_raw)] <- 0
gamma0 <- unname(gamma0_raw)

theta0 <- c(alpha0, gamma0)
cat("  Starting values (OLS):\n")
cat("    alpha:", round(alpha0, 4), "\n")
cat("    gamma:", round(gamma0, 4), "\n")

# =========================================================================
# MOMENT FUNCTION
# =========================================================================

# Computes g_bar(theta): the N_MOMENTS-vector of averaged moment conditions.
#
# M1: E[Z_rs' * (log_rs_obs - alpha'X)] = 0   (risk scores)
# M2: E[Z_cl' * (log_cl_obs - gamma'W)] = 0   (claims)
# M3: E[Z_foc' * foc_residual] = 0             (pricing FOC, evaluated directly)
#
# FOC residual for plan j in cell c:
#   foc_j = s_j + sum_k Omega_{jk} * (p_k - MC_k(alpha,gamma)) + sum_k Omega_broker_{jk} * comm_k
# which should equal zero at the true parameters.

compute_g_bar <- function(theta) {

  alpha <- theta[1:N_ALPHA]
  gamma <- theta[(N_ALPHA + 1):(N_ALPHA + N_GAMMA)]

  # --- M1: Risk score residuals ---
  pred_log_rs_rf <- alpha[1] + X_rs %*% alpha[2:N_ALPHA]
  eps_rs <- as.vector(y_rs - pred_log_rs_rf) * w_rf
  g_rs <- colMeans(Z_rs * eps_rs)  # length 7

  # --- M2: Claims residuals ---
  pred_log_cl_rf <- gamma[1] + gamma[2] * pred_log_rs_rf + X_cl_exog %*% gamma[3:N_GAMMA]
  eps_cl <- as.vector(y_cl - pred_log_cl_rf) * w_rf
  g_cl <- colMeans(Z_cl * eps_cl)  # length 6

  # --- M3: FOC residuals (evaluated directly per cell) ---
  # Accumulate Z_foc' * foc_resid across all cells
  g_foc_sum <- rep(0, N_Z_FOC)
  n_foc <- 0L

  for (fc in foc_cells) {
    J <- length(fc$plan_names)

    # Predict log risk scores for this cell's plans
    pred_log_rs <- alpha[1] +
      alpha[2] * fc$Silver + alpha[3] * fc$Gold + alpha[4] * fc$Platinum +
      alpha[5] * fc$share_18to34 + alpha[6] * fc$share_35to54 +
      alpha[7] * fc$share_hispanic

    # Predict log claims
    pred_log_cl <- gamma[1] + gamma[2] * pred_log_rs +
      gamma[3] * fc$HMO + gamma[4] * fc$trend +
      gamma[5] * fc$Anthem + gamma[6] * fc$Blue_Shield + gamma[7] * fc$Health_Net

    pred_claims <- exp(pred_log_cl)
    pred_rs <- exp(pred_log_rs)

    # RA transfers (budget-neutral within cell)
    sh <- fc$shares
    av <- fc$plan_avs
    avg_p <- mean(fc$posted_premium, na.rm = TRUE)

    av_r <- as.character(round(av, 1))
    mh <- MH_LOOKUP[av_r]; mh[is.na(mh)] <- 1.0
    sum_rs_sh <- sum(pred_rs * sh, na.rm = TRUE)
    util_adj <- av * mh
    sum_util_sh <- sum(util_adj * sh, na.rm = TRUE)
    ra <- (pred_rs / sum_rs_sh - util_adj / sum_util_sh) * avg_p

    # MC(alpha, gamma)
    mc <- pred_claims * (1 - fc$reins_vec) - ra

    # FOC residual: s + ra_foc + Omega * (p - mc) + Omega_broker * comm
    # Includes RA derivative (adverse selection in pricing)
    ra_foc_cell <- if (!is.null(fc$ra_foc)) fc$ra_foc else rep(0, J)
    foc_resid <- fc$shares + ra_foc_cell +
                 as.vector(fc$Omega %*% (fc$posted_premium - mc)) +
                 as.vector(fc$Omega_broker %*% fc$comm_vec)

    # Instruments for this cell: intercept + plan characteristics
    Z_cell <- cbind(1, fc$Silver, fc$Gold, fc$Platinum,
                    fc$HMO, fc$trend, fc$Anthem, fc$Blue_Shield, fc$Health_Net)

    # Accumulate Z' * foc_resid
    g_foc_sum <- g_foc_sum + colSums(Z_cell * foc_resid)
    n_foc <- n_foc + J
  }

  g_foc <- g_foc_sum / n_foc  # average across all plan-cell observations

  c(g_rs, g_cl, g_foc)
}

# =========================================================================
# GMM OBJECTIVE
# =========================================================================

gmm_objective <- function(theta, W) {
  g <- compute_g_bar(theta)
  as.numeric(t(g) %*% W %*% g)
}

# =========================================================================
# STEP 1: IDENTITY-WEIGHTED GMM
# =========================================================================

cat("\n--- GMM Step 1 (identity weighting) ---\n")

# Debug: check initial moments
g_init <- compute_g_bar(theta0)
cat("  g_bar(theta0):", round(g_init, 4), "\n")
cat("  any NA/NaN/Inf:", any(!is.finite(g_init)), "\n")
if (any(!is.finite(g_init))) {
  cat("  non-finite indices:", which(!is.finite(g_init)), "\n")
  cat("  M1 (1-7):", round(g_init[1:7], 4), "\n")
  cat("  M2 (8-13):", round(g_init[8:13], 4), "\n")
  cat("  M3 (14-22):", round(g_init[14:22], 4), "\n")
  stop("Cannot proceed with non-finite initial moments")
}

W1 <- diag(N_MOMENTS)

result1 <- optim(
  par = theta0,
  fn = gmm_objective,
  W = W1,
  method = "BFGS",
  control = list(maxit = 2000, reltol = 1e-12, trace = 1, REPORT = 100)
)

cat("  Converged:", result1$convergence == 0, "\n")
cat("  Objective:", format(result1$value, digits = 6), "\n")

alpha1 <- result1$par[1:N_ALPHA]
gamma1 <- result1$par[(N_ALPHA + 1):(N_ALPHA + N_GAMMA)]

cat("  alpha (Step 1):", round(alpha1, 4), "\n")
cat("  gamma (Step 1):", round(gamma1, 4), "\n")

g1 <- compute_g_bar(result1$par)
cat("  g_bar at Step 1:\n")
cat("    M1 (risk score):", round(g1[1:ncol(Z_rs)], 4), "\n")
cat("    M2 (claims):", round(g1[(ncol(Z_rs)+1):(ncol(Z_rs)+ncol(Z_cl))], 4), "\n")
cat("    M3 (FOC):", round(g1[(ncol(Z_rs)+ncol(Z_cl)+1):N_MOMENTS], 6), "\n")

# =========================================================================
# STEP 2: OPTIMAL WEIGHTING
# =========================================================================

cat("\n--- GMM Step 2 (inverse-variance block-diagonal weighting) ---\n")

g1_rs  <- g1[1:ncol(Z_rs)]
g1_cl  <- g1[(ncol(Z_rs) + 1):(ncol(Z_rs) + ncol(Z_cl))]
g1_foc <- g1[(ncol(Z_rs) + ncol(Z_cl) + 1):N_MOMENTS]

v_rs  <- max(sum(g1_rs^2), 1e-20)
v_cl  <- max(sum(g1_cl^2), 1e-20)
v_foc <- max(sum(g1_foc^2), 1e-20)

cat("  Block moment norms: M1 =", format(sqrt(v_rs), digits = 4),
    " M2 =", format(sqrt(v_cl), digits = 4),
    " M3 =", format(sqrt(v_foc), digits = 4), "\n")

W2 <- as.matrix(Matrix::bdiag(
  diag(ncol(Z_rs)) / v_rs,
  diag(ncol(Z_cl)) / v_cl,
  diag(N_Z_FOC) / v_foc
))

result2 <- optim(
  par = result1$par,
  fn = gmm_objective,
  W = W2,
  method = "BFGS",
  control = list(maxit = 2000, reltol = 1e-12, trace = 1, REPORT = 100)
)

cat("  Converged:", result2$convergence == 0, "\n")
cat("  Objective:", format(result2$value, digits = 6), "\n")

alpha_gmm <- result2$par[1:N_ALPHA]
gamma_gmm <- result2$par[(N_ALPHA + 1):(N_ALPHA + N_GAMMA)]

cat("\n  alpha (GMM):", round(alpha_gmm, 4), "\n")
cat("  gamma (GMM):", round(gamma_gmm, 4), "\n")

g2 <- compute_g_bar(result2$par)
cat("  g_bar at Step 2:\n")
cat("    M1 (risk score):", round(g2[1:ncol(Z_rs)], 4), "\n")
cat("    M2 (claims):", round(g2[(ncol(Z_rs)+1):(ncol(Z_rs)+ncol(Z_cl))], 4), "\n")
cat("    M3 (FOC):", round(g2[(ncol(Z_rs)+ncol(Z_cl)+1):N_MOMENTS], 6), "\n")

# =========================================================================
# DIAGNOSTICS
# =========================================================================

cat("\n--- Diagnostics ---\n")

# Compare OLS vs GMM moment norms
g_ols <- compute_g_bar(theta0)
cat("  Moment norm ||g_bar||:\n")
cat("    OLS:", round(sqrt(sum(g_ols^2)), 4), "\n")
cat("    GMM:", round(sqrt(sum(g2^2)), 4), "\n")

# FOC residual distribution (how well does MC(alpha,gamma) satisfy the FOC?)
cat("\n  FOC moment breakdown (should be near 0):\n")
cat("    OLS g_foc:", round(g_ols[(ncol(Z_rs)+ncol(Z_cl)+1):N_MOMENTS], 6), "\n")
cat("    GMM g_foc:", round(g2[(ncol(Z_rs)+ncol(Z_cl)+1):N_MOMENTS], 6), "\n")

# Parameter comparison
cat("\n  Parameter comparison (OLS → GMM):\n")
comp <- data.frame(
  param = c(alpha_names, gamma_names),
  equation = c(rep("risk_score", N_ALPHA), rep("claims", N_GAMMA)),
  OLS = round(theta0, 4),
  GMM = round(result2$par, 4),
  change = round(result2$par - theta0, 4)
)
print(comp, row.names = FALSE)

# =========================================================================
# SAVE COEFFICIENTS
# =========================================================================

cat("\nSaving GMM cost coefficients...\n")

rs_coefs_gmm <- tibble(term = alpha_names, estimate = alpha_gmm)
cl_coefs_gmm <- tibble(term = gamma_names, estimate = gamma_gmm)

write_csv(rs_coefs_gmm, file.path(Sys.getenv("TEMP_DIR"), "ra_rs_coefs_gmm.csv"))
write_csv(cl_coefs_gmm, file.path(Sys.getenv("TEMP_DIR"), "ra_claims_coefs_gmm.csv"))

cat("  Saved GMM coefficients to", Sys.getenv("TEMP_DIR"), "\n")
cat("\nCost-side GMM complete.\n")
