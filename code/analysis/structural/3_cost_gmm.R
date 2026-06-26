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

# Dependencies: tidyverse, data.table, helpers (loaded by _supply.R)

# =========================================================================
# LOAD DATA
# =========================================================================

cat("Loading data for cost-side GMM...\n")

# --- Rate filing data (moments 1-2) ---
rsdata <- read_csv("data/output/rate_filing_rsdata.csv", show_col_types = FALSE) %>%
  filter(!is.na(log_risk_score), is.finite(log_risk_score),
         !is.na(log_cost), is.finite(log_cost), EXP_MM > 0)

# --- FOC inputs per cell (moment 3) ---
foc_files <- list.files(file.path(TEMP_DIR, "foc_inputs"),
                         pattern = "^foc_.*\\.rds$", full.names = TRUE)
if (length(foc_files) == 0) stop("No FOC input files found — run 2_pricing.R first")

foc_cells <- lapply(foc_files, readRDS)
cat("  FOC cells loaded:", length(foc_cells), "\n")
if (any(sapply(foc_cells, function(fc) is.null(fc$demo_shares)))) {
  stop("foc_inputs lack demo_shares — re-run 2_pricing.R to save predicted demographic shares")
}

# --- Predicted demographic shares (Saltzman Eq. 16): from the demand model, NOT
#     observed enrollment (which is endogenous). Aggregate per-cell predicted
#     shares to plan-year (enrollment-weighted) for the M1 risk-score moment;
#     the per-cell shares feed M3 below. Built from ALL cells (pre-Omega-filter)
#     to maximize M1 plan-year coverage. ---
demo_all <- rbindlist(lapply(foc_cells, function(fc) {
  d <- as.data.table(fc$demo_shares); d[, year := fc$year]; d
}), fill = TRUE)
pred_py <- demo_all[, .(
  share_18to34 = sum(share_18to34 * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE),
  share_35to54 = sum(share_35to54 * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE)
), by = .(plan_id, year)]
rsdata <- rsdata %>%
  left_join(as.data.frame(pred_py), by = c("plan_id", "year")) %>%
  # Small-insurer rows have NA PLAN_TYPE -> NA HMO. The old observed-share filter
  # dropped them; drop here too so M1/M2 share a complete-case sample (the claims
  # moment needs HMO). Small plans still enter the model on the application side,
  # where structural HMO is keyed off the Kaiser prefix, not PLAN_TYPE.
  filter(!is.na(share_18to34), !is.na(share_35to54), !is.na(HMO), !is.na(AV_METAL))
cat("  Rate filing observations:", nrow(rsdata), "\n")

# Filter to cells with valid Omega (non-NA markup)
foc_cells <- Filter(function(fc) {
  !any(is.na(fc$Omega)) && !any(is.na(fc$shares)) && length(fc$plan_ids) >= 2
}, foc_cells)
cat("  FOC cells with valid Omega:", length(foc_cells), "\n")

# --- Supply results (for plan metal classification) ---
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE)

# =========================================================================
# PREPARE MOMENT DATA
# =========================================================================

cat("Preparing moment data...\n")

# Cost-side insurer dummies: big four (Kaiser is absorbed by the HMO indicator,
# which is keyed off the Kaiser prefix) + the seven larger regionals. Other_Small
# is the baseline (no dummy). COST_PREFIX maps each name to its plan_id prefix for
# the M3 per-cell indicators. Defined once so the moment matrices, parameter
# vector, instruments, and predictions all stay in sync.
INS_COST <- c("Anthem", "Blue_Shield", "Health_Net",
              "Molina", "LA_Care", "SHARP", "Chinese_Community",
              "Oscar", "Western", "Valley")
COST_PREFIX <- c(Anthem = "ANT", Blue_Shield = "BS", Health_Net = "HN",
                 Molina = "MOL", LA_Care = "LA", SHARP = "SH",
                 Chinese_Community = "CC", Oscar = "OSC",
                 Western = "WEST", Valley = "VAL")
N_INS_COST <- length(INS_COST)

# --- M1-M2 data matrices (rate filings) ---
w_rf <- sqrt(rsdata$EXP_MM)  # WLS weights

# M1 regressors: risk score equation
X_rs <- as.matrix(rsdata %>% select(Silver, Gold, Platinum,
                                      share_18to34, share_35to54))
y_rs <- rsdata$log_risk_score

# M2 regressors: claims equation exogenous part (observed log_rs enters via Z_cl
# below). AV (generosity / moral hazard) enters directly — see estimate_ra_regressions.
X_cl_exog <- as.matrix(rsdata %>% transmute(AV = AV_METAL, HMO, trend,
                                            across(all_of(INS_COST))))
y_cl <- rsdata$log_cost

# M1 instruments: intercept + regressors
Z_rs <- cbind(1, X_rs)  # 6 columns (intercept + Silver/Gold/Platinum/age shares)

# M2 instruments: intercept + OBSERVED log_rs + exogenous regressors. The claims
# equation regresses on the observed risk score (treated as exogenous), not the
# metal+age-fitted one: the fitted score is ~collinear with AV (R^2~0.98) and cannot
# identify the generosity coef, while observed log_rs has within-metal variation.
Z_cl <- cbind(1, y_rs, X_cl_exog)  # intercept + log_rs + AV/HMO/trend + INS_COST

# --- M3: Precompute FOC cell data ---
# For each cell, we need plan characteristics to predict MC(alpha, gamma).
# Build once, store as list.

MH_LOOKUP <- c("0.6" = 1.00, "0.7" = 1.03, "0.8" = 1.08, "0.9" = 1.15)

for (k in seq_along(foc_cells)) {
  fc <- foc_cells[[k]]
  pn <- fc$plan_ids
  r <- fc$region; y <- fc$year

  # Plan characteristics from supply results (metal is now correct for all plans)
  sr_cell <- supply_results %>%
    filter(region == r, year == y, plan_id %in% pn)
  plan_metal <- setNames(sr_cell$metal[match(pn, sr_cell$plan_id)], pn)

  foc_cells[[k]]$Silver <- as.integer(plan_metal == "Silver")
  foc_cells[[k]]$Gold <- as.integer(plan_metal == "Gold")
  foc_cells[[k]]$Platinum <- as.integer(plan_metal == "Platinum")
  foc_cells[[k]]$HMO <- as.integer(str_detect(pn, "^KA"))
  foc_cells[[k]]$trend <- y - 2014L
  for (ins in INS_COST) {
    foc_cells[[k]][[ins]] <- as.integer(str_detect(pn, paste0("^", COST_PREFIX[[ins]])))
  }
  foc_cells[[k]]$AV <- unname(fc$plan_avs[pn])  # generosity term for the claims eq.

  # Predicted demographic shares for this cell (from the demand model, saved by
  # 2_pricing in the foc_inputs RDS) — consistent with M1 and with the application
  # in 2_pricing/4_counterfactuals. Hispanic/male dropped (unidentified). The
  # cell-mean fallback guards any plan_id absent from demo_shares.
  demo <- as.data.frame(fc$demo_shares)
  foc_cells[[k]]$share_18to34 <- sapply(pn, function(p) {
    v <- demo$share_18to34[demo$plan_id == p]
    if (length(v) == 0) return(mean(demo$share_18to34, na.rm = TRUE))
    v[1]
  })
  foc_cells[[k]]$share_35to54 <- sapply(pn, function(p) {
    v <- demo$share_35to54[demo$plan_id == p]
    if (length(v) == 0) return(mean(demo$share_35to54, na.rm = TRUE))
    v[1]
  })
}

rm(supply_results, demo_all, pred_py)

# Count total FOC equations (one per plan per cell)
n_foc_total <- sum(sapply(foc_cells, function(fc) length(fc$plan_ids)))
cat("  Total FOC equations:", n_foc_total, "\n")
n_foc_below <- sum(sapply(foc_cells, function(fc) sum(fc$shares < SHARE_FLOOR_FOC)))
cat("  Below share floor", SHARE_FLOOR_FOC, "(dropped from M3):",
    n_foc_below, "of", n_foc_total, "\n")

# M3 instruments: plan characteristics (same for each plan within the FOC)
# We'll compute Z_foc * eps_foc inside compute_g_bar by accumulating across cells
N_Z_FOC <- 6L + N_INS_COST  # intercept, Silver, Gold, Platinum, HMO, trend + INS_COST

N_MOMENTS <- ncol(Z_rs) + ncol(Z_cl) + N_Z_FOC
cat("  Total moment conditions:", N_MOMENTS, "(M1:", ncol(Z_rs),
    " M2:", ncol(Z_cl), " M3:", N_Z_FOC, ")\n")

# =========================================================================
# PARAMETER LAYOUT
# =========================================================================

N_ALPHA <- 6L  # intercept, Silver, Gold, Platinum, share_18to34, share_35to54
N_GAMMA <- 5L + N_INS_COST  # intercept, log_risk_score, AV, HMO, trend + INS_COST

alpha_names <- c("(Intercept)", "Silver", "Gold", "Platinum",
                 "share_18to34", "share_35to54")
gamma_names <- c("(Intercept)", "log_risk_score", "AV", "HMO", "trend", INS_COST)

# Starting values from OLS
rs_coefs_start <- read_csv(file.path(TEMP_DIR, "ra_rs_coefs.csv"), show_col_types = FALSE)
cl_coefs_start <- read_csv(file.path(TEMP_DIR, "ra_claims_coefs.csv"), show_col_types = FALSE)

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
  # Regress on OBSERVED risk score (gamma[2]); the M3 FOC block below still predicts
  # claims with the fitted risk score, so endogenous selection is preserved downstream.
  pred_log_cl_rf <- gamma[1] + gamma[2] * y_rs + X_cl_exog %*% gamma[3:N_GAMMA]
  eps_cl <- as.vector(y_cl - pred_log_cl_rf) * w_rf
  g_cl <- colMeans(Z_cl * eps_cl)  # length 6

  # --- M3: FOC residuals (evaluated directly per cell) ---
  # Accumulate Z_foc' * foc_resid across all cells
  g_foc_sum <- rep(0, N_Z_FOC)
  n_foc <- 0L

  for (fc in foc_cells) {
    J <- length(fc$plan_ids)

    # Predict log risk scores for this cell's plans
    pred_log_rs <- alpha[1] +
      alpha[2] * fc$Silver + alpha[3] * fc$Gold + alpha[4] * fc$Platinum +
      alpha[5] * fc$share_18to34 + alpha[6] * fc$share_35to54

    # Predict log claims (AV = gamma[3], generosity term). Insurer dummies are
    # gamma[6:(5+N_INS_COST)], in INS_COST order.
    pred_log_cl <- gamma[1] + gamma[2] * pred_log_rs + gamma[3] * fc$AV +
      gamma[4] * fc$HMO + gamma[5] * fc$trend
    for (j in seq_len(N_INS_COST)) {
      pred_log_cl <- pred_log_cl + gamma[5 + j] * fc[[INS_COST[j]]]
    }

    pred_claims <- exp(pred_log_cl)
    pred_rs <- exp(pred_log_rs)

    # RA transfers (budget-neutral within cell)
    sh <- fc$shares
    av <- fc$plan_avs
    # Enrollment-weighted statewide average premium (ACA RA scale), not a plan mean.
    avg_p <- weighted.mean(fc$posted_premium, fc$shares, na.rm = TRUE)

    av_r <- as.character(round(av, 1))
    mh <- MH_LOOKUP[av_r]; mh[is.na(mh)] <- 1.0
    sum_rs_sh <- sum(pred_rs * sh, na.rm = TRUE)
    util_adj <- av * mh
    sum_util_sh <- sum(util_adj * sh, na.rm = TRUE)
    ra <- (pred_rs / sum_rs_sh - util_adj / sum_util_sh) * avg_p

    # MC(alpha, gamma)
    mc <- pred_claims * (1 - fc$reins_vec) - ra

    # FOC residual: s + ra_foc + Omega * (p - mc) + Omega_broker * comm
    # Includes RA derivative (adverse selection in pricing). RECOMPUTE ra_foc at the
    # current cost parameters (it depends on pred_rs, which moves with theta) rather
    # than reading the stale OLS-stage fc$ra_foc — matches what 4_counterfactuals does.
    ra_foc_cell <- if (!is.null(fc$elast_mat) && !is.null(fc$own_mat)) {
      compute_ra_foc(setNames(pred_rs, fc$plan_ids), fc$shares, fc$plan_avs,
                     avg_p, fc$elast_mat, fc$own_mat)
    } else if (!is.null(fc$ra_foc)) fc$ra_foc else rep(0, J)
    foc_resid <- fc$shares + ra_foc_cell -
                 as.vector(fc$Omega %*% (fc$posted_premium - mc)) +
                 as.vector(fc$Omega_broker %*% fc$comm_vec)

    # Instruments for this cell: intercept + plan characteristics + insurer dummies
    Z_cell <- cbind(1, fc$Silver, fc$Gold, fc$Platinum, fc$HMO, fc$trend,
                    sapply(INS_COST, function(ins) fc[[ins]]))

    # Share floor: a plan with near-zero share has an ill-conditioned, uninformative
    # pricing FOC (the markup inversion blows up as share -> 0), so drop its FOC
    # equation from the M3 moments. It stays in the cell's Omega, so its cross-price
    # effects on the retained plans' FOCs are kept, and it stays in M1/M2.
    keep <- fc$shares >= SHARE_FLOOR_FOC
    if (any(keep)) {
      g_foc_sum <- g_foc_sum + colSums(Z_cell[keep, , drop = FALSE] * foc_resid[keep])
      n_foc <- n_foc + sum(keep)
    }
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
  cat("  M1:", round(g_init[1:ncol(Z_rs)], 4), "\n")
  cat("  M2:", round(g_init[(ncol(Z_rs)+1):(ncol(Z_rs)+ncol(Z_cl))], 4), "\n")
  cat("  M3:", round(g_init[(ncol(Z_rs)+ncol(Z_cl)+1):N_MOMENTS], 4), "\n")
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

# Negative-MC check at the GMM solution, by metal. The OLS-coef MC in 2_pricing
# was already non-negative; the negatives appeared once the FOC bent the cost
# coefs in GMM. With AV in the claims equation, platinum should clear (down to
# roughly the data's own tail) and the risk-score age coef should relax.
cat("\n  Negative MC at GMM solution (by metal):\n")
mc_rows <- lapply(foc_cells, function(fc) {
  plr <- alpha_gmm[1] + alpha_gmm[2]*fc$Silver + alpha_gmm[3]*fc$Gold + alpha_gmm[4]*fc$Platinum +
         alpha_gmm[5]*fc$share_18to34 + alpha_gmm[6]*fc$share_35to54
  pcl <- gamma_gmm[1] + gamma_gmm[2]*plr + gamma_gmm[3]*fc$AV + gamma_gmm[4]*fc$HMO +
         gamma_gmm[5]*fc$trend
  for (j in seq_len(N_INS_COST)) pcl <- pcl + gamma_gmm[5 + j] * fc[[INS_COST[j]]]
  prs <- exp(plr); pclm <- exp(pcl); sh <- fc$shares; av <- fc$plan_avs
  avg_p <- weighted.mean(fc$posted_premium, sh, na.rm = TRUE)
  mh <- MH_LOOKUP[as.character(round(av, 1))]; mh[is.na(mh)] <- 1
  util <- av * mh
  ra <- (prs / sum(prs*sh, na.rm = TRUE) - util / sum(util*sh, na.rm = TRUE)) * avg_p
  mc <- pclm * (1 - fc$reins_vec) - ra
  metal <- ifelse(fc$Platinum==1,"Platinum",ifelse(fc$Gold==1,"Gold",ifelse(fc$Silver==1,"Silver","Bronze")))
  tibble(metal = metal, mc = mc)
})
mc_check <- bind_rows(mc_rows) %>%
  group_by(metal) %>%
  summarise(n = n(), negative = sum(mc < 0), pct_neg = round(100*mean(mc < 0), 1), .groups = "drop")
print(mc_check)
cat("  AV (claims generosity) coef:", round(gamma_gmm[3], 4),
    " | risk-score age coefs:", round(alpha_gmm[5], 3), round(alpha_gmm[6], 3), "\n")

# =========================================================================
# SAVE COEFFICIENTS
# =========================================================================

cat("\nSaving GMM cost coefficients...\n")

rs_coefs_gmm <- tibble(term = alpha_names, estimate = alpha_gmm)
cl_coefs_gmm <- tibble(term = gamma_names, estimate = gamma_gmm)

write_csv(rs_coefs_gmm, file.path(TEMP_DIR, "ra_rs_coefs_gmm.csv"))
write_csv(cl_coefs_gmm, file.path(TEMP_DIR, "ra_claims_coefs_gmm.csv"))

cat("  Saved GMM coefficients to", TEMP_DIR, "\n")
cat("\nCost-side GMM complete.\n")
