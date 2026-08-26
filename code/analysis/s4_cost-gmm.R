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

# --- Rate filing PUF (claims, moment 2) and SRRT plan risk scores (moment 1) ---
rsdata <- read_csv("data/output/rate_filing_rsdata.csv", show_col_types = FALSE) %>%
  filter(!is.na(log_cost), is.finite(log_cost), EXP_MM > 0)
rs_srrt      <- read_csv("data/output/plan_risk_scores.csv", show_col_types = FALSE)
rs_srrt_year <- read_csv("data/output/plan_risk_scores_year.csv", show_col_types = FALSE)
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE)

# --- FOC inputs per cell (moment 3) ---
foc_files <- list.files(file.path(TEMP_DIR, "foc_inputs"),
                         pattern = "^foc_.*\\.rds$", full.names = TRUE)
if (length(foc_files) == 0) stop("No FOC input files found — run 2_pricing.R first")

foc_cells <- lapply(foc_files, readRDS)
cat("  FOC cells loaded:", length(foc_cells), "\n")
if (any(sapply(foc_cells, function(fc) is.null(fc$demo_shares)))) {
  stop("foc_inputs lack demo_shares — re-run 2_pricing.R to save predicted demographic shares")
}

# --- Predicted demographic shares: from the demand model, NOT observed
#     enrollment (which is endogenous). The per-cell predicted shares are
#     aggregated (demand-weighted) to insurer-metal-region-year for the M1
#     risk-score rows (SRRT) and to plan-year for the M2 claims rows (PUF);
#     the per-cell shares feed M3 below. Built from ALL cells. ---
demo_all <- rbindlist(lapply(foc_cells, function(fc) {
  d <- as.data.table(fc$demo_shares); d[, `:=`(year = fc$year, region = fc$region)]; d
}), fill = TRUE)
plan_metal_map <- as.data.table(supply_results)[, .(metal = first(metal)), by = plan_id]
demo_all <- merge(demo_all, plan_metal_map, by = "plan_id")
demo_all[, insurer_prefix := sub("_.*", "", plan_id)]

pred_region <- demo_all[, .(
  share_18to34   = sum(share_18to34 * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE),
  share_35to54   = sum(share_35to54 * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE),
  share_hispanic = sum(share_hispanic * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE)
), by = .(insurer_prefix, metal, region, year)]
rs_srrt <- rs_srrt %>%
  inner_join(as.data.frame(pred_region), by = c("insurer_prefix", "metal", "region", "year")) %>%
  filter(is.finite(log_risk_score), member_months > 0,
         !is.na(share_18to34), !is.na(share_35to54), !is.na(share_hispanic)) %>%
  mutate(Silver = as.integer(metal == "Silver"), Gold = as.integer(metal == "Gold"),
         Platinum = as.integer(metal == "Platinum"))
cat("  SRRT risk-score observations (M1):", nrow(rs_srrt), "\n")

pred_py <- demo_all[, .(
  share_18to34   = sum(share_18to34 * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE),
  share_35to54   = sum(share_35to54 * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE),
  share_hispanic = sum(share_hispanic * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE)
), by = .(plan_id, year)]
rsdata <- rsdata %>%
  left_join(as.data.frame(pred_py), by = c("plan_id", "year")) %>%
  mutate(insurer_prefix = sub("_.*", "", plan_id), metal = METAL) %>%
  # The SRRT plan-year score instruments the predicted score in the claims moment
  left_join(rs_srrt_year %>% transmute(insurer_prefix, metal, year, log_rs_srrt = log_risk_score),
            by = c("insurer_prefix", "metal", "year")) %>%
  filter(!is.na(share_18to34), !is.na(share_35to54), !is.na(share_hispanic),
         !is.na(HMO), !is.na(log_rs_srrt))
cat("  Rate filing claims observations (M2):", nrow(rsdata), "\n")

# Filter to cells with valid Omega (non-NA markup)
foc_cells <- Filter(function(fc) {
  !any(is.na(fc$Omega)) && !any(is.na(fc$shares)) && length(fc$plan_ids) >= 2
}, foc_cells)
cat("  FOC cells with valid Omega:", length(foc_cells), "\n")


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

# --- M1 data matrices (SRRT risk scores) ---
w_rs <- sqrt(rs_srrt$member_months)  # WLS weights
X_rs <- as.matrix(rs_srrt %>% select(Silver, Gold, Platinum,
                                      share_18to34, share_35to54, share_hispanic))
y_rs <- rs_srrt$log_risk_score
Z_rs <- cbind(1, X_rs)               # M1 instruments: intercept + regressors

# --- M2 data matrices (rate filing PUF claims) ---
w_rf <- sqrt(rsdata$EXP_MM)          # WLS weights
# Regressors of the predicted risk score for the claims rows (same terms as M1,
# at the plan-year predicted shares)
X_rs_cl <- as.matrix(rsdata %>% select(Silver, Gold, Platinum,
                                        share_18to34, share_35to54, share_hispanic))
# Claims equation exogenous part: HMO, trend, and big-four insurer indicators.
# AV is OMITTED, since the risk score carries generosity.
X_cl_exog <- as.matrix(rsdata %>% transmute(HMO, trend, Anthem, Blue_Shield, Kaiser, Health_Net))
y_cl <- rsdata$log_cost
# M2 instruments: intercept + the SRRT plan-year log risk score + exogenous
# regressors. The claims equation regresses on the PREDICTED risk score (see
# compute_g_bar); the observed score instruments it.
Z_cl <- cbind(1, rsdata$log_rs_srrt, X_cl_exog)

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
  # Network type from the plan attributes s3 saved (falls back to the Kaiser prefix)
  foc_cells[[k]]$HMO <- if (!is.null(fc$hmo)) as.integer(fc$hmo[pn]) else as.integer(str_detect(pn, "^KA"))
  foc_cells[[k]]$trend <- y - 2014L
  for (ins in INS_COST) {
    foc_cells[[k]][[ins]] <- as.integer(str_detect(pn, paste0("^", COST_PREFIX[[ins]])))
  }
  foc_cells[[k]]$Kaiser <- as.integer(str_detect(pn, "^KA"))

  # Predicted demographic shares for this cell (age, gender, income), from the
  # demand model saved in the foc_inputs RDS — consistent with M1 and with the
  # application in s3_pricing/cf. The cell-mean fallback guards any plan_id absent
  # from demo_shares. AV comes from plan_avs (the dominant risk-score regressor).
  demo <- as.data.frame(fc$demo_shares)
  for (col in c("share_18to34", "share_35to54", "share_hispanic")) {
    foc_cells[[k]][[col]] <- sapply(pn, function(p) {
      v <- demo[[col]][demo$plan_id == p]
      if (length(v) == 0) return(mean(demo[[col]], na.rm = TRUE))
      v[1]
    })
  }
  foc_cells[[k]]$AV <- as.numeric(fc$plan_avs[pn])
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

# M4: the insurer commission FOC, MB_ft = MC_ft per insurer-year, as one moment
# (the mean of the scaled FOC residual over insurer-years). No free parameter of
# its own: it disciplines the cost parameters through the margins in MB.
N_Z_COMM <- 1L
N_MOMENTS <- ncol(Z_rs) + ncol(Z_cl) + N_Z_FOC + N_Z_COMM
cat("  Total moment conditions:", N_MOMENTS, "(M1:", ncol(Z_rs),
    " M2:", ncol(Z_cl), " M3:", N_Z_FOC, " M4:", N_Z_COMM, ")\n")

# Per-cell insurer structure for M4 (theta-independent pieces)
comm_struct <- lapply(foc_cells, function(fc) {
  if (is.null(fc$comm_D) || is.null(fc$comm_qB)) return(NULL)
  pn <- fc$plan_ids; pref <- sub("_.*", "", pn)
  firms <- unique(pref[fc$comm_vec > 0])
  if (length(firms) == 0) return(NULL)
  lapply(firms, function(f) {
    ii <- which(pref == f)
    w_f <- numeric(length(pn)); w_f[ii] <- fc$comm_vec[ii]
    list(firm = f, ii = ii, dq = as.numeric(fc$comm_D %*% w_f),
         MC = sum(fc$comm_qB[ii] * fc$comm_vec[ii]))
  })
})

# =========================================================================
# PARAMETER LAYOUT
# =========================================================================

N_ALPHA <- 7L   # intercept, 3 metal dummies, 3 demographic shares
N_GAMMA <- 8L   # intercept, log_risk_score, HMO, trend, 4 big-four insurer dummies

alpha_names <- c("(Intercept)", "Silver", "Gold", "Platinum",
                 "share_18to34", "share_35to54", "share_hispanic")
gamma_names <- c("(Intercept)", "log_risk_score", "HMO", "trend",
                 "Anthem", "Blue_Shield", "Kaiser", "Health_Net")

# Starting values from OLS
rs_coefs_start <- read_csv(file.path(TEMP_DIR, "ra_rs_coefs.csv"), show_col_types = FALSE)
cl_coefs_start <- read_csv(file.path(TEMP_DIR, "ra_claims_coefs.csv"), show_col_types = FALSE)

alpha0_raw <- setNames(rs_coefs_start$estimate, rs_coefs_start$term)[alpha_names]
alpha0_raw[is.na(alpha0_raw)] <- 0  # any aliased insurer FE -> baseline
alpha0 <- unname(alpha0_raw)
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
# M4: E[MB_ft / MC_ft - 1] = 0                  (commission FOC per insurer-year)
#
# FOC residual for plan j in cell c:
#   foc_j = s_j + sum_k Omega_{jk} * (p_k - MC_k(alpha,gamma)) + sum_k Omega_broker_{jk} * comm_k
# which should equal zero at the true parameters.

# return_contributions = TRUE additionally returns the per-observation moment
# contributions (the matrices whose column means / sums make up g_bar): M12_mat
# (n_rf x [M1+M2], one row per rate-filing obs) and the M3 FOC contributions both
# per kept plan-cell (M3_obs) and aggregated within region-year cell (M3_cell, for
# cluster-robust meat). The default averaged return is unchanged, so the GMM
# objective and the SE sandwich share one code path (cost_gmm_sandwich_se).
compute_g_bar <- function(theta, return_contributions = FALSE) {

  alpha <- theta[1:N_ALPHA]
  gamma <- theta[(N_ALPHA + 1):(N_ALPHA + N_GAMMA)]

  # --- M1: Risk score residuals (SRRT rows) ---
  pred_log_rs_srrt <- alpha[1] + X_rs %*% alpha[2:N_ALPHA]
  eps_rs <- as.vector(y_rs - pred_log_rs_srrt) * w_rs
  M1_mat <- Z_rs * eps_rs           # n_rs x ncol(Z_rs)
  g_rs <- colMeans(M1_mat)

  # --- M2: Claims residuals (PUF rows) ---
  # Regress claims on the PREDICTED risk score at the plan-year predicted shares,
  # matching the FOC and counterfactual, which also predict claims from the
  # fitted score. The SRRT plan-year score is the instrument in Z_cl.
  pred_log_rs_rf <- alpha[1] + X_rs_cl %*% alpha[2:N_ALPHA]
  pred_log_cl_rf <- gamma[1] + gamma[2] * pred_log_rs_rf + X_cl_exog %*% gamma[3:N_GAMMA]
  eps_cl <- as.vector(y_cl - pred_log_cl_rf) * w_rf
  M2_mat <- Z_cl * eps_cl           # n_rf x ncol(Z_cl)
  g_cl <- colMeans(M2_mat)

  # --- M3: FOC residuals (evaluated directly per cell) ---
  # Accumulate Z_foc' * foc_resid across all cells
  g_foc_sum <- rep(0, N_Z_FOC)
  n_foc <- 0L
  if (return_contributions) { m3_obs_list <- list(); m3_cell_list <- list(); ki <- 0L }
  MB_fy <- numeric(0); MC_fy <- numeric(0)   # M4 accumulators keyed firm_year

  for (ci in seq_along(foc_cells)) {
    fc <- foc_cells[[ci]]
    J <- length(fc$plan_ids)

    # Predict log risk scores for this cell's plans (metal dummies + demographic shares)
    pred_log_rs <- alpha[1] + alpha[2] * fc$Silver + alpha[3] * fc$Gold + alpha[4] * fc$Platinum +
      alpha[5] * fc$share_18to34 + alpha[6] * fc$share_35to54 + alpha[7] * fc$share_hispanic

    # Predict log claims from the risk score (AV omitted; carried by the score),
    # HMO, trend, and the big-four insurer indicators.
    pred_log_cl <- gamma[1] + gamma[2] * pred_log_rs +
      gamma[3] * fc$HMO + gamma[4] * fc$trend +
      gamma[5] * fc$Anthem + gamma[6] * fc$Blue_Shield + gamma[7] * fc$Kaiser + gamma[8] * fc$Health_Net

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

    # M4: commission FOC pieces at the current cost parameters. MB_f = sum over the
    # insurer's plans of margin x d qB / d k plus the risk-adjustment response;
    # MC_f = commission outlay. Summed to the insurer-year across cells below.
    cs <- comm_struct[[ci]]
    if (!is.null(cs)) {
      ra_eta <- compute_ra_foc(setNames(pred_rs, fc$plan_ids), fc$shares, fc$plan_avs,
                               avg_p, fc$comm_D, fc$own_mat)
      margin <- fc$posted_premium - mc - fc$comm_vec
      for (cf_ in cs) {
        key <- paste(cf_$firm, fc$year, sep = "_")
        MBf <- sum(margin[cf_$ii] * cf_$dq[cf_$ii]) + sum(fc$comm_vec[cf_$ii] * ra_eta[cf_$ii])
        MB_fy[key] <- (if (is.na(MB_fy[key])) 0 else MB_fy[key]) + MBf
        MC_fy[key] <- (if (is.na(MC_fy[key])) 0 else MC_fy[key]) + cf_$MC
      }
    }

    # Instruments for this cell: intercept + plan characteristics + insurer dummies
    Z_cell <- cbind(1, fc$Silver, fc$Gold, fc$Platinum, fc$HMO, fc$trend,
                    sapply(INS_COST, function(ins) fc[[ins]]))

    # Share floor: a plan with near-zero share has an ill-conditioned, uninformative
    # pricing FOC (the markup inversion blows up as share -> 0), so drop its FOC
    # equation from the M3 moments. It stays in the cell's Omega, so its cross-price
    # effects on the retained plans' FOCs are kept, and it stays in M1/M2.
    keep <- fc$shares >= SHARE_FLOOR_FOC
    if (any(keep)) {
      contrib <- Z_cell[keep, , drop = FALSE] * foc_resid[keep]
      g_foc_sum <- g_foc_sum + colSums(contrib)
      n_foc <- n_foc + sum(keep)
      if (return_contributions) {
        ki <- ki + 1L
        m3_obs_list[[ki]]  <- contrib
        m3_cell_list[[ki]] <- colSums(contrib)
      }
    }
  }

  g_foc <- g_foc_sum / n_foc  # average across all plan-cell observations

  # M4: scaled commission FOC residual per insurer-year, averaged
  ok4 <- is.finite(MB_fy) & is.finite(MC_fy) & MC_fy > 0
  r4 <- MB_fy[ok4] / MC_fy[ok4] - 1
  g_comm <- mean(r4)

  g <- c(g_rs, g_cl, g_foc, g_comm)
  if (!return_contributions) return(g)
  list(g       = g,
       # Moment blocks for the covariance: one row per observation of each block
       # (SRRT rows, PUF rows, region-year cells, insurer-years); blocks are
       # treated as independent
       blocks  = list(list(mat = M1_mat, n = nrow(M1_mat)),
                      list(mat = M2_mat, n = nrow(M2_mat)),
                      list(mat = do.call(rbind, m3_cell_list), n = n_foc),
                      list(mat = matrix(r4, ncol = 1), n = length(r4))),
       M3_obs  = do.call(rbind, m3_obs_list),
       n_foc   = n_foc,
       n_comm  = length(r4),
       comm_fy = data.frame(key = names(r4), MB = MB_fy[ok4], MC = MC_fy[ok4],
                            mu_hat = r4, stringsAsFactors = FALSE))
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
  cat("  M3:", round(g_init[(ncol(Z_rs)+ncol(Z_cl)+1):(N_MOMENTS - N_Z_COMM)], 4), "\n")
  cat("  M4:", round(g_init[N_MOMENTS], 4), "\n")
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
cat("    M3 (FOC):", round(g1[(ncol(Z_rs)+ncol(Z_cl)+1):(N_MOMENTS - N_Z_COMM)], 6), "\n")
cat("    M4 (commission FOC):", round(g1[N_MOMENTS], 6), "\n")

# =========================================================================
# STEP 2: OPTIMAL WEIGHTING
# =========================================================================

cat("\n--- GMM Step 2 (optimal weighting: inverse moment covariance) ---\n")

# Efficient two-step feasible GMM. The step-2 weight is the
# inverse of the moment variance-covariance matrix S, estimated at the step-1
# parameters. S is block-diagonal across the independent data sources: the risk-score
# moments from the SRRT rows, the claims moments from the rate-filing plan-years, the
# FOC moments from the equilibrium cells (one row per region-year), and the
# commission FOC from the insurer-years, so the cross-block covariance is zero. This is the same S the sandwich uses for its meat, so
# feeding S^{-1} back as the weight makes the estimator efficient and the sandwich
# collapse to the correct (G' S^{-1} G)^{-1} variance. The earlier block-diagonal
# SCALAR weight (diag / sum(moment^2)) was a crude stand-in; it over-credited the FOC
# block's sensitivity to the weakly-identified risk-score coefficients as precision,
# distorting those coefficients and understating their SEs by 1-2 orders of magnitude.
contr1 <- compute_g_bar(result1$par, return_contributions = TRUE)
S <- moment_cov_blocks(contr1$blocks, N_MOMENTS)

# Invert S to get the optimal weight. If S is ill-conditioned (near-redundant moments,
# most likely in the FOC block) a plain solve() is unstable, so ridge the diagonal and
# warn. A tiny rcond here is the signal that the analytical route is fragile and we
# should fall back to bootstrapping the SEs.
S_rcond <- rcond(S)
cat("  Moment-covariance rcond:", format(S_rcond, digits = 3),
    " (small => ill-conditioned; consider bootstrap)\n")
if (is.na(S_rcond) || S_rcond < 1e-12) {
  ridge <- 1e-6 * mean(diag(S))
  cat("  S ill-conditioned; ridge-regularizing diagonal by", format(ridge, digits = 3), "\n")
  W2 <- solve(S + diag(ridge, N_MOMENTS))
} else {
  W2 <- solve(S)
}

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
cat("    M3 (FOC):", round(g2[(ncol(Z_rs)+ncol(Z_cl)+1):(N_MOMENTS - N_Z_COMM)], 6), "\n")
cat("    M4 (commission FOC):", round(g2[N_MOMENTS], 6), "\n")

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
cat("    OLS g_foc:", round(g_ols[(ncol(Z_rs)+ncol(Z_cl)+1):(N_MOMENTS - N_Z_COMM)], 6), "\n")
cat("    GMM g_foc:", round(g2[(ncol(Z_rs)+ncol(Z_cl)+1):(N_MOMENTS - N_Z_COMM)], 6), "\n")
cat("    OLS g_comm:", round(g_ols[N_MOMENTS], 6), " | GMM g_comm:", round(g2[N_MOMENTS], 6), "\n")

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

# Negative-MC check at the GMM solution, by metal. With AV out of the claims
# equation, the risk-score pass-through should rise toward one and predicted
# claims for high-metal cells should climb, so the negatives should shrink.
cat("\n  Negative MC at GMM solution (by metal):\n")
mc_rows <- lapply(foc_cells, function(fc) {
  plr <- alpha_gmm[1] + alpha_gmm[2]*fc$Silver + alpha_gmm[3]*fc$Gold + alpha_gmm[4]*fc$Platinum +
         alpha_gmm[5]*fc$share_18to34 + alpha_gmm[6]*fc$share_35to54 + alpha_gmm[7]*fc$share_hispanic
  pcl <- gamma_gmm[1] + gamma_gmm[2]*plr + gamma_gmm[3]*fc$HMO + gamma_gmm[4]*fc$trend +
         gamma_gmm[5]*fc$Anthem + gamma_gmm[6]*fc$Blue_Shield + gamma_gmm[7]*fc$Kaiser + gamma_gmm[8]*fc$Health_Net
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
cat("  Claims pass-through (log risk score):", round(gamma_gmm[2], 4),
    " | risk-score metal effects:", round(alpha_gmm[2:4], 3),
    " demographic shares:", round(alpha_gmm[5:7], 3), "
")

# =========================================================================
# M4 DIAGNOSTIC: commission FOC residuals at the GMM solution
# =========================================================================
# The commission FOC MB_ft = MC_ft is imposed as a moment above, so the
# counterfactual solves it with no insurer-specific term (mu = 0). The residual
# mu_hat = MB/MC - 1 per insurer-year is reported here as a fit diagnostic and
# written alongside mu_fit = 0, which cf1 reads.

cat("\n--- M4: commission FOC residuals at the GMM solution ---\n")
contr2 <- compute_g_bar(result2$par, return_contributions = TRUE)
comm_fy <- contr2$comm_fy %>%
  tidyr::separate(key, into = c("firm", "year"), sep = "_", convert = TRUE) %>%
  mutate(mu_fit = 0)
cat("  insurer-year conditions:", nrow(comm_fy),
    " | distinct insurers:", n_distinct(comm_fy$firm), "\n")
cat("  mu_hat = MB/MC - 1: mean", round(mean(comm_fy$mu_hat), 3),
    " sd", round(sd(comm_fy$mu_hat), 3),
    " range [", round(min(comm_fy$mu_hat), 2), ",", round(max(comm_fy$mu_hat), 2), "]\n")
print(comm_fy %>% group_by(firm) %>%
        summarise(n = n(), mean_mu_hat = round(mean(mu_hat), 2), .groups = "drop"), n = Inf)

write_csv(tibble(term = "(Intercept)", estimate = 0),
          file.path(TEMP_DIR, "commission_foc_coefs.csv"))
write_csv(comm_fy %>% select(firm, year, MB, MC, mu_hat, mu_fit),
          file.path(TEMP_DIR, "commission_foc_fit.csv"))
cat("  Saved commission_foc_fit.csv (mu_fit = 0)\n")

# =========================================================================
# SAVE COEFFICIENTS
# =========================================================================

cat("\nSaving GMM cost coefficients...\n")

rs_coefs_gmm <- tibble(term = alpha_names, estimate = alpha_gmm)
cl_coefs_gmm <- tibble(term = gamma_names, estimate = gamma_gmm)

write_csv(rs_coefs_gmm, file.path(TEMP_DIR, "ra_rs_coefs_gmm.csv"))
write_csv(cl_coefs_gmm, file.path(TEMP_DIR, "ra_claims_coefs_gmm.csv"))

cat("  Saved GMM coefficients to", TEMP_DIR, "\n")
# Standard errors are computed in 10_struc-se.R (reuses result2 / W2 / compute_g_bar).

cat("\nCost-side GMM complete.\n")
