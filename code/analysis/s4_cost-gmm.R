# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-31
## Description:   Two-step GMM for the cost side with demand held fixed: the
##                risk-score coefficients (alpha) and the claims coefficients
##                (gamma); the risk-score equation is estimated once by
##                weighted OLS on the SRRT plan scores and held fixed. GMM
##                moment blocks:
##                  M2: claims regression on the rate-filing plan-years
##                  M3: plan-year pricing FOC residuals (cell-level instruments)
##                beta, the administrative saving per commission dollar, is the
##                per-carrier substitution rate from the national MLR filings
##                (data-build step 9), held fixed; the commission conditions
##                MB = (1 - beta) MC are evaluated at it as diagnostics.
##                Writes the GMM coefficients, beta by insurer-year, and the
##                commission residuals.

# Dependencies: tidyverse, data.table, helpers (loaded by _analysis.R)

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
# Commission substitution: beta, the administrative saving per commission
# dollar. Per-carrier values from the national MLR filings (data-build step 9),
# bounded and varying with filer size; the pooled within-insurer slope is the
# default for carriers outside the table. The insurers' administrative cost
# level enters through the FOC cells (s3).
BETA0 <- read_csv("data/output/mlr_admin_beta.csv", show_col_types = FALSE)$beta0[1]
beta_carrier <- read_csv("data/output/commission_beta_carrier.csv", show_col_types = FALSE)
cat("  per-carrier substitution rates:", nrow(beta_carrier), "carriers\n")

# --- FOC inputs per cell (moment 3) ---
foc_files <- list.files(file.path(TEMP_DIR, "foc_inputs"),
                         pattern = "^foc_.*\\.rds$", full.names = TRUE)
if (length(foc_files) == 0) stop("No FOC input files found — run s3_pricing.R first")

foc_cells <- lapply(foc_files, readRDS)
cat("  FOC cells loaded:", length(foc_cells), "\n")
if (any(sapply(foc_cells, function(fc) is.null(fc$demo_shares)))) {
  stop("foc_inputs lack demo_shares — re-run s3_pricing.R to save predicted demographic shares")
}
if (any(sapply(foc_cells, function(fc) is.null(fc$admin)))) {
  stop("foc_inputs lack the administrative cost per member — re-run s3_pricing.R")
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

pred_region <- demo_all[, lapply(.SD, function(v) sum(v * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE)),
                        by = .(insurer_prefix, metal, region, year), .SDcols = RS_DEMO_TERMS]
rs_srrt <- rs_srrt %>%
  inner_join(as.data.frame(pred_region), by = c("insurer_prefix", "metal", "region", "year")) %>%
  filter(is.finite(log_risk_score), member_months > 0,
         if_all(all_of(RS_DEMO_TERMS), ~ !is.na(.x))) %>%
  mutate(Silver = as.integer(metal == "Silver"), Gold = as.integer(metal == "Gold"),
         Platinum = as.integer(metal == "Platinum"))
cat("  SRRT risk-score observations (M1):", nrow(rs_srrt), "\n")

pred_py <- demo_all[, lapply(.SD, function(v) sum(v * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE)),
                    by = .(plan_id, year), .SDcols = RS_DEMO_TERMS]
# Rating-area shares of the plan-year's enrollment (claims equation), from build3
plan_region_shares <- read_csv(file.path(TEMP_DIR, "plan_choice.csv"), show_col_types = FALSE) %>%
  select(plan_id, year, all_of(CLAIMS_REGION_TERMS)) %>%
  distinct(plan_id, year, .keep_all = TRUE)
rsdata <- rsdata %>%
  left_join(as.data.frame(pred_py), by = c("plan_id", "year")) %>%
  left_join(plan_region_shares, by = c("plan_id", "year")) %>%
  mutate(across(all_of(CLAIMS_REGION_TERMS), ~ ifelse(is.na(.x), 0, .x))) %>%
  mutate(insurer_prefix = sub("_.*", "", plan_id), metal = METAL) %>%
  # The SRRT plan-year score instruments the predicted score in the claims moment
  left_join(rs_srrt_year %>% transmute(insurer_prefix, metal, year, log_rs_srrt = log_risk_score),
            by = c("insurer_prefix", "metal", "year")) %>%
  filter(if_all(all_of(RS_DEMO_TERMS), ~ !is.na(.x)),
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

# --- Risk-score equation: weighted OLS on the SRRT rows (metal dummies and
#     the predicted composition shares), estimated once and held fixed in the
#     GMM ---
rs_ols <- lm(reformulate(c("Silver", "Gold", "Platinum", RS_DEMO_TERMS), "log_risk_score"),
             data = rs_srrt, weights = rs_srrt$member_months)
alpha_names <- c("(Intercept)", "Silver", "Gold", "Platinum", RS_DEMO_TERMS)
ALPHA_FIXED <- unname(coef(rs_ols)[alpha_names])
cat("  Risk-score OLS (fixed): N =", nrow(rs_srrt), " R2 =", round(summary(rs_ols)$r.squared, 3), "\n")
cat("    ", paste(alpha_names, round(ALPHA_FIXED, 3), collapse = "; "), "\n")

# --- M2 data matrices (rate filing PUF claims) ---
# Weights: member months normalized within insurer, so every insurer carries the
# same total weight
w_rf <- rsdata$EXP_MM / ave(rsdata$EXP_MM, rsdata$insurer_prefix, FUN = sum)
X_rs_cl <- as.matrix(rsdata %>% select(Silver, Gold, Platinum, all_of(RS_DEMO_TERMS)))
# Claims equation exogenous part (CLAIMS_EXOG_TERMS): HMO, trend, big-four
# insurer indicators, rating-area shares. AV is OMITTED, since the risk score
# carries generosity.
X_cl_exog <- as.matrix(rsdata %>% select(all_of(CLAIMS_EXOG_TERMS)))
y_cl <- rsdata$log_cost
CLAIMS_SCALE <- mean(exp(y_cl))      # normalization of the level residual
# The risk score in the claims equation is the first-stage fitted value of the
# observed SRRT plan-year score on the predicted score and the claims controls;
# the instruments are the regressors themselves
pred_log_rs_rf <- as.vector(ALPHA_FIXED[1] + X_rs_cl %*% ALPHA_FIXED[-1])
rs_first_stage <- lm(rsdata$log_rs_srrt ~ pred_log_rs_rf + X_cl_exog, weights = rsdata$EXP_MM)
log_rs_fs <- as.vector(fitted(rs_first_stage))
cat("  Claims first stage: coefficient on the predicted score",
    round(coef(rs_first_stage)[2], 3), " R2 =", round(summary(rs_first_stage)$r.squared, 3), "\n")
W_cl <- cbind(1, log_rs_fs, X_cl_exog)   # claims regressors
Z_cl <- W_cl                             # instruments

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

  # Predicted demographic shares for this cell (RS_DEMO_TERMS), from the demand
  # model saved in the foc_inputs RDS — consistent with M1 and with the
  # application in s3_pricing/cf. The cell-mean fallback guards any plan_id absent
  # from demo_shares.
  demo <- as.data.frame(fc$demo_shares)
  for (col in RS_DEMO_TERMS) {
    foc_cells[[k]][[col]] <- sapply(pn, function(p) {
      v <- demo[[col]][demo$plan_id == p]
      if (length(v) == 0) return(mean(demo[[col]], na.rm = TRUE))
      v[1]
    })
  }
  foc_cells[[k]]$AV <- as.numeric(fc$plan_avs[pn])
  # Rating-area shares of the plan-year (claims equation)
  prs <- plan_region_shares[plan_region_shares$year == y, ]
  for (rc in CLAIMS_REGION_TERMS) {
    v <- prs[[rc]][match(pn, prs$plan_id)]
    foc_cells[[k]][[rc]] <- as.numeric(ifelse(is.na(v), 0, v))
  }
  # Per-cell design matrices for the two cost equations (intercepts excluded)
  foc_cells[[k]]$X_rs <- do.call(cbind, lapply(c("Silver", "Gold", "Platinum", RS_DEMO_TERMS),
                                               function(v) as.numeric(foc_cells[[k]][[v]])))
  foc_cells[[k]]$X_cl <- do.call(cbind, lapply(CLAIMS_EXOG_TERMS,
                                               function(v) as.numeric(foc_cells[[k]][[v]])))
}

# Transfer-formula pieces per cell that do not depend on theta: the plans'
# induced-demand factors and the cell's GCF (x multiplier), the AV term y, and
# the cell's members and premium revenue for the statewide sums (ra.R).
for (k in seq_along(foc_cells)) {
  fc <- foc_cells[[k]]; pn <- fc$plan_ids
  demo <- as.data.frame(fc$demo_shares)
  arf <- demo$arf[match(pn, demo$plan_id)]; arf[is.na(arf)] <- 1
  foc_cells[[k]]$arf   <- setNames(arf, pn)
  foc_cells[[k]]$gcf   <- if (!is.null(fc$gcf)) fc$gcf else ra_gcf(fc$region, fc$year)
  idf <- unname(RA_IDF_BY_AV[as.character(round(unname(fc$plan_avs[pn]), 1))]); idf[is.na(idf)] <- 1
  foc_cells[[k]]$x_mult <- idf * foc_cells[[k]]$gcf                       # x_j = r_j * x_mult_j
  sh <- unname(fc$shares[pn])
  foc_cells[[k]]$A_own  <- fc$N * sum(sh * unname(fc$plan_avs[pn]) * arf * idf * foc_cells[[k]]$gcf, na.rm = TRUE)
  foc_cells[[k]]$M_own  <- fc$N * sum(sh, na.rm = TRUE)
  foc_cells[[k]]$PM_own <- fc$N * sum(sh * unname(fc$posted_premium), na.rm = TRUE)
}
stopifnot(all(is.finite(sapply(foc_cells, function(fc) fc$gcf))))

# Plan-year structure for the pricing FOC: each plan is priced once for the year
# (base premium P_jy) with a fixed regional factor g_jc (s3, supply_results), so
# the FOC the model imposes is the plan-year aggregate G_jy = sum_c N_c g_jc
# foc_resid_jc. py_idx maps each plan-cell to its plan-year row.
PY_KEYS <- sort(unique(unlist(lapply(foc_cells, function(fc) paste(fc$plan_ids, fc$year, sep = "|")))))
N_PY <- length(PY_KEYS)
for (k in seq_along(foc_cells)) {
  fc <- foc_cells[[k]]; pn <- fc$plan_ids
  sr_cell <- supply_results %>% filter(region == fc$region, year == fc$year)
  g <- sr_cell$region_factor[match(pn, sr_cell$plan_id)]
  if (anyNA(g)) stop("s4: regional factor missing for a plan in cell ", fc$region, " ", fc$year)
  foc_cells[[k]]$g      <- setNames(g, pn)
  foc_cells[[k]]$w_py   <- fc$N * g                                  # weight of the cell in the plan-year FOC
  foc_cells[[k]]$py_idx <- match(paste(pn, fc$year, sep = "|"), PY_KEYS)
}
FOC_YEARS <- sort(unique(sapply(foc_cells, function(fc) fc$year)))
# Statewide sums that do not depend on theta, by year
A_state  <- sapply(FOC_YEARS, function(y) sum(sapply(foc_cells, function(fc) if (fc$year == y) fc$A_own else 0)))
M_state  <- sapply(FOC_YEARS, function(y) sum(sapply(foc_cells, function(fc) if (fc$year == y) fc$M_own else 0)))
PM_state <- sapply(FOC_YEARS, function(y) sum(sapply(foc_cells, function(fc) if (fc$year == y) fc$PM_own else 0)))
names(A_state) <- names(M_state) <- names(PM_state) <- FOC_YEARS
PBAR_state <- setNames(vapply(FOC_YEARS, ra_pbar_cms, numeric(1)), FOC_YEARS)   # CMS statewide average premium
miss <- is.na(PBAR_state)
PBAR_state[miss] <- (PM_state / M_state * (1 - RA_ADMIN_SHARE[as.character(FOC_YEARS)]))[miss]
cat("  Statewide average premium (net of the admin share) by year:",
    paste(FOC_YEARS, round(PBAR_state), sep = ": ", collapse = ", "), "\n")
# ra_env for cell k at predicted risk scores pred_rs, given the year's R total
ra_env_cell <- function(fc, pred_rs, R_state_y) {
  R_own <- fc$N * sum(unname(fc$shares[fc$plan_ids]) * pred_rs * fc$x_mult, na.rm = TRUE)
  y <- as.character(fc$year)
  list(gcf = fc$gcf, N = fc$N, pbar = PBAR_state[[y]], arf = fc$arf,
       rest = list(R = R_state_y - R_own, A = A_state[[y]] - fc$A_own, M = M_state[[y]] - fc$M_own))
}

rm(supply_results, demo_all, pred_py)

# Count the plan-cell FOC pieces and the plan-year conditions they aggregate to
n_foc_total <- sum(sapply(foc_cells, function(fc) length(fc$plan_ids)))
cat("  Plan-cell FOC pieces:", n_foc_total, " in", N_PY, "plan-year conditions\n")
n_foc_below <- sum(sapply(foc_cells, function(fc) sum(fc$shares < SHARE_FLOOR_FOC)))
cat("  Below share floor", SHARE_FLOOR_FOC, "(dropped from the plan-year sums):",
    n_foc_below, "of", n_foc_total, "\n")

# M3 instruments: plan characteristics at the cell level (HMO varies across
# regions within some plan-years), aggregated with the residual to the plan-year

# The insurer commission condition MB_fy = (1 - beta) MC_fy per insurer-year,
# evaluated at the observed schedules as a diagnostic. beta is the
# administrative saving per commission dollar (a broker enrollee's onboarding
# and servicing the agent takes over), fixed at the per-carrier rates from the
# national MLR filings (step 9).

# Per-cell insurer structure for the commission conditions (theta-independent pieces)
comm_struct <- lapply(foc_cells, function(fc) {
  if (is.null(fc$comm_D) || is.null(fc$comm_qB)) return(NULL)
  pn <- fc$plan_ids; pref <- sub("_.*", "", pn)
  firms <- unique(pref[fc$comm_vec > 0])
  if (length(firms) == 0) return(NULL)
  lapply(firms, function(f) {
    ii <- which(pref == f)
    w_f <- numeric(length(pn)); w_f[ii] <- fc$comm_vec[ii]
    d <- as.data.frame(fc$demo_shares); m <- match(pn[ii], d$plan_id)
    list(firm = f, ii = ii, dq = as.numeric(fc$comm_D %*% w_f),
         MC = sum(fc$comm_qB[ii] * fc$comm_vec[ii]), qB = sum(fc$comm_qB[ii]),
         dem = sum(d$demand[m], na.rm = TRUE))      # predicted enrollment of the insurer's plans
  })
})
# --- The commission block's parameter: per-carrier substitution rates ---
# beta_f from data-build step 9, constant within carrier across years. The
# commission conditions are evaluated at these fixed values as diagnostics.
YEARS_FOC <- sort(unique(vapply(foc_cells, function(fc) fc$year, numeric(1))))
BETA_FY <- unlist(lapply(YEARS_FOC, function(y)
  setNames(beta_carrier$beta, paste(beta_carrier$insurer_prefix, y, sep = "_"))))
BETA_FY_DEFAULT <- BETA0
COMM_KEYS <- sort(unique(unlist(lapply(seq_along(foc_cells), function(ci) {
  cs <- comm_struct[[ci]]; if (is.null(cs)) return(NULL)
  paste(vapply(cs, function(x) x$firm, character(1)), foc_cells[[ci]]$year, sep = "_") }))))
# (1 - beta) for each plan of a cell, by the plan's insurer-year
cs_of <- function(fc) {
  b <- BETA_FY[paste(sub("_.*", "", fc$plan_ids), fc$year, sep = "_")]
  b[is.na(b)] <- BETA_FY_DEFAULT
  1 - unname(b)
}

# M3: one moment per plan-year with at least one plan-cell above the share floor
PY_OK <- rep(FALSE, N_PY)
for (fc in foc_cells) PY_OK[fc$py_idx[fc$shares >= SHARE_FLOOR_FOC]] <- TRUE
N_M3 <- sum(PY_OK)
# The GMM moments are the claims equation (M2) and the plan-year pricing
# conditions (M3). The commission conditions are evaluated at the fixed
# per-carrier beta as diagnostics.
N_MOMENTS <- ncol(Z_cl) + N_M3
IDX_M2 <- seq_len(ncol(Z_cl))
IDX_M3 <- ncol(Z_cl) + seq_len(N_M3)
cat("  Total moment conditions:", N_MOMENTS, "(M2:", ncol(Z_cl), " M3:", N_M3, ")\n")

# =========================================================================
# PARAMETER LAYOUT
# =========================================================================

gamma_names <- c("(Intercept)", "log_risk_score", CLAIMS_EXOG_TERMS)
N_ALPHA <- 0L                    # the risk-score coefficients are fixed (ALPHA_FIXED)
N_GAMMA <- length(gamma_names)   # intercept, log_risk_score, exogenous claims terms
# beta, the administrative saving per commission dollar, is held at its current
# stage-4 value while the cost coefficients are estimated (BETA_FY; the MLR
# within-insurer estimate at the start)

# Starting values: the claims OLS from s3
cl_coefs_start <- read_csv(file.path(TEMP_DIR, "ra_claims_coefs.csv"), show_col_types = FALSE)
gamma0_raw <- setNames(cl_coefs_start$estimate, cl_coefs_start$term)[gamma_names]
gamma0_raw[is.na(gamma0_raw)] <- 0
gamma0 <- unname(gamma0_raw)
theta0 <- gamma0
cat("  Starting values (OLS):\n")
cat("    gamma:", round(gamma0, 4), "\n")
cat("    beta (pooled MLR, default for unmatched carriers):", round(BETA0, 4), "\n")

# =========================================================================
# MOMENT FUNCTION
# =========================================================================

# Computes g_bar(theta): the N_MOMENTS-vector of averaged moment conditions.
#
# The risk-score coefficients (alpha) are fixed at the SRRT OLS above.
# M2: E[W_cl' * (cl_obs - exp(gamma'W))] = 0  (claims, in levels: fits the mean
#     claims per member-month; a log residual understates the mean where claims
#     are dispersed, which is the platinum plans)
# M3: one moment per plan-year, the marginal cost the observed price implies
#     minus the model's, in dollars per member-month (the plan-year FOC residual
#     divided by its own-price term)
# The commission condition MB_ft = (1 - beta_f) MC_ft is evaluated per
# insurer-year as a diagnostic, not a moment.
#
# FOC residual for plan j in cell c:
#   foc_j = s_j + sum_k Omega_{jk} * (p_k - MC_k(alpha,gamma)) + sum_k Omega_broker_{jk} * comm_k
# which should equal zero at the true parameters.

# return_contributions = TRUE additionally returns the per-observation moment
# contributions (the matrices whose column means make up g_bar): the M2 rows
# and the M3 plan-year rows, plus the plan-year FOC residuals and the
# commission diagnostics. The default averaged return is unchanged, so the GMM objective
# and the SE sandwich share one code path (cost_gmm_sandwich_se).
compute_g_bar <- function(theta, return_contributions = FALSE) {

  alpha <- ALPHA_FIXED
  gamma <- theta[1:N_GAMMA]

  # --- M2: Claims residuals (PUF rows), level residual in units of the mean
  #     filed claims so the block is O(1) ---
  pred_log_cl_rf <- as.vector(W_cl %*% gamma)
  eps_cl <- as.vector(exp(y_cl) - exp(pred_log_cl_rf)) / CLAIMS_SCALE * w_rf
  M2_mat <- Z_cl * eps_cl           # n_rf x ncol(Z_cl)
  g_cl <- colMeans(M2_mat)

  # --- M3: plan-year pricing FOC, accumulated from the per-cell residuals ---
  # M3_num[jy] = sum_c w_jc foc_resid_jc, ow_py[jy] = sum_c w_jc Omega_jj,c with
  # w_jc = N_c g_jc; the plan-year moment is M3_num / ow_py, the residual in
  # dollars per member-month (implied minus model marginal cost).
  M3_num <- numeric(N_PY)
  w_py  <- numeric(N_PY)
  ow_py <- numeric(N_PY)                      # sum_c w_jc Omega_jj,c (dollar scale of the residual)
  cp_row <- integer(0); cp_py <- integer(0); cp_val <- numeric(0)   # plan-cell pieces (for the covariance)
  MB_fy <- numeric(0); MC_fy <- numeric(0)   # M4 accumulators keyed firm_year
  qB_fy <- numeric(0)
  cell_mc <- vector("list", length(foc_cells)); cell_env <- vector("list", length(foc_cells))

  # Pass A: predicted risk scores and claims per cell, and the statewide
  # risk-weighted sum by year (the one statewide piece that moves with theta)
  cell_rs <- vector("list", length(foc_cells)); cell_cl <- vector("list", length(foc_cells))
  R_state <- setNames(rep(0, length(FOC_YEARS)), FOC_YEARS)
  for (ci in seq_along(foc_cells)) {
    fc <- foc_cells[[ci]]
    pred_log_rs <- alpha[1] + as.vector(fc$X_rs %*% alpha[-1])
    pred_log_cl <- gamma[1] + gamma[2] * pred_log_rs + as.vector(fc$X_cl %*% gamma[3:N_GAMMA])
    cell_rs[[ci]] <- exp(pred_log_rs); cell_cl[[ci]] <- exp(pred_log_cl)
    y <- as.character(fc$year)
    R_state[[y]] <- R_state[[y]] + fc$N * sum(unname(fc$shares[fc$plan_ids]) * cell_rs[[ci]] * fc$x_mult, na.rm = TRUE)
  }

  # Pass B: transfers, marginal costs, FOC residuals, and the commission FOC pieces
  for (ci in seq_along(foc_cells)) {
    fc <- foc_cells[[ci]]
    J <- length(fc$plan_ids)
    pred_rs <- cell_rs[[ci]]; pred_claims <- cell_cl[[ci]]

    # RA transfers under the statewide formula (ra.R); the cell's own share of the
    # statewide sums moves with theta, the rest of the state is held at its value
    ra_env <- ra_env_cell(fc, pred_rs, R_state[[as.character(fc$year)]])
    ra <- unname(compute_ra_transfers(setNames(pred_rs, fc$plan_ids), fc$shares, ra_env, fc$plan_avs)[fc$plan_ids])

    # MC(alpha, gamma): claims net of reinsurance and the transfer, plus the
    # insurer's administrative cost per member
    mc <- pred_claims * (1 - fc$reins_vec) - ra + fc$admin

    # FOC residual: s + ra_foc - Omega (p - mc) + (1 - beta) Omega_broker comm.
    # A commission dollar on a broker enrollee costs the insurer (1 - beta) net of
    # the administrative work the agent takes over. ra_foc is recomputed at the
    # current cost parameters (it depends on pred_rs, which moves with theta).
    ra_foc_cell <- if (!is.null(fc$elast_mat) && !is.null(fc$own_mat)) {
      compute_ra_foc(setNames(pred_rs, fc$plan_ids), fc$shares, fc$plan_avs,
                     ra_env, fc$elast_mat, fc$own_mat)
    } else if (!is.null(fc$ra_foc)) fc$ra_foc else rep(0, J)
    foc_resid <- fc$shares + ra_foc_cell -
                 as.vector(fc$Omega %*% (fc$posted_premium - mc)) +
                 as.vector(fc$Omega_broker %*% (cs_of(fc) * fc$comm_vec))

    # Kept for the commission pieces (pass C), which use the plan-year pricing
    # residual of this pass in the margin
    cell_mc[[ci]] <- mc; cell_env[[ci]] <- ra_env

    # Share floor: a plan with near-zero share in a cell has an uninformative
    # piece there, so it is left out of that plan-year's sum. It stays in the
    # cell's Omega, so its cross-price effects on the other plans are kept.
    keep <- fc$shares >= SHARE_FLOOR_FOC
    if (any(keep)) {
      idx <- fc$py_idx[keep]
      w   <- fc$w_py[keep]
      M3_num[idx] <- M3_num[idx] + w * foc_resid[keep]
      w_py[idx]  <- w_py[idx] + w
      ow_py[idx] <- ow_py[idx] + w * diag(fc$Omega)[keep]
      cp_row <- c(cp_row, length(cp_row) + seq_along(idx)); cp_py <- c(cp_py, idx); cp_val <- c(cp_val, w * foc_resid[keep])
    }
  }

  # Plan-year pricing residual in dollars: the marginal cost the observed price
  # implies minus the model's (positive when the model's cost sits below what
  # pricing implies). One moment per plan-year.
  e_dollars_py <- numeric(N_PY)
  e_dollars_py[PY_OK] <- M3_num[PY_OK] / ow_py[PY_OK]
  g_foc <- e_dollars_py[PY_OK]
  # Plan-cell pieces of each plan-year moment, scaled so the column means
  # reproduce g_foc (the covariance treats the plan-cells as the observations)
  M3_mat <- matrix(0, length(cp_row), N_M3)
  M3_mat[cbind(cp_row, match(cp_py, which(PY_OK)))] <- length(cp_row) * cp_val / ow_py[cp_py]

  # Pass C: commission pieces at the current cost parameters. MB_f = sum over the
  # insurer's plans of the model margin x d qB / d k plus the risk-adjustment
  # response; MC_f = commission outlay. Each is in the cell's share units, so the
  # insurer-year sums across cells weight by the cell's members.
  for (ci in seq_along(foc_cells)) {
    cs <- comm_struct[[ci]]
    if (is.null(cs)) next
    fc <- foc_cells[[ci]]
    ra_eta <- compute_ra_foc(setNames(cell_rs[[ci]], fc$plan_ids), fc$shares, fc$plan_avs,
                             cell_env[[ci]], fc$comm_D, fc$own_mat)
    margin <- fc$posted_premium - cell_mc[[ci]] - cs_of(fc) * fc$comm_vec
    for (cf_ in cs) {
      key <- paste(cf_$firm, fc$year, sep = "_")
      MBf <- sum(margin[cf_$ii] * cf_$dq[cf_$ii]) + sum(fc$comm_vec[cf_$ii] * ra_eta[cf_$ii])
      MB_fy[key] <- (if (is.na(MB_fy[key])) 0 else MB_fy[key]) + fc$N * MBf
      MC_fy[key] <- (if (is.na(MC_fy[key])) 0 else MC_fy[key]) + fc$N * cf_$MC
      qB_fy[key] <- (if (is.na(qB_fy[key])) 0 else qB_fy[key]) + fc$N * cf_$qB
    }
  }

  # Commission-condition diagnostic at the fixed beta: r4 = MB/MC - 1 and the
  # residual phi = r4 + beta_f of the condition MB = (1 - beta) MC.
  keys <- names(MB_fy)
  ok4 <- is.finite(MB_fy) & is.finite(MC_fy) & MC_fy > 0 & qB_fy > 0
  keys4 <- keys[ok4]
  r4 <- MB_fy[ok4] / MC_fy[ok4] - 1
  cbar <- MC_fy[ok4] / qB_fy[ok4]
  # A parameter step that makes every MB non-finite leaves no commission rows;
  # return a large moment vector so the optimizer rejects the step.
  if (length(keys4) == 0) return(rep(1e3, N_MOMENTS))
  b4 <- BETA_FY[keys4]; b4[is.na(b4)] <- BETA_FY_DEFAULT
  phi4 <- r4 + unname(b4)

  g <- c(g_cl, g_foc)
  if (!return_contributions) return(g)
  list(g       = g,
       ra_state = data.frame(year = FOC_YEARS, R = as.numeric(R_state[as.character(FOC_YEARS)]),
                             A = as.numeric(A_state), M = as.numeric(M_state), PM = as.numeric(PM_state),
                             pbar = as.numeric(PBAR_state)),
       ra_own   = data.frame(region = sapply(foc_cells, function(fc) fc$region),
                             year = sapply(foc_cells, function(fc) fc$year),
                             R = sapply(seq_along(foc_cells), function(ci) {
                               fc <- foc_cells[[ci]]
                               fc$N * sum(unname(fc$shares[fc$plan_ids]) * cell_rs[[ci]] * fc$x_mult, na.rm = TRUE) }),
                             A = sapply(foc_cells, function(fc) fc$A_own),
                             M = sapply(foc_cells, function(fc) fc$M_own)),
       # Moment blocks for the covariance: one row per observation of each block
       # (PUF rows and plan-years); blocks are treated as independent
       blocks  = list(list(mat = M2_mat, n = nrow(M2_mat)),
                      list(mat = M3_mat, n = nrow(M3_mat))),
       # Plan-year FOC residuals: per member (share units) and in dollars
       foc_py  = data.frame(key = PY_KEYS[PY_OK], G_per_member = M3_num[PY_OK] / w_py[PY_OK],
                            G_dollars = e_dollars_py[PY_OK],
                            stringsAsFactors = FALSE),
       n_foc   = N_M3,
       n_comm  = length(r4),
       comm_fy = data.frame(key = keys4, MB = MB_fy[ok4], MC = MC_fy[ok4],
                            mu_hat = r4, comm_bar = cbar, phi = phi4, stringsAsFactors = FALSE))
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
  cat("  M2:", round(g_init[IDX_M2], 4), "\n")
  cat("  M3:", round(g_init[IDX_M3], 4), "\n")
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

gamma1 <- result1$par[1:N_GAMMA]

cat("  gamma (Step 1):", round(gamma1, 4), "\n")

g1 <- compute_g_bar(result1$par)
cat("  g_bar at Step 1:\n")
cat("    M2 (claims):", round(g1[IDX_M2], 4), "\n")
cat("    M3 (FOC, $ per member-month): mean", round(mean(g1[IDX_M3]), 2),
    " RMS", round(sqrt(mean(g1[IDX_M3]^2)), 2), "\n")

# =========================================================================
# STEP 2: OPTIMAL WEIGHTING
# =========================================================================

cat("\n--- GMM Step 2 (optimal weighting: inverse moment covariance) ---\n")

# Efficient two-step feasible GMM. The step-2 weight is the
# inverse of the moment variance-covariance matrix S, estimated at the step-1
# parameters. S is block-diagonal across the independent data sources: the
# claims moments from the rate-filing plan-years and the pricing moments from
# the plan-years, so the cross-block covariance is zero. This is the same S the sandwich uses for its meat, so
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
# The blocks are on different scales (the pricing moments in dollars, the rest
# unit-free), so the inverse is taken on the correlation form, S normalized by
# its diagonal, and mapped back; a ridge, if needed, is then relative to each
# moment's own variance rather than to the largest block's.
d_S <- sqrt(diag(S))
S_corr <- S / tcrossprod(d_S)
S_rcond <- rcond(S_corr)
cat("  Moment-correlation rcond:", format(S_rcond, digits = 3),
    " (small => ill-conditioned; consider bootstrap)\n")
if (is.na(S_rcond) || S_rcond < 1e-12) {
  cat("  S ill-conditioned; ridge-regularizing the correlation diagonal by 1e-6\n")
  S_corr_inv <- solve(S_corr + diag(1e-6, N_MOMENTS))
} else {
  S_corr_inv <- solve(S_corr)
}
W2 <- S_corr_inv / tcrossprod(d_S)

result2 <- optim(
  par = result1$par,
  fn = gmm_objective,
  W = W2,
  method = "BFGS",
  control = list(maxit = 2000, reltol = 1e-12, trace = 1, REPORT = 100)
)

cat("  Converged:", result2$convergence == 0, "\n")
cat("  Objective:", format(result2$value, digits = 6), "\n")

alpha_gmm <- ALPHA_FIXED
gamma_gmm <- result2$par[1:N_GAMMA]

cat("\n  alpha (fixed OLS):", round(alpha_gmm, 4), "\n")
cat("  gamma (GMM):", round(gamma_gmm, 4), "\n")

g2 <- compute_g_bar(result2$par)
cat("  g_bar at Step 2:\n")
cat("    M2 (claims):", round(g2[IDX_M2], 4), "\n")
cat("    M3 (FOC, $ per member-month): mean", round(mean(g2[IDX_M3]), 2),
    " RMS", round(sqrt(mean(g2[IDX_M3]^2)), 2), "\n")

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
cat("    OLS g_foc: mean", round(mean(g_ols[IDX_M3]), 2), " RMS", round(sqrt(mean(g_ols[IDX_M3]^2)), 2), "\n")
cat("    GMM g_foc: mean", round(mean(g2[IDX_M3]), 2), " RMS", round(sqrt(mean(g2[IDX_M3]^2)), 2), "\n")

# Parameter comparison
cat("\n  Parameter comparison (OLS → GMM):\n")
comp <- data.frame(
  param = gamma_names,
  equation = rep("claims", N_GAMMA),
  OLS = round(theta0, 4),
  GMM = round(result2$par, 4),
  change = round(result2$par - theta0, 4)
)
print(comp, row.names = FALSE)

# Negative-MC check at the GMM solution, by metal. With AV out of the claims
# equation, the risk-score pass-through should rise toward one and predicted
# claims for high-metal cells should climb, so the negatives should shrink.
cat("\n  Negative MC at GMM solution (by metal):\n")
contr2 <- compute_g_bar(result2$par, return_contributions = TRUE)
R_state_gmm <- setNames(contr2$ra_state$R, contr2$ra_state$year)
mc_rows <- lapply(foc_cells, function(fc) {
  plr <- alpha_gmm[1] + as.vector(fc$X_rs %*% alpha_gmm[-1])
  pcl <- gamma_gmm[1] + gamma_gmm[2]*plr + as.vector(fc$X_cl %*% gamma_gmm[3:N_GAMMA])
  prs <- exp(plr); pclm <- exp(pcl)
  ra_env <- ra_env_cell(fc, prs, R_state_gmm[[as.character(fc$year)]])
  ra <- unname(compute_ra_transfers(setNames(prs, fc$plan_ids), fc$shares, ra_env, fc$plan_avs)[fc$plan_ids])
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
    " composition shares:", round(alpha_gmm[-(1:4)], 3), "\n")

# =========================================================================
# COMMISSION DIAGNOSTIC: condition residuals at the GMM solution
# =========================================================================
# Per insurer-year: mu_hat = MB/MC - 1 (the steering return net of the outlay),
# the observed commission per broker enrollee (comm_bar), and the residual of the
# condition MB = (1 - beta) MC (phi = mu_hat + beta). beta is written for the
# counterfactual.

cat("\n--- Commission conditions MB = (1 - beta) MC at the GMM solution ---\n")
contr2 <- compute_g_bar(result2$par, return_contributions = TRUE)
comm_fy <- contr2$comm_fy %>%
  tidyr::separate(key, into = c("firm", "year"), sep = "_", convert = TRUE)
cat("  insurer-year conditions:", nrow(comm_fy),
    " | distinct insurers:", n_distinct(comm_fy$firm), "\n")
beta_fy_df <- tibble(key = names(BETA_FY), beta = unname(BETA_FY)) %>%
  tidyr::separate(key, into = c("firm", "year"), sep = "_", convert = TRUE) %>%
  filter(paste(firm, year, sep = "_") %in% COMM_KEYS)
cat("  beta by insurer (mean over years):\n")
print(beta_fy_df %>% group_by(firm) %>% summarise(beta = round(mean(beta), 3), .groups = "drop"), n = Inf)
cat("  mu_hat = MB/MC - 1: mean", round(mean(comm_fy$mu_hat), 3),
    " sd", round(sd(comm_fy$mu_hat), 3), " | residual phi: mean", round(mean(comm_fy$phi), 3),
    " sd", round(sd(comm_fy$phi), 3), "\n")
print(comm_fy %>% group_by(firm) %>%
        summarise(n = n(), MB_MC = round(mean(mu_hat) + 1, 2), comm_bar = round(mean(comm_bar), 2),
                  phi = round(mean(phi), 3), .groups = "drop"), n = Inf)

write_csv(tibble(key = names(BETA_FY), beta = unname(BETA_FY)) %>%
            tidyr::separate(key, into = c("firm", "year"), sep = "_", convert = TRUE),
          file.path(TEMP_DIR, "commission_beta.csv"))
write_csv(comm_fy %>% select(firm, year, MB, MC, mu_hat, comm_bar, phi),
          file.path(TEMP_DIR, "commission_foc_fit.csv"))
cat("  Saved commission_beta.csv and commission_foc_fit.csv\n")

# Plan-year pricing FOC residuals at the GMM solution, in dollars per member-month
# (the counterfactual holds these fixed; cf1 recomputes them in its own system)
foc_py_gmm <- contr2$foc_py %>%
  tidyr::separate(key, into = c("plan_id", "year"), sep = "[|]", convert = TRUE) %>%
  left_join(plan_metal_map, by = "plan_id") %>%
  mutate(insurer = sub("_.*", "", plan_id))
cat("\n--- M3: plan-year pricing FOC residuals at the GMM solution ($ per member-month) ---\n")
cat("  By metal:\n")
print(foc_py_gmm %>% group_by(metal) %>%
        summarise(plan_years = n(), residual = round(mean(G_dollars), 1), .groups = "drop"))
cat("  By insurer:\n")
print(foc_py_gmm %>% group_by(insurer) %>%
        summarise(plan_years = n(), residual = round(mean(G_dollars), 1), .groups = "drop") %>%
        arrange(residual), n = Inf)

# =========================================================================
# SAVE COEFFICIENTS
# =========================================================================

cat("\nSaving GMM cost coefficients...\n")

rs_coefs_gmm <- tibble(term = alpha_names, estimate = alpha_gmm)
cl_coefs_gmm <- tibble(term = gamma_names, estimate = gamma_gmm)

write_csv(rs_coefs_gmm, file.path(TEMP_DIR, "ra_rs_coefs_gmm.csv"))
write_csv(cl_coefs_gmm, file.path(TEMP_DIR, "ra_claims_coefs_gmm.csv"))

cat("  Saved GMM coefficients to", TEMP_DIR, "\n")
# Standard errors are computed in s5_se.R (reuses result2 / W2 / compute_g_bar).

cat("\nCost-side GMM complete.\n")
