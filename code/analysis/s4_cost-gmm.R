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
# Commission substitution: starting value of beta, the administrative saving per
# commission dollar (data-build step 9, within-insurer MLR relation); the
# insurers' administrative cost per member rides in the FOC cells (s3)
BETA0 <- read_csv("data/output/mlr_admin_beta.csv", show_col_types = FALSE)$beta0[1]
# M5: the MLR relation itself as a moment. Sales and G&A cost per member on
# commission outlay per member, both within insurer and year (two-way demeaned,
# member-month weights), so beta is identified by the filings as well as by the
# commission conditions.
mlr_admin <- read_csv("data/output/mlr_admin.csv", show_col_types = FALSE) %>%
  filter(!is.na(mm), is.finite(sales_ga_pmpm), is.finite(commission_pmpm))
mlr_dm <- fixest::demean(X = as.matrix(mlr_admin[, c("sales_ga_pmpm", "commission_pmpm")]),
                         f = mlr_admin[, c("insurer_prefix", "year")], weights = mlr_admin$mm)
a_mlr <- mlr_dm[, 1]; c_mlr <- mlr_dm[, 2]
w_mlr <- mlr_admin$mm / mean(mlr_admin$mm)
VAR_C_MLR <- sum(w_mlr * c_mlr^2) / sum(w_mlr)          # scales the moment to units of beta
cat("  MLR administrative-cost rows (M5):", nrow(mlr_admin), "\n")

# --- FOC inputs per cell (moment 3) ---
foc_files <- list.files(file.path(TEMP_DIR, "foc_inputs"),
                         pattern = "^foc_.*\\.rds$", full.names = TRUE)
if (length(foc_files) == 0) stop("No FOC input files found — run 2_pricing.R first")

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

# --- M1 data matrices (SRRT risk scores) ---
w_rs <- sqrt(rs_srrt$member_months)  # WLS weights
X_rs <- as.matrix(rs_srrt %>% select(Silver, Gold, Platinum, all_of(RS_DEMO_TERMS)))
y_rs <- rs_srrt$log_risk_score
Z_rs <- cbind(1, X_rs)               # M1 instruments: intercept + regressors

# --- M2 data matrices (rate filing PUF claims) ---
w_rf <- sqrt(rsdata$EXP_MM)          # WLS weights
# Regressors of the predicted risk score for the claims rows (same terms as M1,
# at the plan-year predicted shares)
X_rs_cl <- as.matrix(rsdata %>% select(Silver, Gold, Platinum, all_of(RS_DEMO_TERMS)))
# Claims equation exogenous part (CLAIMS_EXOG_TERMS): HMO, trend, big-four
# insurer indicators, rating-area shares. AV is OMITTED, since the risk score
# carries generosity.
X_cl_exog <- as.matrix(rsdata %>% select(all_of(CLAIMS_EXOG_TERMS)))
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
N_Z_FOC <- 6L + N_INS_COST  # intercept, Silver, Gold, Platinum, HMO, trend + INS_COST

# M4: the insurer commission condition MB_fy = (1 - beta) MC_fy per insurer-year,
# evaluated at the observed schedules and interacted with insurer indicators.
# beta is the administrative saving per commission dollar (a broker enrollee's
# onboarding and servicing the agent takes over), estimated with the cost
# parameters; its starting value is the within-insurer MLR relation (step 9).
# N_Z_COMM is set below once the insurer set is known from comm_struct.

# Per-cell insurer structure for M4 (theta-independent pieces)
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
COMM_FIRMS <- sort(unique(unlist(lapply(comm_struct, function(cs)
  vapply(cs, function(x) x$firm, character(1))))))
N_BETA   <- 1L
N_Z_COMM <- length(COMM_FIRMS)                     # insurer indicators
N_Z_MLR  <- 1L                                     # the MLR administrative-cost relation
N_MOMENTS <- ncol(Z_rs) + ncol(Z_cl) + N_Z_FOC + N_Z_COMM + N_Z_MLR
IDX_M1 <- seq_len(ncol(Z_rs))
IDX_M2 <- ncol(Z_rs) + seq_len(ncol(Z_cl))
IDX_M3 <- ncol(Z_rs) + ncol(Z_cl) + seq_len(N_Z_FOC)
IDX_M4 <- ncol(Z_rs) + ncol(Z_cl) + N_Z_FOC + seq_len(N_Z_COMM)
IDX_M5 <- N_MOMENTS
cat("  Total moment conditions:", N_MOMENTS, "(M1:", ncol(Z_rs),
    " M2:", ncol(Z_cl), " M3:", N_Z_FOC, " M4:", N_Z_COMM, " M5:", N_Z_MLR, ")\n")

# =========================================================================
# PARAMETER LAYOUT
# =========================================================================

alpha_names <- c("(Intercept)", "Silver", "Gold", "Platinum", RS_DEMO_TERMS)
gamma_names <- c("(Intercept)", "log_risk_score", CLAIMS_EXOG_TERMS)
N_ALPHA <- length(alpha_names)   # intercept, 3 metal dummies, demographic shares
N_GAMMA <- length(gamma_names)   # intercept, log_risk_score, exogenous claims terms

# Starting values from OLS
rs_coefs_start <- read_csv(file.path(TEMP_DIR, "ra_rs_coefs.csv"), show_col_types = FALSE)
cl_coefs_start <- read_csv(file.path(TEMP_DIR, "ra_claims_coefs.csv"), show_col_types = FALSE)

alpha0_raw <- setNames(rs_coefs_start$estimate, rs_coefs_start$term)[alpha_names]
alpha0_raw[is.na(alpha0_raw)] <- 0  # any aliased insurer FE -> baseline
alpha0 <- unname(alpha0_raw)
gamma0_raw <- setNames(cl_coefs_start$estimate, cl_coefs_start$term)[gamma_names]
gamma0_raw[is.na(gamma0_raw)] <- 0
gamma0 <- unname(gamma0_raw)

# Commission substitution start: the within-insurer MLR relation (step 9)
beta0 <- BETA0
theta0 <- c(alpha0, gamma0, beta0)
cat("  Starting values (OLS):\n")
cat("    alpha:", round(alpha0, 4), "\n")
cat("    gamma:", round(gamma0, 4), "\n")
cat("    beta:", round(beta0, 4), "\n")

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
# contributions (the matrices whose column means make up g_bar): the M1 and M2
# rows, the M3 plan-year rows, and the M4 insurer-year rows, plus the plan-year
# FOC residuals. The default averaged return is unchanged, so the GMM objective
# and the SE sandwich share one code path (cost_gmm_sandwich_se).
compute_g_bar <- function(theta, return_contributions = FALSE) {

  alpha <- theta[1:N_ALPHA]
  gamma <- theta[(N_ALPHA + 1):(N_ALPHA + N_GAMMA)]
  beta <- theta[N_ALPHA + N_GAMMA + 1]

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

  # --- M3: plan-year pricing FOC, accumulated from the per-cell residuals ---
  # M3_py[jy, ] = sum_c Z_jc w_jc foc_resid_jc, w_py[jy] = sum_c w_jc, with
  # w_jc = N_c g_jc; the moment is the mean over plan-years of M3_py / w_py.
  M3_py <- matrix(0, N_PY, N_Z_FOC)
  w_py  <- numeric(N_PY)
  ow_py <- numeric(N_PY)                      # sum_c w_jc Omega_jj,c (dollar scale of the residual)
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
                 (1 - beta) * as.vector(fc$Omega_broker %*% fc$comm_vec)

    # Kept for the commission pieces (pass C), which use the plan-year pricing
    # residual of this pass in the margin
    cell_mc[[ci]] <- mc; cell_env[[ci]] <- ra_env

    # Instruments for this cell: intercept + plan characteristics + insurer dummies
    Z_cell <- cbind(1, fc$Silver, fc$Gold, fc$Platinum, fc$HMO, fc$trend,
                    sapply(INS_COST, function(ins) fc[[ins]]))

    # Share floor: a plan with near-zero share in a cell has an uninformative
    # piece there, so it is left out of that plan-year's sum. It stays in the
    # cell's Omega, so its cross-price effects on the other plans are kept.
    keep <- fc$shares >= SHARE_FLOOR_FOC
    if (any(keep)) {
      idx <- fc$py_idx[keep]
      w   <- fc$w_py[keep]
      M3_py[idx, ] <- M3_py[idx, , drop = FALSE] + Z_cell[keep, , drop = FALSE] * (w * foc_resid[keep])
      w_py[idx]  <- w_py[idx] + w
      ow_py[idx] <- ow_py[idx] + w * diag(fc$Omega)[keep]
    }
  }

  py_ok <- w_py > 0
  M3_py <- M3_py[py_ok, , drop = FALSE] / w_py[py_ok]   # per-member share units
  g_foc <- colMeans(M3_py)                                # average across plan-years
  # Plan-year pricing residual in dollars: the margin the insurer's pricing implies
  # exceeds the model's margin by this amount (positive when the structural cost
  # sits above what pricing implies). It enters the commission benefit below so
  # both conditions see the same margin.
  e_dollars_py <- numeric(N_PY)
  e_dollars_py[py_ok] <- M3_py[, 1] / (ow_py[py_ok] / w_py[py_ok])

  # Pass C: commission pieces at the current cost parameters. MB_f = sum over the
  # insurer's plans of the pricing-consistent margin x d qB / d k plus the
  # risk-adjustment response; MC_f = commission outlay. Each is in the cell's
  # share units, so the insurer-year sums across cells weight by the cell's members.
  for (ci in seq_along(foc_cells)) {
    cs <- comm_struct[[ci]]
    if (is.null(cs)) next
    fc <- foc_cells[[ci]]
    ra_eta <- compute_ra_foc(setNames(cell_rs[[ci]], fc$plan_ids), fc$shares, fc$plan_avs,
                             cell_env[[ci]], fc$comm_D, fc$own_mat)
    margin <- fc$posted_premium - cell_mc[[ci]] - (1 - beta) * fc$comm_vec + e_dollars_py[fc$py_idx]
    for (cf_ in cs) {
      key <- paste(cf_$firm, fc$year, sep = "_")
      MBf <- sum(margin[cf_$ii] * cf_$dq[cf_$ii]) + sum(fc$comm_vec[cf_$ii] * ra_eta[cf_$ii])
      MB_fy[key] <- (if (is.na(MB_fy[key])) 0 else MB_fy[key]) + fc$N * MBf
      MC_fy[key] <- (if (is.na(MC_fy[key])) 0 else MC_fy[key]) + fc$N * cf_$MC
      qB_fy[key] <- (if (is.na(qB_fy[key])) 0 else qB_fy[key]) + fc$N * cf_$qB
    }
  }

  # M4: commission condition MB = (1 - beta) MC per insurer-year at the observed
  # schedules: phi = MB/MC - (1 - beta), interacted with insurer indicators.
  # r4 = MB/MC - 1 is kept for the diagnostic.
  keys <- names(MB_fy)
  ok4 <- is.finite(MB_fy) & is.finite(MC_fy) & MC_fy > 0 & qB_fy > 0
  keys4 <- keys[ok4]
  firm4 <- sub("_.*", "", keys4)
  r4 <- MB_fy[ok4] / MC_fy[ok4] - 1
  cbar <- MC_fy[ok4] / qB_fy[ok4]
  # A parameter step that makes every MB non-finite leaves no M4 rows; return a
  # large moment vector so the optimizer rejects the step.
  if (length(keys4) == 0) return(rep(1e3, N_MOMENTS))
  phi4 <- r4 + beta
  Z4 <- 1 * outer(firm4, COMM_FIRMS, "==")
  g_comm <- as.numeric(crossprod(Z4, phi4)) / length(phi4)

  # --- M5: the MLR administrative-cost relation, a_tilde = -beta c_tilde + e ---
  M5_mat <- matrix(w_mlr * c_mlr * (a_mlr + beta * c_mlr) / VAR_C_MLR, ncol = 1)
  g_mlr <- mean(M5_mat)

  g <- c(g_rs, g_cl, g_foc, g_comm, g_mlr)
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
       # (SRRT rows, PUF rows, plan-years, insurer-years); blocks are treated as
       # independent
       blocks  = list(list(mat = M1_mat, n = nrow(M1_mat)),
                      list(mat = M2_mat, n = nrow(M2_mat)),
                      list(mat = M3_py, n = nrow(M3_py)),
                      list(mat = Z4 * phi4, n = length(phi4)),
                      list(mat = M5_mat, n = nrow(M5_mat))),
       # Plan-year FOC residuals: per member (share units) and in dollars
       foc_py  = data.frame(key = PY_KEYS[py_ok], G_per_member = M3_py[, 1],
                            G_dollars = M3_py[, 1] / (ow_py[py_ok] / w_py[py_ok]),
                            stringsAsFactors = FALSE),
       n_foc   = nrow(M3_py),
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
  cat("  M1:", round(g_init[1:ncol(Z_rs)], 4), "\n")
  cat("  M2:", round(g_init[(ncol(Z_rs)+1):(ncol(Z_rs)+ncol(Z_cl))], 4), "\n")
  cat("  M3:", round(g_init[IDX_M3], 4), "\n")
  cat("  M4:", round(g_init[IDX_M4], 4), "\n")
  cat("  M5:", round(g_init[IDX_M5], 4), "\n")
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
beta1 <- result1$par[N_ALPHA + N_GAMMA + 1]

cat("  alpha (Step 1):", round(alpha1, 4), "\n")
cat("  gamma (Step 1):", round(gamma1, 4), "\n")
cat("  beta (Step 1):", round(beta1, 4), "\n")

g1 <- compute_g_bar(result1$par)
cat("  g_bar at Step 1:\n")
cat("    M1 (risk score):", round(g1[1:ncol(Z_rs)], 4), "\n")
cat("    M2 (claims):", round(g1[(ncol(Z_rs)+1):(ncol(Z_rs)+ncol(Z_cl))], 4), "\n")
cat("    M3 (FOC):", round(g1[IDX_M3], 6), "\n")
cat("    M4 (commission FOC):", round(g1[IDX_M4], 6), "\n")
cat("    M5 (MLR admin):", round(g1[IDX_M5], 6), "\n")

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
beta_gmm <- result2$par[N_ALPHA + N_GAMMA + 1]

cat("\n  alpha (GMM):", round(alpha_gmm, 4), "\n")
cat("  gamma (GMM):", round(gamma_gmm, 4), "\n")
cat("  beta (GMM):", round(beta_gmm, 4), "\n")

g2 <- compute_g_bar(result2$par)
cat("  g_bar at Step 2:\n")
cat("    M1 (risk score):", round(g2[1:ncol(Z_rs)], 4), "\n")
cat("    M2 (claims):", round(g2[(ncol(Z_rs)+1):(ncol(Z_rs)+ncol(Z_cl))], 4), "\n")
cat("    M3 (FOC):", round(g2[IDX_M3], 6), "\n")
cat("    M4 (commission FOC):", round(g2[IDX_M4], 6), "\n")
cat("    M5 (MLR admin):", round(g2[IDX_M5], 6), "\n")

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
cat("    OLS g_foc:", round(g_ols[IDX_M3], 6), "\n")
cat("    GMM g_foc:", round(g2[IDX_M3], 6), "\n")
cat("    OLS g_comm:", round(g_ols[IDX_M4], 6), "\n")
cat("    GMM g_comm:", round(g2[IDX_M4], 6), "\n")
cat("    OLS g_mlr:", round(g_ols[IDX_M5], 6), " GMM g_mlr:", round(g2[IDX_M5], 6), "\n")

# Parameter comparison
cat("\n  Parameter comparison (OLS → GMM):\n")
comp <- data.frame(
  param = c(alpha_names, gamma_names, "beta_admin"),
  equation = c(rep("risk_score", N_ALPHA), rep("claims", N_GAMMA), "commission"),
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
    " demographic shares:", round(alpha_gmm[5:7], 3), "
")

# =========================================================================
# M4 DIAGNOSTIC: commission condition residuals at the GMM solution
# =========================================================================
# Per insurer-year: mu_hat = MB/MC - 1 (the steering return net of the outlay),
# the observed commission per broker enrollee (comm_bar), and the residual of the
# condition MB = (1 - beta) MC (phi = mu_hat + beta). beta is written for the
# counterfactual.

cat("\n--- M4: commission condition MB = (1 - beta) MC at the GMM solution ---\n")
contr2 <- compute_g_bar(result2$par, return_contributions = TRUE)
comm_fy <- contr2$comm_fy %>%
  tidyr::separate(key, into = c("firm", "year"), sep = "_", convert = TRUE)
cat("  insurer-year conditions:", nrow(comm_fy),
    " | distinct insurers:", n_distinct(comm_fy$firm), "\n")
cat("  beta (administrative saving per commission dollar):", round(beta_gmm, 3),
    " | MLR starting value:", round(beta0, 3), "\n")
cat("  mu_hat = MB/MC - 1: mean", round(mean(comm_fy$mu_hat), 3),
    " sd", round(sd(comm_fy$mu_hat), 3), " | residual phi: mean", round(mean(comm_fy$phi), 3),
    " sd", round(sd(comm_fy$phi), 3), "\n")
print(comm_fy %>% group_by(firm) %>%
        summarise(n = n(), MB_MC = round(mean(mu_hat) + 1, 2), comm_bar = round(mean(comm_bar), 2),
                  phi = round(mean(phi), 3), .groups = "drop"), n = Inf)

write_csv(tibble(term = "beta_admin", estimate = beta_gmm), file.path(TEMP_DIR, "commission_beta.csv"))
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
# Statewide transfer sums and each cell's own contribution at the GMM solution
# (the counterfactual holds the rest of the state at these values)
write_csv(contr2$ra_state, file.path(TEMP_DIR, "ra_state_gmm.csv"))
write_csv(contr2$ra_own, file.path(TEMP_DIR, "ra_state_cells_gmm.csv"))
cat("  Saved ra_state_gmm.csv and ra_state_cells_gmm.csv\n")

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
