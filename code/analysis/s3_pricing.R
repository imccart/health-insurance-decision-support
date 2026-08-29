# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-06
## Date Edited:   2026-03-26
## Description:   Supply-side markup recovery (pure R).
##                Reads parquet partitions, computes nested logit shares and
##                elasticities, recovers markups via Bertrand FOC with
##                broker-commission correction. Also estimates structural RA
##                regressions and validates FOC-implied MC against predicted MC.

# Dependencies: preamble + s1_inputs.R (plan_choice, commission_lookup)
# loaded by _analysis.R before this step. Full spec (base + assisted) feeds the
# price-interaction machinery; read from what s2_demand wrote.
STRUCTURAL_SPEC <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))$all

# =========================================================================
# Load coefficients and reference data
# =========================================================================

cat("\nLoading demand coefficients and reference data...\n")

coefs <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
lambda <- coefs %>% filter(term == "lambda") %>% pull(estimate)
cat("  lambda =", round(lambda, 4), "\n")
cat("  Coefficients:", nrow(coefs), "terms\n")

# =========================================================================
# Estimate RA regressions from rate filing data
# =========================================================================

cat("\nEstimating RA regressions...\n")
# Claims rows: rate filing PUF plan-years with observed plan-year demographics
rsdata <- read_csv("data/output/rate_filing_rsdata.csv", show_col_types = FALSE)
plan_demo <- read_csv(file.path(TEMP_DIR, "plan_demographics.csv"), show_col_types = FALSE)
rsdata <- rsdata %>%
  left_join(plan_demo, by = c("plan_id", "year")) %>%
  left_join(plan_choice %>%
              select(plan_id, year, all_of(CLAIMS_REGION_TERMS)) %>%
              distinct(plan_id, year, .keep_all = TRUE),
            by = c("plan_id", "year"))
n_matched <- sum(!is.na(rsdata$share_18to34))
cat("  Demographics merged:", n_matched, "of", nrow(rsdata), "plan-years matched\n")
rm(plan_demo)

# Risk-score rows: SRRT scores at insurer x metal x region x year, with the
# observed enrollment demographics aggregated to that level
rs_srrt <- read_csv("data/output/plan_risk_scores.csv", show_col_types = FALSE)
# Insurers' non-commission administrative cost per member-month (MLR filings,
# data-build step 9) and the starting value of the commission substitution
# parameter beta (a commission dollar's administrative saving); both enter the
# pricing and commission conditions. s4 estimates beta.
mlr_admin <- read_csv("data/output/mlr_admin.csv", show_col_types = FALSE)
ADMIN_LOOKUP <- setNames(mlr_admin$admin0_pmpm, paste(mlr_admin$insurer_prefix, mlr_admin$year, sep = "_"))
BETA_ADMIN <- read_csv("data/output/mlr_admin_beta.csv", show_col_types = FALSE)$beta0[1]
cat("  administrative cost per member: ", length(ADMIN_LOOKUP), " insurer-years; beta0 =", round(BETA_ADMIN, 3), "\n")
plan_metal_map <- plan_choice %>%
  distinct(plan_id, metal) %>%
  mutate(metal = sub(" - Enhanced.*", "", metal)) %>%
  distinct(plan_id, metal)
pdr <- read_csv(file.path(TEMP_DIR, "plan_demographics_region.csv"), show_col_types = FALSE) %>%
  inner_join(plan_metal_map, by = "plan_id") %>%
  mutate(insurer_prefix = sub("_.*", "", plan_id)) %>%
  group_by(insurer_prefix, metal, region, year) %>%
  summarize(across(all_of(RS_DEMO_TERMS), ~ weighted.mean(.x, enrollment)),
            .groups = "drop")
rs_srrt <- rs_srrt %>%
  inner_join(pdr, by = c("insurer_prefix", "metal", "region", "year")) %>%
  mutate(Silver = as.integer(metal == "Silver"), Gold = as.integer(metal == "Gold"),
         Platinum = as.integer(metal == "Platinum"))
cat("  SRRT risk-score rows with demographics:", nrow(rs_srrt), "\n")
rm(pdr, plan_metal_map)

ra_regs <- estimate_ra_regressions(rsdata, rs_srrt)

# Save coefficients for counterfactual worker
rs_coefs_df <- tibble(term = names(ra_regs$rs_coefs), estimate = ra_regs$rs_coefs)
claims_coefs_df <- tibble(term = names(ra_regs$claims_coefs), estimate = ra_regs$claims_coefs)
write_csv(rs_coefs_df, file.path(TEMP_DIR, "ra_rs_coefs.csv"))
write_csv(claims_coefs_df, file.path(TEMP_DIR, "ra_claims_coefs.csv"))

# Reinsurance factors by plan-year (for counterfactuals)
reins_df <- rsdata %>%
  select(plan_id, year, reins_factor) %>%
  filter(!is.na(reins_factor))
write_csv(reins_df, file.path(TEMP_DIR, "reinsurance_factors.csv"))

cat("  RA coefficients and reinsurance factors saved.\n")
rm(rsdata, rs_srrt)

# =========================================================================
# Identify cells and set seeds (same as demand)
# =========================================================================

# cells, cell_seeds, plan_choice come from s1_inputs. Re-read hh_split from disk
# here (s2_demand freed its own copy) so pricing is self-contained.
hh_all <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split <- split(hh_all, by = c("region", "year"))
rm(hh_all)
cat("  Region-year cells:", nrow(cells), "\n")

# =========================================================================
# Loop over cells: build data, compute markups
# =========================================================================

cat("\nComputing supply-side markups...\n")

results_list <- vector("list", nrow(cells))
pass1 <- vector("list", nrow(cells))   # per-cell demand-side pieces for pass 2
n_done <- 0L
n_skip <- 0L

for (i in seq_len(nrow(cells))) {
  tryCatch({
  r <- cells$region[i]
  y <- cells$year[i]

  set.seed(cell_seeds[i])
  cell_key <- paste0(r, ".", y)
  hhs <- hh_split[[cell_key]]
  if (is.null(hhs) || nrow(hhs) == 0) { n_skip <- n_skip + 1L; next }
  hhs <- as.data.frame(hhs)

  plans <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans) == 0) { n_skip <- n_skip + 1L; rm(hhs); next }

  # Add commission PMPM if not already present
  if (!"comm_pmpm" %in% names(plans)) {
    comm_yr <- commission_lookup %>% filter(year == !!y) %>% select(-year)
    plans <- plans %>%
      mutate(insurer_prefix = sub("_.*", "", plan_id)) %>%
      left_join(comm_yr, by = "insurer_prefix") %>%
      mutate(comm_pmpm = case_when(is.na(rate) ~ 0, is_pct ~ rate * premium, TRUE ~ rate)) %>%
      select(-insurer_prefix, -rate, -is_pct)
    rm(comm_yr)
  }

  # Build supply choice data (same seed/sample as demand)
  build_result <- build_structural(plans, hhs, SAMPLE_FRAC, spec = STRUCTURAL_SPEC)
  rm(hhs)

  if (is.null(build_result)) { n_skip <- n_skip + 1L; rm(plans); next }
  cell_data  <- build_result$cell_data
  plan_attrs <- build_result$plan_attrs
  rm(build_result)

  # Channel steering terms (if not already present), matching build_structural.
  if (!"assisted_av" %in% names(cell_data)) {
    if ("any_agent" %in% names(cell_data)) {
      nb <- cell_data$assisted * ifelse(is.na(cell_data$any_agent) | cell_data$any_agent != 1L, 1L, 0L)
      br <- cell_data$assisted * ifelse(!is.na(cell_data$any_agent) & cell_data$any_agent == 1L, 1L, 0L)
    } else {
      nb <- cell_data$assisted; br <- 0L
    }
    cell_data$assisted_av      <- nb * cell_data$av
    cell_data$broker_av        <- br * cell_data$av
    cell_data$assisted_premium <- nb * cell_data$premium
    cell_data$broker_premium   <- br * cell_data$premium
    cell_data$nonbroker        <- nb   # raw_demo for the premium interactions
    cell_data$broker           <- br
  }
  # Commission x broker interaction (broker/agent only, not navigators)
  if (!"commission_broker" %in% names(cell_data)) {
    if ("any_agent" %in% names(cell_data)) {
      cell_data$commission_broker <- cell_data$comm_pmpm * ifelse(cell_data$any_agent == 1L, cell_data$assisted, 0L)
    } else {
      cell_data$commission_broker <- cell_data$comm_pmpm * cell_data$assisted
    }
  }

  # Plan names and attributes from plan_attrs (post-collapse, always consistent)
  plan_ids_cell <- sort(plan_attrs$plan_id)
  J <- length(plan_ids_cell)

  if (J < 2) { n_skip <- n_skip + 1L; rm(cell_data, plans, plan_attrs); next }

  # Read attributes directly from plan_attrs — no lookups against plans_cell
  pa <- plan_attrs[match(plan_ids_cell, plan_attrs$plan_id), ]
  posted_premium <- setNames(pa$premium_posted, pa$plan_id)
  plan_metal     <- setNames(pa$metal, pa$plan_id)
  plan_issuer    <- setNames(pa$issuer, pa$plan_id)
  plan_avs       <- setNames(pa$av, pa$plan_id)
  comm_vec       <- if ("comm_pmpm" %in% names(pa)) setNames(pa$comm_pmpm, pa$plan_id) else setNames(rep(0, J), plan_ids_cell)

  # 2nd cheapest Silver by posted premium (ACA benchmark)
  silver_bp <- plan_attrs[plan_attrs$metal == "Silver", ]
  silver_bp <- silver_bp[order(silver_bp$premium_posted), ]
  benchmark_plan <- if (nrow(silver_bp) == 0) NA_character_ else if (nrow(silver_bp) == 1) silver_bp$plan_id[1] else silver_bp$plan_id[2]
  rm(silver_bp)

  # -----------------------------------------------------------------------
  # Step 1: Compute utility
  # -----------------------------------------------------------------------
  util_result <- compute_utility(cell_data, coefs)
  V <- util_result$V
  V_base <- util_result$V_base

  # -----------------------------------------------------------------------
  # Step 2: Compute shares and elasticities (all HH)
  # -----------------------------------------------------------------------
  se_result <- compute_shares_and_elasticities(
    cell_data, V, lambda, benchmark_plan, plan_attrs, coefs,
    spec = STRUCTURAL_SPEC, V_base = V_base
  )
  shares    <- se_result$shares
  elast_mat <- se_result$elast_mat

  # -----------------------------------------------------------------------
  # Step 3: Ownership matrix and Omega
  # -----------------------------------------------------------------------
  ins_own <- sub("_.*", "", plan_ids_cell)         # ownership matrix: 1 if same firm
  own_mat <- outer(ins_own, ins_own, "==") * 1L
  # Multi-product Bertrand FOC for plan j needs sum_k own[j,k] (p_k - mc_k) ds_k/dp_j.
  # elast_mat[j,l] = ds_j/dp_l (row = responder), so equation j needs elast_mat[k,j]
  # = t(elast_mat)[j,k]. Transpose before forming Omega. (Symmetric off the benchmark,
  # so this only moves the silver/benchmark insurer's markups, but it is the correct FOC.)
  Omega <- -own_mat * t(elast_mat)  # positive diagonal

  # -----------------------------------------------------------------------
  # Step 4: Broker shares and elasticities (assisted HH only)
  # -----------------------------------------------------------------------
  broker_result <- compute_broker_shares_and_elasticities(
    cell_data, V, lambda, benchmark_plan, plan_attrs, coefs,
    spec = STRUCTURAL_SPEC, V_base = V_base
  )
  broker_elast_mat <- broker_result$broker_elast_mat
  Omega_broker <- -own_mat * t(broker_elast_mat)  # same transpose as Omega

  # Commission FOC inputs for the M4 GMM moment (s4): broker enrollment qB_j and the
  # broker commission-derivative matrix D[j,k] = dqB_j/deta_k. Both are fixed given the
  # demand estimates (they run through beta_comm, not the cost parameters), so we
  # precompute them here and the cost GMM evaluates the commission FOC at its own theta
  # using these plus the cost-implied marginal cost. [D %*% w_f]_j = dqB_j/dk_f.
  comm_deriv <- compute_commission_derivatives(cell_data, V, lambda, coefs, V_base = V_base)
  comm_D  <- comm_deriv$D[plan_ids_cell, plan_ids_cell, drop = FALSE]
  comm_qB <- comm_deriv$qB[plan_ids_cell]

  # -----------------------------------------------------------------------
  # Step 5: Risk scores and RA (needed for FOC RA derivative)
  # -----------------------------------------------------------------------
  plan_chars_cell <- tibble(
    plan_id   = plan_ids_cell,
    Silver      = as.integer(unname(plan_metal) == "Silver"),
    Gold        = as.integer(unname(plan_metal) == "Gold"),
    Platinum    = as.integer(unname(plan_metal) == "Platinum"),
    AV          = unname(pa$av),
    HMO         = unname(setNames(pa$hmo, pa$plan_id)[plan_ids_cell]),
    trend       = y - 2014L,
    !!!setNames(lapply(CLAIMS_REGION_TERMS, function(rc)
      as.numeric(ifelse(is.na(plans[[rc]][match(plan_ids_cell, gsub("SIL(94|73|87)", "SIL", plans$plan_id))]), 0,
                        plans[[rc]][match(plan_ids_cell, gsub("SIL(94|73|87)", "SIL", plans$plan_id))]))),
      CLAIMS_REGION_TERMS),
    Anthem      = as.integer(str_detect(plan_ids_cell, "^ANT")),
    Blue_Shield = as.integer(str_detect(plan_ids_cell, "^BS")),
    Health_Net  = as.integer(str_detect(plan_ids_cell, "^HN")),
    Kaiser      = as.integer(str_detect(plan_ids_cell, "^KA")),
    Molina            = as.integer(str_detect(plan_ids_cell, "^MOL")),
    LA_Care           = as.integer(str_detect(plan_ids_cell, "^LA")),
    SHARP             = as.integer(str_detect(plan_ids_cell, "^SH")),
    Chinese_Community = as.integer(str_detect(plan_ids_cell, "^CC")),
    Oscar             = as.integer(str_detect(plan_ids_cell, "^OSC")),
    Western           = as.integer(str_detect(plan_ids_cell, "^WEST")),
    Valley            = as.integer(str_detect(plan_ids_cell, "^VAL"))
  )

  demo_shares <- tryCatch(
    compute_demographic_shares(cell_data, V, lambda, V_base = V_base),
    error = function(e) NULL
  )

  rf_cell <- reins_df %>% filter(year == y)
  reins_vec <- sapply(plan_ids_cell, function(pn) {
    rf <- rf_cell$reins_factor[rf_cell$plan_id == pn]
    if (length(rf) == 0) return(0)
    mean(rf, na.rm = TRUE)
  })
  # Members in the cell (one Uninsured row per household; hh_weight = household size)
  N_cell <- sum(cell_data$hh_weight[cell_data$plan_id == "Uninsured"], na.rm = TRUE)

  # Everything pass 2 needs (transfers, marginal costs, markups) once the
  # statewide sums are known; the household data are dropped here.
  pass1[[i]] <- list(
    region = r, year = y, plan_ids = plan_ids_cell, N = N_cell,
    shares = shares, elast_mat = elast_mat, own_mat = own_mat, Omega = Omega,
    Omega_broker = Omega_broker, comm_D = comm_D, comm_qB = comm_qB, comm_vec = comm_vec,
    posted_premium = posted_premium, plan_avs = plan_avs, plan_metal = plan_metal,
    plan_issuer = plan_issuer, plan_chars_cell = plan_chars_cell, demo_shares = demo_shares,
    reins_vec = reins_vec
  )
  n_done <- n_done + 1L

  rm(cell_data, plans, plan_attrs, pa, V, V_base, se_result, broker_result, util_result,
     shares, elast_mat, own_mat, Omega, broker_elast_mat, Omega_broker, comm_deriv,
     comm_D, comm_qB, comm_vec, posted_premium, plan_metal, plan_issuer, plan_chars_cell,
     plan_avs, demo_shares, reins_vec, rf_cell, N_cell)
  gc(verbose = FALSE)

  if (i %% 20 == 0) {
    cat("  Cell", i, "of", nrow(cells), "(done:", n_done, " skip:", n_skip, ")\n")
  }
  }, error = function(e) {
    cat("  ERROR at cell", i, "(region", r, "year", y, "):", conditionMessage(e), "\n")
    cat("  Traceback:\n")
    traceback(4)
    stop(e)
  })
}

gc(verbose = FALSE)

cat("  Completed:", n_done, "  Skipped:", n_skip, "\n")

# =========================================================================
# Pass 2: statewide transfer sums, then transfers, marginal costs, and markups
# =========================================================================
# The transfer formula pools the whole state (ra.R). The statewide sums are
# built here from every cell's predicted risk scores, predicted age factors,
# and members at the OLS cost coefficients; s4 recomputes them at each GMM
# evaluation, and the counterfactual holds the rest of the state at its
# baseline contribution.
cat("\nPass 2: transfers, marginal costs, and markups...\n")
pass1 <- Filter(Negate(is.null), pass1)
cell_recs <- lapply(pass1, function(cl) {
  rs <- predict_risk_scores(ra_regs$rs_coefs, cl$plan_chars_cell, cl$demo_shares)
  list(region = cl$region, year = cl$year, N = cl$N, shares = cl$shares,
       rs = setNames(rs$predicted_risk_score, rs$plan_id), av = cl$plan_avs,
       arf = setNames(cl$demo_shares$arf, cl$demo_shares$plan_id),
       gcf = ra_gcf(cl$region, cl$year), premium = cl$posted_premium)
})
stopifnot(all(is.finite(sapply(cell_recs, function(x) x$gcf))))
ra_state <- ra_state_totals(cell_recs)
write_csv(ra_state$totals, file.path(TEMP_DIR, "ra_state.csv"))
write_csv(ra_state$own, file.path(TEMP_DIR, "ra_state_cells.csv"))
cat("  Statewide average premium (net of the admin share) by year:",
    paste(ra_state$totals$year, round(ra_state$totals$pbar), sep = ": ", collapse = ", "), "\n")
rm(cell_recs)

foc_inputs_dir <- file.path(TEMP_DIR, "foc_inputs")
if (!dir.exists(foc_inputs_dir)) dir.create(foc_inputs_dir, recursive = TRUE)
for (k in seq_along(pass1)) {
  cl <- pass1[[k]]
  r <- cl$region; y <- cl$year; plan_ids_cell <- cl$plan_ids; J <- length(plan_ids_cell)
  ra_env <- ra_env_for_cell(r, y, cl$N, cl$demo_shares, ra_state$totals, ra_state$own)

  mc_result <- compute_mc(ra_regs$rs_coefs, ra_regs$claims_coefs, cl$plan_chars_cell,
                          cl$demo_shares, cl$shares, ra_env, cl$plan_avs, cl$reins_vec)
  mc_structural <- mc_result$mc
  pred_claims   <- mc_result$predicted_claims
  rs_pred       <- tibble(plan_id = names(mc_result$predicted_risk_scores),
                          predicted_risk_score = unname(mc_result$predicted_risk_scores),
                          log_risk_score_hat = unname(mc_result$log_risk_score_hat))
  ra_transfers  <- mc_result$ra_transfers

  # RA derivative for the FOC: price changes shift enrollment composition and
  # the cell's own contribution to the statewide sums
  rs_levels <- setNames(rs_pred$predicted_risk_score, rs_pred$plan_id)
  ra_foc <- compute_ra_foc(rs_levels, cl$shares, cl$plan_avs, ra_env, cl$elast_mat, cl$own_mat)

  # Per-cell pricing FOC residual (share units per member): s + ra_foc -
  # Omega (p - mc - a) + (1 - beta) Omega_broker comm, with a the insurer's
  # administrative cost per member and beta the administrative saving per
  # commission dollar on broker enrollees. The pricing condition the model imposes
  # is its plan-year aggregate over the plan's regions (below); the per-cell markup
  # inversion mc_foc stays as a diagnostic only.
  admin_vec <- setNames(ADMIN_LOOKUP[paste(sub("_.*", "", plan_ids_cell), y, sep = "_")], plan_ids_cell)
  admin_vec[is.na(admin_vec)] <- 0
  rhs <- cl$shares + ra_foc + (1 - BETA_ADMIN) * as.numeric(cl$Omega_broker %*% cl$comm_vec)
  foc_resid <- rhs - as.vector(cl$Omega %*% (cl$posted_premium - mc_structural - admin_vec))
  markup_inv <- tryCatch(solve(cl$Omega, rhs), error = function(e) rep(NA_real_, J))

  # RA factor = AV * induced demand factor per plan
  ra_factor_static <- setNames(
    cl$plan_avs * ifelse(is.na(RA_IDF_BY_AV[as.character(round(cl$plan_avs, 1))]), 1.0,
                         RA_IDF_BY_AV[as.character(round(cl$plan_avs, 1))]),
    plan_ids_cell)
  mc_foc <- cl$posted_premium - markup_inv - admin_vec
  markup <- cl$posted_premium - mc_structural - admin_vec
  lerner <- ifelse(cl$posted_premium > 0, markup / cl$posted_premium, NA_real_)

  saveRDS(list(
    region         = r,
    year           = y,
    plan_ids       = plan_ids_cell,
    Omega          = cl$Omega,
    Omega_broker   = cl$Omega_broker,
    shares         = cl$shares,
    comm_vec       = cl$comm_vec,
    posted_premium = cl$posted_premium,
    reins_vec      = cl$reins_vec,
    plan_avs       = cl$plan_avs,
    ra_foc         = ra_foc,
    elast_mat      = cl$elast_mat,   # raw (untransposed) E, so the GMM can recompute ra_foc at its theta
    own_mat        = cl$own_mat,
    demo_shares    = cl$demo_shares, # demand-model-predicted demographic shares and ARF for M1/M3
    hmo            = setNames(cl$plan_chars_cell$HMO, cl$plan_chars_cell$plan_id),
    comm_D         = cl$comm_D,      # broker commission-derivative matrix dqB_j/deta_k (M4 commission FOC)
    comm_qB        = cl$comm_qB,     # broker enrollment per plan, share units (M4 commission FOC)
    N              = cl$N,           # members in the cell (transfer formula)
    gcf            = ra_env$gcf,     # geographic cost factor of the cell
    admin          = admin_vec       # insurer administrative cost per member (MLR)
  ), file.path(foc_inputs_dir, paste0("foc_", r, "_", y, ".rds")))

  results_list[[k]] <- tibble(
    region          = r,
    year            = y,
    plan_id         = plan_ids_cell,
    issuer          = unname(cl$plan_issuer),
    metal           = unname(cl$plan_metal),
    share           = unname(cl$shares),
    posted_premium  = unname(cl$posted_premium),
    markup          = unname(markup),
    mc_foc          = unname(mc_foc),
    mc_structural   = unname(mc_structural),
    ra_factor_static = unname(ra_factor_static),
    ra_transfer     = unname(ra_transfers),
    predicted_claims = unname(pred_claims),
    predicted_risk_score = unname(rs_pred$predicted_risk_score),
    lerner_index    = unname(lerner),
    commission_pmpm = unname(cl$comm_vec),
    admin_pmpm      = unname(admin_vec),
    foc_resid       = unname(foc_resid),
    omega_own       = unname(diag(cl$Omega)),
    members         = unname(cl$shares) * cl$N
  )
}
rm(pass1); gc(verbose = FALSE)

# =========================================================================
# Combine and write results
# =========================================================================

supply_results <- bind_rows(results_list)
rm(results_list, hh_split)

# Plan-year base premium and regional factors: an insurer prices each plan once
# for the state (index rate and metal factor) and applies a regional factor
# common to its plans, so p_jc = P_jy * g_jc with P_jy the member-weighted mean
# posted premium over the plan's regions and g_jc fixed. The counterfactual
# solves P_jy and holds g_jc.
supply_results <- supply_results %>%
  group_by(plan_id, year) %>%
  mutate(base_premium = weighted.mean(posted_premium, pmax(members, 1e-9))) %>%
  ungroup() %>%
  mutate(region_factor = posted_premium / base_premium)

# Plan-year pricing FOC: G_jy = sum_c N_c g_jc foc_resid_jc is the derivative of
# insurer profit with respect to the plan-year base premium (N_c foc_resid_jc is
# the derivative with respect to the cell premium). Reported per member and in
# dollars (divided by the weighted own-price term).
foc_plan_year <- supply_results %>%
  mutate(N = members / pmax(share, 1e-12), w = N * region_factor) %>%
  group_by(plan_id, year, metal, issuer) %>%
  summarize(n_cells = n(), members = sum(members),
            G = sum(w * foc_resid), w_sum = sum(w),
            omega_w = sum(w * omega_own) / sum(w), .groups = "drop") %>%
  mutate(G_per_member = G / w_sum, G_dollars = G_per_member / omega_w) %>%
  select(plan_id, year, metal, issuer, n_cells, members, G, G_per_member, G_dollars)
write_csv(foc_plan_year, file.path(TEMP_DIR, "foc_plan_year.csv"))

supply_results <- supply_results %>% select(-foc_resid, -omega_own, -members)
write_csv(supply_results, "results/supply_results.csv")
cat("\nSupply results:", nrow(supply_results), "rows -> results/supply_results.csv\n")
cat("  Plan-years:", nrow(foc_plan_year), "; regional factors reproduce posted premiums:",
    isTRUE(all.equal(supply_results$base_premium * supply_results$region_factor,
                     supply_results$posted_premium)), "\n")

# =========================================================================
# Diagnostics
# =========================================================================

cat("\n--- Supply Diagnostics ---\n")
cat("  Median Lerner index:", round(median(supply_results$lerner_index, na.rm = TRUE), 3), "\n")
cat("  Markup range: [", round(min(supply_results$markup, na.rm = TRUE), 1),
    ",", round(max(supply_results$markup, na.rm = TRUE), 1), "]\n")
cat("  Median markup:", round(median(supply_results$markup, na.rm = TRUE), 1), "$/month\n")
cat("  Negative MC (FOC) count:", sum(supply_results$mc_foc < 0, na.rm = TRUE),
    "of", nrow(supply_results), "\n")
cat("  Commission FOC summary (commission_pmpm):\n")
print(summary(supply_results$commission_pmpm))

# Plan-year pricing FOC residuals at the OLS cost estimates, in dollars per
# member-month (positive = the structural cost sits below what pricing implies)
cat("\n--- Plan-year pricing FOC residuals ($ per member-month, member-weighted) ---\n")
cat("  By metal:\n")
print(foc_plan_year %>% group_by(metal) %>%
        summarize(plan_years = n(), residual = round(weighted.mean(G_dollars, members), 1),
                  .groups = "drop"))
cat("  By insurer:\n")
print(foc_plan_year %>% group_by(issuer) %>%
        summarize(plan_years = n(), residual = round(weighted.mean(G_dollars, members), 1),
                  .groups = "drop") %>% arrange(residual), n = Inf)
mc_valid <- supply_results %>%
  filter(!is.na(mc_foc), !is.na(mc_structural), share >= SHARE_FLOOR_FOC)
if (nrow(mc_valid) > 0)
  cat("  Per-cell inversion diagnostic: correlation of mc_foc with mc_structural",
      round(cor(mc_valid$mc_foc, mc_valid$mc_structural), 3), "\n")

# =========================================================================
# Figures
# =========================================================================

gc(full = TRUE, verbose = FALSE)
graphics.off()

if (!dir.exists("results/figures")) dir.create("results/figures", recursive = TRUE)

plot_data <- supply_results

# 1. Markup distribution by insurer
p_markup_insurer <- plot_data %>%
  filter(!is.na(issuer)) %>%
  ggplot(aes(x = reorder(issuer, markup, median), y = markup)) +
  geom_boxplot(outlier.size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(x = NULL, y = "Markup ($/month)") +
  theme_bw()
cat("  Saving markup_insurer...\n")
ggsave("results/figures/supply_markup_insurer.png", p_markup_insurer, width = 6, height = 4)

# 2. Marginal cost vs posted premium
p_mc_premium <- plot_data %>%
  ggplot(aes(x = posted_premium, y = mc_foc, color = metal)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Posted Premium ($/month)", y = "Marginal Cost ($/month)", color = "Metal") +
  theme_bw()
cat("  Saving mc_vs_premium...\n")
ggsave("results/figures/supply_mc_vs_premium.png", p_mc_premium, width = 7, height = 5)

# 3. Commission cost vs margin by insurer
p_comm_margin <- plot_data %>%
  filter(!is.na(issuer)) %>%
  group_by(issuer) %>%
  summarize(
    avg_markup = mean(markup, na.rm = TRUE),
    avg_commission = mean(commission_pmpm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = avg_commission, y = avg_markup, label = issuer)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.8, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Average Commission ($/month)", y = "Average Markup ($/month)") +
  theme_bw()
cat("  Saving comm_vs_margin...\n")
ggsave("results/figures/supply_comm_vs_margin.png", p_comm_margin, width = 6, height = 5)

# 4. Lerner index by metal tier
p_lerner_metal <- plot_data %>%
  filter(!is.na(metal), !is.na(lerner_index)) %>%
  mutate(metal = factor(metal, levels = c("Platinum", "Gold", "Silver",
                                           "Bronze", "Minimum Coverage"))) %>%
  ggplot(aes(x = metal, y = lerner_index)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(x = "Metal Tier", y = "Lerner Index") +
  theme_bw()
cat("  Saving lerner_metal...\n")
ggsave("results/figures/supply_lerner_metal.png", p_lerner_metal, width = 6, height = 4)

# 5. MC validation: FOC vs structural
p_mc_compare <- plot_data %>%
  filter(!is.na(mc_foc), !is.na(mc_structural)) %>%
  ggplot(aes(x = mc_structural, y = mc_foc, color = metal)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "MC (Structural RA Model)", y = "MC (FOC Inversion)", color = "Metal") +
  theme_bw()
cat("  Saving mc_foc_vs_structural...\n")
ggsave("results/figures/supply_mc_foc_vs_structural.png", p_mc_compare, width = 7, height = 5)

cat("Figures saved to results/figures/.\n")
cat("Supply-side estimation complete.\n")
