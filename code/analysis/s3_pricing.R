# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-06
## Date Edited:   2026-03-26
## Description:   Supply side at the estimated demand. Fits the risk-score and
##                claims regressions (OLS starting values for s4), then two passes
##                over the region-year cells: shares, elasticities, demographic
##                composition, and commission derivatives per cell; the statewide
##                transfer sums; transfers, marginal cost (claims net of
##                reinsurance and transfers, plus the insurer's administrative
##                cost per member), and the pricing FOC residual per plan-cell,
##                aggregated to the plan-year condition (each plan priced once
##                for the year: base premium x fixed regional factor). Writes
##                results/supply_results.csv, the per-cell FOC inputs for s4,
##                and the plan-year residuals; the per-cell markup inversion
##                (mc_foc) is kept as a diagnostic only.

# Dependencies: preamble + s1_inputs.R (plan_choice, commission_lookup)
# loaded by _analysis.R before this step. Full spec (base + assisted) feeds the
# price-interaction machinery; read from what s2_demand wrote.
STRUCTURAL_SPEC <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))$all

# =========================================================================
# Load coefficients and reference data
# =========================================================================


coefs <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
lambda <- coefs %>% filter(term == "lambda") %>% pull(estimate)

# =========================================================================
# Estimate RA regressions from rate filing data
# =========================================================================

# Claims rows: rate filing PUF plan-years with observed plan-year demographics
rsdata <- read_csv("data/output/rate_filing_rsdata.csv", show_col_types = FALSE)
plan_demo <- read_csv(file.path(TEMP_DIR, "plan_demographics.csv"), show_col_types = FALSE)
for (yy in SUPPLY_YEARS[-1]) rsdata[[paste0("year_", yy)]] <- as.integer(rsdata$year == yy)
rsdata <- rsdata %>%
  left_join(plan_demo, by = c("plan_id", "year")) %>%
  left_join(plan_choice %>%
              select(plan_id, year, all_of(CLAIMS_REGION_TERMS)) %>%
              distinct(plan_id, year, .keep_all = TRUE),
            by = c("plan_id", "year"))
n_matched <- sum(!is.na(rsdata$share_18to34))
rm(plan_demo)

# Risk-score rows: SRRT scores at insurer x metal x region x year, with the
# observed enrollment demographics aggregated to that level
rs_srrt <- read_csv("data/output/plan_risk_scores.csv", show_col_types = FALSE)
# Insurers' non-commission administrative cost per member-month (MLR filings,
# data-build step 9) and beta, the administrative saving per commission dollar
# (per-carrier substitution rates, step 9); both enter the pricing and
# commission conditions.
mlr_admin <- read_csv("data/output/mlr_admin.csv", show_col_types = FALSE)
ADMIN_LOOKUP <- setNames(mlr_admin$admin0_pmpm, paste(mlr_admin$insurer_prefix, mlr_admin$year, sep = "_"))
BETA_ADMIN <- read_csv("data/output/mlr_admin_beta.csv", show_col_types = FALSE)$beta0[1]
plan_metal_map <- plan_choice %>%
  distinct(plan_id, metal) %>%
  mutate(metal = sub(" - Enhanced.*", "", metal)) %>%
  distinct(plan_id, metal)
# Risk-score key: only Health Net files separate scores by network, so its plans
# carry HMO or PPO and every other carrier's carry "Both".
plan_net_map <- plan_choice %>%
  distinct(plan_id, network_type) %>%
  mutate(network = if_else(sub("_.*", "", plan_id) == "HN",
                           if_else(network_type == "HMO", "HMO", "PPO"), "Both")) %>%
  distinct(plan_id, network)
stopifnot(!any(duplicated(plan_net_map$plan_id)))
pdr <- read_csv(file.path(TEMP_DIR, "plan_demographics_region.csv"), show_col_types = FALSE) %>%
  inner_join(plan_metal_map, by = "plan_id") %>%
  inner_join(plan_net_map, by = "plan_id") %>%
  mutate(insurer_prefix = sub("_.*", "", plan_id)) %>%
  group_by(insurer_prefix, metal, region, year, network) %>%
  summarize(across(all_of(RS_DEMO_TERMS), ~ weighted.mean(.x, enrollment)),
            .groups = "drop")
rs_srrt <- rs_srrt %>%
  inner_join(pdr, by = c("insurer_prefix", "metal", "region", "year", "network")) %>%
  mutate(Silver = as.integer(metal == "Silver"), Gold = as.integer(metal == "Gold"),
         Platinum = as.integer(metal == "Platinum"))
rm(pdr, plan_metal_map, plan_net_map)

ra_regs <- estimate_ra_regressions(rsdata, rs_srrt)

# Save coefficients for counterfactual worker
rs_coefs_df <- tibble(term = names(ra_regs$rs_coefs), estimate = ra_regs$rs_coefs)
claims_coefs_df <- tibble(term = names(ra_regs$claims_coefs), estimate = ra_regs$claims_coefs)
write_csv(claims_coefs_df, file.path(TEMP_DIR, "ra_claims_coefs.csv"))

# Reinsurance factors by plan-year (for counterfactuals)
reins_df <- rsdata %>%
  select(plan_id, year, reins_factor) %>%
  filter(!is.na(reins_factor))
write_csv(reins_df, file.path(TEMP_DIR, "reinsurance_factors.csv"))

rm(rsdata, rs_srrt)

# =========================================================================
# Identify cells and set seeds (same as demand)
# =========================================================================

# cells, cell_seeds, plan_choice come from s1_inputs. Re-read hh_split from disk
# here (s2_demand freed its own copy) so pricing is self-contained.
hh_all <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split <- split(hh_all, by = c("region", "year"))
rm(hh_all)

# =========================================================================
# Loop over cells: build data, compute markups
# =========================================================================


results_list <- vector("list", nrow(cells))
pass1 <- vector("list", nrow(cells))   # per-cell demand-side pieces for pass 2
n_done <- 0L
n_skip <- 0L

for (i in which(cells$year %in% SUPPLY_YEARS)) {
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

  # Commissions arrive on the plans from build3 (comm_pmpm plan-level basis;
  # rate and is_pct for the household-level covariate)
  if (!all(c("comm_pmpm", "rate", "is_pct") %in% names(plans)))
    stop("plans lack comm_pmpm/rate/is_pct: re-run build3_data-prep.R")

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
  # commission_broker is built from comm_hh in build_structural; a cell without
  # it was built from stale inputs
  if (!"commission_broker" %in% names(cell_data))
    stop("cell data lack commission_broker: rebuild the cells (s2)")

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
  add_N <- util_result$add_N
  add_A <- util_result$add_A

  # -----------------------------------------------------------------------
  # Step 2: Compute shares and elasticities (all HH)
  # -----------------------------------------------------------------------
  se_result <- compute_shares_and_elasticities(
    cell_data, V, lambda, benchmark_plan, plan_attrs, coefs,
    spec = STRUCTURAL_SPEC, V_base = V_base, add_N = add_N, add_A = add_A
  )
  shares    <- se_result$shares
  elast_mat <- se_result$elast_mat
  relast_mat <- se_result$relast_mat
  rshares <- se_result$rshares
  zelast <- se_result$zelast

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
  # Revenue counterpart: the same derivative weighted by the household's
  # age-rating pass-through, since a household pays rf_i times the posted
  # (age-40) premium. Claims and transfers stay per member, on Omega.
  Omega_r <- -own_mat * t(relast_mat)

  # -----------------------------------------------------------------------
  # Step 4: Broker shares and elasticities (assisted HH only)
  # -----------------------------------------------------------------------
  broker_result <- compute_broker_shares_and_elasticities(
    cell_data, V, lambda, benchmark_plan, plan_attrs, coefs,
    spec = STRUCTURAL_SPEC, V_base = V_base, add_N = add_N, add_A = add_A
  )
  broker_elast_mat <- broker_result$broker_elast_mat
  Omega_broker <- -own_mat * t(broker_elast_mat)  # same transpose as Omega

  # Commission-condition inputs for the s4 diagnostics: broker enrollment qB_j and the
  # broker commission-derivative matrix D[j,k] = dqB_j/deta_k. Both are fixed given the
  # demand estimates (they run through the commission coefficients, not the cost parameters), so we
  # precompute them here and the cost GMM evaluates the commission FOC at its own theta
  # using these plus the cost-implied marginal cost. [D %*% w_f]_j = dqB_j/dk_f.
  comm_deriv <- compute_commission_derivatives(cell_data, V, lambda, coefs,
                                               V_base = V_base, add_N = add_N, add_A = add_A)
  comm_D  <- comm_deriv$D[plan_ids_cell, plan_ids_cell, drop = FALSE]
  comm_D_r <- comm_deriv$D_r[plan_ids_cell, plan_ids_cell, drop = FALSE]
  comm_Dz <- lapply(comm_deriv$D_z, function(m) m[plan_ids_cell, plan_ids_cell, drop = FALSE])
  comm_qB <- comm_deriv$qB[plan_ids_cell]

  # -----------------------------------------------------------------------
  # Step 5: Risk scores and RA (needed for FOC RA derivative)
  # -----------------------------------------------------------------------
  plan_chars_cell <- tibble(
    plan_id   = plan_ids_cell,
    Silver      = as.integer(unname(plan_metal) == "Silver"),
    Gold        = as.integer(unname(plan_metal) == "Gold"),
    Platinum    = as.integer(unname(plan_metal) == "Platinum"),
    !!!setNames(lapply(RS_IM_TERMS, function(t)
      as.integer(paste0("im_", sub("_.*", "", plan_ids_cell), "_", unname(plan_metal)) == t)),
      RS_IM_TERMS),
    AV          = unname(pa$av),
    HMO         = unname(setNames(pa$hmo, pa$plan_id)[plan_ids_cell]),
    !!!setNames(as.list(as.integer(SUPPLY_YEARS[-1] == y)), CLAIMS_YEAR_TERMS),
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
    compute_demographic_shares(cell_data, V, lambda, V_base = V_base,
                               add_N = add_N, add_A = add_A),
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
  # Commission-network flag: the plan's schedule side (HMO and HSP pay the
  # HMO schedule); distinct from the claims equation's HMO covariate
  comm_hmo_cell <- setNames(as.integer(!is.na(pa$network_type) &
                                         pa$network_type %in% c("HMO", "HSP")),
                            pa$plan_id)[plan_ids_cell]
  pass1[[i]] <- list(
    region = r, year = y, plan_ids = plan_ids_cell, N = N_cell,
    shares = shares, rshares = rshares, elast_mat = elast_mat,
    relast_mat = relast_mat, zelast = zelast, own_mat = own_mat, Omega = Omega, Omega_r = Omega_r,
    Omega_broker = Omega_broker, comm_D = comm_D, comm_D_r = comm_D_r, comm_Dz = comm_Dz,
    comm_qB = comm_qB, comm_vec = comm_vec,
    posted_premium = posted_premium, plan_avs = plan_avs, plan_metal = plan_metal,
    plan_issuer = plan_issuer, plan_chars_cell = plan_chars_cell, demo_shares = demo_shares,
    reins_vec = reins_vec, comm_hmo = comm_hmo_cell
  )
  n_done <- n_done + 1L

  rm(cell_data, plans, plan_attrs, pa, V, V_base, se_result, broker_result, util_result,
     shares, rshares, elast_mat, relast_mat, own_mat, Omega, Omega_r,
     broker_elast_mat, Omega_broker, comm_deriv,
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


# =========================================================================
# Pass 2: statewide transfer sums, then transfers, marginal costs, and markups
# =========================================================================
# The transfer formula pools the whole state (ra.R). The statewide sums are
# built here from every cell's predicted risk scores, predicted age factors,
# and members at the OLS cost coefficients; s4 recomputes them at each GMM
# evaluation, and the counterfactual holds the rest of the state at its
# baseline contribution.
pass1 <- Filter(Negate(is.null), pass1)
cell_recs <- lapply(pass1, function(cl) {
  rs <- predict_risk_scores(ra_regs$rs_coefs, cl$plan_chars_cell, cl$demo_shares)
  list(region = cl$region, year = cl$year, N = cl$N, shares = cl$shares, rshares = cl$rshares,
       rs = setNames(rs$predicted_risk_score, rs$plan_id), av = cl$plan_avs,
       arf = setNames(cl$demo_shares$arf, cl$demo_shares$plan_id),
       gcf = ra_gcf(cl$region, cl$year), premium = cl$posted_premium)
})
stopifnot(all(is.finite(sapply(cell_recs, function(x) x$gcf))))
# The transfer formula's premium total by year: premiums collected at the
# observed premiums
RA_TP <- ra_premium_total(cell_recs)
ra_state <- ra_state_totals(cell_recs, RA_TP)
rm(cell_recs)

foc_inputs_dir <- file.path(TEMP_DIR, "foc_inputs")
if (!dir.exists(foc_inputs_dir)) dir.create(foc_inputs_dir, recursive = TRUE)
unlink(list.files(foc_inputs_dir, pattern = "^foc_.*\\.rds$", full.names = TRUE))
for (k in seq_along(pass1)) {
  cl <- pass1[[k]]
  r <- cl$region; y <- cl$year; plan_ids_cell <- cl$plan_ids; J <- length(plan_ids_cell)
  ra_env <- ra_env_for_cell(r, y, cl$N, cl$demo_shares, ra_state)

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
  mix <- mix_response(cl$elast_mat, cl$zelast, cl$demo_shares, ra_regs$rs_coefs)
  mkt_rev <- setNames(cl$rshares + as.vector(t(cl$relast_mat) %*% cl$posted_premium), plan_ids_cell)
  ra_foc <- compute_ra_foc(rs_levels, cl$shares, cl$plan_avs, ra_env, cl$elast_mat, cl$own_mat,
                           mix, mkt_rev)$total
  # Claims moving with the enrollee mix (marginal against average cost)
  cc <- compute_claims_comp(pred_claims, cl$reins_vec, mix, cl$own_mat,
                            ra_regs$claims_coefs[["log_risk_score"]])

  # Per-cell pricing FOC residual (share units per member): s + ra_foc -
  # Omega (p - mc - a) + (1 - beta) Omega_broker comm, with a the insurer's
  # administrative cost per member and beta the administrative saving per
  # commission dollar on broker enrollees. The pricing condition the model imposes
  # is its plan-year aggregate over the plan's regions (below); the per-cell markup
  # inversion mc_foc stays as a diagnostic only.
  admin_vec <- setNames(ADMIN_LOOKUP[paste(sub("_.*", "", plan_ids_cell), y, sep = "_")], plan_ids_cell)
  admin_vec[is.na(admin_vec)] <- 0
  rhs <- cl$rshares + ra_foc - cc + (1 - BETA_ADMIN) * as.numeric(cl$Omega_broker %*% cl$comm_vec)
  foc_resid <- rhs - as.vector(cl$Omega_r %*% cl$posted_premium -
                               cl$Omega %*% (mc_structural + admin_vec))
  # The condition is rhs = Omega_r p - Omega (mc + admin), so the cost the
  # observed price implies is Omega^-1 (Omega_r p - rhs) and the markup is the
  # premium less that.
  mc_plus_admin <- tryCatch(solve(cl$Omega, as.vector(cl$Omega_r %*% cl$posted_premium) - rhs),
                            error = function(e) rep(NA_real_, J))
  markup_inv <- cl$posted_premium - mc_plus_admin

  # RA factor = AV * induced demand factor per plan
  ra_factor_static <- setNames(
    cl$plan_avs * ifelse(is.na(RA_IDF_BY_AV[as.character(round(cl$plan_avs, 1))]), 1.0,
                         RA_IDF_BY_AV[as.character(round(cl$plan_avs, 1))]),
    plan_ids_cell)
  mc_foc <- cl$posted_premium - markup_inv - admin_vec
  # Premium collected per member: the posted (age-40) premium times the
  # plan's average rating pass-through, rshares / shares
  realized_premium <- ifelse(cl$shares > 0, cl$posted_premium * cl$rshares / cl$shares,
                             NA_real_)
  markup <- realized_premium - mc_structural - admin_vec
  lerner <- ifelse(realized_premium > 0, markup / realized_premium, NA_real_)

  saveRDS(list(
    region         = r,
    year           = y,
    plan_ids       = plan_ids_cell,
    Omega          = cl$Omega,
    Omega_r        = cl$Omega_r,
    Omega_broker   = cl$Omega_broker,
    shares         = cl$shares,
    rshares        = cl$rshares,     # rating-weighted, the revenue level term
    comm_vec       = cl$comm_vec,
    posted_premium = cl$posted_premium,
    reins_vec      = cl$reins_vec,
    plan_avs       = cl$plan_avs,
    ra_foc         = ra_foc,
    elast_mat      = cl$elast_mat,   # raw (untransposed) E, so the GMM can recompute ra_foc at its theta
    own_mat        = cl$own_mat,
    demo_shares    = cl$demo_shares, # demand-model-predicted demographic shares and ARF for M1/M3
    hmo            = setNames(cl$plan_chars_cell$HMO, cl$plan_chars_cell$plan_id),
    comm_hmo       = cl$comm_hmo,    # schedule side (HMO/HSP vs PPO/EPO) for the M4 units
    comm_D         = cl$comm_D,      # broker commission-derivative matrix dqB_j/deta_k (M4 commission FOC)
    comm_D_r       = cl$comm_D_r,    # the same, rating-weighted, for the revenue half of MB
    comm_Dz        = cl$comm_Dz,     # the same, weighted by each enrollee characteristic
    relast_mat     = cl$relast_mat,  # rating-weighted premium derivative (premiums collected)
    zelast         = cl$zelast,      # premium derivatives weighted by each enrollee characteristic
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
    realized_premium = unname(realized_premium),
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

supply_results <- supply_results %>% select(-foc_resid, -omega_own, -members)
write_csv(supply_results, "results/supply_results.csv")
stopifnot(isTRUE(all.equal(supply_results$base_premium * supply_results$region_factor,
                           supply_results$posted_premium)))


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
ggsave("results/figures/supply_markup_insurer.png", p_markup_insurer, width = 6, height = 4)

# 2. Marginal cost vs posted premium
p_mc_premium <- plot_data %>%
  ggplot(aes(x = posted_premium, y = mc_foc, color = metal)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Posted Premium ($/month)", y = "Marginal Cost ($/month)", color = "Metal") +
  theme_bw()
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
ggsave("results/figures/supply_lerner_metal.png", p_lerner_metal, width = 6, height = 4)

# 5. MC validation at the starting values: FOC inversion vs the start-value
# structural cost. The paper's version of this figure is written by sum2 at the
# GMM marginal cost; this one keeps its own filename so a partial rerun cannot
# leave a start-value figure under the paper's name.
p_mc_compare <- plot_data %>%
  filter(!is.na(mc_foc), !is.na(mc_structural)) %>%
  ggplot(aes(x = mc_structural, y = mc_foc, color = metal)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "MC (Structural RA Model)", y = "MC (FOC Inversion)", color = "Metal") +
  theme_bw()
ggsave("results/figures/supply_mc_foc_vs_structural_startvals.png", p_mc_compare, width = 7, height = 5)

