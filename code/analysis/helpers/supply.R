# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-06
## Description:   Helper functions for supply-side estimation (markup recovery,
##                commission FOC evaluation, two-part nested-logit kernels).


# build_structural --------------------------------------------------------
#
# Single structural choice-data builder, shared by the structural pipeline
# (s2 demand, s3 pricing, and the counterfactual workers).
# Uses NET premium (matches the structural demand price); retains premium_posted
# for the markup FOC and the posted->net chain rule. Returns
# list(cell_data, plan_attrs). Callers must use the SAME seed and SAMPLE_FRAC
# for an identical HH sample.

build_structural <- function(plans, hhs, sample_frac,
                             spec = NULL) {

  hhs_dt <- as.data.table(hhs)
  plans_dt <- as.data.table(plans)

  # Normalize empty-string plan_id (CSV roundtrip artifact) back to NA.
  hhs_dt[plan_id == "", plan_id := NA_character_]

  # 0. Sample HH (identical logic to build_rf): sample sample_frac
  # of insured (CC) and the same sample_frac of uninsured (ACS), stratified
  # by source.
  cc_ids  <- hhs_dt[!is.na(plan_id), unique(household_id)]
  acs_ids <- hhs_dt[ is.na(plan_id), unique(household_id)]
  n_cc    <- max(1L, as.integer(length(cc_ids)  * sample_frac))
  n_acs   <- max(1L, as.integer(length(acs_ids) * sample_frac))

  keep_ids <- c(
    sample(cc_ids,  n_cc,  replace = FALSE),
    sample(acs_ids, n_acs, replace = FALSE)
  )
  hhs_dt <- hhs_dt[household_id %in% keep_ids]

  if (length(unique(hhs_dt$household_id)) < 50) return(NULL)

  # Household-level CSR eligibility for the Pareto-dominated flag (RF definition;
  # subsidized_members coalesced to 0 as in 1_decision-analysis.R). Computed on the
  # household input and carried via the demographics merge so it survives the
  # small-carrier aggregation.
  hhs_dt[, csr94_elig := as.integer(fcoalesce(as.numeric(subsidized_members), 0) > 0 &
                                      fcoalesce(FPL, 99) <= 1.5)]
  hhs_dt[, csr87_elig := as.integer(fcoalesce(as.numeric(subsidized_members), 0) > 0 &
                                      fcoalesce(FPL, 99) > 1.5 & fcoalesce(FPL, 99) <= 2.0)]

  # 1. Choice set (one row per plan_id + Uninsured)
  choice_set <- plans_dt[, .(
    issuer         = first(issuer),
    network_type   = first(network_type),
    metal          = first(metal),
    premium_posted = mean(premium, na.rm = TRUE),
    msp            = min(msp, na.rm = TRUE),
    hsa            = min(hsa, na.rm = TRUE),
    cf_resid       = first(cf_resid)
  ), by = plan_id]
  choice_set[, plan_id := as.character(plan_id)]

  # Carry commission PMPM if present (structural path only)
  if ("comm_pmpm" %in% names(plans_dt)) {
    cs_comm <- plans_dt[, .(comm_pmpm = mean(comm_pmpm, na.rm = TRUE)), by = plan_id]
    choice_set <- merge(choice_set, cs_comm, by = "plan_id", all.x = TRUE)
    choice_set[is.na(comm_pmpm), comm_pmpm := 0]
  }

  uninsured_row <- data.table(
    plan_id = "Uninsured", issuer = "Outside_Option",
    network_type = NA_character_,
    metal = NA_character_, premium_posted = NA_real_,
    msp = NA_real_, hsa = NA_real_, cf_resid = 0
  )
  if ("comm_pmpm" %in% names(choice_set)) uninsured_row$comm_pmpm <- 0
  choice_set <- rbind(choice_set, uninsured_row)

  # 2. Cross-join sampled HH x choice set. `cutoff` looked up from
  # AFFORD_THRESHOLDS by year (not carried as a per-row HH column).
  hh_slim <- hhs_dt[, .(
    household_id, FPL, subsidized_members, rating_factor,
    hh_plan_id = plan_id,
    oldest_member, cheapest_premium, subsidy, penalty,
    poverty_threshold, SLC_contribution, premiumSLC,
    cutoff = AFFORD_THRESHOLDS[as.character(year)]
  )]

  hh_slim[, .xjoin := 1L]
  choice_set[, .xjoin := 1L]
  dt <- merge(hh_slim, choice_set, by = ".xjoin", allow.cartesian = TRUE)
  dt[, .xjoin := NULL]
  rm(hh_slim, choice_set)

  # 3. CSR filter
  dt[, `:=`(
    csr_73 = fifelse(FPL > 2 & FPL <= 2.5 & subsidized_members > 0, 1L, 0L),
    csr_87 = fifelse(FPL > 1.5 & FPL <= 2 & subsidized_members > 0, 1L, 0L),
    csr_94 = fifelse(FPL <= 1.5 & subsidized_members > 0, 1L, 0L)
  )]
  dt <- dt[
    (metal == "Silver - Enhanced 73" & csr_73 == 1L) |
    (metal == "Silver - Enhanced 87" & csr_87 == 1L) |
    (metal == "Silver - Enhanced 94" & csr_94 == 1L) |
    !str_detect(metal, "^Silver") |
    (metal == "Silver" & csr_73 == 0L & csr_87 == 0L & csr_94 == 0L) |
    is.na(metal)
  ]
  dt[, c("csr_73", "csr_87", "csr_94") := NULL]

  # 4. Catastrophic filter
  dt[, `:=`(
    eff_premium = fcase(
      subsidized_members > 0,  (cheapest_premium - subsidy) * 12,
      subsidized_members == 0, cheapest_premium * 12,
      default = NA_real_
    ),
    threshold = cutoff * FPL * poverty_threshold
  )]
  dt <- dt[
    (oldest_member < 30 & !is.na(oldest_member) &
       eff_premium > threshold & metal == "Minimum Coverage") |
    metal != "Minimum Coverage" | is.na(metal)
  ]
  dt[, c("eff_premium", "threshold") := NULL]

  # 5. Plan choice indicator and premiums (RETAIN rating_factor, subsidy)
  dt[, plan_choice := fifelse(
    hh_plan_id == plan_id & !is.na(hh_plan_id),
    1L, 0L
  )]
  dt[, insured := max(plan_choice), by = household_id]

  # Net premium: out-of-pocket after subsidy, less the per-month penalty offset.
  # Matches the structural demand price (choice.R premium_type = "net"). The
  # posted premium is retained separately (premium_posted) for the markup FOC
  # and the posted->net chain rule in compute_shares_and_elasticities.
  dt[, adj_subsidy := fifelse(is.na(subsidy), 0, subsidy)]
  dt[, premium_hh := (premium_posted / RATING_FACTOR_AGE40) * rating_factor]
  dt[, premium_oop := fcase(
    issuer == "Outside_Option",        0.0,
    default = pmax(premium_hh - adj_subsidy, 0) - penalty / 12
  )]
  dt[, av := fcase(
    metal == "Minimum Coverage",     0.55,
    metal == "Bronze",               0.60,
    metal == "Silver",               0.70,
    metal == "Gold",                 0.80,
    metal == "Platinum",             0.90,
    metal == "Silver - Enhanced 73", 0.73,
    metal == "Silver - Enhanced 87", 0.87,
    metal == "Silver - Enhanced 94", 0.94,
    issuer == "Outside_Option",      0,
    default = NA_real_
  )]
  # Subsidized = has a finite contribution cap (FPL 138-400%), i.e. the household for
  # whom the price-linked subsidy is defined and responds to the benchmark. ONE flag,
  # gating BOTH the subsidy level (update_premiums) and the benchmark 4-case derivative
  # below, so the FOC residual and its gradient use the identical household set. Keyed
  # on SLC_contribution (is.finite is FALSE, never NA, when absent) NOT on
  # subsidized_members, which is NA for ~40% of rows — that NA would propagate into
  # the net premium and poison the cell's shares (and was silently dropping those
  # households from the elasticity benchmark column).
  dt[, subsidized := as.integer(is.finite(SLC_contribution))]

  # premium_posted kept on data for supply-side use

  # 6. Keep the big four AND the seven larger regionals separate, each as its own
  # brand/firm (own plan_id, own commission via prefix, own ownership block in the
  # supply FOC). Collapse ONLY the micro-carriers (United, Contra Costa, and any
  # other leftover) into a single "Other_Small" bucket, which serves as the demand
  # baseline. Group by base metal so CSR variants fold in. Prefix "OS" is absent
  # from commission_lookup, so the micro-carriers carry zero commission.
  keep_separate <- c("Anthem", "Blue_Shield", "Kaiser", "Health_Net",
                     "Molina", "LA_Care", "SHARP", "Chinese_Community",
                     "Oscar", "Western", "Valley")

  large <- dt[issuer %in% c(keep_separate, "Outside_Option")]
  small_raw <- dt[!issuer %in% c(keep_separate, "Outside_Option")]
  rm(dt)

  has_comm <- "comm_pmpm" %in% names(small_raw)

  if (nrow(small_raw) > 0) {
    small_raw[, base_metal := sub(" - Enhanced.*", "", metal)]
    small <- small_raw[, .(
      premium_oop    = min(premium_oop, na.rm = TRUE),
      plan_choice    = max(plan_choice, na.rm = TRUE),
      FPL            = first(FPL),
      hh_plan_id     = first(hh_plan_id),
      oldest_member  = first(oldest_member),
      insured        = first(insured),
      penalty        = first(penalty),
      hsa            = mean(hsa, na.rm = TRUE),
      av             = mean(av, na.rm = TRUE),
      cf_resid       = mean(cf_resid, na.rm = TRUE),
      rating_factor  = first(rating_factor),
      adj_subsidy    = first(adj_subsidy),
      subsidized     = first(subsidized),
      SLC_contribution = first(SLC_contribution),
      premiumSLC     = first(premiumSLC),
      premium_hh     = min(premium_hh, na.rm = TRUE),
      premium_posted = min(premium_posted, na.rm = TRUE)
    ), by = .(household_id, base_metal)]
    if (has_comm) {
      comm_agg <- small_raw[, .(comm_pmpm = mean(comm_pmpm, na.rm = TRUE)),
                             by = .(household_id, base_metal)]
      small <- merge(small, comm_agg, by = c("household_id", "base_metal"), all.x = TRUE)
    }
    small[, `:=`(
      issuer = "Other_Small",
      metal  = base_metal,
      plan_id = fcase(
        base_metal == "Platinum",         "OS_P",
        base_metal == "Gold",             "OS_G",
        base_metal == "Silver",           "OS_SIL",
        base_metal == "Bronze",           "OS_BR",
        base_metal == "Minimum Coverage", "OS_CAT",
        default = NA_character_
      )
    )]
    small[, base_metal := NULL]
    dt <- rbind(large, small, fill = TRUE)
    rm(large, small)
  } else {
    dt <- large
    rm(large)
  }
  rm(small_raw)

  # 7. Join HH demographics (include v_hat + tau-gradient cols if available)
  demo_cols <- c("household_id", "household_size", "weight",
                 "perc_0to17", "perc_18to34", "perc_35to54",
                 "perc_black", "perc_hispanic", "perc_asian",
                 "perc_other", "perc_male", "channel",
                 "csr94_elig", "csr87_elig")
  for (extra in c("v_hat", "channel_detail", "any_agent", "p_nav", "new_enrollee")) {
    if (extra %in% names(hhs_dt)) demo_cols <- c(demo_cols, extra)
  }
  hh_demo <- hhs_dt[, ..demo_cols]
  setnames(hh_demo, "household_size", "hh_size")
  dt <- merge(dt, hh_demo, by = "household_id", all.x = TRUE)
  rm(hhs_dt, hh_demo)

  dt[is.na(metal), metal := "Other"]
  dt[, plan_choice := fcase(
    plan_choice == 1L & insured == 1L, 1L,
    plan_choice == 0L & insured == 0L & plan_id == "Uninsured" & is.na(hh_plan_id), 1L,
    default = 0L
  )]

  # 8. Final variables
  dt <- dt[!is.na(premium_oop) & !is.na(plan_id)]
  dt[, `:=`(
    # Premium in $/100/month (matches choice.R; see note there).
    net_premium    = premium_oop / hh_size / 100,
    hmo            = fifelse(fifelse(is.na(network_type), "", network_type) == "HMO", 1L, 0L),
    hsa            = fifelse(is.na(hsa) | hsa <= 0, 0L, 1L),
    FPL_250to400   = fifelse(FPL > 2.50 & FPL <= 4.00, 1L, 0L),
    FPL_400plus    = fifelse(FPL > 4.00, 1L, 0L),
    uninsured_plan = fifelse(plan_id == "Uninsured", 1L, 0L),
    platinum       = fifelse(metal == "Platinum", 1L, 0L),
    gold           = fifelse(metal == "Gold", 1L, 0L),
    silver         = fifelse(str_detect(metal, "^Silver"), 1L, 0L),
    bronze         = fifelse(metal == "Bronze", 1L, 0L),
    Anthem         = fifelse(issuer == "Anthem", 1L, 0L),
    Blue_Shield    = fifelse(issuer == "Blue_Shield", 1L, 0L),
    Kaiser         = fifelse(issuer == "Kaiser", 1L, 0L),
    Health_Net     = fifelse(issuer == "Health_Net", 1L, 0L),
    # No regional brand dummies: the seven regionals stay separate plans (step 6
    # keeps them un-collapsed for per-regional commission/cost), but a brand FE
    # per regional pushes the nesting parameter non-RUM. See _demand.R.
    hh_weight      = as.numeric(weight)
  )]
  # Inside-good intercept: one constant on every insured plan, zero on the
  # outside option, so the enrollment level is not carried by AV and the brands.
  dt[, inside := 1L - uninsured_plan]

  # Demographic x premium interactions (heterogeneous price sensitivity)
  dt[, `:=`(
    hh_size_prem       = hh_size * net_premium,
    perc_0to17_prem    = perc_0to17 * net_premium,
    perc_18to34_prem   = perc_18to34 * net_premium,
    perc_35to54_prem   = perc_35to54 * net_premium,
    perc_male_prem     = perc_male * net_premium,
    perc_black_prem    = perc_black * net_premium,
    perc_hispanic_prem = perc_hispanic * net_premium,
    perc_asian_prem    = perc_asian * net_premium,
    perc_other_prem    = perc_other * net_premium,
    FPL_250to400_prem  = FPL_250to400 * net_premium,
    FPL_400plus_prem   = FPL_400plus * net_premium
  )]

  # Demographic x AV interactions: the same demographic set as the premium
  # interactions, so the valuation of coverage generosity varies with household
  # age, gender, race, income, and size (age and gender sorting across tiers is
  # what identifies the demographic terms in the risk-score equation).
  # Premium-independent: plain covariates, no alpha_i / recompute.
  dt[, `:=`(
    hh_size_av       = hh_size       * av,
    perc_0to17_av    = perc_0to17    * av,
    perc_18to34_av   = perc_18to34   * av,
    perc_35to54_av   = perc_35to54   * av,
    perc_male_av     = perc_male     * av,
    perc_black_av    = perc_black    * av,
    perc_hispanic_av = perc_hispanic * av,
    perc_asian_av    = perc_asian    * av,
    perc_other_av    = perc_other    * av,
    FPL_250to400_av  = FPL_250to400  * av,
    FPL_400plus_av   = FPL_400plus   * av
  )]

  # Family indicator (risk-score demographics; see RS_DEMO_RAWCOL in ra.R) and the
  # Medi-Cal-line indicator (agents' servicing cost; SERVICE_RAWCOL)
  dt[, family := as.integer(hh_size > 1L)]
  dt[, FPL_le150 := as.integer(FPL <= 1.5)]

  # Demographic x insured interactions (cross-nest margin shifters)
  insured_ind <- fifelse(dt$plan_id == "Uninsured", 0, 1)
  dt[, `:=`(
    hh_size_insured       = hh_size * insured_ind,
    perc_0to17_insured    = perc_0to17 * insured_ind,
    perc_18to34_insured   = perc_18to34 * insured_ind,
    perc_35to54_insured   = perc_35to54 * insured_ind,
    perc_male_insured     = perc_male * insured_ind,
    perc_black_insured    = perc_black * insured_ind,
    perc_hispanic_insured = perc_hispanic * insured_ind,
    perc_asian_insured    = perc_asian * insured_ind,
    perc_other_insured    = perc_other * insured_ind,
    FPL_250to400_insured  = FPL_250to400 * insured_ind,
    FPL_400plus_insured   = FPL_400plus * insured_ind
  )]

  # Collapse enhanced silver plan names to SIL
  dt[, plan_id := gsub("SIL(94|73|87)", "SIL", plan_id)]

  # 9. Build plan_attrs — canonical plan attribute table (post-collapse)
  plan_attrs <- dt[plan_id != "Uninsured", .(
    issuer         = first(issuer),
    # Base metal, NOT the CSR-enhanced label. first(metal) can pick up
    # "Silver - Enhanced 73/87/94" depending on row order, which breaks every
    # downstream exact match on metal == "Silver" (benchmark identification and
    # the Silver dummy in the risk-score/MC prediction). Strip the CSR suffix.
    metal          = sub(" - Enhanced.*", "", first(metal)),
    network_type   = first(network_type),
    av             = min(av, na.rm = TRUE),  # base metal AV (not CSR-enhanced)
    hmo            = as.integer(fifelse(is.na(first(network_type)), "", first(network_type)) == "HMO"),
    hsa            = as.integer(!is.na(first(hsa)) & first(hsa) > 0),
    premium_posted = mean(premium_posted, na.rm = TRUE),
    cf_resid       = first(cf_resid)
  ), by = plan_id]
  if ("comm_pmpm" %in% names(dt)) {
    comm_by_plan <- dt[plan_id != "Uninsured",
                        .(comm_pmpm = mean(comm_pmpm, na.rm = TRUE)), by = plan_id]
    plan_attrs <- merge(plan_attrs, comm_by_plan, by = "plan_id", all.x = TRUE)
    plan_attrs[is.na(comm_pmpm), comm_pmpm := 0]
  }

  setorder(dt, household_id, plan_id)

  # Rename for model interface
  setnames(dt, c("plan_choice", "net_premium", "household_id"),
               c("choice", "premium", "household_number"))

  # Exclusion restriction + interactions (same as build_rf)
  # penalty_own identifies outside option utility separately from premium
  dt[, penalty_own := fifelse(plan_id == "Uninsured",
                               penalty / 12 / hh_size, 0)]
  dt[, `:=`(
    Anthem_silver = fifelse(issuer == "Anthem", 1L, 0L) * fifelse(str_detect(metal, "^Silver"), 1L, 0L),
    BS_silver     = fifelse(issuer == "Blue_Shield", 1L, 0L) * fifelse(str_detect(metal, "^Silver"), 1L, 0L),
    Kaiser_silver = fifelse(issuer == "Kaiser", 1L, 0L) * fifelse(str_detect(metal, "^Silver"), 1L, 0L),
    HN_silver     = fifelse(issuer == "Health_Net", 1L, 0L) * fifelse(str_detect(metal, "^Silver"), 1L, 0L),
    Anthem_bronze = fifelse(issuer == "Anthem", 1L, 0L) * fifelse(metal == "Bronze", 1L, 0L),
    BS_bronze     = fifelse(issuer == "Blue_Shield", 1L, 0L) * fifelse(metal == "Bronze", 1L, 0L),
    Kaiser_bronze = fifelse(issuer == "Kaiser", 1L, 0L) * fifelse(metal == "Bronze", 1L, 0L),
    HN_bronze     = fifelse(issuer == "Health_Net", 1L, 0L) * fifelse(metal == "Bronze", 1L, 0L)
  )]

  # Keep ALL households for supply-side aggregation
  dt[, assisted := fifelse(channel != "Unassisted", 1L, 0L)]
  # assisted_* interactions are built from nonbroker (= navigator), not `assisted`; broker_* = broker.

  # Assistance / commission interaction terms (structural). Both channels carry
  # their own generosity-steering term (assisted_av for navigators, broker_av for
  # brokers) and their own premium slope; brokers additionally carry
  # commission_broker (navigators are not commissioned). NA any_agent ->
  # non-broker. All of these enter plan choice within the insured nest only (see
  # extensive_exclude_terms in covariates.R).
  if ("comm_pmpm" %in% names(dt)) {
    if ("any_agent" %in% names(dt)) {
      dt[, nonbroker := assisted * fifelse(any_agent == 1L, 0L, 1L, na = 1L)]
      dt[, broker    := assisted * fifelse(any_agent == 1L, 1L, 0L, na = 0L)]
      dt[, commission_broker := comm_pmpm * fifelse(any_agent == 1L, assisted, 0L, na = 0L)]
    } else {
      dt[, nonbroker := assisted]
      dt[, broker    := 0L]
      dt[, commission_broker := comm_pmpm * assisted]
    }
    dt[, `:=`(
      assisted_av      = nonbroker * av,
      broker_av        = broker * av,
      assisted_premium = nonbroker * premium,
      broker_premium   = broker * premium
    )]
    # nonbroker / broker are KEPT as columns: they are the raw_demo for the
    # assisted_premium / broker_premium price interactions, so compute_alpha_i
    # and recompute_prem_interactions (which fire when premiums change) can find
    # them. Do not delete.
  }

  # Keep only HH where exactly one plan is chosen
  # Avoid .SD (triggers locked binding errors with dplyr 1.2.0)
  valid_hh <- dt[, .(keep = max(choice) == 1L), by = household_number][keep == TRUE, household_number]
  dt <- dt[household_number %in% valid_hh]

  if (nrow(dt) == 0) return(NULL)

  list(cell_data = as_tibble(dt), plan_attrs = plan_attrs)
}


# compute_utility ---------------------------------------------------------
#
# Compute V_ij for each HH-plan pair using estimated demand coefficients.
# Handles adaptive covariates (checks which terms exist in coefs_cell).
# Returns the full utility V and the base utility V_base, which omits the
# assistance terms (extensive_exclude_terms): V_base drives the enrollment
# decision, V the plan choice within the insured nest.

compute_utility <- function(cell_data, coefs_cell) {

  coef_map <- setNames(coefs_cell$estimate, coefs_cell$term)
  lambda <- coef_map[["lambda"]]
  excl <- extensive_exclude_terms(names(coef_map))

  V <- rep(0, nrow(cell_data))
  V_excl <- rep(0, nrow(cell_data))

  # Apply all coefficients that have matching columns in the data
  for (v in names(coef_map)) {
    if (v == "lambda") next
    if (v %in% names(cell_data)) {
      contrib <- coef_map[[v]] * cell_data[[v]]
      V <- V + contrib
      if (v %in% excl) V_excl <- V_excl + contrib
    }
  }

  list(V = V, V_base = V - V_excl, lambda = lambda)
}


# compute_alpha_i ----------------------------------------------------------
#
# Compute heterogeneous price sensitivity alpha_i for each row.
# alpha_i = (beta_p + beta_h * hh_size + ...) / hh_size
# Used by compute_shares_and_elasticities and the counterfactual solver.

# Generic: uses get_prem_interactions() from covariates.R to detect which
# demographic x premium terms are in the spec. Adding/removing a _prem
# variable in the spec automatically updates this derivative.
#
# Falls back to detecting _prem terms from coef_map if spec is NULL.
# base = TRUE returns the enrollment-margin slope, which omits the channel
# premium interactions (assisted_premium, broker_premium).

compute_alpha_i <- function(cell_data, coefs, spec = NULL, base = FALSE) {
  coef_map <- setNames(coefs$estimate, coefs$term)
  get_coef <- function(name) if (name %in% names(coef_map)) coef_map[[name]] else 0

  # dV/dp = beta_premium + sum( beta_{demo_prem} * demo )
  dVdp <- get_coef("premium")

  if (!is.null(spec) && exists("get_prem_interactions", mode = "function")) {
    prem_ints <- get_prem_interactions(spec)
  } else {
    # Fallback: detect _prem terms from estimated coefficients
    prem_names <- grep("_prem$", names(coef_map), value = TRUE)
    prem_ints <- setNames(
      lapply(prem_names, function(nm) sub("_prem$", "", nm)),
      prem_names
    )
  }
  if (base) prem_ints <- prem_ints[setdiff(names(prem_ints), extensive_exclude_terms(names(prem_ints)))]

  for (nm in names(prem_ints)) {
    raw_col <- prem_ints[[nm]]
    if (raw_col %in% names(cell_data))
      dVdp <- dVdp + get_coef(nm) * cell_data[[raw_col]]
  }

  # Per-dollar price sensitivity for the FOC. dVdp is dV/d(net_premium); net_premium
  # is in $100/member, so the derivative w.r.t. a raw-$ posted premium divides by
  # hh_size and by 100.
  dVdp / cell_data$hh_size / 100
}


# add_nest_probs -----------------------------------------------------------
#
# Nested-logit probabilities for one cell, two-part form. Expects a data.table
# of INSIDE rows with columns V (full utility), V_base (utility without the
# assistance terms), V_0 (the household's outside-option utility), lambda_i, and
# household_number. Adds in place:
#   s_jg    conditional share within the insured nest, from V
#   s_jg_b  conditional share from V_base (the enrollment-margin weights)
#   s_g     P(insured) = exp(lambda I_base) / (exp(lambda I_base) + exp(V_0)),
#           with I_base the inclusive value of V_base
#   q_j     s_jg * s_g
# With V_base == V this is the ordinary nested logit.

add_nest_probs <- function(ins_dt) {
  ins_dt[, V_scaled := V / lambda_i]
  ins_dt[, max_V_scaled := max(V_scaled), by = household_number]
  ins_dt[, exp_V := exp(V_scaled - max_V_scaled)]
  ins_dt[, sum_exp_V := sum(exp_V), by = household_number]
  ins_dt[, s_jg := exp_V / sum_exp_V]

  ins_dt[, Vb_scaled := V_base / lambda_i]
  ins_dt[, max_Vb_scaled := max(Vb_scaled), by = household_number]
  ins_dt[, exp_Vb := exp(Vb_scaled - max_Vb_scaled)]
  ins_dt[, sum_exp_Vb := sum(exp_Vb), by = household_number]
  ins_dt[, s_jg_b := exp_Vb / sum_exp_Vb]
  ins_dt[, log_D := max_Vb_scaled + log(sum_exp_Vb)]

  ins_dt[, log_D_lam := lambda_i * log_D]
  ins_dt[, mx := pmax(log_D_lam, V_0)]
  ins_dt[, s_g := exp(log_D_lam - mx) / (exp(log_D_lam - mx) + exp(V_0 - mx))]
  ins_dt[, q_j := s_jg * s_g]
  ins_dt[, c("V_scaled", "max_V_scaled", "exp_V", "sum_exp_V",
             "Vb_scaled", "max_Vb_scaled", "exp_Vb", "sum_exp_Vb",
             "log_D", "log_D_lam", "mx") := NULL]
  invisible(ins_dt)
}

# Inside rows with V, V_base, V_0, lambda_i attached (shared setup for the kernels)
nest_inside_rows <- function(cell_data, V, V_base, lambda) {
  dt <- as.data.table(cell_data)
  dt[, V := V]
  dt[, V_base := if (is.null(V_base)) V else V_base]
  V0_by_hh <- dt[plan_id == "Uninsured", .(V_0 = V), by = household_number]
  ins_dt <- dt[plan_id != "Uninsured"]
  ins_dt <- merge(ins_dt, V0_by_hh, by = "household_number", all.x = TRUE)
  ins_dt[is.na(V_0), V_0 := 0]
  if (!("lambda_i" %in% names(ins_dt))) ins_dt[, lambda_i := lambda]
  add_nest_probs(ins_dt)
  ins_dt
}


# compute_shares_and_elasticities -----------------------------------------
#
# Compute market shares and J x J derivative matrix dshare_j/dposted_l.
#
# Two-part nested logit: q_j = s_jg * s_g with s_jg from the full utility and
# s_g from the base inclusive value (add_nest_probs). A posted-premium change
# on plan l moves both utilities, through alpha_i (full slope, with the channel
# premium interactions) on the within-nest part and alpha_b (base slope) on the
# enrollment part:
#
#   dq_j/dV_l = q_j [ alpha_i (1{j=l} - s_lg)/lambda + alpha_b (1 - s_g) s_lg_b ]
#
# which collapses to the ordinary q_j [1{j=l}/lambda + ((lambda-1)/lambda) s_lg
# - q_l] alpha when V_base = V. Chain rule from V to the posted premium (raw
# $/month): alpha (from compute_alpha_i) carries the /hh_size and $100->$1
# conversion, rf_i = rating_factor/RATING_FACTOR_AGE40 is the age-rating
# pass-through.
#
# Net premium is floored at zero (premium less subsidy, never negative), so a
# household whose subsidy covers the plan does not respond to a small premium
# change. kink_m = 1{premium_hh > subsidy} per row multiplies the pass-through.
# For the benchmark column of a subsidized HH with an interior subsidy
# (sub_interior = 1: benchmark premium above the contribution cap), the subsidy
# absorbs the own-price change and every other plan's net premium falls by rf_i
# on the rows not at the floor. With S_f = sum_{k != l} kink_m_k s_kg and S_b
# the same sum with base shares:
#
#   dq_j/d(posted_l) = -rf q_j [ alpha_i (kink_m_j 1{j != l} - S_f)/lambda
#                                + alpha_b (1 - s_g) S_b ]
#
# which is the closed form -rf q_j [alpha_i (s_lg - 1{j=l})/lambda + alpha_b
# (1 - s_g)(1 - s_lg_b)] when no row is at the floor. A subsidized HH whose
# subsidy is clipped at zero responds to the benchmark like any other plan.
# kink_m and sub_interior are read from cell_data when present (the CF's
# update_premiums writes them at the candidate premiums) and otherwise computed
# from the observed premiums and subsidies on the rows.
#
# If cell_data contains pre-computed columns `alpha_i` and `lambda_i`, those are
# used directly (alpha_b then defaults to alpha_i).

compute_shares_and_elasticities <- function(cell_data, V, lambda, benchmark_plan,
                                             plans_cell, coefs_cell, spec = NULL,
                                             V_base = NULL, channel_filter = NULL) {

  ins_dt <- nest_inside_rows(cell_data, V, V_base, lambda)
  plan_ids <- sort(unique(ins_dt$plan_id))
  J <- length(plan_ids)

  # HH-level price sensitivity: full slope and enrollment-margin (base) slope
  if (!("alpha_i" %in% names(ins_dt))) {
    ins_dt[, alpha_i := compute_alpha_i(ins_dt, coefs_cell, spec)]
    ins_dt[, alpha_b := compute_alpha_i(ins_dt, coefs_cell, spec, base = TRUE)]
  } else if (!("alpha_b" %in% names(ins_dt))) {
    ins_dt[, alpha_b := alpha_i]
  }
  ins_dt[, rf_i := rating_factor / RATING_FACTOR_AGE40]

  # Floor indicators (see header): at observed premiums unless the caller wrote them
  if (!("kink_m" %in% names(ins_dt)))
    ins_dt[, kink_m := as.numeric((premium_posted / RATING_FACTOR_AGE40) * rating_factor - adj_subsidy > 0)]
  if (!("sub_interior" %in% names(ins_dt)))
    ins_dt[, sub_interior := as.numeric(subsidized == 1L & is.finite(SLC_contribution) &
                                          (premiumSLC - SLC_contribution) > 0)]
  ins_dt[is.na(kink_m), kink_m := 1]
  ins_dt[is.na(sub_interior), sub_interior := 0]

  # Total weight over ALL households (the normalization every FOC term uses),
  # computed BEFORE any channel filter
  total_weight <- ins_dt[, .(w = first(hh_weight)), by = household_number][, sum(w)]

  # Optional channel filter (broker-channel households for the commission terms)
  if (!is.null(channel_filter)) {
    ins_dt <- ins_dt[get(channel_filter) == 1L]
    if (nrow(ins_dt) == 0) {
      return(list(shares = setNames(rep(0, J), plan_ids),
                  elast_mat = matrix(0, J, J, dimnames = list(plan_ids, plan_ids)),
                  plan_ids = plan_ids))
    }
  }

  # --- Weighted market shares ---
  shares_dt <- ins_dt[, .(share = sum(hh_weight * q_j) / total_weight), by = plan_id]
  shares <- setNames(shares_dt$share, shares_dt$plan_id)
  shares <- shares[plan_ids]
  shares[is.na(shares)] <- 0

  # --- Derivative matrix (J x J), rows respond, columns move ---
  elast_mat <- matrix(0, nrow = J, ncol = J, dimnames = list(plan_ids, plan_ids))

  for (l_idx in seq_along(plan_ids)) {
    l <- plan_ids[l_idx]
    is_benchmark <- (!is.na(benchmark_plan) && l == benchmark_plan)

    l_info <- ins_dt[plan_id == l, .(household_number, s_lg = s_jg, s_lg_b = s_jg_b, m_l = kink_m)]
    merged <- merge(ins_dt, l_info, by = "household_number", all.x = TRUE)
    merged[is.na(s_lg), s_lg := 0]
    merged[is.na(s_lg_b), s_lg_b := 0]
    merged[is.na(m_l), m_l := 0]

    merged[, own_l := as.numeric(plan_id == l)]
    # Non-benchmark (and, in the benchmark column, unsubsidized households and
    # households whose subsidy is clipped at zero): only V_l moves, and only on
    # rows not at the floor.
    merged[, dq_dposted := q_j * rf_i * m_l *
             (alpha_i * (own_l - s_lg) / lambda_i + alpha_b * (1 - s_g) * s_lg_b)]
    if (is_benchmark) {
      # Subsidized households with an interior subsidy: V_l fixed, every other
      # V_k not at the floor falls by rf_i
      merged[, `:=`(S_f = sum(kink_m * s_jg * (1 - own_l)),
                    S_b = sum(kink_m * s_jg_b * (1 - own_l))), by = household_number]
      merged[subsidized == 1L & sub_interior == 1,
             dq_dposted := -q_j * rf_i *
               (alpha_i * (kink_m * (1 - own_l) - S_f) / lambda_i + alpha_b * (1 - s_g) * S_b)]
    }

    contrib <- merged[, .(elast = sum(hh_weight * dq_dposted) / total_weight), by = plan_id]
    vals <- setNames(contrib$elast, contrib$plan_id)[plan_ids]
    vals[is.na(vals)] <- 0
    elast_mat[, l_idx] <- vals
    rm(merged, l_info, contrib)
  }

  list(shares = shares, elast_mat = elast_mat, plan_ids = plan_ids)
}


# compute_broker_shares_and_elasticities -----------------------------------
#
# Like compute_shares_and_elasticities() but aggregates only over
# broker-channel (broker == 1, i.e. assisted x any_agent) households, the
# households commissions are paid on. Shares and derivatives are normalized by
# TOTAL cell weight (all households), matching the all-HH kernel, so the
# commission-outlay term Omega_broker %*% comm_vec is in the same share units
# as the rest of the FOC. Cells with navigators but no brokers return zeros.
# Returns broker-specific market shares and J x J derivative matrix.

compute_broker_shares_and_elasticities <- function(cell_data, V, lambda,
                                                    benchmark_plan, plans_cell,
                                                    coefs_cell, spec = NULL,
                                                    V_base = NULL) {
  res <- compute_shares_and_elasticities(cell_data, V, lambda, benchmark_plan,
                                         plans_cell, coefs_cell, spec = spec,
                                         V_base = V_base, channel_filter = "broker")
  list(broker_shares = res$shares, broker_elast_mat = res$elast_mat, plan_ids = res$plan_ids)
}


# compute_commission_derivatives --------------------------------------------
#
# D[j,k] = d qB_j / d eta_k over broker-channel households (share units per $
# of commission PMPM, normalized by TOTAL cell weight); qB = broker enrollment
# in the same share units. The commission term enters the within-nest utility
# only (it is excluded from the enrollment inclusive value), so the derivative
# has no extensive-margin part:
#
#   D[j,k] = beta_comm * sum_{i in broker} w_i q_ij (1{j=k} - s_ik|g) / lambda_i / W_total
#
# No alpha_i, no rating factor, and no benchmark 4-case logic (commissions do
# not touch the subsidy). Used by the CF commission FOC (helpers/cf_cell.R):
# [D %*% w_f]_j is d qB_j / d k_f with NO transpose, rows respond, columns move.

compute_commission_derivatives <- function(cell_data, V, lambda, coefs_cell, V_base = NULL) {

  ins_dt <- nest_inside_rows(cell_data, V, V_base, lambda)
  plan_ids <- sort(unique(ins_dt$plan_id))
  J <- length(plan_ids)

  coef_map <- setNames(coefs_cell$estimate, coefs_cell$term)
  beta_comm <- if ("commission_broker" %in% names(coef_map)) coef_map[["commission_broker"]] else 0

  # Total weight over ALL households, before the channel filter
  total_weight <- ins_dt[, .(w = first(hh_weight)), by = household_number][, sum(w)]

  ins_dt <- ins_dt[broker == 1L]

  if (nrow(ins_dt) == 0) {
    return(list(qB = setNames(rep(0, J), plan_ids),
                D = matrix(0, J, J, dimnames = list(plan_ids, plan_ids)),
                plan_ids = plan_ids, W_total = total_weight))
  }

  # Wide matrices (household x plan); dcast sorts rows by household_number
  ins_dt[, wq := hh_weight * q_j]
  Wq_m <- as.matrix(dcast(ins_dt, household_number ~ plan_id,
                          value.var = "wq", fill = 0)[, ..plan_ids])
  Sm_m <- as.matrix(dcast(ins_dt, household_number ~ plan_id,
                          value.var = "s_jg", fill = 0)[, ..plan_ids])
  lam <- ins_dt[, .(lam = first(lambda_i)), by = household_number][order(household_number), lam]

  D <- beta_comm * (diag(colSums(Wq_m / lam), nrow = J) - crossprod(Wq_m / lam, Sm_m)) / total_weight
  dimnames(D) <- list(plan_ids, plan_ids)

  qB <- setNames(colSums(Wq_m) / total_weight, plan_ids)

  list(qB = qB, D = D, plan_ids = plan_ids, W_total = total_weight)
}
