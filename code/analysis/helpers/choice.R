# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-24
## Description:   Helper functions for nested logit discrete choice estimation.
##                Port of _old-repo/analysis/decision-support/choice_data_function_map.R
##                and choice_est_function_map.R.
##                Uses data.table for memory-efficient cross-joins and filtering.


# build_choice_data -------------------------------------------------------
#
# Builds long-format HH x plan choice data for one region-year cell.
#
# Args:
#   plans        — plan_data rows for this region-year (expects snake_case columns:
#                  plan_id, issuer, network_type, metal, premium, msp, hsa)
#   hhs          — hh_full rows for this region-year (expects plan_id; NA = uninsured)
#   sample_frac  — fraction of HH to sample per cell (estimation + OOS)
#
# Returns:
#   Combined estimation + OOS tibble with `assisted` flag.

build_choice_data <- function(plans, hhs, sample_frac,
                              spec = NULL, premium_type = "net") {

  # Convert inputs to data.table (copy, don't modify originals)
  hhs_dt <- as.data.table(hhs)
  plans_dt <- as.data.table(plans)

  # 0. Sample HH by channel BEFORE the cross-join to manage memory.
  untreated_ids <- hhs_dt[channel == "Unassisted", unique(household_id)]
  treated_ids   <- hhs_dt[channel != "Unassisted", unique(household_id)]

  n_untreated <- max(1L, as.integer(length(untreated_ids) * sample_frac))
  n_treated   <- max(1L, as.integer(length(treated_ids) * sample_frac))

  keep_ids <- c(
    sample(untreated_ids, n_untreated, replace = FALSE),
    sample(treated_ids, n_treated, replace = FALSE)
  )
  hhs_dt <- hhs_dt[household_id %in% keep_ids]

  if (length(unique(hhs_dt$household_id)) < 50) return(NULL)

  # 1. Choice set: one row per plan_id, add Uninsured outside option
  choice_set <- plans_dt[, .(
    issuer       = first(issuer),
    network_type = first(network_type),
    metal        = first(metal),
    premium      = mean(premium, na.rm = TRUE),
    msp          = min(msp, na.rm = TRUE),
    hsa          = min(hsa, na.rm = TRUE),
    cf_resid     = first(cf_resid)
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
    metal = NA_character_, premium = NA_real_,
    msp = NA_real_, hsa = NA_real_, cf_resid = 0
  )
  if ("comm_pmpm" %in% names(choice_set)) uninsured_row$comm_pmpm <- 0
  choice_set <- rbind(choice_set, uninsured_row)

  # 2. Cross-join sampled HH x choice set
  hh_slim <- hhs_dt[, .(
    household_id, FPL, subsidized_members, rating_factor,
    hh_plan_id = plan_id,
    oldest_member, cheapest_premium, subsidy, penalty,
    poverty_threshold, cutoff
  )]

  # data.table CJ-style cross join
  hh_slim[, .xjoin := 1L]
  choice_set[, .xjoin := 1L]
  dt <- merge(hh_slim, choice_set, by = ".xjoin", allow.cartesian = TRUE)
  dt[, .xjoin := NULL]
  rm(hh_slim, choice_set)

  # 3. CSR filter: enhanced silver only at correct FPL brackets
  dt[, `:=`(
    csr_73 = fifelse(FPL > 2 & FPL <= 2.5 & subsidized_members > 0, 1L, 0L),
    csr_87 = fifelse(FPL > 1.5 & FPL <= 2 & subsidized_members > 0, 1L, 0L),
    csr_94 = fifelse(FPL <= 1.5 & subsidized_members > 0, 1L, 0L)
  )]
  dt <- dt[
    (metal == "Silver - Enhanced 73" & csr_73 == 1L) |
    (metal == "Silver - Enhanced 87" & csr_87 == 1L) |
    (metal == "Silver - Enhanced 94" & csr_94 == 1L) |
    !grepl("^Silver", metal) |
    (metal == "Silver" & csr_73 == 0L & csr_87 == 0L & csr_94 == 0L) |
    is.na(metal)
  ]
  dt[, c("csr_73", "csr_87", "csr_94") := NULL]

  # 4. Catastrophic filter: only for oldest < 30 with unaffordable premium
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

  # 5. Plan choice indicator and premiums.
  # Insured CC HH: plan_choice=1 on the row where their plan_id matches.
  # Uninsured ACS HH: plan_choice=1 on the Uninsured outside-option row.
  dt[, plan_choice := fifelse(
    hh_plan_id == plan_id & !is.na(hh_plan_id),
    1L, 0L
  )]
  dt[, insured := max(plan_choice), by = household_id]

  # Premium construction depends on premium_type:
  #   "posted" — gross age-adjusted premium (current default)
  #   "oop"    — max(gross - subsidy, 0), keeps penalty_own separate
  #   "net"    — max(gross - subsidy, 0) - penalty/12 (penalty reduces insured cost)
  #   "evan"   — insured: max(gross - subsidy, 0), uninsured: penalty/12
  #              (Saltzman JHE 2019: penalty is the "price" of being uninsured)
  dt[, adj_subsidy := fifelse(is.na(subsidy), 0, subsidy)]
  dt[, premium_hh := (premium / RATING_FACTOR_AGE40) * rating_factor]
  if (premium_type == "posted") {
    dt[, premium_oop := fcase(
      issuer == "Outside_Option",  0.0,
      default = premium_hh
    )]
  } else if (premium_type == "oop") {
    dt[, premium_oop := fcase(
      issuer == "Outside_Option",  0.0,
      default = pmax(premium_hh - adj_subsidy, 0)
    )]
  } else if (premium_type == "net") {
    dt[, premium_oop := fcase(
      issuer == "Outside_Option",  0.0,
      default = pmax(premium_hh - adj_subsidy, 0) - penalty / 12
    )]
  } else if (premium_type == "evan") {
    dt[, premium_oop := fcase(
      issuer == "Outside_Option",  penalty / 12,
      default = pmax(premium_hh - adj_subsidy, 0)
    )]
  }
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
  dt[, c("premium_hh", "adj_subsidy", "premium") := NULL]

  # 6. Collapse small insurers into one row per (HH × base_metal). Base metal
  # collapses CSR-enhanced silvers (Small_SIL covers Silver + Silver-73/87/94).
  big_four <- c("Anthem", "Blue_Shield", "Kaiser", "Health_Net")

  large <- dt[issuer %in% c(big_four, "Outside_Option")]
  small_raw <- dt[!issuer %in% c(big_four, "Outside_Option")]
  rm(dt)

  has_comm <- "comm_pmpm" %in% names(small_raw)

  if (nrow(small_raw) > 0) {
    small_raw[, base_metal := sub(" - Enhanced.*", "", metal)]
    agg_exprs <- quote(.(
      premium_oop    = min(premium_oop, na.rm = TRUE),
      plan_choice    = max(plan_choice, na.rm = TRUE),
      FPL            = first(FPL),
      hh_plan_id     = first(hh_plan_id),
      oldest_member  = first(oldest_member),
      insured        = first(insured),
      penalty        = first(penalty),
      hsa            = mean(hsa, na.rm = TRUE),
      av             = mean(av, na.rm = TRUE),
      cf_resid       = mean(cf_resid, na.rm = TRUE)
    ))
    small <- small_raw[, eval(agg_exprs), by = .(household_id, base_metal)]
    if (has_comm) {
      comm_agg <- small_raw[, .(comm_pmpm = mean(comm_pmpm, na.rm = TRUE)),
                             by = .(household_id, base_metal)]
      small <- merge(small, comm_agg, by = c("household_id", "base_metal"), all.x = TRUE)
    }
    small[, `:=`(
      issuer = "Small_Insurer",
      metal  = base_metal,
      plan_id = fcase(
        base_metal == "Platinum",         "Small_P",
        base_metal == "Gold",             "Small_G",
        base_metal == "Silver",           "Small_SIL",
        base_metal == "Bronze",           "Small_BR",
        base_metal == "Minimum Coverage", "Small_CAT",
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

  # 7. Join HH demographics
  # Include v_hat if available (control function residual from broker density IV)
  demo_cols <- c("household_id", "household_size",
                  "perc_0to17", "perc_18to34", "perc_35to54",
                  "perc_black", "perc_hispanic", "perc_asian",
                  "perc_other", "perc_male", "channel")
  if ("channel_detail" %in% names(hhs_dt)) demo_cols <- c(demo_cols, "channel_detail")
  if ("any_agent" %in% names(hhs_dt)) demo_cols <- c(demo_cols, "any_agent")
  if ("v_hat" %in% names(hhs_dt)) demo_cols <- c(demo_cols, "v_hat")
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
    net_premium    = premium_oop / hh_size,
    hmo            = fifelse(fifelse(is.na(network_type), "", network_type) == "HMO", 1L, 0L),
    hsa            = fifelse(is.na(hsa) | hsa <= 0, 0L, 1L),
    FPL_250to400   = fifelse(FPL > 2.50 & FPL <= 4.00, 1L, 0L),
    FPL_400plus    = fifelse(FPL > 4.00, 1L, 0L),
    uninsured_plan = fifelse(plan_id == "Uninsured", 1L, 0L),
    platinum       = fifelse(metal == "Platinum", 1L, 0L),
    gold           = fifelse(metal == "Gold", 1L, 0L),
    silver         = fifelse(metal == "Silver", 1L, 0L),
    bronze         = fifelse(metal == "Bronze", 1L, 0L),
    Anthem         = fifelse(issuer == "Anthem", 1L, 0L),
    Blue_Shield    = fifelse(issuer == "Blue_Shield", 1L, 0L),
    Kaiser         = fifelse(issuer == "Kaiser", 1L, 0L),
    Health_Net     = fifelse(issuer == "Health_Net", 1L, 0L),
    hh_weight      = as.numeric(hh_size)
  )]

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

  # Insured intercept (= 1 for all insured plans, 0 for uninsured)
  insured_ind <- fifelse(dt$plan_id == "Uninsured", 0L, 1L)
  dt[, insured_intercept := insured_ind]

  # Demographic x insured interactions (cross-nest margin shifters)
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
    FPL_400plus_insured   = FPL_400plus * insured_ind,
    family_insured        = as.integer(hh_size > 1L) * insured_ind,
    subsidy_insured       = fifelse(is.na(subsidy), 0, as.numeric(subsidy)) * insured_ind
  )]

  # Collapse CSR-enhanced silver short codes (e.g. ANT_SIL73 → ANT_SIL).
  # Done at the cell-data level so the demand model treats CSR variants as
  # one alternative.
  dt[, plan_id := gsub("SIL(94|73|87)", "SIL", plan_id)]

  setorder(dt, household_id, plan_id)

  # Rename for model interface
  setnames(dt, c("plan_choice", "net_premium", "household_id"),
               c("choice", "premium", "household_number"))

  # Exclusion restriction: penalty_own identifies outside option utility separately
  # from premium (which is 0 on uninsured row)
  dt[, penalty_own := fifelse(plan_id == "Uninsured",
                               penalty / 12 / hh_size, 0)]

  # Insurer x metal interactions
  dt[, `:=`(
    Anthem_silver = Anthem * silver,
    BS_silver     = Blue_Shield * silver,
    Kaiser_silver = Kaiser * silver,
    HN_silver     = Health_Net * silver,
    Anthem_bronze = Anthem * bronze,
    BS_bronze     = Blue_Shield * bronze,
    Kaiser_bronze = Kaiser * bronze,
    HN_bronze     = Health_Net * bronze
  )]

  # CF interactions (v_hat × plan indicators, for reduced-form selection correction)
  # Only fires when cf_* terms are in the spec
  if (!is.null(spec) && "v_hat" %in% names(dt)) {
    menu <- get_covariate_menu()
    cf_terms <- intersect(spec, names(menu))
    cf_terms <- cf_terms[sapply(cf_terms, function(s) identical(menu[[s]]$type, "cf_interaction"))]
    for (cf_term in cf_terms) {
      raw_col <- menu[[cf_term]]$raw_demo
      if (raw_col %in% names(dt)) {
        dt[, (cf_term) := v_hat * get(raw_col)]
      }
    }
  }

  # 9. Split by channel, keep valid choices, return as tibble
  # Build model_vars from spec (if provided) or use full set
  always_keep <- c("plan_id", "household_number", "choice", "premium",
                   "platinum", "gold", "silver", "bronze",
                   "av", "uninsured_plan", "hh_weight", "hh_size")
  # Raw demographics needed downstream for recomputing _prem interactions
  raw_demos <- c("perc_0to17", "perc_18to34", "perc_35to54",
                 "perc_male", "perc_black", "perc_hispanic",
                 "perc_asian", "perc_other",
                 "FPL_250to400", "FPL_400plus")

  if (!is.null(spec)) {
    # Spec-driven: include spec covariates + always-keep + raw demos
    model_vars <- unique(c(always_keep, raw_demos,
                           intersect(spec, names(dt))))
  } else {
    # Fallback: include everything that looks like a model variable
    model_vars <- unique(c(always_keep, raw_demos,
                           grep("_prem$|_silver$|_bronze$|^Anthem$|^Blue_Shield$|^Kaiser$|^Health_Net$|^penalty_own$|^cf_resid$|^hmo$|^hsa$",
                                names(dt), value = TRUE)))
  }
  if ("comm_pmpm" %in% names(dt)) model_vars <- c(model_vars, "comm_pmpm")
  if ("v_hat" %in% names(dt)) model_vars <- c(model_vars, "v_hat")
  if ("channel_detail" %in% names(dt)) model_vars <- c(model_vars, "channel_detail")
  if ("any_agent" %in% names(dt)) model_vars <- c(model_vars, "any_agent")
  model_vars <- unique(intersect(model_vars, names(dt)))

  untreated <- dt[channel == "Unassisted", ..model_vars]
  treated   <- dt[channel != "Unassisted", ..model_vars]
  rm(dt)

  # Keep only HH where exactly one plan is chosen (avoid .SD to prevent locked binding)
  valid_untr <- untreated[, .(ok = max(choice) == 1L), by = household_number][ok == TRUE, household_number]
  untreated <- untreated[household_number %in% valid_untr]

  valid_tr <- treated[, .(ok = max(choice) == 1L), by = household_number][ok == TRUE, household_number]
  treated <- treated[household_number %in% valid_tr]

  # Ensure OOS plans are subset of estimation plans
  est_plans <- unique(untreated$plan_id)
  treated <- treated[plan_id %in% est_plans]
  valid_tr2 <- treated[, .(ok = max(choice) == 1L), by = household_number][ok == TRUE, household_number]
  treated <- treated[household_number %in% valid_tr2]

  if (nrow(untreated) == 0 || nrow(treated) == 0) return(NULL)

  untreated[, assisted := 0L]
  treated[, assisted := 1L]

  # Assisted x metal interactions (structural path only)
  # Captures general effect of any assistance on metal tier choice
  if ("comm_pmpm" %in% names(untreated)) {
    untreated[, `:=`(
      assisted_silver = assisted * silver,
      assisted_bronze = assisted * bronze,
      assisted_gold   = assisted * gold,
      assisted_plat   = assisted * platinum
    )]
    treated[, `:=`(
      assisted_silver = assisted * silver,
      assisted_bronze = assisted * bronze,
      assisted_gold   = assisted * gold,
      assisted_plat   = assisted * platinum
    )]
  }

  # Commission x broker interaction (structural path only)
  # Only broker/agent-assisted HH receive commission steering; navigators do not
  if ("comm_pmpm" %in% names(untreated)) {
    if ("any_agent" %in% names(untreated)) {
      untreated[, commission_broker := comm_pmpm * fifelse(any_agent == 1L, assisted, 0L)]
      treated[, commission_broker := comm_pmpm * fifelse(any_agent == 1L, assisted, 0L)]
    } else {
      untreated[, commission_broker := comm_pmpm * assisted]
      treated[, commission_broker := comm_pmpm * assisted]
    }
  }

  # CF corrections (structural path only)
  if ("v_hat" %in% names(untreated) && "commission_broker" %in% names(untreated)) {
    untreated[, v_hat_commission := v_hat * commission_broker]
    treated[, v_hat_commission := v_hat * commission_broker]
  }

  as_tibble(rbind(untreated, treated))
}


# predict_nested_logit ----------------------------------------------------
#
# Compute nested logit choice probabilities directly from coefficients +
# data, avoiding predict.mlogit() environment capture issues.
#
# Args:
#   d          — long-format choice data (one row per HH x plan)
#   coefs      — tibble with columns `term` and `estimate` (from tidy())
#   nest_names — plan names in the "insured" nest
#
# Returns:
#   data.table with columns: household_number, plan_id, pred

predict_nested_logit <- function(d, coefs, nest_names) {

  coef_map <- setNames(coefs$estimate, coefs$term)
  lambda <- coef_map[["lambda"]]

  # Compute V_ij for each row
  V <- rep(0, nrow(d))

  # Apply all coefficients that have matching columns in the data
  # Handles core terms, demographics, insurer x region FEs, assisted interactions, etc.
  for (v in names(coef_map)) {
    if (v == "lambda") next
    if (v %in% names(d))
      V <- V + coef_map[[v]] * d[[v]]
  }

  # Nested logit probabilities via data.table
  dt <- as.data.table(d[, c("household_number", "plan_id")])
  dt[, V := V]

  # V_0 = β'X_0 for each HH (NOT zero)
  V0_by_hh <- dt[plan_id == "Uninsured", .(V_0 = V), by = household_number]

  # Insured plans
  ins <- dt[plan_id != "Uninsured"]
  ins <- merge(ins, V0_by_hh, by = "household_number", all.x = TRUE)
  ins[, V_scaled := V / lambda]
  ins[, max_V := max(V_scaled), by = household_number]
  ins[, exp_V := exp(V_scaled - max_V)]
  ins[, D := sum(exp_V), by = household_number]
  ins[, s_jg := exp_V / D]
  ins[, log_D := max_V + log(D)]
  ins[, log_D_lam := lambda * log_D]
  # P(insured) = exp(λI) / (exp(λI) + exp(V_0))
  ins[, mx := pmax(log_D_lam, V_0)]
  ins[, s_g := exp(log_D_lam - mx) / (exp(log_D_lam - mx) + exp(V_0 - mx))]
  ins[, pred := s_jg * s_g]

  # Uninsured probability = 1 - s_g
  sg_by_hh <- ins[, .(s_g = first(s_g)), by = household_number]
  unins <- dt[plan_id == "Uninsured"]
  unins <- merge(unins, sg_by_hh, by = "household_number", all.x = TRUE)
  unins[, pred := 1 - s_g]

  # Combine
  out <- rbind(
    ins[, .(household_number, plan_id, pred)],
    unins[, .(household_number, plan_id, pred)]
  )
  setorder(out, household_number, plan_id)
  out
}
