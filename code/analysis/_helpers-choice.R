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
#                  plan_name, issuer, network_type, metal_level, metal, premium, msp, hsa)
#   hhs          — hh_full rows for this region-year
#   sample_frac  — fraction of HH to sample per cell (estimation + OOS)
#
# Returns:
#   Combined estimation + OOS tibble with `assisted` flag.

build_choice_data <- function(plans, hhs, sample_frac) {

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

  # 1. Choice set: collapse plans to one row per plan_name, add Uninsured
  choice_set <- plans_dt[, .(
    issuer       = first(issuer),
    network_type = first(network_type),
    metal_level  = first(metal_level),
    metal        = first(metal),
    premium      = mean(premium, na.rm = TRUE),
    msp          = min(msp, na.rm = TRUE),
    hsa          = min(hsa, na.rm = TRUE),
    cf_resid     = first(cf_resid)
  ), by = plan_name]
  choice_set[, plan_name := as.character(plan_name)]

  # Carry commission PMPM if present (structural path only)
  if ("comm_pmpm" %in% names(plans_dt)) {
    cs_comm <- plans_dt[, .(comm_pmpm = mean(comm_pmpm, na.rm = TRUE)), by = plan_name]
    choice_set <- merge(choice_set, cs_comm, by = "plan_name", all.x = TRUE)
    choice_set[is.na(comm_pmpm), comm_pmpm := 0]
  }

  uninsured_row <- data.table(
    plan_name = "Uninsured", issuer = "Outside_Option",
    network_type = NA_character_, metal_level = NA_character_,
    metal = NA_character_, premium = NA_real_,
    msp = NA_real_, hsa = NA_real_, cf_resid = 0
  )
  if ("comm_pmpm" %in% names(choice_set)) uninsured_row$comm_pmpm <- 0
  choice_set <- rbind(choice_set, uninsured_row)

  # 2. Cross-join sampled HH x choice set
  hh_slim <- hhs_dt[, .(
    household_id, FPL, subsidized_members, rating_factor,
    hh_plan_number = plan_number_nocsr, hh_plan_name = plan_name,
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
    (metal_level == "Silver - Enhanced 73" & csr_73 == 1L) |
    (metal_level == "Silver - Enhanced 87" & csr_87 == 1L) |
    (metal_level == "Silver - Enhanced 94" & csr_94 == 1L) |
    metal != "Silver" |
    (metal_level == "Silver" & csr_73 == 0L & csr_87 == 0L & csr_94 == 0L) |
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
       eff_premium > threshold & metal_level == "Minimum Coverage") |
    metal_level != "Minimum Coverage" | is.na(metal)
  ]
  dt[, c("eff_premium", "threshold") := NULL]

  # 5. Plan choice indicator and premiums
  dt[, plan_choice := fifelse(
    hh_plan_name == plan_name & !is.na(hh_plan_name) & !is.na(hh_plan_number),
    1L, 0L
  )]
  dt[, insured := max(plan_choice), by = household_id]

  dt[, `:=`(
    adj_subsidy = fifelse(is.na(subsidy), 0, subsidy),
    hh_premium  = (premium / RATING_FACTOR_AGE40) * rating_factor
  )]
  dt[, final_premium := fcase(
    issuer == "Outside_Option",        penalty / 12,
    metal_level == "Minimum Coverage", hh_premium,
    default = pmax(hh_premium - adj_subsidy, 0)
  )]
  dt[, av := fcase(
    metal_level == "Minimum Coverage",     0.55,
    metal_level == "Bronze",               0.60,
    metal_level == "Silver",               0.70,
    metal_level == "Gold",                 0.80,
    metal_level == "Platinum",             0.90,
    metal_level == "Silver - Enhanced 73", 0.73,
    metal_level == "Silver - Enhanced 87", 0.87,
    metal_level == "Silver - Enhanced 94", 0.94,
    issuer == "Outside_Option",            0,
    default = NA_real_
  )]
  dt[, c("adj_subsidy", "hh_premium", "premium") := NULL]

  # 6. Collapse small insurers into one per metal tier
  big_four <- c("Anthem", "Blue_Shield", "Kaiser", "Health_Net")

  large <- dt[issuer %in% c(big_four, "Outside_Option")]
  small_raw <- dt[!issuer %in% c(big_four, "Outside_Option")]
  rm(dt)

  has_comm <- "comm_pmpm" %in% names(small_raw)

  if (nrow(small_raw) > 0) {
    agg_exprs <- quote(.(
      final_premium  = min(final_premium, na.rm = TRUE),
      plan_choice    = max(plan_choice, na.rm = TRUE),
      FPL            = first(FPL),
      hh_plan_name   = first(hh_plan_name),
      hh_plan_number = first(hh_plan_number),
      oldest_member  = first(oldest_member),
      insured        = first(insured),
      penalty        = first(penalty),
      hsa            = mean(hsa, na.rm = TRUE),
      av             = mean(av, na.rm = TRUE),
      cf_resid       = mean(cf_resid, na.rm = TRUE)
    ))
    small <- small_raw[, eval(agg_exprs), by = .(household_id, metal)]
    if (has_comm) {
      comm_agg <- small_raw[, .(comm_pmpm = mean(comm_pmpm, na.rm = TRUE)),
                             by = .(household_id, metal)]
      small <- merge(small, comm_agg, by = c("household_id", "metal"), all.x = TRUE)
    }
    small[, `:=`(
      issuer = "Small_Insurer",
      plan_name = fcase(
        metal == "Platinum",         "Small_P",
        metal == "Gold",             "Small_G",
        metal == "Silver",           "Small_SIL",
        metal == "Bronze",           "Small_BR",
        metal == "Minimum Coverage", "Small_CAT",
        default = NA_character_
      )
    )]
    dt <- rbind(large, small, fill = TRUE)
    rm(large, small)
  } else {
    dt <- large
    rm(large)
  }
  rm(small_raw)

  # 7. Join HH demographics
  hh_demo <- hhs_dt[, .(
    household_id, hh_size = household_size, ipweight,
    perc_0to17, perc_18to34, perc_35to54,
    perc_black, perc_hispanic, perc_asian,
    perc_other, perc_male, channel
  )]
  dt <- merge(dt, hh_demo, by = "household_id", all.x = TRUE)
  rm(hhs_dt, hh_demo)

  dt[is.na(metal), metal := "Other"]
  dt[, plan_choice := fcase(
    plan_choice == 1L & insured == 1L, 1L,
    plan_choice == 0L & insured == 0L & plan_name == "Uninsured" & is.na(hh_plan_number), 1L,
    default = 0L
  )]

  # 8. Final variables
  dt <- dt[!is.na(final_premium) & !is.na(plan_name)]
  dt[, `:=`(
    net_premium    = final_premium / hh_size,
    hmo            = fifelse(fifelse(is.na(network_type), "", network_type) == "HMO", 1L, 0L),
    hsa            = fifelse(is.na(hsa) | hsa <= 0, 0L, 1L),
    FPL_250to400   = fifelse(FPL > 2.50 & FPL <= 4.00, 1L, 0L),
    FPL_400plus    = fifelse(FPL > 4.00, 1L, 0L),
    any_0to17      = fifelse(perc_0to17 > 0, 1L, 0L),
    any_black      = fifelse(perc_black > 0, 1L, 0L),
    any_hispanic   = fifelse(perc_hispanic > 0, 1L, 0L),
    uninsured_plan = fifelse(plan_name == "Uninsured", 1L, 0L),
    platinum       = fifelse(metal == "Platinum", 1L, 0L),
    gold           = fifelse(metal == "Gold", 1L, 0L),
    silver         = fifelse(metal == "Silver", 1L, 0L),
    bronze         = fifelse(metal == "Bronze", 1L, 0L),
    Anthem         = fifelse(issuer == "Anthem", 1L, 0L),
    Blue_Shield    = fifelse(issuer == "Blue_Shield", 1L, 0L),
    Kaiser         = fifelse(issuer == "Kaiser", 1L, 0L),
    Health_Net     = fifelse(issuer == "Health_Net", 1L, 0L),
    ipweight       = fifelse(is.na(ipweight), 1, ipweight)
  )]
  dt[, `:=`(
    hh_size_prem      = hh_size * net_premium,
    any_0to17_prem    = any_0to17 * net_premium,
    any_black_prem    = any_black * net_premium,
    any_hispanic_prem = any_hispanic * net_premium,
    FPL_250to400_prem = FPL_250to400 * net_premium,
    FPL_400plus_prem  = FPL_400plus * net_premium
  )]

  # Collapse enhanced silver plan names to SIL
  dt[, plan_name := gsub("SIL(94|73|87)", "SIL", plan_name)]

  setorder(dt, household_id, plan_name)

  # Rename for model interface
  setnames(dt, c("plan_choice", "net_premium", "household_id"),
               c("choice", "premium", "household_number"))

  # Exclusion restriction: penalty_own identifies outside option scale separately
  dt[, penalty_own := fifelse(plan_name == "Uninsured", premium, 0)]

  # Nonlinear price effect (0 for uninsured)
  dt[, premium_sq := fifelse(plan_name == "Uninsured", 0, premium^2)]

  # Insurer x metal interactions (plan-level cross-alternative variation)
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

  # 9. Split by channel, keep valid choices, return as tibble
  model_vars <- c("plan_name", "household_number", "choice", "premium",
                   "penalty_own", "premium_sq",
                   "platinum", "gold", "silver", "bronze", "hsa", "hmo",
                   "av", "uninsured_plan", "cf_resid",
                   "hh_size_prem", "any_0to17_prem", "FPL_250to400_prem",
                   "FPL_400plus_prem", "any_black_prem", "any_hispanic_prem",
                   "hh_size", "any_0to17", "FPL_250to400", "FPL_400plus",
                   "any_black", "any_hispanic",
                   "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
                   "Anthem_silver", "BS_silver", "Kaiser_silver", "HN_silver",
                   "Anthem_bronze", "BS_bronze", "Kaiser_bronze", "HN_bronze",
                   "ipweight")
  if ("comm_pmpm" %in% names(dt)) model_vars <- c(model_vars, "comm_pmpm")

  untreated <- dt[channel == "Unassisted", ..model_vars]
  treated   <- dt[channel != "Unassisted", ..model_vars]
  rm(dt)

  # Keep only HH where exactly one plan is chosen (avoid .SD to prevent locked binding)
  valid_untr <- untreated[, .(ok = max(choice) == 1L), by = household_number][ok == TRUE, household_number]
  untreated <- untreated[household_number %in% valid_untr]

  valid_tr <- treated[, .(ok = max(choice) == 1L), by = household_number][ok == TRUE, household_number]
  treated <- treated[household_number %in% valid_tr]

  # Ensure OOS plans are subset of estimation plans
  est_plans <- unique(untreated$plan_name)
  treated <- treated[plan_name %in% est_plans]
  valid_tr2 <- treated[, .(ok = max(choice) == 1L), by = household_number][ok == TRUE, household_number]
  treated <- treated[household_number %in% valid_tr2]

  if (nrow(untreated) == 0 || nrow(treated) == 0) return(NULL)

  untreated[, assisted := 0L]
  treated[, assisted := 1L]

  # Commission x broker interaction (structural path only)
  if ("comm_pmpm" %in% names(untreated)) {
    untreated[, commission_broker := comm_pmpm * assisted]  # all 0
    treated[, commission_broker := comm_pmpm * assisted]    # comm_pmpm values
  }

  as_tibble(rbind(untreated, treated))
}


# estimate_nested_logit ---------------------------------------------------
#
# Estimates nested logit on one region-year cell.
#
# Args:
#   d           — estimation data (long format, unassisted HH only)
#   nest_names  — plan names in the "insured" nest (everything except Uninsured)
#
# Returns:
#   Fitted mlogit object, or NULL if estimation fails.

estimate_nested_logit <- function(d, nest_names, pooled = FALSE,
                                   include_commission = FALSE) {

  # Covariate selection
  covars <- c("premium", "penalty_own", "premium_sq",
              "silver", "bronze", "hh_size_prem",
              "cf_resid",
              "any_0to17_prem", "FPL_250to400_prem", "FPL_400plus_prem",
              "any_black_prem", "any_hispanic_prem")

  if (pooled) {
    # Always include all covariates when pooling across cells
    covars <- c(covars, "hmo", "hsa",
                "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
                "Anthem_silver", "BS_silver", "Kaiser_silver", "HN_silver",
                "Anthem_bronze", "BS_bronze", "Kaiser_bronze", "HN_bronze")
  } else {
    # Adaptive selection for single-cell estimation
    tot_chosen <- sum(d$choice == 1)

    if (sum(d$hmo == 1 & d$choice == 1) / tot_chosen > 0.1)
      covars <- c(covars, "hmo")
    if (sum(d$hsa == 1 & d$choice == 1) / tot_chosen > 0.1)
      covars <- c(covars, "hsa")

    ins_share <- c(
      Anthem      = sum(d$Anthem == 1 & d$choice == 1) / tot_chosen,
      Blue_Shield = sum(d$Blue_Shield == 1 & d$choice == 1) / tot_chosen,
      Kaiser      = sum(d$Kaiser == 1 & d$choice == 1) / tot_chosen,
      Health_Net  = sum(d$Health_Net == 1 & d$choice == 1) / tot_chosen
    )
    cml <- 0
    for (ins in names(ins_share)) {
      cml <- cml + ins_share[ins]
      if (ins_share[ins] > 0.4 & cml < 0.9)
        covars <- c(covars, ins)
    }
  }

  # Commission steering covariate (structural path only)
  if (include_commission && "commission_broker" %in% names(d))
    covars <- c(covars, "commission_broker")

  # Format data for mlogit (mlogit.data works with | 0 formula; dfidx does not)
  nested_data <- mlogit.data(d, choice = "choice", shape = "long",
                              chid.var = "household_number",
                              alt.var = "plan_name")

  nested_formula <- as.formula(
    paste("choice ~", paste(covars, collapse = " + "), "| 0")
  )

  # Try nested logit; fall back to GLM starting values on failure
  result <- tryCatch({
    mlogit(nested_formula, data = nested_data, weights = ipweight,
           nests = list(insured = nest_names, uninsured = "Uninsured"),
           un.nest.el = TRUE)
  }, error = function(e1) {
    tryCatch({
      logit_formula <- as.formula(
        paste("choice ~", paste(covars, collapse = " + "))
      )
      logit_start <- glm(logit_formula, data = d, family = "binomial")
      start_vals <- logit_start$coefficients[-1]
      mlogit(nested_formula, data = nested_data, weights = ipweight,
             nests = list(insured = nest_names, uninsured = "Uninsured"),
             un.nest.el = TRUE, start = start_vals)
    }, error = function(e2) {
      NULL
    })
  })

  result
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
#   data.table with columns: household_number, plan_name, pred

predict_nested_logit <- function(d, coefs, nest_names) {

  coef_map <- setNames(coefs$estimate, coefs$term)
  lambda <- coef_map[["lambda"]]

  # Compute V_ij for each row
  V <- rep(0, nrow(d))

  # Core covariates (always present)
  core_terms <- c("premium", "penalty_own", "premium_sq",
                  "silver", "bronze", "hh_size_prem", "cf_resid",
                  "any_0to17_prem", "FPL_250to400_prem", "FPL_400plus_prem",
                  "any_black_prem", "any_hispanic_prem")
  for (v in core_terms) {
    if (v %in% names(coef_map) && v %in% names(d))
      V <- V + coef_map[[v]] * d[[v]]
  }

  # Adaptive covariates
  for (v in c("hmo", "hsa", "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
              "Anthem_silver", "BS_silver", "Kaiser_silver", "HN_silver",
              "Anthem_bronze", "BS_bronze", "Kaiser_bronze", "HN_bronze",
              "commission_broker")) {
    if (v %in% names(coef_map) && v %in% names(d))
      V <- V + coef_map[[v]] * d[[v]]
  }

  # Nested logit probabilities via data.table
  dt <- as.data.table(d[, c("household_number", "plan_name")])
  dt[, V := V]

  # V_0 = β'X_0 for each HH (NOT zero)
  V0_by_hh <- dt[plan_name == "Uninsured", .(V_0 = V), by = household_number]

  # Insured plans
  ins <- dt[plan_name != "Uninsured"]
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
  unins <- dt[plan_name == "Uninsured"]
  unins <- merge(unins, sg_by_hh, by = "household_number", all.x = TRUE)
  unins[, pred := 1 - s_g]

  # Combine
  out <- rbind(
    ins[, .(household_number, plan_name, pred)],
    unins[, .(household_number, plan_name, pred)]
  )
  setorder(out, household_number, plan_name)
  out
}
