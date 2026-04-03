# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-06
## Description:   Helper functions for supply-side estimation (markup recovery,
##                commission FOC evaluation). Used by 7_supply-simple.R.


# Constants ---------------------------------------------------------------

# Moral hazard loading by metal tier (relative to Bronze/CAT baseline)
MH_FACTOR <- c(
  "Minimum Coverage" = 1.00,
  "Bronze"           = 1.00,
  "Silver"           = 1.03,
  "Gold"             = 1.08,
  "Platinum"         = 1.15
)


# identify_benchmark ------------------------------------------------------
#
# Returns the plan_name of the second-cheapest Silver plan by posted premium
# in a given region-year. If only one Silver plan exists, returns that one.

identify_benchmark <- function(plan_attrs) {
  silver <- plan_attrs[plan_attrs$metal == "Silver", ]
  silver <- silver[order(silver$premium_posted), ]
  if (nrow(silver) == 0) return(NA_character_)
  if (nrow(silver) == 1) return(silver$plan_name[1])
  silver$plan_name[2]
}


# build_supply_choice_data ------------------------------------------------
#
# Like build_choice_data() but retains rating_factor, adj_subsidy, subsidized
# flag, and hh_premium. Must use the SAME seed and SAMPLE_FRAC as point
# estimation to get the identical HH sample.

build_supply_choice_data <- function(plans, hhs, sample_frac, weight_var = "hh_size",
                                     spec = NULL) {

  hhs_dt <- as.data.table(hhs)
  plans_dt <- as.data.table(plans)

  # 0. Sample HH by channel (identical logic to build_choice_data)
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

  # 1. Choice set (one row per plan_name + Uninsured)
  choice_set <- plans_dt[, .(
    issuer         = first(issuer),
    network_type   = first(network_type),
    metal_level    = first(metal_level),
    metal          = first(metal),
    premium_posted = mean(premium, na.rm = TRUE),
    msp            = min(msp, na.rm = TRUE),
    hsa            = min(hsa, na.rm = TRUE),
    cf_resid       = first(cf_resid)
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
    metal = NA_character_, premium_posted = NA_real_,
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
    (metal_level == "Silver - Enhanced 73" & csr_73 == 1L) |
    (metal_level == "Silver - Enhanced 87" & csr_87 == 1L) |
    (metal_level == "Silver - Enhanced 94" & csr_94 == 1L) |
    metal != "Silver" |
    (metal_level == "Silver" & csr_73 == 0L & csr_87 == 0L & csr_94 == 0L) |
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
       eff_premium > threshold & metal_level == "Minimum Coverage") |
    metal_level != "Minimum Coverage" | is.na(metal)
  ]
  dt[, c("eff_premium", "threshold") := NULL]

  # 5. Plan choice indicator and premiums (RETAIN rating_factor, subsidy)
  dt[, plan_choice := fifelse(
    hh_plan_name == plan_name & !is.na(hh_plan_name) & !is.na(hh_plan_number),
    1L, 0L
  )]
  dt[, insured := max(plan_choice), by = household_id]

  # Posted premium: age-adjusted posted premium (no subsidy deduction).
  # Subsidy effects captured via FPL x premium interactions in demand model.
  dt[, premium_hh := (premium_posted / RATING_FACTOR_AGE40) * rating_factor]
  # Outside option premium = 0. Penalty effect captured by penalty_own.
  # Keeps β_premium identified only from insured plan variation.
  dt[, premium_oop := fcase(
    issuer == "Outside_Option",        0.0,
    default = premium_hh
  )]
  # adj_subsidy and subsidized still needed for benchmark elasticity split
  dt[, adj_subsidy := fifelse(is.na(subsidy), 0, subsidy)]
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
  dt[, subsidized := fifelse(subsidized_members > 0, 1L, 0L)]

  # premium_posted kept on data for supply-side use

  # 6. Collapse small insurers
  big_four <- c("Anthem", "Blue_Shield", "Kaiser", "Health_Net")

  large <- dt[issuer %in% c(big_four, "Outside_Option")]
  small_raw <- dt[!issuer %in% c(big_four, "Outside_Option")]
  rm(dt)

  has_comm <- "comm_pmpm" %in% names(small_raw)

  if (nrow(small_raw) > 0) {
    small <- small_raw[, .(
      premium_oop    = min(premium_oop, na.rm = TRUE),
      plan_choice    = max(plan_choice, na.rm = TRUE),
      FPL            = first(FPL),
      hh_plan_name   = first(hh_plan_name),
      hh_plan_number = first(hh_plan_number),
      oldest_member  = first(oldest_member),
      insured        = first(insured),
      penalty        = first(penalty),
      hsa            = mean(hsa, na.rm = TRUE),
      av             = mean(av, na.rm = TRUE),
      cf_resid       = mean(cf_resid, na.rm = TRUE),
      rating_factor  = first(rating_factor),
      adj_subsidy    = first(adj_subsidy),
      subsidized     = first(subsidized),
      premium_hh     = min(premium_hh, na.rm = TRUE),
      premium_posted = min(premium_posted, na.rm = TRUE)
    ), by = .(household_id, metal)]
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

  # 7. Join HH demographics (include v_hat + tau-gradient cols if available)
  demo_cols <- c("household_id", "household_size", "ipweight",
                 "perc_0to17", "perc_18to34", "perc_35to54",
                 "perc_black", "perc_hispanic", "perc_asian",
                 "perc_other", "perc_male", "channel")
  for (extra in c("v_hat", "channel_detail", "any_agent", "p_nav")) {
    if (extra %in% names(hhs_dt)) demo_cols <- c(demo_cols, extra)
  }
  hh_demo <- hhs_dt[, ..demo_cols]
  setnames(hh_demo, "household_size", "hh_size")
  dt <- merge(dt, hh_demo, by = "household_id", all.x = TRUE)
  rm(hhs_dt, hh_demo)

  dt[is.na(metal), metal := "Other"]
  dt[, plan_choice := fcase(
    plan_choice == 1L & insured == 1L, 1L,
    plan_choice == 0L & insured == 0L & plan_name == "Uninsured" & is.na(hh_plan_number), 1L,
    default = 0L
  )]

  # 8. Final variables
  dt <- dt[!is.na(premium_oop) & !is.na(plan_name)]
  dt[, `:=`(
    net_premium    = premium_oop / hh_size,
    hmo            = fifelse(fifelse(is.na(network_type), "", network_type) == "HMO", 1L, 0L),
    hsa            = fifelse(is.na(hsa) | hsa <= 0, 0L, 1L),
    FPL_250to400   = fifelse(FPL > 2.50 & FPL <= 4.00, 1L, 0L),
    FPL_400plus    = fifelse(FPL > 4.00, 1L, 0L),
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

  # Override weight for structural estimation (household_size instead of IPW)
  if (weight_var == "hh_size") {
    dt[, ipweight := as.numeric(hh_size)]
  }

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

  # Demographic x insured interactions (cross-nest margin shifters)
  insured_ind <- fifelse(dt$plan_name == "Uninsured", 0, 1)
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
  dt[, plan_name := gsub("SIL(94|73|87)", "SIL", plan_name)]

  # 9. Build plan_attrs — canonical plan attribute table (post-collapse)
  plan_attrs <- dt[plan_name != "Uninsured", .(
    issuer         = first(issuer),
    metal          = first(metal),
    network_type   = first(network_type),
    av             = min(av, na.rm = TRUE),  # base metal AV (not CSR-enhanced)
    hmo            = as.integer(fifelse(is.na(first(network_type)), "", first(network_type)) == "HMO"),
    hsa            = as.integer(!is.na(first(hsa)) & first(hsa) > 0),
    premium_posted = mean(premium_posted, na.rm = TRUE),
    cf_resid       = first(cf_resid)
  ), by = plan_name]
  if ("comm_pmpm" %in% names(dt)) {
    comm_by_plan <- dt[plan_name != "Uninsured",
                        .(comm_pmpm = mean(comm_pmpm, na.rm = TRUE)), by = plan_name]
    plan_attrs <- merge(plan_attrs, comm_by_plan, by = "plan_name", all.x = TRUE)
    plan_attrs[is.na(comm_pmpm), comm_pmpm := 0]
  }

  setorder(dt, household_id, plan_name)

  # Rename for model interface
  setnames(dt, c("plan_choice", "net_premium", "household_id"),
               c("choice", "premium", "household_number"))

  # Exclusion restriction + interactions (same as build_choice_data)
  # penalty_own identifies outside option utility separately from premium
  dt[, penalty_own := fifelse(plan_name == "Uninsured",
                               penalty / 12 / hh_size, 0)]
  dt[, `:=`(
    Anthem_silver = fifelse(issuer == "Anthem", 1L, 0L) * fifelse(metal == "Silver", 1L, 0L),
    BS_silver     = fifelse(issuer == "Blue_Shield", 1L, 0L) * fifelse(metal == "Silver", 1L, 0L),
    Kaiser_silver = fifelse(issuer == "Kaiser", 1L, 0L) * fifelse(metal == "Silver", 1L, 0L),
    HN_silver     = fifelse(issuer == "Health_Net", 1L, 0L) * fifelse(metal == "Silver", 1L, 0L),
    Anthem_bronze = fifelse(issuer == "Anthem", 1L, 0L) * fifelse(metal == "Bronze", 1L, 0L),
    BS_bronze     = fifelse(issuer == "Blue_Shield", 1L, 0L) * fifelse(metal == "Bronze", 1L, 0L),
    Kaiser_bronze = fifelse(issuer == "Kaiser", 1L, 0L) * fifelse(metal == "Bronze", 1L, 0L),
    HN_bronze     = fifelse(issuer == "Health_Net", 1L, 0L) * fifelse(metal == "Bronze", 1L, 0L)
  )]

  # Keep ALL households for supply-side aggregation
  dt[, assisted := fifelse(channel != "Unassisted", 1L, 0L)]

  # Commission x broker interaction (structural path only)
  if ("comm_pmpm" %in% names(dt)) {
    dt[, commission_broker := comm_pmpm * assisted]
  }

  # CF x commission interaction (structural path only)
  if ("v_hat" %in% names(dt) && "commission_broker" %in% names(dt)) {
    dt[, v_hat_commission := v_hat * commission_broker]
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

compute_utility <- function(cell_data, coefs_cell) {

  coef_map <- setNames(coefs_cell$estimate, coefs_cell$term)
  lambda <- coef_map[["lambda"]]

  V <- rep(0, nrow(cell_data))

  # Apply all coefficients that have matching columns in the data
  for (v in names(coef_map)) {
    if (v == "lambda") next
    if (v %in% names(cell_data))
      V <- V + coef_map[[v]] * cell_data[[v]]
  }

  list(V = V, lambda = lambda)
}


# compute_alpha_i ----------------------------------------------------------
#
# Compute heterogeneous price sensitivity alpha_i for each row.
# alpha_i = (beta_p + beta_h * hh_size + ...) / hh_size
# Used by compute_shares_and_elasticities and by 2_supply.R for two-stage demand.

# Generic: uses get_prem_interactions() from covariates.R to detect which
# demographic x premium terms are in the spec. Adding/removing a _prem
# variable in the spec automatically updates this derivative.
#
# Falls back to detecting _prem terms from coef_map if spec is NULL.

compute_alpha_i <- function(cell_data, coefs, spec = NULL) {
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

  for (nm in names(prem_ints)) {
    raw_col <- prem_ints[[nm]]
    if (raw_col %in% names(cell_data))
      dVdp <- dVdp + get_coef(nm) * cell_data[[raw_col]]
  }

  dVdp / cell_data$hh_size
}


# compute_shares_and_elasticities -----------------------------------------
#
# Compute market shares and J x J derivative matrix dshare_j/dposted_l.
# Uses vectorized data.table operations for non-benchmark columns.
# Benchmark column uses closed-form expressions for subsidized HH.
#
# If cell_data contains pre-computed columns `alpha_i` and `lambda_i`,
# those are used directly (two-stage demand with group-specific parameters).
# Otherwise, alpha_i is computed from coefs_cell and scalar lambda is used.
#
# Nested logit derivative (j, l both in insured nest):
#   dq_ij/dV_il = q_ij * [I(j==l)/lambda + ((lambda-1)/lambda)*s_{il|g} - q_il]
#
# Chain rule from V to posted premium:
#   dV_ij/d(net_premium_j) = alpha_i  (heterogeneous price sensitivity)
#   d(net_premium_j)/d(posted_l) depends on benchmark/subsidy status (4-case rule)
#
# For non-benchmark l:
#   Only V_il changes: dq_ij/d(posted_l) = dq_ij/dV_il * alpha_i * rf_i
#
# For benchmark l, subsidized HH:
#   V_il unchanged (subsidy absorbs). All other V_ik (k != l) change.
#   Closed-form sum (derived by direct algebra):
#     j != l: alpha * (-rf) * q_j * [1/lambda + (1 - s_lg) * ((lambda-1)/lambda - s_g)]
#     j == l: alpha * (-rf) * q_l * [(1 - s_lg) * ((lambda-1)/lambda - s_g)]

compute_shares_and_elasticities <- function(cell_data, V, lambda, benchmark_plan,
                                             plans_cell, coefs_cell, spec = NULL) {

  dt <- as.data.table(cell_data)
  dt[, V := V]

  plan_names <- sort(unique(dt$plan_name[dt$plan_name != "Uninsured"]))
  J <- length(plan_names)

  # V_0 = β'X_0 for each HH (NOT zero)
  V0_by_hh <- dt[plan_name == "Uninsured", .(V_0 = V), by = household_number]

  # --- Step 1: Choice probabilities per HH-plan ---
  ins_dt <- dt[plan_name != "Uninsured"]
  ins_dt <- merge(ins_dt, V0_by_hh, by = "household_number", all.x = TRUE)

  # Use per-row lambda if pre-computed, else scalar
  has_lambda_i <- "lambda_i" %in% names(ins_dt)
  if (!has_lambda_i) ins_dt[, lambda_i := lambda]

  # Numerically stable log-sum-exp per HH
  ins_dt[, V_scaled := V / lambda_i]
  ins_dt[, max_V_scaled := max(V_scaled), by = household_number]
  ins_dt[, exp_V := exp(V_scaled - max_V_scaled)]
  ins_dt[, sum_exp_V := sum(exp_V), by = household_number]
  ins_dt[, log_D := max_V_scaled + log(sum_exp_V)]

  # Conditional share within insured nest: s_{j|g}
  ins_dt[, s_jg := exp_V / sum_exp_V]

  # Nest probability: s_g = exp(λI) / (exp(λI) + exp(V_0))
  ins_dt[, log_D_lam := lambda_i * log_D]
  ins_dt[, mx := pmax(log_D_lam, V_0)]
  ins_dt[, s_g := exp(log_D_lam - mx) / (exp(log_D_lam - mx) + exp(V_0 - mx))]

  # Choice probability: q_j = s_{j|g} * s_g
  ins_dt[, q_j := s_jg * s_g]

  # HH-level price sensitivity: use pre-computed if available, else derive from coefs_cell
  if (!("alpha_i" %in% names(ins_dt))) {
    alpha_vec <- compute_alpha_i(ins_dt, coefs_cell, spec)
    ins_dt[, alpha_i := alpha_vec]
  }
  ins_dt[, rf_i := rating_factor / RATING_FACTOR_AGE40]

  # --- Step 2: Weighted market shares ---
  total_weight <- ins_dt[, .(w = first(ipweight)), by = household_number][, sum(w)]

  shares_dt <- ins_dt[, .(share = sum(ipweight * q_j) / total_weight), by = plan_name]
  shares <- setNames(shares_dt$share, shares_dt$plan_name)
  shares <- shares[plan_names]

  # --- Step 3: Elasticity matrix (J x J) ---
  elast_mat <- matrix(0, nrow = J, ncol = J, dimnames = list(plan_names, plan_names))

  for (l_idx in seq_along(plan_names)) {
    l <- plan_names[l_idx]
    is_benchmark <- (!is.na(benchmark_plan) && l == benchmark_plan)

    # Get s_{l|g} and q_l for each HH
    l_info <- ins_dt[plan_name == l, .(household_number, s_lg = s_jg, q_l = q_j)]
    merged <- merge(ins_dt, l_info, by = "household_number", all.x = TRUE)
    merged[is.na(s_lg), s_lg := 0]
    merged[is.na(q_l), q_l := 0]

    if (!is_benchmark) {
      # Non-benchmark: only V_l changes when posted_l changes
      # dq_j/d(posted_l) = dq_j/dV_l * alpha * rf
      # dq_j/dV_l = q_j * [I(j==l)/lambda_i + ((lambda_i-1)/lambda_i)*s_lg - q_l]
      merged[, dq_dposted := q_j * (as.numeric(plan_name == l) / lambda_i +
                                       ((lambda_i - 1) / lambda_i) * s_lg - q_l) *
                                alpha_i * rf_i]

      contrib <- merged[plan_name %in% plan_names,
                          .(elast = sum(ipweight * dq_dposted) / total_weight),
                          by = plan_name]
      for (j_idx in seq_along(plan_names)) {
        val <- contrib[plan_name == plan_names[j_idx], elast]
        if (length(val) > 0) elast_mat[j_idx, l_idx] <- val
      }
      rm(merged, l_info, contrib)

    } else {
      # Benchmark column: split by subsidized status
      #
      # Unsubsidized HH: same as non-benchmark (subsidy doesn't apply)
      # Subsidized HH (closed-form):
      #   j != l: alpha*(-rf)*q_j * [1/lambda_i + (1-s_lg)*((lambda_i-1)/lambda_i - s_g)]
      #   j == l: alpha*(-rf)*q_l * [(1-s_lg)*((lambda_i-1)/lambda_i - s_g)]

      # Unsubsidized contribution
      unsub <- merged[subsidized == 0L]
      if (nrow(unsub) > 0) {
        unsub[, dq_dposted := q_j * (as.numeric(plan_name == l) / lambda_i +
                                        ((lambda_i - 1) / lambda_i) * s_lg - q_l) *
                                 alpha_i * rf_i]
        contrib_unsub <- unsub[plan_name %in% plan_names,
                                 .(elast = sum(ipweight * dq_dposted) / total_weight),
                                 by = plan_name]
      } else {
        contrib_unsub <- data.table(plan_name = character(0), elast = numeric(0))
      }

      # Subsidized contribution (closed-form)
      sub <- merged[subsidized == 1L]
      if (nrow(sub) > 0) {
        sub[, common_factor := (1 - s_lg) * ((lambda_i - 1) / lambda_i - s_g)]
        sub[, dq_dposted := fifelse(
          plan_name == l,
          alpha_i * (-rf_i) * q_j * common_factor,
          alpha_i * (-rf_i) * q_j * (1 / lambda_i + common_factor)
        )]
        contrib_sub <- sub[plan_name %in% plan_names,
                             .(elast = sum(ipweight * dq_dposted) / total_weight),
                             by = plan_name]
      } else {
        contrib_sub <- data.table(plan_name = character(0), elast = numeric(0))
      }

      # Combine
      for (j_idx in seq_along(plan_names)) {
        j <- plan_names[j_idx]
        val_unsub <- contrib_unsub[plan_name == j, elast]
        val_sub   <- contrib_sub[plan_name == j, elast]
        total <- (if (length(val_unsub) > 0) val_unsub else 0) +
                 (if (length(val_sub) > 0) val_sub else 0)
        elast_mat[j_idx, l_idx] <- total
      }
      rm(merged, l_info, unsub, sub, contrib_unsub, contrib_sub)
    }
  }

  list(shares = shares, elast_mat = elast_mat, plan_names = plan_names)
}


# build_ownership_matrix --------------------------------------------------
#
# J x J binary matrix where (j, l) = 1 if same firm.
# Prefix before first underscore determines insurer.

build_ownership_matrix <- function(plan_names) {
  insurers <- sub("_.*", "", plan_names)
  outer(insurers, insurers, "==") * 1L
}


# build_commission_lookup -------------------------------------------------
#
# Standardize commission_input.csv into a lookup table.
# Values < 1 are percentages; values >= 1 are flat PMPM.
# Averages New/Renewal rates.

build_commission_lookup <- function(commission_raw) {

  prefix_map <- c(
    "Anthem"             = "ANT",
    "Blue Shield"        = "BS",
    "Health Net"         = "HN",
    "Kaiser"             = "KA",
    "Oscar"              = "Small",
    "LA Care"            = "Small",
    "Chinese Community"  = "Small",
    "Sharp"              = "Small",
    "Molina"             = "Small",
    "Valley"             = "Small",
    "Western"            = "Small",
    "United"             = "Small"
  )

  long <- commission_raw %>%
    pivot_longer(starts_with("Y"), names_to = "year_col", values_to = "rate") %>%
    mutate(
      year = as.integer(gsub("Y", "", year_col)),
      insurer_prefix = prefix_map[Insurer]
    ) %>%
    filter(!is.na(rate), !is.na(insurer_prefix))

  # Determine is_pct BEFORE averaging: use the original rate to classify
  # (a small insurer mix of flat + pct would be misleading, but Sharp is the
  #  only percentage-based small insurer and its share is tiny)
  long %>%
    mutate(is_pct = rate < 1) %>%
    group_by(insurer_prefix, year) %>%
    summarize(
      rate = mean(rate, na.rm = TRUE),
      is_pct = any(is_pct),
      .groups = "drop"
    )
}


# get_commission_pmpm -----------------------------------------------------
#
# For each plan, look up insurer's commission rate and convert to PMPM.

get_commission_pmpm <- function(plan_names, plans_cell, year, commission_lookup) {

  result <- setNames(rep(0, length(plan_names)), plan_names)

  for (i in seq_along(plan_names)) {
    pn <- plan_names[i]
    prefix <- sub("_.*", "", pn)

    cl <- commission_lookup %>%
      filter(insurer_prefix == prefix, year == !!year)

    if (nrow(cl) == 0) next

    rate <- cl$rate[1]
    is_pct <- cl$is_pct[1]

    if (is_pct) {
      posted <- plans_cell %>%
        filter(plan_name == pn) %>%
        pull(premium) %>%
        mean(na.rm = TRUE)
      result[i] <- rate * posted
    } else {
      result[i] <- rate
    }
  }

  result
}


# get_ra_transfer ---------------------------------------------------------
#
# Metal-tier RA factor (following Saltzman). RA is treated as a cost shifter
# determined by plan generosity: plans with higher AV attract sicker enrollees,
# and RA compensates proportionally. The factor is AV * moral hazard adjustment,
# normalized relative to Bronze (the baseline tier).
#
# Returns RA factor per plan (unitless, relative to Bronze = 0.60).

get_ra_transfer <- function(plan_names, plan_attrs) {

  result <- setNames(rep(0, length(plan_names)), plan_names)

  for (i in seq_along(plan_names)) {
    pn <- plan_names[i]
    pa_row <- plan_attrs[plan_attrs$plan_name == pn, ]
    if (nrow(pa_row) == 0) next

    av <- pa_row$av[1]
    metal <- pa_row$metal[1]
    if (is.na(av) || is.na(metal)) next

    mh <- MH_FACTOR[metal]
    if (is.na(mh)) mh <- 1.0

    result[i] <- av * mh
  }

  result
}


# compute_broker_shares_and_elasticities -----------------------------------
#
# Like compute_shares_and_elasticities() but aggregates only over
# broker-assisted (assisted == 1) households.
# Returns broker-specific market shares and J x J derivative matrix.

compute_broker_shares_and_elasticities <- function(cell_data, V, lambda,
                                                    benchmark_plan, plans_cell,
                                                    coefs_cell, spec = NULL) {

  dt <- as.data.table(cell_data)
  dt[, V := V]

  plan_names <- sort(unique(dt$plan_name[dt$plan_name != "Uninsured"]))
  J <- length(plan_names)

  # V_0 = β'X_0 for each HH (NOT zero)
  V0_by_hh <- dt[plan_name == "Uninsured", .(V_0 = V), by = household_number]

  # Choice probabilities per HH-plan (all HH, same as full model)
  ins_dt <- dt[plan_name != "Uninsured"]
  ins_dt <- merge(ins_dt, V0_by_hh, by = "household_number", all.x = TRUE)

  # Use per-row lambda if pre-computed, else scalar
  if (!("lambda_i" %in% names(ins_dt))) ins_dt[, lambda_i := lambda]

  ins_dt[, V_scaled := V / lambda_i]
  ins_dt[, max_V_scaled := max(V_scaled), by = household_number]
  ins_dt[, exp_V := exp(V_scaled - max_V_scaled)]
  ins_dt[, sum_exp_V := sum(exp_V), by = household_number]
  ins_dt[, log_D := max_V_scaled + log(sum_exp_V)]
  ins_dt[, s_jg := exp_V / sum_exp_V]
  ins_dt[, log_D_lam := lambda_i * log_D]
  ins_dt[, mx := pmax(log_D_lam, V_0)]
  ins_dt[, s_g := exp(log_D_lam - mx) / (exp(log_D_lam - mx) + exp(V_0 - mx))]
  ins_dt[, q_j := s_jg * s_g]

  # HH-level price sensitivity: use pre-computed if available
  if (!("alpha_i" %in% names(ins_dt))) {
    alpha_vec <- compute_alpha_i(ins_dt, coefs_cell, spec)
    ins_dt[, alpha_i := alpha_vec]
  }
  ins_dt[, rf_i := rating_factor / RATING_FACTOR_AGE40]

  # Filter to broker-assisted HH only
  ins_dt <- ins_dt[assisted == 1L]

  if (nrow(ins_dt) == 0) {
    return(list(
      broker_shares = setNames(rep(0, J), plan_names),
      broker_elast_mat = matrix(0, J, J, dimnames = list(plan_names, plan_names)),
      plan_names = plan_names
    ))
  }

  # Weighted broker shares
  total_weight <- ins_dt[, .(w = first(ipweight)), by = household_number][, sum(w)]

  shares_dt <- ins_dt[, .(share = sum(ipweight * q_j) / total_weight), by = plan_name]
  broker_shares <- setNames(shares_dt$share, shares_dt$plan_name)
  broker_shares <- broker_shares[plan_names]
  broker_shares[is.na(broker_shares)] <- 0

  # Elasticity matrix (same derivative formulas, broker HH only)
  elast_mat <- matrix(0, nrow = J, ncol = J, dimnames = list(plan_names, plan_names))

  for (l_idx in seq_along(plan_names)) {
    l <- plan_names[l_idx]
    is_benchmark <- (!is.na(benchmark_plan) && l == benchmark_plan)

    l_info <- ins_dt[plan_name == l, .(household_number, s_lg = s_jg, q_l = q_j)]
    merged <- merge(ins_dt, l_info, by = "household_number", all.x = TRUE)
    merged[is.na(s_lg), s_lg := 0]
    merged[is.na(q_l), q_l := 0]

    if (!is_benchmark) {
      merged[, dq_dposted := q_j * (as.numeric(plan_name == l) / lambda_i +
                                       ((lambda_i - 1) / lambda_i) * s_lg - q_l) *
                                alpha_i * rf_i]

      contrib <- merged[plan_name %in% plan_names,
                          .(elast = sum(ipweight * dq_dposted) / total_weight),
                          by = plan_name]
      for (j_idx in seq_along(plan_names)) {
        val <- contrib[plan_name == plan_names[j_idx], elast]
        if (length(val) > 0) elast_mat[j_idx, l_idx] <- val
      }
      rm(merged, l_info, contrib)

    } else {
      # Benchmark: split by subsidized
      unsub <- merged[subsidized == 0L]
      if (nrow(unsub) > 0) {
        unsub[, dq_dposted := q_j * (as.numeric(plan_name == l) / lambda_i +
                                        ((lambda_i - 1) / lambda_i) * s_lg - q_l) *
                                 alpha_i * rf_i]
        contrib_unsub <- unsub[plan_name %in% plan_names,
                                 .(elast = sum(ipweight * dq_dposted) / total_weight),
                                 by = plan_name]
      } else {
        contrib_unsub <- data.table(plan_name = character(0), elast = numeric(0))
      }

      sub <- merged[subsidized == 1L]
      if (nrow(sub) > 0) {
        sub[, common_factor := (1 - s_lg) * ((lambda_i - 1) / lambda_i - s_g)]
        sub[, dq_dposted := fifelse(
          plan_name == l,
          alpha_i * (-rf_i) * q_j * common_factor,
          alpha_i * (-rf_i) * q_j * (1 / lambda_i + common_factor)
        )]
        contrib_sub <- sub[plan_name %in% plan_names,
                             .(elast = sum(ipweight * dq_dposted) / total_weight),
                             by = plan_name]
      } else {
        contrib_sub <- data.table(plan_name = character(0), elast = numeric(0))
      }

      for (j_idx in seq_along(plan_names)) {
        j <- plan_names[j_idx]
        val_unsub <- contrib_unsub[plan_name == j, elast]
        val_sub   <- contrib_sub[plan_name == j, elast]
        total <- (if (length(val_unsub) > 0) val_unsub else 0) +
                 (if (length(val_sub) > 0) val_sub else 0)
        elast_mat[j_idx, l_idx] <- total
      }
      rm(merged, l_info, unsub, sub, contrib_unsub, contrib_sub)
    }
  }

  list(broker_shares = broker_shares, broker_elast_mat = elast_mat, plan_names = plan_names)
}


# compute_commission_derivatives -------------------------------------------
#
# For the commission FOC, computes ds_j/dc_f and dsB_j/dc_f.
# Chain rule: ds_j/dc_f = sum_{k in J_f} ds_j/dV_k * dV_k/dc_f
# where dV_k/dc_f = beta_eta * I(assisted) for flat commissions.
#
# Returns J x F matrices (F = number of unique firms).

compute_commission_derivatives <- function(cell_data, V, lambda, coefs_cell,
                                            plan_names, own_mat, plans_cell,
                                            commission_lookup, year) {

  coef_map <- setNames(coefs_cell$estimate, coefs_cell$term)
  beta_eta <- if ("commission_broker" %in% names(coef_map)) coef_map[["commission_broker"]] else 0

  dt <- as.data.table(cell_data)
  dt[, V := V]

  J <- length(plan_names)

  # V_0 = β'X_0 for each HH (NOT zero)
  V0_by_hh <- dt[plan_name == "Uninsured", .(V_0 = V), by = household_number]

  # Identify firms from plan name prefix
  firm_of_plan <- sub("_.*", "", plan_names)
  firm_names <- unique(firm_of_plan)
  F_count <- length(firm_names)

  # Choice probabilities (all HH)
  ins_all <- dt[plan_name != "Uninsured"]
  ins_all <- merge(ins_all, V0_by_hh, by = "household_number", all.x = TRUE)

  # Use per-row lambda if pre-computed, else scalar
  if (!("lambda_i" %in% names(ins_all))) ins_all[, lambda_i := lambda]

  ins_all[, V_scaled := V / lambda_i]
  ins_all[, max_V_scaled := max(V_scaled), by = household_number]
  ins_all[, exp_V := exp(V_scaled - max_V_scaled)]
  ins_all[, sum_exp_V := sum(exp_V), by = household_number]
  ins_all[, s_jg := exp_V / sum_exp_V]
  ins_all[, log_D := max_V_scaled + log(sum_exp_V)]
  ins_all[, log_D_lam := lambda_i * log_D]
  ins_all[, mx := pmax(log_D_lam, V_0)]
  ins_all[, s_g := exp(log_D_lam - mx) / (exp(log_D_lam - mx) + exp(V_0 - mx))]
  ins_all[, q_j := s_jg * s_g]

  total_weight_all <- ins_all[, .(w = first(ipweight)), by = household_number][, sum(w)]

  # Broker/agent HH only (navigators don't respond to commission changes)
  if ("any_agent" %in% names(ins_all)) {
    ins_broker <- ins_all[any_agent == 1L]
  } else {
    ins_broker <- ins_all[assisted == 1L]
  }
  total_weight_broker <- if (nrow(ins_broker) > 0) {
    ins_broker[, .(w = first(ipweight)), by = household_number][, sum(w)]
  } else 0

  # ds_j/dc_f (aggregate share derivatives) and dsB_j/dc_f (broker share derivatives)
  ds_dc  <- matrix(0, J, F_count, dimnames = list(plan_names, firm_names))
  dsB_dc <- matrix(0, J, F_count, dimnames = list(plan_names, firm_names))

  for (f_idx in seq_along(firm_names)) {
    f <- firm_names[f_idx]
    firm_plans <- plan_names[firm_of_plan == f]

    # For each plan k owned by firm f, compute dV_k/dc_f for broker HH:
    #   Flat commission: dV_k/dc_f = beta_eta (same for all k in J_f, all broker HH)
    # Then ds_j/dV_k = q_j * [I(j==k)/lambda + ((lambda-1)/lambda)*s_kg - q_k]
    # Sum over k in J_f.

    # All HH: ds_j/dc_f = sum_k beta_eta * ds_j/dV_k (but only broker HH respond)
    # So we compute derivatives using broker HH only, weighted by their share of population

    if (nrow(ins_broker) == 0 || total_weight_broker == 0) next

    for (k in firm_plans) {
      k_info <- ins_broker[plan_name == k, .(household_number, s_kg = s_jg, q_k = q_j)]

      # ds_j/dV_k for ALL market shares (aggregate over all HH but only broker respond)
      merged_all <- merge(ins_all[assisted == 1L], k_info, by = "household_number", all.x = TRUE)
      merged_all[is.na(s_kg), s_kg := 0]
      merged_all[is.na(q_k), q_k := 0]

      merged_all[, dq_dV := q_j * (as.numeric(plan_name == k) / lambda_i +
                                      ((lambda_i - 1) / lambda_i) * s_kg - q_k)]

      # Aggregate share derivative: weight by broker HH weight / total population weight
      contrib_all <- merged_all[plan_name %in% plan_names,
                                  .(deriv = sum(ipweight * dq_dV) / total_weight_all),
                                  by = plan_name]
      for (j_idx in seq_along(plan_names)) {
        val <- contrib_all[plan_name == plan_names[j_idx], deriv]
        if (length(val) > 0) ds_dc[j_idx, f_idx] <- ds_dc[j_idx, f_idx] + beta_eta * val
      }

      # Broker share derivative
      contrib_broker <- merged_all[plan_name %in% plan_names,
                                     .(deriv = sum(ipweight * dq_dV) / total_weight_broker),
                                     by = plan_name]
      for (j_idx in seq_along(plan_names)) {
        val <- contrib_broker[plan_name == plan_names[j_idx], deriv]
        if (length(val) > 0) dsB_dc[j_idx, f_idx] <- dsB_dc[j_idx, f_idx] + beta_eta * val
      }

      rm(k_info, merged_all, contrib_all, contrib_broker)
    }
  }

  list(ds_dc = ds_dc, dsB_dc = dsB_dc, firm_names = firm_names,
       firm_of_plan = firm_of_plan)
}
