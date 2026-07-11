# covariates.R — Centralized covariate specification
#
# All possible demand covariates are defined here. The active spec is a
# character vector of names selected from this menu, defined in _analysis.R.
# Downstream code (Julia, supply, counterfactuals) reads the spec from
# data/output/demand_spec.csv.

# =========================================================================
# Variable dictionary
# =========================================================================
#
# Each entry has:
#   type     — what kind of variable it is (used for generic handling)
#   raw_demo — for price_interaction terms: the raw demographic column
#              whose value is multiplied by premium. Used by
#              compute_alpha_i() and recompute_prem_interactions().

get_covariate_menu <- function() {
  list(
    # --- Price ---
    premium        = list(type = "price"),
    premium_sq     = list(type = "price_nonlinear"),

    # --- Exclusion restriction (outside option identification) ---
    penalty_own    = list(type = "exclusion"),

    # --- Plan attributes ---
    silver         = list(type = "plan_attribute"),
    bronze         = list(type = "plan_attribute"),
    hmo            = list(type = "plan_attribute"),
    hsa            = list(type = "plan_attribute"),
    av             = list(type = "plan_attribute"),

    # --- Family × insured interaction ---
    family_insured = list(type = "insured_interaction", raw_demo = "family"),

    # --- Rating area fixed effects (insured-only dummies, region 1 = ref) ---
    ra_4           = list(type = "rating_area_fe"),
    ra_8           = list(type = "rating_area_fe"),
    ra_13          = list(type = "rating_area_fe"),
    ra_16          = list(type = "rating_area_fe"),

    # --- Insurer × rating-area FEs (insured-only dummies, Anthem×ra1 ref) ---
    # 5 insurers (Anthem, Blue_Shield, Kaiser, Health_Net, Small) x 5 regions
    # = 25 cells. Reference = Anthem in region 1. So 24 dummies named
    # ix_<INSURER>_ra<REGION>. Anthem dummies omitted only for region 1.
    ix_BS_ra1      = list(type = "insurer_market_fe"),
    ix_Kai_ra1     = list(type = "insurer_market_fe"),
    ix_HN_ra1      = list(type = "insurer_market_fe"),
    ix_Sm_ra1      = list(type = "insurer_market_fe"),
    ix_Ant_ra4     = list(type = "insurer_market_fe"),
    ix_BS_ra4      = list(type = "insurer_market_fe"),
    ix_Kai_ra4     = list(type = "insurer_market_fe"),
    ix_HN_ra4      = list(type = "insurer_market_fe"),
    ix_Sm_ra4      = list(type = "insurer_market_fe"),
    ix_Ant_ra8     = list(type = "insurer_market_fe"),
    ix_BS_ra8      = list(type = "insurer_market_fe"),
    ix_Kai_ra8     = list(type = "insurer_market_fe"),
    ix_HN_ra8      = list(type = "insurer_market_fe"),
    ix_Sm_ra8      = list(type = "insurer_market_fe"),
    ix_Ant_ra13    = list(type = "insurer_market_fe"),
    ix_BS_ra13     = list(type = "insurer_market_fe"),
    ix_Kai_ra13    = list(type = "insurer_market_fe"),
    ix_HN_ra13     = list(type = "insurer_market_fe"),
    ix_Sm_ra13     = list(type = "insurer_market_fe"),
    ix_Ant_ra16    = list(type = "insurer_market_fe"),
    ix_BS_ra16     = list(type = "insurer_market_fe"),
    ix_Kai_ra16    = list(type = "insurer_market_fe"),
    ix_HN_ra16     = list(type = "insurer_market_fe"),
    ix_Sm_ra16     = list(type = "insurer_market_fe"),

    # --- Mundlak / Chamberlain group means (insurer × region) ---
    mundlak_prem   = list(type = "plan_attribute"),
    mundlak_av     = list(type = "plan_attribute"),

    # --- Plan-market mean of net premium (Berry-style aggregate) ---
    plan_mkt_mean_prem = list(type = "plan_attribute"),

    # --- Subsidy interactions (control for subsidy when using gross premium) ---
    subsidy_insured = list(type = "insured_interaction", raw_demo = "subsidy"),

    # --- Control function residual (Hausman IV) ---
    cf_resid       = list(type = "cf_correction"),

    # --- Insurer fixed effects ---
    # Big four only. The seven larger regionals are kept as separate plans (so
    # their commission and cost vary), but carry NO brand dummy: one per regional
    # pushes the nesting parameter lambda from ~0.47 to 4.27 (non-RUM). See _demand.R.
    Anthem         = list(type = "insurer_fe"),
    Blue_Shield    = list(type = "insurer_fe"),
    Kaiser         = list(type = "insurer_fe"),
    Health_Net     = list(type = "insurer_fe"),

    # --- Insurer x metal interactions ---
    Anthem_silver  = list(type = "insurer_metal"),
    BS_silver      = list(type = "insurer_metal"),
    Kaiser_silver  = list(type = "insurer_metal"),
    HN_silver      = list(type = "insurer_metal"),
    Anthem_bronze  = list(type = "insurer_metal"),
    BS_bronze      = list(type = "insurer_metal"),
    Kaiser_bronze  = list(type = "insurer_metal"),
    HN_bronze      = list(type = "insurer_metal"),

    # --- Demographic x premium interactions ---
    # Convention: {raw_demo}_prem = raw_demo * premium
    # dV/dp includes beta_{name} * raw_demo for each of these
    hh_size_prem       = list(type = "price_interaction", raw_demo = "hh_size"),
    perc_0to17_prem    = list(type = "price_interaction", raw_demo = "perc_0to17"),
    perc_18to34_prem   = list(type = "price_interaction", raw_demo = "perc_18to34"),
    perc_35to54_prem   = list(type = "price_interaction", raw_demo = "perc_35to54"),
    perc_male_prem     = list(type = "price_interaction", raw_demo = "perc_male"),
    perc_black_prem    = list(type = "price_interaction", raw_demo = "perc_black"),
    perc_hispanic_prem = list(type = "price_interaction", raw_demo = "perc_hispanic"),
    perc_asian_prem    = list(type = "price_interaction", raw_demo = "perc_asian"),
    perc_other_prem    = list(type = "price_interaction", raw_demo = "perc_other"),
    FPL_250to400_prem  = list(type = "price_interaction", raw_demo = "FPL_250to400"),
    FPL_400plus_prem   = list(type = "price_interaction", raw_demo = "FPL_400plus"),

    # --- Age x metal interactions (demographic sorting across tiers) ---
    # age share x metal dummy, so the metal preference varies by household age mix
    # and the young tilt into bronze beyond the common price effect (fixes the
    # inverted age-by-metal sorting). Premium-INDEPENDENT, so plain covariates: no
    # compute_alpha_i contribution and no premium recompute. Built in
    # build_structural from the age and metal columns.
    perc_0to17_silver  = list(type = "demo_metal"),
    perc_0to17_bronze  = list(type = "demo_metal"),
    perc_18to34_silver = list(type = "demo_metal"),
    perc_18to34_bronze = list(type = "demo_metal"),
    perc_35to54_silver = list(type = "demo_metal"),
    perc_35to54_bronze = list(type = "demo_metal"),
    perc_male_silver   = list(type = "demo_metal"),
    perc_male_bronze   = list(type = "demo_metal"),

    # --- Demographic x insured interactions ---
    # Convention: {raw_demo}_insured = raw_demo * I(insured)
    # These shift the insured/uninsured margin by demographics, helping identify λ
    hh_size_insured       = list(type = "insured_interaction", raw_demo = "hh_size"),
    perc_0to17_insured    = list(type = "insured_interaction", raw_demo = "perc_0to17"),
    perc_18to34_insured   = list(type = "insured_interaction", raw_demo = "perc_18to34"),
    perc_35to54_insured   = list(type = "insured_interaction", raw_demo = "perc_35to54"),
    perc_male_insured     = list(type = "insured_interaction", raw_demo = "perc_male"),
    perc_black_insured    = list(type = "insured_interaction", raw_demo = "perc_black"),
    perc_hispanic_insured = list(type = "insured_interaction", raw_demo = "perc_hispanic"),
    perc_asian_insured    = list(type = "insured_interaction", raw_demo = "perc_asian"),
    perc_other_insured    = list(type = "insured_interaction", raw_demo = "perc_other"),
    FPL_250to400_insured  = list(type = "insured_interaction", raw_demo = "FPL_250to400"),
    FPL_400plus_insured   = list(type = "insured_interaction", raw_demo = "FPL_400plus"),

    # --- Assisted x metal interactions ---
    # Navigator (non-broker) metal steering.
    assisted_silver = list(type = "assisted"),
    assisted_bronze = list(type = "assisted"),
    assisted_gold   = list(type = "assisted"),
    assisted_plat   = list(type = "assisted"),

    # --- Broker x metal interactions ---
    # Broker metal steering, estimated rather than assumed zero (symmetric with
    # the navigator assisted_* terms). Brokers additionally carry the commission
    # terms below; navigators are not commissioned.
    broker_silver   = list(type = "assisted"),
    broker_bronze   = list(type = "assisted"),

    # --- Channel x premium (channel-specific price response) ---
    # Price interactions whose raw_demo is the channel indicator, so
    # compute_alpha_i and recompute_prem_interactions treat them like any other
    # _prem term: a navigator household's price slope is beta_premium +
    # beta_assisted_premium, a broker's is beta_premium + beta_broker_premium.
    # The nonbroker / broker indicator columns are kept on the choice data for
    # exactly this reason (see build_structural).
    assisted_premium = list(type = "price_interaction", raw_demo = "nonbroker"),
    broker_premium   = list(type = "price_interaction", raw_demo = "broker"),

    # --- Channel x Pareto-dominated plan (reduced-form definition) ---
    # dominated_plan = 1 on a CSR-eligible household's Gold/Platinum alternatives,
    # which its enhanced Silver dominates (higher AV, lower premium): Gold or
    # Platinum for CSR-94 (FPL <= 1.50), Gold only for CSR-87 (FPL 1.50-2.00).
    # nav_dominated / broker_dominated = channel * dominated_plan. Premium-
    # INDEPENDENT (AV/metal/CSR only), so they are plain covariates: no alpha_i
    # contribution, no premium recompute, and not collinear with the premium effect.
    nav_dominated    = list(type = "assisted"),
    broker_dominated = list(type = "assisted"),

    # --- Commission / CF ---
    commission_broker  = list(type = "commission"),
    v_hat_commission   = list(type = "commission"),

    # --- CF interactions (reduced-form selection correction) ---
    # v_hat × plan indicators, used for reduced-form CF approach
    cf_anthem      = list(type = "cf_interaction", raw_demo = "Anthem",      cf_var = "v_hat"),
    cf_blue_shield = list(type = "cf_interaction", raw_demo = "Blue_Shield", cf_var = "v_hat"),
    cf_kaiser      = list(type = "cf_interaction", raw_demo = "Kaiser",      cf_var = "v_hat"),
    cf_health_net  = list(type = "cf_interaction", raw_demo = "Health_Net",  cf_var = "v_hat"),
    cf_silver      = list(type = "cf_interaction", raw_demo = "silver",      cf_var = "v_hat"),
    cf_bronze      = list(type = "cf_interaction", raw_demo = "bronze",      cf_var = "v_hat"),
    cf_premium     = list(type = "cf_interaction", raw_demo = "premium",     cf_var = "v_hat")
  )
}


# =========================================================================
# Spec I/O — write/read for Julia and 4_counterfactuals subprocesses
# =========================================================================

write_demand_spec <- function(spec, asst, path) {
  df <- data.frame(
    term  = c(spec, asst),
    group = c(rep("base", length(spec)), rep("assisted", length(asst))),
    stringsAsFactors = FALSE
  )
  write.csv(df, path, row.names = FALSE)
  cat("  Demand spec written:", nrow(df), "terms to", path, "\n")
}

read_demand_spec <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  list(
    base     = df$term[df$group == "base"],
    assisted = df$term[df$group == "assisted"],
    all      = df$term
  )
}


# =========================================================================
# Premium interaction helpers
# =========================================================================

# Returns named list: term -> raw_demo column
# Used by compute_alpha_i() and recompute_prem_interactions()
get_prem_interactions <- function(spec) {
  menu <- get_covariate_menu()
  out <- list()
  for (nm in spec) {
    if (nm %in% names(menu) && identical(menu[[nm]]$type, "price_interaction")) {
      out[[nm]] <- menu[[nm]]$raw_demo
    }
  }
  out
}

# Returns character vector of raw demographic columns needed by the spec
# (for exporting in cell CSVs so 4_counterfactuals can recompute interactions)
get_raw_demo_cols <- function(spec) {
  prem_ints <- get_prem_interactions(spec)
  unique(unlist(prem_ints))
}


# =========================================================================
# Generic premium interaction recomputation
# =========================================================================
# Used by 4_counterfactuals when prices change in equilibrium solving

recompute_prem_interactions <- function(dt, spec) {
  prem_ints <- get_prem_interactions(spec)
  for (nm in names(prem_ints)) {
    raw_col <- prem_ints[[nm]]
    if (raw_col %in% names(dt)) {
      dt[, (nm) := get(raw_col) * premium]
    }
  }
  invisible(dt)
}
