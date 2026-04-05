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

    # --- Control function residual (Hausman IV) ---
    cf_resid       = list(type = "cf_correction"),

    # --- Insurer fixed effects ---
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
    assisted_silver = list(type = "assisted"),
    assisted_bronze = list(type = "assisted"),
    assisted_gold   = list(type = "assisted"),
    assisted_plat   = list(type = "assisted"),

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
# Spec I/O — write/read for Julia and cf_worker subprocesses
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
# (for exporting in cell CSVs so cf_worker can recompute interactions)
get_raw_demo_cols <- function(spec) {
  prem_ints <- get_prem_interactions(spec)
  unique(unlist(prem_ints))
}


# =========================================================================
# Generic premium interaction recomputation
# =========================================================================
# Used by cf_worker when prices change in equilibrium solving

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
