# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Structural demand stage. Defines the demand specification,
##                regenerates analysis inputs via 0_data-prep, and estimates the
##                pooled nested-logit demand model. Always re-estimates (no cache
##                skip) so demand can never silently go stale. Run before
##                _supply.R; both are sourced in order by _analysis.R.

# Structural specification ------------------------------------------------

STRUCTURAL_SPEC <- c(
  "premium",
  "silver", "bronze", "hmo", "hsa",
  # Big-four brand dummies only. The seven regionals are kept as SEPARATE plans
  # (own premium, commission, and cost), but carry NO brand fixed effect — adding
  # one per regional pushes the nesting parameter lambda from 0.47 to 4.27
  # (non-RUM). They sit in the dummy-less small baseline, exactly as the lumped
  # "Small" always did. Per-regional commission/cost key off the plan_id prefix,
  # not these dummies, so they are unaffected by this choice.
  "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
  "hh_size_prem", "perc_0to17_prem", "perc_18to34_prem", "perc_35to54_prem",
  "perc_male_prem", "perc_black_prem", "perc_hispanic_prem", "perc_asian_prem", "perc_other_prem",
  "FPL_250to400_prem", "FPL_400plus_prem"
)

STRUCTURAL_ASST <- c(
  "assisted_silver", "assisted_bronze",
  "commission_broker", "v_hat_commission"
)

write_demand_spec(STRUCTURAL_SPEC, STRUCTURAL_ASST,
                  file.path(TEMP_DIR, "demand_spec.csv"))

# Data prep (writes plan_choice.csv, plan_demographics.csv, hh_choice.csv) -

source("code/analysis/0_data-prep.R")

# Demand reads its inputs back from disk; hh_full not needed here
rm(hh_full)
gc(verbose = FALSE)

# Shared structural inputs ------------------------------------------------

source("code/analysis/structural/_inputs.R")

# Demand estimation -------------------------------------------------------

cat("--- Demand estimation ---\n")
source("code/analysis/structural/1_demand.R")
gc(full = TRUE, verbose = FALSE)

cat("\n=== Structural demand stage complete ===\n")
