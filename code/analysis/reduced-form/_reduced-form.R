# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Description:   Master runner for reduced-form analysis.
##                Data prep is sourced from code/analysis/0_data-prep.R
##                so RF and structural cannot drift on filters/columns.

# Reduced-form specification ---------------------------------------------

REDUCED_FORM_SPEC <- c(
  "premium",
  "silver", "bronze", "hmo", "hsa",
  "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
  "hh_size_prem", "perc_0to17_prem", "perc_18to34_prem", "perc_35to54_prem",
  "perc_male_prem", "perc_black_prem", "perc_hispanic_prem", "perc_asian_prem", "perc_other_prem",
  "FPL_250to400_prem", "FPL_400plus_prem"
)

REDUCED_FORM_CF <- c(
  "cf_anthem", "cf_blue_shield", "cf_kaiser", "cf_health_net",
  "cf_silver", "cf_bronze"
)

REDUCED_FORM_FULL <- c(REDUCED_FORM_SPEC, REDUCED_FORM_CF)

write_demand_spec(REDUCED_FORM_FULL, character(0),
                  file.path(TEMP_DIR, "demand_spec_reduced.csv"))

# Data prep (writes plan_choice.csv, hh_choice.csv; leaves hh_full in env) -

source("code/analysis/0_data-prep.R")

# Reduced-form analysis ---------------------------------------------------

# Dominated choice ATT (uses hh_full from memory; manages
# hh_full → hh_clean → hh_po lifecycle internally)
source("code/analysis/reduced-form/1_dominated-choices.R")

# Choice model ATT (reads hh_choice.csv from TEMP_DIR)
source("code/analysis/reduced-form/2_choice-att.R")

# Summary
source("code/analysis/reduced-form/3_summary.R")
