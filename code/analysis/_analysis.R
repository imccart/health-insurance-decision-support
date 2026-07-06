# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Master driver for the full analysis. Loads helpers, then sources
##                the numbered steps 1-13 in order. Convention: _name.R = a driver
##                you run directly; N_name.R = step N (sourced by a driver);
##                helpers/ = shared function libraries. Steps assume this preamble
##                has loaded (run it first to run any single step on its own).

# Parameters --------------------------------------------------------------
TEMP_DIR     <- "D:/temp-research-data/health-insurance-decision-support"
SAMPLE_FRAC  <- 0.05
MASTER_SEED  <- 20260224
N_BOOT       <- 50L   # reduced-form bootstrap reps (steps 4-6); 0 to skip
N_BOOT_CF    <- 30L   # CF welfare-SE bootstrap draws (step 12)

# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, data.table, fixest, kableExtra, nleqslv, mlogit)

# Helpers (function libraries, loaded once) -------------------------------
source("code/data-build/_helpers.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")
source("code/analysis/helpers/ra.R")
source("code/analysis/helpers/estimate_demand.R")
source("code/analysis/helpers/se.R")
source("code/analysis/helpers/cf_cell.R")
source("code/analysis/helpers/welfare_objective.R")
source("code/analysis/helpers/welfare_engine.R")

# 1-3: build analysis data ------------------------------------------------
source("code/analysis/1_decision-analysis.R")
source("code/analysis/2_ipw.R")
source("code/analysis/3_summary-stats.R")
rm(list = intersect(c("hh_full", "hh_clean", "hh_ins", "hh_ins_ps"), ls()))
gc(full = TRUE, verbose = FALSE)

# Shared choice-data prep (writes plan_choice/hh_choice CSVs; leaves hh_full) -
source("code/analysis/helpers/data-prep.R")

# 4-6: reduced form (uses hh_full) ----------------------------------------
source("code/analysis/4_rf-dominated.R")
source("code/analysis/5_rf-choice-att.R")
source("code/analysis/6_rf-summary.R")
rm(hh_full); gc(full = TRUE, verbose = FALSE)

# 7-12: structural --------------------------------------------------------
source("code/analysis/helpers/inputs.R")   # cells + seeds (needs data-prep outputs)
source("code/analysis/7_demand.R")          # writes demand_spec.csv, estimates
source("code/analysis/8_pricing.R")
gc(full = TRUE, verbose = FALSE)
source("code/analysis/9_cost-gmm.R")
gc(full = TRUE, verbose = FALSE)
source("code/analysis/10_struc-se.R")
rm(hh_split); gc(full = TRUE, verbose = FALSE)   # free before the CF
source("code/analysis/11_cf.R")
source("code/analysis/12_cf-se.R")          # slow (re-runs the CF per draw); comment out to skip
rm(list = intersect(c("cells", "cell_seeds", "plan_choice", "commission_lookup"), ls()))
gc(full = TRUE, verbose = FALSE)

# 13: numbers in paper ----------------------------------------------------
source("code/analysis/13_paper-results.R")
