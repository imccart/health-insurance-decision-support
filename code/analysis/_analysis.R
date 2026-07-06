# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Master driver for the full analysis. Loads the function
##                libraries in helpers/, then sources the pipeline steps in
##                order. Steps are grouped by family; each family runs as one
##                contiguous block:
##                  build*  shared data construction (both branches need it)
##                  rf*     reduced-form (decision-support) analysis
##                  s*      structural demand + supply estimation
##                  cf*     counterfactuals
##                  paper*  paper-facing tables and figures
##                Convention: _name.R = a driver you run directly; family+N =
##                step N of that family (sourced here in order); helpers/ =
##                shared function libraries. Steps assume this preamble has
##                loaded (run it first to run any single step on its own).
##                supp1_/supp2_ are standalone supplementary analyses and are
##                NOT part of this driver.

# Parameters --------------------------------------------------------------
TEMP_DIR     <- "D:/temp-research-data/health-insurance-decision-support"
SAMPLE_FRAC  <- 0.05
MASTER_SEED  <- 20260224
N_BOOT       <- 50L   # reduced-form bootstrap reps (rf1-rf3); 0 to skip
N_BOOT_CF    <- 30L   # CF welfare-SE bootstrap draws (cf2_se)

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


# build: shared data construction -----------------------------------------
source("code/analysis/build1_decision-analysis.R")   # writes hh_full.csv
source("code/analysis/build2_ipw.R")                 # writes ipweights.csv
source("code/analysis/build3_data-prep.R")           # writes plan_choice/hh_choice; leaves hh_full
rm(list = intersect(c("hh_clean", "hh_ins", "hh_ins_ps"), ls()))
gc(full = TRUE, verbose = FALSE)

# rf: reduced form (uses hh_full) -----------------------------------------
source("code/analysis/rf1_dominated.R")
source("code/analysis/rf2_choice-att.R")
source("code/analysis/rf3_summary.R")
rm(hh_full); gc(full = TRUE, verbose = FALSE)

# s: structural -----------------------------------------------------------
source("code/analysis/s1_inputs.R")          # cells + seeds (needs build3 outputs)
source("code/analysis/s2_demand.R")          # writes demand_spec.csv, estimates
source("code/analysis/s3_pricing.R")
gc(full = TRUE, verbose = FALSE)
source("code/analysis/s4_cost-gmm.R")
gc(full = TRUE, verbose = FALSE)
source("code/analysis/s5_se.R")
rm(hh_split); gc(full = TRUE, verbose = FALSE)   # free before the CF

# cf: counterfactuals -----------------------------------------------------
source("code/analysis/cf1_estimate.R")
source("code/analysis/cf2_se.R")             # slow (re-runs the CF per draw); comment out to skip
rm(list = intersect(c("cells", "cell_seeds", "plan_choice", "commission_lookup"), ls()))
gc(full = TRUE, verbose = FALSE)

# paper: tables + figures -------------------------------------------------
source("code/analysis/paper1_summary-stats.R")   # reads hh_full.csv from disk
source("code/analysis/paper2_results.R")
