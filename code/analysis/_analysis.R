# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-25
## Description:   Master runner for analysis data prep.
##                Builds analysis datasets (scripts 1-2), then run
##                reduced-form or structural pipelines separately.

# Pipeline parameters (defined once, used by both reduced-form and structural)
TEMP_DIR     <- "D:/temp-research-data/health-insurance-decision-support"
SAMPLE_FRAC  <- 0.05
MASTER_SEED  <- 20260224
N_BOOT       <- 50L   # bootstrap reps for reduced-form CIs; set to 0 to skip

# Setup -------------------------------------------------------------------
source("code/0-setup.R")


# Helpers (loaded once, used by all downstream scripts) -------------------
source("code/data-build/_helpers.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")
source("code/analysis/helpers/ra.R")
source("code/analysis/helpers/estimate_demand.R")
source("code/analysis/helpers/cf_worker.R")


# Build analysis data -----------------------------------------------------
source("code/analysis/1_decision-analysis.R")
source("code/analysis/2_ipw.R")
source("code/analysis/3_summary-stats.R")

# Free data objects before reduced-form (functions/constants stay loaded).
# 3_summary-stats.R already rm's hh_full after deriving its subsets, and
# its own end leaves hh_ins / hh_clean live — sweep up whatever remains.
rm(list = intersect(c("hh_full", "hh_clean", "hh_ins", "hh_ins_ps"), ls()))
gc(full = TRUE, verbose = FALSE)

# Run reduced-form analysis ------------------------------------------------
source('code/analysis/reduced-form/_reduced-form.R')

# Run structural analysis ------------------------------------------------
source('code/analysis/structural/_structural.R')

# Numbers in paper --------------------------------------------------------
source("code/analysis/4_paper-results.R")