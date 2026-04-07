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

if (!dir.exists(TEMP_DIR)) dir.create(TEMP_DIR, recursive = TRUE)
Sys.setenv(TEMP_DIR = TEMP_DIR)
Sys.setenv(SAMPLE_FRAC = SAMPLE_FRAC)
Sys.setenv(MASTER_SEED = MASTER_SEED)

# Setup -------------------------------------------------------------------
source("code/0-setup.R")

# Helpers -----------------------------------------------------------------
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")

# Build analysis data -----------------------------------------------------
source("code/analysis/1_decision-analysis.R")
source("code/analysis/2_ipw.R")
# source("code/analysis/2_summary-stats.R")  # figures/tables, skip for test runs

# Run reduced-form analysis ------------------------------------------------
source('code/analysis/reduced-form/_reduced-form.R')


# Run structural analysis ------------------------------------------------
source('code/analysis/structural/_structural.R')

