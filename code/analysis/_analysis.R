# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-25
## Description:   Master runner for analysis data prep.
##                Builds analysis datasets (scripts 1-2), then run
##                reduced-form or structural pipelines separately.

# Setup -------------------------------------------------------------------
source("code/0-setup.R")

# Helpers -----------------------------------------------------------------
source("code/analysis/_helpers-analysis.R")

# Build analysis data -----------------------------------------------------
source("code/analysis/1_decision-analysis.R")
source("code/analysis/2_summary-stats.R")

# Run main analyses ------------------------------------------------------
source('code/analysis/reduced-form/_reduced-form.R')
source('code/analysis/structural/_structural.R')
