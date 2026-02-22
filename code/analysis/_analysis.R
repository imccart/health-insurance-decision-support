# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Description:   Master runner for reduced-form analysis scripts.

# Setup -------------------------------------------------------------------
source("code/0-setup.R")

# Helpers -----------------------------------------------------------------
source("code/analysis/_helpers-analysis.R")

# Analysis scripts --------------------------------------------------------
source("code/analysis/1_decision-analysis.R")
source("code/analysis/2_summary-stats.R")
source("code/analysis/3_dominated-choices.R")
source("code/analysis/4_switching.R")
