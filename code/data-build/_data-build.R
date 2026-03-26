# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-17
## Date Edited:   2026-02-17
## Description:   Build enrollment and household datasets from Covered
##                California FOIA data. Sources each step in order;
##                each reads the prior step's CSV from data/output/.

source("code/0-setup.R")
source("code/data-build/_helpers-enrollment.R")
source("code/data-build/_helpers-demand.R")

cat("=== Starting data build ===\n\n")

# Phase 1: Enrollment pipeline (scripts 1-5)
cat("--- Step 1: Load and clean enrollment ---\n")
source("code/data-build/1_load-clean-enrollment.R")

cat("\n--- Step 2: Validate geography and income ---\n")
source("code/data-build/2_validate-geography-income.R")

cat("\n--- Step 3: Construct households ---\n")
source("code/data-build/3_construct-households.R")

cat("\n--- Step 4: Reconcile income ---\n")
source("code/data-build/4_reconcile-income.R")

cat("\n--- Step 5: Assemble final datasets ---\n")
source("code/data-build/5_assemble-enrollment.R")

cat("\n=== Enrollment pipeline complete ===\n\n")

# Phase 2: Demand estimation data (scripts 6-10)
cat("--- Step 6: Process SIPP ---\n")
source("code/data-build/6_process-sipp.R")

cat("\n--- Step 7: Build ACS market population ---\n")
source("code/data-build/7_build-acs-market.R")

cat("\n--- Step 8: Merge exchange and ACS ---\n")
source("code/data-build/8_merge-exchange-acs.R")

cat("\n--- Step 9: Extend panel ---\n")
source("code/data-build/9_extend-panel.R")

cat("\n--- Step 10: Finalize demand data ---\n")
source("code/data-build/10_finalize-demand.R")

# Phase 3: SIPP market transitions (script 11)
cat("\n--- Step 11: SIPP market transitions ---\n")
source("code/data-build/11_sipp-market-transitions.R")

# Phase 4: Broker density (script 12)
cat("\n--- Step 12: Broker density ---\n")
source("code/data-build/12_broker-density.R")

cat("\n=== Data build complete ===\n")
