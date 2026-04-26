# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Description:   Master runner for structural estimation (demand + supply).
##                Data prep is sourced from code/analysis/0_data-prep.R
##                so RF and structural cannot drift on filters/columns.

# Structural specification ------------------------------------------------

STRUCTURAL_SPEC <- c(
  "premium",
  "silver", "bronze", "hmo", "hsa",
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

# Structural reads its inputs back from disk; hh_full not needed here
rm(hh_full)
gc(verbose = FALSE)

# Shared structural inputs ------------------------------------------------

cat("Loading shared structural data...\n")
hh_all <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split <- split(hh_all, by = c("region", "year"))
cells <- unique(hh_all[, .(region, year)])[order(region, year)]
rm(hh_all); gc(verbose = FALSE)

plan_choice       <- fread(file.path(TEMP_DIR, "plan_choice.csv")) %>% as_tibble()
commission_lookup <- fread("data/output/commission_lookup.csv")    %>% as_tibble()

set.seed(MASTER_SEED)
cell_seeds <- sample.int(1e7, nrow(cells))

cat("  Cells:", nrow(cells), "\n\n")

# Demand ------------------------------------------------------------------
if (file.exists("results/choice_coefficients_structural.csv")) {
  cat("--- Demand: coefficients found, skipping ---\n")
} else {
  cat("--- Demand estimation ---\n")
  source("code/analysis/structural/1_demand.R")
  gc(full = TRUE, verbose = FALSE)
}

# Pricing -----------------------------------------------------------------
cat("\n--- Pricing (markups, FOC inputs) ---\n")
source("code/analysis/structural/2_pricing.R")
gc(full = TRUE, verbose = FALSE)

# Cost-side GMM -----------------------------------------------------------
cat("\n--- Cost-side GMM ---\n")
source("code/analysis/structural/3_cost_gmm.R")
gc(full = TRUE, verbose = FALSE)

# Counterfactuals ---------------------------------------------------------

# Free hh_split before counterfactuals (CF reads per-cell from disk)
rm(hh_split)
gc(full = TRUE, verbose = FALSE)

cat("\n--- Counterfactual simulation ---\n")
source("code/analysis/structural/4_counterfactuals.R")

rm(cells, cell_seeds, plan_choice, commission_lookup)
gc(full = TRUE, verbose = FALSE)

cat("\n=== Structural pipeline complete ===\n")
