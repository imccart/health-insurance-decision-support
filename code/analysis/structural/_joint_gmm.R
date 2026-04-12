# Master runner for joint demand-supply GMM estimation.
# Self-contained: sets up environment, runs demand + pricing for starting
# values (if needed), then runs joint GMM. Safe to run standalone.
#
# Usage: source from project root, or run via _analysis.R preamble then source.

# Pipeline parameters (set in _analysis.R, recovered via env vars)
TEMP_DIR     <- Sys.getenv("TEMP_DIR")
SAMPLE_FRAC  <- as.numeric(Sys.getenv("SAMPLE_FRAC"))
MASTER_SEED  <- as.integer(Sys.getenv("MASTER_SEED"))

if (TEMP_DIR == "" || is.na(SAMPLE_FRAC)) {
  stop("Run _analysis.R first to set TEMP_DIR, SAMPLE_FRAC, MASTER_SEED env vars.")
}

# Setup -------------------------------------------------------------------
source("code/0-setup.R")
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")

# Spec
STRUCTURAL_SPEC <- c(
  "premium", "penalty_own",
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

write_demand_spec(STRUCTURAL_SPEC, STRUCTURAL_ASST, file.path(TEMP_DIR, "demand_spec.csv"))

# =========================================================================
# STEP 1: Demand (starting values) — skip if already done
# =========================================================================

if (!file.exists("results/choice_coefficients_structural.csv")) {
  cat("--- Demand estimation (starting values) ---\n")
  source("code/analysis/structural/1_demand.R")
  rm(list = setdiff(ls(), c("TEMP_DIR", "SAMPLE_FRAC", "MASTER_SEED",
                              "STRUCTURAL_SPEC", "STRUCTURAL_ASST")))
  gc(full = TRUE, verbose = FALSE)
  suppressPackageStartupMessages({ library(tidyverse); library(data.table) })
  source("code/data-build/_helpers-enrollment.R")
  source("code/analysis/helpers/constants.R")
  source("code/analysis/helpers/covariates.R")
  source("code/analysis/helpers/choice.R")
  source("code/analysis/helpers/supply.R")
} else {
  cat("--- Demand: coefficients found, skipping ---\n")
}

# =========================================================================
# STEP 2: Pricing (OLS cost coefs + supply diagnostics) — skip if done
# =========================================================================

ols_coefs_exist <- file.exists(file.path(TEMP_DIR, "ra_rs_coefs.csv")) &&
                   file.exists(file.path(TEMP_DIR, "ra_claims_coefs.csv"))

if (!ols_coefs_exist) {
  cat("\n--- Pricing (markups, OLS cost coefs) ---\n")
  source("code/analysis/helpers/ra.R")
  source("code/analysis/structural/2_pricing.R")
  rm(list = setdiff(ls(), c("TEMP_DIR", "SAMPLE_FRAC", "MASTER_SEED",
                              "STRUCTURAL_SPEC", "STRUCTURAL_ASST")))
  gc(full = TRUE, verbose = FALSE)
} else {
  cat("--- Pricing: OLS cost coefs found, skipping ---\n")
}


# =========================================================================
# STEP 3: Joint GMM
# =========================================================================

cat("\n--- Joint demand-supply GMM ---\n")

# Full cleanup before heavy estimation
rm(list = setdiff(ls(), character(0)))
gc(full = TRUE, verbose = FALSE)

source("code/analysis/structural/3_joint_gmm.R")
