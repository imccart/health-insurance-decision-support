# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Description:   Master runner for structural estimation (demand + supply).
##                Data prep runs only when intermediate files are missing.
##                Aggressive memory cleanup between stages.

# Pipeline parameters (set in _analysis.R, recovered via env vars)
TEMP_DIR     <- Sys.getenv("TEMP_DIR")
SAMPLE_FRAC  <- as.numeric(Sys.getenv("SAMPLE_FRAC"))
MASTER_SEED  <- as.integer(Sys.getenv("MASTER_SEED"))

# Setup -------------------------------------------------------------------
source("code/0-setup.R")
library(arrow)


# Helpers -----------------------------------------------------------------
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")


# Covariate spec: insurer FEs, insurer x metal, demo x premium/insured.
# Commission and assisted x metal appended in STRUCTURAL_ASST.
# Spec written to TEMP_DIR/demand_spec.csv for Julia and cf_worker.

STRUCTURAL_SPEC <- c(
  "premium", "penalty_own",
  "silver", "bronze", "hmo", "hsa",
  "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
#  "Anthem_silver", "BS_silver", "Kaiser_silver", "HN_silver",
#  "Anthem_bronze", "BS_bronze", "Kaiser_bronze", "HN_bronze",
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
# DATA PREP — only when intermediate files are missing
# =========================================================================

prep_files <- c(
  file.path(TEMP_DIR, "plan_choice.csv"),
  file.path(TEMP_DIR, "plan_demographics.csv"),
  file.path(TEMP_DIR, "hh_choice_partitions")
)
prep_done <- all(file.exists(prep_files))

if (prep_done) {
  cat("Data prep files found — skipping to estimation.\n\n")
} else {
  cat("Building intermediate files...\n\n")

  cat("Reading analysis data from disk...\n")
  hh_full    <- read_csv("data/output/hh_full.csv", show_col_types = FALSE)
  ipweights  <- read_csv("data/output/ipweights.csv", show_col_types = FALSE)
  plan_data  <- read_csv("data/input/Covered California/plan_data.csv",
                          show_col_types = FALSE, name_repair = "minimal")
  broker_density <- read_csv("data/output/broker_density.csv", show_col_types = FALSE)
  commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

  # Join IPW weights (from 2_ipw.R)
  hh_full <- hh_full %>%
    left_join(ipweights, by = "household_year")
  rm(ipweights)

  cat("  hh_full:", nrow(hh_full), "rows\n")

  # Control function
  cat("Computing control function residual...\n")
  hh_full <- hh_full %>%
    left_join(broker_density %>% select(region, year, n_agents),
              by = c("region", "year"))

  fs_model <- lm(assisted ~ n_agents + FPL + perc_0to17 + perc_18to25 +
                    perc_65plus + perc_black + perc_hispanic + perc_asian +
                    perc_male + household_size + factor(year),
                  data = hh_full)
  hh_full$v_hat <- residuals(fs_model)
  cat("  First-stage F:", round(summary(fs_model)$fstatistic[1], 1), "\n")
  rm(fs_model, broker_density)

  # Navigator propensity
  cat("Estimating navigator propensity model...\n")
  assisted_hh <- hh_full %>% filter(assisted == 1)
  nav_model <- glm(
    (channel_detail == "Navigator") ~ FPL + perc_hispanic + perc_black +
      perc_0to17 + perc_65plus + household_size + perc_male + factor(year),
    data = assisted_hh, family = binomial
  )
  hh_full$p_nav <- predict(nav_model, newdata = hh_full, type = "response")
  cat("  p_nav range:", round(range(hh_full$p_nav, na.rm = TRUE), 3), "\n")
  rm(assisted_hh, nav_model)

  # Exclude catastrophic
  n_before <- nrow(hh_full)
  hh_full <- hh_full %>% filter(!grepl("_CAT$", plan_name) | is.na(plan_name))
  cat("  Excluded catastrophic HH:", n_before - nrow(hh_full), "\n")

  # Plan demographics
  cat("Computing plan-level demographics...\n")
  plan_demographics <- hh_full %>%
    filter(!is.na(plan_name), plan_name != "Uninsured") %>%
    mutate(plan_name = gsub("SIL(94|73|87)", "SIL", plan_name),
           wt = ifelse(is.na(ipweight), 1, ipweight)) %>%
    group_by(plan_name, year) %>%
    summarize(share_18to34 = weighted.mean(perc_18to34, wt, na.rm = TRUE),
              share_35to54 = weighted.mean(perc_35to54, wt, na.rm = TRUE),
              share_hispanic = weighted.mean(perc_hispanic, wt, na.rm = TRUE),
              n_hh = n(), .groups = "drop")
  plan_demographics_yr <- plan_demographics %>%
    group_by(plan_name, year) %>%
    summarize(share_18to34 = weighted.mean(share_18to34, n_hh, na.rm = TRUE),
              share_35to54 = weighted.mean(share_35to54, n_hh, na.rm = TRUE),
              share_hispanic = weighted.mean(share_hispanic, n_hh, na.rm = TRUE),
              .groups = "drop")
  write_csv(plan_demographics_yr, file.path(TEMP_DIR, "plan_demographics.csv"))
  cat("  Plan demographics:", nrow(plan_demographics_yr), "rows\n")
  rm(plan_demographics, plan_demographics_yr)

  # Build plan_choice
  plan_choice <- plan_data %>%
    select(region, year = ENROLLMENT_YEAR, Issuer_Name, metal_level,
           plan_name = Plan_Name2, network_type = PLAN_NETWORK_TYPE,
           premium = Premium, msp = MSP, hsa = `HSA`) %>%
    mutate(region = as.integer(region), year = as.integer(year),
           issuer = standardize_insurer(Issuer_Name),
           metal = case_when(
             metal_level %in% c("Silver", "Silver - Enhanced 73",
                                 "Silver - Enhanced 87", "Silver - Enhanced 94") ~ "Silver",
             TRUE ~ metal_level)) %>%
    select(-Issuer_Name) %>%
    filter(metal != "Minimum Coverage")

  plan_choice <- plan_choice %>%
    group_by(issuer, metal, year) %>%
    mutate(n_other = n() - 1L,
           hausman_iv = (sum(premium) - premium) / pmax(n_other, 1L)) %>%
    ungroup() %>%
    mutate(hausman_iv = ifelse(n_other == 0, NA_real_, hausman_iv)) %>%
    group_by(issuer, metal, year) %>%
    mutate(hausman_iv = ifelse(is.na(hausman_iv), mean(premium, na.rm = TRUE), hausman_iv)) %>%
    ungroup() %>%
    select(-n_other)

  first_stage <- lm(premium ~ hausman_iv + metal + network_type + factor(year),
                    data = plan_choice)
  plan_choice$cf_resid <- residuals(first_stage)
  cat("  Hausman first-stage F:", round(summary(first_stage)$fstatistic[1], 1), "\n")
  rm(first_stage)

  plan_choice$comm_pmpm <- 0
  for (y in unique(plan_choice$year)) {
    idx <- plan_choice$year == y
    plan_choice$comm_pmpm[idx] <- get_commission_pmpm(
      plan_choice$plan_name[idx], plan_choice[idx, ], y, commission_lookup)
  }
  write_csv(plan_choice, file.path(TEMP_DIR, "plan_choice.csv"))
  cat("  plan_choice:", nrow(plan_choice), "rows\n")

  # Partition HH
  cat("Partitioning HH data...\n")
  hh_choice <- hh_full %>%
    filter(!grepl("_CAT$", plan_name) | is.na(plan_name)) %>%
    mutate(region = as.integer(region), year = as.integer(year),
           cutoff = AFFORD_THRESHOLDS[as.character(year)]) %>%
    select(region, year, household_id, FPL, subsidized_members, rating_factor,
           plan_number_nocsr, plan_name, previous_plan_number,
           oldest_member, cheapest_premium, subsidy, penalty,
           poverty_threshold, cutoff, household_size, ipweight, v_hat,
           perc_0to17, perc_18to34, perc_35to54,
           perc_black, perc_hispanic, perc_asian, perc_other, perc_male,
           channel, channel_detail, any_agent, p_nav)

  partition_dir <- file.path(TEMP_DIR, "hh_choice_partitions")
  if (dir.exists(partition_dir)) unlink(partition_dir, recursive = TRUE)
  dir.create(partition_dir, recursive = TRUE)

  hh_dt <- as.data.table(hh_choice)
  rm(hh_choice, hh_full, plan_data, plan_choice, commission_lookup)
  gc(full = TRUE, verbose = FALSE)

  hh_dt[, cell_key := paste0("hh_", region, "_", year)]
  split_list <- split(hh_dt, by = "cell_key", keep.by = FALSE)
  rm(hh_dt); gc(full = TRUE, verbose = FALSE)

  for (nm in names(split_list)) {
    write_parquet(split_list[[nm]], file.path(partition_dir, paste0(nm, ".parquet")))
  }
  cat("  Partitioned:", length(split_list), "cells\n")
  rm(split_list)
  gc(full = TRUE, verbose = FALSE)
  cat("  Data prep complete.\n\n")
}

# =========================================================================
# Clear environment before estimation
# =========================================================================
rm(list = setdiff(ls(), c("prep_done")))
gc(full = TRUE, verbose = FALSE)
suppressPackageStartupMessages({ library(tidyverse); library(data.table); library(arrow) })
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")

# Recover spec from file (survives rm(list=ls()) via env var)
demand_spec <- read_demand_spec(file.path(Sys.getenv("TEMP_DIR"), "demand_spec.csv"))
STRUCTURAL_SPEC <- demand_spec$base
STRUCTURAL_ASST <- demand_spec$assisted

# =========================================================================
# DEMAND
# =========================================================================
if (file.exists("results/choice_coefficients_structural.csv")) {
  cat("--- Demand: coefficients found, skipping ---\n")
} else {
  cat("--- Demand estimation ---\n")
  source("code/analysis/structural/1_demand.R")
}

# Full cleanup: drop ALL objects (data AND functions), force GC
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

# =========================================================================
# PRICING
# =========================================================================
cat("\n--- Pricing (markups, FOC inputs) ---\n")
suppressPackageStartupMessages({ library(tidyverse); library(data.table); library(arrow) })
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")
source("code/analysis/helpers/ra.R")
demand_spec <- read_demand_spec(file.path(Sys.getenv("TEMP_DIR"), "demand_spec.csv"))
STRUCTURAL_SPEC <- demand_spec$base
STRUCTURAL_ASST <- demand_spec$assisted
source("code/analysis/structural/2_pricing.R")

rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

# Detach arrow before GMM — arrow interferes with readRDS()
if ("package:arrow" %in% search()) detach("package:arrow", unload = TRUE)

# =========================================================================
# COST-SIDE GMM
# =========================================================================
cat("\n--- Cost-side GMM ---\n")
source("code/0-setup.R")
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/supply.R")
source("code/analysis/helpers/ra.R")
source("code/analysis/structural/3_cost_gmm.R")

rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)
cat("  Memory after GMM cleanup:\n")
print(gc())

# =========================================================================
# COUNTERFACTUALS
# =========================================================================
cat("\n--- Counterfactual simulation ---\n")
source("code/0-setup.R")
library(arrow)
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")
source("code/analysis/helpers/ra.R")
demand_spec <- read_demand_spec(file.path(Sys.getenv("TEMP_DIR"), "demand_spec.csv"))
STRUCTURAL_SPEC <- demand_spec$base
STRUCTURAL_ASST <- demand_spec$assisted
source("code/analysis/structural/4_counterfactuals.R")

cat("\n=== Structural pipeline complete ===\n")
