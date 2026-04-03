# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Date Edited:   2026-03-25
## Description:   Master runner for reduced-form analysis.
##                Reads analysis data from disk (built by scripts 1-2),
##                adds control function residual (broker density IV),
##                prepares partitioned data, runs estimation scripts.

# Pipeline parameters (set in _analysis.R, recovered via env vars)
SAMPLE_FRAC <- as.numeric(Sys.getenv("SAMPLE_FRAC"))
MASTER_SEED <- as.integer(Sys.getenv("MASTER_SEED"))

# Setup -------------------------------------------------------------------
source("code/0-setup.R")
library(arrow)

# Helpers -----------------------------------------------------------------
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")

# Reduced-form specification
REDUCED_FORM_SPEC <- c(
  "premium", "penalty_own",
  "silver", "bronze", "hmo", "hsa",
  "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
  "Anthem_silver", "BS_silver", "Kaiser_silver", "HN_silver",
  "Anthem_bronze", "BS_bronze", "Kaiser_bronze", "HN_bronze",
  "hh_size_prem", "perc_0to17_prem", "perc_18to34_prem", "perc_35to54_prem",
  "perc_male_prem", "perc_black_prem", "perc_hispanic_prem", "perc_asian_prem", "perc_other_prem",
  "FPL_250to400_prem", "FPL_400plus_prem"
)



# =========================================================================
# Read data
# =========================================================================

cat("Reading analysis data from disk...\n")
hh_full    <- read_csv("data/output/hh_full.csv", show_col_types = FALSE)
hh_clean   <- read_csv("data/output/hh_clean.csv", show_col_types = FALSE)
plan_data  <- read_csv("data/input/Covered California/plan_data.csv",
                        show_col_types = FALSE, name_repair = "minimal")
broker_density <- read_csv("data/output/broker_density.csv", show_col_types = FALSE)
commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

cat("  hh_full:", nrow(hh_full), "rows\n")
cat("  hh_clean:", nrow(hh_clean), "rows\n")
cat("  broker_density:", nrow(broker_density), "rows\n")

# =========================================================================
# Exclude catastrophic plans
# =========================================================================

n_before <- nrow(hh_full)
hh_full  <- hh_full %>% filter(!grepl("_CAT$", plan_name) | is.na(plan_name))
hh_clean <- hh_clean %>% filter(!grepl("_CAT$", plan_name) | is.na(plan_name))
cat("  Excluded catastrophic HH:", n_before - nrow(hh_full), "of", n_before, "\n")

# =========================================================================
# Control function: first-stage LPM with broker density
# =========================================================================

cat("Computing control function residual...\n")

# Merge broker density (region-year level)
hh_full <- hh_full %>%
  left_join(broker_density %>% select(region, year, n_agents),
            by = c("region", "year"))

# First stage: LPM on all HH
fs_model <- lm(assisted ~ n_agents + FPL + perc_0to17 + perc_18to25 +
                  perc_65plus + perc_black + perc_hispanic + perc_asian +
                  perc_male + household_size + factor(year),
                data = hh_full)

hh_full$v_hat <- residuals(fs_model)

cat("  First-stage F:", round(summary(fs_model)$fstatistic[1], 1), "\n")
cat("  n_agents coef:", round(coef(fs_model)["n_agents"], 6),
    " (SE:", round(summary(fs_model)$coefficients["n_agents", "Std. Error"], 6), ")\n")
cat("  v_hat mean (unassisted):", round(mean(hh_full$v_hat[hh_full$assisted == 0]), 4), "\n")
cat("  v_hat mean (assisted):  ", round(mean(hh_full$v_hat[hh_full$assisted == 1]), 4), "\n")

# Propagate to hh_clean
hh_clean <- hh_clean %>%
  left_join(hh_full %>% select(household_year, v_hat, n_agents),
            by = "household_year")

# =========================================================================
# Plan data prep
# =========================================================================

plan_choice <- plan_data %>%
  select(region, year = ENROLLMENT_YEAR, Issuer_Name, metal_level,
         plan_name = Plan_Name2, network_type = PLAN_NETWORK_TYPE,
         premium = Premium, msp = MSP, hsa = `HSA`) %>%
  mutate(
    region = as.integer(region),
    year   = as.integer(year),
    issuer = standardize_insurer(Issuer_Name),
    metal = case_when(
      metal_level %in% c("Silver", "Silver - Enhanced 73",
                          "Silver - Enhanced 87", "Silver - Enhanced 94") ~ "Silver",
      TRUE ~ metal_level
    )
  ) %>%
  select(-Issuer_Name) %>%
  filter(metal != "Minimum Coverage")

cat("  plan_choice:", nrow(plan_choice), "rows (catastrophic excluded)\n")

# Hausman IV: leave-one-out mean premium
plan_choice <- plan_choice %>%
  group_by(issuer, metal, year) %>%
  mutate(
    n_other = n() - 1L,
    hausman_iv = (sum(premium) - premium) / pmax(n_other, 1L)
  ) %>%
  ungroup() %>%
  mutate(hausman_iv = ifelse(n_other == 0, NA_real_, hausman_iv)) %>%
  group_by(issuer, metal, year) %>%
  mutate(hausman_iv = ifelse(is.na(hausman_iv), mean(premium, na.rm = TRUE), hausman_iv)) %>%
  ungroup() %>%
  select(-n_other)

# First-stage for premium endogeneity (CF residual for structural, kept here for consistency)
first_stage <- lm(premium ~ hausman_iv + metal + network_type + factor(year),
                  data = plan_choice)
plan_choice$cf_resid <- residuals(first_stage)

fs_summary <- summary(first_stage)
fs_fstat <- fs_summary$fstatistic
cat("  Premium first-stage F:", round(fs_fstat[1], 1), "\n")

plan_choice$comm_pmpm <- 0
for (y in unique(plan_choice$year)) {
  idx <- plan_choice$year == y
  plan_choice$comm_pmpm[idx] <- get_commission_pmpm(
    plan_choice$plan_name[idx], plan_choice[idx, ], y, commission_lookup
  )
}

write_csv(plan_choice, "data/output/plan_choice.csv")
cat("  plan_choice saved -> data/output/plan_choice.csv\n")

# =========================================================================
# Partition HH data for choice model
# =========================================================================

hh_choice <- hh_full %>%
  filter(!grepl("_CAT$", plan_name) | is.na(plan_name)) %>%
  mutate(
    region = as.integer(region),
    year   = as.integer(year),
    cutoff = AFFORD_THRESHOLDS[as.character(year)]
  ) %>%
  select(region, year, household_id, FPL, subsidized_members, rating_factor,
         plan_number_nocsr, plan_name, previous_plan_number,
         oldest_member, cheapest_premium,
         subsidy, penalty, poverty_threshold, cutoff,
         household_size, ipweight, v_hat,
         perc_0to17, perc_18to34, perc_35to54,
         perc_black, perc_hispanic, perc_asian, perc_other, perc_male,
         channel)

cat("  hh_choice:", nrow(hh_choice), "rows,", ncol(hh_choice), "cols\n")

partition_dir <- "data/output/hh_choice_partitions"
if (dir.exists(partition_dir)) unlink(partition_dir, recursive = TRUE)
dir.create(partition_dir, recursive = TRUE)

hh_dt <- as.data.table(hh_choice)
rm(hh_choice)
gc(verbose = FALSE)

hh_dt[, cell_key := paste0("hh_", region, "_", year)]
split_list <- split(hh_dt, by = "cell_key", keep.by = FALSE)
rm(hh_dt)
gc(verbose = FALSE)

for (nm in names(split_list)) {
  write_parquet(split_list[[nm]], file.path(partition_dir, paste0(nm, ".parquet")))
}
n_cells <- length(split_list)
rm(split_list)
gc(verbose = FALSE)

cat("  Partitioned:", n_cells, "cells -> ", partition_dir, "\n")

# =========================================================================
# Run reduced-form analysis
# =========================================================================

# Dominated choice ATT
source("code/analysis/reduced-form/1_dominated-choices.R")

# Free large objects before choice model
rm(hh_full, hh_clean, plan_choice, plan_data)
gc(verbose = FALSE)
cat("  Large objects freed.\n")

# Choice model ATT
source("code/analysis/reduced-form/2_choice-att.R")

# Summary
source("code/analysis/reduced-form/3_summary.R")
