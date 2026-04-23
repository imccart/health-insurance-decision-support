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
TEMP_DIR    <- Sys.getenv("TEMP_DIR")

# Packages and helpers loaded by _analysis.R

# Reduced-form specification
REDUCED_FORM_SPEC <- c(
  "premium",
  "silver", "bronze", "hmo", "hsa",
  "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
  "hh_size_prem", "perc_0to17_prem", "perc_18to34_prem", "perc_35to54_prem",
  "perc_male_prem", "perc_black_prem", "perc_hispanic_prem", "perc_asian_prem", "perc_other_prem",
  "FPL_250to400_prem", "FPL_400plus_prem"
)

# CF interaction terms (v_hat × plan indicators for selection correction)
REDUCED_FORM_CF <- c(
  "cf_anthem", "cf_blue_shield", "cf_kaiser", "cf_health_net",
  "cf_silver", "cf_bronze"
)

# Full spec for cell building and Julia estimation
REDUCED_FORM_FULL <- c(REDUCED_FORM_SPEC, REDUCED_FORM_CF)

# Write spec for Julia
write_demand_spec(REDUCED_FORM_FULL, character(0),
                  file.path(TEMP_DIR, "demand_spec_reduced.csv"))

# =========================================================================
# Read data
# =========================================================================

cat("Reading analysis data from disk...\n")
hh_full    <- read_csv("data/output/hh_full.csv", show_col_types = FALSE) %>%
  mutate(region = as.integer(region), year = as.integer(year))
hh_clean   <- read_csv("data/output/hh_clean.csv", show_col_types = FALSE) %>%
  mutate(region = as.integer(region), year = as.integer(year))
ipweights  <- read_csv("data/output/ipweights.csv", show_col_types = FALSE)
plan_data  <- read_csv("data/input/Covered California/plan_data.csv",
                        show_col_types = FALSE, name_repair = "minimal")
broker_density <- read_csv("data/output/broker_density.csv", show_col_types = FALSE) %>%
  mutate(region = as.integer(region), year = as.integer(year))
commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

# Join IPW weights (from 2_ipw.R)
hh_full  <- hh_full %>%
  left_join(ipweights, by = "household_year")
hh_clean <- hh_clean %>%
  left_join(ipweights, by = "household_year")
rm(ipweights)

cat("  hh_full:", nrow(hh_full), "rows\n")
cat("  hh_clean:", nrow(hh_clean), "rows\n")
cat("  broker_density:", nrow(broker_density), "rows\n")

# =========================================================================
# Exclude catastrophic plans
# =========================================================================

n_before <- nrow(hh_full)
hh_full  <- hh_full %>% filter(!grepl("_CAT$", plan_id) | is.na(plan_id))
hh_clean <- hh_clean %>% filter(!grepl("_CAT$", plan_id) | is.na(plan_id))
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

fs_summary <- summary(fs_model)
cat("  First-stage F:", round(fs_summary$fstatistic[1], 1), "\n")
cat("  n_agents coef:", round(coef(fs_model)["n_agents"], 6),
    " (SE:", round(fs_summary$coefficients["n_agents", "Std. Error"], 6), ")\n")
cat("  v_hat mean (unassisted):", round(mean(hh_full$v_hat[hh_full$assisted == 0]), 4), "\n")
cat("  v_hat mean (assisted):  ", round(mean(hh_full$v_hat[hh_full$assisted == 1]), 4), "\n")
rm(fs_model, fs_summary, broker_density)

# Propagate to hh_clean
hh_clean <- hh_clean %>%
  left_join(hh_full %>% select(household_year, v_hat, n_agents),
            by = "household_year")

# =========================================================================
# Plan data prep
# =========================================================================

plan_choice <- plan_data %>%
  select(region, year = ENROLLMENT_YEAR, Issuer_Name,
         metal = metal_level,
         plan_id = Plan_Name2, network_type = PLAN_NETWORK_TYPE,
         premium = Premium, msp = MSP, hsa = `HSA`) %>%
  mutate(
    region = as.integer(region),
    year   = as.integer(year),
    issuer = standardize_insurer(Issuer_Name),
    base_metal = sub(" - Enhanced.*", "", metal)
  ) %>%
  select(-Issuer_Name) %>%
  filter(metal != "Minimum Coverage")

cat("  plan_choice:", nrow(plan_choice), "rows (catastrophic excluded)\n")

# Hausman IV: leave-one-out mean premium (group on base_metal so all silver
# variants share an IV).
plan_choice <- plan_choice %>%
  group_by(issuer, base_metal, year) %>%
  mutate(
    n_other = n() - 1L,
    hausman_iv = (sum(premium) - premium) / pmax(n_other, 1L)
  ) %>%
  ungroup() %>%
  mutate(hausman_iv = ifelse(n_other == 0, NA_real_, hausman_iv)) %>%
  group_by(issuer, base_metal, year) %>%
  mutate(hausman_iv = ifelse(is.na(hausman_iv), mean(premium, na.rm = TRUE), hausman_iv)) %>%
  ungroup() %>%
  select(-n_other, -base_metal)

# First-stage for premium endogeneity (CF residual for structural, kept here for consistency)
first_stage <- lm(premium ~ hausman_iv + metal + network_type + factor(year),
                  data = plan_choice)
plan_choice$cf_resid <- residuals(first_stage)
cat("  Premium first-stage F:", round(summary(first_stage)$fstatistic[1], 1), "\n")
rm(first_stage)

# Commission PMPM: join lookup, compute flat or pct-of-premium
plan_choice <- plan_choice %>%
  mutate(insurer_prefix = sub("_.*", "", plan_id)) %>%
  left_join(commission_lookup, by = c("insurer_prefix", "year")) %>%
  mutate(
    comm_pmpm = case_when(
      is.na(rate) ~ 0,
      is_pct ~ rate * premium,
      TRUE ~ rate
    )
  ) %>%
  select(-insurer_prefix, -rate, -is_pct)

write_csv(plan_choice, file.path(TEMP_DIR, "plan_choice.csv"))
cat("  plan_choice saved ->", file.path(TEMP_DIR, "plan_choice.csv"), "\n")
rm(commission_lookup)

# =========================================================================
# Partition HH data for choice model. plan_id is the canonical identifier;
# ipweight retained on the reduced-form HH file (used by 1_dominated-choices.R).
# =========================================================================

hh_choice <- hh_full %>%
  filter(!grepl("_CAT$", plan_id) | is.na(plan_id)) %>%
  select(region, year, household_id, FPL, subsidized_members, rating_factor,
         plan_id, oldest_member, cheapest_premium,
         subsidy, penalty, poverty_threshold,
         household_size, ipweight, v_hat,
         perc_0to17, perc_18to34, perc_35to54,
         perc_black, perc_hispanic, perc_asian, perc_other, perc_male,
         channel)

cat("  hh_choice:", nrow(hh_choice), "rows,", ncol(hh_choice), "cols\n")

hh_choice_path <- file.path(TEMP_DIR, "hh_choice_rf.csv")
fwrite(hh_choice, hh_choice_path)
n_cells <- length(unique(paste0(hh_choice$region, "_", hh_choice$year)))
rm(hh_choice)
gc(verbose = FALSE)

cat("  Written:", n_cells, "cells -> ", hh_choice_path, "\n")

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
