# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Description:   Master runner for structural estimation (demand + supply).
##                Data prep runs only when intermediate files are missing.
##                Packages and helpers loaded once by _analysis.R.

# Packages and helpers loaded by _analysis.R

TEMP_DIR <- Sys.getenv("TEMP_DIR")

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

write_demand_spec(STRUCTURAL_SPEC, STRUCTURAL_ASST, file.path(TEMP_DIR, "demand_spec.csv"))


# =========================================================================
# DATA PREP — only when intermediate files are missing
# =========================================================================

prep_files <- c(
  file.path(TEMP_DIR, "plan_choice.csv"),
  file.path(TEMP_DIR, "plan_demographics.csv"),
  file.path(TEMP_DIR, "hh_choice.csv")
)

if (all(file.exists(prep_files))) {
  cat("Data prep files found — skipping to estimation.\n\n")
} else {
  cat("Building intermediate files...\n\n")

  cat("Reading analysis data from disk...\n")
  hh_full    <- read_csv("data/output/hh_full.csv", show_col_types = FALSE)
  plan_data  <- read_csv("data/input/Covered California/plan_data.csv",
                          show_col_types = FALSE, name_repair = "minimal")
  broker_density <- read_csv("data/output/broker_density.csv", show_col_types = FALSE)
  commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

  # Structural uses hh_size as the weight (not ipweight/ATT).
  cat("  hh_full:", nrow(hh_full), "rows\n")

  # Control function residual (broker density IV). `assisted` is observed
  # only on insured (CC) rows; fit on those and leave v_hat = NA for ACS.
  cat("Computing control function residual...\n")
  hh_full <- hh_full %>%
    left_join(broker_density %>% select(region, year, n_agents),
              by = c("region", "year"))
  ins_idx <- which(hh_full$insured == 1L)
  fs_model <- lm(assisted ~ n_agents + FPL + perc_0to17 + perc_18to25 +
                    perc_65plus + perc_black + perc_hispanic + perc_asian +
                    perc_male + household_size + factor(year),
                  data = hh_full[ins_idx, ])
  hh_full$v_hat <- NA_real_
  hh_full$v_hat[ins_idx] <- residuals(fs_model)
  cat("  First-stage F:", round(summary(fs_model)$fstatistic[1], 1), "\n")
  rm(fs_model, broker_density, ins_idx)

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
  hh_full <- hh_full %>% filter(!grepl("_CAT$", plan_id) | is.na(plan_id))
  cat("  Excluded catastrophic HH:", n_before - nrow(hh_full), "\n")

  # Plan demographics (CSR-enhanced silver short codes collapsed for averaging)
  cat("Computing plan-level demographics...\n")
  plan_demographics <- hh_full %>%
    filter(!is.na(plan_id), plan_id != "Uninsured") %>%
    mutate(plan_id = gsub("SIL(94|73|87)", "SIL", plan_id),
           wt = household_size) %>%
    group_by(plan_id, year) %>%
    summarize(share_18to34 = weighted.mean(perc_18to34, wt, na.rm = TRUE),
              share_35to54 = weighted.mean(perc_35to54, wt, na.rm = TRUE),
              share_hispanic = weighted.mean(perc_hispanic, wt, na.rm = TRUE),
              n_hh = n(), .groups = "drop")
  plan_demographics_yr <- plan_demographics %>%
    group_by(plan_id, year) %>%
    summarize(share_18to34 = weighted.mean(share_18to34, n_hh, na.rm = TRUE),
              share_35to54 = weighted.mean(share_35to54, n_hh, na.rm = TRUE),
              share_hispanic = weighted.mean(share_hispanic, n_hh, na.rm = TRUE),
              .groups = "drop")
  write_csv(plan_demographics_yr, file.path(TEMP_DIR, "plan_demographics.csv"))
  cat("  Plan demographics:", nrow(plan_demographics_yr), "rows\n")
  rm(plan_demographics, plan_demographics_yr)

  # Build plan_choice. Keep `metal` CSR-aware (matches enroll/HH metal).
  # base_metal collapses CSR-enhanced silvers for Hausman IV grouping.
  plan_choice <- plan_data %>%
    select(region, year = ENROLLMENT_YEAR, Issuer_Name,
           metal = metal_level,
           plan_id = Plan_Name2, network_type = PLAN_NETWORK_TYPE,
           premium = Premium, msp = MSP, hsa = `HSA`) %>%
    mutate(region = as.integer(region), year = as.integer(year),
           issuer = standardize_insurer(Issuer_Name),
           base_metal = sub(" - Enhanced.*", "", metal)) %>%
    select(-Issuer_Name) %>%
    filter(metal != "Minimum Coverage")

  plan_choice <- plan_choice %>%
    group_by(issuer, base_metal, year) %>%
    mutate(n_other = n() - 1L,
           hausman_iv = (sum(premium) - premium) / pmax(n_other, 1L)) %>%
    ungroup() %>%
    mutate(hausman_iv = ifelse(n_other == 0, NA_real_, hausman_iv)) %>%
    group_by(issuer, base_metal, year) %>%
    mutate(hausman_iv = ifelse(is.na(hausman_iv), mean(premium, na.rm = TRUE), hausman_iv)) %>%
    ungroup() %>%
    select(-n_other, -base_metal)

  first_stage <- lm(premium ~ hausman_iv + metal + network_type + factor(year),
                    data = plan_choice)
  plan_choice$cf_resid <- residuals(first_stage)
  cat("  Hausman first-stage F:", round(summary(first_stage)$fstatistic[1], 1), "\n")
  rm(first_stage)

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
  cat("  plan_choice:", nrow(plan_choice), "rows\n")

  # Write HH data (single file, split in memory by readers).
  cat("Writing HH choice data...\n")
  hh_choice <- hh_full %>%
    filter(!grepl("_CAT$", plan_id) | is.na(plan_id)) %>%
    mutate(region = as.integer(region), year = as.integer(year),
           cutoff = AFFORD_THRESHOLDS[as.character(year)]) %>%
    select(region, year, household_id, FPL, subsidized_members, rating_factor,
           plan_id, oldest_member, cheapest_premium, subsidy, penalty,
           poverty_threshold, cutoff, household_size, v_hat,
           perc_0to17, perc_18to34, perc_35to54,
           perc_black, perc_hispanic, perc_asian, perc_other, perc_male,
           channel, channel_detail, any_agent, p_nav)

  hh_choice_path <- file.path(TEMP_DIR, "hh_choice.csv")
  write.csv(hh_choice, hh_choice_path, row.names = FALSE)
  n_cells <- length(unique(paste0(hh_choice$region, "_", hh_choice$year)))
  cat("  Written:", n_cells, "cells ->", hh_choice_path, "\n")
  rm(hh_choice, hh_full, plan_data, plan_choice, commission_lookup)
  gc(full = TRUE, verbose = FALSE)
  cat("  Data prep complete.\n\n")
}

# =========================================================================
# SHARED DATA (read once, used by all structural scripts)
# =========================================================================

cat("Loading shared structural data...\n")
hh_all <- as.data.table(read.csv(file.path(TEMP_DIR, "hh_choice.csv")))
hh_split <- split(hh_all, by = c("region", "year"), keep.by = FALSE)
cells <- unique(hh_all[, .(region, year)])[order(region, year)]
rm(hh_all); gc(verbose = FALSE)

plan_choice <- read_csv(file.path(TEMP_DIR, "plan_choice.csv"), show_col_types = FALSE)
commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

set.seed(MASTER_SEED)
cell_seeds <- sample.int(1e7, nrow(cells))

cat("  Cells:", nrow(cells), "\n\n")

# =========================================================================
# DEMAND
# =========================================================================
if (file.exists("results/choice_coefficients_structural.csv")) {
  cat("--- Demand: coefficients found, skipping ---\n")
} else {
  cat("--- Demand estimation ---\n")
  source("code/analysis/structural/1_demand.R")
  gc(full = TRUE, verbose = FALSE)
}

# =========================================================================
# PRICING
# =========================================================================
cat("\n--- Pricing (markups, FOC inputs) ---\n")
source("code/analysis/structural/2_pricing.R")
gc(full = TRUE, verbose = FALSE)

# =========================================================================
# COST-SIDE GMM
# =========================================================================
cat("\n--- Cost-side GMM ---\n")
source("code/analysis/structural/3_cost_gmm.R")
gc(full = TRUE, verbose = FALSE)

# =========================================================================
# COUNTERFACTUALS
# =========================================================================

# Free hh_split before counterfactuals (CF reads per-cell from disk)
rm(hh_split)
gc(full = TRUE, verbose = FALSE)

cat("\n--- Counterfactual simulation ---\n")
source("code/analysis/structural/4_counterfactuals.R")

rm(cells, cell_seeds, plan_choice, commission_lookup)
gc(full = TRUE, verbose = FALSE)

cat("\n=== Structural pipeline complete ===\n")
