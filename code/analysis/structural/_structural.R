# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Description:   Master runner for structural estimation (demand + supply).
##                Sources shared scripts 1-2 (data + IPW), partitions data,
##                then runs structural demand (all HH), supply recovery,
##                and counterfactual simulation.

# Setup -------------------------------------------------------------------
source("code/0-setup.R")
library(arrow)

# Helpers -----------------------------------------------------------------
source("code/data-build/_helpers-enrollment.R")  # for standardize_insurer()
source("code/analysis/_helpers-analysis.R")
source("code/analysis/_helpers-choice.R")
source("code/analysis/_helpers-supply.R")

# Read data from disk -----------------------------------------------------

cat("Reading analysis data from disk...\n")
hh_full    <- read_csv("data/output/hh_full.csv", show_col_types = FALSE)
plan_data  <- read_csv("data/input/Covered California/plan_data.csv",
                        show_col_types = FALSE, name_repair = "minimal")
broker_density <- read_csv("data/output/broker_density.csv", show_col_types = FALSE)
commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

cat("  hh_full:", nrow(hh_full), "rows\n")

# Control function: first-stage LPM with broker density -------------------

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

# Navigator propensity model -----------------------------------------------
# Among assisted HH, predict P(navigator vs. broker/agent).
# Used in counterfactual welfare decomposition to allocate broker-assisted
# HH to navigator substitution by propensity rather than randomly.

cat("Estimating navigator propensity model...\n")
assisted_hh <- hh_full %>% filter(assisted == 1)
nav_model <- glm(
  (channel_detail == "Navigator") ~ FPL + perc_hispanic + perc_black +
    perc_0to17 + perc_65plus + household_size + perc_male + factor(year),
  data = assisted_hh, family = binomial
)
cat("  Navigator model N =", nrow(assisted_hh),
    ", AUC proxy (% navigator):", round(mean(assisted_hh$channel_detail == "Navigator") * 100, 1), "%\n")

# Predict for ALL HH (not just assisted) — score is used to rank
# broker-assisted HH by likelihood of switching to navigator
hh_full$p_nav <- predict(nav_model, newdata = hh_full, type = "response")
# For unassisted HH, p_nav is hypothetical but harmless
cat("  p_nav range:", round(range(hh_full$p_nav, na.rm = TRUE), 3), "\n")

rm(assisted_hh, nav_model)

# Prepare and partition data -----------------------------------------------

cat("Preparing partitioned data for structural estimation...\n")

# Exclude catastrophic HH
n_before <- nrow(hh_full)
hh_full <- hh_full %>% filter(!grepl("_CAT$", plan_name) | is.na(plan_name))
cat("  Excluded catastrophic HH:", n_before - nrow(hh_full), "\n")

# Plan-level demographics from observed enrollment (for RA regression) -----
# Weighted mean of HH demographics by chosen plan-year.
# These are structural parameters of the risk score model, estimated once.
cat("Computing plan-level demographics from observed enrollment...\n")
plan_demographics <- hh_full %>%
  filter(!is.na(plan_name), plan_name != "Uninsured") %>%
  mutate(
    plan_name = gsub("SIL(94|73|87)", "SIL", plan_name),
    wt = ifelse(is.na(ipweight), 1, ipweight)
  ) %>%
  group_by(plan_name, year) %>%
  summarize(
    share_18to34  = weighted.mean(perc_18to34, wt, na.rm = TRUE),
    share_35to54  = weighted.mean(perc_35to54, wt, na.rm = TRUE),
    share_hispanic = weighted.mean(perc_hispanic, wt, na.rm = TRUE),
    n_hh = n(),
    .groups = "drop"
  )
# Average across regions to get plan-year level (matching rate filing granularity)
plan_demographics_yr <- plan_demographics %>%
  group_by(plan_name, year) %>%
  summarize(
    share_18to34  = weighted.mean(share_18to34, n_hh, na.rm = TRUE),
    share_35to54  = weighted.mean(share_35to54, n_hh, na.rm = TRUE),
    share_hispanic = weighted.mean(share_hispanic, n_hh, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(plan_demographics_yr, "data/output/plan_demographics.csv")
cat("  Plan demographics:", nrow(plan_demographics_yr), "plan-year rows -> data/output/plan_demographics.csv\n")
rm(plan_demographics, plan_demographics_yr)

# Rename plan_data columns at the boundary
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

cat("  Excluded catastrophic plans. plan_choice:", nrow(plan_choice), "rows\n")

# Hausman IV: leave-one-out mean premium of same insurer-metal in other regions
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

# First-stage regression: premium on Hausman IV + controls
first_stage <- lm(premium ~ hausman_iv + metal + network_type + factor(year),
                  data = plan_choice)
plan_choice$cf_resid <- residuals(first_stage)

# First-stage diagnostics
fs_summary <- summary(first_stage)
fs_fstat <- fs_summary$fstatistic
cat("  First-stage F-stat:", round(fs_fstat[1], 1),
    " (p =", format.pval(pf(fs_fstat[1], fs_fstat[2], fs_fstat[3], lower.tail = FALSE)), ")\n")
cat("  Hausman IV coefficient:", round(coef(first_stage)["hausman_iv"], 4),
    " (SE:", round(fs_summary$coefficients["hausman_iv", "Std. Error"], 4), ")\n")

# Commission data: attach comm_pmpm to plan_choice for structural demand
cat("  Commission lookup:", nrow(commission_lookup), "rows,",
    length(unique(commission_lookup$insurer_prefix)), "insurers\n")

plan_choice$comm_pmpm <- 0
for (y in unique(plan_choice$year)) {
  idx <- plan_choice$year == y
  plan_choice$comm_pmpm[idx] <- get_commission_pmpm(
    plan_choice$plan_name[idx], plan_choice[idx, ], y, commission_lookup
  )
}
cat("  Commission PMPM: nonzero =", sum(plan_choice$comm_pmpm > 0), "/", nrow(plan_choice),
    ", median (nonzero) =", round(median(plan_choice$comm_pmpm[plan_choice$comm_pmpm > 0]), 2),
    ", range = [", round(min(plan_choice$comm_pmpm), 2),
    ",", round(max(plan_choice$comm_pmpm), 2), "]\n")

write_csv(plan_choice, "data/output/plan_choice.csv")
cat("  plan_choice:", nrow(plan_choice), "rows -> data/output/plan_choice.csv\n")

# Select only columns needed by build_choice_data, add affordability threshold
# Exclude HH who chose a catastrophic plan (consistent with plan_choice filter)
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
         channel, channel_detail, any_agent, p_nav)

cat("  hh_choice:", nrow(hh_choice), "rows,", ncol(hh_choice), "cols (catastrophic HH excluded)\n")

# Write as partitioned parquet files (one per region-year cell)
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

cat("  Partitioned parquet written to", partition_dir, "(", n_cells, "cells)\n")

# Free all large objects — subprocesses read from disk
rm(hh_full, plan_choice, plan_data)
if (exists("hh_po")) rm(hh_po)
gc(verbose = FALSE)
cat("  Large objects freed from memory.\n")

# Structural demand estimation (all HH) -----------------------------------
source("code/analysis/structural/1_demand.R")

# Supply-side recovery -----------------------------------------------------
source("code/analysis/structural/2_supply.R")

# Counterfactual simulation ------------------------------------------------
source("code/analysis/structural/3_counterfactuals.R")
