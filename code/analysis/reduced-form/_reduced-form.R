# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Description:   Master runner for reduced-form analysis (design-based).
##                Sources shared scripts 1-2 (data + IPW), partitions data
##                for choice workers, then runs reduced-form scripts 3-6.

# Setup -------------------------------------------------------------------
source("code/0-setup.R")
library(arrow)

# Helpers -----------------------------------------------------------------
source("code/data-build/_helpers-enrollment.R")  # for standardize_insurer()
source("code/analysis/_helpers-analysis.R")
source("code/analysis/_helpers-choice.R")

# Ensure analysis data is ready -------------------------------------------
if (!exists("hh_full") || !"ipweight" %in% names(hh_full)) {
  cat("Analysis data not in memory. Running scripts 1-2...\n")
  source("code/analysis/1_decision-analysis.R")
  source("code/analysis/2_summary-stats.R")
}

# Exclude catastrophic plan enrollees (consistent with structural estimation)
n_before <- nrow(hh_full)
hh_full <- hh_full %>% filter(!grepl("_CAT$", plan_name) | is.na(plan_name))
cat("  Excluded catastrophic HH:", n_before - nrow(hh_full), "of", n_before, "\n")

# Reduced-form regressions ------------------------------------------------
source("code/analysis/reduced-form/3_dominated-choices.R")
source("code/analysis/reduced-form/4_switching.R")

# Prepare and partition data for choice model workers ----------------------

cat("Preparing partitioned data for choice model workers...\n")

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
         household_size, ipweight,
         perc_0to17, perc_18to34, perc_35to54,
         perc_black, perc_hispanic, perc_asian, perc_other, perc_male,
         channel)

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
rm(hh_full, hh_clean, hh_ins, plan_choice, plan_data)
if (exists("hh_po")) rm(hh_po)
gc(verbose = FALSE)
cat("  Large objects freed from memory.\n")

# Demand estimation (ATT) -------------------------------------------------
source("code/analysis/reduced-form/5_choice-att.R")
source("code/analysis/reduced-form/6_choice-summary.R")
