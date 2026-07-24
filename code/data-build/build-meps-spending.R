# Build the age x income spending reference table for the welfare objective.
#
# Mean total health expenditure by age x income (% FPL) cells, MEPS 2018 Full
# Year Consolidated file (HC-209), survey-weighted. Output feeds
# welfare_objective.R's household_spending() (the objective money-metric spending
# level). POVLEV18 is continuous percent-of-poverty, so the 250% FPL cut is exact
# (the categorical POVCAT18 breaks only at 100/125/200/400).
#
# Standalone reference build, NOT part of the numbered 1-7 enrollment sequence and
# NOT sourced by _data-build.R: it depends on an external raw file and only re-runs
# when MEPS is refreshed. Run directly from the project root.
#
# Raw file: D:/research-data/meps/h209.dta
#   (from https://meps.ahrq.gov/data_files/pufs/h209/h209dta.zip)
# Output:   data/input/meps_spending_by_demographics.csv

library(haven)
library(dplyr)
library(readr)

raw <- read_dta("D:/research-data/meps/h209.dta",
                col_select = c(DUPERSID, AGELAST, PERWT18F, TOTEXP18, POVLEV18, POVCAT18))

cat("rows read:", nrow(raw), "\n")
cat("cols:", paste(names(raw), collapse = ", "), "\n\n")

meps <- raw %>% mutate(across(everything(), as.numeric))

# --- external validation: weights and mean must match AHRQ's published 2018 ---
cat("weighted population (all ages):", format(sum(meps$PERWT18F), big.mark = ","),
    "(AHRQ 2018: 326,327,888)\n")
cat("weighted mean TOTEXP18 (all ages):",
    round(sum(meps$PERWT18F * meps$TOTEXP18) / sum(meps$PERWT18F), 2),
    "(AHRQ 2018: 6,063)\n\n")

# --- raw distributions (full file) ---
cat("--- raw distributions (full file) ---\n")
cat("AGELAST : min", min(meps$AGELAST), "max", max(meps$AGELAST),
    "| n < 0:", sum(meps$AGELAST < 0), "| NA:", sum(is.na(meps$AGELAST)), "\n")
cat("PERWT18F: min", min(meps$PERWT18F), "max", round(max(meps$PERWT18F)),
    "| n == 0:", sum(meps$PERWT18F == 0), "| NA:", sum(is.na(meps$PERWT18F)), "\n")
cat("TOTEXP18: min", min(meps$TOTEXP18), "max", max(meps$TOTEXP18),
    "| n < 0:", sum(meps$TOTEXP18 < 0), "| NA:", sum(is.na(meps$TOTEXP18)),
    "| mean", round(mean(meps$TOTEXP18), 1), "\n")
cat("POVLEV18: min", round(min(meps$POVLEV18), 1), "max", round(max(meps$POVLEV18), 1),
    "| n < 0:", sum(meps$POVLEV18 < 0), "| NA:", sum(is.na(meps$POVLEV18)), "\n")

# --- sample restrictions (report survivors at each step) ---
n0 <- nrow(meps)
samp <- meps %>% filter(AGELAST >= 0, AGELAST < 65)
cat("\nunder-65 restriction:", nrow(samp), "kept /", n0, "dropped", n0 - nrow(samp), "\n")

n1 <- nrow(samp)
samp <- samp %>% filter(PERWT18F > 0)
cat("positive person weight  :", nrow(samp), "kept /", n1, "dropped", n1 - nrow(samp), "\n")

n2 <- nrow(samp)
samp <- samp %>% filter(!is.na(POVLEV18), !is.na(TOTEXP18))
cat("non-missing POVLEV/TOTEXP:", nrow(samp), "kept /", n2, "dropped", n2 - nrow(samp), "\n")

# --- cell definitions ---
samp <- samp %>%
  mutate(
    age_group = case_when(
      AGELAST <= 17 ~ "0to17",
      AGELAST <= 34 ~ "18to34",
      AGELAST <= 54 ~ "35to54",
      TRUE          ~ "55plus"
    ),
    income = case_when(
      POVLEV18 <  250 ~ "lt250",
      POVLEV18 <  400 ~ "250to400",
      TRUE            ~ "400plus"
    )
  )

cells <- samp %>%
  group_by(age_group, income) %>%
  summarize(
    n_unwtd        = n(),
    mean_spend_wtd = sum(PERWT18F * TOTEXP18) / sum(PERWT18F),
    .groups = "drop"
  ) %>%
  mutate(
    age_group = factor(age_group, levels = c("0to17", "18to34", "35to54", "55plus")),
    income    = factor(income,    levels = c("lt250", "250to400", "400plus"))
  ) %>%
  arrange(age_group, income)

cat("\n--- cell results (unweighted n, weighted mean spend) ---\n")
print(as.data.frame(cells %>% mutate(mean_spend_wtd = round(mean_spend_wtd, 0))))

# --- write output ---
out <- cells %>%
  transmute(
    age_group = as.character(age_group),
    income    = as.character(income),
    mean_spend = round(mean_spend_wtd, 0),
    source = paste0(
      "MEPS2018_HC209_TOTEXP18_wtd_age",
      recode(as.character(age_group), "0to17" = "0-17", "18to34" = "18-34",
             "35to54" = "35-54", "55plus" = "55-64"),
      "_x_",
      recode(as.character(income), "lt250" = "lt250FPL", "250to400" = "250-400FPL",
             "400plus" = "400plusFPL")
    )
  )

write_csv(out, "data/input/meps_spending_by_demographics.csv")
cat("\nwrote data/input/meps_spending_by_demographics.csv\n")
print(as.data.frame(out))
