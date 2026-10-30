# Data-build orchestrator.
#
# Loads packages, shared helpers, and reference files once, then sources
# each step in order. Each step adds columns or produces a new CSV; later
# steps read those CSVs (or use objects still in the environment).
#
# Output files downstream scripts consume:
#   data/output/demand_households.csv  (CC enrolled + CC uninsured, HH-year)
#   data/output/sipp_logit.rds         (SIPP transition logit from step 3)
#   data/output/broker_density.csv     (region-year broker/agent counts)
#   data/output/rate_filing_rsdata.csv (plan-level risk/rate data for supply)

# Packages ----------------------------------------------------------------
pacman::p_load(
  tidyverse, data.table, fixest, kableExtra, nleqslv, mlogit, readxl
)

options(datatable.showProgress = FALSE)

source("code/data-build/_helpers.R")

# Shared reference files (loaded once, available to all steps) -------------

plan_data <- read.csv("data/input/Covered California/plan_data.csv",
                       stringsAsFactors = FALSE)
plan_data$Issuer_Name <- standardize_insurer(plan_data$Issuer_Name)

product_definitions <- read.csv("data/input/Covered California/product_definitions.csv",
                                 stringsAsFactors = FALSE, row.names = 1)
zip3_choices        <- read.csv("data/input/Covered California/zip3_choices.csv",
                                 stringsAsFactors = FALSE, row.names = 1)
rating_areas        <- read.csv("data/input/Covered California/rating_areas.csv",
                                 stringsAsFactors = FALSE, row.names = 1)
age_rating_factors  <- read.csv("data/input/Covered California/age_rating_factors.csv",
                                 stringsAsFactors = FALSE)
poverty_guidelines  <- read.csv("data/input/Covered California/poverty_guidelines.csv",
                                 stringsAsFactors = FALSE)
# Long form (Family_Size, year, poverty_threshold) for vectorized joins
poverty_guidelines_long <- poverty_guidelines %>%
  pivot_longer(cols = starts_with("YR"),
               names_to = "year", names_prefix = "YR",
               names_transform = list(year = as.integer),
               values_to = "poverty_threshold")

contribution_percentages <- read.csv("data/input/Covered California/contribution_percentages.csv",
                                      stringsAsFactors = FALSE)


# Individual build steps -----------------------------------------------------

source("code/data-build/1_clean-enrollment.R")
source("code/data-build/2_aggregate-to-hh.R")
source("code/data-build/3_process-sipp.R")
source("code/data-build/4_build-cc-uninsured.R")
source("code/data-build/5_merge-and-finalize.R")
source("code/data-build/6_broker-density.R")
source("code/data-build/7_rate-filings.R")
source("code/data-build/8_risk-scores.R")
source("code/data-build/9_mlr-admin.R")
source("code/data-build/10_commissions.R")
