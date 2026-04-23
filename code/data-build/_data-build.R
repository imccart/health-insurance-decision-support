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

source("code/0-setup.R")
source("code/data-build/_helpers.R")

cat("=== Starting data build ===\n\n")

# Shared reference files (loaded once, available to all steps) -------------
cat("Loading reference files...\n")

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

# ----------------------------------------------------------------------------
# Step 1: Clean individual-level CC enrollment data
# ----------------------------------------------------------------------------
cat("\n--- Step 1: Clean individual enrollment ---\n")
source("code/data-build/1_clean-enrollment.R")

# ----------------------------------------------------------------------------
# Step 2: Aggregate to HH-year (one row per household-year)
# ----------------------------------------------------------------------------
cat("\n--- Step 2: Aggregate to HH-year ---\n")
source("code/data-build/2_aggregate-to-hh.R")

# ----------------------------------------------------------------------------
# Step 3: Process SIPP (fit immigration, ESI, market-transition logits)
# ----------------------------------------------------------------------------
cat("\n--- Step 3: Process SIPP ---\n")
source("code/data-build/3_process-sipp.R")

# ----------------------------------------------------------------------------
# Step 4: Build the uninsured pool from CC HHs in their off-enrollment years
#         (RAND 2021 Appendix E approach). Applies SIPP transition logit to
#         drop HH-years where the HH likely lost market eligibility.
# ----------------------------------------------------------------------------
cat("\n--- Step 4: Build CC uninsured pool ---\n")
source("code/data-build/4_build-cc-uninsured.R")

# ----------------------------------------------------------------------------
# Step 5: Merge CC + ACS, impute income for CC >400%, finalize
# ----------------------------------------------------------------------------
cat("\n--- Step 5: Merge and finalize ---\n")
source("code/data-build/5_merge-and-finalize.R")

# ----------------------------------------------------------------------------
# Step 6: Broker density
# ----------------------------------------------------------------------------
cat("\n--- Step 6: Broker density ---\n")
source("code/data-build/6_broker-density.R")

# ----------------------------------------------------------------------------
# Step 7: Rate filings (for supply-side RA regressions)
# ----------------------------------------------------------------------------
cat("\n--- Step 7: Rate filings ---\n")
source("code/data-build/7_rate-filings.R")

cat("\n=== Data build complete ===\n")
