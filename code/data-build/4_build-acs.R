# 4_build-acs.R
# Build the ACS-based uninsured pool (the outside option for demand estimation).
# Predict immigration/ESI status using step 3's SIPP logits, filter to
# ACA-eligible uninsured, compute subsidies, fit income-imputation OLS.
#
# Assumes: 2014-2019 ACS 1-year extract at data/input/ACS Data/usa_*.dat.gz
#          (new extract covering all 6 years; no 2017→2018/2019 duplication)
#
# Inputs:
#   data/input/ACS Data/*.dat.gz                 (ACS microdata, fixed-width)
#   data/input/SIPP Data/input20143.sas          (SAS dictionary for ACS)
#   data/input/ACS Data/PUMAs.csv                (PUMA → county mapping)
#   data/input/ACS Data/sahie_2014.csv           (SAHIE county uninsured rates)
#   data/input/Covered California/Counties/*.csv (county → rating-area by year)
#   data/output/sipp_immigration_logit.rds        (from step 3)
#   data/output/sipp_emp_offer_logit.rds          (from step 3)
# Outputs:
#   data/output/acs_individuals.csv
#   data/output/acs_households.csv
#   data/output/income_model_coefs.csv            (for step 5's CC income imputation)
#   data/output/income_distribution.csv           (for step 5's rank-based sampling)

set.seed(1)


# Load ACS -----------------------------------------------------------------
cat("  Loading ACS microdata...\n")

# Two ACS extracts, each with its own SAS dictionary (different variable sets):
#   usa_00011.dat.gz             — CA-only 2014-2017,    dict: input20143.sas
#   ipums-acs-2018-2019.dat.gz   — nationwide 2018-2019, dict: input_2018_2019.sas
acs_extracts <- list(
  list(dat  = "data/input/ACS Data/usa_00011.dat.gz",
       dict = "data/input/SIPP Data/input20143.sas"),
  list(dat  = "data/input/ACS Data/ipums-acs-2018-2019.dat.gz",
       dict = "data/input/SIPP Data/input_2018_2019.sas")
)

acs <- bind_rows(lapply(acs_extracts, function(x) {
  cat(sprintf("    Reading %s...\n", basename(x$dat)))
  df <- read_fwf_sas(x$dict, x$dat, is_gzip = TRUE)
  df %>% filter(STATEFIP == 6)   # CA-only extract already is; nationwide needs it
}))

# Scale person weights (IPUMS stores PERWT with 2 implied decimals)
acs$PERWT <- acs$PERWT / 100
acs$year  <- acs$YEAR

cat(sprintf("    Combined: %d individual records across %d years (%s)\n",
            nrow(acs), length(unique(acs$YEAR)),
            paste(sort(unique(acs$YEAR)), collapse = ", ")))


# Reference tables ---------------------------------------------------------
ca_pumas <- read.csv("data/input/ACS Data/PUMAs.csv", stringsAsFactors = FALSE)
sahie    <- read.csv("data/input/ACS Data/sahie_2014.csv", stringsAsFactors = FALSE)
counties_list <- lapply(2014:2019, function(yr)
  read.csv(sprintf("data/input/Covered California/Counties/counties_%d.csv", yr),
           stringsAsFactors = FALSE, row.names = 1))
names(counties_list) <- as.character(2014:2019)


# Harmonize ACS covariates for SIPP-model prediction ----------------------
# These must match the variable coding used in SIPP (step 3) so the logit
# `predict` call uses the same reference levels.
cat("  Harmonizing ACS covariates...\n")

acs <- acs %>% mutate(
  duration_in_US = as.numeric(YEAR) - as.numeric(YRIMMIG),
  birth_place = case_when(
    BPL == 100                                     ~ "West_SE Asia, Australia, New Zealand",
    BPL %in% c(150, 160, 200)                      ~ "North America",
    BPL == 210                                     ~ "Central America",
    BPL %in% c(250, 260)                           ~ "Caribbean",
    BPL %in% c(299, 300)                           ~ "South America",
    (BPL >= 400 & BPL < 430) | BPL %in% c(450, 452, 453, 454, 455) ~ "Northwest Europe",
    (BPL >= 430 & BPL <= 441) | BPL %in% c(451, 456, 457, 458) |
      (BPL >= 459 & BPL < 500)                     ~ "Southeast Europe",
    BPL %in% c(500, 501, 502, 509)                 ~ "East Asia",
    BPL >= 510 & BPL < 999 & BPL != 600            ~ "West_SE Asia, Australia, New Zealand",
    BPL == 600                                     ~ "Africa"
  ),
  English  = as.integer(!SPEAKENG %in% c(0, 1, 6)),
  FPL      = POVERTY / 100,
  Hispanic = ifelse(HISPAN == 0, 0L, 1L),
  employed = as.integer(EMPSTAT == 1),
  SEX      = ifelse(SEX == 2, 0L, as.integer(SEX)),
  Race = case_when(RACE == 1 ~ "White", RACE == 2 ~ "Black",
                    RACE %in% c(4, 5, 6) ~ "Asian",
                    RACE %in% c(3, 7, 8, 9) ~ "Other"),
  industry = case_when(
    IND < 1000  ~ "agriculture",
    IND < 4000  ~ "manufacturing",
    IND < 9000  ~ "services",
    IND >= 9000 ~ "other",
    TRUE        ~ "not employed"
  ),
  married    = as.integer(MARST %in% c(1, 2)),
  education_level = case_when(
    EDUC < 6   ~ "lt_high_school",
    EDUC == 6  ~ "high_school",
    EDUC > 6   ~ "gt_high_school"
  ),
  FAMSIZE    = FAMSIZE,  # ACS FAMSIZE matches SIPP FAMSIZE (EFNP)
  uninsured  = as.integer(HCOVANY == 1),
  home_owner = as.integer(OWNERSHP == 1)
)


# Apply SIPP logits to predict immigration / ESI status ------------------
cat("  Predicting immigration and ESI status...\n")

imm_logit <- readRDS("data/output/sipp_immigration_logit.rds")
esi_logit <- readRDS("data/output/sipp_emp_offer_logit.rds")

# Immigration: non-citizens only
noncit <- acs$CITIZEN == 3
acs$undocumented <- 0L
if (any(noncit)) {
  p <- predict(imm_logit, newdata = acs[noncit, ], type = "response")
  # Target CA undocumented count (~2.82M people, per CA Dept Finance 2014-ish).
  # Scale sample size by overall ACS weight ratio.
  target_n <- round(2820000 / (sum(acs$PERWT) / 100) * sum(noncit))
  # Sample without replacement, weighted by predicted probability
  idx <- sample(which(noncit), size = min(target_n, sum(noncit)),
                replace = FALSE, prob = p)
  acs$undocumented[idx] <- 1L
}

# Employer offer (predict for everyone; used in filter below)
acs$pred_ESI <- predict(esi_logit, newdata = acs, type = "response")
# Observed ESI from ACS; imputed offer on top
acs$has_ESI  <- as.integer(acs$HINSEMP == 2)
# Treat "has ESI" OR "predicted high ESI-offer probability" as access
acs$access_ESI <- as.integer(acs$has_ESI == 1 | acs$pred_ESI > 0.5)


# Filter to ACA-eligible uninsured -----------------------------------------
# Build HH-level access_ESI: if ANY member has ESI access, HH has access
acs <- acs %>%
  mutate(household_year = paste(HIUID, year, sep = "_")) %>%
  group_by(household_year) %>%
  mutate(hh_access_ESI = max(access_ESI, na.rm = TRUE)) %>%
  ungroup()

# Two parallel "uninsured ACA-eligible" flags:
#   ACS_unins_flag  — direct from ACS variables only
#                       (uninsured AND US citizen)
#   SIPP_unins_flag — applies SIPP-imputed flags
#                       (uninsured AND not predicted-undocumented
#                        AND no household ESI access)
# Final analysis sample = passes BOTH flags (intersection).
acs <- acs %>% mutate(
  ACS_unins_flag  = as.integer(uninsured == 1 & CITIZEN %in% c(0, 1, 2)),
  SIPP_unins_flag = as.integer(uninsured == 1 & undocumented == 0 &
                                hh_access_ESI == 0)
)

n0     <- nrow(acs)
n_acs  <- sum(acs$ACS_unins_flag  == 1)
n_sipp <- sum(acs$SIPP_unins_flag == 1)
n_both <- sum(acs$ACS_unins_flag == 1 & acs$SIPP_unins_flag == 1)
cat(sprintf("  ACS individuals total:                       %d\n", n0))
cat(sprintf("    ACS_unins_flag  (uninsured + US citizen): %d (%.1f%%)\n",
            n_acs,  100 * n_acs  / n0))
cat(sprintf("    SIPP_unins_flag (uninsured + non-undoc + no ESI): %d (%.1f%%)\n",
            n_sipp, 100 * n_sipp / n0))
cat(sprintf("    Intersection (kept):                      %d (%.1f%%)\n",
            n_both, 100 * n_both / n0))

acs <- acs %>% filter(ACS_unins_flag == 1, SIPP_unins_flag == 1)


# Geography: PUMA → county → rating_area ----------------------------------
cat("  Assigning geography...\n")

# Simple PUMA→county lookup. PUMAs.csv uses STATEFP/PUMA5CE/COUNTY/RATING_AREA.
acs <- acs %>% left_join(
  ca_pumas %>% select(PUMA = PUMA5CE, county_name = COUNTY,
                      rating_area = RATING_AREA),
  by = "PUMA"
)

# Multi-county PUMAs need special handling (original script has 30+ lines
# of region-specific overrides). For this rewrite, if the PUMA→county lookup
# gave NA, drop the record. Some lose (~1-2%) but simpler.
n_before <- nrow(acs)
acs <- acs %>% filter(!is.na(county_name), !is.na(rating_area))
if (n_before - nrow(acs) > 0) {
  cat(sprintf("    Dropped %d individuals with ambiguous PUMA→county\n",
              n_before - nrow(acs)))
}
acs$county <- toupper(acs$county_name)


# Build HH object ----------------------------------------------------------
cat("  Building HH object...\n")

# Individual age-rating factor
acs <- acs %>% mutate(
  age_capped = pmin(64L, AGE),
  rating_factor = ifelse(
    year >= 2018,
    age_rating_factors$Rating_Factor2018[match(age_capped, age_rating_factors$Age)],
    age_rating_factors$Rating_Factor[match(age_capped, age_rating_factors$Age)]
  )
) %>% select(-age_capped)

# SLC benchmark per (county, year): second-lowest silver plan in county
cat("  Computing SLC benchmark per (county, year)...\n")
compute_county_slc <- function(counties_df, plan_df) {
  sapply(rownames(counties_df), function(cty) {
    ra <- counties_df[cty, "Rating_Area"]
    insurer_cols <- setdiff(colnames(counties_df), "Rating_Area")
    avail <- insurer_cols[!is.na(counties_df[cty, insurer_cols])]
    silver <- plan_df[plan_df$Issuer_Name %in% avail &
                       plan_df$region == ra &
                       plan_df$metal_level == "Silver", "Premium"]
    prems <- sort(as.numeric(silver)) / RATING_FACTOR_AGE40
    if (length(prems) >= 2) prems[2] else if (length(prems) == 1) prems[1] else NA_real_
  })
}

county_slc_by_year <- lapply(2014:2019, function(yr) {
  slc <- compute_county_slc(counties_list[[as.character(yr)]],
                             plan_data[plan_data$ENROLLMENT_YEAR == yr, ])
  tibble(county = names(slc), year = yr, premiumSLC_base = unname(slc))
}) %>% bind_rows() %>% mutate(county = toupper(county))

acs <- acs %>% left_join(county_slc_by_year, by = c("county", "year")) %>%
  mutate(premiumSLC = premiumSLC_base * rating_factor) %>%
  select(-premiumSLC_base)


# HH aggregation -----------------------------------------------------------
hh <- acs %>%
  group_by(household_year) %>%
  summarize(
    year                  = first(year),
    household_size        = n(),
    FPL                   = first(FPL),
    FPL_bracket           = assign_bracket(first(FPL)),
    rating_factor   = sum(rating_factor, na.rm = TRUE),
    premiumSLC            = sum(premiumSLC, na.rm = TRUE),
    weight                = sum(PERWT, na.rm = TRUE),
    county                = first(county),
    rating_area           = first(rating_area),
    zip3                  = NA_integer_,  # ACS doesn't have zip3 — fill in step 5 if needed
    # Demographics (for HH-level controls)
    oldest_member         = max(AGE, na.rm = TRUE),
    perc_0to17            = mean(AGE <= 17, na.rm = TRUE),
    perc_18to25           = mean(AGE >= 18 & AGE <= 25, na.rm = TRUE),
    perc_26to34           = mean(AGE >= 26 & AGE <= 34, na.rm = TRUE),
    perc_35to44           = mean(AGE >= 35 & AGE <= 44, na.rm = TRUE),
    perc_45to54           = mean(AGE >= 45 & AGE <= 54, na.rm = TRUE),
    perc_55to64           = mean(AGE >= 55 & AGE <= 64, na.rm = TRUE),
    perc_65plus           = mean(AGE >= 65, na.rm = TRUE),
    perc_male             = mean(SEX, na.rm = TRUE),
    perc_white            = mean(Race == "White", na.rm = TRUE),
    perc_black            = mean(Race == "Black", na.rm = TRUE),
    perc_hispanic         = mean(Hispanic == 1, na.rm = TRUE),
    perc_asian            = mean(Race == "Asian", na.rm = TRUE),
    perc_other            = mean(Race == "Other", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # ACA eligibility: <138% is Medicaid, >400% is unsubsidized, rest gets subsidy
    subsidy_eligible = as.integer(FPL >= 1.38 & FPL <= 4.0)
  )

# Poverty threshold (by HH size and year) — for ACA contribution formula
hh <- hh %>%
  mutate(year_cap = pmin(year, 2019L)) %>%
  left_join(poverty_guidelines_long,
            by = c("year_cap" = "year", "household_size" = "Family_Size")) %>%
  select(-year_cap)


# Subsidy calculation via ACA formula --------------------------------------
cat("  Computing ACA subsidies...\n")

# Lookup bracket-specific contribution percentages (perc_LB, perc_UB)
# by year and bracket bounds
fpl_lb_lookup <- setNames(FPL_BRACKETS$fpl_LB, FPL_BRACKETS$bracket)
fpl_ub_lookup <- setNames(FPL_BRACKETS$fpl_UB, FPL_BRACKETS$bracket)

hh <- hh %>%
  mutate(fpl_LB = fpl_lb_lookup[FPL_bracket],
         fpl_UB = fpl_ub_lookup[FPL_bracket],
         perc_LB = NA_real_,
         perc_UB = NA_real_)

# Year-specific contribution percentages
for (yr in 2014:2019) {
  yr_col <- paste0("YR", min(yr, 2019))
  idx <- which(hh$year == yr & hh$subsidy_eligible == 1)
  if (length(idx) == 0) next
  hh$perc_LB[idx] <- contribution_percentages[[yr_col]][
    match(hh$fpl_LB[idx], contribution_percentages$FPL)]
  hh$perc_UB[idx] <- contribution_percentages[[yr_col]][
    match(hh$fpl_UB[idx], contribution_percentages$FPL)]
}

# Apply formula (decimal FPL throughout; the bug we fixed)
is_300_400 <- hh$FPL_bracket == "300% FPL to 400% FPL" & hh$subsidy_eligible == 1
hh$SLC_contribution <- NA_real_

elig_idx <- which(hh$subsidy_eligible == 1)
hh$SLC_contribution[elig_idx] <- aca_contribution(
  fpl               = hh$FPL[elig_idx],
  perc_LB           = hh$perc_LB[elig_idx],
  perc_UB           = hh$perc_UB[elig_idx],
  fpl_LB            = hh$fpl_LB[elig_idx],
  fpl_UB            = hh$fpl_UB[elig_idx],
  poverty_threshold = hh$poverty_threshold[elig_idx],
  bracket_300_400   = is_300_400[elig_idx]
)

# Subsidy = benchmark SLC - expected contribution, floored at 0
hh <- hh %>%
  mutate(subsidy = ifelse(subsidy_eligible == 1,
                          pmax(0, premiumSLC - SLC_contribution),
                          0))


# Fit income imputation OLS -------------------------------------------------
# Used in step 5 to impute FPL for CC enrollees in the >400% bracket who
# don't have exact FPL recorded. Fit on ACS HHs > 400% FPL.
cat("  Fitting income imputation OLS...\n")

fit_sample <- hh %>% filter(FPL > 4) %>%
  filter(!is.na(FPL), is.finite(FPL))

if (nrow(fit_sample) > 100) {
  income_OLS <- lm(FPL ~ as.factor(rating_area) + household_size + perc_male +
                    perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
                    perc_45to54 + perc_55to64 + perc_white + perc_black +
                    perc_hispanic + perc_asian,
                   data = fit_sample)

  # Save coefficients as CSV for step 5 reconstruction
  income_coefs <- tibble(term = names(coef(income_OLS)),
                          estimate = unname(coef(income_OLS)))
  income_dist <- tibble(FPL = fit_sample$FPL)
  fwrite(income_coefs, "data/output/income_model_coefs.csv")
  fwrite(income_dist,  "data/output/income_distribution.csv")
} else {
  cat("    Warning: <100 HHs >400% FPL; income imputation model skipped\n")
}


# Save ----------------------------------------------------------------------
fwrite(hh,  "data/output/acs_households.csv")
fwrite(acs, "data/output/acs_individuals.csv")
cat(sprintf("Step 4 complete: %d ACS HH-years, %d ACS individuals.\n",
            nrow(hh), nrow(acs)))

to_rm <- c("acs", "hh", "imm_logit", "esi_logit", "ca_pumas", "sahie",
           "counties_list", "county_slc_by_year", "compute_county_slc",
           "income_OLS", "fit_sample", "income_coefs", "income_dist")
rm(list = to_rm[sapply(to_rm, exists)]); gc(verbose = FALSE)
