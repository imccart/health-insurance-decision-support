# 7_build-acs-market.R
# Load ACS microdata, merge SIPP imputations, construct household objects,
# assign geography/subsidies, filter to market population (uninsured, no
# Medicaid, no employer offer), compute ACA subsidies and penalty exemptions.
# Also fits income imputation model for later use on exchange data.
#
# Input:  data/input/ACS Data/input20143.sas, usa_00011.dat.gz
#         data/output/acs_immigration.csv, acs_emp_offer.csv (from step 6)
#         data/input/ACS Data/PUMAs.csv, sahie_2014.csv
#         data/input/Covered California/ (plan_data, rating_areas, counties, etc.)
# Output: data/output/acs_individuals.csv, data/output/acs_households.csv,
#         data/output/income_model.csv

set.seed(1)


# Load ACS microdata --------------------------------------------------------

cat("  Loading ACS microdata...\n")
acs <- read_fwf_sas("data/input/SIPP Data/input20143.sas",
                     "data/input/ACS Data/usa_00011.dat.gz", is_gzip = TRUE)

# Load reference data
acs_immigration <- fread("data/output/acs_immigration.csv") %>% as_tibble()
acs_emp_offer   <- fread("data/output/acs_emp_offer.csv") %>% as_tibble()
ca_pumas <- read.csv("data/input/ACS Data/PUMAs.csv", stringsAsFactors = FALSE)
sahie <- read.csv("data/input/ACS Data/sahie_2014.csv", stringsAsFactors = FALSE)
contribution_percentages <- read.csv("data/input/Covered California/contribution_percentages.csv",
                                      stringsAsFactors = FALSE)
poverty_guidelines <- read.csv("data/input/Covered California/poverty_guidelines.csv",
                                stringsAsFactors = FALSE)
plan_data <- read.csv("data/input/Covered California/plan_data.csv",
                       stringsAsFactors = FALSE)
rating_areas <- read.csv("data/input/Covered California/rating_areas.csv",
                          stringsAsFactors = FALSE, row.names = 1)
age_rating_factors <- read.csv("data/input/Covered California/age_rating_factors.csv",
                                stringsAsFactors = FALSE)

# Load county files for all years
counties_list <- list()
for (yr in 2014:2019) {
  counties_list[[as.character(yr)]] <- read.csv(
    sprintf("data/input/Covered California/Counties/counties_%d.csv", yr),
    stringsAsFactors = FALSE, row.names = 1
  )
}

# Standardize plan_data insurer names
plan_data$Issuer_Name <- standardize_insurer(plan_data$Issuer_Name)


# Merge immigration imputations ---------------------------------------------

acs <- acs %>%
  mutate(
    immigration_id = paste(HIUID, PERNUM, AGE, YEAR, sep = "_"),
    year = YEAR
  )

acs <- acs %>%
  left_join(acs_immigration %>% select(immigration_id, undocumented_immigrant,
                                        permanent_resident, temporary_resident),
            by = "immigration_id") %>%
  mutate(
    undocumented_immigrant = coalesce(undocumented_immigrant, 0L),
    permanent_resident = coalesce(permanent_resident, 0L),
    temporary_resident = coalesce(temporary_resident, 0L)
  )

# Merge employer offer
acs <- acs %>%
  left_join(acs_emp_offer %>% select(immigration_id, employer_offer, access_to_emp_offer),
            by = "immigration_id") %>%
  mutate(
    imputed_offer = coalesce(as.integer(employer_offer == 1), 0L),
    access_to_emp_offer = coalesce(access_to_emp_offer, 0L)
  )

rm(acs_immigration, acs_emp_offer)


# Household construction ----------------------------------------------------

acs <- acs %>%
  mutate(
    household_id = paste(HIUID, FAMUNIT, sep = "_"),
    household_year = paste(household_id, year, sep = "_")
  )

# Recalculate access_to_emp_offer at household level (imputed + ESI)
acs$ESI <- as.integer(acs$HINSEMP == 2)

hh_offers <- acs %>%
  group_by(household_year) %>%
  summarize(
    total_imputed = sum(imputed_offer, na.rm = TRUE),
    total_ESI = sum(ESI, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(access_to_emp_offer = as.integer(total_imputed + total_ESI > 0))

acs <- acs %>%
  select(-access_to_emp_offer) %>%
  left_join(hh_offers %>% select(household_year, access_to_emp_offer),
            by = "household_year")


# Extend to 2018-2019 -------------------------------------------------------

cat("  Extending ACS to 2018-2019...\n")
for (t in c(2018, 2019)) {
  acs_add <- acs %>%
    filter(year == 2017) %>%
    mutate(
      year = t,
      YEAR = t,
      household_year = paste(household_id, year, sep = "_"),
      immigration_id = paste(HIUID, PERNUM, AGE, YEAR, sep = "_")
    )
  acs <- bind_rows(acs, acs_add)
}
rm(acs_add)


# Population weights ---------------------------------------------------------

acs$PERWT <- acs$PERWT / 100


# Build household object -----------------------------------------------------

cat("  Building household objects...\n")
households <- acs %>%
  group_by(household_year) %>%
  summarize(
    household_id = first(household_id),
    year = first(year),
    household_size = n(),
    weight = sum(PERWT, na.rm = TRUE),
    .groups = "drop"
  )

acs$household_size <- households$household_size[match(acs$household_year, households$household_year)]

# Uninsured members
acs$is_uninsured <- as.integer(
  acs$HCOVANY == 1 & acs$HCOVPRIV == 1 & acs$HINSEMP == 1 &
    acs$HINSPUR == 1 & acs$HINSTRI == 1 & acs$HCOVPUB == 1 &
    acs$HINSCARE == 1 & acs$HINSVA == 1 & acs$HINSIHS == 1
)

uninsured_counts <- acs %>%
  filter(is_uninsured == 1) %>%
  group_by(household_year) %>%
  summarize(enrollees = n(), uninsured_weight = sum(PERWT, na.rm = TRUE),
            .groups = "drop")

households <- households %>%
  left_join(uninsured_counts, by = "household_year") %>%
  mutate(enrollees = coalesce(enrollees, 0L),
         uninsured_weight = coalesce(uninsured_weight, 0))


# Income / FPL ---------------------------------------------------------------

# Poverty threshold by household size and year
pov_long <- poverty_guidelines %>%
  pivot_longer(starts_with("YR"), names_to = "yr_col", values_to = "poverty_threshold") %>%
  mutate(year = as.integer(sub("YR", "", yr_col))) %>%
  select(Family_Size, year, poverty_threshold)


acs <- acs %>%
  left_join(pov_long, by = c("household_size" = "Family_Size", "year")) %>%
  mutate(
    POVERTY = ifelse(POVERTY > 500,
                     pmax(501, FTOTINC / poverty_threshold * 100),
                     POVERTY),
    FPL = POVERTY / 100
  )

hh_fpl <- acs %>%
  group_by(household_year) %>%
  summarize(FPL = max(FPL, na.rm = TRUE), .groups = "drop")
households <- households %>%
  left_join(hh_fpl, by = "household_year")
rm(hh_fpl)

# Subsidy linear piece
income_pieces <- c("138% FPL or less", "138% FPL to 150% FPL", "150% FPL to 200% FPL",
                   "200% FPL to 250% FPL", "250% FPL to 300% FPL", "300% FPL to 400% FPL", NA)
income_breakpoints <- c(0, 1.3799999, 1.5, 2, 2.5, 3, 4.0000001, 10000)
households$subsidy_linear_piece <- income_pieces[findInterval(households$FPL, income_breakpoints)]


# Geography ------------------------------------------------------------------

cat("  Assigning geography...\n")
rownames(ca_pumas) <- ca_pumas$PUMA5CE

hh_puma <- acs %>%
  group_by(household_year) %>%
  summarize(PUMA = first(PUMA), .groups = "drop")

households <- households %>%
  left_join(hh_puma, by = "household_year") %>%
  mutate(
    rating_area = ca_pumas[as.character(PUMA), "RATING_AREA"],
    county = ca_pumas[as.character(PUMA), "COUNTY"]
  )

# Fix multi-county PUMAs
# PUMAs 5301 and 5302 -> Monterey
households <- households %>%
  mutate(county = ifelse(PUMA %in% c(5301, 5302), "Monterey", county))

# SAHIE for probabilistic assignment
sahie_ca <- sahie %>%
  filter(statefips == 6, geocat == 50, agecat == 0, racecat == 0,
         sexcat == 0, iprcat == 0)
rownames(sahie_ca) <- sahie_ca$countyfips

# Multi-county PUMA assignments
multi_county_pumas <- list(
  `300`   = c(`3` = "Alpine", `5` = "Amador", `9` = "Calaveras",
              `27` = "Inyo", `43` = "Mariposa", `51` = "Mono", `109` = "Tuolumne"),
  `1100`  = c(`11` = "Colusa", `21` = "Glenn", `103` = "Tehama", `105` = "Trinity"),
  `1500`  = c(`15` = "Del Norte", `35` = "Lassen", `49` = "Modoc",
              `63` = "Plumas", `93` = "Siskiyou"),
  `3300`  = c(`33` = "Lake", `45` = "Mendocino"),
  `5700`  = c(`57` = "Nevada", `91` = "Sierra"),
  `10100` = c(`101` = "Sutter", `115` = "Yuba")
)

# Rating area overrides for PUMA 300
puma300_rating_areas <- c(
  "Alpine" = 1L, "Amador" = 1L, "Calaveras" = 1L,
  "Inyo" = 13L, "Mariposa" = 1L, "Mono" = 13L, "Tuolumne" = 1L
)

for (puma_code in names(multi_county_pumas)) {
  puma_counties <- multi_county_pumas[[puma_code]]
  fips_codes <- names(puma_counties)

  hh_idx <- which(households$PUMA == as.integer(puma_code))
  if (length(hh_idx) == 0) next

  # Get SAHIE uninsured counts for weighting
  sahie_weights <- sapply(fips_codes, function(f) {
    row <- sahie_ca[f, ]
    if (nrow(row) > 0) as.numeric(row$NUI[1]) else 1
  })
  sahie_weights <- sahie_weights / sum(sahie_weights)

  # Cumulative distribution for assignment
  cum_weights <- cumsum(sahie_weights)
  cum_hh_weights <- cumsum(households$uninsured_weight[hh_idx]) /
    sum(households$uninsured_weight[hh_idx])

  # Shuffle for randomness
  shuffle_order <- sample(length(hh_idx))
  hh_idx_shuffled <- hh_idx[shuffle_order]
  cum_hh_weights_shuffled <- cumsum(households$uninsured_weight[hh_idx_shuffled]) /
    sum(households$uninsured_weight[hh_idx_shuffled])

  for (j in seq_along(fips_codes)) {
    cty_name <- puma_counties[j]
    if (j == length(fips_codes)) {
      assign_idx <- hh_idx_shuffled
    } else {
      cutoff <- cum_weights[j]
      assign_mask <- cum_hh_weights_shuffled <= cutoff
      assign_idx <- hh_idx_shuffled[assign_mask]
      hh_idx_shuffled <- hh_idx_shuffled[!assign_mask]
      cum_hh_weights_shuffled <- cumsum(households$uninsured_weight[hh_idx_shuffled]) /
        sum(households$uninsured_weight[hh_idx_shuffled])
    }
    households$county[assign_idx] <- cty_name

    # Rating area override for PUMA 300
    if (puma_code == "300" && cty_name %in% names(puma300_rating_areas)) {
      households$rating_area[assign_idx] <- puma300_rating_areas[cty_name]
    }
  }
}

# PUMA 5303 -> San Benito
households$county[households$PUMA == 5303] <- "San Benito"

# Uppercase counties
households$county <- toupper(households$county)

# Convert rating areas to the adjusted numeric system
households <- households %>%
  mutate(
    rating_area_raw = as.numeric(rating_area),
    rating_area = case_when(
      rating_area_raw == 1 ~ 1L,
      rating_area_raw >= 3 & rating_area_raw <= 12 ~ as.integer(rating_area_raw + 7),
      rating_area_raw >= 13 ~ as.integer(rating_area_raw - 11),
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-rating_area_raw)

# LA split
households$county[households$rating_area == 15] <- "LOS ANGELES1"
households$county[households$rating_area == 16] <- "LOS ANGELES2"

# Propagate to ACS
acs <- acs %>%
  left_join(households %>% select(household_year, county, rating_area),
            by = "household_year")


# Demographic variables -----------------------------------------------------

acs <- acs %>%
  mutate(
    age = AGE,
    gender = ifelse(SEX == 2, 0L, as.integer(SEX)),
    race = case_when(
      HISPAN %in% c(1, 2, 3, 4) ~ "Hispanic",
      RACE == 1 ~ "White",
      RACE == 2 ~ "Black/African American",
      RACE %in% c(4, 5, 6) ~ "Asian",
      RACE %in% c(3, 7, 8, 9) ~ "Other Race",
      TRUE ~ NA_character_
    ),
    married = as.integer(MARST %in% c(1, 2)),
    employed = as.integer(EMPSTAT == 1)
  )

# Initial demographics (full household)
households <- compute_demographic_pcts(acs, households, size_col = household_size)


# Income imputation model ---------------------------------------------------
# Fit on ACS households >400% FPL, save coefficients for script 8

cat("  Fitting income imputation model...\n")
income_model_data <- households %>% filter(FPL > 4)

income_OLS <- lm(
  FPL ~ as.factor(rating_area) + household_size + perc_male +
    perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
    perc_45to54 + perc_55to64 + perc_white + perc_black +
    perc_hispanic + perc_asian,
  data = income_model_data
)

# Save coefficients as CSV
income_coefs <- data.frame(
  term = names(coef(income_OLS)),
  estimate = unname(coef(income_OLS))
)
fwrite(income_coefs, "data/output/income_model_coefs.csv")

# Save income distribution for rank-based sampling
income_dist <- data.frame(FPL = income_model_data$FPL)
fwrite(income_dist, "data/output/income_distribution.csv")

# Keep OLS object in memory for script 8 (if sourced sequentially)
# save as RDS is not preferred, so we reconstruct from coefficients in script 8


# Filter to market population -----------------------------------------------

cat("  Filtering to market population...\n")

# Remove undocumented immigrants
acs <- acs %>% filter(undocumented_immigrant == 0)

# Remove those with employer offer access
acs <- acs %>% filter(access_to_emp_offer == 0)

# Keep only households with uninsured members
remaining_uninsured <- acs %>%
  filter(is_uninsured == 1) %>%
  count(household_year, name = "enrollees")

households <- households %>%
  select(-enrollees) %>%
  left_join(remaining_uninsured, by = "household_year") %>%
  mutate(enrollees = coalesce(enrollees, 0L)) %>%
  filter(enrollees > 0)

acs <- acs %>% filter(household_year %in% households$household_year)
households <- households %>% filter(household_year %in% unique(acs$household_year))


# Subsidy eligibility -------------------------------------------------------

acs$subsidy_eligible <- 1L

# Medicaid eligibles: FPL < 138% AND not temporary resident
medicaid_idx <- which(acs$FPL < 1.38 &
                        (acs$temporary_resident == 0 | is.na(acs$temporary_resident)))
acs$subsidy_eligible[medicaid_idx] <- 0L

# CHIP: <=266% FPL for children, <=322% in high CHIP counties
high_chip_counties <- c("ALAMEDA", "SAN FRANCISCO", "SAN MATEO", "SANTA CLARA")
chip_idx <- which(acs$FPL <= 2.66 & acs$AGE <= 18 &
                    (acs$temporary_resident == 0 | is.na(acs$temporary_resident)))
acs$subsidy_eligible[chip_idx] <- 0L

chip_high_idx <- which(acs$FPL <= 3.22 & acs$AGE <= 18 &
                          (acs$temporary_resident == 0 | is.na(acs$temporary_resident)) &
                          acs$county %in% high_chip_counties)
acs$subsidy_eligible[chip_high_idx] <- 0L

# Combine all Medicaid/CHIP eligible
medicaid_all_idx <- unique(c(medicaid_idx, chip_idx, chip_high_idx))

# Income too high
acs$subsidy_eligible[acs$FPL > 4] <- 0L

# Subsidized/unsubsidized counts
acs$subsidized <- as.integer(acs$subsidy_eligible == 1)

sub_counts <- acs %>%
  group_by(household_year) %>%
  summarize(
    subsidized_members = sum(subsidized, na.rm = TRUE),
    total = n(),
    .groups = "drop"
  ) %>%
  mutate(unsubsidized_members = total - subsidized_members) %>%
  select(-total)

households <- households %>%
  select(-any_of(c("subsidized_members", "unsubsidized_members"))) %>%
  left_join(sub_counts, by = "household_year") %>%
  mutate(
    subsidy_linear_piece = ifelse(subsidized_members == 0, NA_character_, subsidy_linear_piece)
  )


# ACA subsidy calculation ---------------------------------------------------

cat("  Computing ACA subsidies...\n")

# Bounds for linear interpolation
fpl_lb_lookup <- c("138% FPL or less" = 0, "138% FPL to 150% FPL" = 1.38,
                   "150% FPL to 200% FPL" = 1.5, "200% FPL to 250% FPL" = 2,
                   "250% FPL to 300% FPL" = 2.5, "300% FPL to 400% FPL" = 3)
fpl_ub_lookup <- c("138% FPL or less" = 1.38, "138% FPL to 150% FPL" = 1.5,
                   "150% FPL to 200% FPL" = 2, "200% FPL to 250% FPL" = 2.5,
                   "250% FPL to 300% FPL" = 3, "300% FPL to 400% FPL" = 4)

subsidized_hh <- households %>% filter(!is.na(subsidy_linear_piece))

if (nrow(subsidized_hh) > 0) {
  subsidized_hh$fpl_LB <- fpl_lb_lookup[subsidized_hh$subsidy_linear_piece]
  subsidized_hh$fpl_UB <- fpl_ub_lookup[subsidized_hh$subsidy_linear_piece]

  # Contribution percentages by year
  subsidized_hh$perc_LB <- NA_real_
  subsidized_hh$perc_UB <- NA_real_
  for (yr in 2014:2019) {
    yr_col <- paste0("YR", min(yr, 2019))
    idx <- which(subsidized_hh$year == yr)
    if (length(idx) == 0) next
    subsidized_hh$perc_LB[idx] <- contribution_percentages[[yr_col]][
      match(subsidized_hh$fpl_LB[idx], contribution_percentages$FPL)]
    subsidized_hh$perc_UB[idx] <- contribution_percentages[[yr_col]][
      match(subsidized_hh$fpl_UB[idx], contribution_percentages$FPL)]
  }

  # Poverty threshold
  subsidized_hh <- subsidized_hh %>%
    left_join(pov_long, by = c("household_size" = "Family_Size", "year"))

  # SLC contribution
  is_300_400 <- subsidized_hh$subsidy_linear_piece == "300% FPL to 400% FPL"
  subsidized_hh$SLC_contribution <- NA_real_

  subsidized_hh$SLC_contribution[!is_300_400] <- calculate_aca_contribution(
    subsidized_hh$FPL[!is_300_400], subsidized_hh$perc_LB[!is_300_400],
    subsidized_hh$perc_UB[!is_300_400], subsidized_hh$fpl_LB[!is_300_400],
    subsidized_hh$fpl_UB[!is_300_400], subsidized_hh$poverty_threshold[!is_300_400],
    bracket_300_400 = FALSE
  )
  subsidized_hh$SLC_contribution[is_300_400] <- calculate_aca_contribution(
    subsidized_hh$FPL[is_300_400], subsidized_hh$perc_LB[is_300_400],
    subsidized_hh$perc_UB[is_300_400], subsidized_hh$fpl_LB[is_300_400],
    subsidized_hh$fpl_UB[is_300_400], subsidized_hh$poverty_threshold[is_300_400],
    bracket_300_400 = TRUE
  )

  # Merge back
  households <- households %>%
    left_join(subsidized_hh %>% select(household_year, fpl_LB, fpl_UB, perc_LB, perc_UB,
                                        poverty_threshold, SLC_contribution),
              by = "household_year")
}

# SLC premium (second-lowest silver per county)
# Compute per county-year
compute_county_slc <- function(counties_df, plan_df) {
  county_names <- rownames(counties_df)
  sapply(county_names, function(x) {
    ra <- counties_df[x, "Rating_Area"]
    insurer_cols <- setdiff(colnames(counties_df), "Rating_Area")
    avail <- insurer_cols[!is.na(counties_df[x, insurer_cols])]
    silver <- plan_df %>%
      filter(Issuer_Name %in% avail, region == ra, metal_level == "Silver")
    prems <- sort(silver$Premium) / RATING_FACTOR_AGE40
    if (length(prems) == 1) prems[1] else if (length(prems) >= 2) prems[2] else NA_real_
  })
}

for (yr in 2014:2019) {
  counties_list[[as.character(yr)]]$premiumSLC <- compute_county_slc(
    counties_list[[as.character(yr)]],
    plan_data %>% filter(ENROLLMENT_YEAR == yr)
  )
}

# Individual rating factor
acs <- acs %>%
  mutate(
    age_capped = pmin(64L, AGE),
    rating_factor = ifelse(
      year >= 2018,
      age_rating_factors$Rating_Factor2018[match(age_capped, age_rating_factors$Age)],
      age_rating_factors$Rating_Factor[match(age_capped, age_rating_factors$Age)]
    )
  ) %>%
  select(-age_capped)

# Individual SLC premium = county SLC × individual rating factor
acs$premiumSLC <- NA_real_
for (yr in 2014:2019) {
  yr_idx <- which(acs$year == yr)
  if (length(yr_idx) == 0) next
  county_slc <- counties_list[[as.character(yr)]]
  acs$premiumSLC[yr_idx] <- county_slc[acs$county[yr_idx], "premiumSLC"] *
    acs$rating_factor[yr_idx]
}

# Household SLC premium
hh_slc <- acs %>%
  group_by(household_year) %>%
  summarize(premiumSLC_total = sum(premiumSLC, na.rm = TRUE),
            rating_factor_total = sum(rating_factor, na.rm = TRUE),
            .groups = "drop")

households <- households %>%
  left_join(hh_slc, by = "household_year") %>%
  mutate(premiumSLC = premiumSLC_total, rating_factor = rating_factor_total) %>%
  select(-premiumSLC_total, -rating_factor_total)

# Mixed-subsidy households: subtract unsubsidized SLC portion
mixed_hh <- households %>%
  filter(unsubsidized_members > 0 & subsidized_members > 0)

if (nrow(mixed_hh) > 0) {
  unsub_slc <- acs %>%
    filter(household_year %in% mixed_hh$household_year, subsidized == 0) %>%
    group_by(household_year) %>%
    summarize(premiumSLC_unsubsidized = sum(premiumSLC, na.rm = TRUE), .groups = "drop")
  households <- households %>%
    left_join(unsub_slc, by = "household_year") %>%
    mutate(premiumSLC_unsubsidized = coalesce(premiumSLC_unsubsidized, 0))
} else {
  households$premiumSLC_unsubsidized <- 0
}

# Household subsidy
households <- households %>%
  mutate(
    subsidy = pmax(0, (premiumSLC - premiumSLC_unsubsidized) -
                     coalesce(SLC_contribution, 0)),
    subsidy = ifelse(is.na(SLC_contribution), 0, subsidy)
  )

acs$subsidy <- households$subsidy[match(acs$household_year, households$household_year)]
acs$SLC_contribution <- households$SLC_contribution[match(acs$household_year, households$household_year)]


# Penalty exemptions --------------------------------------------------------

cat("  Determining penalty exemptions...\n")
acs$penalty_exempt <- 0L

# American Indians
acs$penalty_exempt[acs$RACAMIND == 2] <- 1L

# Below filing threshold
filing_threshold <- tribble(
  ~year, ~single, ~household_head, ~married, ~widow_wchild,
  2014L, 10150,   13050,           20300,    16350,
  2015L, 10300,   13250,           20600,    16600,
  2016L, 10350,   13350,           20700,    16650,
  2017L, 10400,   16400,           20800,    16750,
  2018L, 12000,   24000,           18000,    24000,
  2019L, 12200,   24400,           18350,    24400
)

# Tax unit type from HIURULE and MARST
hh_tax <- acs %>%
  group_by(household_year) %>%
  summarize(
    is_single = any(HIURULE == 50),
    is_widow = any(MARST == 5),
    is_single_parent = !any(HIURULE %in% c(30, 31, 32, 60, 70)),
    is_married_hh = any(HIURULE %in% c(20, 21)),
    is_married_member = any(MARST %in% c(1, 2, 3)),
    .groups = "drop"
  ) %>%
  mutate(
    widow_wchild = is_widow & is_single_parent,
    tax_unit_type = case_when(
      is_single ~ "single",
      widow_wchild ~ "widow_wchild",
      is_married_hh | is_married_member ~ "married",
      TRUE ~ "household_head"
    )
  )

households <- households %>%
  left_join(hh_tax %>% select(household_year, tax_unit_type), by = "household_year")

# Assign filing threshold
ft_long <- filing_threshold %>%
  pivot_longer(-year, names_to = "tax_unit_type", values_to = "filing_thresh")

households <- households %>%
  left_join(ft_long, by = c("year", "tax_unit_type")) %>%
  rename(filing_threshold = filing_thresh)

# Flag below filing threshold
below_filing <- households %>%
  filter(FPL * poverty_threshold < filing_threshold) %>%
  pull(household_year)
acs$penalty_exempt[acs$household_year %in% below_filing] <- 1L

# Cheapest bronze for affordability test
compute_county_cheapest <- function(counties_df, plan_df) {
  county_names <- rownames(counties_df)
  sapply(county_names, function(x) {
    ra <- counties_df[x, "Rating_Area"]
    insurer_cols <- setdiff(colnames(counties_df), "Rating_Area")
    avail <- insurer_cols[!is.na(counties_df[x, insurer_cols])]
    bronze <- plan_df %>%
      filter(Issuer_Name %in% avail, region == ra, metal_level == "Bronze")
    prems <- sort(bronze$Premium) / RATING_FACTOR_AGE40
    if (length(prems) > 0) prems[1] else NA_real_
  })
}

for (yr in 2014:2019) {
  counties_list[[as.character(yr)]]$cheapest_premium <- compute_county_cheapest(
    counties_list[[as.character(yr)]],
    plan_data %>% filter(ENROLLMENT_YEAR == yr)
  )
}

acs$cheapest_premium <- NA_real_
for (yr in 2014:2019) {
  yr_idx <- which(acs$year == yr)
  if (length(yr_idx) == 0) next
  acs$cheapest_premium[yr_idx] <- counties_list[[as.character(yr)]][acs$county[yr_idx], "cheapest_premium"] *
    acs$rating_factor[yr_idx]
}
acs$cheapest_premium[medicaid_all_idx[medicaid_all_idx %in% seq_len(nrow(acs))]] <- 0

hh_cheapest <- acs %>%
  group_by(household_year) %>%
  summarize(cheapest_premium = sum(cheapest_premium, na.rm = TRUE), .groups = "drop")
households <- households %>%
  left_join(hh_cheapest, by = "household_year")

# Affordability thresholds by year
afford_thresholds <- c(`2014` = 0.08, `2015` = 0.0805, `2016` = 0.0813,
                       `2017` = 0.0816, `2018` = 0.0805, `2019` = 0.083)

unaffordable_hh <- households %>%
  mutate(
    afford_thresh = afford_thresholds[as.character(year)],
    unaffordable = case_when(
      subsidized_members > 0 ~
        (cheapest_premium - subsidy) * 12 > afford_thresh * FPL * poverty_threshold,
      TRUE ~
        cheapest_premium * 12 > afford_thresh * FPL * poverty_threshold
    )
  ) %>%
  filter(unaffordable) %>%
  pull(household_year)

# Exempt non-Medicaid individuals in unaffordable households
non_medicaid_mask <- !seq_len(nrow(acs)) %in% medicaid_all_idx
acs$penalty_exempt[non_medicaid_mask & acs$household_year %in% unaffordable_hh] <- 1L

# Specific exemption flags
acs$penalty_exempt_belowfiling <- as.integer(acs$household_year %in% below_filing)
acs$penalty_exempt_unaffordable <- as.integer(
  !acs$household_year %in% below_filing &
    !seq_len(nrow(acs)) %in% medicaid_all_idx &
    acs$household_year %in% unaffordable_hh
)


# Final market population filter --------------------------------------------

cat("  Final market population filter...\n")
acs <- acs %>% filter(is_uninsured == 1)
acs <- acs %>% filter(!seq_len(nrow(acs)) %in% which(acs$household_year %in%
  acs$household_year[medicaid_all_idx[medicaid_all_idx %in% seq_len(nrow(acs))]]))

# Actually: simpler approach matching old code
# Keep uninsured, remove Medicaid eligible individuals
acs_medicaid_ids <- acs %>% filter(FPL < 1.38 & (temporary_resident == 0 | is.na(temporary_resident)))
acs_chip_ids <- acs %>% filter(FPL <= 2.66 & AGE <= 18 & (temporary_resident == 0 | is.na(temporary_resident)))
acs_chip_high_ids <- acs %>% filter(FPL <= 3.22 & AGE <= 18 &
                                      (temporary_resident == 0 | is.na(temporary_resident)) &
                                      county %in% high_chip_counties)
medicaid_rows <- unique(c(which(acs$household_year %in% acs_medicaid_ids$household_year &
                                  acs$FPL < 1.38),
                          which(acs$AGE <= 18 & acs$FPL <= 2.66),
                          which(acs$AGE <= 18 & acs$FPL <= 3.22 & acs$county %in% high_chip_counties)))
# Simpler: filter out Medicaid/CHIP eligible individuals directly
acs <- acs %>%
  mutate(is_medicaid = as.integer(
    (FPL < 1.38 & (temporary_resident == 0 | is.na(temporary_resident))) |
    (FPL <= 2.66 & AGE <= 18 & (temporary_resident == 0 | is.na(temporary_resident))) |
    (FPL <= 3.22 & AGE <= 18 & (temporary_resident == 0 | is.na(temporary_resident)) &
       county %in% high_chip_counties)
  )) %>%
  filter(is_medicaid == 0) %>%
  select(-is_medicaid)

# Update households
households <- households %>% filter(household_year %in% unique(acs$household_year))

# Recalculate enrollees and subsidy after filtering
enroll_update <- acs %>%
  group_by(household_year) %>%
  summarize(
    enrollees = n(),
    subsidized_members = sum(subsidized, na.rm = TRUE),
    weight = sum(PERWT, na.rm = TRUE),
    premiumSLC = sum(premiumSLC, na.rm = TRUE),
    rating_factor = sum(rating_factor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(unsubsidized_members = enrollees - subsidized_members)

households <- households %>%
  select(-enrollees, -subsidized_members, -unsubsidized_members, -weight,
         -premiumSLC, -rating_factor) %>%
  left_join(enroll_update, by = "household_year") %>%
  mutate(subsidy_linear_piece = ifelse(subsidized_members == 0, NA_character_, subsidy_linear_piece))

# Recalculate subsidy with updated SLC
mixed_hh2 <- households %>%
  filter(unsubsidized_members > 0 & subsidized_members > 0)
if (nrow(mixed_hh2) > 0) {
  unsub_slc2 <- acs %>%
    filter(household_year %in% mixed_hh2$household_year, subsidized == 0) %>%
    group_by(household_year) %>%
    summarize(premiumSLC_unsubsidized = sum(premiumSLC, na.rm = TRUE), .groups = "drop")
  households <- households %>%
    select(-premiumSLC_unsubsidized) %>%
    left_join(unsub_slc2, by = "household_year") %>%
    mutate(premiumSLC_unsubsidized = coalesce(premiumSLC_unsubsidized, 0))
} else {
  households$premiumSLC_unsubsidized <- 0
}

households <- households %>%
  mutate(
    subsidy = pmax(0, (premiumSLC - premiumSLC_unsubsidized) -
                     coalesce(SLC_contribution, 0)),
    subsidy = ifelse(is.na(SLC_contribution), 0, subsidy)
  )
acs$subsidy <- households$subsidy[match(acs$household_year, households$household_year)]

# Recompute demographics on filtered (market) population
households <- compute_demographic_pcts(acs, households, size_col = enrollees)


# Renumber individual IDs ---------------------------------------------------

acs$individual_id <- seq_len(nrow(acs)) + 100000000L


# Save outputs ---------------------------------------------------------------

cat("  Saving ACS market outputs...\n")
acs_out <- acs %>%
  select(individual_id, household_id, household_year, year, AGE, gender, race,
         FPL, subsidized, rating_factor, premiumSLC, subsidy, SLC_contribution,
         county, rating_area, PERWT, penalty_exempt, penalty_exempt_belowfiling,
         penalty_exempt_unaffordable, cheapest_premium, RACAMIND, HIURULE, MARST)

households_out <- households %>%
  select(household_year, household_id, year, household_size, enrollees,
         FPL, subsidy_linear_piece, subsidized_members, unsubsidized_members,
         fpl_LB, fpl_UB, perc_LB, perc_UB, poverty_threshold,
         SLC_contribution, premiumSLC, premiumSLC_unsubsidized, subsidy,
         rating_factor, weight, county, rating_area,
         tax_unit_type, filing_threshold, cheapest_premium,
         perc_male, perc_0to17, perc_18to25, perc_26to34, perc_35to44,
         perc_45to54, perc_55to64, perc_65plus,
         perc_white, perc_black, perc_hispanic, perc_asian, perc_other,
         mean_age)

fwrite(acs_out, "data/output/acs_individuals.csv")
fwrite(households_out, "data/output/acs_households.csv")

cat("Step 7 complete:", nrow(acs_out), "individuals,",
    nrow(households_out), "households.\n")
