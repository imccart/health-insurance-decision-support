# Helper functions for demand estimation data preparation
# Shared across scripts 6-10


# Compute demographic percentages per household ------------------------------
#
# Calculates age-group and race/ethnicity percentages for each household.
# Used after filtering (ACS market population, exchange enrollees, panel).
#
# Args:
#   df        tibble of individual records with columns: household_year,
#             age, gender, race (White/Black/Hispanic/Asian/Other Race)
#   size_col  unquoted name of household-size column in hh_df (for denominator)
#   hh_df     tibble of household records (joined by household_year)
#
# Returns: hh_df with added percentage columns

compute_demographic_pcts <- function(df, hh_df, size_col = members) {
  size_col <- enquo(size_col)

  # Gender
  gender_pct <- df %>%
    group_by(household_year) %>%
    summarize(perc_male = mean(as.numeric(gender), na.rm = TRUE), .groups = "drop")

  # Age groups
  age_pcts <- df %>%
    mutate(
      age_group = case_when(
        age < 18  ~ "perc_0to17",
        age <= 25 ~ "perc_18to25",
        age <= 34 ~ "perc_26to34",
        age <= 44 ~ "perc_35to44",
        age <= 54 ~ "perc_45to54",
        age <= 64 ~ "perc_55to64",
        TRUE      ~ "perc_65plus"
      )
    ) %>%
    count(household_year, age_group) %>%
    pivot_wider(names_from = age_group, values_from = n, values_fill = 0L)

  # Divide by household size
  age_cols <- c("perc_0to17", "perc_18to25", "perc_26to34", "perc_35to44",
                "perc_45to54", "perc_55to64", "perc_65plus")

  # Race/ethnicity
  race_pcts <- df %>%
    mutate(
      race_group = case_when(
        race == "White"                ~ "perc_white",
        race == "Black/African American" ~ "perc_black",
        race == "Hispanic"             ~ "perc_hispanic",
        race == "Asian"                ~ "perc_asian",
        TRUE                           ~ "perc_other"
      )
    ) %>%
    count(household_year, race_group) %>%
    pivot_wider(names_from = race_group, values_from = n, values_fill = 0L)

  race_cols <- c("perc_white", "perc_black", "perc_hispanic", "perc_asian", "perc_other")

  # Mean age
  mean_ages <- df %>%
    group_by(household_year) %>%
    summarize(mean_age = mean(age, na.rm = TRUE), .groups = "drop")

  # Drop existing demographic columns from hh_df to avoid conflicts
  all_demo_cols <- c("perc_male", age_cols, race_cols, "mean_age")
  hh_df <- hh_df %>% select(-any_of(all_demo_cols))

  # Join all

  hh_df <- hh_df %>%
    left_join(gender_pct, by = "household_year") %>%
    left_join(age_pcts, by = "household_year") %>%
    left_join(race_pcts, by = "household_year") %>%
    left_join(mean_ages, by = "household_year")

  # Ensure all columns exist (fill missing with 0)
  for (col in c(age_cols, race_cols)) {
    if (!col %in% names(hh_df)) hh_df[[col]] <- 0L
  }

  # Convert counts to percentages
  hh_size <- hh_df %>% pull(!!size_col)
  for (col in c(age_cols, race_cols)) {
    hh_df[[col]] <- hh_df[[col]] / hh_size
  }

  hh_df
}


# Probabilistic PUMA-to-county assignment ------------------------------------
#
# For multi-county PUMAs, assigns county proportional to SAHIE uninsured
# population. Single-county PUMAs get deterministic assignment.
#
# Args:
#   puma_code     integer PUMA code
#   pumas_lookup  data.frame with PUMA5CE, COUNTY, RATING_AREA columns
#   sahie         SAHIE data filtered to CA, all ages/races/sexes
#   seed          random seed for reproducibility
#
# Returns: data.frame with one row: county, rating_area

assign_puma_county <- function(puma_code, pumas_lookup, sahie, seed = NULL) {
  rows <- pumas_lookup[pumas_lookup$PUMA5CE == puma_code, ]
  if (nrow(rows) == 0) return(data.frame(county = NA_character_, rating_area = NA_integer_))
  if (nrow(rows) == 1) return(data.frame(county = rows$COUNTY[1],
                                          rating_area = as.integer(rows$RATING_AREA[1])))

  # Multi-county: weight by SAHIE uninsured count
  counties <- rows$COUNTY
  sahie_ca <- sahie %>%
    filter(statefips == 6, geocat == 50, agecat == 0, racecat == 0,
           sexcat == 0, iprcat == 0)

  weights <- sapply(counties, function(cty) {
    match_row <- sahie_ca[toupper(sahie_ca$`County Name`) == toupper(cty) |
                           grepl(toupper(cty), toupper(sahie_ca$`County Name`)), ]
    if (nrow(match_row) > 0) as.numeric(match_row$NUI[1]) else 1
  })
  weights <- weights / sum(weights)

  if (!is.null(seed)) set.seed(seed)
  chosen <- sample(seq_along(counties), 1, prob = weights)
  data.frame(county = counties[chosen],
             rating_area = as.integer(rows$RATING_AREA[chosen]))
}


# Compute cheapest bronze premium per market ---------------------------------
#
# For each zip3 x region x year, find the cheapest bronze plan premium
# (age-21 equivalent). Used for affordability exemption calculation.
#
# Args:
#   zip3_long    tibble with one row per zip3 x region x year x available product
#   prod_defs    product_definitions tibble
#   plan_data    plan_data tibble with Premium, Issuer_Name, etc.
#
# Returns: tibble with columns zip3, region, year, cheapest_premium

compute_cheapest_bronze <- function(zip3_long, prod_defs, plan_data) {

  bronze_plans <- plan_data %>%
    filter(metal_level == "Bronze") %>%
    select(Issuer_Name, PLAN_NETWORK_TYPE, MSP, Network_num,
           region, ENROLLMENT_YEAR, Premium)

  available_bronze <- zip3_long %>%
    inner_join(prod_defs, by = c("product" = "Product")) %>%
    inner_join(bronze_plans,
               by = c("insurer" = "Issuer_Name",
                       "plan_network_type" = "PLAN_NETWORK_TYPE",
                       "MSP" = "MSP",
                       "region" = "region",
                       "year" = "ENROLLMENT_YEAR"),
               relationship = "many-to-many") %>%
    filter(is.na(Network_num) | Network == Network_num)

  available_bronze %>%
    group_by(zip3, region, year) %>%
    summarize(cheapest_premium = min(Premium) / RATING_FACTOR_AGE40,
              .groups = "drop")
}


# Get available plans per market row -----------------------------------------
#
# Given a row index into zip3_choices, returns plan_data row indices for all
# plans available in that zip3/region/year market. Uses product_definitions
# to generalize the old per-insurer logic.
#
# Expects in calling scope:
#   zip3_choices, plan_data, product_definitions, zipchoices (matrix form)
#
# Args:
#   i             row name/index into zip3_choices
#   metal_filter  optional metal level string (e.g., "Silver", "Bronze")
#
# Returns: integer vector of plan_data row indices

SINGLE_PRODUCT_INSURERS <- c("Chinese_Community", "Contra_Costa", "Kaiser",
                              "LA_Care", "Molina", "Oscar", "United",
                              "Valley", "Western")

get_available_plans <- function(i, metal_filter = NULL) {
  yr  <- zip3_choices[i, "Year"]
  reg <- zip3_choices[i, "Region"]

  region_year_idx <- which(plan_data$ENROLLMENT_YEAR == yr & plan_data$region == reg)
  if (!is.null(metal_filter)) {
    region_year_idx <- intersect(region_year_idx,
                                  which(plan_data$metal_level == metal_filter))
  }

  available_products <- names(which(!is.na(zipchoices[i, ])))
  available_plans <- integer(0)

  for (prod in available_products) {
    ins <- product_definitions[prod, "insurer"]
    pnt <- product_definitions[prod, "plan_network_type"]
    msp <- product_definitions[prod, "MSP"]
    net <- product_definitions[prod, "Network"]

    if (ins == "SHARP") {
      idx <- intersect(region_year_idx,
                        which(plan_data$Issuer_Name == "SHARP" &
                                plan_data$Network_num == net &
                                !is.na(plan_data$Network_num)))
    } else if (ins %in% SINGLE_PRODUCT_INSURERS) {
      idx <- intersect(region_year_idx,
                        which(plan_data$Issuer_Name == ins))
    } else {
      idx <- intersect(region_year_idx,
                        which(plan_data$Issuer_Name == ins &
                                plan_data$PLAN_NETWORK_TYPE == pnt &
                                plan_data$MSP == msp))
    }
    available_plans <- c(available_plans, idx)
  }

  unique(available_plans)
}
