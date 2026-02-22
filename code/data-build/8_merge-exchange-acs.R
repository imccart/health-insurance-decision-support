# 8_merge-exchange-acs.R
# Merge exchange enrollment with ACS market data. Plan numbering, dynamic
# choice matrix, enrollment timing, demographics, income imputation.
#
# Input:  data/output/enrollment_clean.csv, households_clean.csv (from step 5)
#         data/output/income_model_coefs.csv, income_distribution.csv (from step 7)
#         data/input/Covered California/plan_data.csv, product_definitions.csv,
#         zip3_choices.csv
# Output: data/output/exchange_individuals.csv, data/output/exchange_households.csv,
#         data/output/dynamic_choices.csv

set.seed(3)

years <- 2014:2019


# Load data ----------------------------------------------------------------

cat("  Loading exchange data...\n")
enroll <- fread("data/output/enrollment_clean.csv") %>% as_tibble()
households_raw <- fread("data/output/households_clean.csv") %>% as_tibble()

plan_data <- read.csv("data/input/Covered California/plan_data.csv",
                       stringsAsFactors = FALSE)
product_definitions <- read.csv("data/input/Covered California/product_definitions.csv",
                                 stringsAsFactors = FALSE, row.names = 1)
zip3_choices <- read.csv("data/input/Covered California/zip3_choices.csv",
                          stringsAsFactors = FALSE, row.names = 1)

income_coefs <- fread("data/output/income_model_coefs.csv") %>% as_tibble()
income_dist  <- fread("data/output/income_distribution.csv") %>% as_tibble()

# Standardize plan_data insurer names
plan_data$Issuer_Name <- standardize_insurer(plan_data$Issuer_Name)
plan_data$plan_unique_id <- plan_data$HIOSYR


# Rename and rescale variables ---------------------------------------------

exchange_households <- households_raw %>%
  mutate(
    household_size = implied_household_size,
    enrollees = members,
    rating_area = region,
    FPL = FPL / 100
  ) %>%
  select(-members, -implied_household_size, -region)

enroll <- enroll %>%
  mutate(
    rating_area = region,
    FPL = subsidy_fpl_percent_int / 100
  )

# Household rating factor (sum of individual factors)
hh_rf <- enroll %>%
  group_by(household_year) %>%
  summarize(rating_factor_sum = sum(rating_factor, na.rm = TRUE), .groups = "drop")

exchange_households <- exchange_households %>%
  select(-any_of("rating_factor")) %>%
  left_join(hh_rf, by = "household_year") %>%
  rename(rating_factor = rating_factor_sum)

# Ensure household_id is populated
hh_ids <- enroll %>%
  group_by(household_year) %>%
  summarize(household_id_from_enroll = first(household_id), .groups = "drop")

exchange_households <- exchange_households %>%
  left_join(hh_ids, by = "household_year") %>%
  mutate(household_id = coalesce(household_id, household_id_from_enroll)) %>%
  select(-household_id_from_enroll)


# Plan numbering -----------------------------------------------------------

cat("  Numbering plans...\n")

# Full plan names (Plan_Name2)
plan_names <- sort(unique(as.character(plan_data$Plan_Name2)))
plan_numbers <- setNames(seq_along(plan_names), plan_names)

# No-CSR plan names (Plan_Name2_NOCSR)
plan_names_nocsr <- sort(unique(as.character(plan_data$Plan_Name2_NOCSR)))
plan_numbers_nocsr <- setNames(plan_numbers[plan_names_nocsr], plan_names_nocsr)

# Small plan names (Plan_Name_Small)
plan_names_small <- sort(unique(as.character(plan_data$Plan_Name_Small)))
plan_numbers_small <- setNames(seq_along(plan_names_small), plan_names_small)

# Add plan_number columns to plan_data
plan_data$plan_number <- plan_numbers[as.character(plan_data$Plan_Name2)]
plan_data$plan_number_nocsr <- plan_numbers[as.character(plan_data$Plan_Name2_NOCSR)]
plan_data$plan_number_small <- plan_numbers_small[as.character(plan_data$Plan_Name_Small)]

# Add to enrollment
enroll <- enroll %>%
  mutate(
    plan_unique_id = ifelse(!is.na(plan_id), plan_data$plan_unique_id[plan_id], NA_character_),
    plan_number = ifelse(!is.na(plan_id), plan_numbers[plan_name], NA_integer_),
    plan_name_nocsr = ifelse(!is.na(plan_id), plan_data$Plan_Name2_NOCSR[plan_id], NA_character_),
    plan_number_nocsr = ifelse(!is.na(plan_id), plan_numbers_nocsr[plan_name_nocsr], NA_integer_),
    plan_number_small = ifelse(!is.na(plan_id), plan_numbers_small[plan_name], NA_integer_)
  )


# Assign head plan per household -------------------------------------------

assign_head <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else x[1]
}

hh_plan_info <- enroll %>%
  group_by(household_year) %>%
  summarize(
    hh_plan_id = assign_head(plan_id),
    hh_plan_name = assign_head(plan_name),
    hh_plan_unique_id = assign_head(plan_unique_id),
    hh_plan_number_nocsr = assign_head(plan_number_nocsr),
    .groups = "drop"
  )

exchange_households <- exchange_households %>%
  left_join(hh_plan_info, by = "household_year") %>%
  rename(plan_id = hh_plan_id, plan_name = hh_plan_name,
         plan_unique_id = hh_plan_unique_id, plan_number_nocsr = hh_plan_number_nocsr)


# Prepare zipchoices matrix for get_available_plans() (from _helpers-demand.R)
zipchoices <- as.matrix(zip3_choices[, 4:ncol(zip3_choices)])


# Dynamic choice matrix and previous/next plan tracking --------------------

cat("  Building dynamic choices...\n")
household_names <- unique(exchange_households$household_id)
dynamic_choices <- matrix(NA_integer_, nrow = length(household_names),
                           ncol = length(years),
                           dimnames = list(as.character(household_names),
                                           as.character(years)))

exchange_households$zip_region_year <- paste(exchange_households$zip3,
                                             exchange_households$rating_area,
                                             exchange_households$year, sep = "_")
exchange_households$previous_plan_number <- NA_integer_
exchange_households$next_plan_number <- NA_integer_
exchange_households$previous_plan_offered <- NA_integer_

for (t in years) {
  hh_t <- exchange_households %>%
    filter(year == t, !flagged)
  hh_names_t <- as.character(hh_t$household_id)
  plan_nums_t <- hh_t$plan_number_nocsr

  # Fill dynamic choice matrix
  idx <- match(hh_names_t, rownames(dynamic_choices))
  dynamic_choices[idx[!is.na(idx)], as.character(t)] <- plan_nums_t[!is.na(idx)]

  # Previous plan
  if (t > min(years)) {
    prev_hh <- exchange_households %>%
      filter(year == t - 1, !flagged,
             household_id %in% as.numeric(hh_names_t))
    if (nrow(prev_hh) > 0) {
      curr_keys <- paste(prev_hh$household_id, t, sep = "_")
      match_idx <- match(curr_keys, exchange_households$household_year)
      exchange_households$previous_plan_number[match_idx[!is.na(match_idx)]] <-
        prev_hh$plan_number_nocsr[!is.na(match_idx)]

      # Check if previous plan is still offered
      zry_rows <- rownames(zip3_choices)[zip3_choices$Year == t]
      for (zry in zry_rows) {
        avail_plans <- unique(plan_numbers_nocsr[
          plan_data$Plan_Name2_NOCSR[get_available_plans(zry)]])
        hh_in_zry <- which(exchange_households$household_year %in% curr_keys &
                             exchange_households$zip_region_year == zry)
        if (length(hh_in_zry) > 0) {
          prev_plans <- exchange_households$previous_plan_number[hh_in_zry]
          exchange_households$previous_plan_offered[hh_in_zry] <-
            as.integer(prev_plans %in% avail_plans)
        }
      }
    }
  }

  # Next plan
  if (t < max(years)) {
    next_hh <- exchange_households %>%
      filter(year == t + 1, !flagged,
             household_id %in% as.numeric(hh_names_t))
    if (nrow(next_hh) > 0) {
      curr_keys <- paste(next_hh$household_id, t, sep = "_")
      match_idx <- match(curr_keys, exchange_households$household_year)
      exchange_households$next_plan_number[match_idx[!is.na(match_idx)]] <-
        next_hh$plan_number_nocsr[!is.na(match_idx)]
    }
  }
}

# Assign outside option plan number for missing plan_id
outside_option <- max(enroll$plan_number, na.rm = TRUE) + 1L
outside_option_nocsr <- max(enroll$plan_number_nocsr, na.rm = TRUE) + 1L
outside_option_small <- max(enroll$plan_number_small, na.rm = TRUE) + 1L

enroll$plan_number[is.na(enroll$plan_id)] <- outside_option
enroll$plan_number_nocsr[is.na(enroll$plan_id)] <- outside_option_nocsr
enroll$plan_number_small[is.na(enroll$plan_id)] <- outside_option_small


# Enrollment timing --------------------------------------------------------

cat("  Computing enrollment timing...\n")

# Departed midyear (dropped before Oct 1 = week 39, using 90-day grace period)
enroll$dep_midyear <- as.integer(enroll$end_week < 39 & !is.na(enroll$end_week))

hh_dep <- enroll %>%
  group_by(household_year) %>%
  summarize(dep_count = sum(dep_midyear, na.rm = TRUE), .groups = "drop")

exchange_households <- exchange_households %>%
  left_join(hh_dep, by = "household_year") %>%
  mutate(dep_midyear = as.integer(dep_count >= enrollees)) %>%
  select(-dep_count)

# SEP (special enrollment period): household has members who enrolled outside OEP
hh_oep <- enroll %>%
  group_by(household_year) %>%
  summarize(oep_count = sum(as.numeric(OEP), na.rm = TRUE), .groups = "drop")

exchange_households <- exchange_households %>%
  left_join(hh_oep, by = "household_year") %>%
  mutate(SEP = as.integer(oep_count < enrollees)) %>%
  select(-oep_count)


# Demographics -------------------------------------------------------------

cat("  Computing exchange demographics...\n")

# Drop households with NA gender (matches old code: only ~4 households)
enroll <- enroll %>% mutate(gender = as.numeric(gender))
hh_gender <- enroll %>%
  group_by(household_year) %>%
  summarize(perc_male = mean(gender, na.rm = TRUE), .groups = "drop")

keep_hh <- hh_gender %>% filter(!is.na(perc_male)) %>% pull(household_year)
exchange_households <- exchange_households %>% filter(household_year %in% keep_hh)
enroll <- enroll %>% filter(household_year %in% keep_hh)

# Use shared helper for demographics
exchange_households <- compute_demographic_pcts(enroll, exchange_households,
                                                 size_col = enrollees)


# Income imputation --------------------------------------------------------

cat("  Imputing income for exchange households...\n")

# Reconstruct OLS prediction from saved coefficients
# Build model matrix for >400% FPL unsubsidized households with missing FPL
target_idx <- which(exchange_households$subsidy_fpl_bracket == "400% FPL or greater" &
                      !exchange_households$flagged &
                      is.na(exchange_households$FPL))

if (length(target_idx) > 0) {
  target_hh <- exchange_households[target_idx, ]

  # Build prediction manually from coefficients
  coef_vec <- setNames(income_coefs$estimate, income_coefs$term)

  # Start with intercept
  scores <- rep(coef_vec["(Intercept)"], nrow(target_hh))

  # Rating area dummies
  for (ra in unique(target_hh$rating_area)) {
    ra_term <- paste0("as.factor(rating_area)", ra)
    if (ra_term %in% names(coef_vec)) {
      ra_idx <- which(target_hh$rating_area == ra)
      scores[ra_idx] <- scores[ra_idx] + coef_vec[ra_term]
    }
  }

  # Continuous covariates
  cont_vars <- c("household_size", "perc_male", "perc_0to17", "perc_18to25",
                  "perc_26to34", "perc_35to44", "perc_45to54", "perc_55to64",
                  "perc_white", "perc_black", "perc_hispanic", "perc_asian")
  for (v in cont_vars) {
    if (v %in% names(coef_vec) && v %in% names(target_hh)) {
      scores <- scores + coef_vec[v] * target_hh[[v]]
    }
  }

  # Rank-based sampling from ACS income distribution
  income_ranks <- rank(scores, ties.method = "random")
  sampled_incomes <- sort(sample(income_dist$FPL, size = length(target_idx), replace = TRUE))
  exchange_households$FPL[target_idx] <- sampled_incomes[income_ranks]
}

# For subsidized households with missing FPL: sample within bracket
income_groups <- c("138% FPL or less", "138% FPL to 150% FPL", "150% FPL to 200% FPL",
                   "200% FPL to 250% FPL", "250% FPL to 400% FPL")

for (group in income_groups) {
  missing_idx <- which(exchange_households$subsidy_fpl_bracket == group &
                         is.na(exchange_households$FPL) &
                         !exchange_households$flagged)
  donor_idx <- which(exchange_households$subsidy_fpl_bracket == group &
                       !is.na(exchange_households$FPL) &
                       !exchange_households$flagged)
  if (length(missing_idx) > 0 && length(donor_idx) > 0) {
    exchange_households$FPL[missing_idx] <- sample(
      exchange_households$FPL[donor_idx], size = length(missing_idx), replace = TRUE)
  }
}

# Propagate FPL back to enrollment
enroll$FPL <- exchange_households$FPL[match(enroll$household_year,
                                             exchange_households$household_year)]


# Save outputs -------------------------------------------------------------

cat("  Saving exchange outputs...\n")
exchange_indiv_out <- enroll %>%
  select(individual_id, household_id, household_year, year, age, gender, race,
         plan_id, plan_name, plan_unique_id, plan_number, plan_number_nocsr,
         plan_number_small, plan_name_nocsr,
         premium21, metal, metal_level_enhanced, insurer,
         gross_premium_amt_int, net_premium_amt_int,
         premiumSLC, SLC_contribution, subsidy, FPL,
         subsidy_eligible, subsidy_fpl_bracket,
         rating_factor, rating_area, zip3, zip_region_year,
         OEP, start_week, end_week, dep_midyear,
         flagged, imputed_income_flag,
         service_channel, language_spoken)

exchange_hh_out <- exchange_households %>%
  select(household_year, household_id, year, household_size, enrollees,
         FPL, subsidy_fpl_bracket, subsidy_linear_piece,
         subsidized_members, unsubsidized_members,
         gross_premium_amt_int, net_premium_amt_int,
         premium21, premiumSLC, SLC_contribution, subsidy,
         rating_factor, rating_area, zip3, zip_region_year,
         plan_id, plan_name, plan_unique_id, plan_number_nocsr,
         previous_plan_number, next_plan_number, previous_plan_offered,
         dep_midyear, SEP, metal_level_enhanced,
         perc_male, perc_0to17, perc_18to25, perc_26to34, perc_35to44,
         perc_45to54, perc_55to64, perc_65plus,
         perc_white, perc_black, perc_hispanic, perc_asian, perc_other,
         mean_age, flagged)

fwrite(exchange_indiv_out, "data/output/exchange_individuals.csv")
fwrite(exchange_hh_out, "data/output/exchange_households.csv")
fwrite(as.data.frame(dynamic_choices), "data/output/dynamic_choices.csv",
       row.names = TRUE)

cat("Step 8 complete:", nrow(exchange_indiv_out), "individuals,",
    nrow(exchange_hh_out), "households.\n")
