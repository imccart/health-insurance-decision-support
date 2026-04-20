# 5_assemble-enrollment.R
# Final assembly: verify income, reassign ACS plans, rebuild plan IDs,
# create unified household object, export clean datasets.
#
# Input:  data/output/enrollment_step4.csv, data/output/households_step4.csv
# Output: data/output/enrollment_clean.csv, data/output/households_clean.csv

# Load data ----------------------------------------------------------------

if (!exists("enroll")) enroll <- fread("data/output/enrollment_step4.csv")
enroll <- as_tibble(enroll)
if (exists("households_out")) {
  households_fam <- as_tibble(households_out)
} else {
  households_fam <- fread("data/output/households_step4.csv") %>% as_tibble()
}
plan_data <- read.csv("data/input/Covered California/plan_data.csv",
                       stringsAsFactors = FALSE)
age_rating_factors <- read.csv("data/input/Covered California/age_rating_factors.csv",
                                stringsAsFactors = FALSE)
poverty_guidelines <- read.csv("data/input/Covered California/poverty_guidelines.csv",
                                stringsAsFactors = FALSE)
contribution_percentages <- read.csv("data/input/Covered California/contribution_percentages.csv",
                                      stringsAsFactors = FALSE)

# Standardize plan_data insurer names
plan_data$Issuer_Name <- standardize_insurer(plan_data$Issuer_Name)


# Build unified household object -------------------------------------------

# Singles households from enrollment
singles <- enroll %>%
  filter(members == 1, !flagged) %>%
  group_by(household_year) %>%
  summarize(
    household_id = first(household_id),
    year = first(year),
    members = 1L,
    plans = n_distinct(plan_name),
    gross_premium_amt_int = first(gross_premium_amt_int),
    net_premium_amt_int = first(net_premium_amt_int),
    premium21 = first(premium21),
    premiumSLC = first(premiumSLC),
    subsidy = first(subsidy),
    SLC_contribution = first(SLC_contribution),
    subsidy_fpl_bracket = first(subsidy_fpl_bracket),
    subsidy_linear_piece = first(subsidy_linear_piece),
    FPL = first(subsidy_fpl_percent_int),
    implied_household_size = first(implied_household_size),
    imputed_income_flag = first(imputed_income_flag),
    subsidized_members = as.integer(first(subsidy_eligible) == "Subsidy Eligible"),
    unsubsidized_members = as.integer(first(subsidy_eligible) == "Not Subsidy Elig"),
    metal_level_enhanced = first(metal_level_enhanced),
    region = first(region),
    zip3 = first(zip3),
    flagged = first(flagged),
    .groups = "drop"
  )

# Add metal_level_enhanced to family households
fam_metals <- enroll %>%
  filter(members > 1) %>%
  group_by(household_year) %>%
  summarize(metal_level_enhanced = first(metal_level_enhanced), .groups = "drop")

households_fam <- households_fam %>%
  left_join(fam_metals, by = "household_year")

# Combine
households <- bind_rows(singles, households_fam)


# Verify income variables --------------------------------------------------

# Reassign subsidy_fpl_bracket from FPL values for consistency
households <- households %>%
  mutate(
    subsidy_fpl_bracket = case_when(
      FPL >= 0 & FPL <= 138 & !is.na(FPL) ~ "138% FPL or less",
      FPL > 138 & FPL <= 150 & !is.na(FPL) ~ "138% FPL to 150% FPL",
      FPL > 150 & FPL <= 200 & !is.na(FPL) ~ "150% FPL to 200% FPL",
      FPL > 200 & FPL <= 250 & !is.na(FPL) ~ "200% FPL to 250% FPL",
      FPL > 250 & FPL <= 400 & !is.na(FPL) ~ "250% FPL to 400% FPL",
      FPL > 400 & !is.na(FPL) ~ "400% FPL or greater",
      TRUE ~ subsidy_fpl_bracket
    ),
    # Unsubsidized >400%
    subsidized_members = ifelse(
      subsidy_fpl_bracket == "400% FPL or greater" & !is.na(subsidy_fpl_bracket),
      0L, subsidized_members),
    unsubsidized_members = ifelse(
      subsidy_fpl_bracket == "400% FPL or greater" & !is.na(subsidy_fpl_bracket),
      members, unsubsidized_members),
    # Rebuild subsidy_linear_piece from bracket
    subsidy_linear_piece = case_when(
      FPL > 300 & FPL <= 400 & !is.na(FPL) ~ "300% FPL to 400% FPL",
      FPL > 250 & FPL <= 300 & !is.na(FPL) ~ "250% FPL to 300% FPL",
      !is.na(FPL) ~ subsidy_fpl_bracket,
      TRUE ~ NA_character_
    ),
    subsidy_linear_piece = ifelse(
      subsidized_members == 0 & !is.na(subsidized_members),
      NA_character_, subsidy_linear_piece)
  )


# Recompute subsidy parameters for verification ----------------------------

income_pieces <- c("138% FPL or less", "138% FPL to 150% FPL", "150% FPL to 200% FPL",
                   "200% FPL to 250% FPL", "250% FPL to 300% FPL", "300% FPL to 400% FPL",
                   "400% FPL or greater")
income_breakpoints <- c(0, 1.38, 1.5, 2, 2.5, 3, 4)

fpl_lb_lookup <- setNames(income_breakpoints, income_pieces[seq_along(income_breakpoints)])
fpl_ub_lookup <- setNames(c(income_breakpoints[-1], NA), income_pieces[seq_along(income_breakpoints)])

subsidized_idx <- which(!is.na(households$subsidy_linear_piece))

households$fpl_LB <- households$fpl_UB <- households$perc_LB <-
  households$perc_UB <- households$poverty_threshold <- NA_real_

households$fpl_LB[subsidized_idx] <- fpl_lb_lookup[households$subsidy_linear_piece[subsidized_idx]]
households$fpl_UB[subsidized_idx] <- fpl_ub_lookup[households$subsidy_linear_piece[subsidized_idx]]

for (yr in 2014:2019) {
  yr_col <- paste0("YR", yr)
  yr_idx <- intersect(subsidized_idx, which(households$year == yr))
  if (length(yr_idx) == 0) next

  households$perc_LB[yr_idx] <- contribution_percentages[[yr_col]][
    match(households$fpl_LB[yr_idx], contribution_percentages$FPL)]
  households$perc_UB[yr_idx] <- contribution_percentages[[yr_col]][
    match(households$fpl_UB[yr_idx], contribution_percentages$FPL)]
  households$poverty_threshold[yr_idx] <- poverty_guidelines[[yr_col]][
    match(households$implied_household_size[yr_idx], poverty_guidelines$Family_Size)]
}

# Rating factor (sum of individual rating factors per household)
enroll <- enroll %>%
  mutate(
    age_capped = pmin(64, age),
    rating_factor = ifelse(
      year >= 2018,
      age_rating_factors$Rating_Factor2018[match(age_capped, age_rating_factors$Age)],
      age_rating_factors$Rating_Factor[match(age_capped, age_rating_factors$Age)]
    )
  ) %>%
  select(-age_capped)

hh_rating_factor <- enroll %>%
  group_by(household_year) %>%
  summarize(rating_factor = sum(rating_factor, na.rm = TRUE), .groups = "drop")

households <- households %>%
  left_join(hh_rating_factor, by = "household_year")

# Recompute gross premium, SLC contribution, subsidy, net premium
households <- households %>%
  mutate(
    gross_premium_amt_int = premium21 * rating_factor,
    SLC_contribution = ifelse(
      !is.na(subsidy_linear_piece) & subsidy_linear_piece != "300% FPL to 400% FPL",
      calculate_aca_contribution(FPL, perc_LB, perc_UB, fpl_LB, fpl_UB,
                                  poverty_threshold, bracket_300_400 = FALSE),
      SLC_contribution),
    SLC_contribution = ifelse(
      !is.na(subsidy_linear_piece) & subsidy_linear_piece == "300% FPL to 400% FPL",
      calculate_aca_contribution(FPL, perc_LB, perc_UB, fpl_LB, fpl_UB,
                                  poverty_threshold, bracket_300_400 = TRUE),
      SLC_contribution),
    SLC_contribution = ifelse(is.na(subsidy_linear_piece), NA_real_, SLC_contribution),
    subsidy = ifelse(!is.na(SLC_contribution),
                     pmax(0, premiumSLC - SLC_contribution), subsidy),
    net_premium_amt_int = ifelse(!is.na(subsidy),
                                 pmax(0, gross_premium_amt_int - subsidy),
                                 net_premium_amt_int)
  )

# Catastrophic (Minimum Coverage) singles: no subsidy applies
cat_singles <- which(households$members == 1 &
                       households$metal_level_enhanced == "Minimum Coverage")
households$net_premium_amt_int[cat_singles] <- NA_real_

# Flagged + unsubsidized: clear subsidy variables
flagged_unsub <- which(households$flagged & households$subsidized_members == 0 &
                         !is.na(households$subsidized_members))
households$SLC_contribution[flagged_unsub] <- NA_real_
households$subsidy[flagged_unsub] <- NA_real_
households$net_premium_amt_int[flagged_unsub] <- NA_real_

flagged_gt400 <- which(households$flagged & households$FPL > 400 & !is.na(households$FPL))
households$SLC_contribution[flagged_gt400] <- NA_real_
households$subsidy[flagged_gt400] <- NA_real_
households$net_premium_amt_int[flagged_gt400] <- NA_real_


# ACS plan consistency -----------------------------------------------------
# Reassign metal_level_enhanced based on final income

enroll <- enroll %>%
  left_join(households %>% select(household_year, FPL), by = "household_year") %>%
  rename(FPL = FPL) %>%
  mutate(
    metal_level_enhanced = case_when(
      # FPL >250% or unsubsidized: can't have enhanced silver
      ((FPL > 250 & !is.na(FPL)) | subsidy_eligible == "Not Subsidy Elig") &
        metal_level_enhanced %in% c("Silver - Enhanced 73", "Silver - Enhanced 87",
                                     "Silver - Enhanced 94") ~ "Silver",
      # 200-250%: should be Enhanced 73
      subsidy_fpl_bracket == "200% FPL to 250% FPL" & metal == "Silver" &
        metal_level_enhanced != "Silver - Enhanced 73" &
        subsidy_eligible == "Subsidy Eligible" ~ "Silver - Enhanced 73",
      # 150-200%: should be Enhanced 87
      subsidy_fpl_bracket == "150% FPL to 200% FPL" & metal == "Silver" &
        metal_level_enhanced != "Silver - Enhanced 87" &
        subsidy_eligible == "Subsidy Eligible" ~ "Silver - Enhanced 87",
      # <150%: should be Enhanced 94
      subsidy_fpl_bracket %in% c("138% FPL to 150% FPL", "138% FPL or less") &
        metal == "Silver" &
        metal_level_enhanced != "Silver - Enhanced 94" &
        subsidy_eligible == "Subsidy Eligible" ~ "Silver - Enhanced 94",
      TRUE ~ metal_level_enhanced
    )
  )

# Rebuild plan_id and premium21 after ACS changes
plan_id_lookup <- setNames(seq_len(nrow(plan_data)),
                           paste(plan_data$HIOS, plan_data$metal_level,
                                 plan_data$ENROLLMENT_YEAR, plan_data$region, sep = "_"))

enroll <- enroll %>%
  mutate(
    plan_key = case_when(
      year %in% c(2014, 2015) & insurer != "SHARP" ~
        paste(substr(hios_id_14, 1, 10), metal_level_enhanced, year, region, sep = "_"),
      TRUE ~
        paste(hios_id_14, metal_level_enhanced, year, region, sep = "_")
    ),
    plan_id = unname(plan_id_lookup[plan_key]),
    premium21 = plan_data$Premium[plan_id] / RATING_FACTOR_AGE40,
    plan_name = plan_data$Plan_Name2[plan_id]
  ) %>%
  select(-plan_key)

# Update households with plans count after reassignment
fam_plans <- enroll %>%
  filter(members > 1) %>%
  group_by(household_year) %>%
  summarize(plans_new = n_distinct(plan_id), .groups = "drop")

households <- households %>%
  left_join(fam_plans, by = "household_year") %>%
  mutate(plans = coalesce(plans_new, plans)) %>%
  select(-plans_new)


# Port final variables back to enrollment ----------------------------------

# Update FPL in enrollment from household
enroll <- enroll %>%
  left_join(
    households %>% select(household_year,
                           hh_gross = gross_premium_amt_int,
                           hh_net = net_premium_amt_int,
                           hh_SLC = SLC_contribution,
                           hh_subsidy = subsidy,
                           hh_bracket = subsidy_fpl_bracket,
                           hh_linear = subsidy_linear_piece,
                           hh_FPL = FPL,
                           hh_implied = implied_household_size,
                           hh_flagged = flagged),
    by = "household_year"
  ) %>%
  mutate(
    gross_premium_amt_int = coalesce(hh_gross, gross_premium_amt_int),
    net_premium_amt_int = coalesce(hh_net, net_premium_amt_int),
    SLC_contribution = hh_SLC,
    subsidy = hh_subsidy,
    subsidy_fpl_bracket = coalesce(hh_bracket, subsidy_fpl_bracket),
    subsidy_linear_piece = hh_linear,
    subsidy_fpl_percent_int = coalesce(hh_FPL, subsidy_fpl_percent_int),
    implied_household_size = coalesce(hh_implied, implied_household_size),
    flagged = coalesce(hh_flagged, flagged)
  ) %>%
  select(-starts_with("hh_"))


# Final column selection ---------------------------------------------------

enroll_clean <- enroll %>%
  select(
    household_id, household_year, year, individual_id,
    age, gender, race, plan_id, plan_name, premium21,
    metal, metal_level_enhanced, insurer,
    gross_premium_amt_int, net_premium_amt_int,
    premiumSLC, SLC_contribution, subsidy,
    subsidy_eligible, subsidy_fpl_bracket, subsidy_fpl_percent_int,
    rating_factor, implied_household_size,
    OEP, start_week, end_week,
    region, zip3, zip_region, zip_region_year,
    members, flagged, imputed_income_flag,
    service_channel, language_spoken
  )

households_clean <- households %>%
  select(
    household_year, household_id, year, members, plans,
    gross_premium_amt_int, net_premium_amt_int,
    premium21, premiumSLC, subsidy, SLC_contribution,
    subsidy_fpl_bracket, subsidy_linear_piece, FPL,
    implied_household_size, imputed_income_flag,
    subsidized_members, unsubsidized_members,
    metal_level_enhanced, rating_factor,
    region, zip3, flagged
  )


# Save clean outputs -------------------------------------------------------

fwrite(enroll_clean, "data/output/enrollment_clean.csv")
fwrite(households_clean, "data/output/households_clean.csv")
cat("Step 5 complete:", nrow(enroll_clean), "records,",
    nrow(households_clean), "households.\n")
