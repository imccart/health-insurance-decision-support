# 9_extend-panel.R
# Extend the exchange panel to all 6 years (2014-2019). For each household
# missing in a year, find the closest reference year, replicate records,
# adjust ages, recalculate subsidies and SLC premiums.
#
# Input:  data/output/exchange_individuals.csv, exchange_households.csv (from step 8)
#         data/input/Covered California/ (plan_data, zip3_choices, product_definitions,
#         contribution_percentages, poverty_guidelines, age_rating_factors, counties)
# Output: data/output/panel_individuals.csv, data/output/panel_households.csv

set.seed(4)

years <- 2014:2019


# Load data ----------------------------------------------------------------

cat("  Loading exchange panel data...\n")
enroll <- fread("data/output/exchange_individuals.csv") %>% as_tibble()
exchange_households <- fread("data/output/exchange_households.csv") %>% as_tibble()

plan_data <- read.csv("data/input/Covered California/plan_data.csv",
                       stringsAsFactors = FALSE)
product_definitions <- read.csv("data/input/Covered California/product_definitions.csv",
                                 stringsAsFactors = FALSE, row.names = 1)
zip3_choices <- read.csv("data/input/Covered California/zip3_choices.csv",
                          stringsAsFactors = FALSE, row.names = 1)
contribution_percentages <- read.csv("data/input/Covered California/contribution_percentages.csv",
                                      stringsAsFactors = FALSE)
poverty_guidelines <- read.csv("data/input/Covered California/poverty_guidelines.csv",
                                stringsAsFactors = FALSE)
age_rating_factors <- read.csv("data/input/Covered California/age_rating_factors.csv",
                                stringsAsFactors = FALSE)

# Standardize insurer names
plan_data$Issuer_Name <- standardize_insurer(plan_data$Issuer_Name)

# Plan numbering (must match script 8)
plan_names_nocsr <- sort(unique(as.character(plan_data$Plan_Name2_NOCSR)))
plan_numbers <- setNames(seq_along(sort(unique(as.character(plan_data$Plan_Name2)))),
                          sort(unique(as.character(plan_data$Plan_Name2))))
plan_numbers_nocsr <- setNames(plan_numbers[plan_names_nocsr], plan_names_nocsr)

# County files for SLC premium
counties_list <- list()
for (yr in years) {
  counties_list[[as.character(yr)]] <- read.csv(
    sprintf("data/input/Covered California/Counties/counties_%d.csv", yr),
    stringsAsFactors = FALSE, row.names = 1
  )
}

# Prepare zipchoices matrix for get_available_plans() (from _helpers-demand.R)
zipchoices <- as.matrix(zip3_choices[, 4:ncol(zip3_choices)])


# Identify missing household-years ----------------------------------------

cat("  Identifying missing household-years...\n")
all_hh_ids <- unique(exchange_households$household_id)
full_grid <- expand.grid(household_id = all_hh_ids, year = years,
                          stringsAsFactors = FALSE)
full_grid$household_year <- paste(full_grid$household_id, full_grid$year, sep = "_")

existing_hhy <- exchange_households$household_year
missing_grid <- full_grid %>% filter(!household_year %in% existing_hhy)

if (nrow(missing_grid) == 0) {
  cat("  No missing household-years. Skipping extension.\n")
} else {
  cat("  Found", nrow(missing_grid), "missing household-years to fill.\n")

  missing_grid$reference_household <- NA_character_
  missing_grid$reference_type <- NA_character_

  # Find closest reference year using offset priority: -1, +1, -2, +2, ..., -5, +5
  offsets <- c(-1, 1, -2, 2, -3, 3, -4, 4, -5, 5)
  assigned <- rep(FALSE, nrow(missing_grid))

  for (offset in offsets) {
    unassigned <- which(!assigned)
    if (length(unassigned) == 0) break

    ref_keys <- paste(missing_grid$household_id[unassigned],
                       missing_grid$year[unassigned] + offset, sep = "_")
    found <- ref_keys %in% existing_hhy
    if (any(found)) {
      idx <- unassigned[found]
      missing_grid$reference_household[idx] <- ref_keys[found]
      label <- if (offset < 0) paste0("previous", if (abs(offset) > 1) paste0("_", abs(offset), "years") else "") else
        paste0("next", if (abs(offset) > 1) paste0("_", abs(offset), "years") else "")
      missing_grid$reference_type[idx] <- label
      assigned[idx] <- TRUE
    }
  }

  # Drop rows with no reference
  missing_grid <- missing_grid %>% filter(!is.na(reference_household))

  # Build new household records from references
  ref_idx <- match(missing_grid$reference_household, exchange_households$household_year)
  new_households <- exchange_households[ref_idx, ]
  new_households$year <- missing_grid$year
  new_households$household_year <- missing_grid$household_year
  new_households$household_id <- missing_grid$household_id
  new_households$reference_household <- missing_grid$reference_household
  new_households$reference_type <- missing_grid$reference_type

  new_households$zip_region_year <- paste(new_households$zip3,
                                           new_households$rating_area,
                                           new_households$year, sep = "_")
  new_households$previous_plan_number <- NA_integer_
  new_households$next_plan_number <- NA_integer_
  new_households$previous_plan_offered <- NA_integer_
  new_households$plan_number_nocsr <- NA_integer_

  # Set previous_plan for those whose reference is previous year
  prev_ref <- which(missing_grid$reference_type == "previous")
  if (length(prev_ref) > 0) {
    new_households$previous_plan_number[prev_ref] <-
      exchange_households$plan_number_nocsr[ref_idx[prev_ref]]
  }
  # Set next_plan for those whose reference is next year
  next_ref <- which(missing_grid$reference_type == "next")
  if (length(next_ref) > 0) {
    new_households$next_plan_number[next_ref] <-
      exchange_households$plan_number_nocsr[ref_idx[next_ref]]
  }

  # Check previous_plan_offered for new households
  for (t in years) {
    if (t <= min(years)) next
    nh_t <- which(new_households$year == t & !new_households$flagged &
                    !is.na(new_households$previous_plan_number))
    if (length(nh_t) == 0) next

    zry_rows <- rownames(zip3_choices)[zip3_choices$Year == t]
    for (zry in zry_rows) {
      avail_plans <- unique(plan_numbers_nocsr[
        plan_data$Plan_Name2_NOCSR[get_available_plans(zry)]])
      hh_in_zry <- intersect(nh_t, which(new_households$zip_region_year == zry))
      if (length(hh_in_zry) > 0) {
        prev_plans <- new_households$previous_plan_number[hh_in_zry]
        new_households$previous_plan_offered[hh_in_zry] <-
          as.integer(prev_plans %in% avail_plans)
      }
    }
  }

  # Clear plan/timing fields
  new_households$premium21 <- NA_real_
  new_households$dep_midyear <- NA_integer_
  new_households$SEP <- NA_integer_
  new_households$metal_level_enhanced <- NA_character_


  # Create individual records for new household-years ----------------------

  cat("  Creating individual records for extended panel...\n")

  all_indiv_ids <- unique(enroll$individual_id)
  indiv_grid <- expand.grid(individual_id = all_indiv_ids, year = years,
                              stringsAsFactors = FALSE)
  indiv_grid$indiv_year <- paste(indiv_grid$individual_id, indiv_grid$year, sep = "_")
  existing_iy <- paste(enroll$individual_id, enroll$year, sep = "_")
  missing_indiv <- indiv_grid %>% filter(!indiv_year %in% existing_iy)

  missing_indiv$reference_individual <- NA_character_
  missing_indiv$reference_type <- NA_character_
  assigned_i <- rep(FALSE, nrow(missing_indiv))

  for (offset in offsets) {
    unassigned <- which(!assigned_i)
    if (length(unassigned) == 0) break
    ref_keys <- paste(missing_indiv$individual_id[unassigned],
                       missing_indiv$year[unassigned] + offset, sep = "_")
    found <- ref_keys %in% existing_iy
    if (any(found)) {
      idx <- unassigned[found]
      missing_indiv$reference_individual[idx] <- ref_keys[found]
      label <- if (offset < 0) paste0("previous", if (abs(offset) > 1) paste0("_", abs(offset), "years") else "") else
        paste0("next", if (abs(offset) > 1) paste0("_", abs(offset), "years") else "")
      missing_indiv$reference_type[idx] <- label
      assigned_i[idx] <- TRUE
    }
  }

  missing_indiv <- missing_indiv %>% filter(!is.na(reference_individual))

  # Build individual records from references
  ref_iy_idx <- match(missing_indiv$reference_individual, existing_iy)
  new_data <- enroll[ref_iy_idx, ]
  year_diff <- missing_indiv$year - new_data$year
  new_data$year <- missing_indiv$year
  new_data$age <- new_data$age + year_diff
  new_data$household_year <- paste(new_data$household_id, new_data$year, sep = "_")
  new_data$zip_region_year <- paste(new_data$zip3, "_", new_data$rating_area, "_",
                                     new_data$year, sep = "")
  # Fix zip_region_year format to match
  new_data$zip_region_year <- paste(new_data$zip3, new_data$rating_area,
                                     new_data$year, sep = "_")

  # Clear plan/timing fields
  new_data <- new_data %>%
    mutate(
      metal_level_enhanced = NA_character_, insurer = NA_character_,
      plan_name = NA_character_, premium21 = NA_real_,
      plan_name_nocsr = NA_character_,
      plan_number = NA_integer_, plan_number_nocsr = NA_integer_,
      plan_number_small = NA_integer_, plan_id = NA_integer_,
      plan_unique_id = NA_character_,
      OEP = NA, dep_midyear = NA_integer_, start_week = NA_integer_,
      end_week = NA_integer_
    )

  # Keep only individuals whose household_year exists in new_households
  new_data <- new_data %>%
    filter(household_year %in% new_households$household_year)

  # Drop households with members age >= 65
  over65_hhy <- new_data %>% filter(age >= 65) %>% pull(household_year) %>% unique()
  new_households <- new_households %>% filter(!household_year %in% over65_hhy)
  new_data <- new_data %>% filter(household_year %in% new_households$household_year)

  # Handle infants (age < 0 in multi-member households)
  infants <- which(new_data$age < 0 & new_data$household_id %in%
                     (new_data %>% group_by(household_year) %>%
                        filter(n() > 1) %>% pull(household_year) %>% unique() %>%
                        {new_data$household_year %in% .} %>%
                        {new_data$household_year[.]}))
  # Simpler: remove individuals with age < 0 in multi-member households
  hhy_counts <- new_data %>% count(household_year, name = "n_members")
  multi_hhy <- hhy_counts %>% filter(n_members > 1) %>% pull(household_year)
  infant_rows <- which(new_data$age < 0 & new_data$household_year %in% multi_hhy)
  if (length(infant_rows) > 0) {
    new_data <- new_data[-infant_rows, ]
  }
  new_data$age <- pmax(0L, new_data$age)

  # Update enrollees
  new_enrollees <- new_data %>%
    count(household_year, name = "enrollees_new")
  new_households <- new_households %>%
    select(-enrollees) %>%
    left_join(new_enrollees, by = "household_year") %>%
    rename(enrollees = enrollees_new) %>%
    mutate(household_size = pmax(household_size, enrollees))

  # Update demographics
  new_households <- compute_demographic_pcts(new_data, new_households,
                                              size_col = enrollees)


  # Recalculate subsidy variables for new years ----------------------------

  cat("  Recalculating subsidies for extended panel...\n")

  # Update rating factors
  new_data <- new_data %>%
    mutate(
      age_capped = pmin(64L, age),
      rating_factor = ifelse(
        year >= 2018,
        age_rating_factors$Rating_Factor2018[match(age_capped, age_rating_factors$Age)],
        age_rating_factors$Rating_Factor[match(age_capped, age_rating_factors$Age)]
      )
    ) %>%
    select(-age_capped)

  hh_rf <- new_data %>%
    group_by(household_year) %>%
    summarize(rating_factor = sum(rating_factor, na.rm = TRUE), .groups = "drop")
  new_households <- new_households %>%
    select(-rating_factor) %>%
    left_join(hh_rf, by = "household_year")

  # Reconstruct subsidy bracket bounds (not saved in CSV)
  fpl_lb_lookup <- c("138% FPL or less" = 0, "138% FPL to 150% FPL" = 1.38,
                     "150% FPL to 200% FPL" = 1.5, "200% FPL to 250% FPL" = 2,
                     "250% FPL to 300% FPL" = 2.5, "300% FPL to 400% FPL" = 3)
  fpl_ub_lookup <- c("138% FPL or less" = 1.38, "138% FPL to 150% FPL" = 1.5,
                     "150% FPL to 200% FPL" = 2, "200% FPL to 250% FPL" = 2.5,
                     "250% FPL to 300% FPL" = 3, "300% FPL to 400% FPL" = 4)

  new_households$fpl_LB <- fpl_lb_lookup[new_households$subsidy_linear_piece]
  new_households$fpl_UB <- fpl_ub_lookup[new_households$subsidy_linear_piece]
  new_households$perc_LB <- NA_real_
  new_households$perc_UB <- NA_real_
  new_households$poverty_threshold <- NA_real_

  # Update contribution percentages and poverty threshold
  sub_idx <- which(!is.na(new_households$subsidy_linear_piece))
  if (length(sub_idx) > 0) {
    rownames(contribution_percentages) <- contribution_percentages$FPL
    rownames(poverty_guidelines) <- poverty_guidelines$Family_Size

    for (yr in years) {
      yr_col <- paste0("YR", min(yr, 2019))
      yr_sub <- intersect(sub_idx, which(new_households$year == yr))
      if (length(yr_sub) == 0) next

      new_households$perc_LB[yr_sub] <- contribution_percentages[
        as.character(new_households$fpl_LB[yr_sub]), yr_col]
      new_households$perc_UB[yr_sub] <- contribution_percentages[
        as.character(new_households$fpl_UB[yr_sub]), yr_col]
      new_households$poverty_threshold[yr_sub] <- poverty_guidelines[
        as.character(new_households$household_size[yr_sub]), yr_col]
    }

    # SLC contribution
    is_300_400 <- new_households$subsidy_linear_piece == "300% FPL to 400% FPL" &
      !is.na(new_households$subsidy_linear_piece)
    not_300_400 <- !is_300_400 & !is.na(new_households$subsidy_linear_piece)

    new_households$SLC_contribution[not_300_400] <- calculate_aca_contribution(
      new_households$FPL[not_300_400], new_households$perc_LB[not_300_400],
      new_households$perc_UB[not_300_400], new_households$fpl_LB[not_300_400],
      new_households$fpl_UB[not_300_400], new_households$poverty_threshold[not_300_400],
      bracket_300_400 = FALSE
    )
    new_households$SLC_contribution[is_300_400] <- calculate_aca_contribution(
      new_households$FPL[is_300_400], new_households$perc_LB[is_300_400],
      new_households$perc_UB[is_300_400], new_households$fpl_LB[is_300_400],
      new_households$fpl_UB[is_300_400], new_households$poverty_threshold[is_300_400],
      bracket_300_400 = TRUE
    )
  }

  # SLC premium via zip3_choices
  # Compute SLC premium per zip_region_year
  compute_slc_zry <- function(i) {
    avail <- get_available_plans(i, metal_filter = "Silver")
    if (length(avail) == 0) return(NA_real_)
    prems <- sort(plan_data$Premium[avail] / RATING_FACTOR_AGE40)
    if (length(prems) == 1) prems[1] else prems[2]
  }

  zry_slc <- data.frame(
    zry = rownames(zip3_choices),
    premiumSLC = sapply(rownames(zip3_choices), compute_slc_zry),
    stringsAsFactors = FALSE
  )

  new_data$premiumSLC <- zry_slc$premiumSLC[match(new_data$zip_region_year, zry_slc$zry)] *
    new_data$rating_factor

  hh_slc <- new_data %>%
    group_by(household_year) %>%
    summarize(premiumSLC = sum(premiumSLC, na.rm = TRUE), .groups = "drop")
  new_households <- new_households %>%
    select(-premiumSLC) %>%
    left_join(hh_slc, by = "household_year")

  new_households$subsidy <- pmax(0, new_households$premiumSLC -
                                    coalesce(new_households$SLC_contribution, 0))
  new_households$subsidy <- ifelse(is.na(new_households$SLC_contribution),
                                    0, new_households$subsidy)

  # Update flagged
  flagged_hhy <- new_data %>%
    group_by(household_year) %>%
    summarize(any_flagged = any(flagged, na.rm = TRUE), .groups = "drop") %>%
    filter(any_flagged) %>%
    pull(household_year)
  new_households$flagged[new_households$household_year %in% flagged_hhy] <- TRUE

  # Propagate subsidy variables to individuals
  new_data <- new_data %>%
    select(-any_of(c("SLC_contribution", "subsidy", "premiumSLC"))) %>%
    left_join(new_households %>% select(household_year, SLC_contribution, subsidy, premiumSLC),
              by = "household_year")


  # Merge with original exchange data --------------------------------------

  cat("  Merging extended records with exchange data...\n")

  # Clean up reference columns before merge
  new_households <- new_households %>%
    select(-any_of(c("reference_household", "reference_type")))

  # Align columns
  common_hh_cols <- intersect(names(exchange_households), names(new_households))
  exchange_households <- bind_rows(
    exchange_households %>% select(all_of(common_hh_cols)),
    new_households %>% select(all_of(common_hh_cols))
  )

  common_indiv_cols <- intersect(names(enroll), names(new_data))
  enroll <- bind_rows(
    enroll %>% select(all_of(common_indiv_cols)),
    new_data %>% select(all_of(common_indiv_cols))
  )
}


# Split household flagging -------------------------------------------------

cat("  Flagging split households...\n")

# Flag individuals who appear in split households across all years
if ("split_flag" %in% names(enroll)) {
  split_indivs <- unique(enroll$individual_id[enroll$split_flag == TRUE])
  split_hh_ids <- unique(enroll$household_id[enroll$individual_id %in% split_indivs])
  split_hhy <- paste(rep(split_hh_ids, each = length(years)),
                      rep(years, times = length(split_hh_ids)), sep = "_")
  enroll$flagged[enroll$individual_id %in% split_indivs] <- TRUE
  exchange_households$flagged[exchange_households$household_year %in% split_hhy] <- TRUE
}

# Flag households not in individual data
orphan_hhy <- setdiff(exchange_households$household_year,
                       unique(enroll$household_year))
exchange_households$flagged[exchange_households$household_year %in% orphan_hhy &
                              !exchange_households$flagged] <- TRUE


# Save outputs -------------------------------------------------------------

cat("  Saving panel outputs...\n")
fwrite(enroll, "data/output/panel_individuals.csv")
fwrite(exchange_households, "data/output/panel_households.csv")

cat("Step 9 complete:", nrow(enroll), "individuals,",
    nrow(exchange_households), "households.\n")
