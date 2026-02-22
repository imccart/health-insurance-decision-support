# 4_reconcile-income.R
# Reconcile net premium and income using ACA subsidy formula.
# Inverts the ACA contribution schedule to recover FPL from observed premiums.
# Handles singles (HH size 1) and families (HH size > 1) separately.
#
# Input:  data/output/enrollment_step3.csv, reference CSVs
# Output: data/output/enrollment_step4.csv, data/output/households_step4.csv

set.seed(6)

# Load data ----------------------------------------------------------------

if (!exists("enroll")) enroll <- fread("data/output/enrollment_step3.csv")
enroll <- as_tibble(enroll)
plan_data <- read.csv("data/input/Covered California/plan_data.csv",
                       stringsAsFactors = FALSE)
age_rating_factors <- read.csv("data/input/Covered California/age_rating_factors.csv",
                                stringsAsFactors = FALSE)
poverty_guidelines <- read.csv("data/input/Covered California/poverty_guidelines.csv",
                                stringsAsFactors = FALSE)
contribution_percentages <- read.csv("data/input/Covered California/contribution_percentages.csv",
                                      stringsAsFactors = FALSE)
zip3_choices <- read.csv("data/input/Covered California/zip3_choices.csv",
                          stringsAsFactors = FALSE, row.names = 1)
product_definitions <- read.csv("data/input/Covered California/product_definitions.csv",
                                 stringsAsFactors = FALSE, row.names = 1)

# Standardize plan_data insurer names
plan_data$Issuer_Name <- standardize_insurer(plan_data$Issuer_Name)


# Reference table setup ----------------------------------------------------

income_pieces <- c("138% FPL or less", "138% FPL to 150% FPL", "150% FPL to 200% FPL",
                   "200% FPL to 250% FPL", "250% FPL to 300% FPL", "300% FPL to 400% FPL",
                   "400% FPL or greater")
income_breakpoints <- c(0, 1.38, 1.5, 2, 2.5, 3, 4)
max_imputation_size <- 5

# Pivot contribution_percentages and poverty_guidelines to long format
# so we can join on year instead of repeating 6x per year
contrib_long <- contribution_percentages %>%
  pivot_longer(-FPL, names_to = "year_col", values_to = "pct") %>%
  mutate(year = as.integer(sub("YR", "", year_col))) %>%
  select(fpl_bp = FPL, year, pct)

poverty_long <- poverty_guidelines %>%
  pivot_longer(-Family_Size, names_to = "year_col", values_to = "threshold") %>%
  mutate(year = as.integer(sub("YR", "", year_col))) %>%
  select(hh_size = Family_Size, year, threshold)

# Helper: compute contribution bounds vector for a given (year, hh_size)
get_bounds <- function(yr, sz) {
  yr_col <- paste0("YR", yr)
  contribution_percentages[[yr_col]] *
    poverty_guidelines[[yr_col]][poverty_guidelines$Family_Size == sz] / 12 *
    income_breakpoints
}

# Helper: assign subsidy_linear_piece via findInterval for indices, looping over years
assign_linear_piece <- function(df, idx, hh_size) {
  for (yr in 2014:2019) {
    bounds <- get_bounds(yr, hh_size)
    yr_idx <- intersect(idx, which(df$year == yr))
    if (length(yr_idx) > 0) {
      df$subsidy_linear_piece[yr_idx] <-
        income_pieces[findInterval(df$SLC_contribution[yr_idx], bounds)]
    }
  }
  df
}

# Helper: check if implied bracket matches reported bracket, handle special cases
check_income_match <- function(df, idx, hh_size) {
  # Combine 250-300 and 300-400 into "250% FPL to 400% FPL" for comparison
  implied <- df$subsidy_linear_piece[idx]
  implied[implied %in% c("250% FPL to 300% FPL", "300% FPL to 400% FPL")] <- "250% FPL to 400% FPL"
  df$implied_subsidy_bracket[idx] <- implied

  # Check exact match
  matched <- idx[which(df$subsidy_fpl_bracket[idx] == df$implied_subsidy_bracket[idx] &
                          !is.na(df$implied_subsidy_bracket[idx]))]
  df$income_check[matched] <- TRUE
  df$implied_household_size[matched] <- hh_size

  # Handle "400% FPL or less": any bracket below 400% is acceptable
  still_unmatched <- idx[which(!df$income_check[idx] & !is.na(df$income_check[idx]))]
  assign_400 <- still_unmatched[which(
    df$subsidy_fpl_bracket[still_unmatched] == "400% FPL or less" &
      df$implied_subsidy_bracket[still_unmatched] != "400% FPL or greater")]
  if (length(assign_400) > 0) {
    df <- assign_linear_piece(df, assign_400, hh_size)
    df$subsidy_fpl_bracket[assign_400] <- df$subsidy_linear_piece[assign_400]
    combine_idx <- assign_400[which(df$subsidy_fpl_bracket[assign_400] %in%
                                      c("300% FPL to 400% FPL", "250% FPL to 300% FPL"))]
    df$subsidy_fpl_bracket[combine_idx] <- "250% FPL to 400% FPL"
    df$income_check[assign_400] <- TRUE
    df$implied_household_size[assign_400] <- hh_size
  }

  # Handle "150% FPL or less": 138% or less and 138%-150% are acceptable
  assign_150 <- still_unmatched[which(
    df$subsidy_fpl_bracket[still_unmatched] == "150% FPL or less" &
      df$implied_subsidy_bracket[still_unmatched] %in%
      c("138% FPL or less", "138% FPL to 150% FPL"))]
  if (length(assign_150) > 0) {
    df <- assign_linear_piece(df, assign_150, hh_size)
    df$subsidy_fpl_bracket[assign_150] <- df$subsidy_linear_piece[assign_150]
    df$income_check[assign_150] <- TRUE
    df$implied_household_size[assign_150] <- hh_size
  }

  df
}

# Helper: join contribution bounds for FPL inversion
# Given df with (year, implied_household_size, subsidy_linear_piece),
# add fpl_LB, fpl_UB, perc_LB, perc_UB, poverty_threshold
join_subsidy_params <- function(df, idx) {
  # FPL bounds from income_breakpoints
  fpl_lb_lookup <- setNames(income_breakpoints, income_pieces[seq_along(income_breakpoints)])
  fpl_ub_lookup <- setNames(c(income_breakpoints[-1], NA), income_pieces[seq_along(income_breakpoints)])

  df$fpl_LB[idx] <- fpl_lb_lookup[df$subsidy_linear_piece[idx]]
  df$fpl_UB[idx] <- fpl_ub_lookup[df$subsidy_linear_piece[idx]]

  # Contribution percentages and poverty thresholds via long-format joins
  for (yr in 2014:2019) {
    yr_col <- paste0("YR", yr)
    yr_idx <- intersect(idx, which(df$year == yr))
    if (length(yr_idx) == 0) next

    # perc_LB: contribution percentage at lower FPL bound
    fpl_lb_vals <- df$fpl_LB[yr_idx]
    df$perc_LB[yr_idx] <- contribution_percentages[[yr_col]][
      match(fpl_lb_vals, contribution_percentages$FPL)]

    # perc_UB: contribution percentage at upper FPL bound
    fpl_ub_vals <- df$fpl_UB[yr_idx]
    df$perc_UB[yr_idx] <- contribution_percentages[[yr_col]][
      match(fpl_ub_vals, contribution_percentages$FPL)]

    # poverty_threshold
    hh_sizes <- df$implied_household_size[yr_idx]
    df$poverty_threshold[yr_idx] <- poverty_guidelines[[yr_col]][
      match(hh_sizes, poverty_guidelines$Family_Size)]
  }

  df
}


# Post-loop FPL reconciliation and group handling ---------------------------
#
# Shared logic for both singles and families after the reconciliation loop.
# 1. Assign subsidy_linear_piece for unmatched in subsidy range
# 2. Split "250% FPL to 400% FPL" probabilistically
# 3. Invert ACA subsidy formula for reconciled records
# 4. Classify groups 1/2/4, flag group 2
# 5. Sample FPL for group 1, recalculate contribution
# 6. Handle group 4 (>400% FPL, zero subsidy)
#
# Args:
#   df                 tibble with reconciliation variables
#   fpl_col            column name for FPL values ("subsidy_fpl_percent_int" or "FPL")
#   reconciled_idx     which() indices of successfully reconciled records
#   subsidy_range_idx  which() indices of records in subsidy range needing FPL
#
# Returns: list(df, group1, group2, group4)

reconcile_and_sample_fpl <- function(df, fpl_col, reconciled_idx, subsidy_range_idx) {
  is_singles <- (fpl_col == "subsidy_fpl_percent_int")

  # Initialize parameter columns
  df$fpl_LB <- df$fpl_UB <- df$perc_LB <- df$perc_UB <-
    df$poverty_threshold <- NA_real_

  # 1. Assign subsidy_linear_piece for unmatched in subsidy range
  unmatched_idx <- setdiff(subsidy_range_idx, reconciled_idx)
  df$subsidy_linear_piece[unmatched_idx] <-
    as.character(df$subsidy_fpl_bracket[unmatched_idx])

  # 2. Split "250% FPL to 400% FPL" probabilistically
  prob_250_400 <- table(df$subsidy_linear_piece[reconciled_idx])[
    c("250% FPL to 300% FPL", "300% FPL to 400% FPL")]
  prob_250_400 <- prob_250_400 / sum(prob_250_400)
  split_idx <- intersect(unmatched_idx,
                          which(df$subsidy_fpl_bracket == "250% FPL to 400% FPL"))
  if (length(split_idx) > 0) {
    df$subsidy_linear_piece[split_idx] <-
      sample(c("250% FPL to 300% FPL", "300% FPL to 400% FPL"),
             size = length(split_idx), prob = prob_250_400, replace = TRUE)
  }

  # 3. Join subsidy params and invert for reconciled records
  df <- join_subsidy_params(df, subsidy_range_idx)
  is_300_400 <- df$subsidy_linear_piece == "300% FPL to 400% FPL"
  df[[fpl_col]][reconciled_idx] <- invert_aca_subsidy(
    slc_contribution = df$SLC_contribution[reconciled_idx],
    perc_LB = df$perc_LB[reconciled_idx],
    perc_UB = df$perc_UB[reconciled_idx],
    fpl_LB = df$fpl_LB[reconciled_idx],
    fpl_UB = df$fpl_UB[reconciled_idx],
    poverty_threshold = df$poverty_threshold[reconciled_idx],
    bracket_300_400 = is_300_400[reconciled_idx]
  )

  # 4. Classify groups 1/2/4, flag group 2
  group1 <- which(!df$income_check & !is.na(df$income_check) &
                    !df$subsidy_fpl_bracket %in% c("150% FPL or less", "400% FPL or less"))
  if (is_singles) {
    group4 <- which(is.na(df[[fpl_col]]) &
                      df$subsidy_fpl_bracket == "400% FPL or greater")
  } else {
    group4 <- which((!df$income_check | is.na(df$income_check)) &
                      df$subsidy_fpl_bracket == "400% FPL or greater")
  }
  group2 <- setdiff(which(!df$income_check | is.na(df$income_check)),
                    c(group1, group4))
  df$flagged[group2] <- TRUE

  # 5. Sample FPL for group 1 from reconciled pool, recalculate contribution
  for (piece in income_pieces[1:(length(income_pieces) - 1)]) {
    to_sim <- intersect(group1, which(df$subsidy_linear_piece == piece))
    if (is_singles) {
      sampling_pool <- df[[fpl_col]][
        intersect(reconciled_idx, which(df$subsidy_linear_piece == piece))]
    } else {
      sampling_pool <- df[[fpl_col]][
        which(df$subsidy_linear_piece == piece & !is.na(df[[fpl_col]]))]
    }
    if (length(to_sim) > 0 && length(sampling_pool) > 0) {
      df[[fpl_col]][to_sim] <-
        sample(sampling_pool, size = length(to_sim), replace = TRUE)
    }
  }

  df <- join_subsidy_params(df, group1)
  is_300_400_g1 <- df$subsidy_linear_piece[group1] == "300% FPL to 400% FPL"
  df$SLC_contribution[group1] <- calculate_aca_contribution(
    fpl_pct = df[[fpl_col]][group1],
    perc_LB = df$perc_LB[group1],
    perc_UB = df$perc_UB[group1],
    fpl_LB = df$fpl_LB[group1],
    fpl_UB = df$fpl_UB[group1],
    poverty_threshold = df$poverty_threshold[group1],
    bracket_300_400 = is_300_400_g1
  )
  df$subsidy[group1] <- pmax(0, df$premiumSLC[group1] - df$SLC_contribution[group1])
  df$net_premium_amt_int[group1] <-
    pmax(0, df$gross_premium_amt_int[group1] - df$subsidy[group1])
  df$imputed_income_flag[group1] <- TRUE

  # 6. Handle group 4 (>400% FPL, zero subsidy)
  df$SLC_contribution[group4] <- NA_real_
  df$subsidy[group4] <- 0
  df$net_premium_amt_int[group4] <- df$gross_premium_amt_int[group4]

  list(df = df, group1 = group1, group2 = group2, group4 = group4)
}


# Compute SLC premium per market -------------------------------------------

product_cols <- setdiff(names(zip3_choices), c("Year", "zip3", "Region"))
zip3_long <- zip3_choices %>%
  as_tibble() %>%
  mutate(zip_region_year = rownames(zip3_choices)) %>%
  pivot_longer(cols = all_of(product_cols),
               names_to = "product", values_to = "available",
               values_drop_na = TRUE) %>%
  select(zip3, region = Region, year = Year, zip_region_year, product)

prod_defs <- product_definitions %>%
  as_tibble() %>%
  mutate(Product = rownames(product_definitions))

slc_by_market <- compute_slc_premium(zip3_long, prod_defs, plan_data)


# Rating factor and individual SLC premium ---------------------------------

enroll <- enroll %>%
  mutate(
    age_capped = pmin(64, age),
    rating_factor = ifelse(
      year >= 2018,
      age_rating_factors$Rating_Factor2018[match(age_capped, age_rating_factors$Age)],
      age_rating_factors$Rating_Factor[match(age_capped, age_rating_factors$Age)]
    )
  ) %>%
  select(-age_capped) %>%
  left_join(slc_by_market, by = c("zip3", "region", "year")) %>%
  mutate(premiumSLC = premiumSLC * rating_factor)


# =========================================================================
# SINGLES (members == 1)
# =========================================================================

singles <- enroll %>%
  filter(members == 1) %>%
  mutate(
    net_premium_amt_int = pmax(0, net_premium_amt_int),
    subsidy = pmax(0, gross_premium_amt_int - net_premium_amt_int),
    SLC_contribution = pmax(0, premiumSLC - subsidy)
  )

# Handle >400% FPL with positive subsidy
singles <- singles %>%
  mutate(
    net_premium_amt_int = ifelse(
      subsidy_fpl_bracket == "400% FPL or greater" &
        old_subsidy_fpl_bracket == "400% FPL or greater" &
        subsidy > 0 & !is.na(subsidy),
      gross_premium_amt_int, net_premium_amt_int),
    subsidy = gross_premium_amt_int - net_premium_amt_int,
    flagged = ifelse(
      subsidy_fpl_bracket == "400% FPL or greater" &
        old_subsidy_fpl_bracket != "400% FPL or greater" &
        subsidy > 0 & !is.na(subsidy),
      TRUE, flagged)
  )

# Zero-premium flag
singles <- singles %>%
  mutate(zero_premium = net_premium_amt_int <= 3 & !is.na(net_premium_amt_int))

# Initialize reconciliation variables
singles$subsidy_linear_piece <- NA_character_
singles$implied_subsidy_bracket <- NA_character_
singles$implied_household_size <- 1L
singles$income_check <- FALSE
singles$income_check[singles$subsidy == 0 | is.na(singles$subsidy)] <- NA
singles$income_check[!is.na(singles$subsidy_fpl_percent_int)] <- TRUE

# Reconciliation loop: try household sizes 1..5
for (s in 1:max_imputation_size) {
  to_process <- which(singles$implied_household_size == s &
                        !singles$income_check & !is.na(singles$income_check))
  if (length(to_process) == 0) next

  singles <- assign_linear_piece(singles, to_process, s)
  singles <- check_income_match(singles, to_process, s)

  # Increment unmatched to next size
  if (s < max_imputation_size) {
    still_unmatched <- which(singles$implied_household_size == s &
                               !singles$income_check & !is.na(singles$income_check))
    singles$implied_household_size[still_unmatched] <- s + 1L
  }
}

# Flag remaining problems
singles <- singles %>%
  mutate(
    flagged = ifelse(subsidy_fpl_bracket %in% c("150% FPL or less", "400% FPL or less"),
                     TRUE, flagged),
    imputed_income_flag = !income_check & !is.na(income_check) |
      implied_household_size > 2
  )

# Compute exact FPL for reconciled records ---------------------------------

reconciled_income <- which(is.na(singles$subsidy_fpl_percent_int) &
                             singles$income_check & !is.na(singles$income_check) &
                             !singles$flagged)
subsidy_income_range <- which(is.na(singles$subsidy_fpl_percent_int) &
                                !singles$subsidy_fpl_bracket %in% "400% FPL or greater")

result <- reconcile_and_sample_fpl(singles, "subsidy_fpl_percent_int",
                                    reconciled_income, subsidy_income_range)
singles <- result$df

# Singles-specific post-processing
singles$subsidy_eligible[reconciled_income] <- "Subsidy Eligible"
singles$implied_household_size[c(result$group1, result$group2, result$group4)] <- 1L
singles$subsidy_eligible[result$group1] <- "Subsidy Eligible"
singles$subsidy_eligible[result$group4] <- "Not Subsidy Elig"

# Catastrophic plans: gross = net
cat_idx <- which(singles$metal_level_enhanced == "Minimum Coverage")
singles$net_premium_amt_int[cat_idx] <- singles$gross_premium_amt_int[cat_idx]


# =========================================================================
# FAMILIES (members > 1)
# =========================================================================

# Build household-level summary
families <- enroll %>% filter(members > 1, !flagged)

households <- families %>%
  group_by(household_year) %>%
  summarize(
    household_id = first(household_id),
    year = first(year),
    members = n(),
    plans = n_distinct(plan_name),
    flagged = any(flagged),
    gross_premium_amt_int = pmax(0, first(gross_premium_amt_int)),
    net_premium_amt_int = pmax(0, first(net_premium_amt_int)),
    premium21 = first(premium21),
    premiumSLC = sum(premiumSLC, na.rm = TRUE),
    subsidy_fpl_bracket = first(subsidy_fpl_bracket),
    subsidy_fpl_percent_int = first(subsidy_fpl_percent_int),
    subsidized_members = sum(subsidy_eligible == "Subsidy Eligible", na.rm = TRUE),
    unsubsidized_members = sum(subsidy_eligible == "Not Subsidy Elig", na.rm = TRUE),
    region = first(region),
    zip3 = first(zip3),
    .groups = "drop"
  ) %>%
  mutate(
    subsidy = pmax(0, gross_premium_amt_int - net_premium_amt_int),
    SLC_contribution = pmax(0, premiumSLC - subsidy),
    zero_premium = net_premium_amt_int <= 10 & !is.na(net_premium_amt_int),
    zero_subsidy = gross_premium_amt_int == net_premium_amt_int &
      gross_premium_amt_int > 0
  )

# Track mixed subsidy households
mixed_subsidy_hh <- which(households$unsubsidized_members > 0 &
                            households$subsidized_members > 0)

# Initialize reconciliation variables
households$subsidy_linear_piece <- NA_character_
households$implied_subsidy_bracket <- NA_character_
households$implied_household_size <- households$members
households$income_check <- FALSE
households$income_check[!is.na(households$subsidy_fpl_percent_int)] <- TRUE
households$income_check[households$subsidy == 0 | is.na(households$subsidy)] <- NA
households$FPL <- as.numeric(households$subsidy_fpl_percent_int)

# Reconciliation loop: try household sizes from actual members up to max(members)
max_hh_size <- max(households$members)
for (s in 2:max_hh_size) {
  to_process <- which(households$implied_household_size == s &
                        !households$income_check & !is.na(households$income_check))
  if (length(to_process) == 0) {
    next
  }

  households <- assign_linear_piece(households, to_process, s)
  households <- check_income_match(households, to_process, s)

  # Increment unmatched to next size (up to max_imputation_size above actual)
  still_unmatched <- which(households$implied_household_size == s &
                             !households$income_check & !is.na(households$income_check))
  if (length(still_unmatched) > 0 && s < max_hh_size) {
    households$implied_household_size[still_unmatched] <-
      pmin(s + 1L, households$members[still_unmatched] + max_imputation_size)
  }
}

# Flag remaining problem households
households$flagged[intersect(mixed_subsidy_hh,
  which(!households$income_check & !is.na(households$income_check)))] <- TRUE
# Mixed subsidy households that DID reconcile: mark all as subsidized
reconciled_mixed <- intersect(mixed_subsidy_hh,
  which(households$income_check & !is.na(households$income_check)))
households$subsidized_members[reconciled_mixed] <- households$members[reconciled_mixed]
households$unsubsidized_members[reconciled_mixed] <- 0L

households <- households %>%
  mutate(
    flagged = ifelse(subsidy_fpl_bracket %in% c("150% FPL or less", "400% FPL or less"),
                     TRUE, flagged),
    imputed_income_flag = (!income_check & !is.na(income_check)) |
      (implied_household_size - members > 2)
  )

# Reset >400% FPL households
gt400 <- which(households$subsidy_fpl_bracket == "400% FPL or greater")
households$subsidy[gt400] <- NA_real_
households$income_check[gt400] <- NA
households$subsidy_linear_piece[gt400] <- NA_character_
households$net_premium_amt_int[gt400] <- households$gross_premium_amt_int[gt400]
households$subsidized_members[gt400] <- 0L
households$unsubsidized_members[gt400] <- households$members[gt400]

# Compute exact FPL for reconciled family households -----------------------

reconciled_income <- which(households$income_check & !is.na(households$income_check) &
                             !households$flagged & is.na(households$FPL))
subsidy_income_range <- which(!households$subsidy_fpl_bracket %in% "400% FPL or greater" &
                                is.na(households$FPL))

result <- reconcile_and_sample_fpl(households, "FPL",
                                    reconciled_income, subsidy_income_range)
households <- result$df

# Families-specific post-processing
households$implied_household_size[c(result$group1, result$group2, result$group4)] <-
  households$members[c(result$group1, result$group2, result$group4)]
households$subsidized_members[result$group4] <- 0L
households$unsubsidized_members[result$group4] <- households$members[result$group4]


# =========================================================================
# MERGE BACK TO ENROLLMENT
# =========================================================================

# Singles: port variables back
singles_export <- singles %>%
  select(individual_id, year,
         subsidy_fpl_percent_int, subsidy_fpl_bracket, subsidy_eligible,
         gross_premium_amt_int, net_premium_amt_int,
         premiumSLC, SLC_contribution, subsidy, rating_factor,
         implied_household_size, imputed_income_flag, flagged,
         subsidy_linear_piece, zero_premium)

# Families: port household variables to individual records
family_export <- families %>%
  select(individual_id, year, household_year) %>%
  left_join(
    households %>%
      select(household_year, FPL,
             hh_gross = gross_premium_amt_int, hh_net = net_premium_amt_int,
             hh_subsidy = subsidy, hh_SLC_contribution = SLC_contribution,
             hh_premiumSLC = premiumSLC,
             subsidy_fpl_bracket, subsidy_linear_piece,
             implied_household_size, imputed_income_flag,
             hh_flagged = flagged),
    by = "household_year"
  ) %>%
  mutate(
    subsidy_fpl_percent_int = FPL,
    gross_premium_amt_int = hh_gross,
    net_premium_amt_int = hh_net,
    subsidy = hh_subsidy,
    SLC_contribution = hh_SLC_contribution,
    premiumSLC = hh_premiumSLC,
    flagged = hh_flagged
  ) %>%
  select(individual_id, year,
         subsidy_fpl_percent_int, subsidy_fpl_bracket,
         gross_premium_amt_int, net_premium_amt_int,
         premiumSLC, SLC_contribution, subsidy,
         implied_household_size, imputed_income_flag, flagged,
         subsidy_linear_piece)

# Update enrollment with reconciled values
reconciled <- bind_rows(singles_export, family_export) %>%
  rename_with(~ paste0(.x, "_rc"), -c(individual_id, year))

enroll <- enroll %>%
  left_join(reconciled, by = c("individual_id", "year")) %>%
  mutate(
    subsidy_fpl_percent_int = coalesce(subsidy_fpl_percent_int_rc, subsidy_fpl_percent_int),
    subsidy_fpl_bracket = coalesce(subsidy_fpl_bracket_rc, subsidy_fpl_bracket),
    gross_premium_amt_int = coalesce(gross_premium_amt_int_rc, gross_premium_amt_int),
    net_premium_amt_int = coalesce(net_premium_amt_int_rc, net_premium_amt_int),
    premiumSLC = premiumSLC_rc,
    SLC_contribution = SLC_contribution_rc,
    subsidy = subsidy_rc,
    implied_household_size = implied_household_size_rc,
    imputed_income_flag = coalesce(imputed_income_flag_rc, FALSE),
    flagged = coalesce(flagged_rc, flagged),
    subsidy_linear_piece = subsidy_linear_piece_rc
  ) %>%
  select(-ends_with("_rc"))

# Also update subsidy_eligible for families from household reconciliation
enroll <- enroll %>%
  left_join(
    singles %>% select(individual_id, year, subsidy_eligible_s = subsidy_eligible),
    by = c("individual_id", "year")
  ) %>%
  mutate(subsidy_eligible = coalesce(subsidy_eligible_s, subsidy_eligible)) %>%
  select(-subsidy_eligible_s)

# Rating factor and zero_premium for non-family records
enroll <- enroll %>%
  left_join(
    singles %>% select(individual_id, year,
                        rating_factor_s = rating_factor,
                        zero_premium_s = zero_premium),
    by = c("individual_id", "year")
  ) %>%
  mutate(
    rating_factor = coalesce(rating_factor_s, rating_factor),
    zero_premium = coalesce(zero_premium_s, FALSE)
  ) %>%
  select(-rating_factor_s, -zero_premium_s)


# Save intermediate --------------------------------------------------------

# Clean up household object for export
households_out <- households %>%
  select(household_year, household_id, year, members, plans,
         gross_premium_amt_int, net_premium_amt_int, premium21,
         premiumSLC, subsidy, SLC_contribution,
         subsidy_fpl_bracket, subsidy_linear_piece, FPL,
         implied_household_size, imputed_income_flag,
         subsidized_members, unsubsidized_members,
         region, zip3, flagged)

fwrite(enroll, "data/output/enrollment_step4.csv")
fwrite(households_out, "data/output/households_step4.csv")
cat("Step 4 complete:", nrow(enroll), "records,",
    nrow(households_out), "family households.\n")
