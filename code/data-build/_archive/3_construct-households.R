# 3_construct-households.R
# Detect anomalous cases, split multi-household cases, assign household IDs.
#
# Input:  data/output/enrollment_step2.csv
# Output: data/output/enrollment_step3.csv

# Load data ----------------------------------------------------------------

if (!exists("enroll")) enroll <- fread("data/output/enrollment_step2.csv")
if (!is.data.table(enroll)) enroll <- as.data.table(enroll)

enroll[, `:=`(
  placed_in_household = FALSE,
  split_flag = FALSE,
  case_year = paste0(ahbx_case_id_x, year),
  aptc_amt_int = pmax(0, gross_premium_amt_int - net_premium_amt_int)
)]


# Case-level summary stats -------------------------------------------------

case_stats <- enroll[, .(
  n_members = .N,
  n_incomes = length(unique(aptc_amt_int)),
  n_plans   = length(unique(plan_name)),
  n_gross   = length(unique(gross_premium_amt_int)),
  n_net     = length(unique(net_premium_amt_int)),
  n_regions = length(unique(region)),
  n_zips    = length(unique(zip3))
), by = case_year]

problem_cases <- case_stats[
  n_incomes > 1 | n_zips > 1 | n_regions > 1 |
    n_net > 1 | n_gross > 1 | n_plans > 1, case_year]


# Fill missing income within cases -----------------------------------------
# Cases where income varies only because some members have NA:
# if only 2 distinct values and the case has 1 zip, 1 region, 1 gross, 1 net, 1 plan,
# fill the unique non-NA value across the case.

fill_candidates <- case_stats[
  n_incomes == 2 & n_zips == 1 & n_regions == 1 &
    n_gross == 1 & n_net == 1 & n_plans == 1, case_year]

fill_values <- enroll[case_year %chin% fill_candidates & !is.na(aptc_amt_int),
                      .(fill_aptc = aptc_amt_int[1]), by = case_year]

enroll[fill_values, aptc_amt_int := fill_aptc, on = "case_year"]

# Re-do FPL bracket fill (same as script 2 but on updated aptc)
enroll[, subsidy_fpl_bracket := fcase(
  subsidy_fpl_bracket %chin% c("FPL Unavailable", "Unsubsidized Applica") &
    !is.na(subsidy_fpl_percent_int) & subsidy_fpl_percent_int > 0 &
    subsidy_fpl_percent_int <= 138, "138% FPL or less",
  subsidy_fpl_bracket %chin% c("FPL Unavailable", "Unsubsidized Applica") &
    !is.na(subsidy_fpl_percent_int) & between(subsidy_fpl_percent_int, 138.01, 150), "138% FPL to 150% FPL",
  subsidy_fpl_bracket %chin% c("FPL Unavailable", "Unsubsidized Applica") &
    !is.na(subsidy_fpl_percent_int) & between(subsidy_fpl_percent_int, 150.01, 200), "150% FPL to 200% FPL",
  subsidy_fpl_bracket %chin% c("FPL Unavailable", "Unsubsidized Applica") &
    !is.na(subsidy_fpl_percent_int) & between(subsidy_fpl_percent_int, 200.01, 250), "200% FPL to 250% FPL",
  subsidy_fpl_bracket %chin% c("FPL Unavailable", "Unsubsidized Applica") &
    !is.na(subsidy_fpl_percent_int) & between(subsidy_fpl_percent_int, 250.01, 400), "250% FPL to 400% FPL",
  subsidy_fpl_bracket %chin% c("FPL Unavailable", "Unsubsidized Applica") &
    !is.na(subsidy_fpl_percent_int) & subsidy_fpl_percent_int > 400, "400% FPL or greater",
  rep(TRUE, .N), subsidy_fpl_bracket
)]


# Assign household IDs -----------------------------------------------------

hh_lookup <- enroll[, .(ahbx_case_id_x = unique(ahbx_case_id_x))]
hh_lookup[, household_id := .I]
enroll[hh_lookup, household_id := i.household_id, on = "ahbx_case_id_x"]
enroll[, household_year := paste(household_id, year, sep = "_")]
household_counter <- max(enroll$household_id)


# Round 1: Split cases with multiple premiums AND multiple plans -----------

hh_stats <- enroll[, .(
  n_gross = length(unique(gross_premium_amt_int)),
  n_plans = length(unique(plan_name))
), by = household_year]

split1 <- hh_stats[n_gross > 1 & n_plans > 1, household_year]

if (length(split1) > 0) {
  split1_set <- as.character(split1)
  enroll[, split_key := fifelse(
    household_year %chin% split1_set,
    paste(household_year, gross_premium_amt_int, plan_name, sep = "|"),
    NA_character_
  )]
  new_keys <- unique(na.omit(enroll$split_key))
  new_ids <- setNames(seq(household_counter + 1, household_counter + length(new_keys)), new_keys)
  enroll[!is.na(split_key), `:=`(
    household_id = new_ids[split_key],
    split_flag = TRUE
  )]
  enroll[, household_year := paste(household_id, year, sep = "_")]
  enroll[, split_key := NULL]
  household_counter <- max(enroll$household_id)
}


# Round 2: Split cases with multiple incomes AND multiple premiums ---------

hh_stats <- enroll[, .(
  n_incomes = length(unique(aptc_amt_int)),
  n_gross   = length(unique(gross_premium_amt_int))
), by = household_year]

split2 <- hh_stats[n_incomes >= 2 & n_gross > 1, household_year]

if (length(split2) > 0) {
  split2_set <- as.character(split2)
  enroll[, split_key := fifelse(
    household_year %chin% split2_set,
    paste(household_year, gross_premium_amt_int, aptc_amt_int, sep = "|"),
    NA_character_
  )]
  new_keys <- unique(na.omit(enroll$split_key))
  new_ids <- setNames(seq(household_counter + 1, household_counter + length(new_keys)), new_keys)
  enroll[!is.na(split_key), `:=`(
    household_id = new_ids[split_key],
    split_flag = TRUE
  )]
  enroll[, household_year := paste(household_id, year, sep = "_")]
  enroll[, split_key := NULL]
  household_counter <- max(enroll$household_id)
}


# Round 3: Flag remaining problem households -------------------------------

hh_stats <- enroll[, .(
  n_incomes = length(unique(aptc_amt_int)),
  n_plans   = length(unique(plan_name)),
  n_gross   = length(unique(gross_premium_amt_int)),
  n_net     = length(unique(net_premium_amt_int)),
  n_regions = length(unique(region)),
  n_zips    = length(unique(zip3))
), by = household_year]

# Households with only multiple plans (and nothing else wrong) are OK
any_problem <- hh_stats[n_incomes > 1 | n_zips > 1 | n_regions > 1 |
                          n_net > 1 | n_gross > 1 | n_plans > 1]
only_plans <- hh_stats[n_incomes == 1 & n_zips == 1 & n_regions == 1 &
                         n_net == 1 & n_gross == 1 & n_plans > 1]
remaining_problems <- setdiff(any_problem$household_year, only_plans$household_year)

enroll[household_year %chin% remaining_problems, flagged := TRUE]

# Ensure no one >400% gets subsidies
enroll[subsidy_fpl_bracket == "400% FPL or greater", subsidy_eligible := "Not Subsidy Elig"]

# Member counts
enroll[, members := .N, by = household_year]
enroll[, orig_members := .N, by = case_year]


# Save intermediate --------------------------------------------------------

fwrite(enroll, "data/output/enrollment_step3.csv")
cat("Step 3 complete:", nrow(enroll), "records,",
    length(unique(enroll$household_year)), "household-years.\n")
