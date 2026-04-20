# 2_validate-geography-income.R
# Validate geographic choice sets and reconcile income/subsidy brackets.
#
# Input:  data/output/enrollment_step1.csv, reference CSVs
# Output: data/output/enrollment_step2.csv

# Load data ----------------------------------------------------------------

if (!exists("enroll")) enroll <- fread("data/output/enrollment_step1.csv")
enroll <- as_tibble(enroll)
plan_data <- read.csv("data/input/Covered California/plan_data.csv",
                       stringsAsFactors = FALSE)
product_definitions <- read.csv("data/input/Covered California/product_definitions.csv",
                                 stringsAsFactors = FALSE, row.names = 1)
zip3_choices <- read.csv("data/input/Covered California/zip3_choices.csv",
                          stringsAsFactors = FALSE, row.names = 1)

# Standardize plan_data insurer names
plan_data$Issuer_Name <- standardize_insurer(plan_data$Issuer_Name)


# Geographic validation ----------------------------------------------------
# Flag records whose zip/region combo doesn't appear in the choice set universe

valid_zip_regions <- paste(zip3_choices$zip3, zip3_choices$Region, sep = "_") %>% unique()
enroll <- enroll %>%
  mutate(flagged = ifelse(!zip_region %in% valid_zip_regions, TRUE, flagged))

# Check that the chosen plan is in the consumer's geographic choice set.
# Instead of the original's 100+ line loop, we pivot zip3_choices to long format,
# join to product_definitions, then join to plan_data to get all available plan IDs
# per zip3/region/year market. Then check enrollment against that set.

product_cols <- setdiff(names(zip3_choices), c("Year", "zip3", "Region"))

zip3_long <- zip3_choices %>%
  as_tibble() %>%
  mutate(zip_region_year = rownames(zip3_choices)) %>%
  pivot_longer(cols = all_of(product_cols),
               names_to = "product",
               values_to = "available",
               values_drop_na = TRUE) %>%
  select(zip3, Region, Year, zip_region_year, product)

# Build the available plans lookup via joins
prod_defs <- product_definitions %>%
  as_tibble() %>%
  mutate(product = rownames(product_definitions))

available_plans <- zip3_long %>%
  inner_join(prod_defs, by = "product") %>%
  inner_join(
    plan_data %>% select(plan_id_int = 1:ncol(plan_data)) %>%
      mutate(plan_id_int = row_number()) %>%
      bind_cols(plan_data %>% select(Issuer_Name, PLAN_NETWORK_TYPE, MSP,
                                      Network_num, region, ENROLLMENT_YEAR)),
    by = c("insurer" = "Issuer_Name",
           "plan_network_type" = "PLAN_NETWORK_TYPE",
           "Region" = "region",
           "Year" = "ENROLLMENT_YEAR"),
    relationship = "many-to-many"
  ) %>%
  # Handle MSP: Anthem has MSP and non-MSP variants
  filter(MSP.x == MSP.y) %>%
  # Handle SHARP's two networks
  filter(is.na(Network_num) | Network == Network_num) %>%
  distinct(zip_region_year, plan_id_int)

# Check each enrollment record
plan_in_choice_set <- enroll %>%
  select(zip_region_year, plan_id) %>%
  left_join(available_plans, by = c("zip_region_year", "plan_id" = "plan_id_int")) %>%
  mutate(in_set = !is.na(zip_region_year))

# Actually simpler: just check if (zip_region_year, plan_id) combo exists
valid_combos <- available_plans %>%
  transmute(combo = paste(zip_region_year, plan_id_int, sep = "|"))
enroll <- enroll %>%
  mutate(
    combo = paste(zip_region_year, plan_id, sep = "|"),
    plan_check = combo %in% valid_combos$combo,
    flagged = ifelse(!plan_check & !is.na(plan_check), TRUE, flagged)
  ) %>%
  select(-combo, -plan_check)


# Income validation --------------------------------------------------------

enroll <- enroll %>%
  mutate(
    subsidy_eligible = as.character(subsidy_eligible),
    subsidy_fpl_bracket = as.character(subsidy_fpl_bracket),
    old_subsidy_fpl_bracket = subsidy_fpl_bracket,

    # Fill in bracket from FPL percentage where bracket is missing/unavailable
    subsidy_fpl_bracket = case_when(
      subsidy_fpl_bracket %in% c("FPL Unavailable", "Unsubsidized Applica") &
        !is.na(subsidy_fpl_percent_int) & subsidy_fpl_percent_int > 0 &
        subsidy_fpl_percent_int <= 138 ~ "138% FPL or less",
      subsidy_fpl_bracket %in% c("FPL Unavailable", "Unsubsidized Applica") &
        !is.na(subsidy_fpl_percent_int) &
        between(subsidy_fpl_percent_int, 138.01, 150) ~ "138% FPL to 150% FPL",
      subsidy_fpl_bracket %in% c("FPL Unavailable", "Unsubsidized Applica") &
        !is.na(subsidy_fpl_percent_int) &
        between(subsidy_fpl_percent_int, 150.01, 200) ~ "150% FPL to 200% FPL",
      subsidy_fpl_bracket %in% c("FPL Unavailable", "Unsubsidized Applica") &
        !is.na(subsidy_fpl_percent_int) &
        between(subsidy_fpl_percent_int, 200.01, 250) ~ "200% FPL to 250% FPL",
      subsidy_fpl_bracket %in% c("FPL Unavailable", "Unsubsidized Applica") &
        !is.na(subsidy_fpl_percent_int) &
        between(subsidy_fpl_percent_int, 250.01, 400) ~ "250% FPL to 400% FPL",
      subsidy_fpl_bracket %in% c("FPL Unavailable", "Unsubsidized Applica") &
        !is.na(subsidy_fpl_percent_int) &
        subsidy_fpl_percent_int > 400 ~ "400% FPL or greater",
      TRUE ~ subsidy_fpl_bracket
    )
  )

# ACS (enhanced silver) plan consistency
acs_plans <- c("Silver - Enhanced 73", "Silver - Enhanced 87", "Silver - Enhanced 94")

enroll <- enroll %>%
  mutate(
    # Assign bracket from ACS plan for records with unavailable FPL
    subsidy_fpl_bracket = case_when(
      metal_level_enhanced == "Silver - Enhanced 73" &
        subsidy_fpl_bracket %in% c("FPL Unavailable", "Unsubsidized Applica") ~ "200% FPL to 250% FPL",
      metal_level_enhanced == "Silver - Enhanced 87" &
        subsidy_fpl_bracket %in% c("FPL Unavailable", "Unsubsidized Applica") ~ "150% FPL to 200% FPL",
      metal_level_enhanced == "Silver - Enhanced 94" &
        subsidy_fpl_bracket %in% c("FPL Unavailable", "Unsubsidized Applica") ~ "150% FPL or less",
      TRUE ~ subsidy_fpl_bracket
    )
  )

# Fix mismatched ACS plans: reassign metal_level_enhanced to match bracket
enroll <- enroll %>%
  mutate(
    wrong_acs = metal_level_enhanced %in% acs_plans & case_when(
      metal_level_enhanced == "Silver - Enhanced 73" ~ subsidy_fpl_bracket != "200% FPL to 250% FPL",
      metal_level_enhanced == "Silver - Enhanced 87" ~ subsidy_fpl_bracket != "150% FPL to 200% FPL",
      metal_level_enhanced == "Silver - Enhanced 94" ~ !subsidy_fpl_bracket %in%
        c("138% FPL or less", "138% FPL to 150% FPL", "150% FPL or less"),
      TRUE ~ FALSE
    ),
    metal_level_enhanced = case_when(
      wrong_acs & subsidy_fpl_bracket == "200% FPL to 250% FPL" ~ "Silver - Enhanced 73",
      wrong_acs & subsidy_fpl_bracket == "150% FPL to 200% FPL" ~ "Silver - Enhanced 87",
      wrong_acs & subsidy_fpl_bracket %in% c("138% FPL or less", "138% FPL to 150% FPL",
                                              "150% FPL or less") ~ "Silver - Enhanced 94",
      wrong_acs ~ "Silver",
      TRUE ~ metal_level_enhanced
    )
  ) %>%
  select(-wrong_acs)

# Assign >400% FPL for unsubsidized records where gross == net
enroll <- enroll %>%
  mutate(
    is_unsub_gross_eq_net = !is.na(net_premium_amt_int) &
      gross_premium_amt_int == net_premium_amt_int &
      subsidy_fpl_bracket %in% c("Unsubsidized Applica", "FPL Unavailable"),

    subsidy_fpl_bracket = ifelse(
      is_unsub_gross_eq_net & (subsidy_eligible != "Subsidy Eligible" | subsidy_fpl_bracket == "Unsubsidized Applica"),
      "400% FPL or greater", subsidy_fpl_bracket),
    subsidy_eligible = ifelse(
      is_unsub_gross_eq_net & subsidy_fpl_bracket == "400% FPL or greater",
      "Not Subsidy Elig", subsidy_eligible)
  ) %>%
  select(-is_unsub_gross_eq_net)

# Remaining "Unsubsidized Applica" with gross > net: mark as subsidized, <400%
enroll <- enroll %>%
  mutate(
    subsidy_eligible = ifelse(subsidy_fpl_bracket == "Unsubsidized Applica",
                              "Subsidy Eligible", subsidy_eligible),
    subsidy_fpl_bracket = ifelse(subsidy_fpl_bracket == "Unsubsidized Applica",
                                  "400% FPL or less", subsidy_fpl_bracket)
  )

# "FPL Unavailable" with gross > net: mark as subsidized, <400%
enroll <- enroll %>%
  mutate(
    has_subsidy_indicator = !is.na(gross_premium_amt_int) &
      gross_premium_amt_int > net_premium_amt_int &
      subsidy_fpl_bracket == "FPL Unavailable",
    subsidy_eligible = ifelse(has_subsidy_indicator, "Subsidy Eligible", subsidy_eligible),
    subsidy_fpl_bracket = ifelse(has_subsidy_indicator, "400% FPL or less", subsidy_fpl_bracket)
  ) %>%
  select(-has_subsidy_indicator)

# Any remaining "Subsidy Eligible" with FPL Unavailable: assign <400%
enroll <- enroll %>%
  mutate(
    subsidy_fpl_bracket = ifelse(
      subsidy_eligible == "Subsidy Eligible" & !is.na(subsidy_eligible) &
        subsidy_fpl_bracket == "FPL Unavailable",
      "400% FPL or less", subsidy_fpl_bracket)
  )

# No one >400% should be subsidized (gross > net)
enroll <- enroll %>%
  mutate(
    subsidy_fpl_bracket = ifelse(
      subsidy_eligible == "Subsidy Eligible" & !is.na(subsidy_eligible) &
        subsidy_fpl_bracket == "400% FPL or greater" &
        gross_premium_amt_int > net_premium_amt_int & !is.na(net_premium_amt_int),
      "400% FPL or less", subsidy_fpl_bracket),
    subsidy_eligible = ifelse(
      subsidy_eligible == "Subsidy Eligible" & !is.na(subsidy_eligible) &
        subsidy_fpl_bracket == "400% FPL or greater" &
        gross_premium_amt_int == net_premium_amt_int & !is.na(net_premium_amt_int),
      "Not Subsidy Elig", subsidy_eligible)
  )

# Zero FPL percent: set to NA unless in valid low-income bracket
enroll <- enroll %>%
  mutate(
    subsidy_fpl_percent_int = ifelse(
      subsidy_fpl_percent_int == 0 & !is.na(subsidy_fpl_percent_int) &
        !subsidy_fpl_bracket %in% c("138% FPL or less", "150% FPL or less"),
      NA_real_, subsidy_fpl_percent_int)
  )


# Save intermediate --------------------------------------------------------

fwrite(enroll, "data/output/enrollment_step2.csv")
cat("Step 2 complete:", nrow(enroll), "records.\n")
