# 1_clean-enrollment.R
# Clean individual-level Covered California enrollment data.
#
# Input:  data/input/Covered California/pra_07192019.csv
#         (reference tables already loaded by _data-build.R)
# Output: data/output/enrollment_individual.csv
#
# Key keys:
#   (individual_id, year) uniquely identifies a row (post mid-year dedup)
#   (HIOS, metal_level, year, region) uniquely identifies a plan in plan_data


# Load raw enrollment ------------------------------------------------------
cat("  Loading raw enrollment...\n")
enroll <- fread("data/input/Covered California/pra_07192019.csv") %>%
  as_tibble()


# Variable repair + renames ------------------------------------------------
cat("  Repairing variables...\n")
enroll <- enroll %>%
  rename(
    year          = enrlee_enrlmnt_yr,
    individual_id = indv_id_x,
    insurer       = issuer_name
  ) %>%
  mutate(
    flagged = FALSE,
    insurer = standardize_insurer(as.character(insurer)),

    # Gender → binary 1/0; blank → flag
    gender = as.character(gender),
    flagged = flagged | gender == "",
    gender  = fcase(gender == "Male", 1L, gender == "Female", 0L, default = NA_integer_),

    # Metal: fill from plan_name when blank. CSR-aware (keeps the
    # "Silver - Enhanced 73/87/94" granularity). Where downstream needs the
    # base tier (e.g. small-insurer collapse), derive inline.
    metal = as.character(metal_level_enhanced),
    metal = case_when(
      metal != ""                               ~ metal,
      str_detect(plan_name, "Bronze")           ~ "Bronze",
      str_detect(plan_name, "Silver.*94")       ~ "Silver - Enhanced 94",
      str_detect(plan_name, "Silver.*87")       ~ "Silver - Enhanced 87",
      str_detect(plan_name, "Silver.*73")       ~ "Silver - Enhanced 73",
      str_detect(plan_name, "Silver")           ~ "Silver",
      str_detect(plan_name, "Gold")             ~ "Gold",
      str_detect(plan_name, "Platinum")         ~ "Platinum",
      str_detect(plan_name, "Minimum Coverage") ~ "Minimum Coverage",
      TRUE ~ NA_character_
    ),
    flagged = flagged | is.na(metal),

    # Flag bad age / missing zip3
    flagged = flagged | is.na(age) | age > 120 | is.na(zip3),

    # Race → 5 canonical categories
    race = recode_values(race_ethnicity,
      "Latino"          ~ "Hispanic",
      "Asian"           ~ "Asian",
      "White"           ~ "White",
      "Black or Africa" ~ "Black/African American",
      default           = "Other Race"
    ),
    race = ifelse(is.na(race_ethnicity) | race_ethnicity == "", NA_character_, race),

    # Extract enrollment weeks (YYYYwNN format → integer NN)
    start_week = as.integer(substr(as.character(cov_start_dt_WK), 6, 7)),
    end_week   = as.integer(substr(as.character(cov_end_dt_WK),   6, 7))
  ) %>%
  select(-cov_start_dt_WK, -cov_end_dt_WK)


# Open Enrollment Period classification ------------------------------------
# OEP cutoff weeks by year (CC admin data; outside = SEP)
oep_cutoffs <- tribble(
  ~year, ~max_oep_week,
  2014L, 17L,  2015L, 9L,  2016L, 9L,
  2017L,  9L,  2018L, 8L,  2019L, 4L
)
enroll <- enroll %>%
  left_join(oep_cutoffs, by = "year") %>%
  mutate(OEP = start_week <= max_oep_week) %>%
  select(-max_oep_week)


# Dedupe mid-year transitions ----------------------------------------------
# (individual_id, year) PK. ~1400 records with >1 row = mid-year plan switches.
# Keep the row closest to January 1 (earliest start_week).
cat("  Deduping mid-year transitions...\n")
enroll <- enroll %>%
  arrange(individual_id, year, start_week) %>%
  distinct(individual_id, year, .keep_all = TRUE)


# Plan validation: must exist in plan_data AND be offered in HH's zip3 -----
# Plan PK in plan_data: (HIOS, metal_level, ENROLLMENT_YEAR, region)
# Availability: zip3_choices wide table, one col per product, indicates
# which products (insurer × network × MSP × network_num) are sold in
# each (zip3, region, year).
cat("  Validating plan availability...\n")

# Long-format zip3 × product availability
product_cols <- setdiff(colnames(zip3_choices), c("zip3", "Region", "Year"))
zip_product_long <- zip3_choices %>%
  as_tibble() %>%
  pivot_longer(all_of(product_cols), names_to = "product", values_to = "available") %>%
  filter(!is.na(available)) %>%
  select(zip3, region = Region, year = Year, product)

# product_definitions rows index the products (insurer / network / MSP / Net_num)
prod_defs <- product_definitions %>%
  as_tibble() %>%
  mutate(product = rownames(product_definitions))

# Join product × plan_data to list the valid (zip3, region, year, HIOS+metal) combos
valid_plans <- zip_product_long %>%
  inner_join(prod_defs, by = "product") %>%
  inner_join(
    plan_data %>%
      select(HIOS, metal_level, ENROLLMENT_YEAR, region,
             p_insurer = Issuer_Name, p_network = PLAN_NETWORK_TYPE,
             p_msp = MSP, p_netnum = Network_num),
    by = c("insurer"           = "p_insurer",
           "plan_network_type" = "p_network",
           "region",
           "year"              = "ENROLLMENT_YEAR"),
    relationship = "many-to-many"
  ) %>%
  filter(MSP == p_msp) %>%                          # MSP (for Anthem variants)
  filter(is.na(p_netnum) | Network == p_netnum) %>% # SHARP two-network case
  distinct(zip3, region, year, HIOS, metal_level)

# Enrollment join key: CC records the 16-char hios_id but plan_data has HIOS
# (10 chars for 2014-2015 non-SHARP; 14 chars for 2016+)
enroll <- enroll %>%
  mutate(
    hios_id_14 = substr(hios_id_16, 1, 14),
    HIOS = case_when(
      year %in% c(2014, 2015) & insurer != "SHARP" ~ substr(hios_id_14, 1, 10),
      TRUE                                         ~ hios_id_14
    )
  )

# Validate: does (zip3, region, year, HIOS, metal) appear in valid_plans?
enroll <- enroll %>%
  left_join(
    valid_plans %>% mutate(plan_valid = TRUE) %>%
      rename(metal = metal_level),
    by = c("zip3", "region", "year", "HIOS", "metal")
  ) %>%
  mutate(
    plan_valid = coalesce(plan_valid, FALSE),
    flagged    = flagged | !plan_valid
  ) %>%
  select(-plan_valid)


# FPL bracket fill from FPL% when bracket is missing/unknown ---------------
# Records where subsidy_fpl_bracket is "FPL Unavailable" / "Unsubsidized Applica"
# but subsidy_fpl_percent_int has a valid value: fill the bracket from that.
# Otherwise: flag & will be dropped.
cat("  Filling / validating FPL brackets...\n")
enroll <- enroll %>%
  mutate(
    subsidy_fpl_bracket = as.character(subsidy_fpl_bracket),
    # Fill bracket from FPL% when valid
    subsidy_fpl_bracket = case_when(
      !subsidy_fpl_bracket %in% c("FPL Unavailable", "Unsubsidized Applica") ~ subsidy_fpl_bracket,
      !is.na(subsidy_fpl_percent_int) & subsidy_fpl_percent_int > 0 &
        subsidy_fpl_percent_int <= 138   ~ "138% FPL or less",
      !is.na(subsidy_fpl_percent_int) & between(subsidy_fpl_percent_int, 138.01, 150) ~ "138% FPL to 150% FPL",
      !is.na(subsidy_fpl_percent_int) & between(subsidy_fpl_percent_int, 150.01, 200) ~ "150% FPL to 200% FPL",
      !is.na(subsidy_fpl_percent_int) & between(subsidy_fpl_percent_int, 200.01, 250) ~ "200% FPL to 250% FPL",
      !is.na(subsidy_fpl_percent_int) & between(subsidy_fpl_percent_int, 250.01, 400) ~ "250% FPL to 400% FPL",
      !is.na(subsidy_fpl_percent_int) & subsidy_fpl_percent_int > 400 ~ "400% FPL or greater",
      TRUE ~ NA_character_   # still unavailable → flag
    ),
    flagged = flagged | is.na(subsidy_fpl_bracket)
  )


# Drop flagged records -----------------------------------------------------
n_raw       <- nrow(enroll)
n_flagged   <- sum(enroll$flagged)
cat(sprintf("  Flagged %d / %d records (%.1f%%)\n",
            n_flagged, n_raw, 100 * n_flagged / n_raw))
enroll <- enroll %>% filter(!flagged) %>% select(-flagged)


# Health Net HSP network classification ------------------------------------
# CC quirk: some HN plans coded HMO but are actually HSP in specific regions
# and years. Correct to HSP.
hsp_regions         <- c(1, 3, 7, 11)
partial_hsp_regions <- 14:19
enroll <- enroll %>%
  rename(network_type = plan_network_type) %>%
  mutate(
    network_type = as.character(network_type),
    network_type = case_when(
      insurer == "Health_Net" & region %in% hsp_regions &
        year %in% c(2016, 2017) ~ "HSP",
      insurer == "Health_Net" & region %in% partial_hsp_regions &
        year %in% 2016:2019 & network_type == "HMO" &
        metal %in% c("Minimum Coverage", "Bronze") ~ "HSP",
      TRUE ~ network_type
    )
  ) %>%
  # Drop intermediate HIOS variants — keep only `HIOS` for step 2's
  # plan_id crosswalk; drop `metal_level_enhanced` (folded into `metal`).
  select(-hios_id_16, -hios_id_14, -metal_level_enhanced)


# Save ---------------------------------------------------------------------
fwrite(enroll, "data/output/enrollment_individual.csv")
cat(sprintf("Step 1 complete: %d individuals, %d HH-cases.\n",
            nrow(enroll), length(unique(paste0(enroll$ahbx_case_id_x, "_", enroll$year)))))
