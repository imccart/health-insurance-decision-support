# 2_aggregate-to-hh.R
# Aggregate cleaned individual enrollment to household-year level.
#
# Input:  data/output/enrollment_individual.csv (from step 1)
#         plan_data, age_rating_factors, poverty_guidelines loaded by _data-build.R
# Output: data/output/enrollment_hh.csv            (one row per HH-year)
#         data/output/enrollment_individual.csv    (overwritten: + household_year key,
#                                                   rating_factor, premiumSLC)
#
# Natural keys:
#   individual-level:  (individual_id, year)
#   HH-level:          (household_year) where household_year = paste(hh_case, year, split)
#                      hh_case = ahbx_case_id_x
#                      split   = 1..K when one ahbx_case_id has members with
#                                inconsistent (gross_premium, plan_name, subsidy)

cat("  Loading individual enrollment...\n")
enroll <- fread("data/output/enrollment_individual.csv") %>% as_tibble()


# Per-individual service-channel flags -------------------------------------
# CC service_channel codes map to channel categories:
#   CIA (Certified Insurance Agent)        → agent + broker
#   PBE (Plan-Based Enroller)              → broker (only)
#   SCR (Service Center Rep)               → navigator
#   CEC (Certified Enrollment Counselor)   → navigator
#   CEW (Certified Enrollment Worker)      → navigator
enroll <- enroll %>% mutate(
  agent     = as.integer(service_channel == "CIA"),
  broker    = as.integer(service_channel %in% c("CIA", "PBE")),
  navigator = as.integer(service_channel %in% c("SCR", "CEC", "CEW")),
  # Binary: is the member subsidy-eligible (138%–400% FPL)?
  is_subsidized = as.integer(subsidy_eligible == "Subsidy Eligible")
)


# Individual rating_factor + premiumSLC ------------------------------------
# CA age rating: factor from age_rating_factors, with separate curve for
# 2018+. Cap at age 64 (over-65 not offered exchange plans).
# premiumSLC = 2nd-lowest-silver premium (age-40) × individual rating_factor.
# We need SLC by (zip3, region, year) from plan_data × zip3_choices.
cat("  Computing individual rating_factor and premiumSLC...\n")

enroll <- enroll %>%
  mutate(age_capped = pmin(64L, age),
         rating_factor = ifelse(
           year >= 2018,
           age_rating_factors$Rating_Factor2018[match(age_capped, age_rating_factors$Age)],
           age_rating_factors$Rating_Factor[match(age_capped, age_rating_factors$Age)]
         )) %>%
  select(-age_capped)

# Second-lowest silver premium per (zip3, region, year).
# For each market, find silver plans that are (a) in plan_data and
# (b) offered via zip3_choices × product_definitions lookup, take the
# second-lowest Premium / RATING_FACTOR_AGE40 (= age-21 base premium).
cat("  Computing SLC benchmark per (zip3, region, year)...\n")

product_cols <- setdiff(colnames(zip3_choices), c("zip3", "Region", "Year"))
zip_products <- zip3_choices %>%
  as_tibble() %>%
  pivot_longer(all_of(product_cols), names_to = "product", values_to = "available") %>%
  filter(!is.na(available)) %>%
  select(zip3, region = Region, year = Year, product)

prod_defs <- product_definitions %>%
  as_tibble() %>%
  mutate(product = rownames(product_definitions))

silver_candidates <- zip_products %>%
  inner_join(prod_defs, by = "product") %>%
  inner_join(
    plan_data %>%
      filter(metal_level == "Silver") %>%
      select(HIOS, ENROLLMENT_YEAR, region,
             p_insurer = Issuer_Name, p_network = PLAN_NETWORK_TYPE,
             p_msp = MSP, p_netnum = Network_num,
             Premium),
    by = c("insurer" = "p_insurer", "plan_network_type" = "p_network",
           "region", "year" = "ENROLLMENT_YEAR"),
    relationship = "many-to-many"
  ) %>%
  filter(MSP == p_msp, is.na(p_netnum) | Network == p_netnum) %>%
  select(zip3, region, year, HIOS, Premium)

slc_by_market <- silver_candidates %>%
  group_by(zip3, region, year) %>%
  summarize(
    premiumSLC_base = {
      p <- sort(Premium / RATING_FACTOR_AGE40)
      if (length(p) >= 2) p[2] else if (length(p) == 1) p[1] else NA_real_
    },
    .groups = "drop"
  )

enroll <- enroll %>%
  left_join(slc_by_market, by = c("zip3", "region", "year")) %>%
  mutate(premiumSLC = premiumSLC_base * rating_factor) %>%
  select(-premiumSLC_base)


# HH grouping: identify (case_id, year, split_num) groups ------------------
# Some ahbx_case_ids lump together members that should be separate HHs.
# We split when members within the same case-year have different
# (gross_premium, plan_name, aptc) combos.
cat("  Identifying HH groupings (splitting mixed cases)...\n")

enroll <- enroll %>%
  mutate(
    aptc_amt_int = pmax(0, gross_premium_amt_int - net_premium_amt_int),
    hh_case_year = paste(ahbx_case_id_x, year, sep = "_")
  )

# For each case-year, identify unique (gross, plan_name, aptc) combos.
# Each unique combo → one household.
hh_splits <- enroll %>%
  distinct(hh_case_year, gross_premium_amt_int, plan_name, aptc_amt_int) %>%
  arrange(hh_case_year, gross_premium_amt_int, plan_name, aptc_amt_int) %>%
  group_by(hh_case_year) %>%
  mutate(split = row_number()) %>%
  ungroup()

enroll <- enroll %>%
  left_join(hh_splits,
            by = c("hh_case_year", "gross_premium_amt_int", "plan_name", "aptc_amt_int")) %>%
  mutate(household_year = paste(hh_case_year, split, sep = "_")) %>%
  select(-hh_case_year, -split)


# Fill missing APTC within HH ---------------------------------------------
# Same-HH members should share the APTC amount. If only some members have
# it (rest NA), fill NAs with the non-NA value.
enroll <- enroll %>%
  group_by(household_year) %>%
  mutate(aptc_amt_int = ifelse(is.na(aptc_amt_int),
                                first(na.omit(aptc_amt_int)),
                                aptc_amt_int)) %>%
  ungroup()


# Within-HH consistency check ----------------------------------------------
# A valid HH-year must have consistent: gross_premium, net_premium, FPL,
# bracket, zip3, region. If any vary within HH, flag and drop the HH.
cat("  Checking within-HH consistency...\n")

consistency <- enroll %>%
  group_by(household_year) %>%
  summarize(
    n_gross    = n_distinct(gross_premium_amt_int),
    n_net      = n_distinct(net_premium_amt_int),
    n_fpl      = n_distinct(subsidy_fpl_percent_int, na.rm = TRUE),
    n_bracket  = n_distinct(subsidy_fpl_bracket, na.rm = TRUE),
    n_zip      = n_distinct(zip3),
    n_region   = n_distinct(region),
    .groups    = "drop"
  ) %>%
  mutate(bad = n_gross > 1 | n_net > 1 | n_fpl > 1 | n_bracket > 1 |
                 n_zip > 1 | n_region > 1)

bad_hh <- consistency$household_year[consistency$bad]
cat(sprintf("  Dropping %d inconsistent HH-years (%.2f%%)\n",
            length(bad_hh), 100 * length(bad_hh) / nrow(consistency)))
enroll <- enroll %>% filter(!household_year %in% bad_hh)


# Aggregate to HH-year -----------------------------------------------------
cat("  Aggregating to HH-year...\n")

hh <- enroll %>%
  group_by(household_year) %>%
  summarize(
    # Keys / identifiers
    household_id   = first(ahbx_case_id_x),   # natural CC case key
    year           = first(year),

    # Core HH variables (consistent across members by construction)
    gross_premium_amt_int = first(gross_premium_amt_int),
    net_premium_amt_int   = first(net_premium_amt_int),
    aptc_amt_int          = first(aptc_amt_int),
    subsidy_fpl_bracket   = first(subsidy_fpl_bracket),
    FPL                   = max(subsidy_fpl_percent_int, na.rm = TRUE) / 100, # percent → ratio
    plan_id_HIOS          = first(HIOS),
    plan_unique_id        = paste0(first(HIOS), first(year)),  # matches plan_data$HIOSYR
    plan_name             = first(plan_name),
    metal                 = first(metal),
    metal_level_enhanced  = first(metal_level_enhanced),
    insurer               = first(insurer),
    plan_network_type     = first(plan_network_type),
    zip3                  = first(zip3),
    region                = first(region),
    OEP                   = first(OEP),

    # HH size + demographics
    household_size        = n(),
    oldest_member         = max(age, na.rm = TRUE),
    perc_0to17            = mean(age <= 17, na.rm = TRUE),
    perc_18to25           = mean(age >= 18 & age <= 25, na.rm = TRUE),
    perc_26to34           = mean(age >= 26 & age <= 34, na.rm = TRUE),
    perc_35to44           = mean(age >= 35 & age <= 44, na.rm = TRUE),
    perc_45to54           = mean(age >= 45 & age <= 54, na.rm = TRUE),
    perc_55to64           = mean(age >= 55 & age <= 64, na.rm = TRUE),
    perc_65plus           = mean(age >= 65, na.rm = TRUE),
    perc_male             = mean(gender, na.rm = TRUE),
    perc_white            = mean(race == "White", na.rm = TRUE),
    perc_black            = mean(race == "Black/African American", na.rm = TRUE),
    perc_hispanic         = mean(race == "Hispanic", na.rm = TRUE),
    perc_asian            = mean(race == "Asian", na.rm = TRUE),
    perc_other            = mean(race == "Other Race", na.rm = TRUE),

    # Sums across members (needed for premium/subsidy math)
    rating_factor   = sum(rating_factor, na.rm = TRUE),
    premiumSLC            = sum(premiumSLC, na.rm = TRUE),
    subsidized_members    = sum(is_subsidized, na.rm = TRUE),
    unsubsidized_members  = sum(1L - is_subsidized, na.rm = TRUE),

    # Channel: any member via that channel → HH has that channel
    agent                 = max(agent, na.rm = TRUE),
    broker                = max(broker, na.rm = TRUE),
    navigator             = max(navigator, na.rm = TRUE),

    # Language: English if any member speaks English; Spanish if any Spanish
    # and no English; Other if none speaks English or Spanish
    english               = as.integer(any(language_spoken == "English")),
    spanish               = as.integer(any(language_spoken == "Spanish") &
                                        !any(language_spoken == "English")),
    other_language        = as.integer(!any(language_spoken %in% c("English", "Spanish"))),

    .groups = "drop"
  ) %>%
  mutate(
    # Handle -Inf from max(NA) when all members missing FPL
    FPL = ifelse(is.infinite(FPL), NA_real_, FPL),

    # HH subsidy: trust observed (gross - net, clipped at 0)
    subsidy          = pmax(0, gross_premium_amt_int - net_premium_amt_int),
    # SLC contribution: what HH is "expected to pay" toward 2nd-lowest silver
    SLC_contribution = pmax(0, premiumSLC - subsidy),

    # Poverty threshold (by HH size and year) — for downstream affordability
    poverty_threshold = poverty_guidelines[[paste0("YR", pmin(year, 2019))]][
      match(household_size, poverty_guidelines$Family_Size)]
  )


# Save ---------------------------------------------------------------------
# Preserve individual rows (with household_year key + per-person rating_factor
# / premiumSLC). Some downstream demographic work needs individuals.
fwrite(enroll, "data/output/enrollment_individual.csv")
fwrite(hh,     "data/output/enrollment_hh.csv")
cat(sprintf("Step 2 complete: %d HH-years, %d individuals.\n",
            nrow(hh), nrow(enroll)))
