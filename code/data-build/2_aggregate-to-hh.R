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
#   HH-level:          (household_id, year) where household_id = ahbx_case_id_x
#                      (persistent across years; same HH has same household_id
#                      across all enrolled years and across off-year synthetic
#                      rows constructed in step 4).
#                      household_year = paste(household_id, year, split) — the
#                      composite split-aware key, used for joining individuals.
#                      split = 1..K when one ahbx_case_id has members with
#                              inconsistent (gross_premium, plan_name, subsidy)
#
# Implementation: data.table throughout for memory + speed on the 8.5M-row
# individual table. dplyr is reserved for small reference tables (slc_by_market,
# poverty_guidelines_long, etc.).

cat("  Loading individual enrollment...\n")
enroll <- fread("data/output/enrollment_individual.csv")  # data.table


# Plan-id crosswalk --------------------------------------------------------
# Replace CC's descriptive plan_name with the canonical short-code plan_id
# from plan_data$Plan_Name2. Key (HIOS, year, metal) is unique in plan_data
# and matches enroll 1:1.
cat("  Attaching canonical plan_id from plan_data...\n")
pd_lookup <- as.data.table(plan_data)[, .(HIOS, year = ENROLLMENT_YEAR,
                                          metal = metal_level,
                                          plan_id = Plan_Name2)]
pd_lookup <- unique(pd_lookup, by = c("HIOS", "year", "metal"))
enroll[pd_lookup, on = c("HIOS", "year", "metal"), plan_id := i.plan_id]
n_unmatched <- sum(is.na(enroll$plan_id))
if (n_unmatched > 0) {
  cat(sprintf("    WARNING: %d enroll rows (%.2f%%) missing plan_id after crosswalk\n",
              n_unmatched, 100 * n_unmatched / nrow(enroll)))
}
enroll[, plan_name := NULL]
rm(pd_lookup)


# Per-individual service-channel flags -------------------------------------
# CC service_channel codes map to channel categories:
#   CIA (Certified Insurance Agent)        → agent + broker
#   PBE (Plan-Based Enroller)              → broker (only)
#   SCR (Service Center Rep)               → navigator
#   CEC (Certified Enrollment Counselor)   → navigator
#   CEW (Certified Enrollment Worker)      → navigator
enroll[, `:=`(
  agent         = as.integer(service_channel == "CIA"),
  broker        = as.integer(service_channel %in% c("CIA", "PBE")),
  navigator     = as.integer(service_channel %in% c("SCR", "CEC", "CEW")),
  is_subsidized = as.integer(subsidy_eligible == "Subsidy Eligible")
)]


# Individual rating_factor + premiumSLC ------------------------------------
# CA age rating: factor from age_rating_factors, with separate curve for
# 2018+. Cap at age 64 (over-65 not offered exchange plans).
# premiumSLC = 2nd-lowest-silver premium (age-40) × individual rating_factor.
cat("  Computing individual rating_factor and premiumSLC...\n")

age_capped <- pmin(64L, enroll$age)
enroll[, rating_factor := fifelse(
  year >= 2018,
  age_rating_factors$Rating_Factor2018[match(age_capped, age_rating_factors$Age)],
  age_rating_factors$Rating_Factor[match(age_capped, age_rating_factors$Age)]
)]
rm(age_capped)

# Second-lowest silver premium per (zip3, region, year).
# For each market, find silver plans that are (a) in plan_data and
# (b) offered via zip3_choices × product_definitions lookup, take the
# second-lowest Premium / RATING_FACTOR_AGE40 (= age-21 base premium).
cat("  Computing SLC benchmark per (zip3, region, year)...\n")

product_cols <- setdiff(colnames(zip3_choices), c("zip3", "Region", "Year"))
zip_products <- as_tibble(zip3_choices) %>%
  pivot_longer(all_of(product_cols), names_to = "product", values_to = "available") %>%
  filter(!is.na(available)) %>%
  select(zip3, region = Region, year = Year, product)

prod_defs <- as_tibble(product_definitions) %>%
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
  ) %>%
  as.data.table()

# DT join: pulls premiumSLC_base from slc_by_market into enroll
enroll[slc_by_market, on = c("zip3", "region", "year"),
       premiumSLC_base := i.premiumSLC_base]
enroll[, premiumSLC := premiumSLC_base * rating_factor]
enroll[, premiumSLC_base := NULL]
rm(slc_by_market, silver_candidates, zip_products, prod_defs, product_cols)


# HH grouping: identify (case_id, year, split_num) groups ------------------
# Some ahbx_case_ids lump together members that should be separate HHs.
# We split when members within the same case-year have different
# (gross_premium, plan_name, aptc) combos.
cat("  Identifying HH groupings (splitting mixed cases)...\n")

enroll[, `:=`(
  aptc_amt_int = pmax(0, gross_premium_amt_int - net_premium_amt_int),
  hh_case_year = paste(ahbx_case_id_x, year, sep = "_")
)]

# For each case-year, identify unique (gross, plan_id, aptc) combos.
# Each unique combo → one household.
hh_splits <- unique(enroll, by = c("hh_case_year", "gross_premium_amt_int",
                                    "plan_id", "aptc_amt_int"))[
  , .(hh_case_year, gross_premium_amt_int, plan_id, aptc_amt_int)]
setorder(hh_splits, hh_case_year, gross_premium_amt_int, plan_id, aptc_amt_int)
hh_splits[, split := seq_len(.N), by = hh_case_year]

enroll[hh_splits,
       on = c("hh_case_year", "gross_premium_amt_int", "plan_id", "aptc_amt_int"),
       split := i.split]

# Drop entire households (all years) that EVER had a multi-split case-year.
# A multi-split case-year = different members of the same case picked
# different (gross, plan, aptc) combos in some year. Even if only one year
# is ambiguous, the HH's decision-making is unclear, so we drop every year
# of every affected case. We have plenty of data; safer to drop than to
# imagine the HH is uninsured in their multi-split year (which is what the
# downstream CC-uninsured panel construction would otherwise infer).
multi_split_cases <- hh_splits[, .N, by = hh_case_year][N > 1, hh_case_year]
multi_split_case_ids <- unique(enroll[hh_case_year %in% multi_split_cases,
                                       ahbx_case_id_x])
n_dropped_rows <- sum(enroll$ahbx_case_id_x %in% multi_split_case_ids)
cat(sprintf("  Dropping %d cases (%d enrollment rows) with any multi-split year\n",
            length(multi_split_case_ids), n_dropped_rows))
enroll <- enroll[!ahbx_case_id_x %in% multi_split_case_ids]

enroll[, household_year := paste(hh_case_year, split, sep = "_")]
enroll[, c("hh_case_year", "split") := NULL]
rm(hh_splits, multi_split_cases, multi_split_case_ids, n_dropped_rows)


# Fill missing APTC within HH ---------------------------------------------
# Same-HH members should share the APTC amount. If only some members have
# it (rest NA), fill NAs with the non-NA value.
enroll[, aptc_amt_int := {
  if (anyNA(aptc_amt_int)) {
    nn <- aptc_amt_int[!is.na(aptc_amt_int)]
    if (length(nn) > 0) aptc_amt_int[is.na(aptc_amt_int)] <- nn[1]
  }
  aptc_amt_int
}, by = household_year]


# Within-HH consistency check ----------------------------------------------
# A valid HH-year must have consistent: gross_premium, net_premium, FPL,
# bracket, zip3, region. If any vary within HH, flag and drop the HH.
cat("  Checking within-HH consistency...\n")

consistency <- enroll[, .(
  n_gross   = length(unique(gross_premium_amt_int)),
  n_net     = length(unique(net_premium_amt_int)),
  n_fpl     = length(unique(subsidy_fpl_percent_int[!is.na(subsidy_fpl_percent_int)])),
  n_bracket = length(unique(subsidy_fpl_bracket[!is.na(subsidy_fpl_bracket)])),
  n_zip     = length(unique(zip3)),
  n_region  = length(unique(region))
), by = household_year]

consistency[, bad := n_gross > 1 | n_net > 1 | n_fpl > 1 | n_bracket > 1 |
                     n_zip > 1 | n_region > 1]

bad_hh <- consistency[bad == TRUE, household_year]
cat(sprintf("  Dropping %d inconsistent HH-years (%.2f%%)\n",
            length(bad_hh), 100 * length(bad_hh) / nrow(consistency)))

# Anti-join via DT: much faster than %in% with long bad_hh
bad_dt <- data.table(household_year = bad_hh, key = "household_year")
enroll <- enroll[!bad_dt, on = "household_year"]
rm(consistency, bad_hh, bad_dt)


# Aggregate to HH-year -----------------------------------------------------
# Chunked by year. Single-threaded data.table for the aggregation: with
# 30+ output cols sharing names with input cols, multi-thread writes hit
# "cannot change value of locked binding" race conditions.
cat("  Aggregating to HH-year...\n")

old_threads <- data.table::getDTthreads()
data.table::setDTthreads(1)

years <- sort(unique(enroll$year))
hh_chunks <- vector("list", length(years))
for (i in seq_along(years)) {
  yr <- years[i]
  cat(sprintf("    Year %d (%d/%d)...\n", yr, i, length(years)))
  d <- enroll[year == yr]
  hh_chunks[[i]] <- d[, .(
    household_id          = first(ahbx_case_id_x),
    year                  = first(year),
    gross_premium_amt_int = first(gross_premium_amt_int),
    net_premium_amt_int   = first(net_premium_amt_int),
    aptc_amt_int          = first(aptc_amt_int),
    subsidy_fpl_bracket   = first(subsidy_fpl_bracket),
    FPL                   = max(subsidy_fpl_percent_int, na.rm = TRUE) / 100,
    plan_id               = first(plan_id),
    metal                 = first(metal),
    insurer               = first(insurer),
    network_type          = first(network_type),
    zip3                  = first(zip3),
    region                = first(region),
    OEP                   = first(OEP),
    household_size        = .N,
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
    rating_factor         = sum(rating_factor, na.rm = TRUE),
    premiumSLC            = sum(premiumSLC, na.rm = TRUE),
    subsidized_members    = sum(is_subsidized, na.rm = TRUE),
    unsubsidized_members  = sum(1L - is_subsidized, na.rm = TRUE),
    agent                 = max(agent, na.rm = TRUE),
    broker                = max(broker, na.rm = TRUE),
    navigator             = max(navigator, na.rm = TRUE),
    english               = as.integer(any(language_spoken == "English")),
    spanish               = as.integer(any(language_spoken == "Spanish") &
                                       !any(language_spoken == "English")),
    other_language        = as.integer(!any(language_spoken %in% c("English", "Spanish")))
  ), by = household_year]
  rm(d)
  gc(verbose = FALSE)
}
hh <- rbindlist(hh_chunks)
rm(hh_chunks); gc(verbose = FALSE)
data.table::setDTthreads(old_threads)

# Cleanup post-aggregation
hh[is.infinite(FPL), FPL := NA_real_]
hh[, subsidy          := pmax(0, gross_premium_amt_int - net_premium_amt_int)]
hh[, SLC_contribution := pmax(0, premiumSLC - subsidy)]


# Poverty threshold (by HH size and year) — for downstream affordability
pov_dt <- as.data.table(poverty_guidelines_long)
hh[, year_cap := pmin(year, 2019L)]
hh[pov_dt, on = c("year_cap" = "year", "household_size" = "Family_Size"),
   poverty_threshold := i.poverty_threshold]
hh[, year_cap := NULL]
rm(pov_dt)


# Save ---------------------------------------------------------------------
# Preserve individual rows (with household_year key + per-person rating_factor
# / premiumSLC). Drop HIOS from individuals — plan_id is now canonical.
enroll[, HIOS := NULL]
fwrite(enroll, "data/output/enrollment_individual.csv")
fwrite(hh,     "data/output/enrollment_hh.csv")
cat(sprintf("Step 2 complete: %d HH-years, %d individuals.\n",
            nrow(hh), nrow(enroll)))

rm(enroll, hh); gc(verbose = FALSE)
