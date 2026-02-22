# 1_load-clean-enrollment.R
# Load Covered California enrollment data and reference files.
# Standardize variables, repair missing values, create plan IDs,
# resolve ambiguous regions, and deduplicate mid-year transitions.
#
# Input:  data/input/Covered California/*.csv (8 files)
# Output: data/output/enrollment_step1.csv

set.seed(6)

# Load data ----------------------------------------------------------------

enroll <- fread("data/input/Covered California/pra_07192019.csv") %>% as_tibble()
plan_data <- read.csv("data/input/Covered California/plan_data.csv",
                       stringsAsFactors = FALSE)
product_definitions <- read.csv("data/input/Covered California/product_definitions.csv",
                                 stringsAsFactors = FALSE, row.names = 1)
zip3_choices <- read.csv("data/input/Covered California/zip3_choices.csv",
                          stringsAsFactors = FALSE, row.names = 1)
age_rating_factors <- read.csv("data/input/Covered California/age_rating_factors.csv",
                                stringsAsFactors = FALSE)
rating_areas <- read.csv("data/input/Covered California/rating_areas.csv",
                          stringsAsFactors = FALSE, row.names = 1)

# Insurer name standardization (uses INSURER_MAP from _helpers-enrollment.R)
plan_data$Issuer_Name <- standardize_insurer(plan_data$Issuer_Name)

# Variable repair ----------------------------------------------------------

enroll <- enroll %>%
  rename(
    year = enrlee_enrlmnt_yr,
    individual_id = indv_id_x,
    insurer = issuer_name
  ) %>%
  mutate(
    flagged = FALSE,

    # Insurer
    insurer = standardize_insurer(as.character(insurer)),

    # Gender (Male = 1, Female = 0)
    gender = as.character(gender),
    flagged = ifelse(gender == "", TRUE, flagged),
    gender = case_when(
      gender == "Male"   ~ "1",
      gender == "Female" ~ "0",
      TRUE ~ NA_character_
    ),

    # Metal level
    metal_level_enhanced = as.character(metal_level_enhanced),
    metal_level_enhanced = case_when(
      metal_level_enhanced != "" ~ metal_level_enhanced,
      str_detect(plan_name, "Bronze")           ~ "Bronze",
      str_detect(plan_name, "Silver.*94")        ~ "Silver - Enhanced 94",
      str_detect(plan_name, "Silver.*87")        ~ "Silver - Enhanced 87",
      str_detect(plan_name, "Silver.*73")        ~ "Silver - Enhanced 73",
      str_detect(plan_name, "Silver")            ~ "Silver",
      str_detect(plan_name, "Gold")              ~ "Gold",
      str_detect(plan_name, "Platinum")          ~ "Platinum",
      str_detect(plan_name, "Minimum Coverage")  ~ "Minimum Coverage",
      TRUE ~ "Silver - Enhanced 87"  # 1 residual record, checked by hand
    ),
    metal = case_when(
      metal_level_enhanced == "Minimum Coverage" ~ "Minimum Coverage",
      metal_level_enhanced == "Bronze"           ~ "Bronze",
      str_starts(metal_level_enhanced, "Silver") ~ "Silver",
      metal_level_enhanced == "Gold"             ~ "Gold",
      metal_level_enhanced == "Platinum"         ~ "Platinum"
    ),

    # Age flags
    flagged = ifelse(is.na(age) | age > 120, TRUE, flagged),

    # Zip3 flags
    flagged = ifelse(is.na(zip3), TRUE, flagged),

    # Race
    race = case_match(
      race_ethnicity,
      "Latino"          ~ "Hispanic",
      "Asian"           ~ "Asian",
      "White"           ~ "White",
      "Black or Africa" ~ "Black/African American",
      .default = "Other Race"
    ),
    race = ifelse(is.na(race_ethnicity) | race_ethnicity == "", NA_character_, race),

    # Service channel and language
    service_channel = as.character(service_channel),
    language_spoken = as.character(language_spoken),

    # Week variables (extract from YYYYwNN format)
    start_week = as.numeric(substr(as.character(cov_start_dt_WK), 6, 7)),
    end_week   = as.numeric(substr(as.character(cov_end_dt_WK), 6, 7))
  ) %>%
  select(-cov_start_dt_WK, -cov_end_dt_WK)

# OEP classification
oep_cutoffs <- tribble(
  ~year, ~max_oep_week,
  2014L, 17L,
  2015L,  9L,
  2016L,  9L,
  2017L,  9L,
  2018L,  8L,
  2019L,  4L
)
enroll <- enroll %>%
  left_join(oep_cutoffs, by = "year") %>%
  mutate(OEP = start_week <= max_oep_week) %>%
  select(-max_oep_week)


# Individual IDs and deduplication -----------------------------------------

# Renumber individuals 1..I
id_lookup <- enroll %>%
  distinct(individual_id) %>%
  mutate(individual_id_int = row_number())
enroll <- enroll %>%
  left_join(id_lookup, by = "individual_id") %>%
  mutate(individual_id = individual_id_int) %>%
  select(-individual_id_int)

# Drop mid-year transitions (keep first enrollment per individual-year)
enroll <- enroll %>%
  mutate(id_year = paste(individual_id, year, sep = "_")) %>%
  group_by(id_year) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  select(-id_year)


# Plan ID creation ---------------------------------------------------------

# Build the plan ID lookup: HIOS_metal_year_region
plan_data <- plan_data %>%
  mutate(
    plan_key = paste(HIOS, metal_level, ENROLLMENT_YEAR, region, sep = "_"),
    plan_id_int = row_number()
  )
plan_id_lookup <- setNames(plan_data$plan_id_int, plan_data$plan_key)

# For 2014-2015 (non-SHARP), HIOS is 10 chars; 2016+ is 14 chars
enroll <- enroll %>%
  mutate(
    hios_id_14 = substr(hios_id_16, 1, 14),
    plan_key = case_when(
      year %in% c(2014, 2015) & insurer != "SHARP" ~
        paste(substr(hios_id_14, 1, 10), metal_level_enhanced, year, region, sep = "_"),
      TRUE ~
        paste(hios_id_14, metal_level_enhanced, year, region, sep = "_")
    ),
    plan_id = unname(plan_id_lookup[plan_key])
  ) %>%
  select(-plan_key)


# Region fixes for NA plan_id ---------------------------------------------
# Some records have the wrong region, causing the plan_id lookup to fail.
# Fix deterministic cases first, then probabilistic cases.

# Deterministic: insurer operates in only one region
enroll <- enroll %>%
  mutate(region = case_when(
    insurer == "SHARP"  ~ 19L,
    insurer == "Valley" ~ 7L,
    TRUE ~ as.integer(region)
  ))

# Deterministic: specific insurer + zip3 combos
region_fixes <- tribble(
  ~insurer,             ~zip3, ~fix_region,
  "Chinese_Community",  940L,  8L,
  "Chinese_Community",  941L,  4L,
  "Chinese_Community",  944L,  8L,
  "Molina",             922L,  13L,
  "Oscar",              906L,  18L,
  "Anthem",             959L,  1L,
  "Anthem",             960L,  1L,
  "Anthem",             932L,  10L,
  "Anthem",             940L,  7L,
  "Anthem",             950L,  7L,
  "Anthem",             951L,  7L,
  "Anthem",             952L,  10L,
  "Anthem",             953L,  10L,
  "Anthem",             954L,  1L,
  "Anthem",             955L,  1L,
  "Kaiser",             954L,  2L,
  "Kaiser",             922L,  17L,
  "Kaiser",             935L,  15L,
  "Health_Net",         907L,  15L,
  "Health_Net",         913L,  16L,
  "Health_Net",         922L,  17L,
  "Health_Net",         935L,  15L
)

# Also need Western region fixes based on HIOS
western_region2 <- c("93689CA0110001", "93689CA0120001", "93689CA0110002",
                      "93689CA0120004", "93689CA0130002", "93689CA0120005")
western_region3 <- c("93689CA0150001", "93689CA0160001", "93689CA0150002",
                      "93689CA0160002", "93689CA0170001", "93689CA0160003")

# Rebuild plan_id after deterministic fixes
rebuild_plan_id <- function(df) {
  df %>% mutate(
    plan_key = case_when(
      year %in% c(2014, 2015) & insurer != "SHARP" ~
        paste(substr(hios_id_14, 1, 10), metal_level_enhanced, year, region, sep = "_"),
      TRUE ~
        paste(hios_id_14, metal_level_enhanced, year, region, sep = "_")
    ),
    plan_id = unname(plan_id_lookup[plan_key])
  ) %>%
    select(-plan_key)
}

enroll <- enroll %>%
  # Apply deterministic zip3-based fixes (only where plan_id is still NA)
  left_join(region_fixes, by = c("insurer", "zip3")) %>%
  mutate(region = ifelse(is.na(plan_id) & !is.na(fix_region), fix_region, region)) %>%
  select(-fix_region) %>%
  # Western HIOS-based fixes
  mutate(region = case_when(
    is.na(plan_id) & insurer == "Western" & hios_id_14 %in% western_region2 ~ 2L,
    is.na(plan_id) & insurer == "Western" & hios_id_14 %in% western_region3 ~ 3L,
    TRUE ~ region
  )) %>%
  rebuild_plan_id()

# Probabilistic: use observed distribution of resolved records
# Each entry: insurer, zip3, candidate regions
prob_fixes <- tribble(
  ~insurer,       ~zip3, ~candidates,
  "Kaiser",       940L,  list(c(7L, 8L)),
  "Kaiser",       945L,  list(c(2L, 5L, 6L)),
  "Kaiser",       950L,  list(c(7L, 9L)),
  "Kaiser",       906L,  list(c(15L, 18L)),
  "Health_Net",   906L,  list(c(15L, 18L)),
  "Health_Net",   917L,  list(c(15L, 17L)),
  "Health_Net",   928L,  list(c(17L, 18L)),
  "Blue_Shield",  906L,  list(c(15L, 18L)),
  "Blue_Shield",  913L,  list(c(12L, 16L)),
  "Blue_Shield",  917L,  list(c(15L, 17L)),
  "Blue_Shield",  922L,  list(c(13L, 17L)),
  "Blue_Shield",  928L,  list(c(17L, 18L)),
  "Blue_Shield",  932L,  list(c(10L, 11L, 14L)),
  "Blue_Shield",  935L,  list(c(13L, 14L, 15L, 17L)),
  "Blue_Shield",  940L,  list(c(7L, 8L)),
  "Blue_Shield",  945L,  list(c(2L, 5L, 6L)),
  "Blue_Shield",  950L,  list(c(7L, 9L)),
  "Blue_Shield",  954L,  list(c(1L, 2L)),
  "Blue_Shield",  961L,  list(c(1L, 3L))
) %>%
  mutate(candidates = map(candidates, ~ .x[[1]]))

for (j in seq_len(nrow(prob_fixes))) {
  ins <- prob_fixes$insurer[j]
  z3 <- prob_fixes$zip3[j]
  cands <- prob_fixes$candidates[[j]]

  # Compute observed distribution from resolved records
  resolved <- enroll %>%
    filter(!is.na(plan_id), insurer == ins, zip3 == z3) %>%
    count(region) %>%
    filter(region %in% cands)

  if (nrow(resolved) == 0) next

  obs_dist <- resolved$n / sum(resolved$n)

  # Fill unresolved records
  fill_idx <- which(is.na(enroll$plan_id) & enroll$insurer == ins & enroll$zip3 == z3)
  if (length(fill_idx) == 0) next

  set.seed(1)
  enroll$region[fill_idx] <- sample(resolved$region, length(fill_idx),
                                     prob = obs_dist, replace = TRUE)
  enroll$flagged[fill_idx] <- TRUE
}

# Final plan_id rebuild after all region fixes
enroll <- rebuild_plan_id(enroll)

# Flag records that still have no plan
enroll <- enroll %>%
  mutate(flagged = ifelse(is.na(plan_id), TRUE, flagged))

# Add plan-level variables
enroll <- enroll %>%
  mutate(
    premium21 = plan_data$Premium[plan_id] / 1.278,
    plan_name = plan_data$Plan_Name2[plan_id]
  )


# Health Net HMO vs HSP fix -----------------------------------------------

hsp_regions <- c(1, 3, 7, 11)
partial_hsp_regions <- 14:19

enroll <- enroll %>%
  mutate(
    plan_network_type = as.character(plan_network_type),
    plan_network_type = case_when(
      insurer == "Health_Net" & region %in% hsp_regions &
        year %in% c(2016, 2017) ~ "HSP",
      insurer == "Health_Net" & region %in% partial_hsp_regions &
        year %in% 2016:2019 & plan_network_type == "HMO" &
        metal %in% c("Minimum Coverage", "Bronze") ~ "HSP",
      TRUE ~ plan_network_type
    )
  )


# Composite keys -----------------------------------------------------------

enroll <- enroll %>%
  mutate(
    zip_region = paste(zip3, region, sep = "_"),
    zip_region_year = paste(zip3, region, year, sep = "_")
  )


# Save intermediate --------------------------------------------------------

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
fwrite(enroll, "data/output/enrollment_step1.csv")
cat("Step 1 complete:", nrow(enroll), "records written.\n")
