# 10_finalize-demand.R
# Final assembly: cheapest bronze, tax filing, penalties, broker/navigator
# channel variables, flag consistency, merge ACS + exchange, column selection.
#
# Input:  data/output/panel_individuals.csv, panel_households.csv (from step 9)
#         data/output/acs_individuals.csv, acs_households.csv (from step 7)
#         data/input/Covered California/ (plan_data, zip3_choices, product_definitions,
#         poverty_guidelines, age_rating_factors)
# Output: data/output/demand_individuals.csv, data/output/demand_households.csv

years <- 2014:2019


# Load data ----------------------------------------------------------------

cat("  Loading panel and ACS data...\n")
data <- fread("data/output/panel_individuals.csv") %>% as_tibble()
households <- fread("data/output/panel_households.csv") %>% as_tibble()

acs_indiv <- fread("data/output/acs_individuals.csv") %>% as_tibble()
acs_hh <- fread("data/output/acs_households.csv") %>% as_tibble()

plan_data <- read.csv("data/input/Covered California/plan_data.csv",
                       stringsAsFactors = FALSE)
product_definitions <- read.csv("data/input/Covered California/product_definitions.csv",
                                 stringsAsFactors = FALSE, row.names = 1)
zip3_choices <- read.csv("data/input/Covered California/zip3_choices.csv",
                          stringsAsFactors = FALSE, row.names = 1)
poverty_guidelines <- read.csv("data/input/Covered California/poverty_guidelines.csv",
                                stringsAsFactors = FALSE)

# Standardize insurer names
plan_data$Issuer_Name <- standardize_insurer(plan_data$Issuer_Name)


# Cheapest bronze premium --------------------------------------------------

cat("  Computing cheapest bronze premiums...\n")

# Prepare zipchoices matrix for get_available_plans() (from _helpers-demand.R)
zipchoices <- as.matrix(zip3_choices[, 4:ncol(zip3_choices)])

# Cheapest bronze per zip_region_year
zry_bronze <- data.frame(
  zry = rownames(zip3_choices),
  cheapest_bronze = sapply(rownames(zip3_choices), function(i) {
    avail <- get_available_plans(i, metal_filter = "Bronze")
    if (length(avail) == 0) return(NA_real_)
    min(plan_data$Premium[avail] / RATING_FACTOR_AGE40)
  }),
  stringsAsFactors = FALSE
)

# Individual cheapest premium = per-market bronze × individual rating factor
if (!"zip_region_year" %in% names(data)) {
  data$zip_region_year <- paste(data$zip3, data$rating_area, data$year, sep = "_")
}

data$cheapest_premium <- zry_bronze$cheapest_bronze[
  match(data$zip_region_year, zry_bronze$zry)] * data$rating_factor

# Household cheapest premium
hh_cheapest <- data %>%
  group_by(household_year) %>%
  summarize(cheapest_premium = sum(cheapest_premium, na.rm = TRUE), .groups = "drop")

households <- households %>%
  select(-any_of("cheapest_premium")) %>%
  left_join(hh_cheapest, by = "household_year")


# Tax unit type ------------------------------------------------------------

cat("  Determining tax filing status...\n")

# For exchange enrollees: infer from gender composition of adults
genders_adult <- data %>%
  filter(age >= 26) %>%
  group_by(household_year) %>%
  summarize(n_genders = n_distinct(gender), .groups = "drop")

married_hhy <- genders_adult %>% filter(n_genders == 2) %>% pull(household_year)
single_hhy <- households %>% filter(household_size == 1) %>% pull(household_year)

households <- households %>%
  mutate(
    tax_unit_type = case_when(
      household_year %in% single_hhy ~ "single",
      household_year %in% married_hhy ~ "married",
      TRUE ~ "household_head"
    )
  )


# Filing threshold ---------------------------------------------------------

filing_threshold <- tribble(
  ~year, ~single, ~household_head, ~married, ~widow_wchild,
  2014L, 10150,   13050,           20300,    16350,
  2015L, 10300,   13250,           20600,    16600,
  2016L, 10350,   13350,           20700,    16650,
  2017L, 10400,   16400,           20800,    16750,
  2018L, 12000,   24000,           18000,    24000,
  2019L, 12200,   24400,           18350,    24400
)

ft_long <- filing_threshold %>%
  pivot_longer(-year, names_to = "tax_unit_type", values_to = "filing_thresh")

households <- households %>%
  select(-any_of("filing_threshold")) %>%
  left_join(ft_long, by = c("year", "tax_unit_type")) %>%
  rename(filing_threshold = filing_thresh)


# Poverty threshold --------------------------------------------------------

rownames(poverty_guidelines) <- poverty_guidelines$Family_Size
households$poverty_threshold <- NA_real_
for (yr in years) {
  yr_col <- paste0("YR", min(yr, 2019))
  yr_idx <- which(households$year == yr)
  if (length(yr_idx) == 0) next
  households$poverty_threshold[yr_idx] <- poverty_guidelines[
    as.character(households$household_size[yr_idx]), yr_col]
}


# Household weight ---------------------------------------------------------

data$PERWT <- 1
hh_weights <- data %>%
  group_by(household_year) %>%
  summarize(weight = sum(PERWT), .groups = "drop")

households <- households %>%
  select(-any_of("weight")) %>%
  left_join(hh_weights, by = "household_year")


# Penalty calculation ------------------------------------------------------

cat("  Computing mandate penalties...\n")

# Penalty exempt status
data$penalty_exempt <- 0L
data$penalty_exempt_belowfiling <- 0L
data$penalty_exempt_unaffordable <- 0L

# Below filing threshold
below_filing_hhy <- households %>%
  filter(FPL * poverty_threshold < filing_threshold, !flagged) %>%
  pull(household_year)

data$penalty_exempt[data$household_year %in% below_filing_hhy & !data$flagged] <- 1L
data$penalty_exempt_belowfiling[data$household_year %in% below_filing_hhy] <- 1L

# Unaffordable offer
afford_thresholds <- c(`2014` = 0.08, `2015` = 0.0805, `2016` = 0.0813,
                       `2017` = 0.0816, `2018` = 0.0805, `2019` = 0.083)

unafford_sub <- households %>%
  filter(subsidized_members > 0, !flagged) %>%
  mutate(thresh = afford_thresholds[as.character(year)]) %>%
  filter((cheapest_premium - subsidy) * 12 > thresh * FPL * poverty_threshold) %>%
  pull(household_year)

unafford_unsub <- households %>%
  filter(subsidized_members == 0, !flagged) %>%
  mutate(thresh = afford_thresholds[as.character(year)]) %>%
  filter(cheapest_premium * 12 > thresh * FPL * poverty_threshold) %>%
  pull(household_year)

unafford_hhy <- c(unafford_sub, unafford_unsub)
data$penalty_exempt[data$household_year %in% unafford_hhy & !data$flagged] <- 1L
data$penalty_exempt_unaffordable[data$penalty_exempt == 1 &
                                    data$penalty_exempt_belowfiling == 0] <- 1L

# Household-level exemption flags
hh_below <- data %>%
  group_by(household_year) %>%
  summarize(exempt_belowfiling = max(penalty_exempt_belowfiling), .groups = "drop")
hh_unafford <- data %>%
  group_by(household_year) %>%
  summarize(exempt_unaffordable = max(penalty_exempt_unaffordable), .groups = "drop")

households <- households %>%
  left_join(hh_below, by = "household_year") %>%
  left_join(hh_unafford, by = "household_year") %>%
  rename(exempt.belowfiling = exempt_belowfiling,
         exempt.unaffordable = exempt_unaffordable)

# Count adults/children subject to mandate
subject_counts <- data %>%
  filter(penalty_exempt == 0, !flagged) %>%
  mutate(is_child = age < 18) %>%
  group_by(household_year) %>%
  summarize(
    num_adults_subject = sum(!is_child),
    num_children_subject = sum(is_child),
    .groups = "drop"
  )

households <- households %>%
  left_join(subject_counts, by = "household_year") %>%
  mutate(
    num_adults_subject = coalesce(num_adults_subject, 0L),
    num_children_subject = coalesce(num_children_subject, 0L)
  )

# Penalty parameters by year
flat_amount <- c(`2014` = 95, `2015` = 325, `2016` = 695, `2017` = 695,
                 `2018` = 695, `2019` = 0)
perc_penalty <- c(`2014` = 0.01, `2015` = 0.02, `2016` = 0.025, `2017` = 0.025,
                  `2018` = 0.025, `2019` = 0)
penalty_cap <- c(`2014` = 204, `2015` = 207, `2016` = 223, `2017` = 272,
                 `2018` = 283, `2019` = 0) * 12

households <- households %>%
  mutate(
    flat_yr = flat_amount[as.character(year)],
    perc_yr = perc_penalty[as.character(year)],
    cap_yr = penalty_cap[as.character(year)],
    penalty = ifelse(flagged, NA_real_,
      pmin(
        pmax(
          pmin((num_adults_subject + 0.5 * num_children_subject) * flat_yr,
               3 * flat_yr),
          perc_yr * (FPL * poverty_threshold - filing_threshold)
        ),
        (num_adults_subject + num_children_subject) * cap_yr
      )
    )
  ) %>%
  select(-flat_yr, -perc_yr, -cap_yr)


# Broker/navigator channel variables ---------------------------------------

cat("  Computing channel variables...\n")

data$agent <- as.integer(data$service_channel %in% "CIA" & !is.na(data$plan_name))
data$broker <- as.integer(data$service_channel %in% c("CIA", "PBE") & !is.na(data$plan_name))
data$navigator <- as.integer(data$service_channel %in% c("SCR", "CEW", "CEC") & !is.na(data$plan_name))

# Household-level current year
hh_channels <- data %>%
  filter(!flagged) %>%
  group_by(household_year) %>%
  summarize(
    agent = max(agent, na.rm = TRUE),
    broker = max(broker, na.rm = TRUE),
    navigator = max(navigator, na.rm = TRUE),
    .groups = "drop"
  )

households <- households %>%
  select(-any_of(c("agent", "broker", "navigator"))) %>%
  left_join(hh_channels, by = "household_year")

# Forward and ever variables
households$forward_agent <- 0L
households$forward_broker <- 0L
households$forward_navigator <- 0L
households$ever_agent <- 0L
households$ever_broker <- 0L
households$ever_navigator <- 0L

for (t in years) {
  agent_hh_ids <- households %>%
    filter(year == t, agent == 1, !is.na(agent), !flagged) %>%
    pull(household_id)
  broker_hh_ids <- households %>%
    filter(year == t, broker == 1, !is.na(broker), !flagged) %>%
    pull(household_id)
  navigator_hh_ids <- households %>%
    filter(year == t, navigator == 1, !is.na(navigator), !flagged) %>%
    pull(household_id)

  if (t < max(years)) {
    households$forward_agent[households$household_id %in% agent_hh_ids &
                               households$year >= t] <- 1L
    households$forward_broker[households$household_id %in% broker_hh_ids &
                                households$year >= t] <- 1L
    households$forward_navigator[households$household_id %in% navigator_hh_ids &
                                   households$year >= t] <- 1L
  }

  households$ever_agent[households$household_id %in% agent_hh_ids] <- 1L
  households$ever_broker[households$household_id %in% broker_hh_ids] <- 1L
  households$ever_navigator[households$household_id %in% navigator_hh_ids] <- 1L
}


# Flag consistency ---------------------------------------------------------

cat("  Ensuring flag consistency...\n")
flagged_hhy <- data %>%
  group_by(household_year) %>%
  summarize(any_flagged = any(flagged, na.rm = TRUE), .groups = "drop") %>%
  filter(any_flagged) %>%
  pull(household_year)

households$flagged[households$household_year %in% flagged_hhy] <- TRUE
data$flagged <- households$flagged[match(data$household_year, households$household_year)]


# Standardize ACS columns --------------------------------------------------

cat("  Standardizing ACS columns...\n")

# Rename ACS columns to match exchange
acs_indiv <- acs_indiv %>%
  rename(age = AGE, weight = PERWT)

# Ensure consistent column sets for merge
acs_indiv$plan_id <- NA_integer_
acs_indiv$plan_name <- NA_character_
acs_indiv$plan_number <- NA_integer_
acs_indiv$plan_number_nocsr <- NA_integer_
acs_indiv$plan_number_small <- NA_integer_
acs_indiv$service_channel <- NA_character_
acs_indiv$language_spoken <- NA_character_

data$weight <- 1
data$penalty_exempt <- data$penalty_exempt


# Save outputs -------------------------------------------------------------

cat("  Saving final demand data...\n")
fwrite(data, "data/output/demand_individuals.csv")
fwrite(households, "data/output/demand_households.csv")

# Also save ACS separately for reference
fwrite(acs_indiv, "data/output/demand_acs_individuals.csv")
fwrite(acs_hh, "data/output/demand_acs_households.csv")

cat("Step 10 complete:", nrow(data), "exchange individuals,",
    nrow(households), "exchange households,",
    nrow(acs_indiv), "ACS individuals,",
    nrow(acs_hh), "ACS households.\n")
