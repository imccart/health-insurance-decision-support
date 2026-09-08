# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-24
## Description:   Process CPRA broker/agent data into region-year panel.
##                Source: Covered California FOIA (RatingRegionAgentEnrollment).
##                One row per agent x region in each year sheet. Region-year
##                population from the Census county estimates (data/input/
##                co-est2019-alldata.csv), counties mapped to rating regions
##                via rating_areas; Los Angeles County is split between
##                regions 15 and 16 by each region's enrolled-member share.
##                Output: data/output/broker_density.csv (n_agents, enrollee
##                totals, HHI, population, agents_per_10k)

library(readxl)

# =========================================================================
# Read and stack all year sheets
# =========================================================================

broker_file <- "data/input/Covered California/RatingRegionAgentEnrollment_from_CY2014_to_CY2019__20260316.xlsx"

all_years <- list()

for (yr in 2014:2019) {
  sheet <- paste0("CY_", yr)
  d <- read_excel(broker_file, sheet = sheet, skip = 1,
                  col_names = c("region", "agent_name", "agent_license",
                                "business_name", "total_enrollees"))
  d$year <- as.integer(yr)
  d$region <- suppressWarnings(as.integer(d$region))
  d$total_enrollees <- suppressWarnings(as.numeric(d$total_enrollees))

  # Drop header remnants and NAs
  d <- d %>% filter(!is.na(region), !is.na(total_enrollees))
  all_years[[sheet]] <- d
}

broker_raw <- bind_rows(all_years)

cat("Raw broker data:", nrow(broker_raw), "rows\n")
cat("  Years:", paste(sort(unique(broker_raw$year)), collapse = ", "), "\n")
cat("  Regions:", paste(sort(unique(broker_raw$region)), collapse = ", "), "\n")
cat("  Unique agents:", length(unique(broker_raw$agent_license)), "\n")

# =========================================================================
# Aggregate to region-year panel
# =========================================================================

broker_density <- broker_raw %>%
  group_by(region, year) %>%
  summarize(
    n_agents = length(unique(agent_license)),
    total_broker_enrollees = sum(total_enrollees, na.rm = TRUE),
    top_agent_enrollees = max(total_enrollees, na.rm = TRUE),
    top_agent_share = max(total_enrollees, na.rm = TRUE) / sum(total_enrollees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(region, year)

# HHI of agent market shares within each region-year
broker_hhi <- broker_raw %>%
  group_by(region, year) %>%
  mutate(agent_share = total_enrollees / sum(total_enrollees, na.rm = TRUE)) %>%
  summarize(
    broker_hhi = sum(agent_share^2, na.rm = TRUE),
    .groups = "drop"
  )

broker_density <- broker_density %>%
  left_join(broker_hhi, by = c("region", "year"))

# =========================================================================
# Region-year population (Census county estimates)
# =========================================================================

census_pop <- read.csv("data/input/co-est2019-alldata.csv",
                       stringsAsFactors = FALSE) %>%
  filter(STNAME == "California", COUNTY != 0) %>%
  mutate(county = sub(" County$", "", CTYNAME)) %>%
  select(county, starts_with("POPESTIMATE201")) %>%
  pivot_longer(-county, names_to = "year", names_prefix = "POPESTIMATE",
               names_transform = list(year = as.integer), values_to = "pop") %>%
  filter(year >= 2014, year <= 2019)

# County -> region from the preamble's rating_areas (rownames = county;
# "Los Angeles1"/"Los Angeles2" are regions 15/16)
county_region <- tibble(county = rownames(rating_areas),
                        region = as.integer(str_extract(rating_areas$Region, "\\d+"))) %>%
  mutate(county = sub("[12]$", "", county))

# Non-LA counties map 1:1 to a region
pop_region <- census_pop %>%
  filter(county != "Los Angeles") %>%
  inner_join(county_region %>% filter(county != "Los Angeles") %>% distinct(),
             by = "county") %>%
  group_by(region, year) %>%
  summarize(population = sum(pop), .groups = "drop")

# LA County population split between regions 15/16 by enrolled-member share
la_share <- fread("data/output/enrollment_hh.csv",
                  select = c("region", "year", "household_size")) %>%
  as_tibble() %>%
  filter(region %in% c(15, 16)) %>%
  group_by(region, year) %>%
  summarize(members = sum(household_size), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = members / sum(members)) %>%
  ungroup()

pop_la <- census_pop %>%
  filter(county == "Los Angeles") %>%
  inner_join(la_share %>% select(region, year, share), by = "year",
             relationship = "many-to-many") %>%
  mutate(population = pop * share) %>%
  select(region, year, population)

pop_region <- bind_rows(pop_region, pop_la) %>% arrange(region, year)
cat("\nRegion-year population panel:", nrow(pop_region), "rows;",
    "statewide 2019:", round(sum(pop_region$population[pop_region$year == 2019]) / 1e6, 2), "M\n")

broker_density <- broker_density %>%
  left_join(pop_region, by = c("region", "year")) %>%
  mutate(agents_per_10k = n_agents / population * 10000)

cat("\nBroker density panel:", nrow(broker_density), "rows\n")
cat("  Agents per region-year:\n")
print(summary(broker_density$n_agents))
cat("  Broker HHI:\n")
print(summary(broker_density$broker_hhi))

# =========================================================================
# Write output
# =========================================================================

write_csv(broker_density, "data/output/broker_density.csv")
