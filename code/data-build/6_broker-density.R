# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-24
## Description:   Process CPRA broker/agent data into region-year panel.
##                Source: Covered California FOIA (RatingRegionAgentEnrollment).
##                One row per agent x region in each year sheet.
##                Output: data/output/broker_density.csv

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

cat("\nBroker density panel:", nrow(broker_density), "rows\n")
cat("  Agents per region-year:\n")
print(summary(broker_density$n_agents))
cat("  Broker HHI:\n")
print(summary(broker_density$broker_hhi))

# =========================================================================
# Write output
# =========================================================================

write_csv(broker_density, "data/output/broker_density.csv")
cat("\n  -> data/output/broker_density.csv\n")
