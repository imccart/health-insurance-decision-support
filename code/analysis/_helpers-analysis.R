# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Description:   Shared constants and utility functions for analysis scripts.

# Constants ---------------------------------------------------------------

AFFORD_THRESHOLDS <- c(
  "2014" = 0.0800, "2015" = 0.0805, "2016" = 0.0813,
  "2017" = 0.0816, "2018" = 0.0956, "2019" = 0.0830
)

# Functions ---------------------------------------------------------------

derive_channel_vars <- function(hh) {
  hh %>%
    mutate(
      assisted  = if_else(navigator == 1 | broker == 1 | agent == 1, 1L, 0L),
      any_agent = if_else(broker == 1 | agent == 1, 1L, 0L),
      channel   = if_else(assisted == 1, "Assisted", "Unassisted"),
      channel_detail = case_when(
        navigator == 1            ~ "Navigator",
        broker == 1 | agent == 1  ~ "Agent",
        TRUE                      ~ "Unassisted"
      )
    )
}

derive_plan_characteristics <- function(hh, plan_data) {
  plan_lookup <- plan_data %>%
    distinct(HIOSYR, .keep_all = TRUE) %>%
    select(HIOSYR, insurer = Issuer_Name, plan_network_type = PLAN_NETWORK_TYPE,
           metal = metal_level)

  hh %>%
    left_join(plan_lookup, by = c("plan_unique_id" = "HIOSYR")) %>%
    mutate(
      metal = case_when(
        metal %in% c("Silver - Enhanced 73", "Silver - Enhanced 87",
                      "Silver - Enhanced 94") ~ "Silver",
        metal == "Minimum Coverage" ~ "Catastrophic",
        TRUE ~ metal
      )
    )
}
