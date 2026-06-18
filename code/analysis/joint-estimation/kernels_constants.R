# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Description:   Shared constants for analysis scripts.

# Constants ---------------------------------------------------------------

AFFORD_THRESHOLDS <- c(
  "2014" = 0.0800, "2015" = 0.0805, "2016" = 0.0813,
  "2017" = 0.0816, "2018" = 0.0805, "2019" = 0.0830
)

# Plan-id list for plan-level fixed effects (post small-insurer collapse +
# Silver-CSR collapse via gsub). ANT_SIL is the reference (most common).
# Uninsured row → all PFE_* dummies = 0 by construction.
PLAN_FE_LEVELS <- c(
  "ANT_BR", "ANT_BR_HSA", "ANT_G", "ANT_P",
  "BS_BR",  "BS_BR_HSA",  "BS_G",  "BS_P",  "BS_SIL",
  "HN_BR",                "HN_G",  "HN_P",  "HN_SIL",
  "KA_BR",  "KA_BR_HSA",  "KA_G",  "KA_P",  "KA_SIL",
  "Small_BR",             "Small_G", "Small_P", "Small_SIL"
)
PLAN_FE_TERMS <- paste0("PFE_", PLAN_FE_LEVELS)
