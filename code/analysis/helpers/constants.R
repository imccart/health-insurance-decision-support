# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Description:   Shared constants for analysis scripts.

# Constants ---------------------------------------------------------------

AFFORD_THRESHOLDS <- c(
  "2014" = 0.0800, "2015" = 0.0805, "2016" = 0.0813,
  "2017" = 0.0816, "2018" = 0.0805, "2019" = 0.0830
)

# Minimum within-cell predicted share for a plan-cell's pricing FOC to enter the
# cost-side M3 moments and the MC-FOC diagnostic. Below this the share is a handful
# of sampled enrollees and the FOC-implied markup is sampling noise (the markup
# inversion is ill-conditioned as share -> 0). Plans below the floor stay in the
# demand choice set, in their cell's Omega (cross-effects on other plans' FOCs are
# retained), and in the M1/M2 risk/claims moments; only their own degenerate FOC
# equation is dropped. At 0.002 (~a handful of buyers in an average cell) the
# extreme markups vanish (max retained ~$700 vs tens of thousands below it).
SHARE_FLOOR_FOC <- 0.002

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
