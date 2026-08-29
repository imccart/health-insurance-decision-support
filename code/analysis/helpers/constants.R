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

# Government cost of the uninsured (counterfactual scoring, score_cf.R).
# Coughlin, Holahan, Caswell, and McGrath (2014): government-paid medical costs
# for the nonelderly uninsured, $2,025 per uninsured person in 2013. Inflated to
# each study year by per-capita national health expenditures (CMS NHE Accounts;
# Table 1, per-capita national health expenditures in current dollars, December 2025 release of the 2024 historical accounts).
UC_UNINSURED_2013 <- 2025
NHE_PER_CAPITA <- c("2013" = 9024, "2014" = 9421, "2015" = 9860, "2016" = 10229, "2017" = 10582, "2018" = 11042, "2019" = 11487)
UC_PER_UNINSURED <- UC_UNINSURED_2013 * NHE_PER_CAPITA / NHE_PER_CAPITA[["2013"]]

# HHS risk adjustment transfer formula (Pope et al. 2014; 2014 Payment Notice):
# induced demand factors by metal actuarial value, the geographic cost factors
# by rating region and benefit year (CMS summary reports, individual market), and
# the reduction of the statewide average premium for administrative costs from
# benefit year 2018.
RA_IDF_BY_AV <- c("0.6" = 1.00, "0.7" = 1.03, "0.8" = 1.08, "0.9" = 1.15)
RA_ADMIN_SHARE <- c("2014" = 0, "2015" = 0, "2016" = 0, "2017" = 0, "2018" = 0.14, "2019" = 0.14)
RA_GCF <- read.csv("data/input/cms_gcf_california.csv", stringsAsFactors = FALSE)
# Statewide averages CMS used in the transfer formula (summary reports, CA
# individual market): the statewide average premium per billable member-month,
# already net of the administrative-cost share from 2018, is Pbar.
RA_STATE_CMS <- read.csv("data/input/cms_ca_state_ra_summary.csv", stringsAsFactors = FALSE)
ra_pbar_cms <- function(year) {
  p <- RA_STATE_CMS$avg_premium[RA_STATE_CMS$benefit_year == year]
  if (length(p) == 1) p else NA_real_
}
ra_gcf <- function(region, year) {
  g <- RA_GCF$gcf[RA_GCF$benefit_year == year & RA_GCF$rating_area == region]
  if (length(g) == 1) g else NA_real_
}

# Government share of claims under the silver cost-sharing reductions, by CSR
# variant: 73 - 70 = 3 percent with no induced utilization; 87 and 94 percent
# with 12 percent induced utilization, 19.04 and 26.88 percent.
CSR_GOV_SHARE <- c("73" = 0.03, "87" = 0.1904, "94" = 0.2688)
