# 5_merge-and-finalize.R
# Combine CC enrollee HHs (from step 2) with ACS uninsured HHs (from step 4),
# applying:
#   - SIPP market-transition filter to the ACS uninsured
#   - Income imputation for CC enrollees in "400% FPL or greater" with missing FPL
# Produces the final demand-side HH and individual datasets.
#
# Inputs:
#   data/output/enrollment_hh.csv          (CC, step 2)
#   data/output/enrollment_individual.csv  (CC individuals, step 2)
#   data/output/acs_households.csv         (ACS uninsured, step 4)
#   data/output/acs_individuals.csv
#   data/output/sipp_transition_logit.rds  (step 3)
#   data/output/income_model_coefs.csv     (step 4)
#   data/output/income_distribution.csv    (step 4)
# Outputs:
#   data/output/demand_households.csv
#   data/output/demand_individuals.csv


set.seed(20260224)

cat("  Loading inputs...\n")
cc_hh   <- fread("data/output/enrollment_hh.csv")         %>% as_tibble()
cc_ind  <- fread("data/output/enrollment_individual.csv") %>% as_tibble()
acs_hh  <- fread("data/output/acs_households.csv")        %>% as_tibble()
acs_ind <- fread("data/output/acs_individuals.csv")       %>% as_tibble()


# SIPP transition filter on ACS uninsured ----------------------------------
# Predict P(transition) for each ACS HH, drop those above U(0,1) → keeps
# the ~27% "consistent uninsured" subsample.
cat("  Applying SIPP transition filter to ACS uninsured...\n")

sipp_logit <- readRDS("data/output/sipp_transition_logit.rds")

# Build the predictor variables the logit expects. Names must match:
# family_size, FPL_bracket, perc_0to17, perc_18to34, perc_35to54, perc_male,
# perc_asian, perc_black, perc_hispanic, perc_other
acs_hh <- acs_hh %>% mutate(
  family_size = household_size,
  # ACS has perc_18to25 + perc_26to34 and perc_35to44 + perc_45to54 separately;
  # combine to match SIPP granularity.
  perc_18to34 = perc_18to25 + perc_26to34,
  perc_35to54 = perc_35to44 + perc_45to54
)

p_transition <- predict(sipp_logit, newdata = acs_hh, type = "response")
# Compare against a uniform random draw; HH is "in-market" if draw > p
acs_hh$in_market <- runif(nrow(acs_hh)) > p_transition

n_before <- nrow(acs_hh)
acs_hh <- acs_hh %>% filter(in_market)
cat(sprintf("    Kept %d / %d ACS HHs (%.1f%% — target ~27%%)\n",
            nrow(acs_hh), n_before, 100 * nrow(acs_hh) / n_before))


# Income imputation for CC >400% HHs with missing FPL ----------------------
# Reconstruct OLS prediction from saved coefficients, rank-match to ACS
# >400% FPL distribution.
cat("  Imputing income for CC enrollees in >400% bracket...\n")

income_coefs <- fread("data/output/income_model_coefs.csv") %>% as_tibble()
income_dist  <- fread("data/output/income_distribution.csv") %>% as_tibble()

target <- cc_hh$subsidy_fpl_bracket == "400% FPL or greater" & is.na(cc_hh$FPL)
n_target <- sum(target)

if (n_target > 0) {
  # Build prediction manually from saved coefficients (OLS object not saved)
  coef_vec <- setNames(income_coefs$estimate, income_coefs$term)
  scores <- rep(coef_vec["(Intercept)"], n_target)

  # Rating-area dummies (reference-coded)
  if (!"rating_area" %in% names(cc_hh)) {
    # Build rating_area from region via `rating_areas` reference table
    cc_hh <- cc_hh %>% mutate(rating_area = region)
  }
  ra_vals <- unique(cc_hh$rating_area[target])
  for (ra in ra_vals) {
    term <- paste0("as.factor(rating_area)", ra)
    if (term %in% names(coef_vec)) {
      idx <- which(cc_hh$rating_area[target] == ra)
      scores[idx] <- scores[idx] + coef_vec[term]
    }
  }

  # Continuous covariates (names must match those in step 4's OLS)
  cont_vars <- c("household_size", "perc_male", "perc_0to17", "perc_18to25",
                  "perc_26to34", "perc_35to44", "perc_45to54", "perc_55to64",
                  "perc_white", "perc_black", "perc_hispanic", "perc_asian")
  cc_hh_sub <- cc_hh[target, ]
  for (v in cont_vars) {
    if (v %in% names(coef_vec) && v %in% names(cc_hh_sub)) {
      scores <- scores + coef_vec[v] * cc_hh_sub[[v]]
    }
  }

  # Rank-based sampling: sort scores, sort donor incomes, match ranks
  income_ranks <- rank(scores, ties.method = "random")
  sampled <- sort(sample(income_dist$FPL, size = n_target, replace = TRUE))
  cc_hh$FPL[target] <- sampled[income_ranks]
}


# Combine CC and ACS into unified demand-side dataset ----------------------
cat("  Combining CC + ACS into demand dataset...\n")

# Align column sets. Use common columns; CC has plan info (plan_id_HIOS,
# plan_name, metal, etc.) that ACS doesn't (they're uninsured).
demand_hh <- bind_rows(
  cc_hh  %>% mutate(source = "CC",  insured = 1L),
  acs_hh %>% mutate(source = "ACS", insured = 0L,
                    plan_name = NA_character_,
                    plan_unique_id = NA_character_,
                    plan_id_HIOS = NA_character_,
                    insurer = NA_character_,
                    metal = NA_character_,
                    metal_level_enhanced = NA_character_,
                    plan_network_type = NA_character_)
)

# Combine individuals similarly
demand_ind <- bind_rows(
  cc_ind  %>% mutate(source = "CC"),
  acs_ind %>% mutate(source = "ACS")
)

cat(sprintf("  Demand dataset: %d HH-years (%d CC, %d ACS)\n",
            nrow(demand_hh), sum(demand_hh$source == "CC"),
            sum(demand_hh$source == "ACS")))


# Save ---------------------------------------------------------------------
fwrite(demand_hh,  "data/output/demand_households.csv")
fwrite(demand_ind, "data/output/demand_individuals.csv")
cat("Step 5 complete.\n")
