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

# Align column sets. Use common columns; CC has plan info (plan_id, metal,
# insurer, network_type) that ACS doesn't (uninsured HHs).
demand_hh <- bind_rows(
  cc_hh  %>% mutate(source = "CC",  insured = 1L),
  acs_hh %>% mutate(source = "ACS", insured = 0L,
                    plan_id = NA_character_,
                    insurer = NA_character_,
                    metal = NA_character_,
                    network_type = NA_character_,
                    # ACS uninsured HHs never enrolled, so by definition no
                    # broker/agent/navigator assistance.
                    agent = 0L, broker = 0L, navigator = 0L)
)

# Combine individuals similarly
demand_ind <- bind_rows(
  cc_ind  %>% mutate(source = "CC"),
  acs_ind %>% mutate(source = "ACS")
)

cat(sprintf("  Demand dataset: %d HH-years (%d CC, %d ACS)\n",
            nrow(demand_hh), sum(demand_hh$source == "CC"),
            sum(demand_hh$source == "ACS")))

# CC has `region`, ACS has `rating_area`. Unify so both exist on every row.
demand_hh <- demand_hh %>%
  mutate(rating_area = coalesce(rating_area, region),
         region      = coalesce(region, rating_area))


# Cheapest bronze premium (region × year, scaled by HH rating_factor) ------
# Downstream need: affordability exemption for the mandate penalty, and the
# catastrophic-plan eligibility filter inside build_choice_data.
cat("  Computing cheapest bronze premiums...\n")
plan_data <- read.csv("data/input/Covered California/plan_data.csv",
                       stringsAsFactors = FALSE)
cheapest_br <- plan_data %>%
  filter(metal_level == "Bronze") %>%
  group_by(year = ENROLLMENT_YEAR, region) %>%
  summarize(cheapest_br = min(Premium, na.rm = TRUE), .groups = "drop")

demand_hh <- demand_hh %>%
  left_join(cheapest_br, by = c("year", "region")) %>%
  mutate(cheapest_premium = cheapest_br / RATING_FACTOR_AGE40 * rating_factor) %>%
  select(-cheapest_br)
rm(plan_data, cheapest_br)


# Mandate penalty ----------------------------------------------------------
# IRS formula: max of flat-per-person or percent-of-income, capped at the
# national avg bronze. Zero when below filing threshold or cheapest bronze
# exceeds affordability threshold.
cat("  Computing mandate penalties...\n")

hh_adults <- demand_ind %>%
  mutate(age_u = coalesce(age, AGE)) %>%
  group_by(household_year) %>%
  summarize(n_adults = sum(age_u >= 18, na.rm = TRUE), .groups = "drop")

filing_lookup <- tribble(
  ~year, ~single, ~household_head, ~married,
  2014L, 10150,   13050,           20300,
  2015L, 10300,   13250,           20600,
  2016L, 10350,   13350,           20700,
  2017L, 10400,   16400,           20800,
  2018L, 12000,   24000,           18000,
  2019L, 12200,   24400,           18350
) %>%
  pivot_longer(-year, names_to = "tax_unit_type", values_to = "filing_threshold")

afford <- c(`2014` = 0.08,  `2015` = 0.0805, `2016` = 0.0813,
            `2017` = 0.0816, `2018` = 0.0805, `2019` = 0.083)
flat   <- c(`2014` = 95,  `2015` = 325, `2016` = 695, `2017` = 695,
            `2018` = 695, `2019` = 0)
perc   <- c(`2014` = 0.01, `2015` = 0.02, `2016` = 0.025, `2017` = 0.025,
            `2018` = 0.025, `2019` = 0)
cap    <- c(`2014` = 204, `2015` = 207, `2016` = 223, `2017` = 272,
            `2018` = 283, `2019` = 0) * 12

demand_hh <- demand_hh %>%
  left_join(hh_adults, by = "household_year") %>%
  mutate(
    n_adults   = coalesce(n_adults, 0L),
    n_children = pmax(household_size - n_adults, 0L),
    tax_unit_type = case_when(
      household_size == 1 ~ "single",
      n_adults >= 2       ~ "married",
      TRUE                ~ "household_head"
    )
  ) %>%
  left_join(filing_lookup, by = c("year", "tax_unit_type")) %>%
  mutate(
    eff_cheapest = if_else(coalesce(subsidized_members, 0L) > 0L,
                            cheapest_premium - coalesce(subsidy, 0),
                            cheapest_premium),
    afford_pct   = afford[as.character(year)],
    exempt = (FPL * poverty_threshold < filing_threshold) |
             (eff_cheapest * 12 > afford_pct * FPL * poverty_threshold),
    penalty = if_else(exempt, 0,
      pmin(
        pmax(
          pmin((n_adults + 0.5 * n_children) * flat[as.character(year)],
               3 * flat[as.character(year)]),
          perc[as.character(year)] *
            (FPL * poverty_threshold - filing_threshold)
        ),
        (n_adults + n_children) * cap[as.character(year)]
      )
    ),
    penalty = pmax(penalty, 0)
  ) %>%
  select(-n_adults, -n_children, -tax_unit_type, -filing_threshold,
         -eff_cheapest, -afford_pct, -exempt)
rm(hh_adults, filing_lookup)

cat(sprintf("    penalty: mean $%.0f, %.1f%% zero\n",
            mean(demand_hh$penalty, na.rm = TRUE),
            100 * mean(demand_hh$penalty == 0, na.rm = TRUE)))


# Save ---------------------------------------------------------------------
fwrite(demand_hh,  "data/output/demand_households.csv")
fwrite(demand_ind, "data/output/demand_individuals.csv")
cat("Step 5 complete.\n")
