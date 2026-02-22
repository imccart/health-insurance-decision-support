# Helper functions for enrollment data processing
# Shared across all data build scripts

# Insurer name standardization -----------------------------------------------
#
# Canonical mapping from raw names (both enrollment and plan_data variants)
# to standardized short names. Covers truncated FOIA names (e.g., "Contra Costa
# Heal") and full plan_data names (e.g., "Contra Costa Health Plan").

INSURER_MAP <- c(
  # Enrollment (FOIA) names — truncated at ~17 chars
  "Anthem Blue Cross"  = "Anthem",
  "Blue Shield"        = "Blue_Shield",
  "Chinese Community"  = "Chinese_Community",
  "Contra Costa Heal"  = "Contra_Costa",
  "Health Net"         = "Health_Net",
  "LA Care"            = "LA_Care",
  "Molina Health Car"  = "Molina",
  "Oscar Health Plan"  = "Oscar",
  "SHARP Health Plan"  = "SHARP",
  "UnitedHealthcare"   = "United",
  "Valley Health"      = "Valley",
  "Western Health"     = "Western",
  # plan_data names — full strings
  "Contra Costa Health Plan" = "Contra_Costa",
  "Sharp" = "SHARP"
)

standardize_insurer <- function(x) {
  ifelse(x %in% names(INSURER_MAP), INSURER_MAP[x], x)
}

# Invert the ACA subsidy formula to recover exact FPL -----------------------
#
# The ACA contribution schedule is piecewise-linear in FPL. Within each piece,
# the required contribution is quadratic in FPL (contribution_pct * income),
# so inverting requires the quadratic formula. The 300-400% bracket is a
# special case: contribution percentage is constant, so it's linear.
#
# Args (all vectors, same length):
#   slc_contribution  monthly $ the household must contribute toward SLC silver
#   perc_LB, perc_UB  contribution percentage at lower/upper bound of bracket
#   fpl_LB, fpl_UB    FPL ratio at lower/upper bound (e.g., 1.5, 2.0)
#   poverty_threshold  annual poverty guideline for this household size
#   bracket_300_400    logical: TRUE if this is the 300-400% bracket

invert_aca_subsidy <- function(slc_contribution, perc_LB, perc_UB,
                               fpl_LB, fpl_UB, poverty_threshold,
                               bracket_300_400 = FALSE) {
  exact_fpl <- rep(NA_real_, length(slc_contribution))

  # 300-400% bracket: contribution pct is constant (perc_LB), so linear
  lin <- bracket_300_400
  exact_fpl[lin] <- slc_contribution[lin] / (perc_LB[lin] * poverty_threshold[lin] / 12) * 100

  # All other brackets: quadratic
  quad <- !bracket_300_400
  a <- poverty_threshold[quad] / 12 * (perc_UB[quad] - perc_LB[quad]) / (fpl_UB[quad] - fpl_LB[quad])
  b <- poverty_threshold[quad] / 12 * (perc_LB[quad] - (perc_UB[quad] - perc_LB[quad]) * fpl_LB[quad] / (fpl_UB[quad] - fpl_LB[quad]))
  c <- -slc_contribution[quad]
  exact_fpl[quad] <- (-b + sqrt(b^2 - 4 * a * c)) / (2 * a) * 100

  exact_fpl
}


# Forward ACA contribution calculation --------------------------------------
#
# Given a known FPL percentage, compute the monthly contribution toward SLC.
# This is the forward direction of the inversion above.

calculate_aca_contribution <- function(fpl_pct, perc_LB, perc_UB,
                                       fpl_LB, fpl_UB, poverty_threshold,
                                       bracket_300_400 = FALSE) {
  contribution <- rep(NA_real_, length(fpl_pct))

  lin <- bracket_300_400
  contribution[lin] <- perc_LB[lin] * (poverty_threshold[lin] / 12 * fpl_pct[lin] / 100)

  quad <- !bracket_300_400
  contribution[quad] <- ((perc_UB[quad] - perc_LB[quad]) *
    (fpl_pct[quad] / 100 - fpl_LB[quad]) / (fpl_UB[quad] - fpl_LB[quad]) + perc_LB[quad]) *
    (poverty_threshold[quad] / 12 * fpl_pct[quad] / 100)

  contribution
}


# Compute second-lowest silver premium by market ----------------------------
#
# For each zip3 x region x year market, find the second-lowest-cost silver plan
# premium (age-21 equivalent). This is the ACA benchmark for subsidy calculation.
#
# Args:
#   zip3_long    tibble with one row per zip3 x region x year x available product
#                (pivoted from zip3_choices wide format)
#   prod_defs    product_definitions tibble (Product, insurer, plan_network_type, MSP, Network)
#   plan_data    plan_data tibble with Premium, Issuer_Name, PLAN_NETWORK_TYPE, etc.
#
# Returns: tibble with columns zip3, region, year, premiumSLC

RATING_FACTOR_AGE40 <- 1.278  # age-40 rating factor, used to convert premiums to age-21 base

compute_slc_premium <- function(zip3_long, prod_defs, plan_data) {

  # Get silver plans from plan_data
  silver_plans <- plan_data %>%
    filter(metal_level == "Silver") %>%
    select(Issuer_Name, PLAN_NETWORK_TYPE, MSP, Network_num,
           region, ENROLLMENT_YEAR, Premium)

  # Join available products to their plan-level details
  available_silver <- zip3_long %>%
    inner_join(prod_defs, by = c("product" = "Product")) %>%
    inner_join(silver_plans,
               by = c("insurer" = "Issuer_Name",
                       "plan_network_type" = "PLAN_NETWORK_TYPE",
                       "MSP" = "MSP",
                       "region" = "region",
                       "year" = "ENROLLMENT_YEAR"),
               relationship = "many-to-many") %>%
    # Handle SHARP's two networks
    filter(is.na(Network_num) | Network == Network_num)

  # Second-lowest silver premium per market (age-21 equivalent)
  available_silver %>%
    group_by(zip3, region, year) %>%
    arrange(Premium) %>%
    mutate(rank = row_number()) %>%
    filter(rank == if (n() == 1) 1 else 2) %>%
    ungroup() %>%
    transmute(zip3, region, year,
              premiumSLC = Premium / RATING_FACTOR_AGE40)
}
