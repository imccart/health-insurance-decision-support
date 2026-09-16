# 5_merge-and-finalize.R
# Bind CC enrolled HHs (step 2) with CC uninsured HH-years (step 4). Recompute
# subsidy via the ACA formula (applies uniformly to both groups), derive
# cheapest-bronze premium per rating area × year, then compute the ACA mandate
# penalty.
#
# Inputs:
#   data/output/enrollment_hh.csv   (CC enrolled HHs, step 2)
#   data/output/cc_uninsured.csv    (CC off-year HHs surviving SIPP filter, step 4)
#   data/input/Covered California/plan_data.csv
# Output:
#   data/output/demand_households.csv

cc_enrolled  <- fread("data/output/enrollment_hh.csv")   %>% as_tibble()
cc_uninsured <- fread("data/output/cc_uninsured.csv")    %>% as_tibble()

# Bind: CC enrolled (insured = 1, keeps observed plan) and CC uninsured
# (insured = 0, plan fields NA). Weight = household_size for both.
# market_eligible = 1 always for enrolled (in market by definition);
# step 4's SIPP draw populates it for uninsured rows.
cat("  Binding CC enrolled + CC uninsured...\n")
demand_hh <- bind_rows(
  cc_enrolled  %>% mutate(source = "CC_enrolled",  insured = 1L,
                           weight = as.numeric(household_size),
                           market_eligible = 1L),
  cc_uninsured %>% mutate(source = "CC_uninsured", insured = 0L,
                           weight = as.numeric(household_size),
                           plan_id      = NA_character_,
                           insurer      = NA_character_,
                           metal        = NA_character_,
                           network_type = NA_character_,
                           agent = 0L, broker = 0L, navigator = 0L)
)
cat(sprintf("  Demand dataset: %d HH-years (%d enrolled, %d uninsured)\n",
            nrow(demand_hh),
            sum(demand_hh$insured == 1L),
            sum(demand_hh$insured == 0L)))


# Cheapest bronze + benchmark (second-lowest-cost silver) ------------------
# Cheapest bronze at region × year (mandate affordability). Benchmark at the
# zip3 level from step 2 (availability-screened), applied to the off-year rows
# here; enrolled rows keep the household premiumSLC step 2 already built from
# the same table. Region-level second-lowest silver is only a fallback where
# the zip3 market is missing (or a step-2 value came through as 0/NA).
cat("  Computing cheapest bronze and benchmark per market × year...\n")
plan_data <- read_csv("data/input/Covered California/plan_data.csv",
                       show_col_types = FALSE)

cheapest_br <- plan_data %>%
  filter(metal_level == "Bronze") %>%
  group_by(year = ENROLLMENT_YEAR, region) %>%
  summarize(cheapest_br_base = min(Premium, na.rm = TRUE), .groups = "drop")

slc_zip3 <- fread("data/output/slc_by_market.csv") %>% as_tibble()

slc_region <- plan_data %>%
  filter(metal_level == "Silver") %>%
  group_by(year = ENROLLMENT_YEAR, region) %>%
  summarize(slc_base = {
    p <- sort(Premium)
    if (length(p) >= 2) p[2] else if (length(p) == 1) p[1] else NA_real_
  }, .groups = "drop")

demand_hh <- demand_hh %>%
  left_join(cheapest_br, by = c("year", "region")) %>%
  left_join(slc_zip3,    by = c("zip3", "region", "year")) %>%
  left_join(slc_region,  by = c("year", "region")) %>%
  mutate(cheapest_premium = cheapest_br_base / RATING_FACTOR_AGE40 * rating_factor,
         premiumSLC = case_when(
           insured == 1L & is.finite(premiumSLC) & premiumSLC > 0
             ~ premiumSLC,
           is.finite(premiumSLC_base) ~ premiumSLC_base * rating_factor,
           TRUE ~ slc_base / RATING_FACTOR_AGE40 * rating_factor
         )) %>%
  select(-cheapest_br_base, -premiumSLC_base, -slc_base)
rm(plan_data, cheapest_br, slc_zip3, slc_region)


# Formula subsidy (ACA) for everyone --------------------------------------
# premiumSLC - SLC_contribution, floored at 0, for FPL <= 400% HHs. Below
# 138% the schedule's bottom piece (lower bound 0) applies: these are
# Medi-Cal-ineligible households that Covered California subsidizes.
# The formula is the quote the household saw when choosing (the exchange
# computes the advance credit from the same formula on attested income), and
# it measures enrolled and off-year rows symmetrically; the observed APTC
# additionally reflects credit elections and mid-year adjustments, which are
# about tax reconciliation rather than the monthly price faced at choice.
cat("  Computing formula subsidy...\n")
fpl_lb_lookup <- setNames(FPL_BRACKETS$fpl_LB, FPL_BRACKETS$bracket)
fpl_ub_lookup <- setNames(FPL_BRACKETS$fpl_UB, FPL_BRACKETS$bracket)

demand_hh <- demand_hh %>%
  mutate(FPL_bracket          = assign_bracket(FPL),
         subsidy_eligible_fpl = as.integer(FPL <= 4.0),
         fpl_LB               = fpl_lb_lookup[FPL_bracket],
         fpl_UB               = fpl_ub_lookup[FPL_bracket],
         perc_LB              = NA_real_,
         perc_UB              = NA_real_)
for (yr in 2014:2019) {
  yr_col <- paste0("YR", min(yr, 2019))
  idx <- which(demand_hh$year == yr & demand_hh$subsidy_eligible_fpl == 1)
  if (length(idx) == 0) next
  demand_hh$perc_LB[idx] <- contribution_percentages[[yr_col]][
    match(demand_hh$fpl_LB[idx], contribution_percentages$FPL)]
  demand_hh$perc_UB[idx] <- contribution_percentages[[yr_col]][
    match(demand_hh$fpl_UB[idx], contribution_percentages$FPL)]
}
elig <- which(demand_hh$subsidy_eligible_fpl == 1)
is_300_400 <- demand_hh$FPL_bracket == "300% FPL to 400% FPL" &
               demand_hh$subsidy_eligible_fpl == 1
demand_hh$SLC_contribution <- NA_real_
demand_hh$SLC_contribution[elig] <- aca_contribution(
  fpl               = demand_hh$FPL[elig],
  perc_LB           = demand_hh$perc_LB[elig],
  perc_UB           = demand_hh$perc_UB[elig],
  fpl_LB            = demand_hh$fpl_LB[elig],
  fpl_UB            = demand_hh$fpl_UB[elig],
  poverty_threshold = demand_hh$poverty_threshold[elig],
  bracket_300_400   = is_300_400[elig]
)
demand_hh <- demand_hh %>%
  mutate(subsidy = if_else(subsidy_eligible_fpl == 1,
                            pmax(0, premiumSLC - SLC_contribution),
                            0)) %>%
  select(-subsidy_eligible_fpl, -fpl_LB, -fpl_UB, -perc_LB, -perc_UB)
formula_only_mean <- mean(demand_hh$subsidy[demand_hh$insured == 1L & demand_hh$aptc_amt_int > 0], na.rm = TRUE)


# Tax-household size from the observed advance credit ----------------------
# The poverty guideline is for the tax household, which can be larger than the
# enrolled group (children on Medi-Cal, a spouse with employer coverage). An
# enrolled row with an advance credit below its plan premium reveals the
# contribution the exchange applied, premiumSLC - APTC, and with it the
# guideline: the contribution percentage does not depend on size, so the
# implied guideline is the enrolled-count guideline scaled by observed over
# formula contribution. Guidelines are linear in family size, so the nearest
# size is read off directly; it becomes the household's tax size on every row
# (enrolled and off-year), taken from its earliest enrolled year with a usable
# credit. Households without one keep the enrolled count. Rows with a usable
# credit keep the observed contribution itself.
cat("  Tax-household size from the advance credit...\n")
pov_year <- poverty_guidelines_long %>%
  group_by(year) %>%
  summarize(g1     = poverty_threshold[Family_Size == 1],
            g_step = poverty_threshold[Family_Size == 2] - poverty_threshold[Family_Size == 1],
            g_max  = max(Family_Size), .groups = "drop")
n_before <- nrow(demand_hh)
demand_hh <- demand_hh %>%
  mutate(year_cap = pmin(year, 2019L)) %>%
  left_join(pov_year, by = c("year_cap" = "year")) %>%
  mutate(contrib_obs = if_else(insured == 1L & is.finite(SLC_contribution) & SLC_contribution > 0 &
                                 is.finite(aptc_amt_int) & aptc_amt_int > 0 &
                                 is.finite(net_premium_amt_int) & net_premium_amt_int > 0,
                               premiumSLC - aptc_amt_int, NA_real_),
         size_raw     = 1 + (poverty_threshold * contrib_obs / SLC_contribution - g1) / g_step,
         size_implied = if_else(is.finite(size_raw) & size_raw >= 0.5 & size_raw < g_max + 0.5,
                                pmin(pmax(round(size_raw), 1), g_max), NA_real_))
tax_size <- demand_hh %>%
  filter(!is.na(size_implied)) %>%
  arrange(household_id, year) %>%
  distinct(household_id, .keep_all = TRUE) %>%
  select(household_id, tax_household_size = size_implied)
demand_hh <- demand_hh %>%
  left_join(tax_size, by = "household_id") %>%
  mutate(tax_household_size = coalesce(tax_household_size, as.numeric(household_size)),
         pct_contribution   = SLC_contribution / (poverty_threshold / 12 * FPL),
         poverty_threshold  = g1 + g_step * (tax_household_size - 1),
         SLC_contribution   = coalesce(contrib_obs, pct_contribution * (poverty_threshold / 12 * FPL)),
         subsidy            = if_else(FPL <= 4.0, pmax(0, premiumSLC - SLC_contribution), 0)) %>%
  select(-year_cap, -g1, -g_step, -g_max, -contrib_obs, -size_raw, -size_implied, -pct_contribution)
stopifnot(nrow(demand_hh) == n_before)
rm(pov_year, tax_size)
enr <- demand_hh %>% filter(insured == 1L)
cat(sprintf("    usable credit on %d of %d enrolled rows (%.1f%%); tax size above enrolled count for %.1f%% of households\n",
            sum(enr$aptc_amt_int > 0 & enr$net_premium_amt_int > 0 & enr$FPL <= 4.0, na.rm = TRUE), nrow(enr),
            100 * mean(enr$aptc_amt_int > 0 & enr$net_premium_amt_int > 0 & enr$FPL <= 4.0, na.rm = TRUE),
            100 * mean((enr %>% distinct(household_id, .keep_all = TRUE) %>%
                          mutate(up = tax_household_size > household_size))$up, na.rm = TRUE)))
cat(sprintf("    enrolled rows with a credit: observed APTC mean $%.0f, formula-only subsidy $%.0f, subsidy now $%.0f\n",
            mean(enr$aptc_amt_int[enr$aptc_amt_int > 0], na.rm = TRUE), formula_only_mean,
            mean(enr$subsidy[enr$aptc_amt_int > 0], na.rm = TRUE)))
cat(sprintf("    subsidy: enrolled mean $%.0f, off-year mean $%.0f\n",
            mean(demand_hh$subsidy[demand_hh$insured == 1L], na.rm = TRUE),
            mean(demand_hh$subsidy[demand_hh$insured == 0L], na.rm = TRUE)))
rm(enr, formula_only_mean, n_before)
# SLC_contribution (the income contribution cap zeta_it) and premiumSLC (the HH
# benchmark premium) are RETAINED: the structural counterfactual endogenizes the
# subsidy = pmax(0, premiumSLC(p) - SLC_contribution) as the benchmark price moves.
# NA SLC_contribution flags subsidy-ineligible HHs (above 400% FPL).


# Mandate penalty ----------------------------------------------------------
# n_adults derived from HH-level perc_0to17 × household_size (consistent for
# CC enrolled and CC uninsured — no individual-level data needed).
cat("  Computing mandate penalties...\n")

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
  mutate(
    n_children = round(household_size * coalesce(perc_0to17, 0)),
    n_adults   = pmax(household_size - n_children, 0L),
    tax_unit_type = case_when(
      household_size == 1 ~ "single",
      n_adults >= 2       ~ "married",
      TRUE                ~ "household_head"
    )
  ) %>%
  left_join(filing_lookup, by = c("year", "tax_unit_type")) %>%
  mutate(
    eff_cheapest = pmax(0, cheapest_premium - coalesce(subsidy, 0)),
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
rm(filing_lookup)

cat(sprintf("    penalty: mean $%.0f, %.1f%% zero\n",
            mean(demand_hh$penalty, na.rm = TRUE),
            100 * mean(demand_hh$penalty == 0, na.rm = TRUE)))


# Save ---------------------------------------------------------------------
fwrite(demand_hh, "data/output/demand_households.csv")
