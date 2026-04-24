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

cat("  Loading inputs...\n")
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


# Cheapest bronze + second-lowest-cost silver (region × year) --------------
# Scaled by HH rating_factor for the affordability exemption and the
# formula-subsidy calc below.
cat("  Computing cheapest bronze and SLC per rating area × year...\n")
plan_data <- read_csv("data/input/Covered California/plan_data.csv",
                       show_col_types = FALSE)

cheapest_br <- plan_data %>%
  filter(metal_level == "Bronze") %>%
  group_by(year = ENROLLMENT_YEAR, region) %>%
  summarize(cheapest_br_base = min(Premium, na.rm = TRUE), .groups = "drop")

slc <- plan_data %>%
  filter(metal_level == "Silver") %>%
  group_by(year = ENROLLMENT_YEAR, region) %>%
  summarize(slc_base = {
    p <- sort(Premium)
    if (length(p) >= 2) p[2] else if (length(p) == 1) p[1] else NA_real_
  }, .groups = "drop")

demand_hh <- demand_hh %>%
  left_join(cheapest_br, by = c("year", "region")) %>%
  left_join(slc,         by = c("year", "region")) %>%
  mutate(cheapest_premium = cheapest_br_base / RATING_FACTOR_AGE40 * rating_factor,
         premiumSLC       = slc_base         / RATING_FACTOR_AGE40 * rating_factor) %>%
  select(-cheapest_br_base, -slc_base)
rm(plan_data, cheapest_br, slc)


# Formula subsidy (ACA) for everyone --------------------------------------
# premiumSLC - SLC_contribution, floored at 0, for 138% <= FPL <= 400% HHs.
# Matches Saltzman's JHE Eq. 3 / old-repo prepare.demand.data.R:732-734.
cat("  Computing formula subsidy...\n")
fpl_lb_lookup <- setNames(FPL_BRACKETS$fpl_LB, FPL_BRACKETS$bracket)
fpl_ub_lookup <- setNames(FPL_BRACKETS$fpl_UB, FPL_BRACKETS$bracket)

demand_hh <- demand_hh %>%
  mutate(FPL_bracket          = assign_bracket(FPL),
         subsidy_eligible_fpl = as.integer(FPL >= 1.38 & FPL <= 4.0),
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
  select(-subsidy_eligible_fpl, -fpl_LB, -fpl_UB, -perc_LB, -perc_UB,
         -SLC_contribution)


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
cat("Step 5 complete.\n")
