# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Load demand data (from code/data-build/), derive the few
##                remaining analysis vars, apply SIPP in-market filter to
##                uninsured, save working datasets.

# Load demand data (CC + ACS combined, HH-year level) --------------------
# `plan_id`, `insurer`, `metal`, `network_type` are already attached in
# step 2's data-build. No plan_data join needed here.
hh <- fread("data/output/demand_households.csv") %>% as_tibble()


# Derive analysis variables -----------------------------------------------
# Note: dominated_choice keys on `metal` which now carries CSR-aware labels
# ("Silver - Enhanced 73"). Gold/Platinum strings still match exactly.
hh <- hh %>%
  mutate(
    assisted        = as.integer(navigator == 1 | broker == 1 | agent == 1),
    any_agent       = as.integer(broker == 1 | agent == 1),
    channel         = if_else(assisted == 1, "Assisted", "Unassisted"),
    channel_detail  = case_when(
      navigator == 1            ~ "Navigator",
      broker == 1 | agent == 1  ~ "Agent",
      TRUE                      ~ "Unassisted"
    ),
    FPL_bracket_analysis = case_when(
      FPL <= 1.38 ~ "138orless",
      FPL <= 2.50 ~ "138to250",
      FPL <= 4.00 ~ "250to400",
      TRUE        ~ "400ormore"
    ),
    # ACS demographics use perc_18to25/perc_26to34 and perc_35to44/perc_45to54;
    # demand estimation usually collapses these into perc_18to34 / perc_35to54
    perc_18to34 = coalesce(perc_18to25, 0) + coalesce(perc_26to34, 0),
    perc_35to54 = coalesce(perc_35to44, 0) + coalesce(perc_45to54, 0),
    # OLD pipeline used CC's eligibility flag (`subsidized_members > 0`),
    # not the formula-computed subsidy. Reverted because the formula version
    # drops some subsidized HHs whose computed subsidy rounds to 0, which
    # changes the dominated-choice estimation sample.
    subsidy_eligible = as.integer(coalesce(subsidized_members, 0L) > 0L),
    csr_eligible     = as.integer(subsidy_eligible == 1 & FPL <= 2.50),
    # Match OLD pipeline scope: only CSR-94 (FPL <= 1.50) and CSR-87
    # (FPL 1.50–2.00) HHs are in the dominated-choice analysis. CSR-73
    # (FPL 2.00–2.50) HHs are excluded entirely because Silver-73's AV
    # (~73%) is below Gold (80%) and Platinum (90%), so choosing Gold or
    # Platinum is not "dominated" — it's a real coverage tradeoff.
    dominated_choice = case_when(
      is.na(plan_id)                                         ~ NA_integer_,
      csr_eligible != 1                                      ~ NA_integer_,
      FPL > 2.00                                             ~ NA_integer_,
      FPL <= 1.50 & metal %in% c("Gold", "Platinum")         ~ 1L,
      FPL > 1.50 & FPL <= 2.00 & metal == "Gold"             ~ 1L,
      TRUE                                                   ~ 0L
    )
  )

# new_enrollee: lag-based, matches OLD pipeline's `is.na(previous_plan_offered)`
# concept (insured this year AND no plan to roll over from last year).
# Persistent household_id lets us recover this from year-to-year transitions.
# CRITICAL: lag must run on the FULL panel (including market-ineligible
# uninsured rows from step 4) so gap years are visible. We then drop
# market-ineligible rows after the lag is computed.
hh <- hh %>%
  arrange(household_id, year) %>%
  group_by(household_id) %>%
  mutate(prev_insured = lag(insured, default = 0L),
         new_enrollee = as.integer(insured == 1L & prev_insured == 0L)) %>%
  ungroup() %>%
  select(-prev_insured)

# Drop market-ineligible uninsured rows (HH likely had ESI / Medicaid /
# Medicare / left CA in that year). Done AFTER lag so new_enrollee is
# correctly set for the years immediately following these gaps.
n_before <- nrow(hh)
hh <- hh %>% filter(market_eligible == 1L)
cat(sprintf("  Dropped %d market-ineligible uninsured HH-years (%.1f%%)\n",
            n_before - nrow(hh),
            100 * (n_before - nrow(hh)) / n_before))


# Save working datasets ---------------------------------------------------
# Only hh_full lives in memory. hh_ins / hh_clean are derived on-demand by
# downstream scripts via filter — avoids holding ~3 inflated copies of the
# same rows simultaneously and ~2.5GB of redundant intermediate writes.
hh_full <- hh
rm(hh)

cat("Decision analysis data ready:\n")
cat("  hh_full: ", nrow(hh_full), "HH-years (",
    sum(hh_full$insured == 1L), "insured +",
    sum(hh_full$insured == 0L), "uninsured )\n")

fwrite(hh_full, "data/output/hh_full.csv")
cat("  Saved: hh_full.csv\n")
gc(verbose = FALSE)
