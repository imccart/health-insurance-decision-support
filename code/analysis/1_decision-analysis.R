# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Load demand data (from code/data-build/), derive the few
##                remaining analysis vars, apply SIPP in-market filter to
##                uninsured, save working datasets.

# Load demand data (CC + ACS combined, HH-year level) --------------------
# `plan_id`, `insurer`, `metal`, `network_type` are already attached in
# step 2's data-build. No plan_data join needed here.
hh <- read_csv("data/output/demand_households.csv", show_col_types = FALSE)


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
    subsidy_eligible = as.integer(coalesce(subsidy, 0) > 0),
    csr_eligible     = as.integer(subsidy_eligible == 1 & FPL <= 2.50),
    dominated_choice = case_when(
      is.na(plan_id)                                         ~ NA_integer_,
      csr_eligible != 1                                      ~ NA_integer_,
      FPL <= 1.50 & metal %in% c("Gold", "Platinum")         ~ 1L,
      FPL > 1.50 & FPL <= 2.00 & metal == "Gold"             ~ 1L,
      TRUE                                                   ~ 0L
    )
  )


# Save working datasets ---------------------------------------------------
hh_full <- hh
hh_ins  <- hh %>% filter(insured == 1L)

cat("Decision analysis data ready:\n")
cat("  hh_full:", nrow(hh_full), "HH-years (",
     sum(hh_full$insured == 1L), "insured +",
     sum(hh_full$insured == 0L), "uninsured )\n")
cat("  hh_ins: ", nrow(hh_ins),  "insured HH-years\n")

write_csv(hh_full, "data/output/hh_full.csv")
write_csv(hh_ins,  "data/output/hh_ins.csv")
cat("  Saved: hh_full.csv, hh_ins.csv\n")

rm(hh)
gc(verbose = FALSE)
