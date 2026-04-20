# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Load demand data (from code/data-build/), derive the few
##                remaining analysis vars, apply SIPP in-market filter to
##                uninsured, save working datasets.

# Load demand data (CC + ACS combined, HH-year level) ---------------------
hh <- read_csv("data/output/demand_households.csv", show_col_types = FALSE)


# Attach plan characteristics via plan_unique_id ---------------------------
# plan_unique_id = HIOS concat year, matches plan_data$HIOSYR
plan_data <- read_csv("data/input/Covered California/plan_data.csv",
                       show_col_types = FALSE, name_repair = "minimal")

plan_lookup <- plan_data %>%
  distinct(HIOSYR, .keep_all = TRUE) %>%
  select(HIOSYR, insurer_plan = Issuer_Name,
         plan_network_type_plan = PLAN_NETWORK_TYPE,
         metal_plan = metal_level)

hh <- hh %>%
  left_join(plan_lookup, by = c("plan_unique_id" = "HIOSYR")) %>%
  mutate(
    # Only overwrite insurer/metal/network from plan_data for CC HHs
    # (ACS HHs have plan_unique_id = NA so the join leaves these NA)
    insurer           = coalesce(insurer,           insurer_plan),
    plan_network_type = coalesce(plan_network_type, plan_network_type_plan),
    metal = coalesce(metal,
      case_when(
        metal_plan %in% c("Silver - Enhanced 73", "Silver - Enhanced 87",
                           "Silver - Enhanced 94") ~ "Silver",
        metal_plan == "Minimum Coverage" ~ "Catastrophic",
        TRUE ~ metal_plan
      ))
  ) %>%
  select(-insurer_plan, -plan_network_type_plan, -metal_plan)
rm(plan_lookup, plan_data)


# Derive analysis variables -----------------------------------------------
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
    subsidy_eligible = as.integer(subsidized_members > 0),
    csr_eligible     = as.integer(subsidy_eligible == 1 & FPL <= 2.50),
    dominated_choice = case_when(
      is.na(plan_name)                                       ~ NA_integer_,
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
