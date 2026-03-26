# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Description:   Load demand data, derive analysis variables, create working
##                datasets for reduced-form analysis.

# Load data ---------------------------------------------------------------

hh_raw  <- read_csv("data/output/demand_households.csv", show_col_types = FALSE)
ind_raw <- read_csv("data/output/demand_individuals.csv", show_col_types = FALSE)
plan_data <- read_csv("data/input/Covered California/plan_data.csv",
                      show_col_types = FALSE, name_repair = "minimal")

# Filter flagged observations ---------------------------------------------

hh  <- hh_raw  %>% filter(flagged == FALSE)
ind <- ind_raw %>% filter(flagged == FALSE)

# Derive HH-level variables from individuals ------------------------------

ind_hh_vars <- ind %>%
  group_by(household_id, year) %>%
  summarize(
    oldest_member = max(age, na.rm = TRUE),
    english = if_else(any(language_spoken == "English"), 1L, 0L),
    spanish = if_else(any(language_spoken == "Spanish") & !any(language_spoken == "English"), 1L, 0L),
    other_language = if_else(!any(language_spoken %in% c("English", "Spanish")), 1L, 0L),
    .groups = "drop"
  )

# Join individual-derived vars and plan characteristics -------------------

hh <- hh %>%
  left_join(ind_hh_vars, by = c("household_id", "year")) %>%
  derive_channel_vars() %>%
  derive_plan_characteristics(plan_data)

# Derive analysis variables -----------------------------------------------

hh <- hh %>%
  mutate(
    new_enrollee = if_else(is.na(previous_plan_offered), 1L, 0L),
    switch = case_when(
      new_enrollee == 1 ~ NA_integer_,
      plan_number_nocsr != previous_plan_number ~ 1L,
      TRUE ~ 0L
    ),
    region = str_extract(zip_region_year, "(?<=_)\\d+(?=_)") %>% as.integer(),
    FPL_bracket = case_when(
      FPL <= 1.38 ~ "138orless",
      FPL <= 2.50 ~ "138to250",
      FPL <= 4.00 ~ "250to400",
      TRUE        ~ "400ormore"
    ),
    perc_18to34 = perc_18to25 + perc_26to34,
    perc_35to54 = perc_35to44 + perc_45to54,
    subsidy_eligible = if_else(subsidized_members > 0, 1L, 0L),
    csr_eligible = if_else(subsidy_eligible == 1 & FPL <= 2.50, 1L, 0L),
    dominated_choice = case_when(
      is.na(plan_number_nocsr) ~ NA_integer_,
      csr_eligible != 1 ~ NA_integer_,
      FPL <= 1.50 & metal %in% c("Gold", "Platinum") ~ 1L,
      FPL > 1.50 & FPL <= 2.00 & metal == "Gold"     ~ 1L,
      TRUE ~ 0L
    )
  )

# SIPP out-of-market filter -----------------------------------------------

sipp_hh <- read_csv("data/output/sipp_households.csv", show_col_types = FALSE)

sipp_logit <- glm(
  transitioned ~ FPL_bracket + household_size + perc_0to17 + perc_18to34 +
    perc_35to54 + perc_male + perc_asian + perc_black + perc_hispanic + perc_other,
  data = sipp_hh %>% filter(!is.na(entered_market) | !is.na(exited_market)),
  family = "binomial"
)

set.seed(84324)
hh <- hh %>%
  mutate(
    pred_oom = predict(sipp_logit, newdata = ., type = "response"),
    pred_oom = if_else(is.na(plan_number_nocsr), pred_oom, NA_real_),
    unif_draw = runif(n(), 0, 1),
    out_of_market = if_else(!is.na(pred_oom), pred_oom >= unif_draw, FALSE)
  )

n_before <- nrow(hh)
hh <- hh %>% filter(!out_of_market)
cat("  SIPP filter: dropped", n_before - nrow(hh), "out-of-market uninsured\n")

rm(sipp_hh)

# Working datasets --------------------------------------------------------

hh_full  <- hh
hh_clean <- hh %>% filter(new_enrollee == 1)
hh_ins   <- hh %>% filter(!is.na(plan_number_nocsr))

cat("Decision analysis data ready.\n")
cat("  hh_full: ", nrow(hh_full), " rows\n")
cat("  hh_clean:", nrow(hh_clean), " rows\n")
cat("  hh_ins:  ", nrow(hh_ins), " rows\n")

# Save to disk for downstream scripts
write_csv(hh_full, "data/output/hh_full.csv")
write_csv(hh_clean, "data/output/hh_clean.csv")
write_csv(hh_ins, "data/output/hh_ins.csv")
cat("  Saved: hh_full.csv, hh_clean.csv, hh_ins.csv\n")
