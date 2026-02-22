# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-22
## Description:   Process SIPP 2014 panel (waves 1-3) to build household-year
##                dataset with nongroup market transition indicators. Used to
##                estimate logit predicting "in-market" vs "out-of-market"
##                uninsured for the analysis sample filter.
##
## Port of:       _old-repo/data-code/impute.SIPP.R (771 lines base R)
## Bug fix:       Old code waves 2-3 used | instead of & for age brackets,
##                marking everyone as 18-34 and 35-54. Fixed here.

# Parameters ---------------------------------------------------------------

sipp_dir <- "data/input/SIPP Data"

waves <- tibble(
  wave_num = 1:3,
  year     = 2013:2015,
  wave_csv = file.path(sipp_dir, paste0("pu2014w", 1:3, ".csv")),
  status_csv = file.path(sipp_dir, paste0("status", 2013:2015, ".csv"))
)

month_names <- c("jan", "feb", "mar", "apr", "may", "jun",
                 "jul", "aug", "sep", "oct", "nov", "dec")


# process_sipp_wave() -----------------------------------------------------

process_sipp_wave <- function(wave_csv, status_csv, wave_num, year) {

  # Mover column differs by wave
  mover_col <- if (wave_num == 1) "RMOVER" else "TMOVER"

  wave <- read_csv(wave_csv, show_col_types = FALSE,
                   col_select = c(SSUID, PNUM, SWAVE, MONTHCODE, TAGE, ESEX,
                                  ERACE, EORIGIN, WPFINWGT, THTOTINC, RHPOV,
                                  all_of(mover_col),
                                  EYNOESI_COV, EYNOESI_EXP, EYNOESI_HTH,
                                  EYNOESI_ELS, EYNOESI_UNH)) %>%
    mutate(SSUID = as.character(SSUID))

  # Standardize mover column name
  if (wave_num == 1) {
    wave <- wave %>% rename(TMOVER = RMOVER)
  }

  # Household demographics per SSUID-MONTHCODE ----------------------------
  hh_demog <- wave %>%
    group_by(SSUID, MONTHCODE) %>%
    summarize(
      family_size_month = n_distinct(PNUM),
      percentmale    = mean(ESEX == 1),
      percentasian   = mean(ERACE == 3),
      percentblack   = mean(ERACE == 2),
      percenthispanic = mean(EORIGIN == 1),
      percentother   = mean(ERACE == 4 & EORIGIN == 2),
      percentyoung   = mean(TAGE <= 17),
      percentmiddle  = mean(TAGE >= 18 & TAGE <= 34),
      percentold     = mean(TAGE >= 35 & TAGE <= 54),
      .groups = "drop"
    )

  wave <- wave %>% left_join(hh_demog, by = c("SSUID", "MONTHCODE"))

  # Employer offer (any of the "reason didn't take ESI" flags == 1) --------
  wave <- wave %>%
    mutate(
      employer_offer = if_else(
        (EYNOESI_COV == 1 & !is.na(EYNOESI_COV)) |
        (EYNOESI_EXP == 1 & !is.na(EYNOESI_EXP)) |
        (EYNOESI_HTH == 1 & !is.na(EYNOESI_HTH)) |
        (EYNOESI_ELS == 1 & !is.na(EYNOESI_ELS)) |
        (EYNOESI_UNH == 1 & !is.na(EYNOESI_UNH)),
        1L, 0L
      )
    )

  # Merge status file (monthly insurance type indicators) ------------------
  month_cols <- paste0(month_names, year)

  status <- read_csv(status_csv, show_col_types = FALSE, name_repair = "minimal") %>%
    select(SSUID, PNUM, SWAVE, TAGE, all_of(month_cols)) %>%
    mutate(SSUID = as.character(SSUID))

  wave <- wave %>% inner_join(status, by = c("SSUID", "PNUM", "SWAVE", "TAGE"))

  # STARTNONGROUP: earliest month with nongroup (type == 1) ----------------
  # Walk backwards from December so earlier months overwrite later ones
  wave <- wave %>% mutate(STARTNONGROUP = 0L)
  for (m in 12:1) {
    col <- month_cols[m]
    wave <- wave %>%
      mutate(STARTNONGROUP = if_else(.data[[col]] == 1 & !is.na(.data[[col]]),
                                     as.integer(m), STARTNONGROUP))
  }
  wave <- wave %>% mutate(STARTNONGROUP = if_else(STARTNONGROUP == 0L, NA_integer_, STARTNONGROUP))

  # ENDNONGROUP: first month after start where coverage ends ---------------
  wave <- wave %>% mutate(ENDNONGROUP = 0L)
  for (m in 1:11) {
    next_col <- month_cols[m + 1]
    wave <- wave %>%
      mutate(ENDNONGROUP = if_else(
        STARTNONGROUP <= m & !is.na(STARTNONGROUP) &
          !is.na(.data[[next_col]]) & .data[[next_col]] != 1 & ENDNONGROUP == 0L,
        as.integer(m), ENDNONGROUP
      ))
  }
  # If still on nongroup through December, ENDNONGROUP = 12
  wave <- wave %>%
    mutate(
      ENDNONGROUP = if_else(
        STARTNONGROUP <= 12L & !is.na(STARTNONGROUP) & ENDNONGROUP == 0L,
        12L, ENDNONGROUP
      ),
      ENDNONGROUP = if_else(ENDNONGROUP == 0L, NA_integer_, ENDNONGROUP)
    )

  # Convert month flags: 0 → NA (old code convention for downstream logic)
  for (col in month_cols) {
    wave <- wave %>% mutate(!!col := na_if(.data[[col]], 0))
  }

  wave
}


# Process all 3 waves ------------------------------------------------------

cat("  Processing SIPP waves...\n")
sipp_list <- pmap(waves, function(wave_num, year, wave_csv, status_csv) {
  cat("    Wave", wave_num, "(", year, ")\n")
  process_sipp_wave(wave_csv, status_csv, wave_num, year)
})

sipp <- suppressMessages(bind_rows(sipp_list))
rm(sipp_list)


# Build household-year panel -----------------------------------------------

cat("  Building household-year panel...\n")

# Remap SSUID to integer household_id
old_ids <- unique(sipp$SSUID)
id_map <- setNames(seq_along(old_ids), old_ids)
sipp <- sipp %>%
  mutate(
    household_id = id_map[as.character(SSUID)],
    year = case_when(SWAVE == 1 ~ 2013L, SWAVE == 2 ~ 2014L, SWAVE == 3 ~ 2015L),
    household_year = paste(household_id, year, sep = "_"),
    FPL = pmax(0, THTOTINC / RHPOV)
  )

# Drop households not present in all 3 waves
hh_by_year <- sipp %>%
  distinct(household_id, year) %>%
  count(household_id) %>%
  filter(n == 3)

sipp <- sipp %>% filter(household_id %in% hh_by_year$household_id)

# Keep only households ever on nongroup
hh_ever_nongroup <- sipp %>%
  filter(!is.na(STARTNONGROUP)) %>%
  pull(household_id) %>%
  unique()

# Build one row per household-year (take first person-month for match fields)
match_cols <- c("SSUID", "household_id", "year", "household_year",
                "jan2013", "jan2014", "jan2015",
                "TMOVER", "STARTNONGROUP", "ENDNONGROUP", "FPL",
                "percentyoung", "percentmiddle", "percentold",
                "percentmale", "percentasian", "percentblack",
                "percenthispanic", "percentother")

# Only keep columns that exist (jan columns are wave-specific)
match_cols <- intersect(match_cols, names(sipp))

sipp_hh <- sipp %>%
  filter(!duplicated(household_year)) %>%
  select(all_of(match_cols)) %>%
  filter(household_id %in% hh_ever_nongroup)

# Household-year aggregations from person-month data -----------------------
sipp_agg <- sipp %>%
  filter(household_year %in% sipp_hh$household_year) %>%
  group_by(household_year) %>%
  summarize(
    weight = sum(WPFINWGT, na.rm = TRUE),
    household_size = n() / 12,
    employer_offer = pmin(sum(employer_offer, na.rm = TRUE), 1L),
    max_age = max(TAGE, na.rm = TRUE),
    .groups = "drop"
  )

# Age percentages from person-month counts (divide by 12 for persons)
age_counts <- sipp %>%
  filter(household_year %in% sipp_hh$household_year) %>%
  group_by(household_year) %>%
  summarize(
    n_0to17  = sum(TAGE < 18) / 12,
    n_18to34 = sum(TAGE >= 18 & TAGE < 35) / 12,
    n_35to54 = sum(TAGE >= 35 & TAGE < 55) / 12,
    .groups = "drop"
  )

sipp_hh <- sipp_hh %>%
  left_join(sipp_agg, by = "household_year") %>%
  left_join(age_counts, by = "household_year") %>%
  mutate(
    perc_0to17  = if_else(household_size > 0, n_0to17 / household_size, 0),
    perc_18to34 = if_else(household_size > 0, n_18to34 / household_size, 0),
    perc_35to54 = if_else(household_size > 0, n_35to54 / household_size, 0)
  ) %>%
  select(-n_0to17, -n_18to34, -n_35to54)


# Transition detection -----------------------------------------------------

cat("  Detecting market transitions...\n")

# Who participated in nongroup each year?
ng_participants <- sipp_hh %>%
  filter(!is.na(STARTNONGROUP)) %>%
  select(household_id, year) %>%
  mutate(on_nongroup = TRUE)

# entered: wasn't on nongroup in year t-1, is on nongroup in year t
sipp_hh <- sipp_hh %>%
  mutate(entered = 0L, exited = 0L)

for (yr in c(2014, 2015)) {
  # Entrants: not on nongroup in yr-1, but on nongroup in yr
  ng_this <- ng_participants %>% filter(year == yr) %>% pull(household_id)
  ng_prev <- ng_participants %>% filter(year == yr - 1) %>% pull(household_id)
  entrants <- setdiff(ng_this, ng_prev)
  # Only mark those who also have a yr-1 observation showing no nongroup
  no_ng_prev <- sipp_hh %>% filter(year == yr - 1, is.na(STARTNONGROUP)) %>% pull(household_id)
  entrants <- intersect(entrants, no_ng_prev)
  sipp_hh <- sipp_hh %>%
    mutate(entered = if_else(household_id %in% entrants & year == yr, 1L, entered))
}

for (yr in c(2013, 2014)) {
  # Exited mid-year: on nongroup in yr but ENDNONGROUP < 12
  ng_yr <- ng_participants %>% filter(year == yr) %>% pull(household_id)
  mid_exit <- sipp_hh %>%
    filter(year == yr, household_id %in% ng_yr, ENDNONGROUP < 12, !is.na(ENDNONGROUP)) %>%
    pull(household_id)
  sipp_hh <- sipp_hh %>%
    mutate(exited = if_else(household_id %in% mid_exit & year == yr, 1L, exited))

  # Exited by next year: on nongroup in yr, not on nongroup in yr+1
  ng_next <- ng_participants %>% filter(year == yr + 1) %>% pull(household_id)
  full_exit <- setdiff(ng_yr, ng_next)
  # Only mark those who have a yr+1 observation showing no nongroup
  no_ng_next <- sipp_hh %>% filter(year == yr + 1, is.na(STARTNONGROUP)) %>% pull(household_id)
  full_exit <- intersect(full_exit, no_ng_next)
  sipp_hh <- sipp_hh %>%
    mutate(exited = if_else(household_id %in% full_exit & year == yr, 1L, exited))
}


# Insurance plan before entry / after exit ---------------------------------

# Default to 1 (nongroup) — will be overwritten for actual transitions
sipp_hh <- sipp_hh %>%
  mutate(new_plan_after_exit = 1L, old_plan_before_enter = 1L)

# Look up adjacent year's January insurance type
# Build a lookup from household_id + year -> jan column value
jan_lookup <- sipp_hh %>%
  mutate(
    jan_value = case_when(
      year == 2013 ~ jan2013,
      year == 2014 ~ jan2014,
      year == 2015 ~ jan2015
    )
  ) %>%
  select(household_id, year, jan_value)

for (yr in c(2013, 2014)) {
  # Exiters in yr: look at jan of yr+1
  exit_ids <- sipp_hh %>% filter(exited == 1, year == yr) %>% pull(household_id)
  next_jan <- jan_lookup %>% filter(year == yr + 1, household_id %in% exit_ids) %>%
    select(household_id, jan_next = jan_value)
  sipp_hh <- sipp_hh %>%
    left_join(next_jan, by = "household_id", relationship = "many-to-one") %>%
    mutate(new_plan_after_exit = if_else(
      exited == 1 & year == yr & !is.na(jan_next), as.integer(jan_next), new_plan_after_exit
    )) %>%
    select(-jan_next)

  # Entrants in yr+1: look at jan of yr
  enter_ids <- sipp_hh %>% filter(entered == 1, year == yr + 1) %>% pull(household_id)
  prev_jan <- jan_lookup %>% filter(year == yr, household_id %in% enter_ids) %>%
    select(household_id, jan_prev = jan_value)
  sipp_hh <- sipp_hh %>%
    left_join(prev_jan, by = "household_id", relationship = "many-to-one") %>%
    mutate(old_plan_before_enter = if_else(
      entered == 1 & year == (yr + 1) & !is.na(jan_prev), as.integer(jan_prev), old_plan_before_enter
    )) %>%
    select(-jan_prev)
}


# Classify transitions (in-market vs out-of-market) ------------------------

# Insurance type codes: 4=ESI, 5=Medicaid, 7=uninsured
sipp_hh <- sipp_hh %>%
  mutate(
    entered_market = case_when(
      old_plan_before_enter %in% c(4, 5) ~ 1L,
      old_plan_before_enter == 7 ~ 0L,
      TRUE ~ NA_integer_
    ),
    entered_market = case_when(
      employer_offer == 1 & entered_market == 0L ~ 1L,
      TMOVER >= 4 & !is.na(TMOVER) & entered_market == 0L ~ 1L,
      TRUE ~ entered_market
    ),
    exited_market = case_when(
      new_plan_after_exit %in% c(4, 5) ~ 1L,
      new_plan_after_exit == 7 ~ 0L,
      TRUE ~ NA_integer_
    ),
    exited_market = case_when(
      employer_offer == 1 & exited_market == 0L ~ 1L,
      TMOVER >= 4 & !is.na(TMOVER) & exited_market == 0L ~ 1L,
      TRUE ~ exited_market
    )
  )


# Final variables ----------------------------------------------------------

sipp_hh <- sipp_hh %>%
  mutate(
    perc_male = percentmale,
    perc_asian = percentasian,
    perc_black = percentblack,
    perc_hispanic = percenthispanic,
    perc_other = percentother,
    FPL_bracket = case_when(
      FPL <= 1.38 ~ "138orless",
      FPL <= 2.50 ~ "138to250",
      FPL <= 4.00 ~ "250to400",
      TRUE        ~ "400ormore"
    ),
    transitioned = pmax(entered_market, exited_market, na.rm = TRUE)
  )


# Write output -------------------------------------------------------------

out_cols <- c("household_id", "year", "household_year", "FPL", "FPL_bracket",
              "household_size", "weight", "max_age", "employer_offer",
              "perc_male", "perc_0to17", "perc_18to34", "perc_35to54",
              "perc_asian", "perc_black", "perc_hispanic", "perc_other",
              "entered", "exited", "entered_market", "exited_market",
              "transitioned")

sipp_out <- sipp_hh %>% select(all_of(out_cols))

write_csv(sipp_out, "data/output/sipp_households.csv")
cat("  Wrote", nrow(sipp_out), "SIPP household-year observations to data/output/sipp_households.csv\n")

rm(sipp, sipp_hh, sipp_out, sipp_agg, age_counts, hh_by_year, hh_ever_nongroup,
   ng_participants, jan_lookup, id_map, old_ids)
