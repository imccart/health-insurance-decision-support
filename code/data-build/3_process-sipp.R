# 3_process-sipp.R
# Fit SIPP-based logit models used downstream:
#   (a) immigration_logit     - P(undocumented | non-citizen demographics)
#   (b) emp_offer_logit       - P(access to affordable ESI | demographics)
#   (c) transition_logit      - P(entered/exited nongroup market)
#
# (a) and (b) use SIPP 2008 panel waves with topical modules (TM2 immigration,
# TM6 employer offer). They are applied to ACS non-citizens in step 4 to
# filter out ACA-ineligible respondents.
#
# (c) uses SIPP 2014 waves 1-3 (years 2013-2015) to build a HH-year panel
# with nongroup market entry/exit flags. Used in step 5 to filter ACS
# uninsured down to the "consistent uninsured" subsample.
#
# Outputs:
#   data/output/sipp_immigration_logit.rds
#   data/output/sipp_emp_offer_logit.rds
#   data/output/sipp_transition_logit.rds
#   data/output/sipp_households.csv  (HH-year panel from SIPP 2014 with flags)

set.seed(5)

sipp_dir <- "data/input/SIPP Data"
acs_dir  <- "data/input/ACS Data"


# read_fwf_sas(): parse SAS dictionary + read fixed-width -------------------
# Used for SIPP 2008 Core (.zip) and TM2 / TM6 modules.
read_fwf_sas <- function(sas_path, data_path, is_gzip = FALSE, is_zip = FALSE) {
  input <- parse.SAScii(sas_path)
  col_positions <- fwf_widths(input$width, col_names = input$varname)
  if (is_gzip) {
    read_fwf(data_path, col_positions, col_types = cols(.default = "d"))
  } else if (is_zip) {
    tmp <- tempdir(); extracted <- unzip(data_path, exdir = tmp)
    df <- read_fwf(extracted[1], col_positions, col_types = cols(.default = "d"))
    file.remove(extracted); df
  } else {
    read_fwf(data_path, col_positions, col_types = cols(.default = "d"))
  }
}


# =============================================================================
# (a) IMMIGRATION LOGIT — from SIPP 2008 TM2
# =============================================================================
cat("  (a) Fitting immigration logit on SIPP 2008 TM2...\n")

sipp_tm2 <- read_fwf_sas(file.path(sipp_dir, "inputSIPP_tm2.sas"),
                          file.path(sipp_dir, "p08putm2.zip"), is_zip = TRUE) %>%
  mutate(person_id = paste0(SSUID, EPPPNUM)) %>%
  select(person_id, TBRSTATE, ECITIZNT, TIMSTAT, EADJUST, TADYEAR, TMOVEUS)

sipp_w2 <- read_fwf_sas(file.path(sipp_dir, "inputSIPP_core.sas"),
                         file.path(sipp_dir, "l08puw2.zip"), is_zip = TRUE) %>%
  filter(SREFMON == 4) %>%
  mutate(person_id = paste0(SSUID, EPPPNUM)) %>%
  left_join(sipp_tm2, by = "person_id") %>%
  filter(ECITIZNT == 2)    # non-citizens only

# "Unauthorized" status flag: baseline TRUE for non-citizens, cleared by
# several authorized-status signals
sipp_w2 <- sipp_w2 %>%
  mutate(unauthorized = as.integer(
    !(EADJUST == 1 | TIMSTAT == 1 |
      ESSSELF == 1 | ESSISELF == 1 | EWICYN == 1 | EASST01 == 1 |
      RCUTYP08 == 1 | RCUTYP20 == 1 | RCUTYP27 == 1 |
      EENLEVEL %in% 3:8)
  ))

# Harmonize covariates (shared coding for SIPP and ACS; done here on SIPP,
# again in step 4 on ACS)
tmoveus_map <- c(
  `1` = 49, `2` = 44.5, `3` = 37.5, `4` = 33, `5` = 29.5, `6` = 27,
  `7` = 24.5, `8` = 22, `9` = 19.5, `10` = 17.5, `11` = 15.5, `12` = 13.5,
  `13` = 11.5, `14` = 10, `15` = 9, `16` = 8, `17` = 6.5, `18` = 5,
  `19` = 4, `20` = 3, `21` = 2, `22` = 1
)
sipp_w2 <- sipp_w2 %>% mutate(
  duration_in_US = tmoveus_map[as.character(TMOVEUS)],
  birth_place = case_when(
    TBRSTATE == 562 ~ "North America",
    TBRSTATE == 563 ~ "Northwest Europe",
    TBRSTATE == 564 ~ "Southeast Europe",
    TBRSTATE == 565 ~ "East Asia",
    TBRSTATE %in% c(566, 567) ~ "West_SE Asia, Australia, New Zealand",
    TBRSTATE == 568 ~ "Africa",
    TBRSTATE == 569 ~ "Caribbean",
    TBRSTATE == 570 ~ "Central America",
    TBRSTATE == 571 ~ "South America"
  ),
  English  = as.integer(!EHOWWELL %in% c(3, 4)),
  FPL      = pmax(0, pmin(THTOTINC / RHPOV, 5)),
  Hispanic = ifelse(EORIGIN == 1, 10L, 0L),
  employed = as.integer(EPDJBTHN == 1),
  AGE      = TAGE,
  SEX      = ifelse(ESEX == 2, 0L, as.integer(ESEX)),
  Race = case_when(ERACE == 1 ~ "White", ERACE == 2 ~ "Black",
                    ERACE == 3 ~ "Asian", ERACE == 4 ~ "Other"),
  industry = case_when(
    EJBIND1 < 1000  ~ "agriculture",
    EJBIND1 < 4000  ~ "manufacturing",
    EJBIND1 < 9000  ~ "services",
    EJBIND1 >= 9000 ~ "other",
    TRUE            ~ "not employed"
  ),
  married   = as.integer(EMS %in% c(1, 2)),
  education_level = case_when(
    EEDUCATE < 39  ~ "lt_high_school",
    EEDUCATE == 39 ~ "high_school",
    EEDUCATE > 39  ~ "gt_high_school"
  ),
  FAMSIZE    = EFNP,
  uninsured  = as.integer(EHIMTH == 2 & ECRMTH == 2 & ECDMTH == 2),
  home_owner = as.integer(ETENURE == 1)
)

immigration_logit <- glm(
  unauthorized ~ duration_in_US + birth_place + English + FPL + employed +
    AGE + SEX + Hispanic + Race + as.factor(industry) + married +
    as.factor(education_level) + FAMSIZE + uninsured + home_owner,
  data = sipp_w2, family = binomial
)

# Save also the fitted residual-pool for later rank-based sampling if needed
saveRDS(immigration_logit, "data/output/sipp_immigration_logit.rds")
rm(sipp_tm2, sipp_w2); gc(verbose = FALSE)


# =============================================================================
# (b) EMPLOYER-OFFER LOGIT — from SIPP 2008 TM6
# =============================================================================
cat("  (b) Fitting employer-offer logit on SIPP 2008 TM6...\n")

sipp_tm6 <- read_fwf_sas(file.path(sipp_dir, "inputSIPP_tm6.sas"),
                          file.path(sipp_dir, "p08putm6.zip"), is_zip = TRUE) %>%
  mutate(person_id = paste0(SSUID, EPPPNUM)) %>%
  select(person_id, TAMTPLAN, EHEALPLA, ENOTPLAN, ECOVMEMB,
         ENOELIG1, ENOELIG2, ENOELIG3)

sipp_w6 <- read_fwf_sas(file.path(sipp_dir, "inputSIPP_core.sas"),
                         file.path(sipp_dir, "l08puw6.zip"), is_zip = TRUE) %>%
  filter(SREFMON == 4) %>%
  mutate(person_id = paste0(SSUID, EPPPNUM)) %>%
  left_join(sipp_tm6, by = "person_id")

# "Offered ESI" flag: person has access to an affordable employer plan
sipp_w6 <- sipp_w6 %>%
  mutate(
    has_ESI   = as.integer(EHIMTH == 1 & (ECRMTH == 1 | ECDMTH == 1)),
    declined_offer = as.integer(EHEALPLA == 1 & ENOTPLAN == 2),
    offered_ESI = pmax(has_ESI, declined_offer, na.rm = TRUE)
  )

# Harmonize the same covariates (repeat the SIPP coding)
sipp_w6 <- sipp_w6 %>% mutate(
  FPL      = pmax(0, pmin(THTOTINC / RHPOV, 5)),
  AGE      = TAGE,
  SEX      = ifelse(ESEX == 2, 0L, as.integer(ESEX)),
  Race = case_when(ERACE == 1 ~ "White", ERACE == 2 ~ "Black",
                    ERACE == 3 ~ "Asian", ERACE == 4 ~ "Other"),
  industry = case_when(
    EJBIND1 < 1000  ~ "agriculture",
    EJBIND1 < 4000  ~ "manufacturing",
    EJBIND1 < 9000  ~ "services",
    EJBIND1 >= 9000 ~ "other",
    TRUE            ~ "not employed"
  ),
  married    = as.integer(EMS %in% c(1, 2)),
  education_level = case_when(
    EEDUCATE < 39  ~ "lt_high_school",
    EEDUCATE == 39 ~ "high_school",
    EEDUCATE > 39  ~ "gt_high_school"
  ),
  FAMSIZE    = EFNP,
  home_owner = as.integer(ETENURE == 1),
  Hispanic   = ifelse(EORIGIN == 1, 10L, 0L),
  employed   = as.integer(EPDJBTHN == 1)
)

emp_offer_logit <- glm(
  offered_ESI ~ FPL + AGE + SEX + Hispanic + Race + as.factor(industry) +
    married + as.factor(education_level) + FAMSIZE + home_owner + employed,
  data = sipp_w6, family = binomial
)

saveRDS(emp_offer_logit, "data/output/sipp_emp_offer_logit.rds")
rm(sipp_tm6, sipp_w6); gc(verbose = FALSE)


# =============================================================================
# (c) MARKET-TRANSITION LOGIT — from SIPP 2014 waves 1-3
# =============================================================================
cat("  (c) Fitting market-transition logit on SIPP 2014 waves 1-3...\n")

waves <- tibble(
  wave_num   = 1:3,
  year       = 2013:2015,
  wave_csv   = file.path(sipp_dir, paste0("pu2014w", 1:3, ".csv")),
  status_csv = file.path(sipp_dir, paste0("status", 2013:2015, ".csv"))
)

# Process one SIPP wave → HH-year with entered/exited market flags
process_wave <- function(wave_csv, status_csv, wave_num, year) {
  mover_col <- if (wave_num == 1) "RMOVER" else "TMOVER"
  cols_keep <- c("SSUID", "PNUM", "SWAVE", "MONTHCODE", "TAGE", "ESEX",
                  "ERACE", "EORIGIN", "WPFINWGT", "THTOTINC", "RHPOV",
                  mover_col,
                  "EYNOESI_COV", "EYNOESI_EXP", "EYNOESI_HTH",
                  "EYNOESI_ELS", "EYNOESI_UNH")
  wave <- read_csv(wave_csv, col_select = all_of(cols_keep),
                    show_col_types = FALSE) %>%
    mutate(SSUID = as.character(SSUID))
  if (wave_num == 1) wave <- wave %>% rename(TMOVER = RMOVER)

  hh <- wave %>%
    group_by(SSUID, MONTHCODE) %>%
    summarize(
      family_size     = n_distinct(PNUM),
      perc_male       = mean(ESEX == 1, na.rm = TRUE),
      perc_asian      = mean(ERACE == 3, na.rm = TRUE),
      perc_black      = mean(ERACE == 2, na.rm = TRUE),
      perc_hispanic   = mean(EORIGIN == 1, na.rm = TRUE),
      perc_other      = mean(ERACE == 4 & EORIGIN == 2, na.rm = TRUE),
      perc_0to17      = mean(TAGE <= 17, na.rm = TRUE),
      perc_18to34     = mean(TAGE >= 18 & TAGE <= 34, na.rm = TRUE),
      perc_35to54     = mean(TAGE >= 35 & TAGE <= 54, na.rm = TRUE),
      max_age         = max(TAGE, na.rm = TRUE),
      weight          = sum(WPFINWGT, na.rm = TRUE),
      THTOTINC        = sum(THTOTINC, na.rm = TRUE),
      RHPOV           = first(RHPOV),
      TMOVER          = max(TMOVER, na.rm = TRUE),
      .groups         = "drop"
    ) %>%
    mutate(FPL = pmax(0, THTOTINC / RHPOV),
           household_year = paste(SSUID, year, sep = "_"),
           year = year)

  # Enter/exit: status file has 0/1 flags per month × SSUID
  status <- read_csv(status_csv, show_col_types = FALSE) %>%
    mutate(SSUID = as.character(SSUID))
  hh %>% left_join(status, by = c("SSUID", "MONTHCODE"))
}

sipp_hh <- bind_rows(
  pmap(waves, function(wave_num, year, wave_csv, status_csv) {
    process_wave(wave_csv, status_csv, wave_num, year)
  })
)

# Collapse month-level to year-level: took place if transition happened in any month
sipp_hh <- sipp_hh %>%
  group_by(household_year, year, SSUID) %>%
  summarize(
    entered_market = max(entered, na.rm = TRUE),
    exited_market  = max(exited, na.rm = TRUE),
    transitioned   = pmax(entered_market, exited_market, na.rm = TRUE),
    family_size    = first(family_size),
    FPL            = first(FPL),
    FPL_bracket    = assign_bracket(first(FPL)),
    max_age        = first(max_age),
    weight         = first(weight),
    perc_male      = first(perc_male),
    perc_asian     = first(perc_asian),
    perc_black     = first(perc_black),
    perc_hispanic  = first(perc_hispanic),
    perc_other     = first(perc_other),
    perc_0to17     = first(perc_0to17),
    perc_18to34    = first(perc_18to34),
    perc_35to54    = first(perc_35to54),
    .groups = "drop"
  )

transition_logit <- glm(
  transitioned ~ FPL_bracket + family_size + perc_0to17 + perc_18to34 +
    perc_35to54 + perc_male + perc_asian + perc_black + perc_hispanic + perc_other,
  data   = sipp_hh %>% filter(!is.na(transitioned)),
  family = binomial
)

saveRDS(transition_logit,   "data/output/sipp_transition_logit.rds")
fwrite(sipp_hh,            "data/output/sipp_households.csv")

cat(sprintf("Step 3 complete: fit 3 logits; %d SIPP HH-years saved.\n",
            nrow(sipp_hh)))
