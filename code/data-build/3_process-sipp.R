# 3_process-sipp.R
# Fit SIPP-based logit models used downstream:
#   (a) immigration_logit     - P(undocumented | non-citizen demographics)
#   (b) emp_offer_logit       - P(access to affordable ESI | demographics)
#
# Both use SIPP 2008 panel waves with topical modules (TM2 immigration,
# TM6 employer offer). Step 4 uses (a) on ACS non-citizens to drop predicted
# undocumented and (b) on all ACS individuals to identify those with predicted
# ESI access (offered-but-declined, which ACS doesn't observe directly).
#
# Outputs:
#   data/output/sipp_immigration_logit.rds
#   data/output/sipp_emp_offer_logit.rds

set.seed(5)

sipp_dir <- "data/input/SIPP Data"
acs_dir  <- "data/input/ACS Data"


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

## Workers-only fit: ESI is offered by employers, so only employed people
## can have an offer. Including non-workers (whose offered_ESI is mechanically
## 0) would pollute the logit with covariates that correlate with non-employment.
emp_offer_logit <- glm(
  offered_ESI ~ FPL + AGE + SEX + Hispanic + Race + as.factor(industry) +
    married + as.factor(education_level) + FAMSIZE + home_owner,
  data = sipp_w6 %>% filter(employed == 1), family = binomial
)

saveRDS(emp_offer_logit, "data/output/sipp_emp_offer_logit.rds")
rm(sipp_tm6, sipp_w6); gc(verbose = FALSE)


# =============================================================================
# (c) NONGROUP-TRANSITION LOGIT — from SIPP 2014 panel waves 1-3
# =============================================================================
# Predicts whether a household transitioned into or out of the individual
# market in a given year (RAND 2021 Appendix E). Used in step 4 to filter
# CC HH-years where the HH was likely not market-eligible. Uses SIPP 2014
# panel (calendar 2013-2015) since it tracks plan transitions across years.
# Plan codes (status20YY): 1=nongroup, 2=military, 3=Medicare, 4=ESI,
# 5=Medicaid/CHIP, 6=other, 7=uninsured.
cat("  (c) Fitting nongroup-transition logit on SIPP 2014 panel...\n")

read_wave <- function(pu_path, status_path, yr) {
  pu <- fread(pu_path)
  if ("RMOVER" %in% names(pu) && !"TMOVER" %in% names(pu)) {
    setnames(pu, "RMOVER", "TMOVER")
  }
  st <- fread(status_path)[, .(SSUID, PNUM, SWAVE, TAGE,
                                jan = get(paste0("jan", yr)),
                                dec = get(paste0("dec", yr)))]
  pu <- merge(pu, st, by = c("SSUID", "PNUM", "SWAVE", "TAGE"), all.x = TRUE)
  pu[, year := yr]
  pu[, .(SSUID, PNUM, MONTHCODE, year, TAGE, ESEX, ERACE, EORIGIN,
         THTOTINC, RHPOV, TMOVER, jan, dec,
         EYNOESI_COV, EYNOESI_EXP, EYNOESI_HTH, EYNOESI_ELS, EYNOESI_UNH)]
}

sipp_pm <- rbindlist(list(
  read_wave(file.path(sipp_dir, "pu2014w1.csv"),
            file.path(sipp_dir, "status2013.csv"), 2013),
  read_wave(file.path(sipp_dir, "pu2014w2.csv"),
            file.path(sipp_dir, "status2014.csv"), 2014),
  read_wave(file.path(sipp_dir, "pu2014w3.csv"),
            file.path(sipp_dir, "status2015.csv"), 2015)
))

# Person-month indicators (age uses & not |, fixing a bug in Saltzman's code)
sipp_pm[, `:=`(
  on_nongroup = as.integer(jan == 1 | dec == 1),
  young       = as.integer(TAGE <= 17),
  middle      = as.integer(TAGE >= 18 & TAGE <= 34),
  old         = as.integer(TAGE >= 35 & TAGE <= 54),
  male        = as.integer(ESEX == 1),
  asian       = as.integer(ERACE == 3),
  black       = as.integer(ERACE == 2),
  hispanic    = as.integer(EORIGIN == 1),
  other       = as.integer(ERACE == 4 & EORIGIN == 2),
  emp_offer_decline = as.integer(
    EYNOESI_COV == 1 | EYNOESI_EXP == 1 | EYNOESI_HTH == 1 |
    EYNOESI_ELS == 1 | EYNOESI_UNH == 1)
)]

# Household-head plan in January (lowest PNUM in HH = SIPP reference person)
hh_head_jan <- sipp_pm[MONTHCODE == 1,
                        .(jan_plan_head = jan[which.min(PNUM)]),
                        by = .(SSUID, year)]

# Aggregate to HH-year
sipp_hh <- sipp_pm[, .(
  household_size = .N / 12,
  perc_0to17     = mean(young),
  perc_18to34    = mean(middle),
  perc_35to54    = mean(old),
  perc_male      = mean(male),
  perc_asian     = mean(asian),
  perc_black     = mean(black),
  perc_hispanic  = mean(hispanic),
  perc_other     = mean(other),
  FPL            = pmax(0, mean(THTOTINC) / mean(RHPOV)),
  TMOVER         = max(TMOVER, na.rm = TRUE),
  employer_offer = as.integer(any(emp_offer_decline == 1, na.rm = TRUE)),
  ever_nongroup  = as.integer(any(on_nongroup == 1)),
  end_nongroup   = if (any(on_nongroup == 1)) max(MONTHCODE[on_nongroup == 1])
                   else NA_integer_
), by = .(SSUID, year)]
sipp_hh[is.infinite(TMOVER), TMOVER := NA_real_]
sipp_hh <- merge(sipp_hh, hh_head_jan, by = c("SSUID", "year"), all.x = TRUE)

# Keep only HHs in all 3 waves AND ever on nongroup somewhere in the panel
hh_in_all  <- sipp_hh[, .N, by = SSUID][N == 3, SSUID]
hh_ever_ng <- unique(sipp_hh[ever_nongroup == 1, SSUID])
sipp_hh <- sipp_hh[SSUID %in% intersect(hh_in_all, hh_ever_ng)]
setorder(sipp_hh, SSUID, year)

# Adjacent-year plan codes via shift; fallback to "still nongroup" (code 1)
sipp_hh[, `:=`(
  prev_ng  = shift(ever_nongroup,  1L, type = "lag"),
  next_ng  = shift(ever_nongroup,  1L, type = "lead"),
  prev_jan = shift(jan_plan_head,  1L, type = "lag"),
  next_jan = shift(jan_plan_head,  1L, type = "lead")
), by = SSUID]
sipp_hh[, entered  := as.integer(ever_nongroup == 1 & (is.na(prev_ng) | prev_ng == 0))]
sipp_hh[, exited   := as.integer(ever_nongroup == 1 &
                                 ((is.na(next_ng) | next_ng == 0) | end_nongroup < 12))]
sipp_hh[, old_plan := fifelse(entered == 1 & !is.na(prev_jan), prev_jan, 1)]
sipp_hh[, new_plan := fifelse(exited  == 1 & !is.na(next_jan), next_jan, 1)]

# entered_market: from old_plan; exited_market: from new_plan.
# 0 if from/to uninsured (7), 1 if from/to ESI/Medicaid (4,5), NA otherwise.
# Bumped to 1 if employer offer declined or HH moved (TMOVER >= 4).
sipp_hh[, entered_market := NA_integer_]
sipp_hh[old_plan == 7,            entered_market := 0L]
sipp_hh[old_plan %in% c(4, 5),    entered_market := 1L]
sipp_hh[!is.na(entered_market) & entered_market == 0L &
        (employer_offer == 1 | (!is.na(TMOVER) & TMOVER >= 4)),
        entered_market := 1L]

sipp_hh[, exited_market := NA_integer_]
sipp_hh[new_plan == 7,            exited_market := 0L]
sipp_hh[new_plan %in% c(4, 5),    exited_market := 1L]
sipp_hh[!is.na(exited_market) & exited_market == 0L &
        (employer_offer == 1 | (!is.na(TMOVER) & TMOVER >= 4)),
        exited_market := 1L]

sipp_hh[, transitioned := fcase(
  entered_market == 1L | exited_market == 1L, 1L,
  entered_market == 0L & is.na(exited_market), 0L,
  is.na(entered_market) & exited_market == 0L, 0L,
  entered_market == 0L & exited_market == 0L, 0L,
  default = NA_integer_
)]
sipp_hh[, FPL_bracket := assign_bracket(FPL)]

sipp_logit <- glm(
  transitioned ~ FPL_bracket + household_size +
    perc_0to17 + perc_18to34 + perc_35to54 +
    perc_male + perc_asian + perc_black + perc_hispanic + perc_other,
  data = sipp_hh[!is.na(transitioned)],
  family = binomial
)
saveRDS(sipp_logit, "data/output/sipp_logit.rds")
rm(sipp_pm, sipp_hh, hh_head_jan); gc(verbose = FALSE)
cat("  (c) done. n =", length(sipp_logit$y),
    "; mean(transitioned) =", round(mean(sipp_logit$y), 3), "\n")


cat("Step 3 complete: fit 3 logits (immigration, emp-offer, transition).\n")
rm(immigration_logit, emp_offer_logit, sipp_logit); gc(verbose = FALSE)
