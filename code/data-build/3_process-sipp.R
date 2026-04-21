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

emp_offer_logit <- glm(
  offered_ESI ~ FPL + AGE + SEX + Hispanic + Race + as.factor(industry) +
    married + as.factor(education_level) + FAMSIZE + home_owner + employed,
  data = sipp_w6, family = binomial
)

saveRDS(emp_offer_logit, "data/output/sipp_emp_offer_logit.rds")
rm(sipp_tm6, sipp_w6); gc(verbose = FALSE)

cat("Step 3 complete: fit 2 logits (immigration, emp-offer).\n")
rm(immigration_logit, emp_offer_logit); gc(verbose = FALSE)
