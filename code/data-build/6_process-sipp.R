# 6_process-sipp.R
# Impute unauthorized immigrant status and employer offer onto ACS records
# using 2008 SIPP panel data. Two logit models estimated on SIPP, predicted
# onto ACS non-citizens to create immigration and employer offer indicators.
#
# Input:  data/input/SIPP Data/*.sas, *.zip (SIPP 2008 panel)
#         data/input/ACS Data/input20142.sas, ipumsCA.gz (ACS for immigration)
#         data/input/ACS Data/input2014.sas, ipumsCA.gz  (ACS for employer offer)
# Output: data/output/acs_immigration.csv
#         data/output/acs_emp_offer.csv

set.seed(5)


# Helper: parse SAS input + read fixed-width file ---------------------------

read_fwf_sas <- function(sas_path, data_path, is_gzip = FALSE, is_zip = FALSE) {
  input <- parse.SAScii(sas_path)
  col_positions <- fwf_widths(input$width, col_names = input$varname)

  all_numeric <- cols(.default = "d")

  if (is_gzip) {
    df <- read_fwf(data_path, col_positions, col_types = all_numeric)
  } else if (is_zip) {
    tmp <- tempdir()
    extracted <- unzip(data_path, exdir = tmp)
    df <- read_fwf(extracted[1], col_positions, col_types = all_numeric)
    file.remove(extracted)
  } else {
    df <- read_fwf(data_path, col_positions, col_types = all_numeric)
  }
  as_tibble(df)
}


# ============================================================================
# PART A: Immigration Status Imputation
# ============================================================================

cat("  Loading ACS data (immigration section)...\n")
acs <- read_fwf_sas("data/input/SIPP Data/input20142.sas",
                     "data/input/ACS Data/ipumsCA.gz", is_gzip = TRUE)

cat("  Loading SIPP TM2 (immigration module)...\n")
sipp_tm2 <- read_fwf_sas("data/input/SIPP Data/inputSIPP_tm2.sas",
                          "data/input/SIPP Data/p08putm2.zip", is_zip = TRUE) %>%
  mutate(person_id = paste0(SSUID, EPPPNUM)) %>%
  select(person_id, TBRSTATE, ECITIZNT, TIMSTAT, EADJUST, TADYEAR, TMOVEUS)

cat("  Loading SIPP Core Wave 2...\n")
sipp <- read_fwf_sas("data/input/SIPP Data/inputSIPP_core.sas",
                      "data/input/SIPP Data/l08puw2.zip", is_zip = TRUE) %>%
  filter(SREFMON == 4) %>%
  mutate(person_id = paste0(SSUID, EPPPNUM)) %>%
  left_join(sipp_tm2, by = "person_id")
rm(sipp_tm2)


# Filter to non-citizens ----------------------------------------------------

sipp <- filter(sipp, ECITIZNT == 2)
acs  <- filter(acs, CITIZEN == 3)


# Define unauthorized status in SIPP ----------------------------------------

sipp <- sipp %>%
  mutate(
    unauthorized = 1L,
    # Permanent residents are authorized
    unauthorized = ifelse(EADJUST == 1 | TIMSTAT == 1, 0L, unauthorized),
    # Federal program participants are authorized
    unauthorized = ifelse(ESSSELF == 1 | ESSISELF == 1 | EWICYN == 1 |
                            EASST01 == 1 | RCUTYP08 == 1 | RCUTYP20 == 1 |
                            RCUTYP27 == 1, 0L, unauthorized),
    # College students are authorized
    unauthorized = ifelse(EENLEVEL %in% 3:8, 0L, unauthorized)
  )


# Harmonize covariates -------------------------------------------------------

# Duration in US
tmoveus_map <- c(
  `1` = 49, `2` = 44.5, `3` = 37.5, `4` = 33, `5` = 29.5, `6` = 27,
  `7` = 24.5, `8` = 22, `9` = 19.5, `10` = 17.5, `11` = 15.5, `12` = 13.5,
  `13` = 11.5, `14` = 10, `15` = 9, `16` = 8, `17` = 6.5, `18` = 5,
  `19` = 4, `20` = 3, `21` = 2, `22` = 1
)
sipp$duration_in_US <- tmoveus_map[as.character(sipp$TMOVEUS)]
acs$duration_in_US <- as.numeric(acs$YEAR) - as.numeric(acs$YRIMMIG)

# Birth place
sipp <- sipp %>%
  mutate(birth_place = case_when(
    TBRSTATE == 562 ~ "North America",
    TBRSTATE == 563 ~ "Northwest Europe",
    TBRSTATE == 564 ~ "Southeast Europe",
    TBRSTATE == 565 ~ "East Asia",
    TBRSTATE %in% c(566, 567) ~ "West_SE Asia, Australia, New Zealand",
    TBRSTATE == 568 ~ "Africa",
    TBRSTATE == 569 ~ "Caribbean",
    TBRSTATE == 570 ~ "Central America",
    TBRSTATE == 571 ~ "South America"
  ))

acs <- acs %>%
  mutate(birth_place = case_when(
    BPL == 100 ~ "West_SE Asia, Australia, New Zealand",
    BPL %in% c(150, 160, 200) ~ "North America",
    BPL == 210 ~ "Central America",
    BPL %in% c(250, 260) ~ "Caribbean",
    BPL %in% c(299, 300) ~ "South America",
    (BPL >= 400 & BPL < 430) | BPL %in% c(450, 452, 453, 454, 455) ~ "Northwest Europe",
    (BPL >= 430 & BPL <= 441) | BPL %in% c(451, 456, 457, 458) |
      (BPL >= 459 & BPL < 500) ~ "Southeast Europe",
    BPL %in% c(500, 501, 502, 509) ~ "East Asia",
    BPL >= 510 & BPL < 999 & BPL != 600 ~ "West_SE Asia, Australia, New Zealand",
    BPL == 600 ~ "Africa"
  ))

# English proficiency
sipp$English <- ifelse(sipp$EHOWWELL %in% c(3, 4), 0L, 1L)
acs$English  <- ifelse(acs$SPEAKENG %in% c(0, 1, 6), 0L, 1L)

# FPL
sipp$FPL <- pmax(0, pmin(sipp$THTOTINC / sipp$RHPOV, 5))
acs$FPL  <- acs$POVERTY / 100

# Hispanic
sipp$Hispanic <- ifelse(sipp$EORIGIN == 1, 10, 0)
acs$Hispanic  <- ifelse(acs$HISPAN == 0, 0, 1)

# Employment
sipp$employed <- as.integer(sipp$EPDJBTHN == 1)
acs$employed  <- as.integer(acs$EMPSTAT == 1)

# Age
sipp$AGE <- sipp$TAGE

# Gender (1=male, 0=female)
sipp$SEX <- ifelse(sipp$ESEX == 2, 0L, as.integer(sipp$ESEX))
acs$SEX  <- ifelse(acs$SEX == 2, 0L, as.integer(acs$SEX))

# Race
sipp <- sipp %>%
  mutate(Race = case_when(
    ERACE == 1 ~ "White", ERACE == 2 ~ "Black",
    ERACE == 3 ~ "Asian", ERACE == 4 ~ "Other"
  ))
acs <- acs %>%
  mutate(Race = case_when(
    RACE == 1 ~ "White", RACE == 2 ~ "Black",
    RACE %in% c(4, 5, 6) ~ "Asian", RACE %in% c(3, 7, 8, 9) ~ "Other"
  ))

# Industry (4 categories for immigration section)
classify_industry_4 <- function(ind) {
  case_when(
    ind < 1000  ~ "agriculture",
    ind < 4000  ~ "manufacturing",
    ind < 9000  ~ "services",
    ind >= 9000 ~ "other",
    TRUE        ~ "not employed"
  )
}
sipp$industry <- classify_industry_4(sipp$EJBIND1)
acs$industry  <- classify_industry_4(acs$IND)

# Marital status
sipp$married <- as.integer(sipp$EMS %in% c(1, 2))
acs$married  <- as.integer(acs$MARST %in% c(1, 2))

# Education
sipp <- sipp %>%
  mutate(education_level = case_when(
    EEDUCATE < 39 ~ "lt_high_school",
    EEDUCATE == 39 ~ "high_school",
    EEDUCATE > 39 ~ "gt_high_school"
  ))
acs <- acs %>%
  mutate(education_level = case_when(
    EDUC < 6  ~ "lt_high_school",
    EDUC == 6 ~ "high_school",
    EDUC > 6  ~ "gt_high_school"
  ))

# Family size
sipp$FAMSIZE <- sipp$EFNP

# Insurance
sipp$uninsured <- as.integer(sipp$EHIMTH == 2 & sipp$ECRMTH == 2 & sipp$ECDMTH == 2)
acs$uninsured  <- as.integer(acs$HCOVANY == 1)

# Home ownership
sipp$home_owner <- as.integer(sipp$ETENURE == 1)
acs$home_owner  <- as.integer(acs$OWNERSHP == 1)


# Logit 1: Predict unauthorized status --------------------------------------

cat("  Fitting unauthorized status logit...\n")
spec_unauth <- unauthorized ~ duration_in_US + birth_place + English + FPL +
  employed + AGE + SEX + Hispanic + Race + as.factor(industry) + married +
  as.factor(education_level) + FAMSIZE + uninsured + home_owner

unauth_logit <- glm(spec_unauth, data = sipp, family = binomial(link = "logit"))

# Predict onto ACS and sample
unique_years <- length(unique(acs$YEAR))
unauth_pred <- predict(unauth_logit, newdata = acs, type = "response")
target_size <- unique_years * round(2820000 / (sum(acs$PERWT) / 100) * nrow(acs))
unauth_idx <- sample(seq_len(nrow(acs)), size = target_size,
                     replace = FALSE, prob = unauth_pred)

acs$undocumented_immigrant <- 0L
acs$undocumented_immigrant[unauth_idx] <- 1L
cat("    Imputed", sum(acs$undocumented_immigrant), "undocumented immigrants\n")


# Logit 2: Predict permanent resident status ---------------------------------

cat("  Fitting permanent resident logit...\n")
sipp_auth <- sipp %>%
  filter(unauthorized == 0) %>%
  mutate(permanent_resident = as.integer(EADJUST == 1 | TIMSTAT == 1))

spec_perm <- permanent_resident ~ duration_in_US + birth_place + English + FPL +
  employed + AGE + SEX + Hispanic + Race + as.factor(industry) + married +
  as.factor(education_level) + FAMSIZE + uninsured + home_owner

perm_logit <- glm(spec_perm, data = sipp_auth, family = binomial(link = "logit"))

# Predict onto ACS authorized non-citizens
acs_auth <- acs %>% filter(undocumented_immigrant == 0)
perm_pred <- predict(perm_logit, newdata = acs_auth, type = "response")
prop_perm <- sum(sipp_auth$permanent_resident) / nrow(sipp_auth)
target_perm <- round(prop_perm * nrow(acs_auth))
perm_idx <- sample(seq_len(nrow(acs_auth)), size = target_perm,
                   replace = FALSE, prob = perm_pred)

acs$permanent_resident <- 0L
acs$permanent_resident[which(acs$undocumented_immigrant == 0)[perm_idx]] <- 1L

acs$temporary_resident <- as.integer(
  acs$undocumented_immigrant == 0 & acs$permanent_resident == 0
)


# Save immigration imputation -----------------------------------------------

acs_immigration <- acs %>%
  mutate(immigration_id = paste(HIUID, PERNUM, AGE, YEAR, sep = "_")) %>%
  select(immigration_id, undocumented_immigrant, permanent_resident,
         temporary_resident, duration_in_US)

fwrite(acs_immigration, "data/output/acs_immigration.csv")
cat("  Immigration imputation saved:", nrow(acs_immigration), "records\n")

rm(sipp, acs, sipp_auth, acs_auth, unauth_logit, perm_logit)
gc()


# ============================================================================
# PART B: Employer Offer Imputation
# ============================================================================

cat("  Loading ACS data (employer offer section)...\n")
acs <- read_fwf_sas("data/input/SIPP Data/input2014.sas",
                     "data/input/ACS Data/ipumsCA.gz", is_gzip = TRUE)

cat("  Loading SIPP TM6 (employer offer module)...\n")
sipp_tm6 <- read_fwf_sas("data/input/SIPP Data/inputSIPP_tm6.sas",
                          "data/input/SIPP Data/p08putm6.zip", is_zip = TRUE) %>%
  mutate(person_id = paste0(SSUID, EPPPNUM)) %>%
  select(person_id, TAMTPLAN, EHEALPLA, ENOTPLAN, ECOVMEMB,
         ENOELIG1, ENOELIG2, ENOELIG3)

cat("  Loading SIPP Core Wave 6...\n")
sipp <- read_fwf_sas("data/input/SIPP Data/inputSIPP_core.sas",
                      "data/input/SIPP Data/l08puw6.zip", is_zip = TRUE) %>%
  filter(SREFMON == 4) %>%
  mutate(person_id = paste0(SSUID, EPPPNUM)) %>%
  left_join(sipp_tm6, by = "person_id")
rm(sipp_tm6)


# Insurance and employment indicators ---------------------------------------

sipp$uninsured <- as.integer(sipp$EHIMTH == 2 & sipp$ECRMTH == 2 & sipp$ECDMTH == 2)
sipp$ESI <- as.integer(sipp$EHEMPLY %in% c(1, 2, 3))
sipp$worker <- as.integer(sipp$EPDJBTHN == 1)

acs$uninsured <- as.integer(acs$HCOVANY == 1)
acs$ESI <- as.integer(acs$HINSEMP == 2)
acs$worker <- as.integer(acs$EMPSTAT == 1)


# Filter to target households -----------------------------------------------
# No ESI, at least one uninsured, at least one worker

sipp_hh <- sipp %>%
  group_by(SSUID) %>%
  summarize(number_ESI = sum(ESI), number_uninsured = sum(uninsured),
            number_workers = sum(worker), .groups = "drop") %>%
  mutate(impute = as.integer(number_ESI == 0 & number_uninsured > 0 & number_workers > 0))

acs_hh <- acs %>%
  group_by(HIUID) %>%
  summarize(number_ESI = sum(ESI), number_uninsured = sum(uninsured),
            number_workers = sum(worker), .groups = "drop") %>%
  mutate(impute = as.integer(number_ESI == 0 & number_uninsured > 0 & number_workers > 0))

sipp <- sipp %>% filter(SSUID %in% sipp_hh$SSUID[sipp_hh$impute == 1])
acs  <- acs %>% filter(HIUID %in% acs_hh$HIUID[acs_hh$impute == 1])


# Employer offer definition --------------------------------------------------

sipp <- sipp %>%
  mutate(employer_offer = case_when(
    EHEALPLA == 1 & ENOTPLAN == 3 ~ 1L,
    EHEALPLA == 2 | ENOTPLAN %in% c(1, 2, 4) ~ 0L,
    TRUE ~ NA_integer_
  ))


# Harmonize covariates (employer offer section) ------------------------------

sipp$FPL <- pmax(0, pmin(sipp$THTOTINC / sipp$RHPOV, 5))
acs$FPL  <- acs$POVERTY / 100

sipp$Hispanic <- ifelse(sipp$EORIGIN == 1, 10, 0)
acs$Hispanic  <- ifelse(acs$HISPAN == 0, 0, 1)

sipp$AGE <- sipp$TAGE
sipp$SEX <- ifelse(sipp$ESEX == 2, 0L, as.integer(sipp$ESEX))
acs$SEX  <- ifelse(acs$SEX == 2, 0L, as.integer(acs$SEX))

sipp <- sipp %>%
  mutate(Race = case_when(
    ERACE == 1 ~ "White", ERACE == 2 ~ "Black",
    ERACE == 3 ~ "Asian", ERACE == 4 ~ "Other"
  ))
acs <- acs %>%
  mutate(Race = case_when(
    RACE == 1 ~ "White", RACE == 2 ~ "Black",
    RACE %in% c(4, 5, 6) ~ "Asian", RACE %in% c(3, 7, 8, 9) ~ "Other"
  ))

# Industry (10 categories for employer offer section)
classify_industry_10 <- function(ind) {
  case_when(
    ind <= 290 ~ "agriculture",
    (ind >= 370 & ind < 500) | (ind >= 700 & ind < 1000) ~ "mining_construction",
    ind >= 1000 & ind < 4000 ~ "manufacturing",
    ind >= 4000 & ind < 6000 ~ "retail_wholesale",
    (ind >= 6000 & ind < 6400) | (ind >= 500 & ind < 700) ~ "transportation_utilities",
    ind >= 6800 & ind < 7200 ~ "finance",
    ind >= 7200 & ind < 8500 ~ "services",
    ind >= 8500 & ind < 8700 ~ "leisure",
    ind >= 9300 ~ "government",
    TRUE ~ "other"
  )
}
sipp$industry <- classify_industry_10(sipp$EJBIND1)
acs$industry  <- classify_industry_10(acs$IND)

sipp <- sipp %>%
  mutate(education_level = case_when(
    EEDUCATE < 39 ~ "lt_high_school",
    EEDUCATE == 39 ~ "high_school",
    EEDUCATE > 39 ~ "gt_high_school"
  ))
acs <- acs %>%
  mutate(education_level = case_when(
    EDUC < 6  ~ "lt_high_school",
    EDUC == 6 ~ "high_school",
    EDUC > 6  ~ "gt_high_school"
  ))

sipp$FAMSIZE <- sipp$EFNP

sipp$hours_worked <- ifelse(sipp$EHRSALL > 0, sipp$EHRSALL, NA_real_)
acs$hours_worked  <- ifelse(acs$UHRSWORK > 0, acs$UHRSWORK, NA_real_)


# Logit 3: Predict employer offer -------------------------------------------

cat("  Fitting employer offer logit...\n")
sipp_workers <- sipp %>% filter(!is.na(employer_offer))
acs_workers  <- acs %>% filter(worker == 1)

spec_offer <- employer_offer ~ FPL + Hispanic + AGE + SEX + Race +
  industry + education_level + FAMSIZE + hours_worked

offer_logit <- glm(spec_offer, data = sipp_workers, family = binomial(link = "logit"))

# Predict onto ACS workers with stochastic imputation
offer_pred <- predict(offer_logit, newdata = acs_workers, type = "response")
acs_workers$employer_offer <- as.integer(offer_pred - runif(length(offer_pred)) > 0)

# Aggregate to household level
hh_offers <- acs_workers %>%
  group_by(HIUID) %>%
  summarize(total_offers = sum(employer_offer, na.rm = TRUE), .groups = "drop")

acs <- acs %>%
  left_join(hh_offers, by = "HIUID") %>%
  mutate(
    access_to_emp_offer = as.integer(coalesce(total_offers, 0L) > 0)
  ) %>%
  select(-total_offers)

# Individual-level offer
acs$employer_offer <- NA_integer_
acs_worker_idx <- which(acs$worker == 1)
acs$employer_offer[acs_worker_idx] <- acs_workers$employer_offer


# Save employer offer imputation ---------------------------------------------

acs_emp_offer <- acs %>%
  mutate(immigration_id = paste(HIUID, PERNUM, AGE, YEAR, sep = "_")) %>%
  select(immigration_id, employer_offer, access_to_emp_offer)

fwrite(acs_emp_offer, "data/output/acs_emp_offer.csv")
cat("  Employer offer imputation saved:", nrow(acs_emp_offer), "records\n")

rm(sipp, acs, acs_workers, sipp_workers, offer_logit)
gc()

cat("Step 6 complete.\n")
