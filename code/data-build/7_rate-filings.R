# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-26
## Description:   Process CMS rate filing PUFs for risk score and claims
##                regressions. Produces plan-level rsdata with experienced
##                claims, RA transfers, member months, and risk scores.
##                Ported from _old-repo/data-code/process.rate.data.R.

# Load rate filing PUFs ---------------------------------------------------

cat("Loading rate filing PUFs...\n")
load("data/input/Covered California/2014-2020.RData")

# Standardize column names across years -----------------------------------
# PUF_2020 uses EXP_REINS instead of EXP_REIN; harmonize.

harmonize_puf <- function(d, yr) {
  # Rename 2020-style columns
  if ("EXP_REINS" %in% names(d) && !"EXP_REIN" %in% names(d)) {
    d <- d %>% rename(EXP_REIN = EXP_REINS)
  }
  if ("PRJ_REINS" %in% names(d) && !"PRJ_REIN" %in% names(d)) {
    d <- d %>% rename(PRJ_REIN = PRJ_REINS)
  }
  # 2014-2015 have EXP_PRM_PMPM but not EXP_PLN_ADJ_INDX; later years reversed
  d$year <- yr
  d
}

keep_cols <- c("PLAN_ID", "STATE", "MARKET", "COMPANY", "ISSUER_ID",
               "METAL", "AV_METAL", "PLAN_TYPE", "EXCHANGE",
               "EXP_MM", "EXP_TP", "EXP_INC_CLM", "EXP_RSK_ADJ", "EXP_REIN",
               "PRJ_MM", "PRJ_TP", "PRJ_INC_CLM", "PRJ_RSK_ADJ", "PRJ_REIN",
               "EXP_INC_CLM_PMPM", "year")

puf_list <- list()
for (yr in 2014:2019) {
  obj_name <- paste0("PUF_", yr)
  d <- harmonize_puf(get(obj_name), yr)
  # Keep only columns that exist
  shared <- intersect(keep_cols, names(d))
  puf_list[[as.character(yr)]] <- d[, shared]
}
rm(list = ls(pattern = "^PUF_"))
gc(verbose = FALSE)

rdata <- bind_rows(puf_list)
rm(puf_list)
cat("  Combined PUFs:", nrow(rdata), "rows\n")


# Filter to CA, Individual, on-exchange -----------------------------------

rdata <- rdata %>%
  filter(STATE == "CA", MARKET == "Individual")

cat("  CA Individual:", nrow(rdata), "rows\n")

# Drop catastrophic and "Not Applicable" metals
rdata <- rdata %>%
  filter(!METAL %in% c("Catastrophic", "Not Applicable"))

cat("  After dropping CAT/NA metals:", nrow(rdata), "rows\n")


# Harmonize company names -------------------------------------------------

rdata <- rdata %>%
  mutate(
    insurer = case_when(
      grepl("Anthem", COMPANY)       ~ "Anthem",
      grepl("Blue Shield|Physician", COMPANY) ~ "Blue_Shield",
      grepl("Kaiser", COMPANY)       ~ "Kaiser",
      ISSUER_ID == 67138             ~ "Health_Net_HMO",
      ISSUER_ID == 99110             ~ "Health_Net_PPO",
      grepl("Health Net", COMPANY)   ~ "Health_Net_PPO",
      TRUE                           ~ "Small"
    ),
    # Simplified insurer for regressions (HN_HMO and HN_PPO → Health_Net)
    insurer_small = case_when(
      insurer %in% c("Health_Net_HMO", "Health_Net_PPO") ~ "Health_Net",
      TRUE ~ insurer
    )
  )

cat("  Insurers:", paste(sort(unique(rdata$insurer_small)), collapse = ", "), "\n")


# Build plan_id crosswalk -----------------------------------------------
# Rate filing plan_id = {INSURER_PREFIX}_{METAL_SUFFIX}[NETWORK_SUFFIX]
# PPO plans: ANT_BR, ANT_SIL, BS_G, HN_SIL, etc.
# HMO plans: ANT_SIL3, BS_G3, HN_BR3, etc. (suffix "3")
# Kaiser: always HMO, no suffix: KA_BR, KA_SIL, KA_G, KA_P
# Small insurers: SMALL_BR, SMALL_SIL, etc.

metal_map <- c(Bronze = "BR", Silver = "SIL", Gold = "G", Platinum = "P")

rdata <- rdata %>%
  mutate(
    insurer_prefix = case_when(
      insurer_small == "Anthem"      ~ "ANT",
      insurer_small == "Blue_Shield" ~ "BS",
      insurer_small == "Kaiser"      ~ "KA",
      insurer_small == "Health_Net"  ~ "HN",
      TRUE                           ~ "Small"
    ),
    metal_abbr = metal_map[METAL],
    # Network suffix: HMO gets "3" except for Kaiser (always HMO)
    network_suffix = case_when(
      insurer_small == "Kaiser"      ~ "",   # Kaiser always HMO, no suffix
      insurer_small == "Small"       ~ "",   # Small insurers, no network split
      PLAN_TYPE == "HMO"             ~ "3",
      TRUE                           ~ ""
    ),
    plan_id = paste0(insurer_prefix, "_", metal_abbr, network_suffix)
  )

cat("  Plan names:", length(unique(rdata$plan_id)), "unique\n")
cat("  Examples:", paste(head(sort(unique(rdata$plan_id)), 10), collapse = ", "), "\n")


# Aggregate to plan-name × year level ------------------------------------
# This is the level at which risk scores and claims regressions run.
# No region dimension in rate filings — insurers file at plan level.

rsdata <- rdata %>%
  filter(EXP_MM > 0 | PRJ_MM > 0) %>%
  group_by(plan_id, year, insurer_small, METAL, AV_METAL, PLAN_TYPE) %>%
  summarize(
    EXP_MM      = sum(EXP_MM, na.rm = TRUE),
    EXP_TP      = sum(EXP_TP, na.rm = TRUE),
    EXP_INC_CLM = sum(EXP_INC_CLM, na.rm = TRUE),
    EXP_RSK_ADJ = sum(EXP_RSK_ADJ, na.rm = TRUE),
    EXP_REIN    = sum(EXP_REIN, na.rm = TRUE),
    PRJ_MM      = sum(PRJ_MM, na.rm = TRUE),
    PRJ_INC_CLM = sum(PRJ_INC_CLM, na.rm = TRUE),
    PRJ_RSK_ADJ = sum(PRJ_RSK_ADJ, na.rm = TRUE),
    PRJ_REIN    = sum(PRJ_REIN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(EXP_MM > 0)

# Compute PMPM measures
rsdata <- rsdata %>%
  mutate(
    EXP_INC_CLM_PMPM = EXP_INC_CLM / EXP_MM,
    EXP_RSK_ADJ_PMPM = EXP_RSK_ADJ / EXP_MM,
    EXP_REIN_PMPM    = EXP_REIN / EXP_MM,
    EXP_TP_PMPM      = EXP_TP / EXP_MM,
    PRJ_INC_CLM_PMPM = if_else(PRJ_MM > 0, PRJ_INC_CLM / PRJ_MM, NA_real_),
    PRJ_RSK_ADJ_PMPM = if_else(PRJ_MM > 0, PRJ_RSK_ADJ / PRJ_MM, NA_real_),
    PRJ_REIN_PMPM    = if_else(PRJ_MM > 0, PRJ_REIN / PRJ_MM, NA_real_)
  )

cat("\n  rsdata:", nrow(rsdata), "plan-year observations\n")
cat("  Years:", paste(sort(unique(rsdata$year)), collapse = ", "), "\n")


# Compute risk scores -----------------------------------------------------
# Following Saltzman: risk_score = (EXP_RSK_ADJ / total_premium + s_av) / share
# where s_av is the AV-weighted utilization share

MH_LOOKUP <- c(Bronze = 1.00, Silver = 1.03, Gold = 1.08, Platinum = 1.15)
AV_LOOKUP <- c(Bronze = 0.60, Silver = 0.70, Gold = 0.80, Platinum = 0.90)

rsdata <- rsdata %>%
  mutate(
    total_adjustment = AV_LOOKUP[METAL] * MH_LOOKUP[METAL]
  )

rsdata <- rsdata %>%
  group_by(year) %>%
  mutate(
    total_premium = sum(EXP_TP),
    s_av  = (total_adjustment * EXP_MM) / sum(total_adjustment * EXP_MM),
    share = EXP_MM / sum(EXP_MM),
    risk_score = (EXP_RSK_ADJ / total_premium + s_av) / share,
    log_risk_score = log(if_else(risk_score > 0, risk_score, NA_real_))
  ) %>%
  ungroup()

cat("  Risk score range:", round(range(rsdata$risk_score, na.rm = TRUE), 3), "\n")

# Check for invalid risk scores
n_invalid <- sum(is.na(rsdata$risk_score) | rsdata$risk_score <= 0)
if (n_invalid > 0) {
  cat("  WARNING:", n_invalid, "invalid risk scores (NA or <= 0)\n")
}


# Add plan characteristics for regressions --------------------------------

rsdata <- rsdata %>%
  mutate(
    Silver   = as.integer(METAL == "Silver"),
    Gold     = as.integer(METAL == "Gold"),
    Platinum = as.integer(METAL == "Platinum"),
    HMO      = as.integer(PLAN_TYPE == "HMO"),
    Anthem      = as.integer(insurer_small == "Anthem"),
    Blue_Shield = as.integer(insurer_small == "Blue_Shield"),
    Health_Net  = as.integer(insurer_small == "Health_Net"),
    Kaiser      = as.integer(insurer_small == "Kaiser"),
    trend    = year - min(year),
    AV_Demean = AV_LOOKUP[METAL] - 0.70,
    log_cost = log(if_else(EXP_INC_CLM_PMPM > 0, EXP_INC_CLM_PMPM, NA_real_))
  )


# Compute reinsurance factors ---------------------------------------------
# Reinsurance was phased out after 2016

rsdata <- rsdata %>%
  mutate(
    reins_factor = if_else(PRJ_INC_CLM > 0, PRJ_REIN / PRJ_INC_CLM, 0),
    reins_factor = pmin(reins_factor, 1)  # cap at 100%
  )

cat("  Reinsurance factor by year:\n")
rsdata %>%
  group_by(year) %>%
  summarize(
    mean_reins = round(mean(reins_factor, na.rm = TRUE), 4),
    max_reins  = round(max(reins_factor, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  print(n = Inf)


# Save --------------------------------------------------------------------

write_csv(rsdata, "data/output/rate_filing_rsdata.csv")
cat("\nRate filing data saved:", nrow(rsdata), "rows -> data/output/rate_filing_rsdata.csv\n")

# Diagnostics
cat("\n--- Rate Filing Diagnostics ---\n")
cat("  Plans per year:\n")
rsdata %>% count(year) %>% print(n = Inf)
cat("\n  Risk score by metal:\n")
rsdata %>%
  group_by(METAL) %>%
  summarize(
    mean_rs = round(mean(risk_score, na.rm = TRUE), 3),
    sd_rs   = round(sd(risk_score, na.rm = TRUE), 3),
    n       = n(),
    .groups = "drop"
  ) %>%
  print(n = Inf)
cat("\n  Claims PMPM by metal:\n")
rsdata %>%
  group_by(METAL) %>%
  summarize(
    mean_claims = round(mean(EXP_INC_CLM_PMPM, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  print(n = Inf)

rm(rdata)
gc(verbose = FALSE)
cat("Rate filing processing complete.\n")
