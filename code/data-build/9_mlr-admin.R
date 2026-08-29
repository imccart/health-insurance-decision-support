# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Administrative costs of the California individual-market
##                insurers from the CMS medical loss ratio public use files,
##                2014-2018 (the March-31 restatement, CMM_INDIVIDUAL_Q1):
##                agents' and brokers' fees and commissions, direct sales
##                salaries, other general and administrative expense, and
##                claims adjustment expense, per member-month, keyed to our
##                insurer prefixes (Health Net's two filers combined). 2019 is
##                not in the files on disk and carries the 2018 values. Also
##                the within-insurer relation between non-commission
##                administrative cost and commission outlay (insurer and year
##                effects), the starting value of the substitution parameter
##                beta in the commission condition.
## Input:         D:/research-data/insurance-mlr/MLR_2014.zip, MLR_2015.zip,
##                D:/research-data/insurance-mlr/2016-2018/ (PUF csv files)
## Output:        data/output/mlr_admin.csv        (insurer_prefix, year, admin_pmpm, ...)
##                data/output/mlr_admin_beta.csv   (beta0, se, n)

cat("Loading MLR administrative costs...\n")
MLR_DIR <- "D:/research-data/insurance-mlr"
MLR_ROWS <- c(MEMBER_MONTHS = "mm", TOTAL_DIRECT_PREMIUM_EARNED = "premium",
              TOTAL_INCURRED_CLAIMS_PT1 = "claims", AGNTS_AND_BROKERS_FEES_COMMS = "commissions",
              DIR_SALES_SALARIES_AND_BENEFITS = "direct_sales", OTHER_GENERAL_AND_ADM_EXPENSES = "other_ga",
              ALL_OTHER_CLAIMS_ADJ_EXPENSES = "claims_adj")
MLR_PREFIX <- c("blue cross of california|anthem" = "ANT", "blue shield" = "BS", "kaiser" = "KA",
                "health net" = "HN", "molina" = "MOL", "local initiative|l\\.a\\. care|la care" = "LA",
                "sharp" = "SH", "chinese community" = "CC", "oscar" = "OSC", "western health" = "WEST",
                "valley health|county of santa clara" = "VAL")

read_mlr_year <- function(y) {
  if (y %in% c(2014, 2015)) {
    zp <- file.path(MLR_DIR, paste0("MLR_", y, ".zip"))
    hdr <- read_csv(unz(zp, "MR_Submission_Template_Header.csv"), show_col_types = FALSE, name_repair = "minimal")
    p12 <- read_csv(unz(zp, "Part1_2_Summary_Data_Premium_Claims.csv"), show_col_types = FALSE, name_repair = "minimal")
  } else {
    hdr <- read_csv(file.path(MLR_DIR, y, "MR_Submission_Template_Header.csv"), show_col_types = FALSE, name_repair = "minimal")
    p12 <- read_csv(file.path(MLR_DIR, y, "Part1_2_Summary_Data_Premium_Claims.csv"), show_col_types = FALSE, name_repair = "minimal")
  }
  names(hdr) <- sub("^\ufeff", "", names(hdr)); names(p12) <- sub("^\ufeff", "", names(p12))
  hdr <- hdr %>%
    filter(str_trim(BUSINESS_STATE) %in% c("California", "CA")) %>%
    transmute(MR_SUBMISSION_TEMPLATE_ID, HIOS_ISSUER_ID, COMPANY_NAME, DBA_MARKETING_NAME)
  p12 <- p12 %>%
    mutate(ROW_LOOKUP_CODE = str_trim(gsub('"', "", ROW_LOOKUP_CODE))) %>%
    filter(ROW_LOOKUP_CODE %in% names(MLR_ROWS)) %>%
    transmute(MR_SUBMISSION_TEMPLATE_ID, item = unname(MLR_ROWS[ROW_LOOKUP_CODE]),
              value = ifelse(is.na(CMM_INDIVIDUAL_Q1), CMM_INDIVIDUAL_YEARLY, CMM_INDIVIDUAL_Q1)) %>%
    pivot_wider(names_from = item, values_from = value, values_fn = first)
  hdr %>% inner_join(p12, by = "MR_SUBMISSION_TEMPLATE_ID") %>% mutate(year = y)
}

mlr <- bind_rows(lapply(2014:2018, read_mlr_year)) %>%
  filter(!is.na(mm), mm > 20000) %>%
  mutate(name = tolower(paste(COMPANY_NAME, DBA_MARKETING_NAME)),
         insurer_prefix = NA_character_)
for (pat in names(MLR_PREFIX)) mlr$insurer_prefix[is.na(mlr$insurer_prefix) & str_detect(mlr$name, pat)] <- MLR_PREFIX[[pat]]
cat("  California individual-market filers matched to our insurers:", sum(!is.na(mlr$insurer_prefix)),
    "of", nrow(mlr), "filer-years\n")

mlr_admin <- mlr %>%
  filter(!is.na(insurer_prefix)) %>%
  group_by(insurer_prefix, year) %>%
  summarize(across(c(mm, premium, claims, commissions, direct_sales, other_ga, claims_adj), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(admin_pmpm      = (direct_sales + other_ga + claims_adj) / mm,   # the per-member level in marginal cost
         sales_ga_pmpm   = (direct_sales + other_ga) / mm,                # the part agents substitute for
         commission_pmpm = commissions / mm,
         premium_pmpm    = premium / mm,
         claims_pmpm     = claims / mm) %>%
  select(insurer_prefix, year, mm, admin_pmpm, sales_ga_pmpm, commission_pmpm, premium_pmpm, claims_pmpm)

# 2019 is not in the files on disk: carry the 2018 values
mlr_admin <- bind_rows(mlr_admin,
                       mlr_admin %>% filter(year == 2018) %>% mutate(year = 2019L, mm = NA_real_))
cat("  insurer-years:", nrow(mlr_admin), "(2019 carried from 2018)\n")
cat("  non-commission administrative cost per member-month by insurer (mean over years):\n")
print(mlr_admin %>% group_by(insurer_prefix) %>%
        summarize(admin_pmpm = round(mean(admin_pmpm), 1), commission_pmpm = round(mean(commission_pmpm, na.rm = TRUE), 1),
                  .groups = "drop"))

# Within-insurer relation of sales and G&A cost to commission outlay, per
# member-month (claims adjustment expense does not move with commissions and is
# left out of the outcome): the starting value of beta (a commission dollar's
# administrative saving) for the cost GMM
reg <- mlr_admin %>% filter(!is.na(mm), is.finite(sales_ga_pmpm), is.finite(commission_pmpm))
fit <- feols(sales_ga_pmpm ~ commission_pmpm | insurer_prefix + year, data = reg, weights = ~mm, cluster = ~insurer_prefix)
beta0 <- -unname(coef(fit)["commission_pmpm"])
cat("  administrative saving per commission dollar (within insurer): beta0 =", round(beta0, 3),
    " se", round(unname(se(fit)["commission_pmpm"]), 3), " n =", nobs(fit), "\n")

# The administrative-cost level that enters marginal cost: the fitted sales and
# G&A cost before any commission saving (insurer effect + year effect; the
# commission saving is applied per broker enrollee in the model) plus the
# observed claims adjustment cost, which does not move with commissions. 2019
# carries the 2018 year effect.
fe <- fixef(fit)
mlr_admin <- mlr_admin %>%
  mutate(fe_year = ifelse(as.character(year) %in% names(fe$year), fe$year[as.character(year)], fe$year[["2018"]]),
         fe_ins  = fe$insurer_prefix[insurer_prefix],
         sales_ga0_pmpm = fe_ins + fe_year,
         admin0_pmpm    = sales_ga0_pmpm + (admin_pmpm - sales_ga_pmpm)) %>%
  select(-fe_year, -fe_ins)
cat("  administrative level before commission saving (admin0), mean by insurer:\n")
print(mlr_admin %>% group_by(insurer_prefix) %>%
        summarize(admin0 = round(mean(admin0_pmpm), 1), observed = round(mean(admin_pmpm), 1), .groups = "drop"))

write_csv(mlr_admin, "data/output/mlr_admin.csv")
write_csv(tibble(beta0 = beta0, se = unname(se(fit)["commission_pmpm"]), n = nobs(fit)),
          "data/output/mlr_admin_beta.csv")
cat("  -> data/output/mlr_admin.csv, mlr_admin_beta.csv\n")
