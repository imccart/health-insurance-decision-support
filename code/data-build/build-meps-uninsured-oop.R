# Build the age x income UNINSURED out-of-pocket reference table for the welfare
# objective. Standalone reference build (not in the numbered 1-7 sequence, not
# sourced by _data-build.R), parallel to build-meps-spending.R.
#
# Motivation: the objective welfare measure had been charging an uninsured person
# their FULL medical spending, uncapped, and applying the risk penalty to that
# enormous variance -- giving an uninsured value near -$30,000/year that dominated
# and destabilized the counterfactual. In reality the uninsured do not pay the
# catastrophic bills (bad debt, charity care), so realized out-of-pocket is far
# lower and far less variable. This table measures what uninsured people actually
# pay, and how often their spending crosses a catastrophic-expenditure line, so the
# objective measure can value the uninsured option realistically and separate the
# measured burden from the assumption-driven cost of financial distress.
#
# Uninsured all year: INSCOV18 == 3. OOP paid: TOTSLF18. Total spending: TOTEXP18.
# Family income: FAMINC18. Catastrophic = total spending > 40% of family income
# (the standard catastrophic-health-expenditure line); when family income is <= 0,
# any positive spending counts as catastrophic (no income to pay from).
#
# Raw file: D:/research-data/meps/h209.dta
# Output:   data/input/meps_uninsured_oop.csv  (gitignored, like the spending table)

suppressMessages(pacman::p_load(haven, dplyr))

CATASTROPHIC_FRAC <- 0.40   # spending above this share of family income = catastrophic

raw <- read_dta("D:/research-data/meps/h209.dta",
                col_select = c(DUPERSID, AGELAST, PERWT18F, TOTSLF18, TOTEXP18,
                               FAMINC18, POVLEV18, INSCOV18))

meps <- raw %>%
  filter(AGELAST >= 0, AGELAST < 65, PERWT18F > 0, !is.na(POVLEV18),
         INSCOV18 == 3) %>%                         # uninsured all year
  mutate(
    age_group = case_when(AGELAST <= 17 ~ "0to17", AGELAST <= 34 ~ "18to34",
                          AGELAST <= 54 ~ "35to54", TRUE ~ "55plus"),
    income = case_when(POVLEV18 < 250 ~ "lt250", POVLEV18 < 400 ~ "250to400",
                       TRUE ~ "400plus"),
    catastrophic = if_else(FAMINC18 > 0,
                           TOTEXP18 > CATASTROPHIC_FRAC * FAMINC18,
                           TOTEXP18 > 0)
  )

cat("uninsured-all-year persons under 65, non-missing POVLEV:", nrow(meps), "\n")
cat("weighted mean realized OOP (all cells):",
    round(sum(meps$PERWT18F * meps$TOTSLF18) / sum(meps$PERWT18F), 0), "\n")
cat("weighted catastrophic rate (all cells):",
    round(100 * sum(meps$PERWT18F * meps$catastrophic) / sum(meps$PERWT18F), 1), "%\n\n")

sched <- meps %>%
  group_by(age_group, income) %>%
  summarize(
    n              = n(),
    mean_oop       = sum(PERWT18F * TOTSLF18) / sum(PERWT18F),
    var_oop        = sum(PERWT18F * (TOTSLF18 - sum(PERWT18F * TOTSLF18) / sum(PERWT18F))^2) /
                       sum(PERWT18F),
    catastrophic_rate = sum(PERWT18F * catastrophic) / sum(PERWT18F),
    .groups = "drop"
  ) %>%
  mutate(
    age_group = factor(age_group, levels = c("0to17", "18to34", "35to54", "55plus")),
    income    = factor(income,    levels = c("lt250", "250to400", "400plus"))
  ) %>%
  arrange(age_group, income) %>%
  mutate(age_group = as.character(age_group), income = as.character(income),
         mean_oop = round(mean_oop, 0), var_oop = round(var_oop, 0),
         catastrophic_rate = round(catastrophic_rate, 4),
         source = sprintf("MEPS2018_HC209_uninsured_TOTSLF18_cat%.0fpct_%s_x_%s",
                          100 * CATASTROPHIC_FRAC, age_group, income))

print(as.data.frame(sched[, c("age_group", "income", "n", "mean_oop",
                              "catastrophic_rate")]), row.names = FALSE)

write.csv(sched[, c("age_group", "income", "mean_oop", "var_oop",
                    "catastrophic_rate", "n", "source")],
          "data/input/meps_uninsured_oop.csv", row.names = FALSE)
cat("\nwrote data/input/meps_uninsured_oop.csv\n")
