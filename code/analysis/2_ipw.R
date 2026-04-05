# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-05
## Description:   Propensity score estimation and IPW weights.
##                Outputs: data/output/ipweights.csv (household_year lookup).
##                Does NOT re-save hh_full/hh_clean/hh_ins — those are
##                canonical outputs of 1_decision-analysis.R only.

# Load from disk if not already in memory -----------------------------------
if (!exists("hh_ins")) {
  hh_ins <- read_csv("data/output/hh_ins.csv", show_col_types = FALSE)
  cat("  Loaded hh_ins from disk\n")
}

# Propensity score estimation (nest by year) ------------------------------

ps_formula <- assisted ~ FPL + perc_0to17 + perc_18to25 + perc_65plus +
  perc_black + perc_hispanic + perc_asian + perc_male + household_size

estimate_ps <- function(df) {
  df %>%
    nest(data = -year) %>%
    mutate(
      model = map(data, ~ glm(ps_formula, data = .x, family = binomial)),
      pred  = map2(model, data, ~ predict(.x, newdata = .y, type = "response"))
    ) %>%
    select(year, data, pred) %>%
    unnest(cols = c(data, pred)) %>%
    rename(pred_assist = pred)
}

# Estimate PS on insured HH, compute ATT weights
hh_ins_ps <- estimate_ps(hh_ins) %>%
  mutate(ipweight = if_else(assisted == 1, 1, pred_assist / (1 - pred_assist)))

# Save lookup: one row per household-year
ipweights <- hh_ins_ps %>%
  select(household_year, pred_assist, ipweight)

write_csv(ipweights, "data/output/ipweights.csv")
cat("  IPW weights:", nrow(ipweights), "rows -> data/output/ipweights.csv\n")

rm(hh_ins_ps, ipweights)
