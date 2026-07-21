# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-05
## Description:   Propensity score estimation and IPW weights.
##                Outputs: data/output/ipweights.csv (household_year lookup).
##                Derives hh_ins from hh_full on demand, frees it at the end.

# Materialize hh_ins (load hh_full from disk if not already in memory).
# hh_full is NOT freed here — 3_summary-stats.R uses it next and will free
# it after deriving its own subsets.
if (!exists("hh_full")) {
  hh_full <- fread("data/output/hh_full.csv") %>% as_tibble()
  cat("  Loaded hh_full from disk\n")
}
hh_ins <- hh_full %>%
  filter(insured == 1L) %>%
  # FPL brackets (created in choice.R for the structural side; recreated here so
  # the propensity uses the same income parameterization as everything else).
  mutate(FPL_250to400 = as.integer(FPL > 2.50 & FPL <= 4.00),
         FPL_400plus  = as.integer(FPL > 4.00))

# Propensity score estimation (nest by year) ------------------------------
# Demographic set is the canonical structural spec (age 0-17/18-34/35-54 with
# 55+ base, race, male, FPL brackets, household size), so IPW, the v_hat first
# stage (build3), and the outcome models all share one specification.

ps_formula <- assisted ~ perc_0to17 + perc_18to34 + perc_35to54 +
  perc_male + perc_black + perc_hispanic + perc_asian + perc_other +
  FPL_250to400 + FPL_400plus + household_size

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

fwrite(ipweights, "data/output/ipweights.csv")
cat("  IPW weights:", nrow(ipweights), "rows -> data/output/ipweights.csv\n")

rm(hh_ins_ps, ipweights, hh_ins)
gc(verbose = FALSE)
