# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Description:   Switching behavior regressions.
##                Port of _old-repo/analysis/decision-support/_Switching.R

# Switching regressions ---------------------------------------------------

# Model 1: region FE, region cluster
mod_sw1 <- feols(
  switch ~ assisted + english + spanish +
    FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 +
    perc_male + perc_asian + perc_black + perc_hispanic + perc_other +
    household_size | region + year + insurer,
  cluster = "region",
  data = hh_full
)

# Model 2: HH FE, HH cluster
mod_sw2 <- feols(
  switch ~ assisted + english + spanish +
    FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 +
    perc_male + perc_asian + perc_black + perc_hispanic + perc_other +
    household_size | household_id + year + insurer,
  cluster = "household_id",
  data = hh_full
)

# Model 3: dominated choice among switchers, region FE
mod_sw3 <- feols(
  dominated_choice ~ assisted + english + spanish +
    FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 +
    perc_male + perc_asian + perc_black + perc_hispanic + perc_other +
    household_size | region + year + insurer,
  cluster = "region",
  data = hh_full %>% filter(switch == 1)
)

# Regression table --------------------------------------------------------

switch_models <- list(
  "Switch (Region FE)" = mod_sw1,
  "Switch (HH FE)"     = mod_sw2,
  "Dom. Choice (Switchers)" = mod_sw3
)

modelsummary(
  switch_models,
  output = "results/tables/switching_regression.tex",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_omit = "AIC|BIC|Log|Std|RMSE",
  coef_map = c(
    "assisted"       = "Assisted",
    "english"        = "English",
    "spanish"        = "Spanish",
    "FPL"            = "FPL",
    "household_size" = "HH Size"
  )
)

# Coefficient plot --------------------------------------------------------

plot_switch <- modelplot(switch_models,
                         coef_map = c("assisted" = "Assisted")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Effect of Assistance on Switching") +
  theme_minimal()
ggsave("results/figures/switching_regression.png", plot_switch, width = 7, height = 4)

cat("Switching analysis complete.\n")
