# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Date Edited:   2026-03-25
## Description:   Dominated choice regressions and potential outcomes ATT.
##                Includes control function (v_hat) for selection correction.
##                Expects hh_full, hh_clean in memory from _reduced-form.R.

# =========================================================================
# Regression specifications
# =========================================================================

cat("Dominated choice regressions...\n")

# Model 1: all enrollees, no CF
mod1 <- feols(
  dominated_choice ~ assisted + english + spanish +
    FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 +
    perc_male + perc_asian + perc_black + perc_hispanic + perc_other +
    household_size + new_enrollee | region + year + insurer,
  cluster = "region",
  data = hh_full,
  weights = ~ipweight
)

# Model 2: new enrollees, no CF
mod2 <- feols(
  dominated_choice ~ assisted + english + spanish +
    FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 +
    perc_male + perc_asian + perc_black + perc_hispanic + perc_other +
    household_size | region + year + insurer,
  cluster = "region",
  data = hh_clean,
  weights = ~ipweight
)

# Model 3: new enrollees with CF
mod3 <- feols(
  dominated_choice ~ assisted + v_hat + english + spanish +
    FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 +
    perc_male + perc_asian + perc_black + perc_hispanic + perc_other +
    household_size | region + year + insurer,
  cluster = "region",
  data = hh_clean,
  weights = ~ipweight
)

dom_models <- list(
  "All Enrollees" = mod1,
  "New Enrollees" = mod2,
  "New + CF"      = mod3
)

modelsummary(
  dom_models,
  output = "results/tables/dominated_choice_regression.tex",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_omit = "AIC|BIC|Log|Std|RMSE",
  coef_map = c(
    "assisted"       = "Assisted",
    "v_hat"          = "CF Residual",
    "english"        = "English",
    "spanish"        = "Spanish",
    "FPL"            = "FPL",
    "household_size" = "HH Size",
    "new_enrollee"   = "New Enrollee"
  )
)

plot_dom_reg <- modelplot(dom_models,
                          coef_map = c("assisted" = "Assisted")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal()
ggsave("results/figures/dominated_choice_regression.png", plot_dom_reg, width = 7, height = 4)


# =========================================================================
# Potential outcomes ATT (with CF)
# =========================================================================

cat("Potential outcomes ATT...\n")

po_formula <- dominated_choice ~ v_hat + english + spanish +
  FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 +
  perc_male + perc_asian + perc_black + perc_hispanic + perc_other +
  household_size | insurer

compute_att <- function(df, channel_filter) {
  if (channel_filter == "any_assist") {
    treated   <- df %>% filter(assisted == 1)
    untreated <- df %>% filter(assisted == 0)
  } else if (channel_filter == "agent") {
    treated   <- df %>% filter(any_agent == 1)
    untreated <- df %>% filter(assisted == 0)
  } else {
    treated   <- df %>% filter(navigator == 1)
    untreated <- df %>% filter(assisted == 0)
  }

  fit <- tryCatch(
    feglm(po_formula, data = untreated, weights = ~ipweight,
           family = binomial),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NA_real_)

  predicted <- predict(fit, newdata = treated, type = "response")
  mean(treated$dominated_choice, na.rm = TRUE) - mean(predicted, na.rm = TRUE)
}

# CSR-eligible insured (where dominated_choice is non-NA)
hh_po <- hh_clean %>% filter(!is.na(dominated_choice))
cat("  PO sample:", nrow(hh_po), "CSR-eligible new enrollees\n")

att_any   <- compute_att(hh_po, "any_assist")
att_agent <- compute_att(hh_po, "agent")
att_nav   <- compute_att(hh_po, "navigator")


# =========================================================================
# Bootstrap
# =========================================================================

set.seed(42)
max_boot <- 200

boot_att <- function(df, channel_filter, B = max_boot) {
  df$.grp <- paste(df$region, df$year, sep = "_")
  grp_rows <- split(seq_len(nrow(df)), df$.grp)
  grp_names <- names(grp_rows)
  replicate(B, {
    sampled <- sample(grp_names, length(grp_names), replace = TRUE)
    row_idx <- unlist(grp_rows[sampled], use.names = FALSE)
    compute_att(df[row_idx, ], channel_filter)
  })
}

setFixest_notes(FALSE)
cat("  Bootstrap (200 x 3 channels)...\n")
boot_any   <- boot_att(hh_po, "any_assist")
cat("    any_assist done\n")
boot_agent <- boot_att(hh_po, "agent")
cat("    agent done\n")
boot_nav   <- boot_att(hh_po, "navigator")
cat("    navigator done\n")
setFixest_notes(TRUE)


# =========================================================================
# Summarize
# =========================================================================

att_summary <- tibble(
  Channel = c("Any Assistance", "Agent/Broker", "Navigator"),
  ATT = c(att_any, att_agent, att_nav),
  CI_lower = c(quantile(boot_any, 0.05, na.rm = TRUE),
               quantile(boot_agent, 0.05, na.rm = TRUE),
               quantile(boot_nav, 0.05, na.rm = TRUE)),
  CI_upper = c(quantile(boot_any, 0.95, na.rm = TRUE),
               quantile(boot_agent, 0.95, na.rm = TRUE),
               quantile(boot_nav, 0.95, na.rm = TRUE))
)

plot_att <- ggplot(att_summary, aes(x = Channel, y = ATT)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "ATT (Dominated Choice)", x = NULL) +
  theme_minimal()
ggsave("results/figures/dom_choice.png", plot_att, width = 6, height = 4)

cat("Dominated choice analysis complete.\n")
print(att_summary)
