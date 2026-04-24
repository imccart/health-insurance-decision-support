# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Date Edited:   2026-04-23
## Description:   Dominated choice regressions and potential outcomes ATT.
##                Includes control function (v_hat) for selection correction.
##                Expects hh_full in memory from _reduced-form.R.
##                Manages hh_full → hh_clean → hh_po lifecycle internally
##                to keep only one big HH object in memory at a time.

# =========================================================================
# Regression specifications
# =========================================================================

cat("Dominated choice regressions...\n")

# Model 1: all enrollees, no CF (hh_full)
mod1 <- feols(
  dominated_choice ~ assisted + english + spanish +
    FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 +
    perc_male + perc_asian + perc_black + perc_hispanic + perc_other +
    household_size + new_enrollee | region + year + insurer,
  cluster = "region",
  data = hh_full,
  weights = ~ipweight
)

# Done with hh_full. Derive hh_clean (new enrollees) and free hh_full so
# both never live simultaneously.
hh_clean <- hh_full %>% filter(new_enrollee == 1L)
rm(hh_full)
gc(verbose = FALSE)

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

# Hand-built regression table (modelsummary backends unreliable for this layout)
coef_labels <- c(
  "assisted"       = "Assisted",
  "v_hat"          = "CF Residual",
  "english"        = "English",
  "spanish"        = "Spanish",
  "FPL"            = "FPL",
  "household_size" = "HH Size",
  "new_enrollee"   = "New Enrollee"
)

stars_for <- function(p) {
  fcase(is.na(p), "",
        p < 0.01, "***",
        p < 0.05, "**",
        p < 0.10, "*",
        default  = "")
}

format_cell <- function(coef, se, p) {
  if (is.na(coef)) return(c("", ""))
  c(sprintf("%.4f%s", coef, stars_for(p)),
    sprintf("(%.4f)", se))
}

extract_col <- function(mod, terms) {
  ct <- as.data.frame(coeftable(mod))
  ct$term <- rownames(ct)
  out <- character(2 * length(terms))
  for (i in seq_along(terms)) {
    row <- ct[ct$term == terms[i], , drop = FALSE]
    if (nrow(row) == 1) {
      cell <- format_cell(row$Estimate, row[["Std. Error"]], row[["Pr(>|t|)"]])
    } else {
      cell <- c("", "")
    }
    out[(2 * i - 1):(2 * i)] <- cell
  }
  out
}

terms <- names(coef_labels)
row_labels <- as.vector(rbind(unname(coef_labels), ""))

tab <- data.frame(
  Variable           = row_labels,
  `All Enrollees`    = extract_col(mod1, terms),
  `New Enrollees`    = extract_col(mod2, terms),
  `New + CF`         = extract_col(mod3, terms),
  check.names        = FALSE,
  stringsAsFactors   = FALSE
)

gof_rows <- data.frame(
  Variable        = c("Observations", "R$^2$"),
  `All Enrollees` = c(format(nobs(mod1), big.mark = ","), sprintf("%.3f", r2(mod1, "r2"))),
  `New Enrollees` = c(format(nobs(mod2), big.mark = ","), sprintf("%.3f", r2(mod2, "r2"))),
  `New + CF`      = c(format(nobs(mod3), big.mark = ","), sprintf("%.3f", r2(mod3, "r2"))),
  check.names     = FALSE,
  stringsAsFactors = FALSE
)
tab <- rbind(tab, gof_rows)

dom_tab <- kable(tab, format = "latex", booktabs = TRUE,
                 align = c("l", "c", "c", "c"),
                 linesep = "", escape = FALSE) %>%
  kable_styling(latex_options = c("scale_down"))
writeLines(as.character(dom_tab), "results/tables/dominated_choice_regression.tex")

# =========================================================================
# Potential outcomes ATT (with CF)
# =========================================================================

cat("Potential outcomes ATT...\n")

po_formula <- dominated_choice ~ v_hat + english + spanish +
  FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 +
  perc_male + perc_asian + perc_black + perc_hispanic + perc_other +
  household_size | insurer + region + year

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

  fit <- feglm(po_formula, data = untreated, weights = ~ipweight,
               family = binomial)
  predicted <- predict(fit, newdata = treated, type = "response")
  mean(treated$dominated_choice, na.rm = TRUE) - mean(predicted, na.rm = TRUE)
}

# CSR-eligible insured (where dominated_choice is non-NA). hh_po is much
# smaller than hh_clean — derive it then drop hh_clean.
hh_po <- hh_clean %>% filter(!is.na(dominated_choice))
rm(hh_clean)
gc(verbose = FALSE)
cat("  PO sample:", nrow(hh_po), "CSR-eligible new enrollees\n")

att_any   <- compute_att(hh_po, "any_assist")
att_agent <- compute_att(hh_po, "agent")
att_nav   <- compute_att(hh_po, "navigator")


# =========================================================================
# Bootstrap
# =========================================================================

set.seed(42)
max_boot <- 50

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

boot_any   <- boot_att(hh_po, "any_assist")
boot_agent <- boot_att(hh_po, "agent")
boot_nav   <- boot_att(hh_po, "navigator")


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

theme_paper <- theme_bw() +
  theme(text = element_text(size = 12), panel.grid.minor = element_blank(),
        plot.title = element_blank())

plot_att <- ggplot(att_summary, aes(x = Channel, y = ATT)) +
  geom_point(size = 2, color = "black") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(y = "ATT (percentage points)", x = NULL) +
  theme_paper
ggsave("results/figures/dom_choice.png", plot_att, width = 6, height = 4, bg = "white")

cat("Dominated choice analysis complete.\n")
print(att_summary)

# Free hh_po and bootstrap residues; _reduced-form.R is done with HH-level data.
rm(hh_po, boot_any, boot_agent, boot_nav, dom_models, mod1, mod2, mod3)
gc(verbose = FALSE)
