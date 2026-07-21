# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Date Edited:   2026-04-23
## Description:   Dominated choice ATT. Body = prediction-based ATT (fit on the
##                unassisted, predict onto the assisted), IPW-weighted, no
##                control function. Appendix = pooled progressive specs ending
##                in control-function-without-region-FE and region-FE-without-
##                control-function, plus the body ATT re-run on new enrollees.
##                Reads the prepped HH panel (hh_full_prepped.csv) written by
##                build3_data-prep, so it runs without any prior step in memory.
##                Manages hh_full → hh_clean → hh_po lifecycle internally
##                to keep only one big HH object in memory at a time.

# =========================================================================
# Regression specifications
# =========================================================================

cat("Dominated choice regressions...\n")

# Prepped HH panel (augmented with v_hat) from build3_data-prep.
hh_full <- fread(file.path(TEMP_DIR, "hh_full_prepped.csv")) %>% as_tibble()

# Appendix pooled specifications, all on ALL enrollees, reported as a progressive
# sequence so the reader sees the assisted coefficient settle rather than being
# asked to compare two numbers. Canonical structural demographic set, shared with
# build2/build3 and rf2: age 0-17/18-34/35-54 (55+ base), race (white base), male,
# household size. No insurer FE (it is the post-choice selected insurer).
#
# The FPL bracket dummies used everywhere else (FPL_250to400, FPL_400plus) are
# NOT here. dominated_choice is defined only for CSR-eligible households with
# FPL <= 2.00 (build1), so both dummies are identically zero on this sample and
# feols was silently dropping them for collinearity. Income variation that does
# survive is the 1.50 FPL line, which is where the definition of dominance
# itself changes, so it is not a control -- it is part of the outcome.
#
# The sequence ends in two terminal specifications that never appear together.
# Column 4 adds the broker-density control function without region FE; column 5
# adds region FE without the control function. Both is incoherent, not merely
# underpowered: n_agents varies at the region-year level, so region FE leaves the
# instrument nothing to work with, and region FE only becomes a legitimate
# first-stage covariate once the second stage carries it too.
#
# New enrollees are no longer a column here. That comparison is the body's
# prediction-based ATT re-run on new enrollees (dominated_new_vs_all.csv below).

# (1) unconditional
mod1 <- feols(
  dominated_choice ~ assisted,
  cluster = "region", data = hh_full, weights = ~ipweight
)

# (2) + demographics
mod2 <- feols(
  dominated_choice ~ assisted +
    perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
    perc_black + perc_hispanic + perc_asian + perc_other +
    household_size,
  cluster = "region", data = hh_full, weights = ~ipweight
)

# (3) + new enrollee indicator + year FE -- the full control set
mod3 <- feols(
  dominated_choice ~ assisted +
    perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
    perc_black + perc_hispanic + perc_asian + perc_other +
    household_size + new_enrollee | year,
  cluster = "region", data = hh_full, weights = ~ipweight
)

# (4) full controls + control function, no region FE
mod4 <- feols(
  dominated_choice ~ assisted + v_hat +
    perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
    perc_black + perc_hispanic + perc_asian + perc_other +
    household_size + new_enrollee | year,
  cluster = "region", data = hh_full, weights = ~ipweight
)

# (5) full controls + region FE, no control function
mod5 <- feols(
  dominated_choice ~ assisted +
    perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
    perc_black + perc_hispanic + perc_asian + perc_other +
    household_size + new_enrollee | year + region,
  cluster = "region", data = hh_full, weights = ~ipweight
)

# All-enrollee CSR-eligible sample for the body ATT, captured before hh_full is
# freed. hh_po (new enrollees) is derived below for the appendix sample check.
hh_po_all <- hh_full %>% filter(!is.na(dominated_choice))

# Done with hh_full. Derive hh_clean (new enrollees) and free hh_full so
# both never live simultaneously.
hh_clean <- hh_full %>% filter(new_enrollee == 1L)
rm(hh_full)
gc(verbose = FALSE)

# Hand-built regression table (modelsummary backends unreliable for this layout)
coef_labels <- c(
  "assisted"       = "Assisted",
  "v_hat"          = "CF Residual",
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

mods <- list(mod1, mod2, mod3, mod4, mod5)
col_names <- c("(1)", "(2)", "(3)", "(4)", "(5)")

tab <- data.frame(Variable = row_labels, stringsAsFactors = FALSE)
for (j in seq_along(mods)) tab[[col_names[j]]] <- extract_col(mods[[j]], terms)

spec_rows <- data.frame(
  Variable = c("Demographics", "Year FE", "Region FE", "Control Function"),
  `(1)` = c("", "", "", ""),
  `(2)` = c("X", "", "", ""),
  `(3)` = c("X", "X", "", ""),
  `(4)` = c("X", "X", "", "X"),
  `(5)` = c("X", "X", "X", ""),
  check.names = FALSE, stringsAsFactors = FALSE
)

gof_rows <- data.frame(
  Variable = c("Observations", "R$^2$"),
  stringsAsFactors = FALSE
)
for (j in seq_along(mods)) {
  gof_rows[[col_names[j]]] <- c(format(nobs(mods[[j]]), big.mark = ","),
                                sprintf("%.3f", r2(mods[[j]], "r2")))
}

tab <- rbind(tab, spec_rows, gof_rows)

# Bare tabular: the appendix supplies the table environment, caption, and
# \resizebox. A kable_styling() table wrapper here would nest inside it.
dom_tab <- kable(tab, format = "latex", booktabs = TRUE,
                 align = c("l", rep("c", length(mods))),
                 linesep = "", escape = FALSE)
writeLines(as.character(dom_tab), "results/tables/dominated_choice_regression.tex")

# =========================================================================
# Baseline prediction-based ATT (IPW, selection on observables)
# =========================================================================
# Fit the dominated-choice model on the unassisted, predict the counterfactual
# for the assisted, ATT = observed - predicted. This is the body estimate: no
# control function anywhere in it. The appendix companion is the pooled sequence
# above. Same demographic set and year-FE-only structure as those models.

cat("Baseline prediction-based ATT...\n")

po_formula <- dominated_choice ~
  perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
  perc_black + perc_hispanic + perc_asian + perc_other +
  household_size | year

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

# CSR-eligible samples (dominated_choice non-NA). hh_po_all (all enrollees) is
# the main sample, matching the structural model; hh_po (new enrollees) is the
# appendix comparison. hh_po_all was built above from hh_full.
hh_po <- hh_clean %>% filter(!is.na(dominated_choice))
rm(hh_clean)
gc(verbose = FALSE)
cat("  Main (all enrollees):", nrow(hh_po_all),
    " comparison (new enrollees):", nrow(hh_po), "CSR-eligible\n")

# Main estimates: baseline prediction-based ATT on ALL enrollees.
att_any   <- compute_att(hh_po_all, "any_assist")
att_agent <- compute_att(hh_po_all, "agent")
att_nav   <- compute_att(hh_po_all, "navigator")

# Appendix: the same ATT on new enrollees only, reported next to the all-enrollee
# main estimate to show the reduced-form result is not sensitive to the sample.
att_any_new   <- compute_att(hh_po, "any_assist")
att_agent_new <- compute_att(hh_po, "agent")
att_nav_new   <- compute_att(hh_po, "navigator")
dom_sample_tab <- tibble(
  Channel           = c("Any Assistance", "Agent/Broker", "Navigator"),
  ATT_all_enrollees = c(att_any, att_agent, att_nav),
  ATT_new_enrollees = c(att_any_new, att_agent_new, att_nav_new)
)
fwrite(dom_sample_tab, "results/dominated_new_vs_all.csv")
cat("  Dominated baseline ATT, all vs new enrollees:\n"); print(dom_sample_tab)

# Bare tabular for the appendix (the appendix supplies table env and caption).
dom_sample_tex <- dom_sample_tab %>%
  mutate(across(c(ATT_all_enrollees, ATT_new_enrollees), ~ sprintf("%.4f", .x))) %>%
  rename(`All Enrollees` = ATT_all_enrollees, `New Enrollees` = ATT_new_enrollees) %>%
  kable(format = "latex", booktabs = TRUE, align = c("l", "c", "c"),
        linesep = "", escape = FALSE)
writeLines(as.character(dom_sample_tex), "results/tables/dominated_new_vs_all.tex")


# =========================================================================
# Bootstrap (within-cell HH resampling, stratified by region × year)
# =========================================================================

set.seed(42)
B <- if (exists("N_BOOT")) N_BOOT else 50L

boot_att <- function(df, channel_filter, B) {
  df$.grp <- paste(df$region, df$year, sep = "_")
  grp_rows <- split(seq_len(nrow(df)), df$.grp)
  replicate(B, {
    row_idx <- unlist(
      lapply(grp_rows, function(rows) sample(rows, length(rows), replace = TRUE)),
      use.names = FALSE
    )
    compute_att(df[row_idx, ], channel_filter)
  })
}

if (B > 0) {
  boot_any   <- boot_att(hh_po_all, "any_assist", B)
  boot_agent <- boot_att(hh_po_all, "agent",      B)
  boot_nav   <- boot_att(hh_po_all, "navigator",  B)
} else {
  boot_any <- boot_agent <- boot_nav <- NA_real_
}


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

# Free hh_po and bootstrap residues; the structural block is done with HH data.
rm(hh_po, hh_po_all, boot_any, boot_agent, boot_nav,
   mod1, mod2, mod3, mod4, mod5)
gc(verbose = FALSE)
