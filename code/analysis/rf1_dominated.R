# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Date Edited:   2026-04-23
## Description:   Dominated choice regressions and potential outcomes ATT.
##                Includes control function (v_hat) for selection correction.
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

# Canonical structural demographic set, shared with build2/build3 and rf2:
# age 0-17/18-34/35-54 (55+ base), race (white base), male, FPL brackets
# (<250 base), household size. No insurer FE (it is the post-choice selected
# insurer), no region FE in the body (region FE collapses the broker-density
# instrument the MTE needs; region FE is the appendix robustness). Year FE only.

# Model 1: all enrollees, no CF (hh_full)
mod1 <- feols(
  dominated_choice ~ assisted +
    perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
    perc_black + perc_hispanic + perc_asian + perc_other +
    FPL_250to400 + FPL_400plus + household_size + new_enrollee | year,
  cluster = "region",
  data = hh_full,
  weights = ~ipweight
)

# Model 1 + region FE (appendix robustness). The body specs drop region FE
# because it collapses the broker-density instrument the CF/MTE rely on; this
# adds it to the no-CF pooled model to show the assisted coefficient survives
# cross-region controls. The +CF model gets no region-FE variant -- region FE
# would absorb the instrument that identifies v_hat.
mod1_rfe <- feols(
  dominated_choice ~ assisted +
    perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
    perc_black + perc_hispanic + perc_asian + perc_other +
    FPL_250to400 + FPL_400plus + household_size + new_enrollee | year + region,
  cluster = "region",
  data = hh_full,
  weights = ~ipweight
)

# Model 3: all enrollees with CF (v_hat = broker-density first-stage residual).
# The CF-corrected pooled estimate on the main (all-enrollee) sample, matching
# the body's prediction-based ATT and MTE. Fit on hh_full before it is freed.
mod3 <- feols(
  dominated_choice ~ assisted + v_hat +
    perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
    perc_black + perc_hispanic + perc_asian + perc_other +
    FPL_250to400 + FPL_400plus + household_size + new_enrollee | year,
  cluster = "region",
  data = hh_full,
  weights = ~ipweight
)

# All-enrollee CSR-eligible sample for the appendix new-vs-all baseline ATT,
# captured before hh_full is freed. hh_po (new enrollees) is derived below.
hh_po_all <- hh_full %>% filter(!is.na(dominated_choice))

# Done with hh_full. Derive hh_clean (new enrollees) and free hh_full so
# both never live simultaneously.
hh_clean <- hh_full %>% filter(new_enrollee == 1L)
rm(hh_full)
gc(verbose = FALSE)

# Model 2: new enrollees, no CF
mod2 <- feols(
  dominated_choice ~ assisted +
    perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
    perc_black + perc_hispanic + perc_asian + perc_other +
    FPL_250to400 + FPL_400plus + household_size | year,
  cluster = "region",
  data = hh_clean,
  weights = ~ipweight
)

# Model 2 + region FE (appendix robustness, new enrollees, no CF).
mod2_rfe <- feols(
  dominated_choice ~ assisted +
    perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
    perc_black + perc_hispanic + perc_asian + perc_other +
    FPL_250to400 + FPL_400plus + household_size | year + region,
  cluster = "region",
  data = hh_clean,
  weights = ~ipweight
)

dom_models <- list(
  "All Enrollees" = mod1,
  "New Enrollees" = mod2,
  "All + CF"      = mod3
)

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

tab <- data.frame(
  Variable           = row_labels,
  `All Enrollees`    = extract_col(mod1, terms),
  `New Enrollees`    = extract_col(mod2, terms),
  `All + CF`         = extract_col(mod3, terms),
  check.names        = FALSE,
  stringsAsFactors   = FALSE
)

gof_rows <- data.frame(
  Variable        = c("Observations", "R$^2$"),
  `All Enrollees` = c(format(nobs(mod1), big.mark = ","), sprintf("%.3f", r2(mod1, "r2"))),
  `New Enrollees` = c(format(nobs(mod2), big.mark = ","), sprintf("%.3f", r2(mod2, "r2"))),
  `All + CF`      = c(format(nobs(mod3), big.mark = ","), sprintf("%.3f", r2(mod3, "r2"))),
  check.names     = FALSE,
  stringsAsFactors = FALSE
)
tab <- rbind(tab, gof_rows)

dom_tab <- kable(tab, format = "latex", booktabs = TRUE,
                 align = c("l", "c", "c", "c"),
                 linesep = "", escape = FALSE) %>%
  kable_styling(latex_options = c("scale_down"))
writeLines(as.character(dom_tab), "results/tables/dominated_choice_regression.tex")

# Region-FE robustness table (appendix): no-CF pooled models with region FE added.
tab_rfe <- data.frame(
  Variable                = row_labels,
  `All Enr. + Region FE`  = extract_col(mod1_rfe, terms),
  `New Enr. + Region FE`  = extract_col(mod2_rfe, terms),
  check.names             = FALSE,
  stringsAsFactors        = FALSE
)
gof_rfe <- data.frame(
  Variable               = c("Observations", "R$^2$"),
  `All Enr. + Region FE` = c(format(nobs(mod1_rfe), big.mark = ","), sprintf("%.3f", r2(mod1_rfe, "r2"))),
  `New Enr. + Region FE` = c(format(nobs(mod2_rfe), big.mark = ","), sprintf("%.3f", r2(mod2_rfe, "r2"))),
  check.names            = FALSE,
  stringsAsFactors       = FALSE
)
tab_rfe <- rbind(tab_rfe, gof_rfe)
dom_tab_rfe <- kable(tab_rfe, format = "latex", booktabs = TRUE,
                     align = c("l", "c", "c"), linesep = "", escape = FALSE) %>%
  kable_styling(latex_options = c("scale_down"))
writeLines(as.character(dom_tab_rfe), "results/tables/dominated_choice_regionfe.tex")

# =========================================================================
# Baseline prediction-based ATT (IPW, selection on observables)
# =========================================================================
# Fit the dominated-choice model on the unassisted, predict the counterfactual
# for the assisted, ATT = observed - predicted. No control function here (this is
# the observational baseline); the selection-corrected versions are the pooled CF
# model (mod3) and the MTE. Same demographic set and year-FE-only structure as
# the pooled models above.

cat("Baseline prediction-based ATT...\n")

po_formula <- dominated_choice ~
  perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
  perc_black + perc_hispanic + perc_asian + perc_other +
  FPL_250to400 + FPL_400plus + household_size | year

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


# =========================================================================
# MTE (separate approach): a LATE over the propensity support
# =========================================================================
# Untreated and treated regressions of dominated choice on demographics and a
# flexible propensity, estimated SEPARATELY on the unassisted and the assisted,
# each household-size weighted (the broker-density instrument carries the
# selection correction here, not IPW). Propensity P = assisted - v_hat, the
# build3 first stage. MTE(p) = MTR1(p) - MTR0(p) with
#   MTR1(p) =  d/dp[ p * E(Y | X, P=p, assisted) ]
#   MTR0(p) = -d/dp[ (1-p) * E(Y | X, P=p, unassisted) ].
# Reported only over the common support of P -- a LATE for the marginal
# households the instrument moves, no extrapolation to a full ATT. Uses
# SAMPLE_FRAC so the assisted + unassisted fits stay tractable.

cat("Dominated MTE (LATE over the propensity support)...\n")

# Main sample = all enrollees (hh_po_all), matching the structural model.
set.seed(MASTER_SEED)
mte_po <- hh_po_all %>%
  mutate(P = assisted - v_hat, P2 = (assisted - v_hat)^2) %>%
  filter(!is.na(P), !is.na(dominated_choice)) %>%
  slice_sample(prop = SAMPLE_FRAC)

dom_mte_un <- feols(
  dominated_choice ~ perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
    perc_black + perc_hispanic + perc_asian + perc_other +
    FPL_250to400 + FPL_400plus + household_size + P + P2,
  data = filter(mte_po, assisted == 0), weights = ~weight)
dom_mte_as <- feols(
  dominated_choice ~ perc_0to17 + perc_18to34 + perc_35to54 + perc_male +
    perc_black + perc_hispanic + perc_asian + perc_other +
    FPL_250to400 + FPL_400plus + household_size + P + P2,
  data = filter(mte_po, assisted == 1), weights = ~weight)

# Common support of P (1st-99th pct overlap of the two groups).
p_lo <- max(quantile(mte_po$P[mte_po$assisted == 0], 0.01),
            quantile(mte_po$P[mte_po$assisted == 1], 0.01))
p_hi <- min(quantile(mte_po$P[mte_po$assisted == 0], 0.99),
            quantile(mte_po$P[mte_po$assisted == 1], 0.99))

# E(Y | X, P=p) marginalized over the sample's X, for one group's fit.
ebar <- function(fit, p) {
  nd <- mte_po; nd$P <- p; nd$P2 <- p^2
  weighted.mean(predict(fit, newdata = nd), mte_po$weight)
}
eps <- 0.01
dom_mte_grid <- seq(p_lo, p_hi, length.out = 9)
dom_mte <- vapply(dom_mte_grid, function(p) {
  mtr1 <-  ((p + eps) * ebar(dom_mte_as, p + eps) -
            (p - eps) * ebar(dom_mte_as, p - eps)) / (2 * eps)
  mtr0 <- -(((1 - p - eps) * ebar(dom_mte_un, p + eps) -
             (1 - p + eps) * ebar(dom_mte_un, p - eps)) / (2 * eps))
  mtr1 - mtr0
}, numeric(1))

dom_mte_tab <- tibble(propensity = dom_mte_grid, mte = dom_mte)
dom_late <- mean(dom_mte)
fwrite(dom_mte_tab, "results/dominated_mte.csv")
cat(sprintf("  Dominated LATE over support [%.2f, %.2f]: %+.4f\n", p_lo, p_hi, dom_late))
print(dom_mte_tab)


cat("Dominated choice analysis complete.\n")
print(att_summary)

# Free hh_po and bootstrap residues; _reduced-form.R is done with HH-level data.
rm(hh_po, hh_po_all, boot_any, boot_agent, boot_nav, dom_models, mod1, mod2, mod3, mod1_rfe, mod2_rfe)
gc(verbose = FALSE)
