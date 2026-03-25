# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-24
## Description:   Summarize nested logit choice model results: ATT by metal
##                and insurer with bootstrap confidence intervals.
##                Port of _old-repo/analysis/decision-support/_ChoiceSummary.R.


# Load results (if not already in memory) ---------------------------------

if (!exists("all_prob")) {
  all_prob     <- read_csv("data/output/choice_point_estimates.csv", show_col_types = FALSE)
  sim_bs_pred  <- read_csv("data/output/choice_bootstrap_pred.csv", show_col_types = FALSE)
}


# Parse plan names ---------------------------------------------------------
# Plan names follow the pattern {INSURER}_{METAL}[_SUFFIX] where SUFFIX is
# a network variant (3, 2), HSA, or COIN. We separate on the first underscore
# and classify the metal by its leading characters.

parse_plan_names <- function(df) {
  df %>%
    separate(plan_name, c("insurer_abbr", "metal_raw"),
             sep = "_", extra = "merge", fill = "right") %>%
    mutate(
      metal = case_when(
        str_starts(metal_raw, "SIL") ~ "SIL",
        str_starts(metal_raw, "BR")  ~ "BR",
        str_starts(metal_raw, "G")   ~ "G",
        str_starts(metal_raw, "P")   ~ "P",
        str_starts(metal_raw, "CAT") ~ "CAT",
        is.na(metal_raw)             ~ "Uninsured",
        TRUE                         ~ metal_raw
      )
    )
}

# Compute ATT shares (observed - predicted, conditional on insured)
compute_att <- function(df, group_var) {
  df %>%
    filter(metal != "Uninsured") %>%
    group_by({{ group_var }}) %>%
    summarize(
      obs_purchase  = sum(obs_purchase, na.rm = TRUE),
      pred_purchase = sum(pred_purchase, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      e_y1 = obs_purchase / sum(obs_purchase),
      e_y0 = pred_purchase / sum(pred_purchase),
      att  = e_y1 - e_y0
    )
}


# Point estimates ----------------------------------------------------------

pred_parsed <- all_prob %>% ungroup() %>% parse_plan_names()

metal_summary <- compute_att(pred_parsed, metal)
ins_summary   <- compute_att(pred_parsed, insurer_abbr)


# Bootstrap CIs ------------------------------------------------------------

bs_parsed <- sim_bs_pred %>% ungroup() %>% parse_plan_names()

# Metal CIs (bootstrap SE, centered on point estimate)
bs_metal <- bs_parsed %>%
  filter(metal != "Uninsured") %>%
  group_by(metal, boot) %>%
  summarize(
    obs_purchase  = sum(obs_purchase, na.rm = TRUE),
    pred_purchase = sum(pred_purchase, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(boot) %>%
  mutate(
    e_y1 = obs_purchase / sum(obs_purchase),
    e_y0 = pred_purchase / sum(pred_purchase),
    att  = e_y1 - e_y0
  ) %>%
  ungroup() %>%
  group_by(metal) %>%
  summarize(
    se = sd(att, na.rm = TRUE),
    .groups = "drop"
  )

# Insurer CIs (bootstrap SE, centered on point estimate)
bs_ins <- bs_parsed %>%
  filter(metal != "Uninsured") %>%
  group_by(insurer_abbr, boot) %>%
  summarize(
    obs_purchase  = sum(obs_purchase, na.rm = TRUE),
    pred_purchase = sum(pred_purchase, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(boot) %>%
  mutate(
    e_y1 = obs_purchase / sum(obs_purchase),
    e_y0 = pred_purchase / sum(pred_purchase),
    att  = e_y1 - e_y0
  ) %>%
  ungroup() %>%
  group_by(insurer_abbr) %>%
  summarize(
    se = sd(att, na.rm = TRUE),
    .groups = "drop"
  )


# Merge point estimates with CIs ------------------------------------------

metal_final <- metal_summary %>%
  left_join(bs_metal, by = "metal") %>%
  mutate(ci_lo = att - 1.96 * se, ci_hi = att + 1.96 * se)

ins_final <- ins_summary %>%
  left_join(bs_ins, by = "insurer_abbr") %>%
  mutate(ci_lo = att - 1.96 * se, ci_hi = att + 1.96 * se)


# Print summaries ----------------------------------------------------------
cat("\nATT by Metal Level:\n")
print(metal_final %>% select(metal, att, se, ci_lo, ci_hi), n = Inf)
cat("\nATT by Insurer:\n")
print(ins_final %>% select(insurer_abbr, att, se, ci_lo, ci_hi), n = Inf)

# Figures ------------------------------------------------------------------

# Decode metal abbreviations
metal_labels <- c(G = "Gold", BR = "Bronze", P = "Platinum",
                  SIL = "Silver", CAT = "Catastrophic")
metal_order  <- c("Platinum", "Gold", "Silver", "Bronze", "Catastrophic")

choice_metals <- metal_final %>%
  mutate(metal = factor(metal_labels[metal], levels = metal_order)) %>%
  ggplot(aes(x = metal, y = att)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.15, linewidth = 0.8) +
  geom_point(size = 1.5, shape = 21, fill = "white") +
  labs(y = "Estimate and\n95% Confidence Interval", x = "Metal Level") +
  theme_bw()
ggsave("results/figures/choice_metals.png", choice_metals, width = 6, height = 4)

# Decode insurer abbreviations
ins_labels <- c(ANT = "Anthem", BS = "Blue Shield", HN = "Health Net",
                KA = "Kaiser", Small = "Other")
ins_order  <- c("Anthem", "Blue Shield", "Health Net", "Kaiser", "Other")

choice_insurer <- ins_final %>%
  mutate(insurer = factor(ins_labels[insurer_abbr], levels = ins_order)) %>%
  ggplot(aes(x = insurer, y = att)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.15, linewidth = 0.8) +
  geom_point(size = 1.5, shape = 21, fill = "white") +
  labs(y = "Estimate and\n95% Confidence Interval", x = "Insurer") +
  theme_bw()
ggsave("results/figures/choice_insurer.png", choice_insurer, width = 6, height = 4)

cat("Choice summary complete. Figures saved to results/figures/.\n")
