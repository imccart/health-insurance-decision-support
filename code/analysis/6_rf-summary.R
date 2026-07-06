# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-24
## Date Edited:   2026-03-25
## Description:   Summarize choice model ATT results by metal tier and insurer.


# Load results (if not already in memory) ---------------------------------

if (!exists("all_prob")) {
  all_prob <- read_csv("results/choice_point_estimates.csv", show_col_types = FALSE,
                       col_types = cols(plan_id = "c", tot_nonmiss = "i",
                                        obs_purchase = "d", pred_purchase = "d",
                                        region = "i", year = "i"))
}
if (!exists("sim_bs_pred")) {
  bs_file <- "results/choice_bootstrap_pred.csv"
  if (file.exists(bs_file)) {
    sim_bs_pred <- read_csv(bs_file, show_col_types = FALSE)
  } else {
    sim_bs_pred <- tibble(plan_id = character(0), tot_nonmiss = integer(0),
                          obs_purchase = numeric(0), pred_purchase = numeric(0),
                          region = integer(0), year = integer(0), boot = integer(0))
  }
}


# Parse plan names ---------------------------------------------------------
# Plan names follow the pattern {INSURER}_{METAL}[_SUFFIX] where SUFFIX is
# a network variant (3, 2), HSA, or COIN. We separate on the first underscore
# and classify the metal by its leading characters.

parse_plan_ids <- function(df) {
  df %>%
    mutate(
      insurer_abbr = sub("_.*", "", plan_id),
      metal_raw = sub("^[^_]*_?", "", plan_id),
      metal_raw = if_else(metal_raw == "", NA_character_, metal_raw),
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

pred_parsed <- all_prob %>% ungroup() %>% parse_plan_ids()

metal_summary <- compute_att(pred_parsed, metal)
ins_summary   <- compute_att(pred_parsed, insurer_abbr)


# Bootstrap CIs ------------------------------------------------------------

has_bootstrap <- nrow(sim_bs_pred) > 0

if (has_bootstrap) {
bs_parsed <- sim_bs_pred %>% ungroup() %>% parse_plan_ids()

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
} else {
  bs_metal <- tibble(metal = character(0), se = numeric(0))
  bs_ins   <- tibble(insurer_abbr = character(0), se = numeric(0))
}


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

theme_paper <- theme_bw() +
  theme(text = element_text(size = 12), panel.grid.minor = element_blank(),
        plot.title = element_blank())

choice_metals <- metal_final %>%
  mutate(metal = factor(metal_labels[metal], levels = metal_order)) %>%
  ggplot(aes(x = metal, y = att)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.15, linewidth = 0.8) +
  geom_point(size = 2, color = "black") +
  labs(y = "ATT (percentage points)", x = "Metal Level") +
  theme_paper
ggsave("results/figures/choice_metals.png", choice_metals, width = 6, height = 4, bg = "white")

# Decode insurer abbreviations
ins_labels <- c(ANT = "Anthem", BS = "Blue Shield", HN = "Health Net",
                KA = "Kaiser", Small = "Other")
ins_order  <- c("Anthem", "Blue Shield", "Health Net", "Kaiser", "Other")

choice_insurer <- ins_final %>%
  mutate(insurer = factor(ins_labels[insurer_abbr], levels = ins_order)) %>%
  ggplot(aes(x = insurer, y = att)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.15, linewidth = 0.8) +
  geom_point(size = 2, color = "black") +
  labs(y = "ATT (percentage points)", x = "Insurer") +
  theme_paper
ggsave("results/figures/choice_insurer.png", choice_insurer, width = 6, height = 4, bg = "white")

cat("Choice summary complete. Figures saved to results/figures/.\n")
