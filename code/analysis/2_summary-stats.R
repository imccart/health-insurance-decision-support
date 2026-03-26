# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Description:   Summary statistics, propensity scores, IPW, covariate balance.
##                Port of _old-repo/analysis/decision-support/_SummaryStats.R

# Metal x channel crosstabs -----------------------------------------------

metal_channel_all <- hh_ins %>%
  count(metal, channel) %>%
  group_by(channel) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

metal_channel_new <- hh_clean %>%
  filter(!is.na(plan_number_nocsr)) %>%
  count(metal, channel) %>%
  group_by(channel) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# Stacked bar charts: metal by channel ------------------------------------

plot_metal_all <- ggplot(metal_channel_all,
                         aes(x = channel, y = pct, fill = metal)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Share", fill = "Metal Level",
       title = "Metal Level by Channel (All Enrollees)") +
  theme_minimal()
ggsave("results/figures/metal_stack_all.png", plot_metal_all, width = 6, height = 4)

plot_metal_new <- ggplot(metal_channel_new,
                         aes(x = channel, y = pct, fill = metal)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Share", fill = "Metal Level",
       title = "Metal Level by Channel (New Enrollees)") +
  theme_minimal()
ggsave("results/figures/metal_stack_any.png", plot_metal_new, width = 6, height = 4)

# Stacked bar charts: insurer by channel ----------------------------------

insurer_channel <- hh_ins %>%
  mutate(
    insurer_group = case_when(
      insurer %in% c("Anthem", "Blue_Shield", "Kaiser", "Health_Net") ~ insurer,
      is.na(insurer) ~ "Uninsured",
      TRUE ~ "Other"
    )
  ) %>%
  count(insurer_group, channel) %>%
  group_by(channel) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

plot_insurer_all <- ggplot(insurer_channel,
                           aes(x = channel, y = pct, fill = insurer_group)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Share", fill = "Insurer",
       title = "Insurer by Channel (All Enrollees)") +
  theme_minimal()
ggsave("results/figures/insurer_stack_all.png", plot_insurer_all, width = 6, height = 4)

insurer_channel_new <- hh_clean %>%
  filter(!is.na(plan_number_nocsr)) %>%
  mutate(
    insurer_group = case_when(
      insurer %in% c("Anthem", "Blue_Shield", "Kaiser", "Health_Net") ~ insurer,
      is.na(insurer) ~ "Uninsured",
      TRUE ~ "Other"
    )
  ) %>%
  count(insurer_group, channel) %>%
  group_by(channel) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

plot_insurer_new <- ggplot(insurer_channel_new,
                           aes(x = channel, y = pct, fill = insurer_group)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Share", fill = "Insurer",
       title = "Insurer by Channel (New Enrollees)") +
  theme_minimal()
ggsave("results/figures/insurer_stack_any.png", plot_insurer_new, width = 6, height = 4)

# Enrollee count time series ----------------------------------------------

enroll_ts <- hh_ins %>%
  count(year, channel) %>%
  bind_rows(
    hh_ins %>% count(year) %>% mutate(channel = "Total")
  )

plot_enroll <- ggplot(enroll_ts, aes(x = year, y = n, color = channel)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = "Enrollees", color = NULL,
       title = "Enrollment by Channel") +
  theme_minimal()
ggsave("results/figures/enrollee_count.png", plot_enroll, width = 7, height = 4)

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

# Estimate PS on insured only — the population used in downstream regressions
hh_ins <- estimate_ps(hh_ins) %>%
  mutate(ipweight = if_else(assisted == 1, 1, pred_assist / (1 - pred_assist)))

# Propagate weights to hh_full and hh_clean via join
hh_full <- hh_full %>%
  left_join(hh_ins %>% select(household_year, pred_assist, ipweight),
            by = "household_year")

hh_clean <- hh_clean %>%
  left_join(hh_ins %>% select(household_year, pred_assist, ipweight),
            by = "household_year")

# Propensity score histograms --------------------------------------------

plot_ps_ins <- ggplot(hh_ins, aes(x = pred_assist, fill = channel)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(x = "Propensity Score", y = "Count", fill = NULL,
       title = "Propensity Score Distribution (Insured)") +
  theme_minimal()
ggsave("results/figures/ps_assist_full.png", plot_ps_ins, width = 7, height = 4)

hh_clean_ins <- hh_clean %>% filter(!is.na(plan_number_nocsr))
plot_ps_clean <- ggplot(hh_clean_ins, aes(x = pred_assist, fill = channel)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(x = "Propensity Score", y = "Count", fill = NULL,
       title = "Propensity Score Distribution (New Enrollees)") +
  theme_minimal()
ggsave("results/figures/ps_assist_clean.png", plot_ps_clean, width = 7, height = 4)

# Covariate balance (cobalt) ----------------------------------------------

bal_vars <- c("FPL", "perc_0to17", "perc_18to25", "perc_65plus",
              "perc_black", "perc_hispanic", "perc_asian", "perc_male",
              "household_size")

bal <- bal.tab(
  x = hh_ins %>% select(all_of(bal_vars)),
  treat = hh_ins %>% pull(assisted),
  weights = hh_ins %>% pull(ipweight),
  method = "weighting",
  estimand = "ATT",
  s.d.denom = "treated",
  binary = "std",
  un = TRUE
)

plot_bal <- love.plot(bal, thresholds = c(m = 0.1),
                      abs = TRUE, var.order = "unadjusted",
                      title = "Covariate Balance (IPW)") +
  theme_minimal()
ggsave("results/figures/cov_balance.png", plot_bal, width = 7, height = 5)

# Summary statistics table ------------------------------------------------

summary_by_group <- function(df, group_var) {
  df %>%
    group_by({{ group_var }}) %>%
    summarize(
      N = n(),
      FPL = mean(FPL, na.rm = TRUE),
      `HH Size` = mean(household_size, na.rm = TRUE),
      `% Male` = mean(perc_male, na.rm = TRUE),
      `% Age 0-17` = mean(perc_0to17, na.rm = TRUE),
      `% Age 18-34` = mean(perc_18to34, na.rm = TRUE),
      `% Age 55-64` = mean(perc_55to64, na.rm = TRUE),
      `% Black` = mean(perc_black, na.rm = TRUE),
      `% Hispanic` = mean(perc_hispanic, na.rm = TRUE),
      `% Asian` = mean(perc_asian, na.rm = TRUE),
      `% Dominated` = mean(dominated_choice, na.rm = TRUE),
      .groups = "drop"
    )
}

overall_stats <- hh_clean %>%
  summarize(
    N = n(),
    FPL = mean(FPL, na.rm = TRUE),
    `HH Size` = mean(household_size, na.rm = TRUE),
    `% Male` = mean(perc_male, na.rm = TRUE),
    `% Age 0-17` = mean(perc_0to17, na.rm = TRUE),
    `% Age 18-34` = mean(perc_18to34, na.rm = TRUE),
    `% Age 55-64` = mean(perc_55to64, na.rm = TRUE),
    `% Black` = mean(perc_black, na.rm = TRUE),
    `% Hispanic` = mean(perc_hispanic, na.rm = TRUE),
    `% Asian` = mean(perc_asian, na.rm = TRUE),
    `% Dominated` = mean(dominated_choice, na.rm = TRUE)
  ) %>%
  mutate(Group = "Overall", .before = 1)

sum_stats <- bind_rows(
  summary_by_group(hh_clean, channel) %>% rename(Group = channel),
  overall_stats
)

sum_tab <- sum_stats %>%
  kable(format = "latex", booktabs = TRUE, digits = 3, linesep = "",
        caption = "Summary Statistics: New Enrollees") %>%
  kable_styling(font_size = 10)

writeLines(sum_tab, "results/tables/summary_stats.tex")

# Save datasets with IPW weights for downstream scripts
write_csv(hh_full, "data/output/hh_full.csv")
write_csv(hh_clean, "data/output/hh_clean.csv")
write_csv(hh_ins, "data/output/hh_ins.csv")
cat("  Updated: hh_full.csv, hh_clean.csv, hh_ins.csv (with IPW weights)\n")

cat("Summary statistics complete.\n")
