# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-21
## Description:   Summary statistics, covariate balance, and figures.
##                IPW weights computed in 2_ipw.R (must run first).

hh_full   <- fread("data/output/hh_full.csv") %>% as_tibble()
ipweights <- fread("data/output/ipweights.csv") %>% as_tibble()

hh_ins   <- hh_full %>% filter(insured == 1L)     %>% left_join(ipweights, by = "household_year")
hh_clean <- hh_full %>% filter(new_enrollee == 1L) %>% left_join(ipweights, by = "household_year")
rm(hh_full, ipweights)
gc(verbose = FALSE)

# Grayscale theme for all figures ------------------------------------------

theme_paper <- theme_bw() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )

grey_palette <- c("gray30", "gray70")
grey_fill <- scale_fill_grey(start = 0.3, end = 0.7)
grey_color <- scale_color_grey(start = 0.2, end = 0.6)

# Metal x channel crosstabs -----------------------------------------------

metal_channel_all <- hh_ins %>%
  count(metal, channel) %>%
  group_by(channel) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

metal_channel_new <- hh_clean %>%
  filter(!is.na(plan_id)) %>%
  count(metal, channel) %>%
  group_by(channel) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# Stacked bar charts: metal by channel ------------------------------------

plot_metal_all <- ggplot(metal_channel_all,
                         aes(x = channel, y = pct, fill = metal)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_grey(start = 0.2, end = 0.9) +
  labs(x = NULL, y = "Share") +
  theme_paper
ggsave("results/figures/metal_stack_all.png", plot_metal_all, width = 6, height = 4, bg = "white")

plot_metal_new <- ggplot(metal_channel_new,
                         aes(x = channel, y = pct, fill = metal)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_grey(start = 0.2, end = 0.9) +
  labs(x = NULL, y = "Share") +
  theme_paper
ggsave("results/figures/metal_stack_any.png", plot_metal_new, width = 6, height = 4, bg = "white")

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
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_grey(start = 0.2, end = 0.9) +
  labs(x = NULL, y = "Share") +
  theme_paper
ggsave("results/figures/insurer_stack_all.png", plot_insurer_all, width = 6, height = 4, bg = "white")

insurer_channel_new <- hh_clean %>%
  filter(!is.na(plan_id)) %>%
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
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_grey(start = 0.2, end = 0.9) +
  labs(x = NULL, y = "Share") +
  theme_paper
ggsave("results/figures/insurer_stack_any.png", plot_insurer_new, width = 6, height = 4, bg = "white")

# Enrollee count time series ----------------------------------------------

enroll_ts <- hh_ins %>%
  count(year, channel) %>%
  bind_rows(
    hh_ins %>% count(year) %>% mutate(channel = "Total")
  )

plot_enroll <- ggplot(enroll_ts, aes(x = year, y = n, linetype = channel)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 2, color = "black") +
  scale_y_continuous(labels = scales::comma) +
  scale_linetype_manual(values = c("Assisted" = "solid", "Unassisted" = "dashed", "Total" = "dotted")) +
  labs(x = "Year", y = "Enrollees") +
  theme_paper
ggsave("results/figures/enrollee_count.png", plot_enroll, width = 7, height = 4, bg = "white")


# Propensity score histograms (grayscale) ---------------------------------

plot_ps_ins <- ggplot(hh_ins, aes(x = pred_assist, fill = channel)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  grey_fill +
  labs(x = "Propensity Score", y = "Count") +
  theme_paper
ggsave("results/figures/ps_assist_full.png", plot_ps_ins, width = 7, height = 4, bg = "white")

hh_clean_ins <- hh_clean %>% filter(!is.na(plan_id))
plot_ps_clean <- ggplot(hh_clean_ins, aes(x = pred_assist, fill = channel)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  grey_fill +
  labs(x = "Propensity Score", y = "Count") +
  theme_paper
ggsave("results/figures/ps_assist_clean.png", plot_ps_clean, width = 7, height = 4, bg = "white")

# Covariate balance (ATT, s.d. of treated) --------------------------------

bal_vars <- c("FPL", "perc_0to17", "perc_18to25", "perc_65plus",
              "perc_black", "perc_hispanic", "perc_asian", "perc_male",
              "household_size")

bal_labels <- c(
  FPL = "Federal Poverty Level",
  perc_0to17 = "Share Age 0-17",
  perc_18to25 = "Share Age 18-25",
  perc_65plus = "Share Age 65+",
  perc_black = "Share Black",
  perc_hispanic = "Share Hispanic",
  perc_asian = "Share Asian",
  perc_male = "Share Male",
  household_size = "Household Size"
)

smd_att <- function(x, treat, w) {
  ok_t  <- treat == 1L & !is.na(x)
  ok_c  <- treat == 0L & !is.na(x)
  m_t   <- mean(x[ok_t])
  sd_t  <- sd(x[ok_t])
  m_c_u <- mean(x[ok_c])
  m_c_w <- sum(x[ok_c] * w[ok_c]) / sum(w[ok_c])
  c(unadj = (m_t - m_c_u) / sd_t, adj = (m_t - m_c_w) / sd_t)
}

bal_df <- map_dfr(bal_vars, function(v) {
  s <- smd_att(hh_ins[[v]], hh_ins$assisted, hh_ins$ipweight)
  tibble(var = v, unadj = s["unadj"], adj = s["adj"])
}) %>%
  mutate(label = bal_labels[var]) %>%
  arrange(abs(unadj)) %>%
  mutate(label = factor(label, levels = label)) %>%
  pivot_longer(c(unadj, adj), names_to = "sample", values_to = "smd") %>%
  mutate(sample = factor(sample, levels = c("unadj", "adj"),
                         labels = c("Unadjusted", "IPW-weighted")))

plot_bal <- ggplot(bal_df, aes(x = abs(smd), y = label,
                               color = sample, shape = sample)) +
  geom_vline(xintercept = 0.1, linetype = "dashed", color = "gray50") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Unadjusted" = "gray60", "IPW-weighted" = "gray20")) +
  scale_shape_manual(values = c("Unadjusted" = 17, "IPW-weighted" = 16)) +
  labs(x = "Absolute standardized mean difference", y = NULL) +
  theme_paper
ggsave("results/figures/cov_balance.png", plot_bal, width = 7, height = 5, bg = "white")

# Summary statistics table (transposed: variables as rows, groups as cols) -

summary_by_group <- function(df, group_var) {
  df %>%
    group_by({{ group_var }}) %>%
    summarize(
      N = n(),
      FPL = mean(FPL, na.rm = TRUE),
      `HH Size` = mean(household_size, na.rm = TRUE),
      `\\% Male` = mean(perc_male, na.rm = TRUE),
      `\\% Age 0-17` = mean(perc_0to17, na.rm = TRUE),
      `\\% Age 18-34` = mean(perc_18to34, na.rm = TRUE),
      `\\% Age 55-64` = mean(perc_55to64, na.rm = TRUE),
      `\\% Black` = mean(perc_black, na.rm = TRUE),
      `\\% Hispanic` = mean(perc_hispanic, na.rm = TRUE),
      `\\% Asian` = mean(perc_asian, na.rm = TRUE),
      `\\% Dominated` = mean(dominated_choice, na.rm = TRUE),
      .groups = "drop"
    )
}

group_stats <- summary_by_group(hh_clean, channel)
overall <- hh_clean %>%
  summarize(
    N = n(),
    FPL = mean(FPL, na.rm = TRUE),
    `HH Size` = mean(household_size, na.rm = TRUE),
    `\\% Male` = mean(perc_male, na.rm = TRUE),
    `\\% Age 0-17` = mean(perc_0to17, na.rm = TRUE),
    `\\% Age 18-34` = mean(perc_18to34, na.rm = TRUE),
    `\\% Age 55-64` = mean(perc_55to64, na.rm = TRUE),
    `\\% Black` = mean(perc_black, na.rm = TRUE),
    `\\% Hispanic` = mean(perc_hispanic, na.rm = TRUE),
    `\\% Asian` = mean(perc_asian, na.rm = TRUE),
    `\\% Dominated` = mean(dominated_choice, na.rm = TRUE)
  ) %>%
  mutate(channel = "Overall", .before = 1)

sum_stats <- bind_rows(group_stats, overall)

# Transpose: variables as rows, groups as columns
var_names <- setdiff(names(sum_stats), "channel")
tab_rows <- lapply(var_names, function(v) {
  vals <- sum_stats %>% pull(v)
  fmt <- if (v == "N") scales::comma(vals) else sprintf("%.3f", vals)
  names(fmt) <- sum_stats$channel
  c(Variable = v, fmt)
})
tab_df <- do.call(rbind, tab_rows) %>% as.data.frame()

# Build bare tabular
header <- paste0("Variable & Assisted & Unassisted & Overall")
rows <- apply(tab_df, 1, function(r) paste(r, collapse = " & "))
tex_lines <- c(
  "\\begin{tabular}{lrrr}",
  "\\hline\\hline",
  paste0(header, " \\\\"),
  "\\hline",
  paste0(rows, " \\\\"),
  "\\hline\\hline",
  "\\end{tabular}"
)
writeLines(tex_lines, "results/tables/summary_stats.tex")

cat("Summary statistics complete.\n")
