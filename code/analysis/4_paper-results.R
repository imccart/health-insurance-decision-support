# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-26
## Description:   Generate all paper tables and figures from pipeline outputs.
##                Reads estimation outputs from results/ and intermediate data
##                from data/output/. Writes tables to results/tables/ and
##                figures to results/figures/. Re-run this script (seconds)
##                whenever you want to update paper assets without re-running
##                estimation.

source("code/0-setup.R")

TEMP_DIR <- Sys.getenv("TEMP_DIR")

# Ensure output dirs exist
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)


# =========================================================================
# 0. Load pipeline outputs
# =========================================================================

cat("Loading pipeline outputs...\n")

hh_full  <- read_csv("data/output/hh_full.csv", show_col_types = FALSE)
hh_clean <- read_csv("data/output/hh_clean.csv", show_col_types = FALSE)
hh_ins   <- read_csv("data/output/hh_ins.csv", show_col_types = FALSE)

commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)
plan_choice       <- read_csv(file.path(TEMP_DIR, "plan_choice.csv"), show_col_types = FALSE)

coefs_structural <- read_csv("results/choice_coefficients_structural.csv",
                              show_col_types = FALSE)
supply_results   <- read_csv("results/supply_results.csv", show_col_types = FALSE)

# Counterfactual and bootstrap results (may not exist yet)
cf_results <- tryCatch(
  read_csv("results/counterfactual_results.csv", show_col_types = FALSE),
  error = function(e) { cat("  counterfactual_results.csv not found\n"); NULL }
)
boot_coefs <- tryCatch(
  read_csv("results/choice_bootstrap_coef.csv", show_col_types = FALSE),
  error = function(e) { cat("  choice_bootstrap_coef.csv not found\n"); NULL }
)

cat("  hh_full:", nrow(hh_full), "rows\n")
cat("  supply_results:", nrow(supply_results), "rows\n")
if (!is.null(cf_results)) cat("  cf_results:", nrow(cf_results), "rows\n")


# =========================================================================
# 1. Summary Statistics Table
# =========================================================================

cat("\n--- Table: Summary Statistics ---\n")

# Build summary stats by assistance status
make_summary <- function(df, label) {
  df %>%
    summarize(
      label = label,
      n = n(),
      pct_insured = mean(!is.na(plan_number_nocsr)) * 100,
      mean_FPL = mean(FPL, na.rm = TRUE),
      mean_hh_size = mean(household_size, na.rm = TRUE),
      pct_new = mean(new_enrollee, na.rm = TRUE) * 100,
      pct_silver = mean(metal == "Silver", na.rm = TRUE) * 100,
      pct_bronze = mean(metal == "Bronze", na.rm = TRUE) * 100,
      pct_gold = mean(metal == "Gold", na.rm = TRUE) * 100,
      pct_hmo = mean(grepl("HMO", plan_network_type, ignore.case = TRUE),
                      na.rm = TRUE) * 100,
      mean_age_oldest = mean(oldest_member, na.rm = TRUE),
      pct_hispanic = mean(perc_hispanic > 0, na.rm = TRUE) * 100,
      pct_black = mean(perc_black > 0, na.rm = TRUE) * 100,
      .groups = "drop"
    )
}

# Check which columns exist (names vary across pipeline versions)
has_col <- function(df, col) col %in% names(df)

if (has_col(hh_full, "channel")) {
  ss_assisted   <- hh_full %>% filter(channel != "Unassisted") %>% make_summary("Assisted")
  ss_unassisted <- hh_full %>% filter(channel == "Unassisted") %>% make_summary("Unassisted")
  ss_overall    <- hh_full %>% make_summary("Overall")
  ss <- bind_rows(ss_assisted, ss_unassisted, ss_overall)

  # Format for LaTeX (bare tabular, \hline\hline style)
  fmt <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")

  tab_lines <- c(
    "\\begin{tabular}{lrrr}",
    "\\hline\\hline",
    " & Assisted & Unassisted & Overall \\\\",
    "\\hline",
    sprintf("Observations & %s & %s & %s \\\\",
            fmt(ss$n[1], 0), fmt(ss$n[2], 0), fmt(ss$n[3], 0)),
    sprintf("Insured (\\%%) & %s & %s & %s \\\\",
            fmt(ss$pct_insured[1]), fmt(ss$pct_insured[2]), fmt(ss$pct_insured[3])),
    sprintf("New enrollee (\\%%) & %s & %s & %s \\\\",
            fmt(ss$pct_new[1]), fmt(ss$pct_new[2]), fmt(ss$pct_new[3])),
    sprintf("Mean FPL & %s & %s & %s \\\\",
            fmt(ss$mean_FPL[1]), fmt(ss$mean_FPL[2]), fmt(ss$mean_FPL[3])),
    sprintf("Mean household size & %s & %s & %s \\\\",
            fmt(ss$mean_hh_size[1]), fmt(ss$mean_hh_size[2]), fmt(ss$mean_hh_size[3])),
    sprintf("Mean age (oldest) & %s & %s & %s \\\\",
            fmt(ss$mean_age_oldest[1]), fmt(ss$mean_age_oldest[2]), fmt(ss$mean_age_oldest[3])),
    sprintf("Any Hispanic (\\%%) & %s & %s & %s \\\\",
            fmt(ss$pct_hispanic[1]), fmt(ss$pct_hispanic[2]), fmt(ss$pct_hispanic[3])),
    sprintf("Any Black (\\%%) & %s & %s & %s \\\\",
            fmt(ss$pct_black[1]), fmt(ss$pct_black[2]), fmt(ss$pct_black[3])),
    "\\hline",
    "\\emph{Conditional on insured} & & & \\\\",
    sprintf("Silver (\\%%) & %s & %s & %s \\\\",
            fmt(ss$pct_silver[1]), fmt(ss$pct_silver[2]), fmt(ss$pct_silver[3])),
    sprintf("Bronze (\\%%) & %s & %s & %s \\\\",
            fmt(ss$pct_bronze[1]), fmt(ss$pct_bronze[2]), fmt(ss$pct_bronze[3])),
    sprintf("Gold (\\%%) & %s & %s & %s \\\\",
            fmt(ss$pct_gold[1]), fmt(ss$pct_gold[2]), fmt(ss$pct_gold[3])),
    sprintf("HMO (\\%%) & %s & %s & %s \\\\",
            fmt(ss$pct_hmo[1]), fmt(ss$pct_hmo[2]), fmt(ss$pct_hmo[3])),
    "\\hline\\hline",
    "\\end{tabular}"
  )

  writeLines(tab_lines, "results/tables/summary_stats.tex")
  cat("  Wrote results/tables/summary_stats.tex\n")
} else {
  cat("  Skipped (channel column not found in hh_full)\n")
}


# =========================================================================
# 2. Commission Schedule Figures
# =========================================================================

cat("\n--- Figures: Commission Schedule ---\n")

comm <- commission_lookup %>%
  filter(year >= 2014, year <= 2019)

# Flat commission insurers
flat_comm <- comm %>%
  filter(!is_pct) %>%
  mutate(insurer = case_when(
    insurer_prefix == "ANT" ~ "Anthem",
    insurer_prefix == "KA"  ~ "Kaiser",
    insurer_prefix == "HN"  ~ "Health Net",
    insurer_prefix == "Small" ~ "Small Insurers",
    TRUE ~ insurer_prefix
  )) %>%
  filter(insurer_prefix %in% c("ANT", "KA", "HN", "Small"))

p_flat <- ggplot(flat_comm, aes(x = year, y = rate, color = insurer, shape = insurer)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(x = "Year", y = "Commission ($ PMPM)", color = NULL, shape = NULL) +
  scale_x_continuous(breaks = 2014:2019) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("results/figures/flat_comm.pdf", p_flat, width = 6, height = 4)
cat("  Wrote results/figures/flat_comm.pdf\n")

# Percentage commission insurers
pct_comm <- comm %>%
  filter(is_pct) %>%
  mutate(
    insurer = case_when(
      insurer_prefix == "BS" ~ "Blue Shield",
      insurer_prefix == "HN" ~ "Health Net",
      insurer_prefix == "Small" ~ "Sharp",
      TRUE ~ insurer_prefix
    ),
    rate_pct = rate * 100
  )

p_pct <- ggplot(pct_comm, aes(x = year, y = rate_pct, color = insurer, shape = insurer)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(x = "Year", y = "Commission (% of premium)", color = NULL, shape = NULL) +
  scale_x_continuous(breaks = 2014:2019) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("results/figures/perc_comm.pdf", p_pct, width = 6, height = 4)
cat("  Wrote results/figures/perc_comm.pdf\n")


# =========================================================================
# 3. Structural Demand Estimates Table
# =========================================================================

cat("\n--- Table: Structural Demand Estimates ---\n")

if (nrow(coefs_structural) > 0) {
  # Clean term names for display
  label_map <- c(
    "premium"            = "Premium",
    "penalty_own"        = "Penalty (outside option)",
    "premium_sq"         = "Premium$^2$",
    "silver"             = "Silver",
    "bronze"             = "Bronze",
    "hh_size_prem"       = "HH size $\\times$ premium",
    "any_0to17_prem"     = "Children $\\times$ premium",
    "FPL_250to400_prem"  = "FPL 250--400\\% $\\times$ premium",
    "FPL_400plus_prem"   = "FPL 400+\\% $\\times$ premium",
    "any_black_prem"     = "Black $\\times$ premium",
    "any_hispanic_prem"  = "Hispanic $\\times$ premium",
    "hmo"                = "HMO",
    "hsa"                = "HSA",
    "Anthem"             = "Anthem",
    "Blue_Shield"        = "Blue Shield",
    "Kaiser"             = "Kaiser",
    "Health_Net"         = "Health Net",
    "Anthem_silver"      = "Anthem $\\times$ Silver",
    "BS_silver"          = "Blue Shield $\\times$ Silver",
    "Kaiser_silver"      = "Kaiser $\\times$ Silver",
    "HN_silver"          = "Health Net $\\times$ Silver",
    "Anthem_bronze"      = "Anthem $\\times$ Bronze",
    "BS_bronze"          = "Blue Shield $\\times$ Bronze",
    "Kaiser_bronze"      = "Kaiser $\\times$ Bronze",
    "HN_bronze"          = "Health Net $\\times$ Bronze",
    "commission_broker"  = "Commission $\\times$ assisted",
    "v_hat_commission"   = "CF $\\times$ commission $\\times$ assisted",
    "lambda"             = "$\\lambda$ (nesting parameter)"
  )

  coefs_display <- coefs_structural %>%
    mutate(
      label = ifelse(term %in% names(label_map), label_map[term], term),
      est_str = formatC(estimate, format = "f", digits = 4),
      se_str  = if ("std_error" %in% names(.))
                  paste0("(", formatC(std_error, format = "f", digits = 4), ")")
                else ""
    )

  tab_lines <- c(
    "\\begin{tabular}{lr}",
    "\\hline\\hline",
    "Variable & Estimate \\\\",
    "\\hline"
  )

  for (i in seq_len(nrow(coefs_display))) {
    tab_lines <- c(tab_lines,
      sprintf("%s & %s \\\\", coefs_display$label[i], coefs_display$est_str[i])
    )
    if (coefs_display$se_str[i] != "") {
      tab_lines <- c(tab_lines,
        sprintf(" & %s \\\\", coefs_display$se_str[i])
      )
    }
  }

  tab_lines <- c(tab_lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(tab_lines, "results/tables/demand_estimates.tex")
  cat("  Wrote results/tables/demand_estimates.tex\n")
} else {
  cat("  Skipped (no structural coefficients)\n")
}


# =========================================================================
# 4. Supply-Side Results Table
# =========================================================================

cat("\n--- Table: Supply-Side Results ---\n")

if (nrow(supply_results) > 0) {
  sr <- supply_results %>%
    filter(!is.na(mc_foc), !is.na(posted_premium))

  # Summary by metal tier
  supply_by_metal <- sr %>%
    group_by(metal) %>%
    summarize(
      n_plan_years = n(),
      mean_premium = mean(posted_premium, na.rm = TRUE),
      mean_markup  = mean(markup, na.rm = TRUE),
      mean_mc_foc  = mean(mc_foc, na.rm = TRUE),
      mean_mc_str  = mean(mc_structural, na.rm = TRUE),
      mean_lerner  = mean(lerner_index, na.rm = TRUE),
      mean_comm    = mean(commission_pmpm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(factor(metal, levels = c("Bronze", "Silver", "Gold", "Platinum")))

  fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)

  tab_lines <- c(
    "\\begin{tabular}{lrrrrrrr}",
    "\\hline\\hline",
    "Metal & N & Premium & Markup & MC (FOC) & MC (Structural) & Lerner & Commission \\\\",
    "\\hline"
  )

  for (i in seq_len(nrow(supply_by_metal))) {
    r <- supply_by_metal[i, ]
    tab_lines <- c(tab_lines, sprintf(
      "%s & %d & %s & %s & %s & %s & %s & %s \\\\",
      r$metal, r$n_plan_years,
      fmt(r$mean_premium), fmt(r$mean_markup),
      fmt(r$mean_mc_foc), fmt(r$mean_mc_str),
      fmt(r$mean_lerner, 3), fmt(r$mean_comm)
    ))
  }

  tab_lines <- c(tab_lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(tab_lines, "results/tables/supply_results.tex")
  cat("  Wrote results/tables/supply_results.tex\n")

  # MC validation figure: FOC vs structural
  if ("mc_structural" %in% names(sr)) {
    sr_valid <- sr %>% filter(!is.na(mc_structural), mc_foc > 0, mc_structural > 0)
    if (nrow(sr_valid) > 5) {
      p_mc <- ggplot(sr_valid, aes(x = mc_structural, y = mc_foc)) +
        geom_point(alpha = 0.5, size = 1.5) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
        labs(x = "MC (Structural prediction)", y = "MC (FOC inversion)") +
        theme_minimal(base_size = 12)
      ggsave("results/figures/supply_mc_foc_vs_structural.png", p_mc, width = 6, height = 5)
      cat("  Wrote results/figures/supply_mc_foc_vs_structural.png\n")
    }
  }
} else {
  cat("  Skipped (no supply results)\n")
}


# =========================================================================
# 5. Counterfactual Results Table and Welfare Gradient Figure
# =========================================================================

cat("\n--- Table/Figures: Counterfactual Results ---\n")

if (!is.null(cf_results) && nrow(cf_results) > 0) {

  # --- 5a. Premium change summary table ---
  cf_summary <- cf_results %>%
    group_by(scenario, tau) %>%
    summarize(
      n_cells = length(unique(paste(region, year))),
      mean_premium_obs  = weighted.mean(premium_obs, share_obs, na.rm = TRUE),
      mean_premium_cf   = weighted.mean(premium_cf, share_cf, na.rm = TRUE),
      mean_premium_chg  = weighted.mean(premium_change, share_obs, na.rm = TRUE),
      mean_cs           = mean(cs_weighted, na.rm = TRUE),
      .groups = "drop"
    )

  fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)

  tab_lines <- c(
    "\\begin{tabular}{llrrrrr}",
    "\\hline\\hline",
    "Scenario & $\\tau$ & Cells & Avg Premium & CF Premium & $\\Delta$ Premium & CS \\\\",
    "\\hline"
  )

  for (i in seq_len(nrow(cf_summary))) {
    r <- cf_summary[i, ]
    tau_str <- if (is.na(r$tau)) "--" else fmt(r$tau)
    tab_lines <- c(tab_lines, sprintf(
      "%s & %s & %d & %s & %s & %s & %s \\\\",
      gsub("_", "\\\\_", r$scenario), tau_str, r$n_cells,
      fmt(r$mean_premium_obs), fmt(r$mean_premium_cf),
      fmt(r$mean_premium_chg), fmt(r$mean_cs)
    ))
  }

  tab_lines <- c(tab_lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(tab_lines, "results/tables/counterfactual_results.tex")
  cat("  Wrote results/tables/counterfactual_results.tex\n")

  # --- 5b. Welfare gradient figure (CS by tau) ---
  tau_results <- cf_results %>%
    filter(grepl("^zero_tau", scenario)) %>%
    group_by(tau) %>%
    summarize(
      mean_cs = mean(cs_weighted, na.rm = TRUE),
      mean_premium_chg = weighted.mean(premium_change, share_obs, na.rm = TRUE),
      .groups = "drop"
    )

  # Get observed CS for reference line
  obs_cs <- cf_results %>%
    filter(scenario == "observed") %>%
    summarize(cs = mean(cs_weighted, na.rm = TRUE)) %>%
    pull(cs)

  if (nrow(tau_results) > 1) {
    tau_results <- tau_results %>%
      mutate(cs_change = mean_cs - obs_cs)

    p_tau <- ggplot(tau_results, aes(x = tau, y = cs_change)) +
      geom_line(linewidth = 1.2, color = "#2C3E50") +
      geom_point(size = 3, color = "#2C3E50") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        x = expression(tau ~ "(broker" %->% "navigator substitution rate)"),
        y = "Change in consumer surplus ($/member/month)"
      ) +
      scale_x_continuous(breaks = tau_results$tau) +
      theme_minimal(base_size = 12)

    ggsave("results/figures/cf_welfare_gradient.png", p_tau, width = 7, height = 5)
    cat("  Wrote results/figures/cf_welfare_gradient.png\n")
  }

  # --- 5c. Premium change by scenario figure ---
  cf_by_scenario <- cf_results %>%
    filter(scenario %in% c("observed", "uniform") |
             scenario %in% c("zero_tau0.00", "zero_tau0.50", "zero_tau1.00")) %>%
    mutate(
      scenario_label = case_when(
        scenario == "observed"      ~ "Observed",
        scenario == "uniform"       ~ "Uniform commission",
        scenario == "zero_tau0.00"  ~ "Zero comm (tau=0)",
        scenario == "zero_tau0.50"  ~ "Zero comm (tau=0.5)",
        scenario == "zero_tau1.00"  ~ "Zero comm (tau=1)",
        TRUE ~ scenario
      )
    ) %>%
    group_by(scenario_label) %>%
    summarize(
      mean_chg = weighted.mean(premium_change, share_obs, na.rm = TRUE),
      .groups = "drop"
    )

  if (nrow(cf_by_scenario) > 1) {
    p_prem <- ggplot(cf_by_scenario %>% filter(scenario_label != "Observed"),
                      aes(x = reorder(scenario_label, mean_chg), y = mean_chg)) +
      geom_col(fill = "#2C3E50", width = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      coord_flip() +
      labs(x = NULL, y = "Change in premium ($/member/month, relative to observed)") +
      theme_minimal(base_size = 12)

    ggsave("results/figures/cf_premium_change.png", p_prem, width = 7, height = 4)
    cat("  Wrote results/figures/cf_premium_change.png\n")
  }
} else {
  cat("  Skipped (no counterfactual results)\n")
}


# =========================================================================
# 6. Paper numbers (inline \newcommand definitions)
# =========================================================================

cat("\n--- Paper numbers ---\n")

numbers <- c()
add_num <- function(name, val, d = 1) {
  numbers <<- c(numbers, sprintf("\\newcommand{\\%s}{%s}", name,
                                  formatC(val, format = "f", digits = d, big.mark = ",")))
}

# Sample sizes
add_num("nHHfull", nrow(hh_full), 0)
add_num("nHHclean", nrow(hh_clean), 0)
add_num("nHHins", nrow(hh_ins), 0)
add_num("pctNewEnrollee", mean(hh_full$new_enrollee, na.rm = TRUE) * 100)

# Assistance rates
if (has_col(hh_full, "channel")) {
  add_num("pctAssisted", mean(hh_full$channel != "Unassisted", na.rm = TRUE) * 100)
}

# Demand headline
beta_p <- coefs_structural$estimate[coefs_structural$term == "premium"]
beta_c <- coefs_structural$estimate[coefs_structural$term == "commission_broker"]
if (length(beta_p) == 1 && length(beta_c) == 1 && abs(beta_p) > 1e-10) {
  add_num("commPremRatio", beta_c / abs(beta_p), 2)
}

lambda_hat <- coefs_structural$estimate[coefs_structural$term == "lambda"]
if (length(lambda_hat) == 1) {
  add_num("lambdaHat", lambda_hat, 3)
}

add_num("nDemandParams", nrow(coefs_structural), 0)

# Supply headline
if (nrow(supply_results) > 0) {
  sr <- supply_results %>% filter(!is.na(mc_foc), !is.na(posted_premium))
  add_num("meanMarkup", mean(sr$markup, na.rm = TRUE))
  add_num("meanLerner", mean(sr$lerner_index, na.rm = TRUE), 3)
  add_num("nSupplyCells", length(unique(paste(sr$region, sr$year))), 0)
}

# Counterfactual headline
if (!is.null(cf_results) && nrow(cf_results) > 0) {
  zero_full <- cf_results %>% filter(scenario == "zero_tau1.00")
  if (nrow(zero_full) > 0) {
    add_num("cfZeroPremChg", weighted.mean(zero_full$premium_change,
                                            zero_full$share_obs, na.rm = TRUE))
  }
}

writeLines(numbers, "results/tables/paper-numbers.tex")
cat("  Wrote results/tables/paper-numbers.tex (", length(numbers), "commands)\n")


cat("\n=== Paper results generation complete ===\n")
