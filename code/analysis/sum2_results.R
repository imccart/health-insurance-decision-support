# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-26
## Description:   Generate all paper tables and figures from pipeline outputs.
##                Reads estimation outputs from results/ and intermediate data
##                from data/output/. Writes tables to results/tables/ and
##                figures to results/figures/. Re-run this script (seconds)
##                whenever you want to update paper assets without re-running
##                estimation.

# Packages ----------------------------------------------------------------
pacman::p_load(
  tidyverse, data.table, fixest, kableExtra, nleqslv, mlogit
)

TEMP_DIR <- "D:/temp-research-data/health-insurance-decision-support"

# Ensure output dirs exist
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)


# =========================================================================
# 0. Load pipeline outputs
# =========================================================================

cat("Loading pipeline outputs...\n")

hh_full  <- fread("data/output/hh_full.csv") %>% as_tibble()

# Counts only — materialize these before building any subset so we never
# duplicate hh_full's rows in memory.
n_hh_full  <- nrow(hh_full)
n_hh_ins   <- sum(hh_full$insured == 1L)
n_hh_clean <- sum(hh_full$new_enrollee == 1L)

commission_lookup <- fread("data/output/commission_lookup.csv") %>% as_tibble()
plan_choice       <- fread(file.path(TEMP_DIR, "plan_choice.csv")) %>% as_tibble()

coefs_structural <- read_csv("results/choice_coefficients_structural.csv",
                              show_col_types = FALSE)
# s5 writes the sandwich SEs to a separate file; use it when present so the
# demand table carries standard errors (section 3 already prints them if the
# std_error column exists).
if (file.exists("results/choice_coefficients_structural_se.csv")) {
  coefs_structural <- read_csv("results/choice_coefficients_structural_se.csv",
                               show_col_types = FALSE) %>%
    select(term, estimate, std_error = se)
}
supply_results   <- read_csv("results/supply_results.csv", show_col_types = FALSE)

# Counterfactual results. cf1's file carries the solved premiums/commissions; the
# welfare columns come from cf2 (counterfactual_welfare.csv), the single scorer with
# the spending schedule. Replace cf1's provisional welfare with cf2's (cell-level,
# broadcast across the plan rows by region/year/scenario).
cf_results <- tryCatch(
  read_csv("results/counterfactual_results.csv", show_col_types = FALSE),
  error = function(e) { cat("  counterfactual_results.csv not found\n"); NULL }
)
if (!is.null(cf_results)) {
  cf_welfare <- tryCatch(read_csv("results/counterfactual_welfare.csv", show_col_types = FALSE),
                         error = function(e) { cat("  counterfactual_welfare.csv not found\n"); NULL })
  if (!is.null(cf_welfare))
    cf_results <- cf_results %>%
      select(-any_of(c("cs_weighted", "cs_nocomm", "cs_welfare_nav", "cs_welfare_obj",
                       "obj_prem", "obj_eoop", "obj_risk"))) %>%
      left_join(cf_welfare, by = c("region", "year", "scenario"))
}
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
      pct_insured = mean(!is.na(plan_id)) * 100,
      mean_FPL = mean(FPL, na.rm = TRUE),
      mean_hh_size = mean(household_size, na.rm = TRUE),
      pct_new = mean(new_enrollee, na.rm = TRUE) * 100,
      pct_silver = mean(metal == "Silver", na.rm = TRUE) * 100,
      pct_bronze = mean(metal == "Bronze", na.rm = TRUE) * 100,
      pct_gold = mean(metal == "Gold", na.rm = TRUE) * 100,
      pct_hmo = mean(str_detect(network_type, regex("HMO", ignore_case = TRUE)),
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

  # summary_stats.tex is owned by sum1_desc-stats.R, whose table carries the
  # dominated-choice row the paper caption references.
  cat("  summary_stats.tex left to sum1_desc-stats.R (not overwritten)\n")
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
    "inside"             = "Insured (constant)",
    "premium"            = "Premium",
    "penalty_own"        = "Penalty (outside option)",
    "premium_sq"         = "Premium$^2$",
    "silver"             = "Silver",
    "bronze"             = "Bronze",
    "av"                 = "Actuarial value (AV)",
    "hh_size_av"         = "HH size $\\times$ AV",
    "perc_0to17_av"      = "Age 0--17 $\\times$ AV",
    "perc_18to34_av"     = "Age 18--34 $\\times$ AV",
    "perc_35to54_av"     = "Age 35--54 $\\times$ AV",
    "perc_male_av"       = "Male $\\times$ AV",
    "perc_black_av"      = "Black $\\times$ AV",
    "perc_hispanic_av"   = "Hispanic $\\times$ AV",
    "perc_asian_av"      = "Asian $\\times$ AV",
    "perc_other_av"      = "Other race $\\times$ AV",
    "FPL_250to400_av"    = "FPL 250--400\\% $\\times$ AV",
    "FPL_400plus_av"     = "FPL 400+\\% $\\times$ AV",
    "assisted_av"        = "Navigator $\\times$ AV",
    "broker_av"          = "Broker $\\times$ AV",
    "perc_0to17_bronze"  = "Age 0--17 $\\times$ Bronze",
    "perc_18to34_bronze" = "Age 18--34 $\\times$ Bronze",
    "perc_35to54_bronze" = "Age 35--54 $\\times$ Bronze",
    "perc_0to17_silver"  = "Age 0--17 $\\times$ Silver",
    "perc_18to34_silver" = "Age 18--34 $\\times$ Silver",
    "perc_35to54_silver" = "Age 35--54 $\\times$ Silver",
    "perc_male_silver"   = "Male $\\times$ Silver",
    "perc_male_bronze"   = "Male $\\times$ Bronze",
    "hh_size_prem"       = "HH size $\\times$ premium",
    "any_0to17_prem"     = "Children $\\times$ premium",
    "perc_0to17_prem"    = "Age 0--17 $\\times$ premium",
    "perc_18to34_prem"   = "Age 18--34 $\\times$ premium",
    "perc_35to54_prem"   = "Age 35--54 $\\times$ premium",
    "perc_male_prem"     = "Male $\\times$ premium",
    "FPL_250to400_prem"  = "FPL 250--400\\% $\\times$ premium",
    "FPL_400plus_prem"   = "FPL 400+\\% $\\times$ premium",
    "any_black_prem"     = "Black $\\times$ premium",
    "any_hispanic_prem"  = "Hispanic $\\times$ premium",
    "perc_black_prem"    = "Black $\\times$ premium",
    "perc_hispanic_prem" = "Hispanic $\\times$ premium",
    "perc_asian_prem"    = "Asian $\\times$ premium",
    "perc_other_prem"    = "Other race $\\times$ premium",
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
    "assisted_silver"    = "Navigator $\\times$ Silver",
    "assisted_bronze"    = "Navigator $\\times$ Bronze",
    "broker_silver"      = "Broker $\\times$ Silver",
    "broker_bronze"      = "Broker $\\times$ Bronze",
    "assisted_premium"   = "Navigator $\\times$ premium",
    "broker_premium"     = "Broker $\\times$ premium",
    "commission_broker"  = "Commission $\\times$ broker",
    "lambda"             = "$\\lambda$ (nesting parameter)"
  )

  coefs_display <- coefs_structural %>%
    mutate(
      # Unmapped terms fall back to the raw name, so escape underscores rather
      # than emitting math-mode subscripts that break the LaTeX compile.
      label = ifelse(term %in% names(label_map), label_map[term],
                     gsub("_", "\\\\_", term)),
      est_str = formatC(estimate, format = "f", digits = 4),
      se_str  = if ("std_error" %in% names(.))
                  formatC(std_error, format = "f", digits = 4)
                else ""
    )

  # One row per coefficient with the standard error in its own column. The old
  # two-row-per-coefficient layout ran to 77 rows and overflowed the page inside
  # the table float ("Float too large"); a single ~38-row 3-column tabular fits.
  has_se <- any(coefs_display$se_str != "")
  tab_lines <- if (has_se) c(
    "\\begin{tabular}{lrr}",
    "\\hline\\hline",
    "Variable & Estimate & Std. Error \\\\",
    "\\hline"
  ) else c(
    "\\begin{tabular}{lr}",
    "\\hline\\hline",
    "Variable & Estimate \\\\",
    "\\hline"
  )

  for (i in seq_len(nrow(coefs_display))) {
    if (has_se) {
      tab_lines <- c(tab_lines,
        sprintf("%s & %s & %s \\\\", coefs_display$label[i],
                coefs_display$est_str[i], coefs_display$se_str[i]))
    } else {
      tab_lines <- c(tab_lines,
        sprintf("%s & %s \\\\", coefs_display$label[i], coefs_display$est_str[i]))
    }
  }

  tab_lines <- c(tab_lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(tab_lines, "results/tables/demand_estimates.tex")
  cat("  Wrote results/tables/demand_estimates.tex\n")
} else {
  cat("  Skipped (no structural coefficients)\n")
}


# =========================================================================
# 3b. Cost Estimates Table (risk score + claims GMM, with s5 sandwich SEs)
# =========================================================================

cat("\n--- Table: Cost Estimates ---\n")

if (file.exists("results/cost_coefficients_gmm_se.csv")) {
  cost_coefs <- read_csv("results/cost_coefficients_gmm_se.csv",
                         show_col_types = FALSE)

  cost_labels <- c(
    "(Intercept)"       = "Constant",
    "AV"                = "Actuarial value",
    "share_18to34"      = "Share age 18--34",
    "share_35to54"      = "Share age 35--54",
    "share_250to400"    = "Share FPL 250--400\\%",
    "share_400plus"     = "Share FPL above 400\\%",
    "share_male"        = "Share male",
    "share_family"      = "Share family households",
    "share_asian"       = "Share Asian",
    "share_black"       = "Share Black",
    "share_hispanic"    = "Share Hispanic",
    "share_other"       = "Share other race",
    "Silver"            = "Silver",
    "Gold"              = "Gold",
    "Platinum"          = "Platinum",
    "Kaiser"            = "Kaiser",
    "Anthem"            = "Anthem",
    "Blue_Shield"       = "Blue Shield",
    "Health_Net"        = "Health Net",
    "Molina"            = "Molina",
    "LA_Care"           = "L.A. Care",
    "SHARP"             = "Sharp",
    "Chinese_Community" = "Chinese Community",
    "Oscar"             = "Oscar",
    "Western"           = "Western",
    "Valley"            = "Valley",
    "log_risk_score"    = "Log predicted risk score",
    "HMO"               = "HMO",
    "trend"             = "Linear trend",
    "Kaiser"            = "Kaiser",
    "log_size"          = "Log insurer enrollment"
  )

  rs <- cost_coefs %>% filter(equation == "risk_score")
  cl <- cost_coefs %>% filter(equation == "claims", !str_detect(param, "^share_ra"))
  has_region <- any(cost_coefs$equation == "claims" & str_detect(cost_coefs$param, "^share_ra"))

  tab_lines <- c("\\begin{tabular}{lr}", "\\hline\\hline",
                 "Variable & Estimate \\\\", "\\hline",
                 "\\emph{Risk score equation} & \\\\")
  for (i in seq_len(nrow(rs))) {
    lab <- ifelse(rs$param[i] %in% names(cost_labels), cost_labels[rs$param[i]],
                  gsub("_", "\\\\_", rs$param[i]))
    tab_lines <- c(tab_lines,
      sprintf("%s & %s \\\\", lab, formatC(rs$estimate[i], format = "f", digits = 4)),
      sprintf(" & (%s) \\\\", formatC(rs$se[i], format = "f", digits = 4)))
  }
  tab_lines <- c(tab_lines, "\\hline", "\\emph{Claims equation} & \\\\")
  for (i in seq_len(nrow(cl))) {
    lab <- ifelse(cl$param[i] %in% names(cost_labels), cost_labels[cl$param[i]],
                  gsub("_", "\\\\_", cl$param[i]))
    tab_lines <- c(tab_lines,
      sprintf("%s & %s \\\\", lab, formatC(cl$estimate[i], format = "f", digits = 4)),
      sprintf(" & (%s) \\\\", formatC(cl$se[i], format = "f", digits = 4)))
  }
  if (has_region) tab_lines <- c(tab_lines, "Rating-area shares & Yes \\\\")
  sv <- cost_coefs %>% filter(equation == "commission")
  if (nrow(sv) > 0) {
    tab_lines <- c(tab_lines, "\\hline", "\\emph{Commission condition} & \\\\")
    for (i in seq_len(nrow(sv))) {
      lab <- ifelse(sv$param[i] == "beta_admin", "Administrative saving per commission dollar ($\\beta$)",
                    gsub("_", "\\\\_", sv$param[i]))
      tab_lines <- c(tab_lines,
        sprintf("%s & %s \\\\", lab, formatC(sv$estimate[i], format = "f", digits = 3)),
        sprintf(" & (%s) \\\\", formatC(sv$se[i], format = "f", digits = 3)))
    }
  }
  tab_lines <- c(tab_lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(tab_lines, "results/tables/cost_estimates.tex")
  cat("  Wrote results/tables/cost_estimates.tex\n")
} else {
  cat("  Skipped (no cost GMM standard errors)\n")
}


# =========================================================================
# 4. Supply-Side Results Table
# =========================================================================

cat("\n--- Table: Supply-Side Results ---\n")

if (nrow(supply_results) > 0) {
  sr <- supply_results %>%
    filter(!is.na(mc_foc), !is.na(posted_premium))

  # Summary by metal tier. Report MEDIANS, not means: the tier mean markup is
  # dragged down (platinum's mean is negative) by negative-net-of-transfer-MC
  # plans (Kaiser gold/platinum) and near-zero-share plan-cells, even though the
  # typical plan in the tier carries a large positive markup. The median is the
  # object s3_pricing.R prints and the paper prose reports.
  supply_by_metal <- sr %>%
    group_by(metal) %>%
    summarize(
      n_plan_years = n(),
      med_premium = median(posted_premium, na.rm = TRUE),
      med_markup  = median(markup, na.rm = TRUE),
      med_mc_foc  = median(mc_foc, na.rm = TRUE),
      med_mc_str  = median(mc_structural, na.rm = TRUE),
      med_lerner  = median(lerner_index, na.rm = TRUE),
      med_comm    = median(commission_pmpm, na.rm = TRUE),
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
      fmt(r$med_premium), fmt(r$med_markup),
      fmt(r$med_mc_foc), fmt(r$med_mc_str),
      fmt(r$med_lerner, 3), fmt(r$med_comm)
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

  fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)

  # --- 5a. Counterfactual headline table: premium and all three welfare measures ---
  # Welfare is the three measures of Section 6.3 (Small-Rosen CS, the navigator-
  # rule money metric V^nav, and the objective money metric V^obj), each a change
  # relative to the model baseline equilibrium, per member per year. Premium change is
  # share-weighted over plan-cells; welfare is averaged over the cell-level
  # welfare file (one row per region-year-scenario) so cells with more plans are
  # not over-weighted, and cf1's plan-level join is not used for the aggregates.
  cf_welf <- read_csv("results/counterfactual_welfare.csv", show_col_types = FALSE)

  # Objective welfare band: rebuild low/central/high from cf2's components with the
  # uninsured-cost constants (from welfare_objective.R). Per member per year.
  if (!exists("UNINS_RISK_PROT")) source("code/analysis/helpers/welfare_objective.R")
  obj_band <- function(cm, cs) cm$ins - cm$oop - UNINS_RISK_PROT[[cs]] * cm$shu -
    UNINS_MORT_REDUX[[cs]] * UNINS_VSL[[cs]] * cm$mort - DISTRESS_COST * cm$cat

  comp_means <- cf_welf %>%
    group_by(scenario) %>%
    summarize(cs = mean(cs_nocomm, na.rm = TRUE), nav = mean(cs_welfare_nav, na.rm = TRUE),
              ps = mean(producer_surplus, na.rm = TRUE), gov = mean(gov_total, na.rm = TRUE),
              gov_sub = mean(gov_subsidy, na.rm = TRUE), gov_csr = mean(gov_csr, na.rm = TRUE),
              gov_uc = mean(gov_uc, na.rm = TRUE), gov_pen = mean(gov_penalty, na.rm = TRUE),
              ins = mean(obj_insured, na.rm = TRUE), shu = mean(share_unins, na.rm = TRUE),
              oop = mean(unins_oop, na.rm = TRUE), mort = mean(unins_mort, na.rm = TRUE),
              cat = mean(unins_cat, na.rm = TRUE), .groups = "drop")
  obs_w  <- comp_means %>% filter(scenario == "baseline")
  obs_ob <- sapply(c("low", "central", "high"), function(cs) obj_band(obs_w, cs))

  welf_summary <- comp_means %>%
    mutate(
      d_cs   = cs  - obs_w$cs,
      d_nav  = nav - obs_w$nav,
      d_ps   = ps  - obs_w$ps,
      d_gov  = gov - obs_w$gov,
      d_gov_sub = gov_sub - obs_w$gov_sub, d_gov_csr = gov_csr - obs_w$gov_csr,
      d_gov_uc = gov_uc - obs_w$gov_uc, d_gov_pen = gov_pen - obs_w$gov_pen,
      d_shu  = shu - obs_w$shu,                                 # coverage effect (share pt)
      d_obj_low = obj_band(., "low")     - obs_ob[["low"]],
      d_obj    = obj_band(., "central")  - obs_ob[["central"]],  # central objective
      d_obj_hi = obj_band(., "high")     - obs_ob[["high"]]
    ) %>%
    select(scenario, d_cs, d_nav, d_ps, d_gov, d_shu, d_obj_low, d_obj, d_obj_hi)

  prem_summary <- cf_results %>%
    group_by(scenario, tau) %>%
    summarize(
      n_cells = length(unique(paste(region, year))),
      d_prem  = weighted.mean(premium_change, share_obs, na.rm = TRUE),
      .groups = "drop"
    )

  # Readable labels + display order. Every scenario the pipeline solves appears
  # here; add a row when a new scenario family is introduced in cf1_estimate.R.
  scen_levels <- c("baseline",
                   "zero_tau0.00", "zero_tau0.25", "zero_tau0.50", "zero_tau0.75", "zero_tau1.00",
                   "uniform", "aligned",
                   "scale_0.25", "scale_0.50", "scale_0.75",
                   "endog_tau0.50", "endog_tau1.00",
                   "flat_mandate", "defund_0.50", "defund_1.00")
  scen_labels <- c(baseline = "Baseline",
                   zero_tau0.00 = "Zero commission", zero_tau0.25 = "Zero commission",
                   zero_tau0.50 = "Zero commission", zero_tau0.75 = "Zero commission",
                   zero_tau1.00 = "Zero commission",
                   uniform = "Uniform commission", aligned = "Aligned commissions",
                   scale_0.25 = "Scaled commission (25\\%)", scale_0.50 = "Scaled commission (50\\%)",
                   scale_0.75 = "Scaled commission (75\\%)",
                   endog_tau0.50 = "Navigator expansion", endog_tau1.00 = "Navigator expansion",
                   flat_mandate = "Flat-fee mandate",
                   defund_0.50 = "Navigator defunding (50\\%)", defund_1.00 = "Navigator defunding (100\\%)")

  missing_scen <- setdiff(unique(as.character(prem_summary$scenario)), scen_levels)
  if (length(missing_scen) > 0)
    cat("  WARNING: scenarios missing from label map (dropped from table):",
        paste(missing_scen, collapse = ", "), "\n")

  cf_summary <- prem_summary %>%
    left_join(welf_summary, by = "scenario") %>%
    mutate(scenario = factor(scenario, levels = scen_levels)) %>%
    filter(!is.na(scenario)) %>%
    arrange(scenario) %>%
    mutate(label = scen_labels[as.character(scenario)])

  # Headline table. The coverage effect (change in the uninsured share, in points)
  # is the robust, parameter-driven result; the objective column is the central
  # uninsured-cost case, with the low/high band in the companion table below.
  tab_lines <- c(
    "\\begin{tabular}{llrrrrr}",
    "\\hline\\hline",
    "Scenario & $\\tau$ & $\\Delta$ Premium & $\\Delta$ Uninsured (pp) & $\\Delta$ CS & $\\Delta V^{nav}$ & $\\Delta V^{obj}$ \\\\",
    "\\hline"
  )
  for (i in seq_len(nrow(cf_summary))) {
    r <- cf_summary[i, ]
    tau_str <- if (is.na(r$tau)) "--" else fmt(r$tau)
    # d_nav is per member per month; x12 to the annual basis of d_cs and d_obj
    tab_lines <- c(tab_lines, sprintf(
      "%s & %s & %s & %s & %s & %s & %s \\\\",
      r$label, tau_str,
      fmt(12 * r$d_prem), fmt(100 * r$d_shu, 1), fmt(r$d_cs), fmt(12 * r$d_nav, 0), fmt(r$d_obj, 0)
    ))
  }
  tab_lines <- c(tab_lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(tab_lines, "results/tables/counterfactual_results.tex")
  cat("  Wrote results/tables/counterfactual_results.tex\n")

  # --- 5a1. Objective welfare band (low / central / high uninsured cost), annual $ ---
  # Multi-scenario families print under one label with the tau or scale value as an
  # indented sub-row.
  fam_label <- c(
    zero_tau0.00 = "Zero commission", zero_tau0.25 = "Zero commission",
    zero_tau0.50 = "Zero commission", zero_tau0.75 = "Zero commission",
    zero_tau1.00 = "Zero commission",
    scale_0.25 = "Scaled commission", scale_0.50 = "Scaled commission",
    scale_0.75 = "Scaled commission",
    endog_tau0.50 = "Navigator expansion", endog_tau1.00 = "Navigator expansion",
    defund_0.50 = "Navigator defunding", defund_1.00 = "Navigator defunding")
  sub_label <- c(
    zero_tau0.00 = "$\\tau=0.00$", zero_tau0.25 = "$\\tau=0.25$",
    zero_tau0.50 = "$\\tau=0.50$", zero_tau0.75 = "$\\tau=0.75$",
    zero_tau1.00 = "$\\tau=1.00$",
    scale_0.25 = "25\\%", scale_0.50 = "50\\%", scale_0.75 = "75\\%",
    endog_tau0.50 = "$\\tau=0.50$", endog_tau1.00 = "$\\tau=1.00$",
    defund_0.50 = "50\\%", defund_1.00 = "100\\%")
  band_lines <- c(
    "\\begin{tabular}{lrrr}",
    "\\hline\\hline",
    "Scenario & $\\Delta V^{obj}$ low & central & high \\\\",
    "\\hline"
  )
  prev_fam <- ""
  for (i in seq_len(nrow(cf_summary))) {
    r <- cf_summary[i, ]
    sc <- as.character(r$scenario)
    vals <- sprintf("%s & %s & %s", fmt(r$d_obj_low, 0), fmt(r$d_obj, 0), fmt(r$d_obj_hi, 0))
    if (sc %in% names(fam_label)) {
      if (fam_label[[sc]] != prev_fam) {
        band_lines <- c(band_lines, sprintf("%s & & & \\\\", fam_label[[sc]]))
        prev_fam <- fam_label[[sc]]
      }
      band_lines <- c(band_lines, sprintf("\\quad %s & %s \\\\", sub_label[[sc]], vals))
    } else {
      prev_fam <- ""
      band_lines <- c(band_lines, sprintf("%s & %s \\\\", r$label, vals))
    }
  }
  band_lines <- c(band_lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(band_lines, "results/tables/counterfactual_welfare_band.tex")
  cat("  Wrote results/tables/counterfactual_welfare_band.tex\n")

  # --- 5a1b. Producer surplus and government cost, per member per year ---
  fisc_lines <- c(
    "\\begin{tabular}{llrrrrrr}",
    "\\hline\\hline",
    "Scenario & $\\tau$ & $\\Delta$ Producer surplus & $\\Delta$ Subsidies & $\\Delta$ CSR & $\\Delta$ Uncomp.\\ care & $\\Delta$ Penalties & $\\Delta$ Gov.\\ cost \\\\",
    "\\hline"
  )
  for (i in seq_len(nrow(cf_summary))) {
    r <- cf_summary[i, ]
    tau_str <- if (is.na(r$tau)) "--" else fmt(r$tau)
    fisc_lines <- c(fisc_lines, sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\",
      r$label, tau_str, fmt(r$d_ps, 0), fmt(r$d_gov_sub, 0), fmt(r$d_gov_csr, 0),
      fmt(r$d_gov_uc, 0), fmt(r$d_gov_pen, 0), fmt(r$d_gov, 0)))
  }
  fisc_lines <- c(fisc_lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(fisc_lines, "results/tables/counterfactual_fiscal.tex")
  cat("  Wrote results/tables/counterfactual_fiscal.tex\n")

  # --- 5a2. Welfare effects with SEs (cf3 demand bootstrap + cf4 commission delta method) ---
  # Reconstruct each draw's coverage effect and central objective from the component
  # columns in cf_bootstrap_draws.csv, pair with the cf2 point estimate. The
  # cost-parameter channel adds a' V_comm a for the same linear combination of
  # headline statistics, with V_comm from cf4 (cf_delta_vcov.csv); the two stages
  # are independent, so the variances add. Skips until cf3 has run.
  draws <- tryCatch(read_csv("results/cf_bootstrap_draws.csv", show_col_types = FALSE),
                    error = function(e) NULL)
  V_comm <- tryCatch({
    d <- read.csv("results/cf_delta_vcov.csv", check.names = FALSE, stringsAsFactors = FALSE)
    M <- as.matrix(d[, -1]); dimnames(M) <- list(d[[1]], d[[1]]); M
  }, error = function(e) NULL)
  comm_var <- function(a) {
    if (is.null(V_comm) || !all(names(a) %in% rownames(V_comm))) return(0)
    as.numeric(t(a) %*% V_comm[names(a), names(a)] %*% a)
  }
  if (!is.null(draws) && nrow(draws) > 1) {
    if (is.null(V_comm)) cat("  cf_delta_vcov.csv not found -- welfare SEs carry the demand channel only\n")
    RPc <- UNINS_RISK_PROT[["central"]]; MRc <- UNINS_MORT_REDUX[["central"]]
    VSLc <- UNINS_VSL[["central"]]
    se_scen <- c(zero_tau0.00  = "Remove assistance ($\\tau$=0)",
                 zero_tau1.00  = "Brokers to navigators ($\\tau$=1)",
                 uniform       = "Uniform commission",
                 aligned       = "Aligned commissions",
                 endog_tau1.00 = "Navigator expansion",
                 flat_mandate  = "Flat-fee mandate",
                 defund_1.00   = "Navigator defunding")
    col <- function(p, s) draws[[paste0(p, "_", s)]]
    # Point estimate with bootstrap SE in parentheses (2 significant figures).
    sefmt <- function(x) format(signif(x, 2), scientific = FALSE, trim = TRUE)
    wl <- c("\\begin{tabular}{lrr}", "\\hline\\hline",
            "Scenario & $\\Delta$ Uninsured (pp) & $\\Delta V^{obj}$ \\\\", "\\hline")
    for (s in names(se_scen)) {
      if (!all(paste0(c("dshare","dobjins","doop","dmort","dcat"), "_", s) %in% names(draws))) next
      cov_d <- 100 * col("dshare", s)
      obj_d <- col("dobjins", s) - col("doop", s) - RPc * col("dshare", s) -
               MRc * VSLc * col("dmort", s) - DISTRESS_COST * col("dcat", s)
      pr <- welf_summary[welf_summary$scenario == s, ]
      cov_pt <- if (nrow(pr)) 100 * pr$d_shu else mean(cov_d, na.rm = TRUE)
      obj_pt <- if (nrow(pr)) pr$d_obj else mean(obj_d, na.rm = TRUE)
      # Same linear combinations over the cf4 headline statistics
      a_cov <- setNames(100, paste0("dshare_", s))
      a_obj <- setNames(c(1, -1, -RPc, -MRc * VSLc, -DISTRESS_COST),
                        paste0(c("dobjins", "doop", "dshare", "dmort", "dcat"), "_", s))
      se_cov <- sqrt(var(cov_d, na.rm = TRUE) + comm_var(a_cov))
      se_obj <- sqrt(var(obj_d, na.rm = TRUE) + comm_var(a_obj))
      wl <- c(wl, sprintf("%s & %s (%s) & %s (%s) \\\\",
        se_scen[[s]], fmt(cov_pt, 1), sefmt(se_cov), fmt(obj_pt, 0), sefmt(se_obj)))
    }
    wl <- c(wl, "\\hline\\hline", "\\end{tabular}")
    writeLines(wl, "results/tables/counterfactual_welfare_se.tex")
    cat("  Wrote results/tables/counterfactual_welfare_se.tex (point est with SE: demand bootstrap + commission delta method)\n")
  } else {
    cat("  cf_bootstrap_draws.csv not found -- run cf3 to populate welfare SEs\n")
  }

  # --- 5b. Welfare gradient figure (CS by tau) ---
  tau_results <- cf_results %>%
    filter(str_detect(scenario, "^zero_tau")) %>%
    group_by(tau) %>%
    summarize(
      mean_cs = mean(cs_nocomm, na.rm = TRUE),
      mean_premium_chg = weighted.mean(premium_change, share_obs, na.rm = TRUE),
      .groups = "drop"
    )

  # Baseline CS for the reference line
  obs_cs <- cf_results %>%
    filter(scenario == "baseline") %>%
    summarize(cs = mean(cs_nocomm, na.rm = TRUE)) %>%
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
        y = "Change in consumer surplus ($/member/year)"
      ) +
      scale_x_continuous(breaks = tau_results$tau) +
      theme_minimal(base_size = 12)

    ggsave("results/figures/cf_welfare_gradient.png", p_tau, width = 7, height = 5)
    cat("  Wrote results/figures/cf_welfare_gradient.png\n")
  }

  # --- 5c. Premium change by scenario figure ---
  cf_by_scenario <- cf_results %>%
    filter(scenario %in% c("baseline", "uniform") |
             scenario %in% c("zero_tau0.00", "zero_tau0.50", "zero_tau1.00")) %>%
    mutate(
      scenario_label = case_when(
        scenario == "baseline"      ~ "Baseline",
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
    p_prem <- ggplot(cf_by_scenario %>% filter(scenario_label != "Baseline"),
                      aes(x = reorder(scenario_label, mean_chg), y = mean_chg)) +
      geom_col(fill = "#2C3E50", width = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      coord_flip() +
      labs(x = NULL, y = "Change in premium ($/member/month, relative to baseline)") +
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
add_num("nHHfull", n_hh_full, 0)
add_num("nHHclean", n_hh_clean, 0)

# nHHins and the enrollee rates are reported over the prepped analysis sample
# (catastrophic households dropped), so they match Table 1 and the estimation
# sample rather than the pre-drop panel.
hh_prep <- fread(file.path(TEMP_DIR, "hh_full_prepped.csv"),
                 select = c("insured", "new_enrollee", "channel", "any_agent", "navigator")) %>% as_tibble()
ins <- hh_prep$insured == 1L
add_num("nHHins", sum(ins), 0)
add_num("pctNewEnrollee", mean(hh_prep$new_enrollee[ins], na.rm = TRUE) * 100)
add_num("pctAssisted", mean(hh_prep$channel[ins] != "Unassisted", na.rm = TRUE) * 100)
add_num("pctBroker", mean(hh_prep$any_agent[ins] == 1L, na.rm = TRUE) * 100)
add_num("pctNavigator", mean(hh_prep$navigator[ins] == 1L, na.rm = TRUE) * 100)
rm(hh_prep); gc(verbose = FALSE)

# Demand headline: commission vs premium for broker-assisted households.
# The per-dollar equivalence divides the commission coefficient by the mean
# price coefficient among broker-assisted households (base + demographic +
# broker premium interactions), and the elasticity ratio scales each by its
# mean level (commission and net premium of the chosen plan). Components go
# to results/commission_equivalence.csv so the paper numbers trace to a file.
b <- setNames(coefs_structural$estimate, coefs_structural$term)
cell_files <- list.files(file.path(TEMP_DIR, "choice_cells"),
                         pattern = "_data\\.csv$", full.names = TRUE)
if (length(cell_files) > 0) {
  broker_hh <- lapply(cell_files, function(f) {
    fread(f, select = c("choice", "broker", "comm_pmpm", "premium",
                        "uninsured_plan", "hh_size", "perc_0to17", "perc_18to34",
                        "perc_35to54", "perc_male", "perc_black", "perc_hispanic",
                        "perc_asian", "perc_other", "FPL_250to400", "FPL_400plus")) %>%
      filter(broker == 1, choice == 1, uninsured_plan == 0)
  }) %>% bind_rows()

  equiv <- broker_hh %>%
    mutate(alpha100 = b[["premium"]] + b[["broker_premium"]] +
             b[["hh_size_prem"]] * hh_size +
             b[["perc_0to17_prem"]] * perc_0to17 +
             b[["perc_18to34_prem"]] * perc_18to34 +
             b[["perc_35to54_prem"]] * perc_35to54 +
             b[["perc_male_prem"]] * perc_male +
             b[["perc_black_prem"]] * perc_black +
             b[["perc_hispanic_prem"]] * perc_hispanic +
             b[["perc_asian_prem"]] * perc_asian +
             b[["perc_other_prem"]] * perc_other +
             b[["FPL_250to400_prem"]] * FPL_250to400 +
             b[["FPL_400plus_prem"]] * FPL_400plus) %>%
    summarize(n_broker_hh = n(),
              alpha_per100 = weighted.mean(alpha100, hh_size),
              mean_net_premium = weighted.mean(100 * premium, hh_size),
              mean_commission = weighted.mean(comm_pmpm, hh_size)) %>%
    mutate(dollar_equiv = b[["commission_broker"]] / (abs(alpha_per100) / 100),
           elast_ratio = (b[["commission_broker"]] * mean_commission) /
                         (abs(alpha_per100) / 100 * mean_net_premium))

  write_csv(equiv, "results/commission_equivalence.csv")
  add_num("commPremRatio", equiv$dollar_equiv, 2)
  add_num("commPremElast", equiv$elast_ratio, 2)
  cat("  Commission equivalence: $", formatC(equiv$dollar_equiv, format = "f", digits = 2),
      " per $1 commission; elasticity ratio ",
      formatC(equiv$elast_ratio, format = "f", digits = 2), "\n", sep = "")
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
