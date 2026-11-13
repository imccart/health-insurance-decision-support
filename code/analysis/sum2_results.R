# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-26
## Description:   Generate all paper tables and figures from pipeline outputs.
##                Reads estimation outputs from results/ and intermediate data
##                from data/output/. Writes tables to results/tables/ and
##                figures to results/figures/. Re-run this script (seconds)
##                whenever you want to update paper assets without re-running
##                estimation.

# Output directories ------------------------------------------------------
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)


# =========================================================================
# 0. Load pipeline outputs
# =========================================================================


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
  error = function(e) NULL
)
if (!is.null(cf_results)) {
  # premium_change is measured from the baseline equilibrium, the reference for
  # every other counterfactual column
  cf_results <- cf_results %>%
    left_join(cf_results %>% filter(scenario == "baseline") %>%
                select(region, year, plan_id, premium_base = premium_cf),
              by = c("region", "year", "plan_id"),
    relationship = "many-to-one") %>%
    mutate(premium_change = premium_cf - premium_base) %>%
    select(-premium_base)
  cf_welfare <- tryCatch(read_csv("results/counterfactual_welfare.csv", show_col_types = FALSE),
                         error = function(e) NULL)
  if (!is.null(cf_welfare))
    cf_results <- cf_results %>%
      select(-any_of(c("cs_weighted", "cs_nocomm", "cs_welfare_nav", "cs_welfare_obj",
                       "obj_prem", "obj_eoop", "obj_risk"))) %>%
      left_join(cf_welfare, by = c("region", "year", "scenario"), relationship = "many-to-one")
}


# =========================================================================
# 1. Summary Statistics Table
# =========================================================================


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
}


# =========================================================================
# 2. Commission Schedule Figures
# =========================================================================


comm <- commission_lookup %>%
  filter(year >= 2014, year <= 2019)

# A carrier with network-level rates anywhere in a panel is drawn as one
# series per network throughout it (coincident where the rates are equal);
# every other carrier is one series from its hmo = 0 rows
label_networks <- function(d) {
  d %>%
    group_by(insurer_prefix, year) %>%
    mutate(year_split = n_distinct(rate) > 1) %>%
    group_by(insurer_prefix) %>%
    mutate(ever_split = any(year_split)) %>%
    ungroup() %>%
    filter(ever_split | hmo == 0) %>%
    mutate(network = ifelse(ever_split, ifelse(hmo == 1, " (HMO)", " (PPO)"), "")) %>%
    select(-year_split, -ever_split)
}

# Flat commission insurers
flat_comm <- comm %>%
  filter(!is_pct) %>%
  label_networks() %>%
  mutate(insurer = paste0(case_when(
    insurer_prefix == "ANT" ~ "Anthem",
    insurer_prefix == "KA"  ~ "Kaiser",
    insurer_prefix == "HN"  ~ "Health Net",
    insurer_prefix == "Small" ~ "Regional insurers",
    TRUE ~ insurer_prefix
  ), network)) %>%
  filter(insurer_prefix %in% c("ANT", "KA", "HN", "Small"))

p_flat <- ggplot(flat_comm, aes(x = year, y = rate, color = insurer, shape = insurer)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(x = "Year", y = "Commission ($ PMPM)", color = NULL, shape = NULL) +
  scale_x_continuous(breaks = 2014:2019) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("results/figures/flat_comm.pdf", p_flat, width = 6, height = 4)

# Percentage commission insurers
pct_comm <- comm %>%
  filter(is_pct) %>%
  label_networks() %>%
  mutate(
    insurer = paste0(case_when(
      insurer_prefix == "BS" ~ "Blue Shield",
      insurer_prefix == "HN" ~ "Health Net",
      insurer_prefix == "Small" ~ "Sharp",
      TRUE ~ insurer_prefix
    ), network),
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


# =========================================================================
# 3. Structural Demand Estimates Table
# =========================================================================


if (nrow(coefs_structural) > 0) {
  # Supplemental-appendix table. Each cell is the estimate with its standard
  # error in parentheses. Panel A holds the plan and enrollment terms, panel B
  # the channel interactions, and panel C the household characteristics, one
  # row per characteristic with its premium, actuarial value, and enrollment
  # terms side by side.
  cs_est <- setNames(coefs_structural$estimate, coefs_structural$term)
  cs_se  <- if ("std_error" %in% names(coefs_structural))
              setNames(coefs_structural$std_error, coefs_structural$term)
            else setNames(rep(NA_real_, nrow(coefs_structural)), coefs_structural$term)
  cell <- function(tm) {
    if (!tm %in% names(cs_est)) return("")
    se <- cs_se[[tm]]
    se_str <- if (is.na(se)) "" else if (se < 0.0005) " ($<$0.001)" else
      sprintf(" (%s)", formatC(se, format = "f", digits = 3))
    paste0(formatC(cs_est[[tm]], format = "f", digits = 3), se_str)
  }

  panel_a <- c(premium = "Premium", av = "Actuarial value", hmo = "HMO",
               Anthem = "Anthem", Blue_Shield = "Blue Shield", Kaiser = "Kaiser",
               Health_Net = "Health Net", inside = "Enrolled (constant)",
               lambda = "$\\lambda$ (nesting parameter)")
  demo_labels <- c(hh_size = "Household size", perc_0to17 = "Share age 0--17",
                   perc_18to34 = "Share age 18--34", perc_35to54 = "Share age 35--54",
                   perc_male = "Share male", perc_black = "Share Black",
                   perc_hispanic = "Share Hispanic", perc_asian = "Share Asian",
                   perc_other = "Share other race", FPL_250to400 = "FPL 250--400\\%",
                   FPL_400plus = "FPL above 400\\%")

  tab_lines <- c("\\begin{tabular}{lrrrr}", "\\hline\\hline",
                 "\\multicolumn{5}{l}{\\emph{A. Plan and enrollment terms}} \\\\",
                 " & Coefficient & & & \\\\", "\\hline")
  for (tm in names(panel_a))
    tab_lines <- c(tab_lines, sprintf("%s & %s & & & \\\\", panel_a[[tm]], cell(tm)))
  tab_lines <- c(tab_lines, "\\hline",
                 "\\multicolumn{5}{l}{\\emph{B. Assistance channel}} \\\\",
                 " & $\\times$ Premium & $\\times$ AV & $\\times$ Commission & $\\times$ Commission$^2$/100 \\\\",
                 "\\hline",
                 sprintf("Navigator & %s & %s & & \\\\", cell("assisted_premium"), cell("assisted_av")),
                 sprintf("Agent & %s & %s & %s & %s \\\\", cell("broker_premium"), cell("broker_av"),
                         cell("commission_broker"), cell("commission_broker_sq")),
                 "\\hline",
                 "\\multicolumn{5}{l}{\\emph{C. Household characteristics}} \\\\",
                 " & $\\times$ Premium & $\\times$ AV & $\\times$ Enrolled & \\\\", "\\hline")
  for (dm in names(demo_labels))
    tab_lines <- c(tab_lines, sprintf("%s & %s & %s & %s & \\\\", demo_labels[[dm]],
                                      cell(paste0(dm, "_prem")), cell(paste0(dm, "_av")),
                                      cell(paste0(dm, "_insured"))))
  tab_lines <- c(tab_lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(tab_lines, "results/tables/demand_estimates.tex")
}


# =========================================================================
# 3b. Cost Estimates Table (risk score + claims GMM, with s5 sandwich SEs)
# =========================================================================


if (file.exists("results/cost_coefficients_gmm_se.csv")) {
  cost_coefs <- read_csv("results/cost_coefficients_gmm_se.csv",
                         show_col_types = FALSE)

  cost_labels <- c(
    "(Intercept)"       = "Constant",
    "share_male"        = "Share male",
    "share_family"      = "Share family households",
    "share_0to34"       = "Share age 0--34",
    "share_minority"    = "Share minority",
    "log_risk_score"    = "Log predicted risk score",
    "HMO"               = "HMO",
    "year_2015"         = "Year 2015",
    "year_2016"         = "Year 2016",
    "year_2017"         = "Year 2017",
    "year_2018"         = "Year 2018",
    "Kaiser"            = "Kaiser",
    "Anthem"            = "Anthem",
    "Blue_Shield"       = "Blue Shield",
    "Health_Net"        = "Health Net"
  )
  carrier_names <- c(ANT = "Anthem", BS = "Blue Shield", CC = "Chinese Community",
                     HN = "Health Net", KA = "Kaiser", LA = "L.A. Care", MOL = "Molina",
                     OSC = "Oscar", SH = "Sharp", VAL = "Valley", WEST = "Western")
  lab_of <- function(p, map) ifelse(p %in% names(map), map[p], gsub("_", "\\\\_", p))
  row_of <- function(lab, est, se)
    sprintf("%s & %s & %s \\\\", lab, formatC(est, format = "f", digits = 3),
            formatC(se, format = "f", digits = 3))

  # Risk score equation: the weighted OLS s4 holds fixed; the insurer-by-metal
  # effects are summarized in one row
  rs <- read_csv("results/risk_score_coefficients.csv", show_col_types = FALSE) %>%
    filter(!startsWith(term, "im_")) %>%
    rename(param = term)
  cl <- cost_coefs %>% filter(equation == "claims", !str_detect(param, "^share_ra"))
  has_region <- any(cost_coefs$equation == "claims" & str_detect(cost_coefs$param, "^share_ra"))
  sv <- cost_coefs %>% filter(equation == "commission", grepl("^wedge_", param)) %>%
    mutate(carrier = sub("^wedge_", "", param)) %>%
    arrange(lab_of(carrier, carrier_names))

  tab_lines <- c("\\begin{tabular}{lrr}", "\\hline\\hline",
                 " & Estimate & Std.\\ error \\\\", "\\hline",
                 "\\multicolumn{3}{l}{\\emph{A. Risk score equation}} \\\\")
  for (i in seq_len(nrow(rs)))
    tab_lines <- c(tab_lines, row_of(paste0("\\quad ", lab_of(rs$param[i], cost_labels)),
                                     rs$estimate[i], rs$se[i]))
  tab_lines <- c(tab_lines, "\\quad Insurer-by-metal effects & \\multicolumn{2}{c}{Yes} \\\\",
                 "\\multicolumn{3}{l}{\\emph{B. Claims equation}} \\\\")
  for (i in seq_len(nrow(cl)))
    tab_lines <- c(tab_lines, row_of(paste0("\\quad ", lab_of(cl$param[i], cost_labels)),
                                     cl$estimate[i], cl$se[i]))
  if (has_region) tab_lines <- c(tab_lines, "\\quad Rating-area shares & \\multicolumn{2}{c}{Yes} \\\\")
  if (nrow(sv) > 0) {
    tab_lines <- c(tab_lines, "\\multicolumn{3}{l}{\\emph{C. Commission condition, carrier constant $\\delta_f$}} \\\\")
    for (i in seq_len(nrow(sv)))
      tab_lines <- c(tab_lines, row_of(paste0("\\quad ", lab_of(sv$carrier[i], carrier_names)),
                                       sv$estimate[i], sv$se[i]))
  }
  tab_lines <- c(tab_lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(tab_lines, "results/tables/cost_estimates.tex")
}


# =========================================================================
# 4. Supply-Side Results Table
# =========================================================================


if (nrow(supply_results) > 0) {
  # Marginal cost at the GMM solution (s4's mc_gmm.csv), the final cost
  # estimates; the mc_structural column in supply_results is s3's
  # starting-value version and is not reported.
  mc_gmm_df <- read_csv(file.path(TEMP_DIR, "mc_gmm.csv"), show_col_types = FALSE)
  sr <- supply_results %>%
    inner_join(mc_gmm_df, by = c("region", "year", "plan_id"), relationship = "many-to-one") %>%
    mutate(markup = realized_premium - mc_gmm,
           lerner_index = markup / realized_premium) %>%
    filter(!is.na(mc_foc), !is.na(realized_premium))

  # Summary by metal tier. Report MEDIANS, not means: the tier mean markup is
  # dragged down (platinum's mean is negative) by negative-net-of-transfer-MC
  # plans (Kaiser gold/platinum) and near-zero-share plan-cells, even though the
  # typical plan in the tier carries a large positive markup.
  supply_by_metal <- sr %>%
    group_by(metal) %>%
    summarize(
      n_plan_years = n(),
      med_premium = median(realized_premium, na.rm = TRUE),
      med_markup  = median(markup, na.rm = TRUE),
      # mc_foc excludes admin; adding it back (mc_gmm - mc_gmm_net) puts both
      # MC columns on the full-cost basis the markup uses
      med_mc_foc  = median(mc_foc + (mc_gmm - mc_gmm_net), na.rm = TRUE),
      med_mc_str  = median(mc_gmm, na.rm = TRUE),
      med_lerner  = median(lerner_index, na.rm = TRUE),
      med_comm    = median(commission_pmpm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(factor(metal, levels = c("Bronze", "Silver", "Gold", "Platinum")))

  fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")

  # Premium, marginal cost, and markup read left to right (markup = premium
  # less the estimated marginal cost); the cost implied by the pricing
  # condition sits beside the commission as the comparison
  tab_lines <- c(
    "\\begin{tabular}{lrrrrrrr}",
    "\\hline\\hline",
    " & Plan- & & Marginal & & Lerner & Implied & \\\\",
    "Metal tier & cells & Premium & cost & Markup & index & marginal cost & Commission \\\\",
    "\\hline"
  )

  for (i in seq_len(nrow(supply_by_metal))) {
    r <- supply_by_metal[i, ]
    tab_lines <- c(tab_lines, sprintf(
      "%s & %s & %s & %s & %s & %s & %s & %s \\\\",
      r$metal, fmt(r$n_plan_years),
      fmt(r$med_premium), fmt(r$med_mc_str), fmt(r$med_markup),
      fmt(r$med_lerner, 2), fmt(r$med_mc_foc), fmt(r$med_comm)
    ))
  }

  tab_lines <- c(tab_lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(tab_lines, "results/tables/supply_results.tex")

  # MC validation figure: FOC inversion vs the GMM structural cost, both on the
  # claims-net-of-transfers basis (mc_foc excludes admin, so mc_gmm_net is the
  # comparable object)
  if ("mc_gmm_net" %in% names(sr)) {
    sr_valid <- sr %>% filter(!is.na(mc_gmm_net), mc_foc > 0, mc_gmm_net > 0)
    if (nrow(sr_valid) > 5) {
      p_mc <- ggplot(sr_valid, aes(x = mc_gmm_net, y = mc_foc)) +
        geom_point(alpha = 0.5, size = 1.5) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
        labs(x = "MC (Structural prediction)", y = "MC (FOC inversion)") +
        theme_minimal(base_size = 12)
      ggsave("results/figures/supply_mc_foc_vs_structural.png", p_mc, width = 6, height = 5)
    }
  }
}


# =========================================================================
# 5. Counterfactual Results Table and Welfare Gradient Figure
# =========================================================================


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
  # uninsured-cost constants (from helpers/welfare.R). Per member per year.
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
  # Objective effect of removing assistance with the uninsured valued at their
  # out-of-pocket cost alone
  cf_zero_obj_oop <- with(comp_means %>% filter(scenario == "zero_tau0.00"),
                          (ins - oop) - (obs_w$ins - obs_w$oop))
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
    select(scenario, d_cs, d_nav, d_ps, d_gov, d_gov_sub, d_gov_csr, d_gov_uc, d_gov_pen,
           d_shu, d_obj_low, d_obj, d_obj_hi)

  prem_summary <- cf_results %>%
    group_by(scenario, tau) %>%
    summarize(
      n_cells = length(unique(paste(region, year))),
      d_prem  = weighted.mean(premium_change, share_obs, na.rm = TRUE),
      .groups = "drop"
    )

  # The counterfactual tables share one layout: three panels (removing
  # commissions, reshaping the schedule, changing navigator use), each scenario
  # an indented row, the baseline omitted since every entry is a change from it
  cf_panels <- list(
    "Zero commission" = c(zero_tau0.00 = "$\\tau = 0$", zero_tau0.50 = "$\\tau = 0.5$",
                          zero_tau1.00 = "$\\tau = 1$"),
    "Commission schedule" = c(uniform_low = "Low uniform commission",
                              scale_0.50 = "Halved commissions",
                              aligned = "Aligned commissions",
                              flat_mandate = "Flat-fee mandate"),
    "Navigators" = c(defund_0.50 = "Navigator defunding (50\\%)",
                     endog_tau0.50 = "Navigator expansion (50\\%)"))
  # Rounded values print without a sign when they round to zero
  fmtz <- function(x, d = 0) sub("^-(0(\\.0+)?)$", "\\1", fmt(x, d))
  panel_lines <- function(ncol, scen_row) {
    out <- character(0)
    for (pn in names(cf_panels)) {
      rows <- character(0)
      for (sc in names(cf_panels[[pn]])) {
        v <- scen_row(sc)
        if (!is.null(v)) rows <- c(rows, sprintf("\\quad %s & %s \\\\", cf_panels[[pn]][[sc]], v))
      }
      if (length(rows)) out <- c(out, sprintf("\\multicolumn{%d}{l}{\\emph{%s}} \\\\", ncol, pn), rows)
    }
    out
  }

  cf_summary <- prem_summary %>%
    left_join(welf_summary, by = "scenario", relationship = "many-to-one")
  cf_row <- function(sc) {
    r <- cf_summary[cf_summary$scenario == sc, ]
    if (nrow(r) == 1) r else NULL
  }

  # Headline table. The coverage effect (change in the uninsured share, in points)
  # is the robust, parameter-driven result; the objective column is the central
  # uninsured-cost case, with the low/high band in the companion table below.
  # d_prem and d_nav are per member per month; x12 to the annual basis of d_cs and d_obj
  tab_lines <- c(
    "\\begin{tabular}{lrrrrr}",
    "\\hline\\hline",
    " & $\\Delta$ Premium & $\\Delta$ Uninsured & \\multicolumn{3}{c}{$\\Delta$ Welfare} \\\\",
    "\\cline{4-6}",
    " & (\\$) & (pp) & CS & $V^{nav}$ & $V^{obj}$ \\\\",
    "\\hline",
    panel_lines(6, function(sc) {
      r <- cf_row(sc); if (is.null(r)) return(NULL)
      sprintf("%s & %s & %s & %s & %s", fmtz(12 * r$d_prem), fmtz(100 * r$d_shu, 1),
              fmtz(r$d_cs), fmtz(12 * r$d_nav), fmtz(r$d_obj))
    }),
    "\\hline\\hline", "\\end{tabular}")
  writeLines(tab_lines, "results/tables/counterfactual_results.tex")

  # --- 5a1. Objective welfare band (low / central / high uninsured cost), annual $ ---
  band_lines <- c(
    "\\begin{tabular}{lrrr}",
    "\\hline\\hline",
    " & \\multicolumn{3}{c}{$\\Delta V^{obj}$ by cost of being uninsured} \\\\",
    "\\cline{2-4}",
    " & Low & Central & High \\\\",
    "\\hline",
    panel_lines(4, function(sc) {
      r <- cf_row(sc); if (is.null(r)) return(NULL)
      sprintf("%s & %s & %s", fmtz(r$d_obj_low), fmtz(r$d_obj), fmtz(r$d_obj_hi))
    }),
    "\\hline\\hline", "\\end{tabular}")
  writeLines(band_lines, "results/tables/counterfactual_welfare_band.tex")

  # --- 5a1b. Producer surplus and government cost, per member per year ---
  fisc_lines <- c(
    "\\begin{tabular}{lrrrrrr}",
    "\\hline\\hline",
    " & $\\Delta$ Producer & \\multicolumn{5}{c}{$\\Delta$ Government cost} \\\\",
    "\\cline{3-7}",
    " & surplus & Subsidies & CSR & Uncomp.\\ care & Penalties & Total \\\\",
    "\\hline",
    # penalty revenue enters with a negative sign so the cost columns sum to the total
    panel_lines(7, function(sc) {
      r <- cf_row(sc); if (is.null(r)) return(NULL)
      sprintf("%s & %s & %s & %s & %s & %s", fmtz(r$d_ps), fmtz(r$d_gov_sub), fmtz(r$d_gov_csr),
              fmtz(r$d_gov_uc), fmtz(-r$d_gov_pen), fmtz(r$d_gov))
    }),
    "\\hline\\hline", "\\end{tabular}")
  writeLines(fisc_lines, "results/tables/counterfactual_fiscal.tex")

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
    RPc <- UNINS_RISK_PROT[["central"]]; MRc <- UNINS_MORT_REDUX[["central"]]
    VSLc <- UNINS_VSL[["central"]]
    col <- function(p, s) draws[[paste0(p, "_", s)]]
    # Point estimate with the bootstrap SE in parentheses, the SE carrying one
    # more decimal than the point estimate
    sefmt <- function(x, d) if (x < 0.5 * 10^(-d)) sprintf("$<$%s", formatC(10^(-d), format = "f", digits = d)) else
      formatC(x, format = "f", digits = d)
    se_cells <- list()
    for (s in unlist(lapply(cf_panels, names))) {
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
      se_cells[[s]] <- sprintf("%s (%s) & %s (%s)", fmtz(cov_pt, 1), sefmt(se_cov, 2),
                               fmtz(obj_pt), sefmt(se_obj, 1))
    }
    wl <- c("\\begin{tabular}{lrr}", "\\hline\\hline",
            " & $\\Delta$ Uninsured (pp) & $\\Delta V^{obj}$ (\\$) \\\\", "\\hline",
            panel_lines(3, function(sc) se_cells[[sc]]),
            "\\hline\\hline", "\\end{tabular}")
    writeLines(wl, "results/tables/counterfactual_welfare_se.tex")
  }

  # --- 5b. Welfare gradient figure (CS by tau), from the cell-level welfare
  # rows (the plan-level join would weight each cell by its plan count) ---
  if (!is.null(cf_welfare) && "cs_nocomm" %in% names(cf_welfare)) {
  scen_tau <- cf_results %>% distinct(scenario, tau)
  tau_results <- cf_welfare %>%
    inner_join(scen_tau, by = "scenario", relationship = "many-to-one") %>%
    filter(str_detect(scenario, "^zero_tau")) %>%
    group_by(tau) %>%
    summarize(mean_cs = mean(cs_nocomm, na.rm = TRUE), .groups = "drop")

  # Baseline CS for the reference line
  obs_cs <- cf_welfare %>%
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
        x = expression(tau ~ "(agent" %->% "navigator substitution rate)"),
        y = "Change in consumer surplus ($/member/year)"
      ) +
      scale_x_continuous(breaks = tau_results$tau) +
      theme_minimal(base_size = 12)

    ggsave("results/figures/cf_welfare_gradient.png", p_tau, width = 7, height = 5)
  }
  }

  # --- 5c. Premium change by scenario figure ---
  cf_by_scenario <- cf_results %>%
    filter(scenario %in% c("baseline", "uniform_low") |
             scenario %in% c("zero_tau0.00", "zero_tau0.50", "zero_tau1.00")) %>%
    mutate(
      scenario_label = case_when(
        scenario == "baseline"      ~ "Baseline",
        scenario == "uniform_low"   ~ "Low uniform commission",
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
  }
}


# =========================================================================
# 6. Paper numbers (inline \newcommand definitions)
# =========================================================================


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
    hdr <- names(fread(f, nrows = 0L))
    fread(f, select = intersect(c("choice", "broker", "comm_pmpm", "comm_hh", "premium",
                        "uninsured_plan", "hh_size", "perc_0to17", "perc_18to34",
                        "perc_35to54", "perc_male", "perc_black", "perc_hispanic",
                        "perc_asian", "perc_other", "FPL_250to400", "FPL_400plus"), hdr)) %>%
      filter(broker == 1, choice == 1, uninsured_plan == 0)
  }) %>% bind_rows()
  if (!"comm_hh" %in% names(broker_hh)) broker_hh$comm_hh <- broker_hh$comm_pmpm

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
              mean_commission = weighted.mean(comm_hh, hh_size)) %>%
    # marginal utility of a commission dollar at the mean commission, and the
    # commission at which it reaches zero
    mutate(comm_mu = b[["commission_broker"]] + 2 * b[["commission_broker_sq"]] * mean_commission / 100,
           comm_turning = -100 * b[["commission_broker"]] / (2 * b[["commission_broker_sq"]]),
           dollar_equiv = comm_mu / (abs(alpha_per100) / 100),
           elast_ratio = (comm_mu * mean_commission) /
                         (abs(alpha_per100) / 100 * mean_net_premium))

  write_csv(equiv, "results/commission_equivalence.csv")
  add_num("commPremRatio", equiv$dollar_equiv, 2)
  add_num("commPremElast", equiv$elast_ratio, 2)
  add_num("commTurningPoint", equiv$comm_turning, 1)
  add_num("meanCommission", equiv$mean_commission, 2)
  add_num("meanNetPremium", equiv$mean_net_premium, 2)
}

lambda_hat <- coefs_structural$estimate[coefs_structural$term == "lambda"]
if (length(lambda_hat) == 1) {
  add_num("lambdaHat", lambda_hat, 3)
}

add_num("nDemandParams", nrow(coefs_structural), 0)
add_num("nDemandHH", sum(vapply(cell_files, function(f)
  uniqueN(fread(f, select = "household_number")$household_number), numeric(1))), 0)

# Supply headline, at the GMM marginal cost (mc_gmm.csv, as in the supply table)
if (nrow(supply_results) > 0) {
  sr <- supply_results %>%
    inner_join(read_csv(file.path(TEMP_DIR, "mc_gmm.csv"), show_col_types = FALSE),
               by = c("region", "year", "plan_id"),
    relationship = "many-to-one") %>%
    mutate(markup = realized_premium - mc_gmm,
           lerner_index = markup / realized_premium) %>%
    filter(!is.na(mc_foc), !is.na(realized_premium))
  add_num("meanMarkup", mean(sr$markup, na.rm = TRUE))
  add_num("meanLerner", mean(sr$lerner_index, na.rm = TRUE), 3)
  add_num("nSupplyCells", length(unique(paste(sr$region, sr$year))), 0)
  add_num("nPlanCells", nrow(sr), 0)
  add_num("nPlanCellsFOC", sum(sr$share >= 0.005, na.rm = TRUE), 0)
  add_num("mcFitCorr", cor(sr$mc_foc, sr$mc_gmm_net, use = "complete.obs"), 2)
  add_num("nNegMC", sum(sr$mc_gmm_net < 0, na.rm = TRUE), 0)

  # Markups and Lerner indices by insurer and by metal tier (plan-cell means)
  bind_rows(
    sr %>%
      mutate(group = if_else(issuer %in% c("Anthem", "Blue_Shield", "Kaiser", "Health_Net"),
                             issuer, "Regional")) %>%
      group_by(group) %>%
      summarize(markup = mean(markup, na.rm = TRUE), lerner = mean(lerner_index, na.rm = TRUE),
                n_plan_cells = n(), .groups = "drop") %>%
      mutate(dimension = "insurer"),
    sr %>%
      group_by(group = metal) %>%
      summarize(markup = mean(markup, na.rm = TRUE), lerner = mean(lerner_index, na.rm = TRUE),
                n_plan_cells = n(), .groups = "drop") %>%
      mutate(dimension = "metal")
  ) %>%
    select(dimension, group, markup, lerner, n_plan_cells) %>%
    write_csv("results/supply_markups_by_group.csv")
}

# Counterfactual headline
if (!is.null(cf_results) && nrow(cf_results) > 0) {
  zero_full <- cf_results %>% filter(scenario == "zero_tau1.00")
  if (nrow(zero_full) > 0) {
    add_num("cfZeroPremChg", weighted.mean(zero_full$premium_change,
                                            zero_full$share_obs, na.rm = TRUE))
  }
  if (exists("cf_zero_obj_oop")) add_num("cfZeroObjOOP", cf_zero_obj_oop, 0)
}

writeLines(numbers, "results/tables/paper-numbers.tex")

