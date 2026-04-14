# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-09
## Date Edited:   2026-04-12
## Description:   Demand sensitivity analysis — systematic build-up of
##                specification components to assess what moves beta_premium.
##                Fully standalone: run after the preliminaries portion of
##                _analysis.R (env vars set, packages/helpers loaded).
##
##                Evan's JHE 2019 reports beta = -0.429 per $100/month.
##                Our estimates are per $1/month; multiply by 100 to compare.

# Pipeline parameters
SAMPLE_FRAC   <- as.numeric(Sys.getenv("SAMPLE_FRAC"))
MASTER_SEED   <- as.integer(Sys.getenv("MASTER_SEED"))
TEMP_DIR      <- Sys.getenv("TEMP_DIR")
SENS_DIR      <- file.path(TEMP_DIR, "demand_sensitivity")

if (!dir.exists(SENS_DIR)) dir.create(SENS_DIR, recursive = TRUE)

cat("=== Demand Sensitivity Analysis ===\n")
cat("  Start time:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")

# =========================================================================
# DATA PREP — only when intermediate files are missing
# =========================================================================

prep_files <- c(
  file.path(TEMP_DIR, "plan_choice.csv"),
  file.path(TEMP_DIR, "plan_demographics.csv"),
  file.path(TEMP_DIR, "hh_choice.csv")
)
prep_done <- all(file.exists(prep_files))

if (prep_done) {
  cat("Data prep files found — skipping.\n\n")
} else {
  cat("Building intermediate files...\n\n")

  hh_full    <- read_csv("data/output/hh_full.csv", show_col_types = FALSE)
  ipweights  <- read_csv("data/output/ipweights.csv", show_col_types = FALSE)
  plan_data  <- read_csv("data/input/Covered California/plan_data.csv",
                          show_col_types = FALSE, name_repair = "minimal")
  broker_density <- read_csv("data/output/broker_density.csv", show_col_types = FALSE)
  commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

  hh_full <- hh_full %>%
    left_join(ipweights, by = "household_year")
  rm(ipweights)
  cat("  hh_full:", nrow(hh_full), "rows\n")

  # Control function residual (broker density IV)
  hh_full <- hh_full %>%
    left_join(broker_density %>% select(region, year, n_agents),
              by = c("region", "year"))
  fs_model <- lm(assisted ~ n_agents + FPL + perc_0to17 + perc_18to25 +
                    perc_65plus + perc_black + perc_hispanic + perc_asian +
                    perc_male + household_size + factor(year),
                  data = hh_full)
  hh_full$v_hat <- residuals(fs_model)
  cat("  First-stage F:", round(summary(fs_model)$fstatistic[1], 1), "\n")
  rm(fs_model, broker_density)

  # Navigator propensity
  assisted_hh <- hh_full %>% filter(assisted == 1)
  nav_model <- glm(
    (channel_detail == "Navigator") ~ FPL + perc_hispanic + perc_black +
      perc_0to17 + perc_65plus + household_size + perc_male + factor(year),
    data = assisted_hh, family = binomial
  )
  hh_full$p_nav <- predict(nav_model, newdata = hh_full, type = "response")
  rm(assisted_hh, nav_model)

  # Exclude catastrophic
  hh_full <- hh_full %>% filter(!grepl("_CAT$", plan_name) | is.na(plan_name))

  # Plan demographics
  plan_demographics <- hh_full %>%
    filter(!is.na(plan_name), plan_name != "Uninsured") %>%
    mutate(plan_name = gsub("SIL(94|73|87)", "SIL", plan_name),
           wt = ifelse(is.na(ipweight), 1, ipweight)) %>%
    group_by(plan_name, year) %>%
    summarize(share_18to34 = weighted.mean(perc_18to34, wt, na.rm = TRUE),
              share_35to54 = weighted.mean(perc_35to54, wt, na.rm = TRUE),
              share_hispanic = weighted.mean(perc_hispanic, wt, na.rm = TRUE),
              n_hh = n(), .groups = "drop")
  plan_demographics_yr <- plan_demographics %>%
    group_by(plan_name, year) %>%
    summarize(share_18to34 = weighted.mean(share_18to34, n_hh, na.rm = TRUE),
              share_35to54 = weighted.mean(share_35to54, n_hh, na.rm = TRUE),
              share_hispanic = weighted.mean(share_hispanic, n_hh, na.rm = TRUE),
              .groups = "drop")
  write_csv(plan_demographics_yr, file.path(TEMP_DIR, "plan_demographics.csv"))
  rm(plan_demographics, plan_demographics_yr)

  # Build plan_choice
  plan_choice <- plan_data %>%
    select(region, year = ENROLLMENT_YEAR, Issuer_Name, metal_level,
           plan_name = Plan_Name2, network_type = PLAN_NETWORK_TYPE,
           premium = Premium, msp = MSP, hsa = `HSA`) %>%
    mutate(region = as.integer(region), year = as.integer(year),
           issuer = standardize_insurer(Issuer_Name),
           metal = case_when(
             metal_level %in% c("Silver", "Silver - Enhanced 73",
                                 "Silver - Enhanced 87", "Silver - Enhanced 94") ~ "Silver",
             TRUE ~ metal_level)) %>%
    select(-Issuer_Name) %>%
    filter(metal != "Minimum Coverage")

  plan_choice <- plan_choice %>%
    group_by(issuer, metal, year) %>%
    mutate(n_other = n() - 1L,
           hausman_iv = (sum(premium) - premium) / pmax(n_other, 1L)) %>%
    ungroup() %>%
    mutate(hausman_iv = ifelse(n_other == 0, NA_real_, hausman_iv)) %>%
    group_by(issuer, metal, year) %>%
    mutate(hausman_iv = ifelse(is.na(hausman_iv), mean(premium, na.rm = TRUE), hausman_iv)) %>%
    ungroup() %>%
    select(-n_other)

  first_stage <- lm(premium ~ hausman_iv + metal + network_type + factor(year),
                    data = plan_choice)
  plan_choice$cf_resid <- residuals(first_stage)
  rm(first_stage)

  plan_choice <- plan_choice %>%
    mutate(insurer_prefix = sub("_.*", "", plan_name)) %>%
    left_join(commission_lookup, by = c("insurer_prefix", "year")) %>%
    mutate(
      comm_pmpm = case_when(
        is.na(rate) ~ 0,
        is_pct ~ rate * premium,
        TRUE ~ rate
      )
    ) %>%
    select(-insurer_prefix, -rate, -is_pct)
  write_csv(plan_choice, file.path(TEMP_DIR, "plan_choice.csv"))
  cat("  plan_choice:", nrow(plan_choice), "rows\n")

  # Write HH data
  hh_choice <- hh_full %>%
    filter(!grepl("_CAT$", plan_name) | is.na(plan_name)) %>%
    mutate(region = as.integer(region), year = as.integer(year),
           cutoff = AFFORD_THRESHOLDS[as.character(year)]) %>%
    select(region, year, household_id, FPL, subsidized_members, rating_factor,
           plan_number_nocsr, plan_name, previous_plan_number,
           oldest_member, cheapest_premium, subsidy, penalty,
           poverty_threshold, cutoff, household_size, ipweight, v_hat,
           perc_0to17, perc_18to34, perc_35to54,
           perc_black, perc_hispanic, perc_asian, perc_other, perc_male,
           channel, channel_detail, any_agent, p_nav)

  hh_choice_path <- file.path(TEMP_DIR, "hh_choice.csv")
  write.csv(hh_choice, hh_choice_path, row.names = FALSE)
  n_cells <- length(unique(paste0(hh_choice$region, "_", hh_choice$year)))
  cat("  Written:", n_cells, "cells ->", hh_choice_path, "\n")
  rm(hh_choice, hh_full, plan_data, plan_choice, commission_lookup)
  gc(full = TRUE, verbose = FALSE)
  cat("  Data prep complete.\n\n")
}

# =========================================================================
# SELECT CELLS
# =========================================================================

TARGET_REGIONS <- c(1L, 4L, 8L, 13L, 16L)
TARGET_YEARS   <- c(2014L, 2016L, 2018L)
TARGET_CELLS   <- expand.grid(region = TARGET_REGIONS, year = TARGET_YEARS) %>%
  as_tibble()

cat("  Target cells:", nrow(TARGET_CELLS), "\n")

# =========================================================================
# DEFINE SPECIFICATIONS — systematic build-up
# =========================================================================

# Building blocks
PLAN_ATTRS <- c("silver", "bronze", "hmo", "hsa")
INSURER_FE <- c("Anthem", "Blue_Shield", "Kaiser", "Health_Net")
ASST_TERMS <- c("assisted_silver", "assisted_bronze",
                 "commission_broker", "v_hat_commission")

DEMO_PREM <- c(
  "hh_size_prem", "perc_0to17_prem", "perc_18to34_prem", "perc_35to54_prem",
  "perc_male_prem", "perc_black_prem", "perc_hispanic_prem", "perc_asian_prem",
  "perc_other_prem", "FPL_250to400_prem", "FPL_400plus_prem"
)

DEMO_INSURED <- c(
  "FPL_250to400_insured", "FPL_400plus_insured",
  "perc_male_insured", "perc_0to17_insured",
  "perc_18to34_insured", "perc_35to54_insured",
  "perc_black_insured", "perc_hispanic_insured",
  "perc_asian_insured", "perc_other_insured"
)

# Outside option types:
#   "exchange" — uninsured are exchange non-enrollees (SIPP-filtered)
#   "acs"      — uninsured are ACS-eligible uninsured (Evan's approach)

specs <- list(
  # ---------------------------------------------------------------
  # Group A: Net premium, exchange uninsured (current approach)
  # ---------------------------------------------------------------
  list(name = "A1_net_exch",
       premium_type = "net", outside_option = "exchange",
       base = c("premium", PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS),
  list(name = "A2_net_exch_dprem",
       premium_type = "net", outside_option = "exchange",
       base = c("premium", PLAN_ATTRS, INSURER_FE, DEMO_PREM),
       asst = ASST_TERMS),

  # ---------------------------------------------------------------
  # Group B: OOP premium + penalty on outside, exchange uninsured
  # ---------------------------------------------------------------
  list(name = "B1_oop_exch",
       premium_type = "oop", outside_option = "exchange",
       base = c("premium", "penalty_own", PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS),
  list(name = "B2_oop_exch_dprem",
       premium_type = "oop", outside_option = "exchange",
       base = c("premium", "penalty_own", PLAN_ATTRS, INSURER_FE, DEMO_PREM),
       asst = ASST_TERMS),

  # ---------------------------------------------------------------
  # Group C: Net premium, ACS uninsured (Evan's outside option)
  # ---------------------------------------------------------------
  list(name = "C1_net_acs",
       premium_type = "net", outside_option = "acs",
       base = c("premium", PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS),
  list(name = "C2_net_acs_dprem",
       premium_type = "net", outside_option = "acs",
       base = c("premium", PLAN_ATTRS, INSURER_FE, DEMO_PREM),
       asst = ASST_TERMS),

  # ---------------------------------------------------------------
  # Group D: OOP premium + penalty on outside, ACS uninsured
  # (closest to Evan RAND 2021)
  # ---------------------------------------------------------------
  list(name = "D1_oop_acs",
       premium_type = "oop", outside_option = "acs",
       base = c("premium", "penalty_own", PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS),
  list(name = "D2_oop_acs_dprem",
       premium_type = "oop", outside_option = "acs",
       base = c("premium", "penalty_own", PLAN_ATTRS, INSURER_FE, DEMO_PREM),
       asst = ASST_TERMS)
)

cat("  Specifications:", length(specs), "\n\n")

# =========================================================================
# BUILD CELL DATA (once per premium_type x outside_option combination)
# =========================================================================

plan_choice <- read_csv(file.path(TEMP_DIR, "plan_choice.csv"), show_col_types = FALSE)

# Exchange HH (current outside option)
hh_exch <- as.data.table(read.csv(file.path(TEMP_DIR, "hh_choice.csv")))

# ACS uninsured HH (Evan's outside option)
acs_hh <- as.data.table(read.csv("data/output/demand_acs_households.csv"))
acs_hh[, `:=`(
  region = as.integer(rating_area),
  plan_number_nocsr = NA_integer_,
  plan_name = NA_character_,
  previous_plan_number = NA_integer_,
  oldest_member = NA_real_,
  subsidy = fifelse(is.na(subsidy), 0, subsidy),
  ipweight = weight,
  v_hat = 0,
  channel = "Unassisted",
  channel_detail = "Unassisted",
  any_agent = 0L,
  p_nav = 0,
  assisted = 0L,
  navigator = 0L,
  broker = 0L,
  agent = 0L,
  perc_18to34 = perc_18to25 + perc_26to34,
  perc_35to54 = perc_35to44 + perc_45to54
)]

# Compute penalty for ACS HH (same formula as data build)
penalty_flat <- c("2014" = 95, "2015" = 325, "2016" = 695, "2017" = 695, "2018" = 695, "2019" = 0)
penalty_pct  <- c("2014" = 0.01, "2015" = 0.02, "2016" = 0.025, "2017" = 0.025, "2018" = 0.025, "2019" = 0)
penalty_cap  <- c("2014" = 204, "2015" = 207, "2016" = 223, "2017" = 272, "2018" = 283, "2019" = 0) * 12
acs_hh[, penalty := pmin(
  pmax(household_size * penalty_flat[as.character(year)],
       penalty_pct[as.character(year)] * pmax(FPL * poverty_threshold - 10150, 0)),
  household_size * penalty_cap[as.character(year)]
)]

# Select common columns
keep_cols <- intersect(names(hh_exch), names(acs_hh))
acs_slim <- acs_hh[, ..keep_cols]

# Build exchange+ACS combined dataset (insured from exchange, uninsured from ACS)
hh_insured <- hh_exch[!is.na(plan_number_nocsr)]
hh_acs_combined <- rbind(hh_insured, acs_slim, fill = TRUE)
n_acs <- nrow(acs_slim)
rm(acs_hh, acs_slim, hh_insured)

cat("  Exchange HH:", nrow(hh_exch), "\n")
cat("  ACS uninsured:", n_acs, "\n")
cat("  Combined (insured + ACS uninsured):", nrow(hh_acs_combined), "\n")

# Split both datasets by region x year
hh_split_exch <- split(hh_exch, by = c("region", "year"), keep.by = FALSE)
hh_split_acs  <- split(hh_acs_combined, by = c("region", "year"), keep.by = FALSE)
all_cells_meta <- unique(hh_exch[, .(region, year)])[order(region, year)]
rm(hh_exch, hh_acs_combined); gc(verbose = FALSE)

set.seed(MASTER_SEED)
all_seeds <- sample.int(1e7, nrow(all_cells_meta))

all_covars_union <- unique(unlist(lapply(specs, function(s) c(s$base, s$asst))))

# Unique (premium_type, outside_option) combos
spec_combos <- unique(data.frame(
  premium_type = sapply(specs, function(s) s$premium_type),
  outside_option = sapply(specs, function(s) s$outside_option),
  stringsAsFactors = FALSE
))

# =========================================================================
# BUILD CELLS + ESTIMATE (one spec at a time)
# =========================================================================

# Track which cell dirs are already built
built_combos <- character(0)

for (si in seq_along(specs)) {
  sp <- specs[[si]]
  all_terms <- c(sp$base, sp$asst)
  K_spec <- length(all_terms)
  pt <- sp$premium_type
  oo <- sp$outside_option

  out_csv <- file.path(SENS_DIR, paste0("coefs_", sp$name, ".csv"))
  summary_path <- sub("\\.csv$", "_summary.txt", out_csv)

  # Skip if already estimated
  if (file.exists(out_csv) && file.exists(summary_path)) {
    summ <- readLines(summary_path, n = 1)
    cat("--- Spec", si, "/", length(specs), ":", sp$name, "— cached:", summ, "---\n")
    next
  }

  cat("--- Spec", si, "/", length(specs), ":", sp$name,
      "(", K_spec + 1, "params,", pt, "+", oo, ") ---\n")

  # Build cells if not already built for this (premium_type, outside_option) combo
  combo_key <- paste0(pt, "_", oo)
  combo_dir <- file.path(SENS_DIR, paste0("cells_", combo_key))

  if (!(combo_key %in% built_combos)) {
    existing <- list.files(combo_dir, pattern = "^cell_.*_data\\.csv$")
    if (length(existing) == nrow(TARGET_CELLS)) {
      cat("  Cells for", pt, "+", oo, "already exist — skipping build.\n")
    } else {
      if (dir.exists(combo_dir)) unlink(combo_dir, recursive = TRUE)
      dir.create(combo_dir, recursive = TRUE)

      hh_split_cur <- if (oo == "acs") hh_split_acs else hh_split_exch

      cat("  Building cells for", pt, "+", oo, "...\n")
      n_built <- 0L
      for (j in seq_len(nrow(all_cells_meta))) {
        r_j <- all_cells_meta$region[j]
        y_j <- all_cells_meta$year[j]
        if (!any(TARGET_CELLS$region == r_j & TARGET_CELLS$year == y_j)) next

        set.seed(all_seeds[j])
        cell_key <- paste0(r_j, ".", y_j)
        hhs <- hh_split_cur[[cell_key]]
        if (is.null(hhs) || nrow(hhs) == 0) next
        hhs <- as.data.frame(hhs)

        plans <- plan_choice %>% filter(region == r_j, year == y_j)
        if (nrow(plans) == 0) { rm(hhs); next }

        cd <- build_choice_data(plans, hhs, SAMPLE_FRAC, weight_var = "hh_size",
                                spec = all_covars_union, premium_type = pt)
        rm(hhs, plans)

        if (!is.null(cd)) {
          cd$region <- r_j; cd$year <- y_j
          write.csv(cd, file.path(combo_dir, paste0("cell_", r_j, "_", y_j, "_data.csv")),
                    row.names = FALSE)
          n_built <- n_built + 1L
          cat("    Cell", r_j, y_j, ":", nrow(cd), "rows\n")
        }
        rm(cd); gc(verbose = FALSE)
      }
      cat("  Built", n_built, "cells\n")
    }
    built_combos <- c(built_combos, combo_key)
  }

  # Estimate
  loaded <- load_all_cells(combo_dir, all_terms, filter_assisted = -1L)
  demand_cells <- loaded$cells
  total_hh <- loaded$total_hh
  rm(loaded)
  demand_cells <- normalize_weights(demand_cells)

  cat("  HH:", total_hh, "\n")

  theta0 <- c(rep(0, K_spec), 1.0)
  if (!is.null(sp$warm_start)) {
    ws_path <- file.path(SENS_DIR, paste0("coefs_", sp$warm_start, ".csv"))
    if (file.exists(ws_path)) {
      sv <- read.csv(ws_path, stringsAsFactors = FALSE)
      for (k in seq_along(all_terms)) {
        m <- match(all_terms[k], sv$term)
        if (!is.na(m)) theta0[k] <- sv$estimate[m]
      }
      m_lam <- match("lambda", sv$term)
      if (!is.na(m_lam)) theta0[K_spec + 1] <- sv$estimate[m_lam]
      cat("  Warm-started from:", sp$warm_start, "\n")
    }
  }

  t0 <- proc.time()
  theta_opt <- tryCatch(
    bfgs_bhhh(theta0, demand_cells, max_iter = 500, print_every = 100),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  elapsed <- round((proc.time() - t0)[3], 1)

  if (is.null(theta_opt)) {
    cat("  FAILED (", elapsed, "sec)\n\n")
    rm(demand_cells)
    gc(verbose = FALSE)
    next
  }

  negll <- accumulate(theta_opt, demand_cells, compute_grad = FALSE)$negll

  coefs_out <- data.frame(term = c(all_terms, "lambda"), estimate = theta_opt)
  write.csv(coefs_out, out_csv, row.names = FALSE)

  summ_str <- sprintf("%.8f,%.6f,%.2f", theta_opt[1], theta_opt[K_spec + 1], negll)
  writeLines(summ_str, summary_path)

  cat(sprintf("  beta_prem = %.6f  lambda = %.4f  negLL = %.1f  (%s sec)\n\n",
              theta_opt[1], theta_opt[K_spec + 1], negll, elapsed))

  rm(demand_cells, theta_opt, coefs_out)
  gc(verbose = FALSE)
}

rm(plan_choice, all_cells_meta, all_seeds, hh_split_exch, hh_split_acs)
gc(verbose = FALSE)

# =========================================================================
# COLLECT AND DISPLAY RESULTS
# =========================================================================

cat("\n================================================================\n")
cat("DEMAND SENSITIVITY RESULTS\n")
cat("  Coefficients are per $1/month. Multiply by 100 for Evan's scale.\n")
cat("  Evan JHE 2019 CA base: -0.429 per $100 = -0.00429 per $1\n")
cat("================================================================\n\n")

results_list <- list()
for (si in seq_along(specs)) {
  sp <- specs[[si]]
  out_csv <- file.path(SENS_DIR, paste0("coefs_", sp$name, ".csv"))
  summary_path <- sub("\\.csv$", "_summary.txt", out_csv)

  if (!file.exists(out_csv) || !file.exists(summary_path)) next

  coefs <- read.csv(out_csv, stringsAsFactors = FALSE)
  summ <- strsplit(readLines(summary_path, n = 1), ",")[[1]]

  get_b <- function(nm) {
    v <- coefs$estimate[coefs$term == nm]
    if (length(v) == 0) NA_real_ else v
  }

  results_list[[length(results_list) + 1]] <- tibble(
    spec           = sp$name,
    premium_type   = sp$premium_type,
    outside_option = sp$outside_option,
    K              = nrow(coefs),
    b_prem_d1    = as.numeric(summ[1]),
    b_prem_d100  = as.numeric(summ[1]) * 100,
    lambda       = as.numeric(summ[2]),
    negLL        = as.numeric(summ[3]),
    intercept    = get_b("insured_intercept"),
    penalty_own  = get_b("penalty_own"),
    commission   = get_b("commission_broker")
  )
}

if (length(results_list) > 0) {
  summary_df <- bind_rows(results_list)

  print(summary_df %>%
    mutate(across(where(is.numeric), ~round(.x, 4))) %>%
    select(spec, outside_option, K, b_prem_d100, lambda, negLL, intercept, penalty_own, commission),
    n = Inf, width = 140)

  write_csv(summary_df, file.path(SENS_DIR, "sensitivity_summary.csv"))
  cat("\n  Saved to:", file.path(SENS_DIR, "sensitivity_summary.csv"), "\n")
} else {
  cat("  No results found.\n")
}

cat("\n  End time:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("Done.\n")
