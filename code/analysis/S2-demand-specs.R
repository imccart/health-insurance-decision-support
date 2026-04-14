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

# Checkpoint log — survives terminal crashes
S2_LOG <- file.path(SENS_DIR, "checkpoint.log")
s2log <- function(msg) {
  line <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg, "\n")
  cat(line, file = S2_LOG, append = TRUE)
  cat(line)
  flush.console()
}
cat("", file = S2_LOG)  # truncate

s2log("=== Demand Sensitivity Analysis ===")

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

# Rating area FEs (insured-only dummies, region 1 = reference)
REGION_FE <- paste0("ra_", TARGET_REGIONS[-1])   # ra_4, ra_8, ra_13, ra_16

DEMO_INSURED <- c(
  "FPL_250to400_insured", "FPL_400plus_insured",
  "perc_male_insured", "perc_0to17_insured",
  "perc_18to34_insured", "perc_35to54_insured",
  "perc_black_insured", "perc_hispanic_insured",
  "perc_asian_insured", "perc_other_insured"
)

# Outside option types:
#   "exchange"      — uninsured are exchange non-enrollees (SIPP-filtered), raw weights
#   "acs_reweight"  — same exchange HH, but uninsured reweighted to match ACS market size

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
  # Group B: OOP premium + penalty_own, exchange uninsured
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
  # Group C: Net premium, ACS-reweighted uninsured
  # ---------------------------------------------------------------
  list(name = "C1_net_acsrw",
       premium_type = "net", outside_option = "acs_reweight",
       base = c("premium", PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS),
  list(name = "C2_net_acsrw_dprem",
       premium_type = "net", outside_option = "acs_reweight",
       base = c("premium", PLAN_ATTRS, INSURER_FE, DEMO_PREM),
       asst = ASST_TERMS),

  # Groups D (evan+exchange) and E (evan+acs_reweight) dropped —
  # proven algebraically identical to A and C respectively.
  # Penalty placement (net vs evan) doesn't matter in nested logit
  # when penalty enters through the same beta_prem coefficient.

  # ---------------------------------------------------------------
  # Group D: Net premium + rating area FEs, exchange uninsured
  # ---------------------------------------------------------------
  list(name = "D1_net_exch_raFE",
       premium_type = "net", outside_option = "exchange",
       base = c("premium", PLAN_ATTRS, INSURER_FE, REGION_FE),
       asst = ASST_TERMS),
  list(name = "D2_net_exch_raFE_dprem",
       premium_type = "net", outside_option = "exchange",
       base = c("premium", PLAN_ATTRS, INSURER_FE, REGION_FE, DEMO_PREM),
       asst = ASST_TERMS)
)

s2log(paste("  Specs:", length(specs)))

# =========================================================================
# BUILD CELL DATA (once per premium_type x outside_option combination)
# =========================================================================

plan_choice <- read_csv(file.path(TEMP_DIR, "plan_choice.csv"), show_col_types = FALSE)

# Exchange HH (both insured and SIPP-filtered uninsured)
hh_exch <- as.data.table(read.csv(file.path(TEMP_DIR, "hh_choice.csv")))

# ACS uninsured — used only for market size targets (not as observations)
acs_hh <- as.data.table(read.csv("data/output/demand_acs_households.csv"))
acs_hh[, region := as.integer(rating_area)]

# ACS target: weighted uninsured count per region x year
acs_targets <- acs_hh[, .(acs_uninsured = sum(weight)), by = .(region, year)]

# Exchange counts per region x year
exch_insured_n   <- hh_exch[!is.na(plan_number_nocsr), .N, by = .(region, year)]
exch_uninsured_n <- hh_exch[is.na(plan_number_nocsr),  .N, by = .(region, year)]
setnames(exch_insured_n, "N", "n_insured")
setnames(exch_uninsured_n, "N", "n_uninsured")

reweight_tab <- merge(exch_insured_n, exch_uninsured_n, by = c("region", "year"), all.x = TRUE)
reweight_tab <- merge(reweight_tab, acs_targets, by = c("region", "year"), all.x = TRUE)
reweight_tab[is.na(n_uninsured), n_uninsured := 0L]
reweight_tab[is.na(acs_uninsured), acs_uninsured := 0]

# Reweight factor: scale exchange uninsured ipweight so weighted count = ACS target
# If no exchange uninsured in a cell, factor is NA (cell will use raw weights)
reweight_tab[, rw_factor := fifelse(n_uninsured > 0, acs_uninsured / n_uninsured, NA_real_)]

s2log(paste("  Exchange HH:", nrow(hh_exch)))
s2log(paste("  ACS target uninsured:", round(sum(acs_targets$acs_uninsured))))
s2log(paste("  Exchange uninsured HH:", sum(reweight_tab$n_uninsured)))
s2log(paste("  Median reweight factor:", round(median(reweight_tab$rw_factor, na.rm = TRUE), 1)))
s2log(paste("  Memory:", round(as.numeric(object.size(hh_exch)) / 1e9, 2), "GB for hh_exch"))
rm(acs_hh, acs_targets, exch_insured_n, exch_uninsured_n)

# Keep reweight_tab for post-cell-build reweighting (build_choice_data
# overwrites ipweight with hh_size, so we apply the factor after)

# Split exchange data by region x year
s2log("  Splitting exchange data...")
hh_split_exch  <- split(hh_exch, by = c("region", "year"), keep.by = FALSE)
all_cells_meta <- unique(hh_exch[, .(region, year)])[order(region, year)]
rm(hh_exch); gc(verbose = FALSE)
s2log("  Split complete. Starting estimation loop.")

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

  s2log(paste0("--- Spec ", si, "/", length(specs), ": ", sp$name,
               " (", K_spec + 1, " params, ", pt, "+", oo, ") ---"))

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

      cat("  Building cells for", pt, "+", oo, "...\n")
      n_built <- 0L
      for (j in seq_len(nrow(all_cells_meta))) {
        r_j <- all_cells_meta$region[j]
        y_j <- all_cells_meta$year[j]
        if (!any(TARGET_CELLS$region == r_j & TARGET_CELLS$year == y_j)) next

        set.seed(all_seeds[j])
        cell_key <- paste0(r_j, ".", y_j)
        hhs <- hh_split_exch[[cell_key]]
        if (is.null(hhs) || nrow(hhs) == 0) next
        hhs <- as.data.frame(hhs)

        plans <- plan_choice %>% filter(region == r_j, year == y_j)
        if (nrow(plans) == 0) { rm(hhs); next }

        cd <- build_choice_data(plans, hhs, SAMPLE_FRAC, weight_var = "hh_size",
                                spec = all_covars_union, premium_type = pt)
        rm(hhs, plans)

        if (!is.null(cd)) {
          # Rating area FE columns (insured-only dummies, 0 for uninsured)
          insured_flag <- as.integer(cd$uninsured_plan == 0)
          for (ra in TARGET_REGIONS[-1]) {
            cd[[paste0("ra_", ra)]] <- as.integer(r_j == ra) * insured_flag
          }

          # For acs_reweight: scale uninsured HH weights by ACS market-size factor
          if (oo == "acs_reweight") {
            rw <- reweight_tab[region == r_j & year == y_j, rw_factor]
            if (length(rw) == 1 && !is.na(rw)) {
              unins_rows <- which(cd$plan_name == "Uninsured" | cd$uninsured_plan == 1)
              if (length(unins_rows) > 0) {
                # Identify uninsured HH ids, scale their weight across all rows
                unins_hh <- unique(cd$household_id[unins_rows[cd$plan_choice[unins_rows] == 1]])
                cd$ipweight[cd$household_id %in% unins_hh] <-
                  cd$ipweight[cd$household_id %in% unins_hh] * rw
              }
            }
          }
          cd$region <- r_j; cd$year <- y_j
          write.csv(cd, file.path(combo_dir, paste0("cell_", r_j, "_", y_j, "_data.csv")),
                    row.names = FALSE)
          n_built <- n_built + 1L
          cat("    Cell", r_j, y_j, ":", nrow(cd), "rows\n")
        }
        rm(cd); gc(verbose = FALSE)
      }
      s2log(paste("  Built", n_built, "cells"))
    }
    built_combos <- c(built_combos, combo_key)
  }

  # Estimate
  s2log(paste("  Loading cells from", combo_dir))
  loaded <- load_all_cells(combo_dir, all_terms, filter_assisted = -1L)
  demand_cells <- loaded$cells
  total_hh <- loaded$total_hh
  rm(loaded)
  demand_cells <- normalize_weights(demand_cells)

  s2log(paste("  Cells loaded. HH:", total_hh))

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

  s2log("  Starting BFGS-BHHH...")
  t0 <- proc.time()
  theta_opt <- tryCatch(
    bfgs_bhhh(theta0, demand_cells, max_iter = 500, print_every = 100),
    error = function(e) { s2log(paste("  ERROR:", e$message)); NULL }
  )
  elapsed <- round((proc.time() - t0)[3], 1)

  if (is.null(theta_opt)) {
    s2log(paste("  FAILED (", elapsed, "sec)"))
    rm(demand_cells)
    gc(verbose = FALSE)
    next
  }

  negll <- accumulate(theta_opt, demand_cells, compute_grad = FALSE)$negll

  coefs_out <- data.frame(term = c(all_terms, "lambda"), estimate = theta_opt)
  write.csv(coefs_out, out_csv, row.names = FALSE)

  summ_str <- sprintf("%.8f,%.6f,%.2f", theta_opt[1], theta_opt[K_spec + 1], negll)
  writeLines(summ_str, summary_path)

  s2log(sprintf("  DONE: beta_prem=%.6f lambda=%.4f negLL=%.1f (%s sec)",
                theta_opt[1], theta_opt[K_spec + 1], negll, elapsed))

  rm(demand_cells, theta_opt, coefs_out)
  gc(verbose = FALSE)
}

rm(plan_choice, all_cells_meta, all_seeds, hh_split_exch, reweight_tab)
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
