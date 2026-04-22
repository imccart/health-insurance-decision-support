# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Standalone demand sensitivity analysis. Run from a cold
##                R session: activates renv, loads packages/helpers, and
##                regenerates hh_full.csv if missing.
##                Evan's JHE 2019 reports beta = -0.429 per $100/month.
##                Our estimates are per $1/month; multiply by 100 to compare.

# Setup (packages + helpers) ----------------------------------------------
options(vsc.rstudioapi = FALSE)
source("code/0-setup.R")  # activates renv and loads packages

source("code/data-build/_helpers.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/estimate_demand.R")

# Pipeline parameters -----------------------------------------------------
TEMP_DIR     <- "D:/temp-research-data/health-insurance-decision-support"
SAMPLE_FRAC  <- 0.02
MASTER_SEED  <- 20260224
SENS_DIR     <- file.path(TEMP_DIR, "demand_sensitivity")
if (!dir.exists(TEMP_DIR)) dir.create(TEMP_DIR, recursive = TRUE)
Sys.setenv(TEMP_DIR = TEMP_DIR, SAMPLE_FRAC = SAMPLE_FRAC, MASTER_SEED = MASTER_SEED)

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

  # Regenerate hh_full.csv from the current demand-build outputs if missing.
  if (!file.exists("data/output/hh_full.csv")) {
    source("code/analysis/1_decision-analysis.R")
    rm(hh_full, hh_ins)
    gc(verbose = FALSE)
  }

  hh_full    <- read_csv("data/output/hh_full.csv", show_col_types = FALSE)
  plan_data  <- read_csv("data/input/Covered California/plan_data.csv",
                          show_col_types = FALSE, name_repair = "minimal")
  broker_density <- read_csv("data/output/broker_density.csv", show_col_types = FALSE)
  commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

  # `plan_id` (set in step 2 from plan_data$Plan_Name2) is the canonical
  # plan identifier. ACS uninsured HHs have plan_id = NA. ipweight (ATT
  # weight) is reduced-form only; structural uses hh_size.
  cat("  hh_full:", nrow(hh_full), "rows\n")

  # Control function residual (broker density IV). `assisted` is observed
  # only on insured (CC) rows; fit on those and leave v_hat = NA for ACS.
  hh_full <- hh_full %>%
    left_join(broker_density %>% select(region, year, n_agents),
              by = c("region", "year"))
  ins_idx <- which(hh_full$insured == 1L)
  fs_model <- lm(assisted ~ n_agents + FPL + perc_0to17 + perc_18to25 +
                    perc_65plus + perc_black + perc_hispanic + perc_asian +
                    perc_male + household_size + factor(year),
                  data = hh_full[ins_idx, ])
  hh_full$v_hat <- NA_real_
  hh_full$v_hat[ins_idx] <- residuals(fs_model)
  cat("  First-stage F:", round(summary(fs_model)$fstatistic[1], 1), "\n")
  rm(fs_model, broker_density, ins_idx)

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
  hh_full <- hh_full %>% filter(!grepl("_CAT$", plan_id) | is.na(plan_id))

  # Plan demographics (weighted by household_size — structural weighting).
  # Collapse CSR-enhanced silver short codes (ANT_SIL73/87/94 → ANT_SIL).
  plan_demographics <- hh_full %>%
    filter(!is.na(plan_id), plan_id != "Uninsured") %>%
    mutate(plan_id = gsub("SIL(94|73|87)", "SIL", plan_id),
           wt = household_size) %>%
    group_by(plan_id, year) %>%
    summarize(share_18to34 = weighted.mean(perc_18to34, wt, na.rm = TRUE),
              share_35to54 = weighted.mean(perc_35to54, wt, na.rm = TRUE),
              share_hispanic = weighted.mean(perc_hispanic, wt, na.rm = TRUE),
              n_hh = n(), .groups = "drop")
  plan_demographics_yr <- plan_demographics %>%
    group_by(plan_id, year) %>%
    summarize(share_18to34 = weighted.mean(share_18to34, n_hh, na.rm = TRUE),
              share_35to54 = weighted.mean(share_35to54, n_hh, na.rm = TRUE),
              share_hispanic = weighted.mean(share_hispanic, n_hh, na.rm = TRUE),
              .groups = "drop")
  write_csv(plan_demographics_yr, file.path(TEMP_DIR, "plan_demographics.csv"))
  rm(plan_demographics, plan_demographics_yr)

  # Build plan_choice. Keep `metal` CSR-aware (matches enroll/HH metal).
  # base_metal collapses CSR-enhanced silvers for Hausman IV grouping (so
  # all silver variants share an IV across plans within issuer).
  plan_choice <- plan_data %>%
    select(region, year = ENROLLMENT_YEAR, Issuer_Name,
           metal = metal_level,
           plan_id = Plan_Name2, network_type = PLAN_NETWORK_TYPE,
           premium = Premium, msp = MSP, hsa = `HSA`) %>%
    mutate(region = as.integer(region), year = as.integer(year),
           issuer = standardize_insurer(Issuer_Name),
           base_metal = sub(" - Enhanced.*", "", metal)) %>%
    select(-Issuer_Name) %>%
    filter(metal != "Minimum Coverage")

  plan_choice <- plan_choice %>%
    group_by(issuer, base_metal, year) %>%
    mutate(n_other = n() - 1L,
           hausman_iv = (sum(premium) - premium) / pmax(n_other, 1L)) %>%
    ungroup() %>%
    mutate(hausman_iv = ifelse(n_other == 0, NA_real_, hausman_iv)) %>%
    group_by(issuer, base_metal, year) %>%
    mutate(hausman_iv = ifelse(is.na(hausman_iv), mean(premium, na.rm = TRUE), hausman_iv)) %>%
    ungroup() %>%
    select(-n_other, -base_metal)

  first_stage <- lm(premium ~ hausman_iv + metal + network_type + factor(year),
                    data = plan_choice)
  plan_choice$cf_resid <- residuals(first_stage)
  rm(first_stage)

  plan_choice <- plan_choice %>%
    mutate(insurer_prefix = sub("_.*", "", plan_id)) %>%
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

  # Write HH data. plan_id is the canonical identifier; NA for uninsured.
  hh_choice <- hh_full %>%
    filter(!grepl("_CAT$", plan_id) | is.na(plan_id)) %>%
    mutate(region = as.integer(region), year = as.integer(year),
           cutoff = AFFORD_THRESHOLDS[as.character(year)]) %>%
    select(region, year, household_id, FPL, subsidized_members, rating_factor,
           plan_id, oldest_member, cheapest_premium, subsidy, penalty,
           poverty_threshold, cutoff, household_size, v_hat,
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

# Full insurer × rating-area FEs (24 dummies, Anthem × region 1 = reference)
INSURER_MARKET_FE <- {
  ins_short <- c("Ant", "BS", "Kai", "HN", "Sm")
  combos <- expand.grid(ins = ins_short, ra = TARGET_REGIONS,
                        stringsAsFactors = FALSE)
  combos <- combos[!(combos$ins == "Ant" & combos$ra == 1), ]
  paste0("ix_", combos$ins, "_ra", combos$ra)
}

specs <- list(
  # A1 — core baseline: net premium + plan attrs + insurer FE (no demo, no FEs).
  list(name = "A1_net_exch",
       premium_type = "net", outside_option = "exchange",
       base = c("premium", PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS),
  # A2 — A1 + demographic × premium interactions.
  list(name = "A2_net_exch_dprem",
       premium_type = "net", outside_option = "exchange",
       base = c("premium", PLAN_ATTRS, INSURER_FE, DEMO_PREM),
       asst = ASST_TERMS),
  # L2 — saturated: net premium + plan attrs + full insurer × rating-area FEs
  # + demographic × premium (24 ix_<insurer>_ra<region> dummies in place of
  # INSURER_FE).
  list(name = "L2_net_exch_imxFE_dprem",
       premium_type = "net", outside_option = "exchange",
       base = c("premium", PLAN_ATTRS, INSURER_MARKET_FE, DEMO_PREM),
       asst = ASST_TERMS)
)
s2log(paste("  Specs:", length(specs)))

# =========================================================================
# BUILD CELL DATA (once per premium_type x outside_option combination)
# =========================================================================

s2log("  Reading plan_choice.csv")
plan_choice <- as_tibble(fread(file.path(TEMP_DIR, "plan_choice.csv")))

# Exchange HH (both insured and SIPP-filtered uninsured)
# Use fread (much faster than read.csv); avoids sustained I/O that triggers CrowdStrike
s2log("  Reading hh_choice.csv (1.6 GB)")
hh_exch <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
s2log(paste0("    loaded ", nrow(hh_exch), " rows"))

s2log(paste("  Exchange HH:", nrow(hh_exch)))
s2log(paste("  Memory:", round(as.numeric(object.size(hh_exch)) / 1e9, 2), "GB for hh_exch"))

# Split exchange data by region x year
s2log("  Splitting exchange data...")
hh_split_exch  <- split(hh_exch, by = c("region", "year"), keep.by = FALSE)
all_cells_meta <- unique(hh_exch[, .(region, year)])[order(region, year)]
rm(hh_exch); gc(verbose = FALSE)
s2log("  Split complete. Starting estimation loop.")

set.seed(MASTER_SEED)
all_seeds <- sample.int(1e7, nrow(all_cells_meta))

all_covars_union <- unique(unlist(lapply(specs, function(s) c(s$base, s$asst))))

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
    need_build <- length(existing) != nrow(TARGET_CELLS)
    if (!need_build) {
      cat("  Cells for", pt, "+", oo, "already exist — skipping build.\n")
    }
    if (need_build) {
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

        cd <- build_choice_data(plans, hhs, SAMPLE_FRAC,
                                spec = all_covars_union, premium_type = pt)
        rm(hhs, plans)

        if (!is.null(cd)) {
          # Insurer × rating-area FEs (insured-only dummies; Anthem in region 1
          # is the reference cell). Small insurers collapse to "Sm".
          insured_flag <- as.integer(cd$uninsured_plan == 0)
          ins_short <- c(Anthem = "Ant", Blue_Shield = "BS", Kaiser = "Kai",
                         Health_Net = "HN", Small = "Sm")
          insurer_id_cell <- ifelse(cd$Anthem == 1, "Anthem",
                              ifelse(cd$Blue_Shield == 1, "Blue_Shield",
                              ifelse(cd$Kaiser == 1, "Kaiser",
                              ifelse(cd$Health_Net == 1, "Health_Net",
                              ifelse(insured_flag == 1, "Small", NA_character_)))))
          for (ins_full in names(ins_short)) {
            for (ra in TARGET_REGIONS) {
              if (ins_full == "Anthem" && ra == 1) next
              col <- paste0("ix_", ins_short[ins_full], "_ra", ra)
              cd[[col]] <- as.integer(insurer_id_cell == ins_full & r_j == ra)
              cd[[col]][is.na(cd[[col]])] <- 0L
            }
          }

          cd$region <- r_j; cd$year <- y_j
          fwrite(cd, file.path(combo_dir, paste0("cell_", r_j, "_", y_j, "_data.csv")))
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
    rm(demand_cells, total_hh)
    closeAllConnections()
    gc(verbose = FALSE, full = TRUE); gc(verbose = FALSE, full = TRUE)
    Sys.sleep(2)
    next
  }

  negll <- accumulate(theta_opt, demand_cells, compute_grad = FALSE)$negll

  coefs_out <- data.frame(term = c(all_terms, "lambda"), estimate = theta_opt)
  write.csv(coefs_out, out_csv, row.names = FALSE)

  summ_str <- sprintf("%.8f,%.6f,%.2f", theta_opt[1], theta_opt[K_spec + 1], negll)
  writeLines(summ_str, summary_path)

  s2log(sprintf("  DONE: beta_prem=%.6f lambda=%.4f negLL=%.1f (%s sec)",
                theta_opt[1], theta_opt[K_spec + 1], negll, elapsed))

  rm(demand_cells, theta_opt, coefs_out, total_hh, theta0)
  closeAllConnections()
  # Two full GCs (R has 3 generations; multiple sweeps clear stragglers)
  gc(verbose = FALSE, full = TRUE); gc(verbose = FALSE, full = TRUE)
  Sys.sleep(2)
  s2log(paste0("  Cleanup done. Mem: ",
               round(sum(gc(verbose = FALSE)[, 2]) / 1024, 2), " GB"))
}

rm(plan_choice, all_cells_meta, all_seeds, hh_split_exch)
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
