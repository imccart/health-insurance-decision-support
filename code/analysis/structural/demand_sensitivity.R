# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-09
## Description:   Demand sensitivity analysis — systematic build-up of
##                specification components to assess what moves β_premium.
##                Self-contained: includes data prep if intermediate files
##                are missing. Each spec runs as a SUBPROCESS to avoid
##                memory accumulation.
##
##                Evan's JHE 2019 reports β = -0.429 per $100/month.
##                Our estimates are per $1/month; multiply by 100 to compare.
##
##                Usage (from _analysis.R or standalone):
##                  Sys.setenv(TEMP_DIR = "...", SAMPLE_FRAC = "0.05", MASTER_SEED = "20260224")
##                  source("code/analysis/structural/demand_sensitivity.R")

# Setup -------------------------------------------------------------------
source("code/0-setup.R")
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")

SAMPLE_FRAC   <- as.numeric(Sys.getenv("SAMPLE_FRAC"))
MASTER_SEED   <- as.integer(Sys.getenv("MASTER_SEED"))
TEMP_DIR      <- Sys.getenv("TEMP_DIR")
PARTITION_DIR <- file.path(TEMP_DIR, "hh_choice_partitions")
SENS_DIR      <- file.path(TEMP_DIR, "demand_sensitivity")

if (!dir.exists(SENS_DIR)) dir.create(SENS_DIR, recursive = TRUE)

RSCRIPT <- file.path(R.home("bin"), "Rscript.exe")

cat("=== Demand Sensitivity Analysis ===\n")
cat("  Start time:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")

# =========================================================================
# DATA PREP — only when intermediate files are missing
# =========================================================================

prep_files <- c(
  file.path(TEMP_DIR, "plan_choice.csv"),
  file.path(TEMP_DIR, "plan_demographics.csv"),
  file.path(TEMP_DIR, "hh_choice_partitions")
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

  plan_choice$comm_pmpm <- 0
  for (y in unique(plan_choice$year)) {
    idx <- plan_choice$year == y
    plan_choice$comm_pmpm[idx] <- get_commission_pmpm(
      plan_choice$plan_name[idx], plan_choice[idx, ], y, commission_lookup)
  }
  write_csv(plan_choice, file.path(TEMP_DIR, "plan_choice.csv"))
  cat("  plan_choice:", nrow(plan_choice), "rows\n")

  # Partition HH
  cat("Partitioning HH data...\n")
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

  partition_dir <- file.path(TEMP_DIR, "hh_choice_partitions")
  if (dir.exists(partition_dir)) unlink(partition_dir, recursive = TRUE)
  dir.create(partition_dir, recursive = TRUE)

  hh_dt <- as.data.table(hh_choice)
  rm(hh_choice, hh_full, plan_data, plan_choice, commission_lookup)
  gc(full = TRUE, verbose = FALSE)

  hh_dt[, cell_key := paste0("hh_", region, "_", year)]
  split_list <- split(hh_dt, by = "cell_key", keep.by = FALSE)
  rm(hh_dt); gc(full = TRUE, verbose = FALSE)

  for (nm in names(split_list)) {
    write.csv(split_list[[nm]], file.path(partition_dir, paste0(nm, ".csv")), row.names = FALSE)
  }
  cat("  Partitioned:", length(split_list), "cells\n")
  rm(split_list)
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

# Comprehensive comparison: posted vs evan × specification components
specs <- list(
  # --- Group A: Posted premium (our current approach) ---
  # A1: Minimal baseline
  list(name = "A1_posted_base",
       premium_type = "posted",
       base = c("premium", "penalty_own", PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS),
  # A2: + demo x premium (our current structural spec)
  list(name = "A2_posted_dprem",
       premium_type = "posted",
       base = c("premium", "penalty_own", PLAN_ATTRS, INSURER_FE, DEMO_PREM),
       asst = ASST_TERMS),
  # A3: + insured intercept + demo x insured (Evan's approach with posted premium)
  list(name = "A3_posted_icept_dins",
       premium_type = "posted",
       base = c("premium", "penalty_own", "insured_intercept", DEMO_INSURED,
                 PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS),

  # --- Group B: Net premium (net of subsidy, penalty baked in) ---
  # --- Group C: OOP premium (net of subsidy, penalty_own separate) ---
  list(name = "C1_oop_base",
       premium_type = "oop",
       base = c("premium", "penalty_own", PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS),

  # --- Group B: Net premium (net of subsidy, penalty baked in) ---
  # B1: Minimal baseline
  list(name = "B1_net_base",
       premium_type = "net",
       base = c("premium", PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS),
  # B2: + insured intercept (warm-start from B1)
  list(name = "B2_net_icept",
       premium_type = "net",
       base = c("premium", "insured_intercept", PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS,
       warm_start = "B1_net_base"),
  # B3: + insured intercept + demo x insured (warm-start from B2)
  list(name = "B3_net_icept_dins",
       premium_type = "net",
       base = c("premium", "insured_intercept", DEMO_INSURED, PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS,
       warm_start = "B2_net_icept"),
  # B4: + demo x premium (no intercept — shows demo×prem effect with evan premium)
  list(name = "B4_net_dprem",
       premium_type = "net",
       base = c("premium", DEMO_PREM, PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS),
  # B5: + insured intercept + demo x insured + demo x premium (warm-start from B3)
  list(name = "B5_net_full",
       premium_type = "net",
       base = c("premium", "insured_intercept", DEMO_INSURED, DEMO_PREM,
                 PLAN_ATTRS, INSURER_FE),
       asst = ASST_TERMS,
       warm_start = "B3_net_icept_dins")
)

cat("  Specifications:", length(specs), "\n\n")

# =========================================================================
# BUILD CELL DATA (once per premium_type)
# =========================================================================

plan_choice <- read_csv(file.path(TEMP_DIR, "plan_choice.csv"), show_col_types = FALSE)

all_cells_meta <- tibble(
  file = list.files(PARTITION_DIR, pattern = "^hh_\\d+_\\d+\\.parquet$", full.names = FALSE)
) %>%
  mutate(
    region = as.integer(str_extract(file, "(?<=hh_)\\d+")),
    year   = as.integer(str_extract(file, "(?<=_)\\d{4}"))
  ) %>%
  arrange(region, year)

set.seed(MASTER_SEED)
all_seeds <- sample.int(1e7, nrow(all_cells_meta))

# All covariates across all specs (union)
all_covars_union <- unique(unlist(lapply(specs, function(s) c(s$base, s$asst))))

premium_types <- unique(sapply(specs, function(s) s$premium_type))

for (pt in premium_types) {
  pt_dir <- file.path(SENS_DIR, paste0("cells_", pt))

  # Skip if cells already exist with the right count
  existing <- list.files(pt_dir, pattern = "^cell_.*_data\\.csv$")
  if (length(existing) == nrow(TARGET_CELLS)) {
    cat("  Cells for", pt, "already exist (", length(existing), "files) — skipping.\n")
    next
  }

  if (dir.exists(pt_dir)) unlink(pt_dir, recursive = TRUE)
  dir.create(pt_dir, recursive = TRUE)

  cat("  Building cells for premium_type =", pt, "...\n")
  n_built <- 0L

  for (i in seq_len(nrow(all_cells_meta))) {
    r <- all_cells_meta$region[i]
    y <- all_cells_meta$year[i]
    if (!any(TARGET_CELLS$region == r & TARGET_CELLS$year == y)) next

    set.seed(all_seeds[i])
    hhs <- tryCatch(read.csv(file.path(PARTITION_DIR, paste0("hh_", r, "_", y, ".csv"))),
                     error = function(e) NULL)
    if (is.null(hhs) || nrow(hhs) == 0) next

    plans <- plan_choice %>% filter(region == r, year == y)
    if (nrow(plans) == 0) { rm(hhs); next }

    cd <- build_choice_data(plans, hhs, SAMPLE_FRAC, weight_var = "hh_size",
                            spec = all_covars_union, premium_type = pt)
    rm(hhs, plans)

    if (!is.null(cd)) {
      cd$region <- r; cd$year <- y
      write_csv(cd, file.path(pt_dir, paste0("cell_", r, "_", y, "_data.csv")))
      n_built <- n_built + 1L
      cat("    Cell", r, y, ":", nrow(cd), "rows\n")
    }
    rm(cd); gc(verbose = FALSE)
  }
  cat("  Built", n_built, "cells for", pt, "\n\n")
}
rm(plan_choice, all_cells_meta, all_seeds)
gc(verbose = FALSE)

# =========================================================================
# WRITE WORKER SCRIPT
# =========================================================================

worker_path <- file.path(SENS_DIR, "worker.R")
writeLines(con = worker_path, text = '
# Demand sensitivity worker — runs ONE spec in isolated R process
args <- commandArgs(trailingOnly = TRUE)
spec_csv   <- args[1]
cell_dir   <- args[2]
out_csv    <- args[3]
spec_name  <- args[4]
start_csv  <- if (length(args) >= 5) args[5] else ""

# Set renv library path directly (activate.R crashes in subprocesses)
.libPaths(c("renv/library/windows/R-4.5/x86_64-w64-mingw32", .libPaths()))
library(data.table)
library(readr)

source("code/analysis/helpers/estimate_demand.R")

spec_df <- read.csv(spec_csv, stringsAsFactors = FALSE)
covars <- spec_df$term
K <- length(covars)

cat("  Worker:", spec_name, " K =", K, "\\n")

loaded <- load_all_cells(cell_dir, covars, filter_assisted = -1L)
cells <- loaded$cells
total_hh <- loaded$total_hh
rm(loaded)
cells <- normalize_weights(cells)

cat("  HH:", total_hh, "\\n")

# Starting values: use warm-start CSV if provided, else zeros + lambda=1
theta0 <- c(rep(0, K), 1.0)
if (start_csv != "" && file.exists(start_csv)) {
  sv <- read.csv(start_csv, stringsAsFactors = FALSE)
  for (i in seq_along(covars)) {
    m <- match(covars[i], sv$term)
    if (!is.na(m)) theta0[i] <- sv$estimate[m]
  }
  m_lam <- match("lambda", sv$term)
  if (!is.na(m_lam)) theta0[K + 1] <- sv$estimate[m_lam]
  cat("  Warm-started from:", start_csv, "\\n")
}

theta_opt <- tryCatch(
  bfgs_bhhh(theta0, cells, max_iter = 500, print_every = 100),
  error = function(e) { cat("  ERROR:", e$message, "\\n"); NULL }
)

if (is.null(theta_opt)) {
  cat("  FAILED\\n")
  quit(save = "no", status = 1)
}

negll <- accumulate(theta_opt, cells, compute_grad = FALSE)$negll

coefs <- data.frame(term = c(covars, "lambda"), estimate = theta_opt)
write.csv(coefs, out_csv, row.names = FALSE)

cat(sprintf("  beta_prem = %.6f  lambda = %.4f  negLL = %.1f\\n",
            theta_opt[1], theta_opt[K + 1], negll))

summary_path <- sub("\\\\.csv$", "_summary.txt", out_csv)
writeLines(sprintf("%.8f,%.6f,%.2f", theta_opt[1], theta_opt[K + 1], negll),
           summary_path)
')

cat("  Worker script written.\n\n")

# =========================================================================
# DISPATCH EACH SPEC AS SUBPROCESS
# =========================================================================

cat("Dispatching", length(specs), "specifications...\n\n")

for (si in seq_along(specs)) {
  sp <- specs[[si]]
  all_terms <- c(sp$base, sp$asst)
  K_spec <- length(all_terms)

  out_csv <- file.path(SENS_DIR, paste0("coefs_", sp$name, ".csv"))
  summary_path <- sub("\\.csv$", "_summary.txt", out_csv)

  # Skip if already estimated
  if (file.exists(out_csv) && file.exists(summary_path)) {
    summ <- readLines(summary_path, n = 1)
    cat("--- Spec", si, "/", length(specs), ":", sp$name, "— cached:", summ, "---\n")
    next
  }

  cat("--- Spec", si, "/", length(specs), ":", sp$name,
      "(", K_spec + 1, "params) ---\n")

  # Write spec CSV
  spec_csv <- file.path(SENS_DIR, paste0("spec_", sp$name, ".csv"))
  spec_df <- data.frame(
    term  = all_terms,
    group = c(rep("base", length(sp$base)), rep("assisted", length(sp$asst))),
    stringsAsFactors = FALSE
  )
  write.csv(spec_df, spec_csv, row.names = FALSE)

  cell_dir <- file.path(SENS_DIR, paste0("cells_", sp$premium_type))

  # Warm-start from a prior spec's coefficients if specified
  start_arg <- ""
  if (!is.null(sp$warm_start)) {
    ws_path <- file.path(SENS_DIR, paste0("coefs_", sp$warm_start, ".csv"))
    if (file.exists(ws_path)) start_arg <- ws_path
  }

  log_file <- file.path(SENS_DIR, paste0("log_", sp$name, ".log"))

  t0 <- proc.time()
  exit_code <- system2(
    RSCRIPT,
    args = c(shQuote(worker_path), shQuote(spec_csv), shQuote(cell_dir),
             shQuote(out_csv), shQuote(sp$name), shQuote(start_arg)),
    stdout = log_file, stderr = log_file
  )
  elapsed <- round((proc.time() - t0)[3], 1)

  if (file.exists(summary_path)) {
    summ <- readLines(summary_path, n = 1)
    cat("    Result:", summ, " (", elapsed, "sec)\n\n")
  } else {
    # Show last few lines of log for diagnosis
    if (file.exists(log_file)) {
      log_tail <- tail(readLines(log_file), 5)
      cat("    FAILED (exit code", exit_code, ",", elapsed, "sec)\n")
      cat("    Log tail:", paste(log_tail, collapse = "\n             "), "\n\n")
    } else {
      cat("    FAILED (exit code", exit_code, ",", elapsed, "sec)\n\n")
    }
  }
}

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
    spec         = sp$name,
    premium_type = sp$premium_type,
    K            = nrow(coefs),
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
    select(spec, K, b_prem_d100, lambda, negLL, intercept, penalty_own, commission),
    n = Inf, width = 140)

  write_csv(summary_df, file.path(SENS_DIR, "sensitivity_summary.csv"))
  cat("\n  Saved to:", file.path(SENS_DIR, "sensitivity_summary.csv"), "\n")
} else {
  cat("  No results found.\n")
}

cat("\n  End time:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("Done.\n")
