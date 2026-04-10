# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-09
## Description:   Comprehensive demand specification sensitivity analysis.
##                Estimates nested logit on a subset of cells with many specs.
##                Each spec runs as a SUBPROCESS to avoid memory accumulation.
##                All HH (filter_assisted = -1). Designed to run overnight.
##
##                Usage:
##                  Sys.setenv(TEMP_DIR = "D:/temp-research-data/health-insurance-decision-support")
##                  Sys.setenv(SAMPLE_FRAC = "0.20")
##                  Sys.setenv(MASTER_SEED = "20260224")
##                  source("code/analysis/structural/demand_sensitivity.R")

# Setup -------------------------------------------------------------------
source("code/0-setup.R")
library(arrow)
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

cat("=== Demand Specification Sensitivity Analysis (Overnight) ===\n")
cat("  Start time:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")

# =========================================================================
# SELECT CELLS
# =========================================================================

TARGET_REGIONS <- c(1L, 4L, 8L, 13L, 16L)
TARGET_YEARS   <- c(2014L, 2016L, 2018L)
TARGET_CELLS   <- expand.grid(region = TARGET_REGIONS, year = TARGET_YEARS) %>%
  as_tibble()

cat("  Target cells:", nrow(TARGET_CELLS), "\n")

# =========================================================================
# DEFINE SPECIFICATIONS
# =========================================================================

BASE <- c("premium", "penalty_own", "silver", "bronze", "hmo", "hsa")

DEMO_PREM <- c(
  "hh_size_prem", "perc_0to17_prem", "perc_18to34_prem", "perc_35to54_prem",
  "perc_male_prem", "perc_black_prem", "perc_hispanic_prem", "perc_asian_prem",
  "perc_other_prem", "FPL_250to400_prem", "FPL_400plus_prem"
)

INSURER_FE     <- c("Anthem", "Blue_Shield", "Kaiser", "Health_Net")
INSURER_METAL  <- c("Anthem_silver", "BS_silver", "Kaiser_silver", "HN_silver",
                     "Anthem_bronze", "BS_bronze", "Kaiser_bronze", "HN_bronze")
ASST_METAL     <- c("assisted_silver", "assisted_bronze")
ASST_METAL_4   <- c("assisted_silver", "assisted_bronze", "assisted_gold", "assisted_plat")
COMMISSION     <- c("commission_broker", "v_hat_commission")
CF_METAL       <- c("cf_silver", "cf_bronze")
CF_INSURER     <- c("cf_anthem", "cf_blue_shield", "cf_kaiser", "cf_health_net")
CF_PREMIUM     <- c("cf_resid")

specs <- list(
  # A: Current structural variants
  list(name="A1_baseline",       base=c(BASE, INSURER_FE, DEMO_PREM),                         asst=c(ASST_METAL, COMMISSION)),
  list(name="A2_no_demo_prem",   base=c(BASE, INSURER_FE),                                    asst=c(ASST_METAL, COMMISSION)),
  list(name="A3_asst_metal_4",   base=c(BASE, INSURER_FE, DEMO_PREM),                         asst=c(ASST_METAL_4, COMMISSION)),
  list(name="A4_no_commission",  base=c(BASE, INSURER_FE, DEMO_PREM),                         asst=c(ASST_METAL)),
  list(name="A5_no_assisted",    base=c(BASE, INSURER_FE, DEMO_PREM),                         asst=c(COMMISSION)),
  list(name="A6_minimal",        base=c(BASE, INSURER_FE, DEMO_PREM),                         asst=character(0)),

  # B: Premium nonlinearity
  list(name="B1_prem_sq",           base=c(BASE, "premium_sq", INSURER_FE, DEMO_PREM),        asst=c(ASST_METAL, COMMISSION)),
  list(name="B2_prem_sq_no_demo",   base=c(BASE, "premium_sq", INSURER_FE),                   asst=c(ASST_METAL, COMMISSION)),
  list(name="B3_prem_sq_no_comm",   base=c(BASE, "premium_sq", INSURER_FE, DEMO_PREM),        asst=c(ASST_METAL)),

  # C: Petrin-Train CF
  list(name="C1_cf_resid",          base=c(BASE, INSURER_FE, DEMO_PREM, CF_PREMIUM),          asst=c(ASST_METAL, COMMISSION)),
  list(name="C2_cf_resid_no_comm",  base=c(BASE, INSURER_FE, DEMO_PREM, CF_PREMIUM),          asst=c(ASST_METAL)),
  list(name="C3_cf_resid_no_demo",  base=c(BASE, INSURER_FE, CF_PREMIUM),                     asst=c(ASST_METAL, COMMISSION)),
  list(name="C4_cf_resid_prem_sq",  base=c(BASE, "premium_sq", INSURER_FE, DEMO_PREM, CF_PREMIUM), asst=c(ASST_METAL, COMMISSION)),

  # D: Insurer×metal FEs
  list(name="D1_ins_metal",          base=c(BASE, INSURER_FE, INSURER_METAL, DEMO_PREM),       asst=c(ASST_METAL, COMMISSION)),
  list(name="D2_ins_metal_cf_resid", base=c(BASE, INSURER_FE, INSURER_METAL, DEMO_PREM, CF_PREMIUM), asst=c(ASST_METAL, COMMISSION)),
  list(name="D3_ins_metal_no_comm",  base=c(BASE, INSURER_FE, INSURER_METAL, DEMO_PREM),       asst=c(ASST_METAL)),

  # E: CF interactions (v_hat × plan indicators)
  list(name="E1_cf_metal",        base=c(BASE, INSURER_FE, DEMO_PREM),                        asst=c(ASST_METAL, COMMISSION, CF_METAL)),
  list(name="E2_cf_insurer",      base=c(BASE, INSURER_FE, DEMO_PREM),                        asst=c(ASST_METAL, COMMISSION, CF_INSURER)),
  list(name="E3_cf_all",          base=c(BASE, INSURER_FE, DEMO_PREM),                        asst=c(ASST_METAL, COMMISSION, CF_INSURER, CF_METAL)),
  list(name="E4_cf_all_cf_resid", base=c(BASE, INSURER_FE, DEMO_PREM, CF_PREMIUM),            asst=c(ASST_METAL, COMMISSION, CF_INSURER, CF_METAL)),

  # F: Combined
  list(name="F1_ins_metal_prem_sq",          base=c(BASE, "premium_sq", INSURER_FE, INSURER_METAL, DEMO_PREM),            asst=c(ASST_METAL, COMMISSION)),
  list(name="F2_ins_metal_cf_resid_prem_sq", base=c(BASE, "premium_sq", INSURER_FE, INSURER_METAL, DEMO_PREM, CF_PREMIUM), asst=c(ASST_METAL, COMMISSION))
)

cat("  Specifications:", length(specs), "\n\n")

# =========================================================================
# BUILD CELL DATA (union of all covariates, once)
# =========================================================================

all_covars_union <- unique(unlist(lapply(specs, function(s) c(s$base, s$asst))))
all_covars_union <- setdiff(all_covars_union, "premium_sq")
cat("  Union of covariates:", length(all_covars_union), "\n")

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

sens_cell_dir <- file.path(SENS_DIR, "cells")
if (dir.exists(sens_cell_dir)) unlink(sens_cell_dir, recursive = TRUE)
dir.create(sens_cell_dir, recursive = TRUE)

cat("  Building cell data...\n")
n_built <- 0L
for (i in seq_len(nrow(all_cells_meta))) {
  r <- all_cells_meta$region[i]
  y <- all_cells_meta$year[i]
  if (!any(TARGET_CELLS$region == r & TARGET_CELLS$year == y)) next

  set.seed(all_seeds[i])
  hhs <- tryCatch(read_parquet(file.path(PARTITION_DIR, paste0("hh_", r, "_", y, ".parquet"))),
                   error = function(e) NULL)
  if (is.null(hhs) || nrow(hhs) == 0) next

  plans <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans) == 0) { rm(hhs); next }

  cd <- build_choice_data(plans, hhs, SAMPLE_FRAC, weight_var = "hh_size",
                          spec = all_covars_union)
  rm(hhs, plans)

  if (!is.null(cd)) {
    cd$region <- r; cd$year <- y
    if ("premium" %in% names(cd)) cd$premium_sq <- cd$premium^2
    write_csv(cd, file.path(sens_cell_dir, paste0("cell_", r, "_", y, "_data.csv")))
    n_built <- n_built + 1L
    cat("    Cell", r, y, ":", nrow(cd), "rows\n")
  }
  rm(cd); gc(verbose = FALSE)
}
rm(plan_choice, all_cells_meta, all_seeds)
gc(verbose = FALSE)

cat("  Cells built:", n_built, "\n\n")

# =========================================================================
# WRITE WORKER SCRIPT (runs one spec in a fresh R process)
# =========================================================================

worker_path <- file.path(SENS_DIR, "worker.R")
writeLines(con = worker_path, text = '
# Demand sensitivity worker — runs ONE spec in isolated R process
args <- commandArgs(trailingOnly = TRUE)
spec_csv   <- args[1]  # path to spec CSV (term, group columns)
cell_dir   <- args[2]  # path to cell CSVs
out_csv    <- args[3]  # path to write coefficients
spec_name  <- args[4]  # spec name for logging

source("code/0-setup.R")
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

theta_opt <- tryCatch(
  bfgs_bhhh(c(rep(0, K), 1.0), cells, max_iter = 500, print_every = 100),
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

# Write a one-line summary for the dispatcher to read
summary_path <- sub("\\\\.csv$", "_summary.txt", out_csv)
writeLines(sprintf("%.8f,%.6f,%.2f", theta_opt[1], theta_opt[K + 1], negll),
           summary_path)
')

cat("  Worker script written to:", worker_path, "\n\n")

# =========================================================================
# DISPATCH EACH SPEC AS SUBPROCESS
# =========================================================================

cat("Dispatching", length(specs), "specifications...\n\n")

for (si in seq_along(specs)) {
  sp <- specs[[si]]
  all_terms <- c(sp$base, sp$asst)
  K_spec <- length(all_terms)

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

  out_csv <- file.path(SENS_DIR, paste0("coefs_", sp$name, ".csv"))

  # Run as subprocess
  t0 <- proc.time()
  exit_code <- system2(
    RSCRIPT,
    args = c(shQuote(worker_path), shQuote(spec_csv), shQuote(sens_cell_dir),
             shQuote(out_csv), shQuote(sp$name)),
    stdout = "", stderr = ""
  )
  elapsed <- round((proc.time() - t0)[3], 1)

  # Read summary if it exists
  summary_path <- sub("\\.csv$", "_summary.txt", out_csv)
  if (file.exists(summary_path)) {
    summ <- readLines(summary_path, n = 1)
    cat("    Result:", summ, " (", elapsed, "sec)\n\n")
  } else {
    cat("    FAILED (exit code", exit_code, ",", elapsed, "sec)\n\n")
  }
}

# =========================================================================
# COLLECT RESULTS
# =========================================================================

cat("\n================================================================\n")
cat("SENSITIVITY ANALYSIS SUMMARY\n")
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
    spec_name    = sp$name,
    n_params     = nrow(coefs),
    beta_premium = as.numeric(summ[1]),
    lambda       = as.numeric(summ[2]),
    negLL        = as.numeric(summ[3]),
    premium_sq   = get_b("premium_sq"),
    cf_resid     = get_b("cf_resid"),
    commission   = get_b("commission_broker"),
    asst_silver  = get_b("assisted_silver"),
    Anthem       = get_b("Anthem"),
    Kaiser       = get_b("Kaiser")
  )
}

if (length(results_list) > 0) {
  summary_df <- bind_rows(results_list) %>%
    arrange(beta_premium)  # sort by beta_premium (most negative first)

  print(summary_df %>%
    mutate(across(where(is.numeric), ~round(.x, 6))) %>%
    select(spec_name, n_params, beta_premium, lambda, negLL,
           premium_sq, cf_resid, commission),
    n = Inf, width = 140)

  write_csv(summary_df, file.path(SENS_DIR, "sensitivity_summary.csv"))
  cat("\n  Saved to:", file.path(SENS_DIR, "sensitivity_summary.csv"), "\n")
} else {
  cat("  No results found.\n")
}

cat("\n  End time:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("Done.\n")
