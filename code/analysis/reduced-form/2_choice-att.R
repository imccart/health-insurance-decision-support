# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-24
## Date Edited:   2026-04-05
## Description:   Pooled nested logit choice model ATT with control function.
##                Estimate on unassisted HH via R estimator (optim L-BFGS-B),
##                predict counterfactual for assisted HH, compute ATT.
##                Expects plan_choice, partitioned parquets, and v_hat
##                available from _reduced-form.R runner.

# Dependencies ------------------------------------------------------------
library(arrow)

# Tuning ------------------------------------------------------------------

SAMPLE_FRAC   <- as.numeric(Sys.getenv("SAMPLE_FRAC"))
MASTER_SEED   <- as.integer(Sys.getenv("MASTER_SEED"))
TEMP_DIR      <- Sys.getenv("TEMP_DIR")
CELL_DIR      <- file.path(TEMP_DIR, "choice_cells")
PARTITION_DIR <- file.path(TEMP_DIR, "hh_choice_partitions_rf")

# Read plan data (runner already saved plan_choice.csv)
plan_choice <- read_csv(file.path(TEMP_DIR, "plan_choice.csv"), show_col_types = FALSE)

if (!dir.exists(CELL_DIR)) dir.create(CELL_DIR, recursive = TRUE)

# =========================================================================
# Phase 1: Build cell data (with CF interaction columns)
# =========================================================================

partition_files <- list.files(PARTITION_DIR, pattern = "^hh_\\d+_\\d+\\.parquet$",
                              full.names = FALSE)
cells <- tibble(file = partition_files) %>%
  mutate(
    region = as.integer(str_extract(file, "(?<=hh_)\\d+")),
    year   = as.integer(str_extract(file, "(?<=_)\\d{4}"))
  ) %>%
  arrange(region, year)

cat("Phase 1: Building cell data (", nrow(cells), "cells)...\n")

set.seed(MASTER_SEED)
cell_seeds <- sample.int(1e7, nrow(cells))

# Clean and recreate cell directory to ensure fresh data
if (dir.exists(CELL_DIR)) unlink(CELL_DIR, recursive = TRUE)
dir.create(CELL_DIR, recursive = TRUE)

n_built <- 0L
n_skip  <- 0L
cell_cache <- list()  # keep in memory for OOS prediction in Phase 4

for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]

  out_file <- file.path(CELL_DIR, paste0("cell_", r, "_", y, "_data.csv"))

  set.seed(cell_seeds[i])
  hhs <- tryCatch(
    read_parquet(file.path(PARTITION_DIR, paste0("hh_", r, "_", y, ".parquet"))),
    error = function(e) NULL
  )
  if (is.null(hhs) || nrow(hhs) == 0) { n_skip <- n_skip + 1L; next }

  plans <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans) == 0) { n_skip <- n_skip + 1L; next }

  # Pass full spec including CF terms — build_choice_data creates cf_* columns
  cd <- build_choice_data(plans, hhs, SAMPLE_FRAC, spec = REDUCED_FORM_FULL,
                          premium_type = "net")
  rm(hhs, plans)

  if (!is.null(cd)) {
    cd$region <- r
    cd$year <- y
    write_csv(cd, out_file)
    cell_cache[[paste0(r, "_", y)]] <- cd
    n_built <- n_built + 1L
  } else {
    n_skip <- n_skip + 1L
  }
  rm(cd)

  if (i %% 20 == 0) {
    gc(verbose = FALSE)
    cat("  Cell", i, "of", nrow(cells), "\n")
  }
}

rm(plan_choice)
gc(verbose = FALSE)
cat("  Built:", n_built, "  Skipped:", n_skip, "\n")

# Free cell_cache before estimation — 3GB of tibbles in memory.
# Rebuilt from CSVs in Phase 4 if needed.
rm(cell_cache)
gc(verbose = FALSE)

# =========================================================================
# Phase 2: Estimate demand (R, unassisted only)
# =========================================================================

cat("\nPhase 2: Running demand estimation (unassisted HH)...\n")

source("code/analysis/helpers/estimate_demand.R")

estimate_demand(
  cell_dir        = CELL_DIR,
  spec_path       = file.path(TEMP_DIR, "demand_spec_reduced.csv"),
  out_path        = "results/choice_coefficients.csv",
  filter_assisted = 0L,
  temp_dir        = NULL  # no MNL caching for reduced-form
)


# =========================================================================
# Phase 3: Read coefficients
# =========================================================================

cat("\nPhase 3: Reading coefficient estimates...\n")

coefs_path <- "results/choice_coefficients.csv"
if (!file.exists(coefs_path)) {
  cat("  Coefficients not found at", coefs_path, "\n")
  stop("Demand estimation failed — no output file.", call. = FALSE)
}

coefs <- read_csv(coefs_path, show_col_types = FALSE)
cat("  Coefficients:\n")
print(coefs, n = Inf)


# =========================================================================
# Phase 4: Per-cell OOS predictions and ATT
# =========================================================================

cat("\nPhase 4: Computing per-cell predictions...\n")

pred_list <- list()

for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]

  csv_path <- file.path(CELL_DIR, paste0("cell_", r, "_", y, "_data.csv"))
  if (!file.exists(csv_path)) next
  cell_data <- read_csv(csv_path, show_col_types = FALSE)

  # Assisted HH only for OOS prediction
  cell_oos <- cell_data %>% filter(assisted == 1)
  if (nrow(cell_oos) == 0) { rm(cell_data, cell_oos); next }

  preds <- predict_nested_logit(cell_oos, coefs,
                                unique(cell_data$plan_name[cell_data$plan_name != "Uninsured"]))

  cell_oos_dt <- as.data.table(cell_oos[, c("household_number", "plan_name", "choice")])
  cell_oos_dt <- merge(cell_oos_dt, preds, by = c("household_number", "plan_name"), all.x = TRUE)

  plan_summary <- cell_oos_dt[, .(
    tot_nonmiss   = sum(!is.na(pred)),
    obs_purchase  = sum(choice, na.rm = TRUE),
    pred_purchase = sum(pred, na.rm = TRUE)
  ), by = plan_name]
  plan_summary[, `:=`(region = r, year = y)]

  pred_list[[length(pred_list) + 1]] <- plan_summary
  rm(cell_data, cell_oos, preds, cell_oos_dt, plan_summary)
}

all_prob <- bind_rows(pred_list)
write_csv(all_prob, "results/choice_point_estimates.csv")

cat("  Predictions:", nrow(all_prob), "rows -> results/choice_point_estimates.csv\n")

rm(pred_list)
gc(verbose = FALSE)

cat("Choice model ATT estimation complete.\n")
