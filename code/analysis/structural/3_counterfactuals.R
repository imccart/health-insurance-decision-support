# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Date Edited:   2026-03-24
## Description:   Counterfactual simulation coordinator. Dispatches one Rscript
##                subprocess per region-year cell (avoids memory accumulation
##                from nleqslv copies), then collects and summarizes results.

# Setup (idempotent — safe to re-source) -----------------------------------
source("code/0-setup.R")
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/_helpers-analysis.R")
source("code/analysis/_helpers-choice.R")
source("code/analysis/_helpers-supply.R")
library(arrow)

# Tuning parameters --------------------------------------------------------
SAMPLE_FRAC   <- 0.20
PARTITION_DIR <- "data/output/hh_choice_partitions"

# =========================================================================
# PHASE 1: Discover cells
# =========================================================================

cat("\nPhase 1: Discovering region-year cells...\n")

partition_files <- list.files(PARTITION_DIR, pattern = "^hh_\\d+_\\d+\\.parquet$",
                              full.names = FALSE)
cells <- tibble(file = partition_files) %>%
  mutate(
    region = as.integer(str_extract(file, "(?<=hh_)\\d+")),
    year   = as.integer(str_extract(file, "(?<=_)\\d{4}"))
  ) %>%
  arrange(region, year)

cat("  Region-year cells:", nrow(cells), "\n")

set.seed(20260224)
cell_seeds <- sample.int(1e7, nrow(cells))

# =========================================================================
# PHASE 2: Check prerequisites
# =========================================================================

cat("\nPhase 2: Checking prerequisites...\n")

if (!file.exists("data/output/supply_results.csv")) {
  stop("supply_results.csv not found — run 2_supply.R first")
}
if (!file.exists("data/output/choice_coefficients_structural.csv")) {
  stop("choice_coefficients_structural.csv not found — run 1_demand.R first")
}
cat("  Prerequisites OK.\n")

# =========================================================================
# PHASE 3: Dispatch workers
# =========================================================================

cat("\nPhase 3: Dispatching counterfactual workers...\n")

if (!dir.exists("data/output/cf_cells")) dir.create("data/output/cf_cells", recursive = TRUE)

rscript_exe <- file.path(R.home("bin"), "Rscript.exe")
worker_script <- "code/analysis/structural/3a_cf-worker.R"

n_success <- 0L
n_fail    <- 0L
failed_cells <- character(0)
t_start <- Sys.time()

for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]
  out_file <- file.path("data/output/cf_cells", paste0("cf_", r, "_", y, ".csv"))

  cmd <- paste(
    shQuote(rscript_exe),
    worker_script,
    r, y, cell_seeds[i], SAMPLE_FRAC
  )

  exit_code <- system(cmd)

  if (exit_code == 0 && file.exists(out_file)) {
    n_success <- n_success + 1L
  } else {
    n_fail <- n_fail + 1L
    failed_cells <- c(failed_cells, paste0(r, "_", y))
    cat("  FAILED: cell", r, y, "(exit code", exit_code, ")\n")
  }

  if (i %% 5 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
    cat(sprintf("  Progress: %d/%d cells (%.1f min elapsed)\n", i, nrow(cells), elapsed))
  }
}

elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
cat(sprintf("\nDispatch complete: %d success, %d failed (%.1f min total)\n",
            n_success, n_fail, elapsed_total))

if (n_fail > 0) {
  cat("  Failed cells:", paste(failed_cells, collapse = ", "), "\n")
}

# =========================================================================
# PHASE 4: Collect results
# =========================================================================

cat("\nPhase 4: Collecting results...\n")

cf_files <- list.files("data/output/cf_cells", pattern = "^cf_\\d+_\\d+\\.csv$",
                        full.names = TRUE)

if (length(cf_files) == 0) {
  stop("No counterfactual cell results found in data/output/cf_cells/")
}

cf_results <- cf_files %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows()

write_csv(cf_results, "data/output/counterfactual_results.csv")
cat("  Collected", nrow(cf_results), "rows from", length(cf_files), "cells\n")
cat("  Written to data/output/counterfactual_results.csv\n")

# =========================================================================
# PHASE 5: Summary
# =========================================================================

cat("\n--- Counterfactual Summary ---\n")

for (sc_name in c("observed", "zero", "uniform")) {
  sc_data <- cf_results %>% filter(scenario == sc_name)
  if (nrow(sc_data) == 0) next

  converged_pct <- mean(sc_data$nleqslv_termcd <= 2, na.rm = TRUE) * 100

  cat("\nScenario:", sc_name, "\n")
  cat("  Plans:", nrow(sc_data), "\n")
  cat("  Premium change: mean =", round(mean(sc_data$premium_change, na.rm = TRUE), 2),
      ", median =", round(median(sc_data$premium_change, na.rm = TRUE), 2),
      ", range = [", round(min(sc_data$premium_change, na.rm = TRUE), 2),
      ",", round(max(sc_data$premium_change, na.rm = TRUE), 2), "]\n")
  cat("  CS (weighted avg):", round(mean(sc_data$cs_weighted, na.rm = TRUE), 2), "\n")
  cat("  Converged:", round(converged_pct, 1), "%\n")
}

# Welfare comparison: zero vs observed
cs_obs <- cf_results %>%
  filter(scenario == "observed") %>%
  distinct(region, year, cs_weighted) %>%
  rename(cs_obs = cs_weighted)

cs_zero <- cf_results %>%
  filter(scenario == "zero") %>%
  distinct(region, year, cs_weighted) %>%
  rename(cs_zero = cs_weighted)

welfare <- inner_join(cs_obs, cs_zero, by = c("region", "year"))
if (nrow(welfare) > 0) {
  welfare <- welfare %>% mutate(delta_cs = cs_zero - cs_obs)
  cat("\nWelfare effect of eliminating commissions:\n")
  cat("  Mean delta CS:", round(mean(welfare$delta_cs, na.rm = TRUE), 2), "$/month/HH\n")
  cat("  Positive (consumers gain):", sum(welfare$delta_cs > 0, na.rm = TRUE),
      "of", nrow(welfare), "cells\n")
}

cat("\nCounterfactual simulation complete.\n")
