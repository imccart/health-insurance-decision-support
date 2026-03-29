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

if (!file.exists("results/supply_results.csv")) {
  stop("supply_results.csv not found — run 2_supply.R first")
}
if (!file.exists("results/choice_coefficients_structural.csv")) {
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

write_csv(cf_results, "results/counterfactual_results.csv")
cat("  Collected", nrow(cf_results), "rows from", length(cf_files), "cells\n")
cat("  Written to results/counterfactual_results.csv\n")

# =========================================================================
# PHASE 5: Summary
# =========================================================================

cat("\n--- Counterfactual Summary ---\n")

# Summarize observed and uniform scenarios
for (sc_name in c("observed", "uniform")) {
  sc_data <- cf_results %>% filter(scenario == sc_name)
  if (nrow(sc_data) == 0) next

  converged_pct <- mean(sc_data$nleqslv_termcd <= 2, na.rm = TRUE) * 100

  cat("\nScenario:", sc_name, "\n")
  cat("  Plans:", nrow(sc_data), "\n")
  cat("  Premium change: mean =", round(mean(sc_data$premium_change, na.rm = TRUE), 2),
      ", median =", round(median(sc_data$premium_change, na.rm = TRUE), 2), "\n")
  cat("  CS (weighted avg):", round(mean(sc_data$cs_weighted, na.rm = TRUE), 2), "\n")
  cat("  Converged:", round(converged_pct, 1), "%\n")
  if ("ra_iter" %in% names(sc_data)) {
    cat("  RA iterations (median):", round(median(sc_data$ra_iter, na.rm = TRUE), 1), "\n")
  }
}

# Tau gradient summary
tau_scenarios <- cf_results %>% filter(grepl("^zero_tau", scenario))
if (nrow(tau_scenarios) > 0) {
  cat("\n--- Broker-to-Navigator Substitution Gradient ---\n")

  cs_obs_val <- cf_results %>%
    filter(scenario == "observed") %>%
    distinct(region, year, cs_weighted)

  tau_summary <- tau_scenarios %>%
    distinct(region, year, scenario, tau, cs_weighted) %>%
    left_join(cs_obs_val %>% rename(cs_obs = cs_weighted), by = c("region", "year")) %>%
    mutate(delta_cs = cs_weighted - cs_obs) %>%
    group_by(tau) %>%
    summarize(
      mean_delta_cs = mean(delta_cs, na.rm = TRUE),
      mean_premium_change = mean(
        tau_scenarios$premium_change[tau_scenarios$tau == first(tau)], na.rm = TRUE
      ),
      n_cells = length(unique(paste(region, year))),
      .groups = "drop"
    )

  cat("\n")
  print(tau_summary %>%
    mutate(across(where(is.numeric), ~round(., 2))),
    n = Inf
  )

  # Welfare gradient figure
  if (!dir.exists("results/figures")) dir.create("results/figures", recursive = TRUE)

  p_tau <- tau_summary %>%
    ggplot(aes(x = tau, y = mean_delta_cs)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Broker-to-Navigator Substitution Rate",
         y = "Mean Welfare Change ($/month/HH)") +
    scale_x_continuous(breaks = seq(0, 1, 0.25)) +
    theme_bw()
  ggsave("results/figures/cf_welfare_gradient.png", p_tau, width = 6, height = 4)
  cat("  Welfare gradient figure saved.\n")

  cat("\n  Value of assistance (tau=1 vs tau=0):",
      round(tau_summary$mean_delta_cs[tau_summary$tau == 1] -
              tau_summary$mean_delta_cs[tau_summary$tau == 0], 2),
      "$/month/HH\n")
}

cat("\nCounterfactual simulation complete.\n")
