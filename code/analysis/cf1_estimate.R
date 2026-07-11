# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Structural counterfactuals: loops run_cf_cell() (helpers/cf_cell.R,
##                loaded in the preamble) over all region-year cells in parallel,
##                then writes results and the summary. Sourced by _analysis.R.

# =========================================================================
# DRIVER: loop run_cf_cell() (defined above) over all region-year cells.
# Loads data once, then runs each cell in-process.
# =========================================================================

# PHASE 1: Load counterfactual-specific data ------------------------------

cat("\nPhase 1: Loading counterfactual data...\n")

# hh_split, cells, cell_seeds, plan_choice, commission_lookup loaded by _supply.R (via _inputs.R)

coefs <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE)

# Cost parameters from GMM
rs_coefs_df <- read_csv(file.path(TEMP_DIR, "ra_rs_coefs_gmm.csv"), show_col_types = FALSE)
claims_coefs_df <- read_csv(file.path(TEMP_DIR, "ra_claims_coefs_gmm.csv"), show_col_types = FALSE)
reins_df <- read_csv(file.path(TEMP_DIR, "reinsurance_factors.csv"), show_col_types = FALSE)

rs_coefs <- setNames(rs_coefs_df$estimate, rs_coefs_df$term)
claims_coefs <- setNames(claims_coefs_df$estimate, claims_coefs_df$term)

demand_spec <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
# Full spec (base + assisted): STRUCTURAL_SPEC here only feeds the price-interaction
# machinery, which must see assisted_premium / broker_premium (assisted group) so
# the channel-specific price slopes enter alpha_i and the equilibrium recompute.
STRUCTURAL_SPEC <- demand_spec$all

# Welfare engine (objective money-metric helpers + CA cost-sharing table). Loaded
# for the main session and the serial fallback; parallel workers source these in
# clusterEvalQ below and receive CS_TABLE via clusterExport.
source("code/analysis/helpers/welfare_objective.R")
source("code/analysis/helpers/welfare_engine.R")
CS_TABLE <- read.csv("data/input/ca_standard_cost_sharing.csv", stringsAsFactors = FALSE)

cat("  Region-year cells:", nrow(cells), "\n")

# =========================================================================
# PHASE 2: Run counterfactuals
# =========================================================================

# Load HH data fresh (hh_split freed by _supply.R to save memory)
cat("  Loading HH data for counterfactuals...\n")
hh_all <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split_cf <- split(hh_all, by = c("region", "year"))
rm(hh_all); gc(verbose = FALSE)

cat("\nPhase 2: Running counterfactual simulations...\n")

# Build one task per cell, each carrying its own household slice, so parallel
# workers never hold the full household panel (memory stays flat per worker).
n_cells_total <- nrow(cells)
tasks <- lapply(seq_len(n_cells_total), function(i) {
  hhs <- hh_split_cf[[paste0(cells$region[i], ".", cells$year[i])]]
  list(r = cells$region[i], y = cells$year[i], seed = cell_seeds[i],
       idx = i, n_total = n_cells_total,
       hhs = if (is.null(hhs) || nrow(hhs) == 0) NULL else as.data.frame(hhs))
})
rm(hh_split_cf); gc(verbose = FALSE)

# Per-cell results are written here as each cell finishes, so a run is observable
# (watch the directory fill) and survives a kill (completed cells stay on disk).
# Cleaned each run so nothing stale is combined.
CF_CELL_DIR <- file.path(TEMP_DIR, "cf_cells")
if (dir.exists(CF_CELL_DIR)) unlink(CF_CELL_DIR, recursive = TRUE)
dir.create(CF_CELL_DIR, recursive = TRUE)

# Solve one cell, write its result, and print a one-line progress summary (cell
# index, rows, share of scenarios converged, wall seconds). With the cluster's
# outfile="" these stream to the console live, so the run is not a blank cursor and
# a slow or non-converging cell is visible as it happens. Returns NULL for empty or
# failed cells.
run_one_cf <- function(task) {
  if (is.null(task$hhs)) return(NULL)
  t0 <- Sys.time()
  res <- tryCatch(
    run_cf_cell(task$r, task$y, task$seed, SAMPLE_FRAC, task$hhs,
                plan_choice, supply_results, coefs, commission_lookup,
                rs_coefs, claims_coefs, reins_df, STRUCTURAL_SPEC),
    error = function(e) { cat(sprintf("  [cell %d/%d] r%s y%s ERROR: %s\n",
      task$idx, task$n_total, task$r, task$y, conditionMessage(e))); NULL }
  )
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (!is.null(res)) {
    data.table::fwrite(res, file.path(CF_CELL_DIR, sprintf("cell_%s_%s.csv", task$r, task$y)))
    conv <- if ("nleqslv_termcd" %in% names(res))
      round(100 * mean(res$nleqslv_termcd <= 2, na.rm = TRUE)) else NA_real_
    cat(sprintf("  [cell %d/%d] r%s y%s: %d rows, %s%% conv, %.0fs\n",
      task$idx, task$n_total, task$r, task$y, nrow(res), conv, el))
  }
  res
}

t_start <- Sys.time()

# Run cells in parallel (PSOCK, load-balanced); fall back to serial if the
# cluster can't be created. Each worker sources the helpers and receives
# run_cf_cell plus the small reference objects; household data rides with the task.
n_workers <- max(1L, parallel::detectCores() - 2L)
# outfile = "" streams each worker's per-cell progress line to the console so the
# run reports live instead of sitting on a blank cursor.
cl <- tryCatch(parallel::makeCluster(n_workers, type = "PSOCK", outfile = ""),
               error = function(e) NULL)

if (!is.null(cl)) {
  cat("  Parallel:", n_workers, "workers (per-cell progress streams below)\n")
  parallel::clusterEvalQ(cl, {
    suppressMessages({ library(tidyverse); library(data.table); library(nleqslv) })
    source("code/data-build/_helpers.R")
    source("code/analysis/helpers/constants.R")
    source("code/analysis/helpers/covariates.R")
    source("code/analysis/helpers/choice.R")
    source("code/analysis/helpers/supply.R")
    source("code/analysis/helpers/ra.R")
    source("code/analysis/helpers/estimate_demand.R")
    source("code/analysis/helpers/welfare_objective.R")
    source("code/analysis/helpers/welfare_engine.R")
    data.table::setDTthreads(1)
  })
  parallel::clusterExport(cl, c("run_cf_cell", "SAMPLE_FRAC", "plan_choice",
    "supply_results", "coefs", "commission_lookup", "rs_coefs", "claims_coefs",
    "reins_df", "STRUCTURAL_SPEC", "CS_TABLE", "CF_CELL_DIR"))
  results_list <- parallel::parLapplyLB(cl, tasks, run_one_cf)
  parallel::stopCluster(cl)
} else {
  cat("  Cluster unavailable — running serial\n")
  results_list <- lapply(tasks, run_one_cf)
}

n_success <- sum(vapply(results_list, Negate(is.null), logical(1)))
n_fail    <- length(results_list) - n_success
elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
cat(sprintf("\nComplete: %d success, %d failed (%.1f min total)\n",
            n_success, n_fail, elapsed_total))

# =========================================================================
# PHASE 3: Collect and write results
# =========================================================================

cat("\nPhase 3: Writing results...\n")

cf_results <- bind_rows(results_list)
rm(results_list)

if (nrow(cf_results) == 0) {
  stop("No counterfactual results — all cells failed")
}

write_csv(cf_results, "results/counterfactual_results.csv")
cat("  Written", nrow(cf_results), "rows to results/counterfactual_results.csv\n")

# =========================================================================
# PHASE 4: Summary
# =========================================================================

cat("\n--- Counterfactual Summary ---\n")

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
}

# Tau gradient summary
tau_scenarios <- cf_results %>% filter(str_detect(scenario, "^zero_tau"))
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

# Commission-design counterfactuals (level sweep + aligned), vs observed
comm_scenarios <- cf_results %>% filter(scenario == "aligned" | str_detect(scenario, "^scale_"))
if (nrow(comm_scenarios) > 0) {
  cat("\n--- Commission-Design Counterfactuals (vs observed) ---\n")
  cat("    delta_cs = full welfare; delta_cs_nc = welfare excluding commission utility\n")

  obs_cs <- cf_results %>%
    filter(scenario == "observed") %>%
    distinct(region, year, cs_weighted, cs_nocomm) %>%
    rename(cs_obs = cs_weighted, cs_obs_nc = cs_nocomm)

  cs_summary <- comm_scenarios %>%
    distinct(region, year, scenario, cs_weighted, cs_nocomm) %>%
    left_join(obs_cs, by = c("region", "year")) %>%
    mutate(delta_cs = cs_weighted - cs_obs,
           delta_cs_nc = cs_nocomm - cs_obs_nc) %>%
    group_by(scenario) %>%
    summarize(mean_delta_cs = mean(delta_cs, na.rm = TRUE),
              mean_delta_cs_nc = mean(delta_cs_nc, na.rm = TRUE),
              n_cells = n(), .groups = "drop")

  prem_summary <- comm_scenarios %>%
    group_by(scenario) %>%
    summarize(mean_premium_change = mean(premium_change, na.rm = TRUE), .groups = "drop")

  comm_summary <- cs_summary %>% left_join(prem_summary, by = "scenario")
  cat("\n")
  print(comm_summary %>% mutate(across(where(is.numeric), ~round(., 2))), n = Inf)
}

# Welfare at actual choices (navigator vs objective benchmark), mean change vs observed
if ("cs_welfare_obj" %in% names(cf_results)) {
  cat("\n--- Welfare at actual choices (mean change vs observed) ---\n")
  cat("    nav = navigator-informed V^N ($, utils / informed alpha); obj = objective money metric ($)\n")
  wbase <- cf_results %>%
    filter(scenario == "observed") %>%
    distinct(region, year, cs_welfare_nav, cs_welfare_obj) %>%
    rename(nav0 = cs_welfare_nav, obj0 = cs_welfare_obj)
  wsum <- cf_results %>%
    filter(!is.na(cs_welfare_obj)) %>%
    distinct(region, year, scenario, tau, cs_welfare_nav, cs_welfare_obj) %>%
    left_join(wbase, by = c("region", "year")) %>%
    mutate(d_nav = cs_welfare_nav - nav0, d_obj = cs_welfare_obj - obj0) %>%
    group_by(scenario, tau) %>%
    summarize(d_nav = mean(d_nav, na.rm = TRUE), d_obj = mean(d_obj, na.rm = TRUE),
              n_cells = n(), .groups = "drop") %>%
    arrange(scenario, tau)
  cat("\n")
  print(wsum %>% mutate(across(where(is.numeric), ~round(., 2))), n = Inf)
}

cat("\nCounterfactual simulation complete.\n")
