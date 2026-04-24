# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Date Edited:   2026-04-12
## Description:   Counterfactual simulation. Loads all data once, then calls
##                run_cf_cell() in-process for each region-year cell.

# Setup (packages and helpers already loaded by _structural.R) ---------------

# Tuning parameters --------------------------------------------------------
SAMPLE_FRAC   <- as.numeric(Sys.getenv("SAMPLE_FRAC"))
MASTER_SEED   <- as.integer(Sys.getenv("MASTER_SEED"))
TEMP_DIR      <- Sys.getenv("TEMP_DIR")

# =========================================================================
# PHASE 1: Load counterfactual-specific data
# =========================================================================

cat("\nPhase 1: Loading counterfactual data...\n")

# hh_split, cells, cell_seeds, plan_choice, commission_lookup loaded by _structural.R
# cf_worker.R (run_cf_cell function) loaded by _analysis.R

coefs <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE)

# Cost parameters from GMM
rs_coefs_df <- read_csv(file.path(TEMP_DIR, "ra_rs_coefs_gmm.csv"), show_col_types = FALSE)
claims_coefs_df <- read_csv(file.path(TEMP_DIR, "ra_claims_coefs_gmm.csv"), show_col_types = FALSE)
reins_df <- read_csv(file.path(TEMP_DIR, "reinsurance_factors.csv"), show_col_types = FALSE)

rs_coefs <- setNames(rs_coefs_df$estimate, rs_coefs_df$term)
claims_coefs <- setNames(claims_coefs_df$estimate, claims_coefs_df$term)

demand_spec <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
STRUCTURAL_SPEC <- demand_spec$base

cat("  Region-year cells:", nrow(cells), "\n")

# =========================================================================
# PHASE 2: Run counterfactuals
# =========================================================================

# Load HH data fresh (hh_split freed by _structural.R to save memory)
cat("  Loading HH data for counterfactuals...\n")
hh_all <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split_cf <- split(hh_all, by = c("region", "year"), keep.by = FALSE)
rm(hh_all); gc(verbose = FALSE)

cat("\nPhase 2: Running counterfactual simulations...\n")

results_list <- vector("list", nrow(cells))
n_success <- 0L
n_fail    <- 0L
failed_cells <- character(0)
t_start <- Sys.time()

for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]

  cell_key <- paste0(r, ".", y)
  hhs_raw <- hh_split_cf[[cell_key]]
  if (is.null(hhs_raw) || nrow(hhs_raw) == 0) {
    n_fail <- n_fail + 1L
    failed_cells <- c(failed_cells, paste0(r, "_", y))
    next
  }
  hhs_raw <- as.data.frame(hhs_raw)

  cell_result <- tryCatch(
    run_cf_cell(r, y, cell_seeds[i], SAMPLE_FRAC, hhs_raw,
                plan_choice, supply_results, coefs,
                commission_lookup, rs_coefs, claims_coefs,
                reins_df, STRUCTURAL_SPEC),
    error = function(e) {
      cat("  ERROR in cell", r, y, ":", conditionMessage(e), "\n")
      NULL
    }
  )
  rm(hhs_raw)

  if (!is.null(cell_result)) {
    results_list[[i]] <- cell_result
    n_success <- n_success + 1L
  } else {
    n_fail <- n_fail + 1L
    failed_cells <- c(failed_cells, paste0(r, "_", y))
  }

  gc(verbose = FALSE)
  if (i %% 5 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
    cat(sprintf("  Progress: %d/%d cells (%.1f min elapsed)\n", i, nrow(cells), elapsed))
  }
}

rm(hh_split_cf)
gc(verbose = FALSE)

elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
cat(sprintf("\nComplete: %d success, %d failed (%.1f min total)\n",
            n_success, n_fail, elapsed_total))

if (n_fail > 0) {
  cat("  Failed cells:", paste(failed_cells, collapse = ", "), "\n")
}

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

cat("\nCounterfactual simulation complete.\n")
