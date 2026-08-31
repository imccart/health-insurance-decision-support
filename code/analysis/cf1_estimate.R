# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Structural counterfactuals, one year at a time. For each year
##                the 19 region cells sit on their own workers (helpers/
##                cf_cell.R); the master solves the plan-year base premiums to
##                the model's premium equilibrium (helpers/cf_year.R) with
##                commissions exogenous in every scenario. The baseline holds
##                the observed schedules; scenarios with a commission response
##                run at the observed schedules (the point) and at the
##                revealed-preference band edges (0.75x and 1.25x), which
##                bound the response. Writes results/counterfactual_results.csv.
##                Sourced by _analysis.R.

# PHASE 1: Load counterfactual-specific data ------------------------------

cat("\nPhase 1: Loading counterfactual data...\n")

# cells, cell_seeds, plan_choice, commission_lookup loaded by s1_inputs.R

coefs <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE)
if (!"region_factor" %in% names(supply_results))
  stop("supply_results.csv lacks base_premium / region_factor: re-run s3_pricing.R")

rs_coefs_df <- read_csv(file.path(TEMP_DIR, "ra_rs_coefs_gmm.csv"), show_col_types = FALSE)
claims_coefs_df <- read_csv(file.path(TEMP_DIR, "ra_claims_coefs_gmm.csv"), show_col_types = FALSE)
reins_df <- read_csv(file.path(TEMP_DIR, "reinsurance_factors.csv"), show_col_types = FALSE)
rs_coefs <- setNames(rs_coefs_df$estimate, rs_coefs_df$term)
claims_coefs <- setNames(claims_coefs_df$estimate, claims_coefs_df$term)

# Insurers' administrative cost per member (MLR, data-build step 9) and the
# administrative saving per commission dollar beta (s4); both enter marginal cost
mlr_admin <- read_csv("data/output/mlr_admin.csv", show_col_types = FALSE)
ADMIN_LOOKUP <- setNames(mlr_admin$admin0_pmpm, paste(mlr_admin$insurer_prefix, mlr_admin$year, sep = "_"))
beta_df <- read_csv(file.path(TEMP_DIR, "commission_beta.csv"), show_col_types = FALSE)
BETA_LOOKUP <- setNames(beta_df$beta, paste(beta_df$firm, beta_df$year, sep = "_"))
cat("  beta (administrative saving per commission dollar) by insurer-year:", length(BETA_LOOKUP),
    "values, range", round(min(BETA_LOOKUP), 3), "to", round(max(BETA_LOOKUP), 3), "\n")

demand_spec <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
# Full spec (base + assisted): the price-interaction machinery must see the
# channel-specific premium slopes
STRUCTURAL_SPEC <- demand_spec$all

# Scenario grids
TAU_GRID       <- c(0, 0.25, 0.5, 0.75, 1.0)   # commission ban, agents -> navigators
ENDOG_TAU_GRID <- c(0.5, 1.0)                  # navigator expansion, commissions at observed
DEFUND_GRID    <- c(0.5, 1.0)                  # navigators -> agents, commissions at observed
SCALE_GRID     <- c(0.25, 0.5, 0.75)           # commission levels scaled down
BAND_EDGES     <- c(0.75, 1.25)                # band-edge runs bounding the commission response

CF_YEAR_DIR <- file.path(TEMP_DIR, "cf_years")
if (dir.exists(CF_YEAR_DIR)) unlink(CF_YEAR_DIR, recursive = TRUE)
dir.create(CF_YEAR_DIR, recursive = TRUE)

cat("  Loading HH data for counterfactuals...\n")
hh_all <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split_cf <- split(hh_all, by = c("region", "year"))
rm(hh_all); gc(verbose = FALSE)

years <- sort(unique(cells$year))
cat("  Years:", paste(years, collapse = ", "), "; cells:", nrow(cells), "\n")

# =========================================================================
# PHASE 2: solve each year
# =========================================================================

cat("\nPhase 2: Running counterfactual simulations...\n")
t_start <- Sys.time()
year_results <- list()

for (y in years) {
  cat(sprintf("\n=== Year %d ===\n", y))
  idx_y <- which(cells$year == y)
  tasks <- lapply(idx_y, function(i) {
    hhs <- hh_split_cf[[paste0(cells$region[i], ".", cells$year[i])]]
    list(r = cells$region[i], y = cells$year[i], seed = cell_seeds[i],
         hhs = if (is.null(hhs) || nrow(hhs) == 0) NULL else as.data.frame(hhs))
  })

  # One worker per cell; each keeps its cell's state for the whole year
  cl <- parallel::makeCluster(length(tasks), type = "PSOCK", outfile = "")
  parallel::clusterEvalQ(cl, {
    suppressMessages({ library(tidyverse); library(data.table); library(nleqslv) })
    source("code/data-build/_helpers.R")
    source("code/analysis/helpers/constants.R")
    source("code/analysis/helpers/covariates.R")
    source("code/analysis/helpers/choice.R")
    source("code/analysis/helpers/supply.R")
    source("code/analysis/helpers/ra.R")
    source("code/analysis/helpers/estimate_demand.R")
    source("code/analysis/helpers/cf_cell.R")
    data.table::setDTthreads(1)
  })
  parallel::clusterExport(cl, c("SAMPLE_FRAC", "plan_choice", "supply_results", "coefs",
    "commission_lookup", "rs_coefs", "claims_coefs", "reins_df", "STRUCTURAL_SPEC",
    "ADMIN_LOOKUP", "BETA_LOOKUP"))
  t_init <- Sys.time()
  inits <- parallel::clusterApply(cl, tasks, function(task) {
    if (is.null(task$hhs)) return(NULL)
    tryCatch(cf_cell_init(task$r, task$y, task$seed, SAMPLE_FRAC, task$hhs,
                          plan_choice, supply_results, coefs, commission_lookup,
                          rs_coefs, claims_coefs, reins_df, STRUCTURAL_SPEC,
                          ADMIN_LOOKUP, BETA_LOOKUP),
             error = function(e) { cat("  init error cell", task$r, task$y, ":", conditionMessage(e), "\n"); NULL })
  })
  active <- !vapply(inits, is.null, logical(1))
  cat(sprintf("  cells initialized: %d of %d (%.1f min)\n", sum(active), length(tasks),
              as.numeric(difftime(Sys.time(), t_init, units = "mins"))))
  if (!any(active)) { parallel::stopCluster(cl); next }
  yr <- list(y = y, cl = cl, cells = inits, active = active)

  # Plan-year base premiums (observed) and the plans priced in the solve: those
  # whose year-level member-weighted share is at least the share floor
  sr_y <- supply_results %>% filter(year == y)
  P_obs <- sr_y %>% group_by(plan_id) %>% summarize(base_premium = first(base_premium), .groups = "drop") %>%
    { setNames(.$base_premium, .$plan_id) }
  share_y <- bind_rows(lapply(inits[active], function(cs)
    tibble(plan_id = cs$plan_ids, N = cs$N, s = unname(cs$share_obs)))) %>%
    group_by(plan_id) %>% summarize(share = sum(N * s) / sum(N), .groups = "drop")
  solve_ids <- share_y$plan_id[share_y$share >= SHARE_FLOOR_FOC & share_y$plan_id %in% names(P_obs)]
  cat("  plan-years:", length(P_obs), "; priced in the solve:", length(solve_ids), "\n")

  # Observed point: the pricing-residual fit diagnostic and the insurers'
  # observed mean commissions
  spec_obs <- list(comm = "observed", calib = TRUE)
  invisible(parallel::clusterCall(cl, cf_cell_scenario, "baseline", spec_obs))
  pieces_obs <- cf_year_evaluate(cl, P_obs, NULL)
  if (is.null(pieces_obs) || !all(!vapply(pieces_obs[active], is.null, logical(1)))) {
    cat("  observed-point evaluation failed; year skipped\n"); parallel::stopCluster(cl); next
  }
  ag_obs <- cf_year_aggregate(pieces_obs)
  e_target <- ag_obs$G
  N_year <- sum(vapply(inits[active], function(cs) cs$N, numeric(1)))
  e_dollars <- ag_obs$G / ag_obs$omega_w
  metal_of <- sr_y %>% distinct(plan_id, metal) %>% { setNames(.$metal, .$plan_id) }
  cat("  pricing residual in dollars per member-month, mean by metal:",
      paste(names(tapply(e_dollars, metal_of[names(e_dollars)], mean)),
            round(tapply(e_dollars, metal_of[names(e_dollars)], mean)), collapse = ", "), "\n")

  # Insurers with commissions in the year: positive outlay and an agent pool at
  # or above the share floor. Their observed mean commission per agent member
  # sets the flat-mandate level.
  firms <- names(ag_obs$MC)
  gate <- firms[ag_obs$MC[firms] > 0 & is.finite(ag_obs$MB[firms]) &
                ag_obs$qB[firms] / N_year >= SHARE_FLOOR_FOC]
  etabar_y <- ag_obs$MC[gate] / ag_obs$qB[gate]
  cat("  pricing residual at observed premiums |e| =",
      signif(sqrt(sum(e_target[solve_ids]^2)), 3), ";",
      length(gate), "insurers with commissions\n")

  # Baseline: the model's premium equilibrium at the observed commissions. The
  # best-response iteration carries the system from the observed premiums to
  # the fixed point; the year's Jacobian in the base premiums is then computed
  # there (numerical), the baseline is polished with it, and every scenario
  # starts from it.
  rows_y <- list()
  save_rows <- function(label, rows) {
    if (is.null(rows) || nrow(rows) == 0) return(invisible(NULL))
    data.table::fwrite(rows, file.path(CF_YEAR_DIR, sprintf("year_%d_%s.csv", y, label)))
    rows_y[[label]] <<- rows
  }
  invisible(parallel::clusterCall(cl, cf_cell_scenario, "baseline",
                                  list(comm = "observed")))
  # Warm start from a saved fixed point of an earlier run of this year, if any
  fp_file <- file.path(CF_YEAR_DIR, sprintf("fixed_point_%d.csv", y))
  P_start <- P_obs
  if (file.exists(fp_file)) {
    fp_saved <- read_csv(fp_file, show_col_types = FALSE)
    P_start[fp_saved$id[fp_saved$kind == "P"]] <- fp_saved$value[fp_saved$kind == "P"]
    cat("  baseline warm start from", basename(fp_file), "\n")
  }
  fp <- solve_cf_year_fixed_point(yr, "baseline", solve_ids, P_start)
  if (is.null(fp)) { cat("  baseline iteration failed; year skipped\n"); parallel::stopCluster(cl); next }
  cat(sprintf("  baseline fixed point: %d iterations, converged %s, %.1f min\n", fp$iter, fp$converged, fp$elapsed))
  write_csv(tibble(kind = "P", id = names(fp$P), value = unname(fp$P)), fp_file)
  J_P_year <- cf_year_jacobian_P(yr, solve_ids, fp$P)
  if (is.null(J_P_year)) { cat("  jacobian evaluation failed; year skipped\n"); parallel::stopCluster(cl); next }
  data.table::fwrite(data.table::data.table(row = rownames(J_P_year), J_P_year),
                     file.path(CF_YEAR_DIR, sprintf("jacobian_%d.csv", y)))

  run_scenario <- function(label, tau, spec, P_init, comm_scale = 1, set_scenario = TRUE) {
    if (set_scenario) invisible(parallel::clusterCall(cl, cf_cell_scenario, label, spec))
    res <- solve_cf_year(yr, label, solve_ids, P_init, J_P_year, tol_dollars = 5)
    if (is.null(res)) { cat("  ", label, "- did not converge\n"); return(NULL) }
    P_full <- P_obs; P_full[names(res$P)] <- res$P
    cat(sprintf("   %s - converged (termcd %d, %d iterations, %d evaluations, %.1f min)\n",
                label, res$sol$termcd, res$sol$iter, res$n_eval, res$elapsed))
    save_rows(label, cf_year_rows(yr, label, tau, res$pieces, P_full, comm_scale,
                                  res$sol$termcd, res$sol$iter))
    list(P = P_full)
  }

  # Baseline polish with the Jacobian at the fixed point (the scenario is
  # already set on the workers)
  base <- run_scenario("baseline", NA_real_, NULL, fp$P, set_scenario = FALSE)
  if (is.null(base)) { cat("  baseline did not converge; year skipped\n"); parallel::stopCluster(cl); next }
  P_base <- base$P
  gap <- P_base[names(P_obs)] - P_obs
  cat("  model baseline vs observed base premium, mean by metal ($):",
      paste(names(tapply(gap, metal_of[names(gap)], mean)),
            round(tapply(gap, metal_of[names(gap)], mean)), collapse = ", "), "\n")

  # Commission ban with the agent-to-navigator gradient (chained warm starts)
  P_warm <- P_base
  for (tau in TAU_GRID) {
    out <- run_scenario(paste0("zero_tau", sprintf("%.2f", tau)), tau,
                        list(comm = "zero", tau = tau), P_warm, comm_scale = 0)
    if (!is.null(out)) P_warm <- out$P
  }
  run_scenario("uniform", NA_real_, list(comm = "uniform"), P_base,
               comm_scale = NA_real_)
  for (sc in SCALE_GRID)
    run_scenario(paste0("scale_", sprintf("%.2f", sc)), NA_real_,
                 list(comm = "scale", sc = sc), P_base, comm_scale = sc)
  run_scenario("aligned", NA_real_, list(comm = "aligned"), P_base,
               comm_scale = NA_real_)

  # Scenarios with a commission response: the point run holds the observed (or
  # mandated) schedules, and the band-edge runs, the whole schedule at each
  # multiplier in BAND_EDGES, bound the insurers' response within the
  # revealed-preference bands
  run_banded <- function(label, tau, spec_point, P_init) {
    out <- run_scenario(label, tau, spec_point, P_init)
    for (bk in BAND_EDGES) {
      spec_b <- spec_point
      if (identical(spec_b$comm, "flatbar")) spec_b$levels <- spec_point$levels * bk
      else { spec_b$comm <- "scale"; spec_b$sc <- bk }
      run_scenario(paste0(label, "_k", sprintf("%.2f", bk)), tau, spec_b,
                   if (!is.null(out)) out$P else P_init, comm_scale = bk)
    }
    out
  }

  # Navigator expansion: a fraction tau of agent-assisted households become
  # navigator-assisted, the rest stay agent-assisted
  P_e <- P_base
  for (tau in ENDOG_TAU_GRID) {
    out <- run_banded(paste0("endog_tau", sprintf("%.2f", tau)), tau,
                      list(comm = "observed", tau = tau, broker_remain = TRUE), P_e)
    if (!is.null(out)) P_e <- out$P
  }

  # Flat-fee mandate: every insurer with commissions pays its observed mean
  # commission per agent member as a flat fee (a budget-neutral level)
  if (length(gate) > 0)
    run_banded("flat_mandate", NA_real_,
               list(comm = "flatbar", levels = etabar_y), P_base)

  # Navigator defunding: navigators become agent-assisted at the observed schedules
  P_d <- P_base
  for (df in DEFUND_GRID) {
    out <- run_banded(paste0("defund_", sprintf("%.2f", df)), NA_real_,
                      list(comm = "observed", defund = df), P_d)
    if (!is.null(out)) P_d <- out$P
  }

  parallel::stopCluster(cl)
  year_results[[as.character(y)]] <- bind_rows(rows_y)
  cat(sprintf("  year %d done: %d scenarios, %.1f min elapsed overall\n", y, length(rows_y),
              as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
}
rm(hh_split_cf); gc(verbose = FALSE)

# =========================================================================
# PHASE 3: Collect and write results
# =========================================================================

cat("\nPhase 3: Writing results...\n")
cf_results <- bind_rows(year_results)
if (nrow(cf_results) == 0) stop("No counterfactual results")
write_csv(cf_results, "results/counterfactual_results.csv")
cat("  Written", nrow(cf_results), "rows to results/counterfactual_results.csv\n")

# =========================================================================
# PHASE 4: Summary
# =========================================================================

cat("\n--- Counterfactual Summary (equilibria only; welfare is scored in cf2) ---\n")
scen_summary <- cf_results %>%
  group_by(scenario) %>%
  summarize(n_cells = length(unique(paste(region, year))),
            mean_premium_change = mean(premium_change, na.rm = TRUE),
            converged_pct = 100 * mean(nleqslv_termcd <= 2, na.rm = TRUE),
            .groups = "drop")
cat("\n"); print(scen_summary %>% mutate(across(where(is.numeric), ~round(., 2))), n = Inf)

endog_scenarios <- cf_results %>%
  filter(str_detect(scenario, "^endog_tau|^defund_|^flat_mandate"))
if (nrow(endog_scenarios) > 0) {
  cat("\n--- Commission-response scenarios: point runs and band edges ---\n")
  cat("    comm_scale = the commission multiplier applied (1 = the point run at observed schedules)\n")
  endog_summary <- endog_scenarios %>%
    group_by(scenario) %>%
    summarize(comm_scale = if (all(is.na(comm_scale_cf))) NA_real_ else
                weighted.mean(comm_scale_cf, share_cf, na.rm = TRUE),
              mean_comm = weighted.mean(commission_pmpm, share_cf, na.rm = TRUE),
              mean_premium_change = mean(premium_change, na.rm = TRUE),
              converged_pct = 100 * mean(nleqslv_termcd <= 2, na.rm = TRUE),
              .groups = "drop")
  cat("\n"); print(endog_summary %>% mutate(across(where(is.numeric), ~round(., 3))), n = Inf)
}

cat("\nCounterfactual simulation complete.\n")
