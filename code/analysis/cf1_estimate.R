# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Structural counterfactuals, one year at a time. For each year
##                the 19 region cells sit on their own workers (helpers/
##                cf_cell.R); the master solves the plan-year base premiums and
##                the insurer-year commission scales jointly (helpers/cf_year.R)
##                with the pricing and commission conditions held at their
##                observed-point residuals, so the baseline is the observed
##                premiums and commissions and every scenario is a move away
##                from it. Writes results/counterfactual_results.csv. Sourced
##                by _analysis.R.

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
TAU_GRID       <- c(0, 0.25, 0.5, 0.75, 1.0)   # commission ban, brokers -> navigators
ENDOG_TAU_GRID <- c(0.5, 1.0)                  # navigator expansion, commissions endogenous
DEFUND_GRID    <- c(0.5, 1.0)                  # navigators -> brokers, commissions endogenous
SCALE_GRID     <- c(0.25, 0.5, 0.75)           # commission levels scaled down

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

  # Observed point: the held pricing residuals e_jy, the commission calibration
  # per insurer-year, and the baseline rows
  spec_obs <- list(kind = "exog", comm = "observed", calib = TRUE)
  invisible(parallel::clusterCall(cl, cf_cell_scenario, "baseline", spec_obs))
  pieces_obs <- cf_year_evaluate(cl, P_obs, NULL)
  if (is.null(pieces_obs) || !all(!vapply(pieces_obs[active], is.null, logical(1)))) {
    cat("  observed-point evaluation failed; year skipped\n"); parallel::stopCluster(cl); next
  }
  ag_obs <- cf_year_aggregate(pieces_obs)
  e_target <- ag_obs$G
  N_year <- sum(vapply(inits[active], function(cs) cs$N, numeric(1)))
  # The pricing residual in dollars is the margin correction the commission
  # benefit uses (both conditions see the same margin); re-evaluate the observed
  # point with it in place for the commission calibration
  e_dollars <- ag_obs$G / ag_obs$omega_w
  metal_of <- sr_y %>% distinct(plan_id, metal) %>% { setNames(.$metal, .$plan_id) }
  cat("  pricing residual in dollars per member-month, mean by metal:",
      paste(names(tapply(e_dollars, metal_of[names(e_dollars)], mean)),
            round(tapply(e_dollars, metal_of[names(e_dollars)], mean)), collapse = ", "), "\n")

  # Endogenous-insurer gate at the year level: positive commissions and a broker
  # pool at or above the share floor. The commission condition is MB = (1 - beta) MC
  # (its gap at observed commissions, b_obs, is reported as a fit statistic);
  # mu = -beta in the output's markup convention MB = (1 + mu) MC.
  firms <- names(ag_obs$MC)
  gate <- firms[ag_obs$MC[firms] > 0 & is.finite(ag_obs$MB[firms]) &
                ag_obs$qB[firms] / N_year >= SHARE_FLOOR_FOC]
  beta_y <- BETA_LOOKUP[paste(gate, y, sep = "_")]
  beta_y[is.na(beta_y)] <- mean(BETA_LOOKUP)
  beta_y <- setNames(unname(beta_y), gate)
  mu_y <- -beta_y
  etabar_y <- ag_obs$MC[gate] / ag_obs$qB[gate]
  comm_scale <- 1 - beta_y                       # (1 - beta) by insurer
  endog_native <- function(prefixes) {
    if (length(prefixes) == 0) return(NULL)
    list(prefixes = prefixes, comm_scale = comm_scale[prefixes],
         b_obs = 1 - ag_obs$MB[prefixes] / (comm_scale[prefixes] * ag_obs$MC[prefixes]))
  }
  cat("  commission conditions:", length(gate), "insurers endogenous;",
      "pricing residual at observed premiums |e| =", signif(sqrt(sum(e_target[solve_ids]^2)), 3),
      "; commission gap at observed commissions in [", if (length(gate)) round(min(endog_native(gate)$b_obs), 2) else NA, ",",
      if (length(gate)) round(max(endog_native(gate)$b_obs), 2) else NA, "]\n")

  # Baseline: the model's own equilibrium, residuals at zero. The best-response
  # iteration carries the system from the observed premiums to the fixed point;
  # the year's Jacobian in the base premiums is then computed there (numerical),
  # the baseline is polished with it, and every scenario starts from it.
  rows_y <- list()
  save_rows <- function(label, rows) {
    if (is.null(rows) || nrow(rows) == 0) return(invisible(NULL))
    data.table::fwrite(rows, file.path(CF_YEAR_DIR, sprintf("year_%d_%s.csv", y, label)))
    rows_y[[label]] <<- rows
  }
  zero_target <- setNames(rep(0, length(P_obs)), names(P_obs))
  k_one <- setNames(rep(1, length(gate)), gate)
  invisible(parallel::clusterCall(cl, cf_cell_scenario, "baseline",
                                  if (length(gate)) list(kind = "endog", comm = "observed", prefixes = gate)
                                  else list(kind = "exog", comm = "observed")))
  # Warm start from a saved fixed point of an earlier run of this year, if any
  fp_file <- file.path(CF_YEAR_DIR, sprintf("fixed_point_%d.csv", y))
  P_start <- P_obs; k_start <- k_one
  if (file.exists(fp_file)) {
    fp_saved <- read_csv(fp_file, show_col_types = FALSE)
    P_start[fp_saved$id[fp_saved$kind == "P"]] <- fp_saved$value[fp_saved$kind == "P"]
    kk <- fp_saved[fp_saved$kind == "k", ]; k_start[intersect(kk$id, gate)] <- kk$value[match(intersect(kk$id, gate), kk$id)]
    cat("  baseline warm start from", basename(fp_file), "\n")
  }
  fp <- solve_cf_year_fixed_point(yr, "baseline", solve_ids, P_start, k_start, endog_native(gate))
  if (is.null(fp)) { cat("  baseline iteration failed; year skipped\n"); parallel::stopCluster(cl); next }
  cat(sprintf("  baseline fixed point: %d iterations, converged %s, %.1f min\n", fp$iter, fp$converged, fp$elapsed))
  write_csv(bind_rows(tibble(kind = "P", id = names(fp$P), value = unname(fp$P)),
                      if (length(gate)) tibble(kind = "k", id = names(fp$k), value = unname(fp$k))), fp_file)
  J_P_year <- cf_year_jacobian_P(yr, solve_ids, fp$P, fp$k, zero_target, endog_native(gate), NULL,
                                 residual_scale = 0)
  if (is.null(J_P_year)) { cat("  jacobian evaluation failed; year skipped\n"); parallel::stopCluster(cl); next }
  data.table::fwrite(data.table::data.table(row = rownames(J_P_year), J_P_year),
                     file.path(CF_YEAR_DIR, sprintf("jacobian_%d.csv", y)))

  run_scenario <- function(label, tau, spec, P_init, k_init = NULL, endog = NULL, set_scenario = TRUE) {
    if (set_scenario) invisible(parallel::clusterCall(cl, cf_cell_scenario, label, spec))
    res <- solve_cf_year(yr, label, solve_ids, P_init, k_init, zero_target, endog, NULL,
                         residual_scale = 0, J_P = J_P_year, tol_dollars = 5, tol_b = 0.025)
    if (is.null(res)) { cat("  ", label, "- did not converge\n"); return(NULL) }
    P_full <- P_obs; P_full[names(res$P)] <- res$P
    cat(sprintf("   %s - converged (termcd %d, %d iterations, %d evaluations, %.1f min)\n",
                label, res$sol$termcd, res$sol$iter, res$n_eval, res$elapsed))
    save_rows(label, cf_year_rows(yr, label, tau, res$pieces, P_full, res$k, endog, mu_y,
                                  res$sol$termcd, res$sol$iter))
    list(P = P_full, k = res$k)
  }

  # Baseline polish with the Jacobian at the fixed point (the scenario is
  # already set on the workers)
  base <- run_scenario("baseline", NA_real_, NULL, fp$P, fp$k, endog_native(gate), set_scenario = FALSE)
  if (is.null(base)) { cat("  baseline did not converge; year skipped\n"); parallel::stopCluster(cl); next }
  P_base <- base$P
  k_base <- if (length(gate)) setNames(unname(base$k[gate]), gate) else NULL
  gap <- P_base[names(P_obs)] - P_obs
  cat("  model baseline vs observed base premium, mean by metal ($):",
      paste(names(tapply(gap, metal_of[names(gap)], mean)),
            round(tapply(gap, metal_of[names(gap)], mean)), collapse = ", "), "\n")
  if (length(gate)) cat("  model baseline commission scale vs observed:",
                        paste(gate, round(k_base, 2), collapse = " "), "\n")

  # Commission ban with the broker-to-navigator gradient (chained warm starts)
  P_warm <- P_base
  for (tau in TAU_GRID) {
    out <- run_scenario(paste0("zero_tau", sprintf("%.2f", tau)), tau,
                        list(kind = "exog", comm = "zero", tau = tau), P_warm)
    if (!is.null(out)) P_warm <- out$P
  }
  run_scenario("uniform", NA_real_, list(kind = "exog", comm = "uniform"), P_base)
  for (sc in SCALE_GRID)
    run_scenario(paste0("scale_", sprintf("%.2f", sc)), NA_real_,
                 list(kind = "exog", comm = "scale", sc = sc), P_base)
  run_scenario("aligned", NA_real_, list(kind = "exog", comm = "aligned"), P_base)

  # Navigator expansion with endogenous commissions: the endogenous set is
  # re-gated on the scenario's broker pool at the warm point
  if (length(gate) > 0) {
    P_e <- P_base; k_e <- k_base
    for (tau in ENDOG_TAU_GRID) {
      label <- paste0("endog_tau", sprintf("%.2f", tau))
      spec <- list(kind = "endog", comm = "observed", tau = tau, broker_remain = TRUE, prefixes = gate)
      invisible(parallel::clusterCall(cl, cf_cell_scenario, label, spec))
      pz <- cf_year_evaluate(cl, P_e, k_e)
      agz <- cf_year_aggregate(pz)
      gate_s <- gate[!is.na(agz$qB[gate]) & agz$qB[gate] / N_year >= SHARE_FLOOR_FOC]
      if (length(gate_s) > 0) {
        spec$prefixes <- gate_s
        out <- run_scenario(label, tau, spec, P_e, k_e[gate_s], endog_native(gate_s))
      } else {
        out <- run_scenario(label, tau, list(kind = "exog", comm = "observed", tau = tau,
                                             broker_remain = TRUE), P_e)
      }
      if (!is.null(out)) { P_e <- out$P; if (!is.null(out$k)) k_e[names(out$k)] <- out$k }
    }

    # Flat-fee mandate: every endogenous insurer re-chooses a flat dollar level
    # (basis $1 per plan, so k is the level; start at the observed mean commission)
    run_scenario("flat_mandate", NA_real_,
                 list(kind = "endog", comm = "flat", prefixes = gate),
                 P_base, etabar_y[gate] * k_base, endog_native(gate))

    # Navigator defunding: navigators become brokers, commissions endogenous
    P_d <- P_base; k_d <- k_base
    for (df in DEFUND_GRID) {
      out <- run_scenario(paste0("defund_", sprintf("%.2f", df)), NA_real_,
                          list(kind = "endog", comm = "observed", defund = df, prefixes = gate),
                          P_d, k_d, endog_native(gate))
      if (!is.null(out)) { P_d <- out$P; if (!is.null(out$k)) k_d[names(out$k)] <- out$k }
    }
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
  filter(str_detect(scenario, "^endog_tau|^defund_") | scenario == "flat_mandate")
if (nrow(endog_scenarios) > 0) {
  cat("\n--- Endogenous-Commission Scenarios: solved commission scale ---\n")
  cat("    comm_scale = share-weighted mean solved k of endogenous insurers (1 = observed schedule)\n")
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
