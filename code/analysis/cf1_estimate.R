# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Structural counterfactuals, one year at a time. For each year
##                the 19 region cells sit on their own workers (helpers/
##                cf_cell.R); the master solves the plan-year base premiums to
##                the model's premium equilibrium (helpers/cf_year.R). The
##                baseline is the model's own equilibrium in BOTH margins:
##                premiums from the pricing conditions and each insurer's
##                commission from the estimated condition MB/MC = (1 - beta_f)
##                + wedge_f (solve_cf_commissions), with the baseline-versus-
##                observed schedules persisted as the fit check
##                (results/cf_baseline_commissions.csv). Scenarios per year:
##                where the rate is the policy (the zero-commission removal
##                gradient over TAU_GRID, the headline low-uniform commission
##                with band-edge runs, one scaled-schedule point, aligned, the
##                flat-fee mandate) the schedule is imposed and premiums
##                re-solve; where the shock changes who is agent-assisted
##                (navigator defunding, navigator expansion endog_tau) the
##                affected insurers re-solve their rates from the same
##                estimated condition. Every scenario solve is gated on the
##                scenario's own shares, scaled by the baseline own-price
##                terms, and pre-iterated when the start is far from its
##                equilibrium. Writes results/counterfactual_results.csv.
##                Sourced by _analysis.R.

# PHASE 1: Load counterfactual-specific data ------------------------------

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

# The estimated cross-market wedge (s4's M4): the commission condition each
# re-solving insurer satisfies is MB/MC = (1 - beta_f) + delta_f + delta1 x
# leverage, leverage = (1 - w)/w with w the on-exchange share of the carrier's
# individual book (data-build step 10; the exchange schedule prices the whole
# individual book under the QHP contract's commission-parity terms)
wedge_df <- read_csv(file.path(TEMP_DIR, "commission_wedge.csv"), show_col_types = FALSE)
DELTA_W <- setNames(wedge_df$estimate, wedge_df$term)
comm_filings_cf <- read_csv("data/output/commission_filings.csv", show_col_types = FALSE)
LEV_LOOKUP <- setNames((1 - comm_filings_cf$on_share) / comm_filings_cf$on_share,
                       paste(comm_filings_cf$insurer_prefix, comm_filings_cf$year, sep = "_"))
WEDGE_LOOKUP <- setNames(
  unname(DELTA_W[paste0("wedge_", comm_filings_cf$insurer_prefix)]) +
    DELTA_W[["wedge_leverage"]] * unname(LEV_LOOKUP),
  names(LEV_LOOKUP))

demand_spec <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
# Full spec (base + assisted): the price-interaction machinery must see the
# channel-specific premium slopes
STRUCTURAL_SPEC <- demand_spec$all

# Scenario grids. Rate-policy scenarios impose schedules defined on the
# OBSERVED rates (a mandate references real-world levels); composition-shock
# scenarios re-solve the affected insurers' rates from the estimated condition.
TAU_GRID       <- c(0, 0.5, 1.0)   # commission ban, agents -> navigators
DEFUND_GRID    <- c(0.5)           # navigators -> agents, rates re-solved
ENDOG_TAU_GRID <- c(0.5)           # agents -> navigators with the rest keeping
                                   # agents, rates re-solved (navigator expansion)
SCALE_GRID     <- c(0.5)           # commission level scaled down
UNIFORM_LOW_SC <- 0.5              # headline: uniform commission at this fraction of the cell mean
BAND_EDGES     <- c(0.75, 1.25)    # band-edge runs on the headline scenario

# Per-scenario row files are cleared each run; fixed_point_<y>.csv persists as
# the warm start for the next run's baseline iteration.
CF_YEAR_DIR <- file.path(TEMP_DIR, "cf_years")
if (!dir.exists(CF_YEAR_DIR)) dir.create(CF_YEAR_DIR, recursive = TRUE)
old_rows <- list.files(CF_YEAR_DIR, pattern = "^(year|firms)_", full.names = TRUE)
if (length(old_rows) > 0) invisible(file.remove(old_rows))

hh_all <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split_cf <- split(hh_all, by = c("region", "year"))
rm(hh_all); gc(verbose = FALSE)

years <- sort(unique(cells$year))

# =========================================================================
# PHASE 2: solve each year
# =========================================================================

t_start <- Sys.time()
year_results <- list()
base_comm_rows <- list()

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
  pieces_obs <- cf_year_evaluate(cl, P_obs)
  if (is.null(pieces_obs) || !all(!vapply(pieces_obs[active], is.null, logical(1)))) {
    cat("  observed-point evaluation failed; year skipped\n"); parallel::stopCluster(cl); next
  }
  ag_obs <- cf_year_aggregate(pieces_obs)
  e_target <- ag_obs$G
  N_year <- sum(vapply(inits[active], function(cs) cs$N, numeric(1)))
  e_dollars <- ag_obs$G / ag_obs$omega_w
  metal_of <- sr_y %>% distinct(plan_id, metal) %>% { setNames(.$metal, .$plan_id) }

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
  # Warm start from a saved fixed point of an earlier run of this year, if any.
  # A saved point predates the current estimates unless this year already ran
  # under them, so it is used only if it starts closer than observed premiums.
  fp_file <- file.path(CF_YEAR_DIR, sprintf("fixed_point_%d.csv", y))
  P_start <- P_obs
  if (file.exists(fp_file)) {
    fp_saved <- read_csv(fp_file, show_col_types = FALSE)
    P_warm <- P_obs
    P_warm[fp_saved$id[fp_saved$kind == "P"]] <- fp_saved$value[fp_saved$kind == "P"]
    f_warm <- tryCatch({
      agw <- cf_year_aggregate(cf_year_evaluate(cl, P_warm))
      max(abs((agw$G / ag_obs$omega_w)[solve_ids]))
    }, error = function(e) Inf)
    if (is.finite(f_warm) && f_warm < max(abs(e_dollars[solve_ids]))) {
      P_start <- P_warm
      cat("  baseline warm start from", basename(fp_file), "\n")
    }
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
    ids <- solve_ids
    P_run <- P_init
    if (set_scenario) {
      # One evaluation under the scenario at the start point: gate the solve
      # set on the scenario's own shares (a removed channel can empty a plan,
      # whose pricing condition is then ill-conditioned) and pre-iterate when
      # the start is far from the scenario's equilibrium.
      pieces0 <- cf_year_evaluate(cl, P_init)
      if (is.null(pieces0) || !all(!vapply(pieces0[active], is.null, logical(1)))) {
        cat("  ", label, "- evaluation failed at the start\n"); return(NULL)
      }
      sh_num <- sh_den <- setNames(numeric(0), character(0))
      for (pc in pieces0) {
        if (is.null(pc)) next
        pn0 <- pc$plan_ids
        sh_num[setdiff(pn0, names(sh_num))] <- 0
        sh_den[setdiff(pn0, names(sh_den))] <- 0
        sh_num[pn0] <- sh_num[pn0] + pc$N * unname(pc$shares[pn0])
        sh_den[pn0] <- sh_den[pn0] + pc$N
      }
      share_scen <- sh_num / sh_den
      ids <- solve_ids[solve_ids %in% names(share_scen)[share_scen >= SHARE_FLOOR_FOC]]
      if (length(ids) == 0) { cat("  ", label, "- no plans above the share floor\n"); return(NULL) }
      if (length(ids) < length(solve_ids))
        cat(sprintf("    [%d %s] %d plans below the share floor held at the start premiums\n",
                    y, label, length(solve_ids) - length(ids)))
      f0 <- cf_year_aggregate(pieces0)$G[ids] / om_base[ids]
      if (max(abs(f0), na.rm = TRUE) > 25) {
        fp_s <- solve_cf_year_fixed_point(yr, label, ids, P_init, om_base = om_base,
                                          maxit_P = 25, tol_dollars = 5)
        if (!is.null(fp_s)) P_run <- fp_s$P
      }
    }
    res <- solve_cf_year(yr, label, ids, P_run, J_P_year, tol_dollars = 5,
                         om_base = om_base)
    if (is.null(res)) { cat("  ", label, "- did not converge\n"); return(NULL) }
    P_full <- P_obs; P_full[names(res$P)] <- res$P
    cat(sprintf("   %s - converged (termcd %d, %d iterations, %d evaluations, %.1f min)\n",
                label, res$sol$termcd, res$sol$iter, res$n_eval, res$elapsed))
    save_rows(label, cf_year_rows(yr, label, tau, res$pieces, P_full, comm_scale,
                                  res$sol$termcd, res$sol$iter))
    data.table::fwrite(cf_year_firm_rows(yr, label, res$pieces),
                       file.path(CF_YEAR_DIR, sprintf("firms_%d_%s.csv", y, label)))
    list(P = P_full, pieces = res$pieces)
  }

  # Baseline polish with the Jacobian at the fixed point (the scenario is
  # already set on the workers): the premium equilibrium at observed
  # commissions, the warm start for the commission solve
  om_base <- NULL
  base <- run_scenario("baseline", NA_real_, NULL, fp$P, set_scenario = FALSE)
  if (is.null(base)) { cat("  baseline did not converge; year skipped\n"); parallel::stopCluster(cl); next }
  P_base <- base$P
  # Baseline own-price terms: the residual scale and far-start gauge for every
  # scenario solve
  om_base <- cf_year_aggregate(base$pieces)$omega_w

  # The baseline commission equilibrium: each gated insurer's rate re-solved to
  # the estimated condition, so the baseline is the model's own equilibrium in
  # both margins. Firms without a wedge (no book-share row) hold at observed.
  beta_gate <- setNames(BETA_LOOKUP[paste(gate, y, sep = "_")], gate)
  wedge_gate <- setNames(WEDGE_LOOKUP[paste(gate, y, sep = "_")], gate)
  firms_solve <- gate[is.finite(beta_gate[gate]) & is.finite(wedge_gate[gate])]
  k_base <- setNames(rep(1, length(gate)), gate)
  if (length(firms_solve) > 0) {
    base_c <- solve_cf_commissions(yr, "baseline", solve_ids, P_base, J_P_year,
                                   list(), firms = firms_solve,
                                   beta_f = beta_gate, wedge_f = wedge_gate,
                                   om_base = om_base)
    if (!is.null(base_c)) {
      P_base <- P_obs; P_base[names(base_c$P)] <- base_c$P
      base <- list(P = P_base, pieces = base_c$pieces)
      agb <- cf_year_aggregate(base_c$pieces)
      om_base <- agb$omega_w
      k_base[names(base_c$k)] <- base_c$k
      save_rows("baseline", cf_year_rows(yr, "baseline", NA_real_, base_c$pieces, P_base,
                                         NA_real_, if (base_c$converged) 1L else 9L, base_c$rounds))
      data.table::fwrite(cf_year_firm_rows(yr, "baseline", base_c$pieces,
                                           k = base_c$k, phi = base_c$phi),
                         file.path(CF_YEAR_DIR, sprintf("firms_%d_baseline.csv", y)))
      base_comm_rows[[as.character(y)]] <- tibble(
        year = y, firm = firms_solve,
        k_base = unname(base_c$k[firms_solve]),
        phi_base = unname(base_c$phi[firms_solve]),
        eta_obs = unname(etabar_y[firms_solve]),
        eta_base = unname((agb$MC / agb$qB)[firms_solve]),
        converged = base_c$converged)
    } else cat("  baseline commission solve failed; commissions held at observed\n")
  }

  # Commission ban with the agent-to-navigator gradient (chained warm starts)
  P_warm <- P_base
  for (tau in TAU_GRID) {
    out <- run_scenario(paste0("zero_tau", sprintf("%.2f", tau)), tau,
                        list(comm = "zero", tau = tau), P_warm, comm_scale = 0)
    if (!is.null(out)) P_warm <- out$P
  }

  # Headline: a uniform commission at UNIFORM_LOW_SC of the cell mean, which
  # removes differential steering and cuts the level while keeping the agent
  # channel alive; band-edge runs bound the commission response
  run_banded <- function(label, tau, spec_point, P_init, comm_scale_point = NA_real_) {
    out <- run_scenario(label, tau, spec_point, P_init, comm_scale = comm_scale_point)
    for (bk in BAND_EDGES) {
      spec_b <- spec_point
      if (identical(spec_b$comm, "flatbar")) spec_b$levels <- spec_point$levels * bk
      else if (identical(spec_b$comm, "uniform"))
        spec_b$u_sc <- (if (is.null(spec_point$u_sc)) 1 else spec_point$u_sc) * bk
      else { spec_b$comm <- "scale"; spec_b$sc <- bk }
      run_scenario(paste0(label, "_k", sprintf("%.2f", bk)), tau, spec_b,
                   if (!is.null(out)) out$P else P_init, comm_scale = NA_real_)
    }
    out
  }
  run_banded("uniform_low", NA_real_,
             list(comm = "uniform", u_sc = UNIFORM_LOW_SC), P_base)

  for (sc in SCALE_GRID)
    run_scenario(paste0("scale_", sprintf("%.2f", sc)), NA_real_,
                 list(comm = "scale", sc = sc), P_base, comm_scale = sc)
  run_scenario("aligned", NA_real_, list(comm = "aligned"), P_base,
               comm_scale = NA_real_)

  # Flat-fee mandate: every insurer with commissions pays its observed mean
  # commission per agent member as a flat fee (a budget-neutral level)
  if (length(gate) > 0)
    run_scenario("flat_mandate", NA_real_,
                 list(comm = "flatbar", levels = etabar_y), P_base,
                 comm_scale = NA_real_)

  # Composition shocks: the policy changes who is agent-assisted, so the gated
  # insurers re-solve their rates from the estimated commission condition,
  # starting at the baseline equilibrium schedules
  run_comm_scenario <- function(label, spec_base, tau_row = NA_real_) {
    out <- solve_cf_commissions(yr, label, solve_ids, P_base, J_P_year, spec_base,
                                firms = firms_solve, beta_f = beta_gate,
                                wedge_f = wedge_gate, k_init = k_base,
                                om_base = om_base)
    if (is.null(out)) { cat("  ", label, "- dropped\n"); return(invisible(NULL)) }
    P_full <- P_obs; P_full[names(out$P)] <- out$P
    save_rows(label, cf_year_rows(yr, label, tau_row, out$pieces, P_full, NA_real_,
                                  if (out$converged) 1L else 9L, out$rounds))
    data.table::fwrite(cf_year_firm_rows(yr, label, out$pieces, k = out$k, phi = out$phi),
                       file.path(CF_YEAR_DIR, sprintf("firms_%d_%s.csv", y, label)))
    invisible(out)
  }
  if (length(firms_solve) > 0) {
    for (df in DEFUND_GRID)
      run_comm_scenario(paste0("defund_", sprintf("%.2f", df)), list(defund = df))
    for (tt in ENDOG_TAU_GRID)
      run_comm_scenario(paste0("endog_tau", sprintf("%.2f", tt)),
                        list(tau = tt, broker_remain = TRUE), tau_row = tt)
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

cf_results <- bind_rows(year_results)
if (nrow(cf_results) == 0) stop("No counterfactual results")
write_csv(cf_results, "results/counterfactual_results.csv")
if (length(base_comm_rows) > 0)
  write_csv(bind_rows(base_comm_rows), "results/cf_baseline_commissions.csv")

firm_files <- list.files(CF_YEAR_DIR, pattern = "^firms_", full.names = TRUE)
if (length(firm_files) > 0) {
  cf_firms <- bind_rows(lapply(firm_files, read_csv, show_col_types = FALSE))
  write_csv(cf_firms, "results/cf_firm_profits.csv")
}

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
  filter(str_detect(scenario, "^uniform_low|^defund_|^flat_mandate|^endog_tau"))
if (nrow(endog_scenarios) > 0) {
  cat("\n--- Commission-level scenarios: point runs and band edges ---\n")
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

