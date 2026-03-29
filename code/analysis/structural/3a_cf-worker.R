# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-23
## Date Edited:   2026-03-26
## Description:   Subprocess worker for counterfactual simulation on one
##                region-year cell. Called by 3_counterfactuals.R via system().
##                Includes endogenous RA (outer iteration on MC) and
##                broker-to-navigator substitution gradient (tau).
##
## Usage:         Rscript code/analysis/structural/3a_cf-worker.R <region> <year> <seed> <sample_frac>
## Outputs:       data/output/cf_cells/cf_{r}_{y}.csv

# Parse CLI arguments -----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("Usage: Rscript 3a_cf-worker.R <region> <year> <seed> <sample_frac>")
}
r           <- as.integer(args[1])
y           <- as.integer(args[2])
seed        <- as.integer(args[3])
sample_frac <- as.numeric(args[4])

# Setup -------------------------------------------------------------------
source("renv/activate.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(arrow)
  library(nleqslv)
})

source("code/data-build/_helpers-enrollment.R")
source("code/analysis/_helpers-analysis.R")
source("code/analysis/_helpers-choice.R")
source("code/analysis/_helpers-supply.R")
source("code/analysis/_helpers-ra.R")

MAX_HH <- 1000
RA_TOL <- 1e-4
RA_MAX_ITER <- 10
RA_DAMPING <- 0.7
TAU_GRID <- c(0, 0.25, 0.5, 0.75, 1.0)

# Load global data --------------------------------------------------------
coefs <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
plan_choice <- read_csv("data/output/plan_choice.csv", show_col_types = FALSE)
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE)
commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

# RA regression coefficients
rs_coefs_df <- read_csv("data/output/ra_rs_coefs.csv", show_col_types = FALSE)
claims_coefs_df <- read_csv("data/output/ra_claims_coefs.csv", show_col_types = FALSE)
reins_df <- read_csv("data/output/reinsurance_factors.csv", show_col_types = FALSE)

rs_coefs <- setNames(rs_coefs_df$estimate, rs_coefs_df$term)
claims_coefs <- setNames(claims_coefs_df$estimate, claims_coefs_df$term)

coef_map <- setNames(coefs$estimate, coefs$term)
lambda <- coef_map[["lambda"]]

# Cell data ---------------------------------------------------------------
sr_cell <- supply_results %>%
  filter(region == r, year == y, !is.na(posted_premium), !is.na(mc_foc))
if (nrow(sr_cell) == 0) {
  cat("No supply results for cell", r, y, "\n")
  quit(save = "no", status = 0)
}

mc_vec <- setNames(sr_cell$mc_foc, sr_cell$plan_name)
p_obs <- setNames(sr_cell$posted_premium, sr_cell$plan_name)
plan_names_cell <- sr_cell$plan_name

plans_cell <- plan_choice %>% filter(region == r, year == y)
if (nrow(plans_cell) == 0) {
  cat("No plans for cell", r, y, "\n")
  quit(save = "no", status = 0)
}

set.seed(seed)
hhs_raw <- tryCatch(
  read_parquet(file.path("data/output/hh_choice_partitions",
                          paste0("hh_", r, "_", y, ".parquet"))),
  error = function(e) NULL
)
if (is.null(hhs_raw) || nrow(hhs_raw) == 0) {
  cat("No HH data for cell", r, y, "\n")
  quit(save = "no", status = 0)
}

n_hh_cell <- length(unique(hhs_raw$household_id))
eff_frac <- min(sample_frac, MAX_HH / max(n_hh_cell, 1))

benchmark_plan <- identify_benchmark(plans_cell)
comm_obs <- get_commission_pmpm(plan_names_cell, plans_cell, y, commission_lookup)
plans_cell$comm_pmpm <- comm_obs[plans_cell$plan_name]
plans_cell$comm_pmpm[is.na(plans_cell$comm_pmpm)] <- 0

cell_data_base <- build_supply_choice_data(plans_cell, hhs_raw, eff_frac)
rm(hhs_raw)
gc(verbose = FALSE)

if (is.null(cell_data_base) || nrow(cell_data_base) == 0) {
  cat("Empty cell data for", r, y, "\n")
  quit(save = "no", status = 0)
}

if (!"adj_subsidy" %in% names(cell_data_base)) {
  cell_data_base$adj_subsidy <- ifelse(is.na(cell_data_base$subsidy), 0, cell_data_base$subsidy)
}

# Restrict to plans present in cell data
cd_plans <- unique(cell_data_base$plan_name[cell_data_base$plan_name != "Uninsured"])
plan_names_cell <- intersect(plan_names_cell, cd_plans)
p_obs <- p_obs[plan_names_cell]
mc_vec <- mc_vec[plan_names_cell]

if (length(plan_names_cell) < 3) {
  cat("Too few plans for cell", r, y, "\n")
  quit(save = "no", status = 0)
}

# Plan characteristics for RA predictions
plan_metal <- sapply(plan_names_cell, function(pn) {
  m <- plans_cell$metal[plans_cell$plan_name == pn]
  if (length(m) == 0) return(NA_character_)
  m[1]
})
plan_chars_cell <- tibble(
  plan_name = plan_names_cell,
  Silver   = as.integer(unname(plan_metal) == "Silver"),
  Gold     = as.integer(unname(plan_metal) == "Gold"),
  Platinum = as.integer(unname(plan_metal) == "Platinum"),
  HMO      = as.integer(sapply(plan_names_cell, function(pn) {
    pt <- plans_cell$network_type[plans_cell$plan_name == pn]
    if (length(pt) == 0) return(0L)
    as.integer(pt[1] == "HMO")
  })),
  trend    = y - 2014L,
  Anthem      = as.integer(grepl("^ANT", plan_names_cell)),
  Blue_Shield = as.integer(grepl("^BS", plan_names_cell)),
  Health_Net  = as.integer(grepl("^HN", plan_names_cell)),
  Kaiser      = as.integer(grepl("^KA", plan_names_cell))
)

plan_avs <- sapply(plan_names_cell, function(pn) {
  switch(plan_metal[pn], Platinum = 0.90, Gold = 0.80, Silver = 0.70, Bronze = 0.60, 0.70)
})

# Reinsurance factors for this year
rf_year <- reins_df %>% filter(year == y)
reins_vec <- sapply(plan_names_cell, function(pn) {
  rf <- rf_year$reins_factor[rf_year$plan_name == pn]
  if (length(rf) == 0) return(0)
  mean(rf, na.rm = TRUE)
})

# Mean observed commission for uniform scenario
mean_comm_pmpm <- mean(sr_cell$commission_pmpm[sr_cell$commission_pmpm > 0], na.rm = TRUE)
if (is.na(mean_comm_pmpm)) mean_comm_pmpm <- 0

# Observed benchmark premium (for subsidy endogenization)
p_bench_obs <- if (!is.na(benchmark_plan) && benchmark_plan %in% names(p_obs)) {
  p_obs[benchmark_plan]
} else {
  NA_real_
}

# Update subsidies for counterfactual benchmark premium -------------------
# ACA subsidy = max(0, benchmark_prem * rf/RF40 - contribution).
# contribution is fixed by income, so delta_subsidy = delta_benchmark * rf/RF40.
# Only applies to subsidized HH (adj_subsidy > 0 at baseline).

update_subsidies <- function(dt, p_vec) {
  if (is.na(p_bench_obs) || is.na(benchmark_plan)) return(invisible(dt))
  if (!benchmark_plan %in% names(p_vec)) return(invisible(dt))
  delta_bench <- p_vec[benchmark_plan] - p_bench_obs
  if (abs(delta_bench) < 1e-6) return(invisible(dt))
  # adj_subsidy_base is the observed subsidy (stored once at data build)
  if (!"adj_subsidy_base" %in% names(dt)) {
    dt[, adj_subsidy_base := adj_subsidy]
  }
  dt[adj_subsidy_base > 0, adj_subsidy := pmax(0, adj_subsidy_base +
    delta_bench * rating_factor / RATING_FACTOR_AGE40)]
  invisible(dt)
}


# FOC function ------------------------------------------------------------
build_foc_function <- function(cell_data_base, coefs_cell, mc_current,
                                comm_scenario, benchmark_plan, plans_cell) {
  dt_base <- as.data.table(cell_data_base)

  function(p_vec) {
    pn_solve <- names(p_vec)
    dt <- copy(dt_base)

    # Update subsidies for counterfactual benchmark premium
    update_subsidies(dt, p_vec)

    for (pn in pn_solve) {
      idx <- dt$plan_name == pn
      if (sum(idx) == 0) next
      hh_prem_new <- (p_vec[pn] / RATING_FACTOR_AGE40) * dt$rating_factor[idx]
      final_prem <- fifelse(
        dt$metal_level[idx] == "Minimum Coverage",
        hh_prem_new,
        pmax(hh_prem_new - dt$adj_subsidy[idx], 0)
      )
      dt$premium[idx] <- final_prem / dt$hh_size[idx]
    }

    dt[plan_name != "Uninsured", `:=`(
      premium_sq        = premium^2,
      hh_size_prem      = hh_size * premium,
      any_0to17_prem    = any_0to17 * premium,
      any_black_prem    = any_black * premium,
      any_hispanic_prem = any_hispanic * premium,
      FPL_250to400_prem = FPL_250to400 * premium,
      FPL_400plus_prem  = FPL_400plus * premium
    )]

    util <- compute_utility(dt, coefs_cell)
    V <- util$V

    se_result <- tryCatch(
      compute_shares_and_elasticities(dt, V, lambda, benchmark_plan, plans_cell, coefs_cell),
      error = function(e) NULL
    )
    if (is.null(se_result)) return(rep(NA_real_, length(p_vec)))

    shares <- se_result$shares[pn_solve]
    elast_mat <- se_result$elast_mat[pn_solve, pn_solve]

    own_mat <- build_ownership_matrix(pn_solve)
    dimnames(own_mat) <- list(pn_solve, pn_solve)
    Omega <- -own_mat * elast_mat

    broker_result <- tryCatch(
      compute_broker_shares_and_elasticities(dt, V, lambda, benchmark_plan, plans_cell, coefs_cell),
      error = function(e) NULL
    )

    if (!is.null(broker_result)) {
      broker_elast <- broker_result$broker_elast_mat[pn_solve, pn_solve]
      Omega_broker <- -own_mat * broker_elast
      comm_vec <- comm_scenario[pn_solve]
      residual <- shares - as.vector(Omega %*% (p_vec - mc_current[pn_solve])) +
                  as.vector(Omega_broker %*% comm_vec)
    } else {
      residual <- shares - as.vector(Omega %*% (p_vec - mc_current[pn_solve]))
    }

    residual
  }
}

# Consumer surplus --------------------------------------------------------
compute_consumer_surplus <- function(cell_data, coefs_cell) {
  coef_map_cs <- setNames(coefs_cell$estimate, coefs_cell$term)
  lambda_cs <- coef_map_cs[["lambda"]]
  beta_p_cs <- coef_map_cs[["premium"]]
  beta_h_cs <- coef_map_cs[["hh_size_prem"]]

  util <- compute_utility(cell_data, coefs_cell)
  V <- util$V

  dt <- as.data.table(cell_data)
  dt[, V := V]

  # V_0 = β'X_0 per HH (NOT zero)
  V0_by_hh <- dt[plan_name == "Uninsured", .(V_0 = V), by = household_number]

  ins_dt <- dt[plan_name != "Uninsured"]
  ins_dt[, V_scaled := V / lambda_cs]
  ins_dt[, max_V_scaled := max(V_scaled), by = household_number]
  ins_dt[, exp_V := exp(V_scaled - max_V_scaled)]
  ins_dt[, sum_exp_V := sum(exp_V), by = household_number]
  ins_dt[, log_D := max_V_scaled + log(sum_exp_V)]
  ins_dt[, log_D_lam := lambda_cs * log_D]

  beta_0to17 <- if ("any_0to17_prem" %in% names(coef_map_cs)) coef_map_cs[["any_0to17_prem"]] else 0
  beta_250   <- if ("FPL_250to400_prem" %in% names(coef_map_cs)) coef_map_cs[["FPL_250to400_prem"]] else 0
  beta_400   <- if ("FPL_400plus_prem" %in% names(coef_map_cs)) coef_map_cs[["FPL_400plus_prem"]] else 0
  beta_blk   <- if ("any_black_prem" %in% names(coef_map_cs)) coef_map_cs[["any_black_prem"]] else 0
  beta_hisp  <- if ("any_hispanic_prem" %in% names(coef_map_cs)) coef_map_cs[["any_hispanic_prem"]] else 0
  beta_psq   <- if ("premium_sq" %in% names(coef_map_cs)) coef_map_cs[["premium_sq"]] else 0

  ins_dt[, alpha_i := (beta_p_cs + beta_h_cs * hh_size +
                          beta_0to17 * any_0to17 +
                          beta_250 * FPL_250to400 +
                          beta_400 * FPL_400plus +
                          beta_blk * any_black +
                          beta_hisp * any_hispanic +
                          2 * beta_psq * premium) / hh_size]

  ins_dt <- merge(ins_dt, V0_by_hh, by = "household_number", all.x = TRUE)
  ins_dt[is.na(V_0), V_0 := 0]

  hh_cs <- ins_dt[, .(
    log_D_lam = first(log_D_lam),
    V_0 = first(V_0),
    alpha_i = first(alpha_i),
    ipweight = first(ipweight)
  ), by = household_number]

  # Small-Rosen CS: (1/alpha) * log(exp(V_0) + exp(lambda*I))
  hh_cs[, mx := pmax(V_0, log_D_lam)]
  hh_cs[, cs := (1 / alpha_i) * (mx + log(exp(V_0 - mx) + exp(pmin(log_D_lam - mx, 500))))]

  total_weight <- sum(hh_cs$ipweight)
  weighted_cs <- sum(hh_cs$ipweight * hh_cs$cs) / total_weight
  weighted_cs
}


# Solve with endogenous RA ------------------------------------------------
# Outer loop iterates on MC; inner solve finds pricing equilibrium.

solve_equilibrium <- function(cd_scenario, comm_sc, mc_init, p_init) {

  mc_current <- mc_init

  for (ra_iter in seq_len(RA_MAX_ITER)) {
    # Inner: solve pricing FOC given current MC
    foc_fn <- build_foc_function(cd_scenario, coefs, mc_current,
                                  comm_sc, benchmark_plan, plans_cell)

    sol <- tryCatch(
      nleqslv(x = p_init, fn = foc_fn, method = "Broyden",
              control = list(maxit = 200, xtol = 1e-6, ftol = 1e-8)),
      error = function(e) NULL
    )

    if (is.null(sol) || sol$termcd > 2) return(NULL)

    p_new <- sol$x

    # Compute shares at new prices (for RA update)
    dt_new <- as.data.table(copy(cd_scenario))
    update_subsidies(dt_new, p_new)
    for (pn in names(p_new)) {
      idx <- dt_new$plan_name == pn
      if (sum(idx) == 0) next
      hh_prem_new <- (p_new[pn] / RATING_FACTOR_AGE40) * dt_new$rating_factor[idx]
      final_prem <- fifelse(
        dt_new$metal_level[idx] == "Minimum Coverage",
        hh_prem_new,
        pmax(hh_prem_new - dt_new$adj_subsidy[idx], 0)
      )
      dt_new$premium[idx] <- final_prem / dt_new$hh_size[idx]
    }
    dt_new[plan_name != "Uninsured", `:=`(
      premium_sq        = premium^2,
      hh_size_prem      = hh_size * premium,
      any_0to17_prem    = any_0to17 * premium,
      any_black_prem    = any_black * premium,
      any_hispanic_prem = any_hispanic * premium,
      FPL_250to400_prem = FPL_250to400 * premium,
      FPL_400plus_prem  = FPL_400plus * premium
    )]

    util_new <- compute_utility(dt_new, coefs)
    se_new <- tryCatch(
      compute_shares_and_elasticities(dt_new, util_new$V, lambda,
                                       benchmark_plan, plans_cell, coefs),
      error = function(e) NULL
    )
    if (is.null(se_new)) return(NULL)

    shares_new <- se_new$shares[plan_names_cell]

    # Predict new risk scores and MC (with endogenous demographics)
    demo_new <- tryCatch(
      compute_demographic_shares(dt_new, util_new$V, lambda),
      error = function(e) NULL
    )
    rs_new <- predict_risk_scores(rs_coefs, plan_chars_cell, demo_new)
    log_rs_new <- setNames(rs_new$log_risk_score_hat, rs_new$plan_name)
    claims_new <- predict_claims(claims_coefs, plan_chars_cell, log_rs_new)
    avg_prem_new <- mean(p_new, na.rm = TRUE)
    ra_new <- compute_ra_transfers(rs_new, shares_new, avg_prem_new, plan_avs)
    mc_new <- predict_mc_structural(claims_new, ra_new, reins_vec)

    # Check convergence
    mc_diff <- max(abs(mc_new - mc_current), na.rm = TRUE)
    if (mc_diff < RA_TOL) {
      return(list(sol = sol, p = p_new, mc = mc_new, shares = shares_new,
                  ra_iter = ra_iter, dt_final = dt_new))
    }

    # Damped update
    mc_current <- RA_DAMPING * mc_new + (1 - RA_DAMPING) * mc_current
    p_init <- p_new  # warm start
    rm(dt_new)
  }

  # Hit max iterations — return last result
  list(sol = sol, p = p_new, mc = mc_current, shares = shares_new,
       ra_iter = RA_MAX_ITER, dt_final = NULL)
}


# Build scenario cell data ------------------------------------------------
# Modifies cell_data_base for a given commission scenario and tau value.

build_scenario_data <- function(cell_data_base, comm_sc, tau = NULL) {
  cd <- as.data.table(copy(cell_data_base))

  # Update commissions (broker/agent only)
  for (pn in plan_names_cell) {
    idx <- cd$plan_name == pn
    if (sum(idx) > 0 && "commission_broker" %in% names(cd)) {
      if ("any_agent" %in% names(cd)) {
        cd$commission_broker[idx] <- comm_sc[pn] * fifelse(cd$any_agent[idx] == 1L, cd$assisted[idx], 0L)
      } else {
        cd$commission_broker[idx] <- comm_sc[pn] * cd$assisted[idx]
      }
    }
  }

  # Apply broker-to-navigator substitution if tau is specified
  # tau = fraction of broker-assisted HH that switch to navigator
  if (!is.null(tau) && "any_agent" %in% names(cd)) {
    # Identify broker-assisted HH (any_agent == 1)
    agent_hh <- cd[plan_name == "Uninsured" & any_agent == 1, .(household_number, p_nav)]
    if (nrow(agent_hh) == 0) {
      agent_hh <- cd[any_agent == 1, .(household_number, p_nav)]
      agent_hh <- unique(agent_hh, by = "household_number")
    }

    if (nrow(agent_hh) > 0) {
      # Sort by p_nav descending — highest propensity to use navigator first
      agent_hh <- agent_hh[order(-p_nav)]
      n_switch <- ceiling(tau * nrow(agent_hh))
      switch_ids <- agent_hh$household_number[seq_len(n_switch)]

      # Switched HH: keep assisted=1, zero commission_broker (now navigator-like)
      cd[household_number %in% switch_ids, `:=`(
        commission_broker = 0,
        any_agent = 0L,
        channel_detail = "Navigator"
      )]

      # Non-switched broker HH (tau < 1): become unassisted
      if (tau < 1) {
        remain_ids <- setdiff(agent_hh$household_number, switch_ids)
        cd[household_number %in% remain_ids, `:=`(
          assisted = 0L,
          commission_broker = 0,
          any_agent = 0L,
          channel_detail = "Unassisted"
        )]
      }
    }
  }

  # Recompute assisted x metal interactions
  cd[, `:=`(
    assisted_silver = assisted * silver,
    assisted_bronze = assisted * bronze,
    assisted_gold   = assisted * gold,
    assisted_plat   = assisted * platinum
  )]

  # Recompute CF interactions
  if ("v_hat" %in% names(cd) && "commission_broker" %in% names(cd)) {
    cd$v_hat_commission <- cd$v_hat * cd$commission_broker
  }

  cd
}


# Main solve loop ---------------------------------------------------------
cat("Cell", r, y, "- plans:", length(plan_names_cell), "\n")

results_list <- list()

# Scenario 1: Observed
comm_obs_sc <- comm_obs[plan_names_cell]
cd_obs <- build_scenario_data(cell_data_base, comm_obs_sc)
eq_obs <- solve_equilibrium(cd_obs, comm_obs_sc, mc_vec, p_obs)

if (!is.null(eq_obs)) {
  cs_obs <- tryCatch(compute_consumer_surplus(eq_obs$dt_final %||% cd_obs, coefs),
                      error = function(e) NA_real_)
  results_list[[length(results_list) + 1]] <- tibble(
    region = r, year = y, scenario = "observed", tau = NA_real_,
    plan_name = plan_names_cell,
    premium_obs = p_obs[plan_names_cell],
    premium_cf = eq_obs$p[plan_names_cell],
    premium_change = eq_obs$p[plan_names_cell] - p_obs[plan_names_cell],
    share_obs = setNames(sr_cell$share, sr_cell$plan_name)[plan_names_cell],
    share_cf = eq_obs$shares[plan_names_cell],
    mc = eq_obs$mc[plan_names_cell],
    commission_pmpm = comm_obs_sc[plan_names_cell],
    markup_cf = eq_obs$p[plan_names_cell] - eq_obs$mc[plan_names_cell],
    cs_weighted = cs_obs,
    nleqslv_termcd = eq_obs$sol$termcd,
    nleqslv_iter = eq_obs$sol$iter,
    ra_iter = eq_obs$ra_iter
  )
  cat("  observed - converged (nleqslv iter =", eq_obs$sol$iter,
      ", RA iter =", eq_obs$ra_iter, ")\n")
  p_warm <- eq_obs$p  # warm start for subsequent scenarios
} else {
  cat("  observed - did not converge\n")
  p_warm <- p_obs
}
rm(cd_obs)


# Scenario 2: Zero commission with tau gradient
comm_zero <- setNames(rep(0, length(plan_names_cell)), plan_names_cell)

for (tau in TAU_GRID) {
  sc_label <- paste0("zero_tau", sprintf("%.2f", tau))
  cd_tau <- build_scenario_data(cell_data_base, comm_zero, tau = tau)

  eq_tau <- solve_equilibrium(cd_tau, comm_zero, mc_vec, p_warm)

  if (!is.null(eq_tau)) {
    # CS at counterfactual
    dt_cs <- eq_tau$dt_final
    if (is.null(dt_cs)) {
      dt_cs <- as.data.table(copy(cd_tau))
      update_subsidies(dt_cs, eq_tau$p)
      for (pn in names(eq_tau$p)) {
        idx <- dt_cs$plan_name == pn
        if (sum(idx) == 0) next
        hh_prem_new <- (eq_tau$p[pn] / RATING_FACTOR_AGE40) * dt_cs$rating_factor[idx]
        final_prem <- fifelse(
          dt_cs$metal_level[idx] == "Minimum Coverage",
          hh_prem_new,
          pmax(hh_prem_new - dt_cs$adj_subsidy[idx], 0)
        )
        dt_cs$premium[idx] <- final_prem / dt_cs$hh_size[idx]
      }
      dt_cs[plan_name != "Uninsured", `:=`(
        hh_size_prem      = hh_size * premium,
        any_0to17_prem    = any_0to17 * premium,
        any_black_prem    = any_black * premium,
        any_hispanic_prem = any_hispanic * premium,
        FPL_250to400_prem = FPL_250to400 * premium,
        FPL_400plus_prem  = FPL_400plus * premium
      )]
    }
    cs_tau <- tryCatch(compute_consumer_surplus(dt_cs, coefs),
                        error = function(e) NA_real_)

    results_list[[length(results_list) + 1]] <- tibble(
      region = r, year = y, scenario = sc_label, tau = tau,
      plan_name = plan_names_cell,
      premium_obs = p_obs[plan_names_cell],
      premium_cf = eq_tau$p[plan_names_cell],
      premium_change = eq_tau$p[plan_names_cell] - p_obs[plan_names_cell],
      share_obs = setNames(sr_cell$share, sr_cell$plan_name)[plan_names_cell],
      share_cf = eq_tau$shares[plan_names_cell],
      mc = eq_tau$mc[plan_names_cell],
      commission_pmpm = comm_zero[plan_names_cell],
      markup_cf = eq_tau$p[plan_names_cell] - eq_tau$mc[plan_names_cell],
      cs_weighted = cs_tau,
      nleqslv_termcd = eq_tau$sol$termcd,
      nleqslv_iter = eq_tau$sol$iter,
      ra_iter = eq_tau$ra_iter
    )
    cat("  ", sc_label, "- converged (nleqslv iter =", eq_tau$sol$iter,
        ", RA iter =", eq_tau$ra_iter, ")\n")
    p_warm <- eq_tau$p  # warm start for next tau
  } else {
    cat("  ", sc_label, "- did not converge\n")
  }
  rm(cd_tau)
  gc(verbose = FALSE)
}


# Scenario 3: Uniform commission
comm_uniform <- setNames(rep(mean_comm_pmpm, length(plan_names_cell)), plan_names_cell)
cd_unif <- build_scenario_data(cell_data_base, comm_uniform)
eq_unif <- solve_equilibrium(cd_unif, comm_uniform, mc_vec, p_obs)

if (!is.null(eq_unif)) {
  cs_unif <- tryCatch(compute_consumer_surplus(eq_unif$dt_final %||% cd_unif, coefs),
                       error = function(e) NA_real_)
  results_list[[length(results_list) + 1]] <- tibble(
    region = r, year = y, scenario = "uniform", tau = NA_real_,
    plan_name = plan_names_cell,
    premium_obs = p_obs[plan_names_cell],
    premium_cf = eq_unif$p[plan_names_cell],
    premium_change = eq_unif$p[plan_names_cell] - p_obs[plan_names_cell],
    share_obs = setNames(sr_cell$share, sr_cell$plan_name)[plan_names_cell],
    share_cf = eq_unif$shares[plan_names_cell],
    mc = eq_unif$mc[plan_names_cell],
    commission_pmpm = comm_uniform[plan_names_cell],
    markup_cf = eq_unif$p[plan_names_cell] - eq_unif$mc[plan_names_cell],
    cs_weighted = cs_unif,
    nleqslv_termcd = eq_unif$sol$termcd,
    nleqslv_iter = eq_unif$sol$iter,
    ra_iter = eq_unif$ra_iter
  )
  cat("  uniform - converged (nleqslv iter =", eq_unif$sol$iter,
      ", RA iter =", eq_unif$ra_iter, ")\n")
} else {
  cat("  uniform - did not converge\n")
}
rm(cd_unif)


# Write results -----------------------------------------------------------
out_dir <- "data/output/cf_cells"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

if (length(results_list) > 0) {
  out <- bind_rows(results_list)
  write_csv(out, file.path(out_dir, paste0("cf_", r, "_", y, ".csv")))
  cat("Cell", r, y, "- wrote", nrow(out), "rows\n")
} else {
  cat("Cell", r, y, "- no scenarios converged\n")
}
