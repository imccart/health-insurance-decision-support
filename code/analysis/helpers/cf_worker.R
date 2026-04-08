# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-23
## Date Edited:   2026-03-26
## Description:   Subprocess worker for counterfactual simulation on one
##                region-year cell. Called by 4_counterfactuals.R via system().
##                Includes endogenous RA (outer iteration on MC) and
##                broker-to-navigator substitution gradient (tau).
##                Uses GMM-estimated cost parameters (FOC-consistent).
##
## Usage:         Rscript code/analysis/structural/4a_cf-worker.R <region> <year> <seed> <sample_frac>
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
# Point to renv library directly (avoids full renv activation overhead)
.libPaths(c(
  file.path("renv/library/windows", paste0("R-", getRversion()[,1:2]), "x86_64-w64-mingw32"),
  .libPaths()
))
suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(arrow)
  library(nleqslv)
})

source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")
source("code/analysis/helpers/ra.R")

TEMP_DIR <- Sys.getenv("TEMP_DIR")

# Load demand spec (written by _structural.R)
demand_spec <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
STRUCTURAL_SPEC <- demand_spec$base
STRUCTURAL_ASST <- demand_spec$assisted

RA_TOL <- 0.5       # $/month MC change — ~0.2% of typical MC ($200-300)
RA_MAX_ITER <- 10
RA_DAMPING <- 0.7
TAU_GRID <- c(0, 0.25, 0.5, 0.75, 1.0)

# Load global data --------------------------------------------------------
coefs <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
plan_choice <- read_csv(file.path(TEMP_DIR, "plan_choice.csv"), show_col_types = FALSE)
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE)
commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

# Cost parameters from GMM (consistent with FOC)
rs_coefs_df <- read_csv(file.path(TEMP_DIR, "ra_rs_coefs_gmm.csv"), show_col_types = FALSE)
claims_coefs_df <- read_csv(file.path(TEMP_DIR, "ra_claims_coefs_gmm.csv"), show_col_types = FALSE)
reins_df <- read_csv(file.path(TEMP_DIR, "reinsurance_factors.csv"), show_col_types = FALSE)

rs_coefs <- setNames(rs_coefs_df$estimate, rs_coefs_df$term)
claims_coefs <- setNames(claims_coefs_df$estimate, claims_coefs_df$term)
plan_demo_yr <- read_csv(file.path(TEMP_DIR, "plan_demographics.csv"), show_col_types = FALSE)

coef_map <- setNames(coefs$estimate, coefs$term)
lambda <- coef_map[["lambda"]]

# Cell data ---------------------------------------------------------------
sr_cell <- supply_results %>%
  filter(region == r, year == y, !is.na(posted_premium), !is.na(mc_foc))
if (nrow(sr_cell) == 0) {
  cat("No supply results for cell", r, y, "\n")
  quit(save = "no", status = 0)
}

plans_cell <- plan_choice %>% filter(region == r, year == y)
if (nrow(plans_cell) == 0) {
  cat("No plans for cell", r, y, "\n")
  quit(save = "no", status = 0)
}

set.seed(seed)
hhs_raw <- tryCatch(
  read_parquet(file.path(TEMP_DIR, "hh_choice_partitions",
                          paste0("hh_", r, "_", y, ".parquet"))),
  error = function(e) NULL
)
if (is.null(hhs_raw) || nrow(hhs_raw) == 0) {
  cat("No HH data for cell", r, y, "\n")
  quit(save = "no", status = 0)
}

# Add commissions to plans before building choice data
comm_obs_raw <- get_commission_pmpm(unique(plans_cell$plan_name), plans_cell, y, commission_lookup)
plans_cell$comm_pmpm <- comm_obs_raw[plans_cell$plan_name]
plans_cell$comm_pmpm[is.na(plans_cell$comm_pmpm)] <- 0

build_result <- build_supply_choice_data(plans_cell, hhs_raw, sample_frac,
                                         spec = STRUCTURAL_SPEC)
rm(hhs_raw)
gc(verbose = FALSE)

if (is.null(build_result)) {
  cat("Empty cell data for", r, y, "\n")
  quit(save = "no", status = 0)
}
cell_data_base <- build_result$cell_data
plan_attrs     <- build_result$plan_attrs
rm(build_result)

if (!"adj_subsidy" %in% names(cell_data_base)) {
  cell_data_base$adj_subsidy <- ifelse(is.na(cell_data_base$subsidy), 0, cell_data_base$subsidy)
}

# Plan names and attributes from plan_attrs (post-collapse, always consistent)
plan_names_cell <- sort(plan_attrs$plan_name)

# Restrict to plans also in supply results
plan_names_cell <- intersect(plan_names_cell, sr_cell$plan_name)

if (length(plan_names_cell) < 3) {
  cat("Too few plans for cell", r, y, "\n")
  quit(save = "no", status = 0)
}

# Read all plan attributes from plan_attrs
pa <- plan_attrs[match(plan_names_cell, plan_attrs$plan_name), ]
p_obs      <- setNames(pa$premium_posted, pa$plan_name)
plan_avs   <- setNames(pa$av, pa$plan_name)
comm_obs   <- if ("comm_pmpm" %in% names(pa)) setNames(pa$comm_pmpm, pa$plan_name) else setNames(rep(0, length(plan_names_cell)), plan_names_cell)

benchmark_plan <- identify_benchmark(plan_attrs)

plan_chars_cell <- tibble(
  plan_name   = plan_names_cell,
  Silver      = as.integer(pa$metal == "Silver"),
  Gold        = as.integer(pa$metal == "Gold"),
  Platinum    = as.integer(pa$metal == "Platinum"),
  HMO         = pa$hmo,
  trend       = y - 2014L,
  Anthem      = as.integer(grepl("^ANT", plan_names_cell)),
  Blue_Shield = as.integer(grepl("^BS", plan_names_cell)),
  Health_Net  = as.integer(grepl("^HN", plan_names_cell)),
  Kaiser      = as.integer(grepl("^KA", plan_names_cell))
)

# Reinsurance factors for this year
rf_year <- reins_df %>% filter(year == y)
reins_vec <- sapply(plan_names_cell, function(pn) {
  rf <- rf_year$reins_factor[rf_year$plan_name == pn]
  if (length(rf) == 0) return(0)
  mean(rf, na.rm = TRUE)
})

# Use mc_foc from supply results as initial MC
# mc_foc is the FOC-implied MC at observed equilibrium — by construction,
# the FOC residual is zero at p_obs with mc_foc, giving nleqslv a clean start.
# The RA outer loop will adjust MC for enrollment composition changes.
mc_init_foc <- setNames(sr_cell$mc_foc, sr_cell$plan_name)[plan_names_cell]
mc_vec <- mc_init_foc

# Demographics for RA iteration (used in solve_equilibrium)
demo_obs <- plan_demo_yr %>% filter(year == y)
demo_obs_cell <- tibble(
  plan_name = plan_names_cell,
  share_18to34 = sapply(plan_names_cell, function(p) {
    v <- demo_obs$share_18to34[demo_obs$plan_name == p]
    if (length(v) == 0) mean(demo_obs$share_18to34, na.rm = TRUE) else v[1]
  }),
  share_35to54 = sapply(plan_names_cell, function(p) {
    v <- demo_obs$share_35to54[demo_obs$plan_name == p]
    if (length(v) == 0) mean(demo_obs$share_35to54, na.rm = TRUE) else v[1]
  }),
  share_hispanic = sapply(plan_names_cell, function(p) {
    v <- demo_obs$share_hispanic[demo_obs$plan_name == p]
    if (length(v) == 0) mean(demo_obs$share_hispanic, na.rm = TRUE) else v[1]
  })
)

# Mean observed commission for uniform scenario
mean_comm_pmpm <- mean(sr_cell$commission_pmpm[sr_cell$commission_pmpm > 0], na.rm = TRUE)
if (is.na(mean_comm_pmpm)) mean_comm_pmpm <- 0

# FOC function ------------------------------------------------------------
# ra_foc_vec is pre-computed per RA outer iteration (not inside every nleqslv call)
build_foc_function <- function(cell_data_base, coefs_cell, mc_current,
                                comm_scenario, ra_foc_vec,
                                benchmark_plan, plans_cell) {
  dt_base <- as.data.table(cell_data_base)

  function(p_vec) {
    pn_solve <- names(p_vec)
    dt <- copy(dt_base)

    # Posted premium: age-adjusted posted (no subsidy deduction)
    for (pn in pn_solve) {
      idx <- dt$plan_name == pn
      if (sum(idx) == 0) next
      hh_prem_new <- (p_vec[pn] / RATING_FACTOR_AGE40) * dt$rating_factor[idx]
      dt$premium[idx] <- hh_prem_new / dt$hh_size[idx]
    }

    # Recompute demographic x premium interactions (spec-driven)
    recompute_prem_interactions(dt[plan_name != "Uninsured"], STRUCTURAL_SPEC)

    util <- compute_utility(dt, coefs_cell)
    V <- util$V

    se_result <- tryCatch(
      compute_shares_and_elasticities(dt, V, lambda, benchmark_plan, plans_cell,
                                       coefs_cell, spec = STRUCTURAL_SPEC),
      error = function(e) NULL
    )
    if (is.null(se_result)) return(rep(NA_real_, length(p_vec)))

    shares <- se_result$shares[pn_solve]
    elast_mat <- se_result$elast_mat[pn_solve, pn_solve]

    own_mat <- build_ownership_matrix(pn_solve)
    dimnames(own_mat) <- list(pn_solve, pn_solve)
    Omega <- -own_mat * elast_mat

    broker_result <- tryCatch(
      compute_broker_shares_and_elasticities(dt, V, lambda, benchmark_plan, plans_cell,
                                              coefs_cell, spec = STRUCTURAL_SPEC),
      error = function(e) NULL
    )

    if (!is.null(broker_result)) {
      broker_elast <- broker_result$broker_elast_mat[pn_solve, pn_solve]
      Omega_broker <- -own_mat * broker_elast
      comm_vec <- comm_scenario[pn_solve]
      residual <- shares + ra_foc_vec[pn_solve] -
                  as.vector(Omega %*% (p_vec - mc_current[pn_solve])) +
                  as.vector(Omega_broker %*% comm_vec)
    } else {
      residual <- shares + ra_foc_vec[pn_solve] -
                  as.vector(Omega %*% (p_vec - mc_current[pn_solve]))
    }

    residual
  }
}

# Consumer surplus --------------------------------------------------------
compute_consumer_surplus <- function(cell_data, coefs_cell) {
  coef_map_cs <- setNames(coefs_cell$estimate, coefs_cell$term)
  lambda_cs <- coef_map_cs[["lambda"]]

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

  # Generic alpha_i from spec (automatically adapts to covariate changes)
  alpha_vec <- compute_alpha_i(ins_dt, coefs_cell, STRUCTURAL_SPEC)
  ins_dt[, alpha_i := alpha_vec]

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


# Solve pricing equilibrium ------------------------------------------------
# Outer loop iterates on MC via endogenous RA: solve FOC → recompute shares →
# recompute demographic composition → predict risk scores → update RA → update MC.
# Inner solve uses nleqslv with fixed MC and ra_foc.

solve_equilibrium <- function(cd_scenario, comm_sc, mc_init, p_init) {

  # Compute ra_foc at initial state (first-order RA response to price changes)
  dt_init <- as.data.table(copy(cd_scenario))
  util_init <- compute_utility(dt_init, coefs)
  se_init <- tryCatch(
    compute_shares_and_elasticities(dt_init, util_init$V, lambda,
                                     benchmark_plan, plan_attrs, coefs,
                                     spec = STRUCTURAL_SPEC),
    error = function(e) NULL
  )
  if (is.null(se_init)) return(NULL)

  shares_init <- se_init$shares[plan_names_cell]
  elast_init <- se_init$elast_mat

  demo_init <- tryCatch(
    compute_demographic_shares(dt_init, util_init$V, lambda),
    error = function(e) NULL
  )
  mc_result_init <- compute_mc(rs_coefs, claims_coefs, plan_chars_cell,
                                demo_init, shares_init,
                                mean(p_init, na.rm = TRUE),
                                plan_avs, reins_vec)
  rs_init <- setNames(
    mc_result_init$predicted_risk_scores,
    names(mc_result_init$predicted_risk_scores)
  )[plan_names_cell]

  own_mat_ra <- build_ownership_matrix(plan_names_cell)
  ra_foc_vec <- compute_ra_foc(rs_init, shares_init, plan_avs,
                                mean(p_init, na.rm = TRUE),
                                elast_init, own_mat_ra)
  rm(dt_init)

  # Single-pass FOC solve with ra_foc from initial state
  foc_fn <- build_foc_function(cd_scenario, coefs, mc_init,
                                comm_sc, ra_foc_vec, benchmark_plan, plan_attrs)

  f0 <- foc_fn(p_init)
  cat("    initial |FOC| =", round(sqrt(sum(f0^2, na.rm=TRUE)), 6),
      ", any NA:", any(is.na(f0)), "\n")
  if (any(is.na(f0))) return(NULL)

  sol <- tryCatch(
    nleqslv(x = p_init, fn = foc_fn, method = "Broyden",
            control = list(maxit = 200, xtol = 1e-6, ftol = 1e-8)),
    error = function(e) { cat("    nleqslv error:", conditionMessage(e), "\n"); NULL }
  )

  # Accept if residual is small enough even with non-ideal termcd
  if (!is.null(sol) && sol$termcd > 2) {
    f_norm <- sqrt(sum(sol$fvec^2))
    if (f_norm < 0.01) {
      cat("    Accepting termcd", sol$termcd, "with |f| =", round(f_norm, 6), "\n")
    } else {
      cat("    nleqslv termcd:", sol$termcd, ", |f|:", round(f_norm, 6), "\n")
      return(NULL)
    }
  }
  if (is.null(sol)) return(NULL)

  p_sol <- sol$x

  # Compute shares at solution prices for output
  dt_sol <- as.data.table(copy(cd_scenario))
  for (pn in names(p_sol)) {
    idx <- dt_sol$plan_name == pn
    if (sum(idx) == 0) next
    hh_prem_new <- (p_sol[pn] / RATING_FACTOR_AGE40) * dt_sol$rating_factor[idx]
    dt_sol$premium[idx] <- hh_prem_new / dt_sol$hh_size[idx]
  }
  recompute_prem_interactions(dt_sol[plan_name != "Uninsured"], STRUCTURAL_SPEC)

  util_sol <- compute_utility(dt_sol, coefs)
  se_sol <- tryCatch(
    compute_shares_and_elasticities(dt_sol, util_sol$V, lambda,
                                     benchmark_plan, plan_attrs, coefs,
                                     spec = STRUCTURAL_SPEC),
    error = function(e) NULL
  )
  shares_sol <- if (!is.null(se_sol)) se_sol$shares[plan_names_cell] else shares_init

  list(sol = sol, p = p_sol, mc = mc_init, shares = shares_sol,
       ra_iter = 1L, dt_final = dt_sol)
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
      for (pn in names(eq_tau$p)) {
        idx <- dt_cs$plan_name == pn
        if (sum(idx) == 0) next
        hh_prem_new <- (eq_tau$p[pn] / RATING_FACTOR_AGE40) * dt_cs$rating_factor[idx]
        dt_cs$premium[idx] <- hh_prem_new / dt_cs$hh_size[idx]
      }
      recompute_prem_interactions(dt_cs[plan_name != "Uninsured"], STRUCTURAL_SPEC)
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
out_dir <- file.path(TEMP_DIR, "cf_cells")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

if (length(results_list) > 0) {
  out <- bind_rows(results_list)
  write_csv(out, file.path(out_dir, paste0("cf_", r, "_", y, ".csv")))
  cat("Cell", r, y, "- wrote", nrow(out), "rows\n")
} else {
  cat("Cell", r, y, "- no scenarios converged\n")
}
