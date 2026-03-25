# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-23
## Description:   Subprocess worker for counterfactual simulation on one
##                region-year cell. Called by 3_counterfactuals.R via system().
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

MAX_HH <- 1000

# Load global data --------------------------------------------------------
coefs <- read_csv("data/output/choice_coefficients_structural.csv", show_col_types = FALSE)
plan_choice <- read_csv("data/output/plan_choice.csv", show_col_types = FALSE)
supply_results <- read_csv("data/output/supply_results.csv", show_col_types = FALSE)
commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

coef_map <- setNames(coefs$estimate, coefs$term)
lambda <- coef_map[["lambda"]]

# Cell data ---------------------------------------------------------------
sr_cell <- supply_results %>% filter(region == r, year == y, !is.na(posted_premium), !is.na(mc))
if (nrow(sr_cell) == 0) {
  cat("No supply results for cell", r, y, "\n")
  quit(save = "no", status = 0)
}

mc_vec <- setNames(sr_cell$mc, sr_cell$plan_name)
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

# Restrict to plans present in cell data (CAT plans may be absent)
cd_plans <- unique(cell_data_base$plan_name[cell_data_base$plan_name != "Uninsured"])
plan_names_cell <- intersect(plan_names_cell, cd_plans)
p_obs <- p_obs[plan_names_cell]
mc_vec <- mc_vec[plan_names_cell]

if (length(plan_names_cell) < 3) {
  cat("Too few plans for cell", r, y, "\n")
  quit(save = "no", status = 0)
}

# Mean observed commission for uniform scenario
mean_comm_pmpm <- mean(sr_cell$commission_pmpm[sr_cell$commission_pmpm > 0], na.rm = TRUE)
if (is.na(mean_comm_pmpm)) mean_comm_pmpm <- 0

# Scenarios ---------------------------------------------------------------
scenarios <- list(
  observed = list(name = "observed", type = "observed"),
  zero     = list(name = "zero", type = "zero"),
  uniform  = list(name = "uniform", type = "uniform", comm_pmpm = mean_comm_pmpm)
)

# FOC function ------------------------------------------------------------
build_foc_function <- function(cell_data_base, coefs_cell, mc_vec,
                                comm_scenario, benchmark_plan, plans_cell) {
  dt_base <- as.data.table(cell_data_base)

  function(p_vec) {
    pn_solve <- names(p_vec)
    dt <- copy(dt_base)

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
      residual <- shares - as.vector(Omega %*% (p_vec - mc_vec[pn_solve])) +
                  as.vector(Omega_broker %*% comm_vec)
    } else {
      residual <- shares - as.vector(Omega %*% (p_vec - mc_vec[pn_solve]))
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

  ins_dt[, alpha_i := (beta_p_cs + beta_h_cs * hh_size +
                          beta_0to17 * any_0to17 +
                          beta_250 * FPL_250to400 +
                          beta_400 * FPL_400plus +
                          beta_blk * any_black +
                          beta_hisp * any_hispanic) / hh_size]

  hh_cs <- ins_dt[, .(
    log_D_lam = first(log_D_lam),
    alpha_i = first(alpha_i),
    ipweight = first(ipweight)
  ), by = household_number]

  hh_cs[, cs := (1 / alpha_i) * log(1 + exp(pmin(log_D_lam, 500)))]

  total_weight <- sum(hh_cs$ipweight)
  weighted_cs <- sum(hh_cs$ipweight * hh_cs$cs) / total_weight
  weighted_cs
}

# Solve -------------------------------------------------------------------
cat("Cell", r, y, "- plans:", length(plan_names_cell), "\n")

results_list <- list()

for (sc_name in names(scenarios)) {
  sc <- scenarios[[sc_name]]

  if (sc$type == "zero") {
    comm_sc <- setNames(rep(0, length(plan_names_cell)), plan_names_cell)
  } else if (sc$type == "uniform") {
    comm_sc <- setNames(rep(sc$comm_pmpm, length(plan_names_cell)), plan_names_cell)
  } else {
    comm_sc <- comm_obs[plan_names_cell]
  }

  # Update commission_broker for this scenario
  cd_scenario <- as.data.table(copy(cell_data_base))
  for (pn in plan_names_cell) {
    idx <- cd_scenario$plan_name == pn
    if (sum(idx) > 0 && "commission_broker" %in% names(cd_scenario)) {
      cd_scenario$commission_broker[idx] <- comm_sc[pn] * cd_scenario$assisted[idx]
    }
  }

  foc_fn <- build_foc_function(cd_scenario, coefs, mc_vec, comm_sc, benchmark_plan, plans_cell)

  sol <- tryCatch(
    nleqslv(x = p_obs, fn = foc_fn, method = "Broyden",
            control = list(maxit = 200, xtol = 1e-6, ftol = 1e-8)),
    error = function(e) {
      cat("  ", sc_name, "- nleqslv error:", conditionMessage(e), "\n")
      NULL
    }
  )

  if (is.null(sol) || sol$termcd > 2) {
    cat("  ", sc_name, "- did not converge (termcd =",
        if (!is.null(sol)) sol$termcd else "NULL", ")\n")
    next
  }

  p_cf <- sol$x

  # CS at counterfactual premiums
  dt_cf <- as.data.table(copy(cd_scenario))
  for (pn in names(p_cf)) {
    idx <- dt_cf$plan_name == pn
    if (sum(idx) == 0) next
    hh_prem_new <- (p_cf[pn] / RATING_FACTOR_AGE40) * dt_cf$rating_factor[idx]
    final_prem <- fifelse(
      dt_cf$metal_level[idx] == "Minimum Coverage",
      hh_prem_new,
      pmax(hh_prem_new - dt_cf$adj_subsidy[idx], 0)
    )
    dt_cf$premium[idx] <- final_prem / dt_cf$hh_size[idx]
  }
  dt_cf[plan_name != "Uninsured", `:=`(
    hh_size_prem      = hh_size * premium,
    any_0to17_prem    = any_0to17 * premium,
    any_black_prem    = any_black * premium,
    any_hispanic_prem = any_hispanic * premium,
    FPL_250to400_prem = FPL_250to400 * premium,
    FPL_400plus_prem  = FPL_400plus * premium
  )]

  cs_cf <- tryCatch(compute_consumer_surplus(dt_cf, coefs), error = function(e) NA_real_)

  # Shares at counterfactual
  util_cf <- compute_utility(dt_cf, coefs)
  se_cf <- tryCatch(
    compute_shares_and_elasticities(dt_cf, util_cf$V, lambda, benchmark_plan, plans_cell, coefs),
    error = function(e) NULL
  )
  cf_shares <- if (!is.null(se_cf)) se_cf$shares[plan_names_cell] else rep(NA, length(plan_names_cell))

  results_list[[length(results_list) + 1]] <- tibble(
    region = r,
    year = y,
    scenario = sc_name,
    plan_name = plan_names_cell,
    premium_obs = p_obs[plan_names_cell],
    premium_cf = p_cf[plan_names_cell],
    premium_change = p_cf[plan_names_cell] - p_obs[plan_names_cell],
    share_obs = setNames(sr_cell$share, sr_cell$plan_name)[plan_names_cell],
    share_cf = cf_shares,
    mc = mc_vec[plan_names_cell],
    commission_pmpm = comm_sc[plan_names_cell],
    markup_cf = p_cf[plan_names_cell] - mc_vec[plan_names_cell],
    cs_weighted = cs_cf,
    nleqslv_termcd = sol$termcd,
    nleqslv_iter = sol$iter
  )

  rm(cd_scenario, dt_cf)
  gc(verbose = FALSE)

  cat("  ", sc_name, "- converged (iter =", sol$iter, ")\n")
}

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
