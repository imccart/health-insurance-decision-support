# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-23
## Date Edited:   2026-03-26
## Description:   Subprocess worker for counterfactual simulation on one
##                region-year cell. Called by 4_counterfactuals.R via system().
##                Endogenous MC(p): demographics → risk scores → claims → RA → MC
##                computed inside each FOC evaluation (Saltzman RAND JE 2021).
##                Broker-to-navigator substitution gradient (tau).
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

# Mean observed commission for uniform scenario
mean_comm_pmpm <- mean(sr_cell$commission_pmpm[sr_cell$commission_pmpm > 0], na.rm = TRUE)
if (is.na(mean_comm_pmpm)) mean_comm_pmpm <- 0

# FOC function and analytical Jacobian -------------------------------------
# Returns list(fn, jac) sharing a cache. fn computes the FOC residual with
# endogenous MC(p) (Saltzman RAND JE 2021, eq. 13) and caches intermediates.
# jac computes the analytical J×J Jacobian using cached quantities.
#
# Jacobian terms:
#   J[l,m] = E[l,m] - Ω[l,m]                            (T1+T2: shares + markup)
#          + Σ_k Ω[l,k] dmc_k/dp_m                      (T3: endogenous MC)
#          - Σ_k O[l,k] dE[k,l]/dp_m (p_k - mc_k)       (T4: elasticity curvature)
#          + Σ_k O[l,k] dE_B[k,l]/dp_m c_k               (T5: broker curvature)
#          + d(ra_foc_l)/dp_m                             (T6: RA FOC derivative)
#
# T1-T4 fully analytical. T5 omitted (broker channel small with exogenous commissions).
# T6 uses cheap analytical-FD hybrid (perturb shares/rs by analytical derivatives).

build_foc_function <- function(cell_data_base, coefs_cell, comm_scenario,
                                benchmark_plan, plans_cell,
                                rs_coefs, claims_coefs, plan_chars_cell,
                                plan_avs, reins_vec, lambda, plan_names_cell) {
  dt_base <- as.data.table(cell_data_base)
  own_mat <- build_ownership_matrix(plan_names_cell)
  dimnames(own_mat) <- list(plan_names_cell, plan_names_cell)
  J <- length(plan_names_cell)

  # MH lookup for RA (same as in compute_ra_foc / compute_ra_transfers)
  MH_LOOKUP <- c("0.6" = 1.00, "0.7" = 1.03, "0.8" = 1.08, "0.9" = 1.15)
  av_rounded <- as.character(round(plan_avs[plan_names_cell], 1))
  mh_vec <- MH_LOOKUP[av_rounded]; mh_vec[is.na(mh_vec)] <- 1.0
  util_adj <- unname(plan_avs[plan_names_cell]) * unname(mh_vec)

  # Claims regression coefficient on log_risk_score (for eq. 19)
  theta_r <- claims_coefs[["log_risk_score"]]

  # Reinsurance factors
  reins_local <- reins_vec[plan_names_cell]
  reins_local[is.na(reins_local)] <- 0

  # Demographics used in risk score regression
  demo_names <- intersect(c("share_18to34", "share_35to54", "share_hispanic"),
                           names(rs_coefs))

  # Shared cache between fn and jac
  cache <- new.env(parent = emptyenv())
  cache$iter <- 0L
  cache$p_vec_prev <- setNames(rep(0, J), plan_names_cell)

  # --- Helper: update premiums in a data.table copy ---
  update_premiums <- function(dt, p_vec) {
    for (pn in names(p_vec)) {
      idx <- dt$plan_name == pn
      if (sum(idx) == 0) next
      hh_prem_new <- (p_vec[pn] / RATING_FACTOR_AGE40) * dt$rating_factor[idx]
      dt$premium[idx] <- hh_prem_new / dt$hh_size[idx]
    }
    recompute_prem_interactions(dt[plan_name != "Uninsured"], STRUCTURAL_SPEC)
    dt
  }

  # --- Helper: compute all FOC quantities at a price vector ---
  # Returns list of quantities; used by both fn (at p_vec) and jac (at perturbed p)
  eval_foc_quantities <- function(dt, p_vec) {
    util <- compute_utility(dt, coefs_cell)
    V <- util$V
    se <- tryCatch(
      compute_shares_and_elasticities(dt, V, lambda, benchmark_plan, plans_cell,
                                       coefs_cell, spec = STRUCTURAL_SPEC),
      error = function(e) NULL
    )
    if (is.null(se)) return(NULL)
    pn <- names(p_vec)
    shares <- se$shares[pn]
    elast <- se$elast_mat[pn, pn]
    avg_p <- mean(p_vec, na.rm = TRUE)
    demo <- tryCatch(compute_demographic_shares(dt, V, lambda), error = function(e) NULL)
    if (is.null(demo)) return(NULL)
    mc_res <- compute_mc(rs_coefs, claims_coefs, plan_chars_cell,
                          demo, shares, avg_p, plan_avs, reins_local)
    rs <- mc_res$predicted_risk_scores[pn]
    ra_foc <- compute_ra_foc(rs, shares, plan_avs, avg_p,
                              elast, own_mat[pn, pn])
    br <- tryCatch(
      compute_broker_shares_and_elasticities(dt, V, lambda, benchmark_plan, plans_cell,
                                              coefs_cell, spec = STRUCTURAL_SPEC),
      error = function(e) NULL
    )
    br_elast <- if (!is.null(br)) br$broker_elast_mat[pn, pn] else NULL
    list(V = V, shares = shares, elast_mat = elast, mc_result = mc_res,
         mc_p = mc_res$mc[pn], rs_p = rs, ra_foc_p = ra_foc,
         demo_shares = demo, avg_prem = avg_p, broker_elast = br_elast)
  }

  # =======================================================================
  # fn: FOC residual with caching
  # =======================================================================
  fn <- function(p_vec) {
    pn_solve <- names(p_vec)
    dt <- update_premiums(copy(dt_base), p_vec)

    q <- eval_foc_quantities(dt, p_vec)
    if (is.null(q)) return(rep(NA_real_, length(p_vec)))

    Omega <- -own_mat[pn_solve, pn_solve] * q$elast_mat

    # Cache for jac
    cache$dt <- dt
    cache$p_vec <- p_vec
    cache$pn_solve <- pn_solve
    cache$q <- q
    cache$Omega <- Omega

    # FOC residual
    if (!is.null(q$broker_elast)) {
      Omega_B <- -own_mat[pn_solve, pn_solve] * q$broker_elast
      comm_vec <- comm_scenario[pn_solve]
      resid <- q$shares + q$ra_foc_p[pn_solve] -
        as.vector(Omega %*% (p_vec - q$mc_p)) +
        as.vector(Omega_B %*% comm_vec)
    } else {
      resid <- q$shares + q$ra_foc_p[pn_solve] -
        as.vector(Omega %*% (p_vec - q$mc_p))
    }

    # Per-iteration diagnostic
    cache$iter <- cache$iter + 1L
    if (cache$iter <= 5 || cache$iter %% 10 == 0) {
      f_norm <- sqrt(sum(resid^2))
      dp <- max(abs(p_vec - cache$p_vec_prev), na.rm = TRUE)
      cat(sprintf("      iter %3d: |f|=%.6f, max|dp|=%.2f, mean(mc)=%.1f\n",
                  cache$iter, f_norm, dp, mean(q$mc_p, na.rm = TRUE)))
    }
    cache$p_vec_prev <- p_vec

    resid
  }

  # =======================================================================
  # jac: Analytical Jacobian (J×J matrix)
  # =======================================================================
  jac <- function(p_vec) {
    pn_solve <- cache$pn_solve
    q <- cache$q
    Omega <- cache$Omega
    E <- q$elast_mat
    mc_p <- q$mc_p
    shares <- q$shares
    markup <- p_vec - mc_p
    J_local <- length(pn_solve)

    # ------------------------------------------------------------------
    # T1 + T2: E[l,m] - Ω[l,m]  (already computed)
    # ------------------------------------------------------------------
    J_mat <- E - Omega

    # ------------------------------------------------------------------
    # T3: Σ_k Ω[l,k] dmc_k/dp_m  (analytical MC Jacobian)
    # ------------------------------------------------------------------
    # Chain: ∂s_dk/∂p_m → ∂r_k/∂p_m (eq.17) → ∂c_k/∂p_m (eq.19) → ∂mc/∂p

    # ∂r_k/∂p_m = r_k * Σ_d γ^d * ∂s_dk/∂p_m
    # ∂s_dk/∂p_m = (1/Q_k) * [∂D_dk/∂p_m - s_dk * ∂Q_k/∂p_m]
    # where D_dk = Σ_i(w*q*demo), Q_k = Σ_i(w*q), ∂Q_k/∂p_m = E[k,m]*total_weight
    #
    # We need ∂D_dk/∂p_m = Σ_i(w * dq_ik/dp_m * demo_id)
    # This requires per-HH dq/dp weighted by demographic indicators.
    # Rather than recompute per-HH derivatives, use the fact that
    # E[k,m] = Σ_i(w * dq_ik/dp_m) / total_weight, and compute the
    # demographic-weighted version similarly.

    # Compute per-HH choice probs and derivatives for demographic weighting
    dt_full <- as.data.table(cache$dt)
    dt_full[, V := q$V]
    ins_dt <- dt_full[plan_name != "Uninsured"]
    ins_dt[, V_scaled := V / lambda]
    ins_dt[, max_V_scaled := max(V_scaled), by = household_number]
    ins_dt[, exp_V := exp(V_scaled - max_V_scaled)]
    ins_dt[, sum_exp_V := sum(exp_V), by = household_number]
    ins_dt[, s_jg := exp_V / sum_exp_V]

    V0_hh <- dt_full[plan_name == "Uninsured", .(V_0 = V[1]), by = household_number]
    ins_dt <- merge(ins_dt, V0_hh, by = "household_number", all.x = TRUE)
    ins_dt[is.na(V_0), V_0 := 0]
    ins_dt[, log_D := max_V_scaled + log(sum_exp_V)]
    ins_dt[, log_D_lam := lambda * log_D]
    ins_dt[, mx := pmax(log_D_lam, V_0)]
    ins_dt[, s_g := exp(log_D_lam - mx) / (exp(log_D_lam - mx) + exp(V_0 - mx))]
    ins_dt[, q_j := s_jg * s_g]

    if (!("alpha_i" %in% names(ins_dt))) {
      ins_dt[, alpha_i := compute_alpha_i(ins_dt, coefs_cell, STRUCTURAL_SPEC)]
    }
    ins_dt[, rf_i := rating_factor / RATING_FACTOR_AGE40]

    w <- if ("ipweight" %in% names(ins_dt)) ins_dt$ipweight else rep(1, nrow(ins_dt))
    ins_dt[, w := w]
    total_weight <- ins_dt[, .(w = first(w)), by = household_number][, sum(w)]

    # Weighted demand Q_k = Σ(w * q) for each plan
    Q_k <- ins_dt[plan_name %in% pn_solve,
                   .(Q = sum(w * q_j)), by = plan_name]
    Q_vec <- setNames(Q_k$Q, Q_k$plan_name)[pn_solve]

    # Demographic composition (from cache)
    demo_sh <- q$demo_shares
    demo_sh <- demo_sh[match(pn_solve, demo_sh$plan_name), ]

    # Prepare demographic indicators for perc_18to34
    if (!"perc_18to34" %in% names(ins_dt) && "perc_18to25" %in% names(ins_dt)) {
      ins_dt[, perc_18to34 := perc_18to25 + perc_26to34]
    }

    # Pre-compute wide-format matrices for second derivatives (T4-T6)
    q_wide <- dcast(ins_dt[plan_name %in% pn_solve],
                     household_number ~ plan_name, value.var = "q_j", fill = 0)
    s_wide <- dcast(ins_dt[plan_name %in% pn_solve],
                     household_number ~ plan_name, value.var = "s_jg", fill = 0)
    hh_order <- q_wide$household_number
    q_mat <- as.matrix(q_wide[, ..pn_solve])   # N_HH × J
    s_mat <- as.matrix(s_wide[, ..pn_solve])   # N_HH × J
    N_HH <- nrow(q_mat)

    # Per-HH scalars aligned with wide matrices (one row per HH, plan-invariant)
    hh_sc <- ins_dt[, .SD[1], by = household_number][match(hh_order, household_number)]
    arf2 <- (hh_sc$alpha_i * hh_sc$rf_i)^2  # N_HH: (α·rf)²
    arf  <- hh_sc$alpha_i * hh_sc$rf_i       # N_HH: α·rf
    w_hh <- hh_sc$w                           # N_HH
    sub_hh <- if ("subsidized" %in% names(hh_sc)) hh_sc$subsidized == 1L else rep(FALSE, N_HH)
    bm_idx <- if (!is.na(benchmark_plan)) match(benchmark_plan, pn_solve) else NA_integer_

    # Compute dmc_dp and dE_dp (T3 + T4) in the same loop over m
    dmc_dp <- matrix(0, J_local, J_local, dimnames = list(pn_solve, pn_solve))
    dr_dp_mat <- matrix(0, J_local, J_local, dimnames = list(pn_solve, pn_solve))
    T4_mat <- matrix(0, J_local, J_local, dimnames = list(pn_solve, pn_solve))
    T5_mat <- matrix(0, J_local, J_local, dimnames = list(pn_solve, pn_solve))

    for (m_idx in seq_along(pn_solve)) {
      m <- pn_solve[m_idx]
      is_bm <- (!is.na(benchmark_plan) && m == benchmark_plan)

      # Per-HH dq_j/dp_m for all insured rows
      m_info <- ins_dt[plan_name == m, .(household_number, s_mg = s_jg, q_m = q_j)]
      merged <- merge(ins_dt[plan_name %in% pn_solve], m_info,
                       by = "household_number", all.x = TRUE)
      merged[is.na(s_mg), s_mg := 0]
      merged[is.na(q_m), q_m := 0]

      if (!is_bm) {
        merged[, dq_dp := q_j * (as.numeric(plan_name == m) / lambda +
                                   ((lambda - 1) / lambda) * s_mg - q_m) *
                            alpha_i * rf_i]
      } else {
        # Benchmark: split by subsidy status
        merged[, dq_dp := fifelse(
          subsidized == 1L,
          fifelse(plan_name == m,
            alpha_i * (-rf_i) * q_j * ((1 - s_mg) * ((lambda - 1) / lambda - s_g)),
            alpha_i * (-rf_i) * q_j * (1 / lambda + (1 - s_mg) * ((lambda - 1) / lambda - s_g))
          ),
          q_j * (as.numeric(plan_name == m) / lambda +
                   ((lambda - 1) / lambda) * s_mg - q_m) * alpha_i * rf_i
        )]
      }

      # ∂Q_k/∂p_m = Σ_i(w * dq_ik/dp_m) = E[k,m] * total_weight
      dQ_k <- merged[, .(dQ = sum(w * dq_dp)), by = plan_name]
      dQ_vec <- setNames(dQ_k$dQ, dQ_k$plan_name)[pn_solve]

      # ∂r_k/∂p_m = r_k * Σ_d γ^d * ∂s_dk/∂p_m
      # ∂s_dk/∂p_m = (1/Q_k)[∂D_dk/∂p_m - s_dk * ∂Q_k/∂p_m]
      dr_dp_m <- setNames(rep(0, J_local), pn_solve)

      for (d in demo_names) {
        gamma_d <- rs_coefs[[d]]
        raw_col <- sub("share_", "perc_", d)
        if (!(raw_col %in% names(merged))) next

        dD_dk <- merged[, .(dD = sum(w * dq_dp * get(raw_col))), by = plan_name]
        dD_vec <- setNames(dD_dk$dD, dD_dk$plan_name)[pn_solve]

        s_dk <- setNames(demo_sh[[d]], demo_sh$plan_name)[pn_solve]

        ds_dk_dp_m <- (dD_vec - s_dk * dQ_vec) / Q_vec
        dr_dp_m <- dr_dp_m + gamma_d * ds_dk_dp_m
      }
      # Multiply by r_k
      rs_vec <- q$rs_p[pn_solve]
      dr_dp_m <- rs_vec * dr_dp_m

      # ∂c_k/∂p_m = θ^r * (c_k / r_k) * ∂r_k/∂p_m  (eq. 19)
      pred_claims <- q$mc_result$predicted_claims[pn_solve]
      dc_dp_m <- theta_r * (pred_claims / rs_vec) * dr_dp_m

      # ∂RA_k/∂p_m via appendix formulas
      # RA_k = (rs_k/S_rs - u_k/S_u) * avg_premium
      # where S_rs = Σ(rs*sh), S_u = Σ(u*sh), u_k = AV_k * MH_k
      sh <- unname(shares[pn_solve])
      rs <- unname(rs_vec)
      ua <- unname(util_adj)
      S_rs <- sum(rs * sh)
      S_u  <- sum(ua * sh)
      avg_p <- q$avg_prem

      # d(rs_k/S_rs)/dp_m (quotient rule on risk score ratio, NOT risk share):
      # RA_k = (rs_k/S_rs - u_k/S_u) * avg_premium
      E_col_m <- unname(E[, m_idx])  # dsh/dp_m for all plans
      dS_rs_dp_m <- sum(dr_dp_m * sh + rs * E_col_m)

      # u_k is fixed (AV * MH), only S_u changes via shares
      dS_u_dp_m <- sum(ua * E_col_m)

      dRA_dp_m <- setNames(rep(0, J_local), pn_solve)
      for (k_idx in seq_along(pn_solve)) {
        # d(rs_k / S_rs)/dp_m: quotient rule, rs_k varies via demographics
        drs_ratio <- (dr_dp_m[k_idx] * S_rs - rs[k_idx] * dS_rs_dp_m) / S_rs^2
        # d(u_k / S_u)/dp_m: u_k is fixed, only S_u changes
        dus_ratio <- (-ua[k_idx] * dS_u_dp_m) / S_u^2
        dRA_dp_m[k_idx] <- (drs_ratio - dus_ratio) * avg_p
      }

      # ∂mc_k/∂p_m = ∂c_k/∂p_m * (1 - reins) - ∂RA_k/∂p_m
      dmc_dp[, m_idx] <- dc_dp_m * (1 - reins_local[pn_solve]) - dRA_dp_m
      dr_dp_mat[, m_idx] <- dr_dp_m  # save for T6

      # ------------------------------------------------------------------
      # T4: -Σ_k O[l,k] · dE[k,l]/dp_m · (p_k - mc_k)  (analytical)
      # ------------------------------------------------------------------
      # d²q_j/(dV_l dV_m) = dq_j/dV_m · A_jl + q_j · ((λ-1)/λ · ds_{l|g}/dV_m - dq_l/dV_m)
      # For non-benchmark m: d²q/(dp_l dp_m) = d²q/(dV_l dV_m) · (α·rf)²
      # For benchmark m (subsidized): d²q/(dp_l dp_b) = -(α·rf)² · Σ_{k≠b} d²q/(dV_l dV_k)

      # dq_j/dV_m for all HH × plan: N_HH × J using wide matrices
      q_m_col <- q_mat[, m_idx]  # N_HH
      s_m_col <- s_mat[, m_idx]  # N_HH
      dq_dV_m <- q_mat * ((lambda - 1) / lambda * s_m_col - q_m_col)  # N_HH × J
      dq_dV_m[, m_idx] <- dq_dV_m[, m_idx] + q_mat[, m_idx] / lambda

      for (l_idx in seq_along(pn_solve)) {
        q_l_col <- q_mat[, l_idx]
        s_l_col <- s_mat[, l_idx]
        dq_l_dV_m <- dq_dV_m[, l_idx]  # N_HH

        # A_jl = I(j==l)/λ + (λ-1)/λ · s_l - q_l  (broadcast to N_HH × J)
        # d²q_j/(dV_l dV_m) = dq_dV_m[,j] · A_jl + q[j] · C_l_m
        # where C_l_m = (λ-1)/λ² · s_l · (I(l==m) - s_m) - dq_l/dV_m
        C_lm <- (lambda - 1) / lambda^2 * s_l_col * (as.numeric(l_idx == m_idx) - s_m_col) - dq_l_dV_m

        # d²q for all j: N_HH × J
        d2q <- dq_dV_m * ((lambda - 1) / lambda * s_l_col - q_l_col) + q_mat * C_lm
        d2q[, l_idx] <- d2q[, l_idx] + dq_dV_m[, l_idx] / lambda  # I(j==l)/λ correction

        if (!is_bm) {
          # Non-benchmark m: d²q/(dp_l dp_m) = d²q/(dV_l dV_m) · (α·rf)²
          dE_col <- colSums(w_hh * arf2 * d2q) / total_weight  # J-vector
        } else {
          # Benchmark m: subsidized HH use -(α·rf)² · Σ_{k≠b} d²q/(dV_l dV_k)
          # Unsubsidized: same as non-benchmark
          unsub_contrib <- colSums(w_hh * (!sub_hh) * arf2 * d2q) / total_weight

          # Subsidized: d²q/(dp_l dp_b) = -(α·rf)² · Σ_{k≠b} d²q/(dV_l dV_k)
          # We need Σ_{k≠b} d²q_j/(dV_l dV_k). Compute d²q/(dV_l dV_k) for each k≠b, sum.
          d2q_sum <- matrix(0, N_HH, J_local)
          for (k_idx in seq_along(pn_solve)) {
            if (k_idx == bm_idx) next
            q_k_col <- q_mat[, k_idx]
            s_k_col <- s_mat[, k_idx]
            dq_dV_k <- q_mat * ((lambda - 1) / lambda * s_k_col - q_k_col)
            dq_dV_k[, k_idx] <- dq_dV_k[, k_idx] + q_mat[, k_idx] / lambda
            dq_l_dV_k <- dq_dV_k[, l_idx]
            C_lk <- (lambda - 1) / lambda^2 * s_l_col * (as.numeric(l_idx == k_idx) - s_k_col) - dq_l_dV_k
            d2q_k <- dq_dV_k * ((lambda - 1) / lambda * s_l_col - q_l_col) + q_mat * C_lk
            d2q_k[, l_idx] <- d2q_k[, l_idx] + dq_dV_k[, l_idx] / lambda
            d2q_sum <- d2q_sum + d2q_k
          }
          sub_contrib <- -colSums(w_hh * sub_hh * arf2 * d2q_sum) / total_weight
          dE_col <- unsub_contrib + sub_contrib
        }

        T4_mat[l_idx, m_idx] <- -sum(own_mat[pn_solve[l_idx], pn_solve] * markup * dE_col)
      }

      rm(merged, m_info, dQ_k)
    }

    # T3: Ω %*% dmc_dp
    J_mat <- J_mat + Omega %*% dmc_dp

    # T4: elasticity curvature
    J_mat <- J_mat + T4_mat

    # ------------------------------------------------------------------
    # T5: broker elasticity curvature (same structure as T4)
    # Omitted — broker channel is small relative to T4 and commissions are
    # exogenous. Add later if needed.
    # ------------------------------------------------------------------

    # ------------------------------------------------------------------
    # T6: d(ra_foc_l)/dp_m  (cheap FD using analytical dr_dp)
    # ------------------------------------------------------------------
    # Perturb shares and risk scores by analytical derivatives, recompute ra_foc
    eps_ra <- 1e-5
    for (m_idx in seq_along(pn_solve)) {
      shares_plus <- shares + E[, m_idx] * eps_ra
      rs_plus <- q$rs_p[pn_solve] + dr_dp_mat[, m_idx] * eps_ra
      ra_foc_plus <- compute_ra_foc(rs_plus, shares_plus, plan_avs, q$avg_prem,
                                     E, own_mat[pn_solve, pn_solve])
      J_mat[, m_idx] <- J_mat[, m_idx] + (ra_foc_plus[pn_solve] - q$ra_foc_p[pn_solve]) / eps_ra
    }

    J_mat
  }

  list(fn = fn, jac = jac)
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
# Newton's method with analytical Jacobian. The FOC includes endogenous MC(p)
# and the Jacobian captures the full price→composition→cost feedback analytically
# (T1-T3) plus numerical FD for second-order corrections (T4-T6).

solve_equilibrium <- function(cd_scenario, comm_sc, p_init) {

  fns <- build_foc_function(cd_scenario, coefs, comm_sc,
                             benchmark_plan, plan_attrs,
                             rs_coefs, claims_coefs, plan_chars_cell,
                             plan_avs, reins_vec, lambda, plan_names_cell)

  f0 <- fns$fn(p_init)
  cat("    initial |FOC| =", round(sqrt(sum(f0^2, na.rm=TRUE)), 6),
      ", any NA:", any(is.na(f0)), "\n")
  if (any(is.na(f0))) return(NULL)

  sol <- tryCatch(
    nleqslv(x = p_init, fn = fns$fn, jac = fns$jac, method = "Newton",
            control = list(maxit = 100, xtol = 1e-6, ftol = 1e-8)),
    error = function(e) { cat("    nleqslv error:", conditionMessage(e), "\n"); NULL }
  )

  if (!is.null(sol) && sol$termcd > 2) {
    f_norm <- sqrt(sum(sol$fvec^2))
    cat("    nleqslv termcd:", sol$termcd, ", |f|:", round(f_norm, 6), "\n")
    if (f_norm >= 0.05) return(NULL)
    cat("    Accepting with small residual\n")
  }
  if (is.null(sol)) return(NULL)

  p_sol <- sol$x

  # Recompute shares and MC at solution prices for output
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
  shares_sol <- if (!is.null(se_sol)) se_sol$shares[plan_names_cell] else setNames(rep(NA_real_, length(plan_names_cell)), plan_names_cell)

  # MC at equilibrium prices (endogenous)
  demo_sol <- tryCatch(
    compute_demographic_shares(dt_sol, util_sol$V, lambda),
    error = function(e) NULL
  )
  mc_sol <- compute_mc(rs_coefs, claims_coefs, plan_chars_cell,
                        demo_sol, shares_sol, mean(p_sol, na.rm = TRUE),
                        plan_avs, reins_vec)

  list(sol = sol, p = p_sol, mc = mc_sol$mc, shares = shares_sol,
       dt_final = dt_sol)
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
eq_obs <- solve_equilibrium(cd_obs, comm_obs_sc, p_obs)

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
    nleqslv_iter = eq_obs$sol$iter
  )
  cat("  observed - converged (nleqslv iter =", eq_obs$sol$iter, ")\n")
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

  eq_tau <- solve_equilibrium(cd_tau, comm_zero, p_warm)

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
      nleqslv_iter = eq_tau$sol$iter
    )
    cat("  ", sc_label, "- converged (nleqslv iter =", eq_tau$sol$iter, ")\n")
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
eq_unif <- solve_equilibrium(cd_unif, comm_uniform, p_obs)

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
    nleqslv_iter = eq_unif$sol$iter
  )
  cat("  uniform - converged (nleqslv iter =", eq_unif$sol$iter, ")\n")
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
