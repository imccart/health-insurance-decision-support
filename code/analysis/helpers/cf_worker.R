# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-23
## Date Edited:   2026-04-12
## Description:   Counterfactual simulation for one region-year cell.
##                Endogenous MC(p): demographics -> risk scores -> claims -> RA -> MC
##                computed inside each FOC evaluation (Saltzman RAND JE 2021).
##                Broker-to-navigator substitution gradient (tau).
##
##                Called as a function from 4_counterfactuals.R (in-process).

# run_cf_cell ---------------------------------------------------------------
#
# Runs counterfactual scenarios for one region-year cell.
# Returns a tibble of results, or NULL if the cell can't be processed.

run_cf_cell <- function(r, y, seed, sample_frac, hhs_raw,
                        plan_choice, supply_results, coefs,
                        commission_lookup, rs_coefs, claims_coefs,
                        reins_df, STRUCTURAL_SPEC) {

  TAU_GRID <- c(0, 0.25, 0.5, 0.75, 1.0)

  coef_map <- setNames(coefs$estimate, coefs$term)
  lambda <- coef_map[["lambda"]]

  # Cell data ---------------------------------------------------------------
  sr_cell <- supply_results %>%
    filter(region == r, year == y, !is.na(posted_premium), !is.na(mc_foc))
  if (nrow(sr_cell) == 0) {
    cat("No supply results for cell", r, y, "\n")
    return(NULL)
  }

  plans_cell <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans_cell) == 0) {
    cat("No plans for cell", r, y, "\n")
    return(NULL)
  }

  set.seed(seed)
  if (nrow(hhs_raw) == 0) {
    cat("No HH data for cell", r, y, "\n")
    return(NULL)
  }

  # Add commissions to plans before building choice data
  comm_yr <- commission_lookup %>% filter(year == !!y) %>% select(-year)
  plans_cell <- plans_cell %>%
    mutate(insurer_prefix = sub("_.*", "", plan_id)) %>%
    left_join(comm_yr, by = "insurer_prefix") %>%
    mutate(comm_pmpm = case_when(is.na(rate) ~ 0, is_pct ~ rate * premium, TRUE ~ rate)) %>%
    select(-insurer_prefix, -rate, -is_pct)
  rm(comm_yr)

  build_result <- build_supply_choice_data(plans_cell, hhs_raw, sample_frac,
                                           spec = STRUCTURAL_SPEC)

  if (is.null(build_result)) {
    cat("Empty cell data for", r, y, "\n")
    return(NULL)
  }
  cell_data_base <- build_result$cell_data
  plan_attrs     <- build_result$plan_attrs
  rm(build_result)

  if (!"adj_subsidy" %in% names(cell_data_base)) {
    cell_data_base$adj_subsidy <- ifelse(is.na(cell_data_base$subsidy), 0, cell_data_base$subsidy)
  }

  # Plan names and attributes from plan_attrs (post-collapse, always consistent)
  plan_ids_cell <- sort(plan_attrs$plan_id)

  # Restrict to plans also in supply results
  plan_ids_cell <- intersect(plan_ids_cell, sr_cell$plan_id)

  if (length(plan_ids_cell) < 3) {
    cat("Too few plans for cell", r, y, "\n")
    return(NULL)
  }

  # Read all plan attributes from plan_attrs
  pa <- plan_attrs[match(plan_ids_cell, plan_attrs$plan_id), ]
  p_obs      <- setNames(pa$premium_posted, pa$plan_id)
  plan_avs   <- setNames(pa$av, pa$plan_id)
  comm_obs   <- if ("comm_pmpm" %in% names(pa)) setNames(pa$comm_pmpm, pa$plan_id) else setNames(rep(0, length(plan_ids_cell)), plan_ids_cell)

  # 2nd cheapest Silver by posted premium (ACA benchmark)
  silver <- plan_attrs[plan_attrs$metal == "Silver", ]
  silver <- silver[order(silver$premium_posted), ]
  benchmark_plan <- if (nrow(silver) == 0) NA_character_ else if (nrow(silver) == 1) silver$plan_id[1] else silver$plan_id[2]

  plan_chars_cell <- tibble(
    plan_id   = plan_ids_cell,
    Silver      = as.integer(pa$metal == "Silver"),
    Gold        = as.integer(pa$metal == "Gold"),
    Platinum    = as.integer(pa$metal == "Platinum"),
    HMO         = pa$hmo,
    trend       = y - 2014L,
    Anthem      = as.integer(str_detect(plan_ids_cell, "^ANT")),
    Blue_Shield = as.integer(str_detect(plan_ids_cell, "^BS")),
    Health_Net  = as.integer(str_detect(plan_ids_cell, "^HN")),
    Kaiser      = as.integer(str_detect(plan_ids_cell, "^KA"))
  )

  # Reinsurance factors for this year
  rf_year <- reins_df %>% filter(year == y)
  reins_vec <- sapply(plan_ids_cell, function(pn) {
    rf <- rf_year$reins_factor[rf_year$plan_id == pn]
    if (length(rf) == 0) return(0)
    mean(rf, na.rm = TRUE)
  })

  # Mean observed commission for uniform scenario
  mean_comm_pmpm <- mean(sr_cell$commission_pmpm[sr_cell$commission_pmpm > 0], na.rm = TRUE)
  if (is.na(mean_comm_pmpm)) mean_comm_pmpm <- 0

  # FOC function and analytical Jacobian -------------------------------------
  # Returns list(fn, jac) sharing a cache. fn computes the FOC residual with
  # endogenous MC(p) (Saltzman RAND JE 2021, eq. 13) and caches intermediates.
  # jac computes the analytical J*J Jacobian using cached quantities.
  #
  # Jacobian terms:
  #   J[l,m] = E[l,m] - Omega[l,m]                         (T1+T2: shares + markup)
  #          + Sum_k Omega[l,k] dmc_k/dp_m                  (T3: endogenous MC)
  #          - Sum_k O[l,k] dE[k,l]/dp_m (p_k - mc_k)      (T4: elasticity curvature)
  #          + Sum_k O[l,k] dE_B[k,l]/dp_m c_k              (T5: broker curvature)
  #          + d(ra_foc_l)/dp_m                              (T6: RA FOC derivative)
  #
  # T1-T4 fully analytical. T5 omitted (broker channel small with exogenous commissions).
  # T6 uses cheap analytical-FD hybrid (perturb shares/rs by analytical derivatives).

  build_foc_function <- function(cell_data_base, coefs_cell, comm_scenario,
                                  benchmark_plan, plans_cell,
                                  rs_coefs, claims_coefs, plan_chars_cell,
                                  plan_avs, reins_vec, lambda, plan_ids_cell) {
    dt_base <- as.data.table(cell_data_base)
    own_mat <- build_ownership_matrix(plan_ids_cell)
    dimnames(own_mat) <- list(plan_ids_cell, plan_ids_cell)
    J <- length(plan_ids_cell)

    # MH lookup for RA (same as in compute_ra_foc / compute_ra_transfers)
    MH_LOOKUP <- c("0.6" = 1.00, "0.7" = 1.03, "0.8" = 1.08, "0.9" = 1.15)
    av_rounded <- as.character(round(plan_avs[plan_ids_cell], 1))
    mh_vec <- MH_LOOKUP[av_rounded]; mh_vec[is.na(mh_vec)] <- 1.0
    util_adj <- unname(plan_avs[plan_ids_cell]) * unname(mh_vec)

    # Claims regression coefficient on log_risk_score (for eq. 19)
    theta_r <- claims_coefs[["log_risk_score"]]

    # Reinsurance factors
    reins_local <- reins_vec[plan_ids_cell]
    reins_local[is.na(reins_local)] <- 0

    # Demographics used in risk score regression
    demo_names <- intersect(c("share_18to34", "share_35to54", "share_hispanic"),
                             names(rs_coefs))

    # Shared cache between fn and jac
    cache <- new.env(parent = emptyenv())
    cache$iter <- 0L
    cache$p_vec_prev <- setNames(rep(0, J), plan_ids_cell)

    # --- Helper: update premiums in a data.table copy ---
    update_premiums <- function(dt, p_vec) {
      for (pn in names(p_vec)) {
        idx <- dt$plan_id == pn
        if (sum(idx) == 0) next
        hh_prem_new <- (p_vec[pn] / RATING_FACTOR_AGE40) * dt$rating_factor[idx]
        dt$premium[idx] <- hh_prem_new / dt$hh_size[idx]
      }
      recompute_prem_interactions(dt[plan_id != "Uninsured"], STRUCTURAL_SPEC)
      dt
    }

    # --- Helper: compute all FOC quantities at a price vector ---
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
    # jac: Analytical Jacobian (J*J matrix)
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
      # T1 + T2: E[l,m] - Omega[l,m]  (already computed)
      # ------------------------------------------------------------------
      J_mat <- E - Omega

      # ------------------------------------------------------------------
      # T3: Sum_k Omega[l,k] dmc_k/dp_m  (analytical MC Jacobian)
      # ------------------------------------------------------------------
      dt_full <- as.data.table(cache$dt)
      dt_full[, V := q$V]
      ins_dt <- dt_full[plan_id != "Uninsured"]
      ins_dt[, V_scaled := V / lambda]
      ins_dt[, max_V_scaled := max(V_scaled), by = household_number]
      ins_dt[, exp_V := exp(V_scaled - max_V_scaled)]
      ins_dt[, sum_exp_V := sum(exp_V), by = household_number]
      ins_dt[, s_jg := exp_V / sum_exp_V]

      V0_hh <- dt_full[plan_id == "Uninsured", .(V_0 = V[1]), by = household_number]
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

      w <- if ("hh_weight" %in% names(ins_dt)) ins_dt$hh_weight else rep(1, nrow(ins_dt))
      ins_dt[, w := w]
      total_weight <- ins_dt[, .(w = first(w)), by = household_number][, sum(w)]

      Q_k <- ins_dt[plan_id %in% pn_solve,
                     .(Q = sum(w * q_j)), by = plan_id]
      Q_vec <- setNames(Q_k$Q, Q_k$plan_id)[pn_solve]

      demo_sh <- q$demo_shares
      demo_sh <- demo_sh[match(pn_solve, demo_sh$plan_id), ]

      if (!"perc_18to34" %in% names(ins_dt) && "perc_18to25" %in% names(ins_dt)) {
        ins_dt[, perc_18to34 := perc_18to25 + perc_26to34]
      }

      q_wide <- dcast(ins_dt[plan_id %in% pn_solve],
                       household_number ~ plan_id, value.var = "q_j", fill = 0)
      s_wide <- dcast(ins_dt[plan_id %in% pn_solve],
                       household_number ~ plan_id, value.var = "s_jg", fill = 0)
      hh_order <- q_wide$household_number
      q_mat <- as.matrix(q_wide[, ..pn_solve])
      s_mat <- as.matrix(s_wide[, ..pn_solve])
      N_HH <- nrow(q_mat)

      hh_sc <- ins_dt[, .SD[1], by = household_number][match(hh_order, household_number)]
      arf2 <- (hh_sc$alpha_i * hh_sc$rf_i)^2
      arf  <- hh_sc$alpha_i * hh_sc$rf_i
      w_hh <- hh_sc$w
      sub_hh <- if ("subsidized" %in% names(hh_sc)) hh_sc$subsidized == 1L else rep(FALSE, N_HH)
      bm_idx <- if (!is.na(benchmark_plan)) match(benchmark_plan, pn_solve) else NA_integer_

      dmc_dp <- matrix(0, J_local, J_local, dimnames = list(pn_solve, pn_solve))
      dr_dp_mat <- matrix(0, J_local, J_local, dimnames = list(pn_solve, pn_solve))
      T4_mat <- matrix(0, J_local, J_local, dimnames = list(pn_solve, pn_solve))
      T5_mat <- matrix(0, J_local, J_local, dimnames = list(pn_solve, pn_solve))

      for (m_idx in seq_along(pn_solve)) {
        m <- pn_solve[m_idx]
        is_bm <- (!is.na(benchmark_plan) && m == benchmark_plan)

        m_info <- ins_dt[plan_id == m, .(household_number, s_mg = s_jg, q_m = q_j)]
        merged <- merge(ins_dt[plan_id %in% pn_solve], m_info,
                         by = "household_number", all.x = TRUE)
        merged[is.na(s_mg), s_mg := 0]
        merged[is.na(q_m), q_m := 0]

        if (!is_bm) {
          merged[, dq_dp := q_j * (as.numeric(plan_id == m) / lambda +
                                     ((lambda - 1) / lambda) * s_mg - q_m) *
                              alpha_i * rf_i]
        } else {
          merged[, dq_dp := fifelse(
            subsidized == 1L,
            fifelse(plan_id == m,
              alpha_i * (-rf_i) * q_j * ((1 - s_mg) * ((lambda - 1) / lambda - s_g)),
              alpha_i * (-rf_i) * q_j * (1 / lambda + (1 - s_mg) * ((lambda - 1) / lambda - s_g))
            ),
            q_j * (as.numeric(plan_id == m) / lambda +
                     ((lambda - 1) / lambda) * s_mg - q_m) * alpha_i * rf_i
          )]
        }

        dQ_k <- merged[, .(dQ = sum(w * dq_dp)), by = plan_id]
        dQ_vec <- setNames(dQ_k$dQ, dQ_k$plan_id)[pn_solve]

        dr_dp_m <- setNames(rep(0, J_local), pn_solve)

        for (d in demo_names) {
          gamma_d <- rs_coefs[[d]]
          raw_col <- sub("share_", "perc_", d)
          if (!(raw_col %in% names(merged))) next

          dD_dk <- merged[, .(dD = sum(w * dq_dp * get(raw_col))), by = plan_id]
          dD_vec <- setNames(dD_dk$dD, dD_dk$plan_id)[pn_solve]

          s_dk <- setNames(demo_sh[[d]], demo_sh$plan_id)[pn_solve]

          ds_dk_dp_m <- (dD_vec - s_dk * dQ_vec) / Q_vec
          dr_dp_m <- dr_dp_m + gamma_d * ds_dk_dp_m
        }
        rs_vec <- q$rs_p[pn_solve]
        dr_dp_m <- rs_vec * dr_dp_m

        pred_claims <- q$mc_result$predicted_claims[pn_solve]
        dc_dp_m <- theta_r * (pred_claims / rs_vec) * dr_dp_m

        sh <- unname(shares[pn_solve])
        rs <- unname(rs_vec)
        ua <- unname(util_adj)
        S_rs <- sum(rs * sh)
        S_u  <- sum(ua * sh)
        avg_p <- q$avg_prem

        E_col_m <- unname(E[, m_idx])
        dS_rs_dp_m <- sum(dr_dp_m * sh + rs * E_col_m)
        dS_u_dp_m <- sum(ua * E_col_m)

        dRA_dp_m <- setNames(rep(0, J_local), pn_solve)
        for (k_idx in seq_along(pn_solve)) {
          drs_ratio <- (dr_dp_m[k_idx] * S_rs - rs[k_idx] * dS_rs_dp_m) / S_rs^2
          dus_ratio <- (-ua[k_idx] * dS_u_dp_m) / S_u^2
          dRA_dp_m[k_idx] <- (drs_ratio - dus_ratio) * avg_p
        }

        dmc_dp[, m_idx] <- dc_dp_m * (1 - reins_local[pn_solve]) - dRA_dp_m
        dr_dp_mat[, m_idx] <- dr_dp_m

        # T4: elasticity curvature
        q_m_col <- q_mat[, m_idx]
        s_m_col <- s_mat[, m_idx]
        dq_dV_m <- q_mat * ((lambda - 1) / lambda * s_m_col - q_m_col)
        dq_dV_m[, m_idx] <- dq_dV_m[, m_idx] + q_mat[, m_idx] / lambda

        for (l_idx in seq_along(pn_solve)) {
          q_l_col <- q_mat[, l_idx]
          s_l_col <- s_mat[, l_idx]
          dq_l_dV_m <- dq_dV_m[, l_idx]

          C_lm <- (lambda - 1) / lambda^2 * s_l_col * (as.numeric(l_idx == m_idx) - s_m_col) - dq_l_dV_m
          d2q <- dq_dV_m * ((lambda - 1) / lambda * s_l_col - q_l_col) + q_mat * C_lm
          d2q[, l_idx] <- d2q[, l_idx] + dq_dV_m[, l_idx] / lambda

          if (!is_bm) {
            dE_col <- colSums(w_hh * arf2 * d2q) / total_weight
          } else {
            unsub_contrib <- colSums(w_hh * (!sub_hh) * arf2 * d2q) / total_weight
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

      # T3: Omega %*% dmc_dp
      J_mat <- J_mat + Omega %*% dmc_dp

      # T4: elasticity curvature
      J_mat <- J_mat + T4_mat

      # T6: d(ra_foc_l)/dp_m  (cheap FD using analytical dr_dp)
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

    V0_by_hh <- dt[plan_id == "Uninsured", .(V_0 = V[1]), by = household_number]

    ins_dt <- dt[plan_id != "Uninsured"]
    ins_dt[, V_scaled := V / lambda_cs]
    ins_dt[, max_V_scaled := max(V_scaled), by = household_number]
    ins_dt[, exp_V := exp(V_scaled - max_V_scaled)]
    ins_dt[, sum_exp_V := sum(exp_V), by = household_number]
    ins_dt[, log_D := max_V_scaled + log(sum_exp_V)]
    ins_dt[, log_D_lam := lambda_cs * log_D]

    alpha_vec <- compute_alpha_i(ins_dt, coefs_cell, STRUCTURAL_SPEC)
    ins_dt[, alpha_i := alpha_vec]

    ins_dt <- merge(ins_dt, V0_by_hh, by = "household_number", all.x = TRUE)
    ins_dt[is.na(V_0), V_0 := 0]

    hh_cs <- ins_dt[, .(
      log_D_lam = first(log_D_lam),
      V_0 = first(V_0),
      alpha_i = first(alpha_i),
      hh_weight = first(hh_weight)
    ), by = household_number]

    hh_cs[, mx := pmax(V_0, log_D_lam)]
    hh_cs[, cs := (1 / alpha_i) * (mx + log(exp(V_0 - mx) + exp(pmin(log_D_lam - mx, 500))))]

    total_weight <- sum(hh_cs$hh_weight)
    weighted_cs <- sum(hh_cs$hh_weight * hh_cs$cs) / total_weight
    weighted_cs
  }


  # Solve pricing equilibrium ------------------------------------------------
  solve_equilibrium <- function(cd_scenario, comm_sc, p_init) {

    fns <- build_foc_function(cd_scenario, coefs, comm_sc,
                               benchmark_plan, plan_attrs,
                               rs_coefs, claims_coefs, plan_chars_cell,
                               plan_avs, reins_vec, lambda, plan_ids_cell)

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

    dt_sol <- as.data.table(copy(cd_scenario))
    for (pn in names(p_sol)) {
      idx <- dt_sol$plan_id == pn
      if (sum(idx) == 0) next
      hh_prem_new <- (p_sol[pn] / RATING_FACTOR_AGE40) * dt_sol$rating_factor[idx]
      dt_sol$premium[idx] <- hh_prem_new / dt_sol$hh_size[idx]
    }
    recompute_prem_interactions(dt_sol[plan_id != "Uninsured"], STRUCTURAL_SPEC)

    util_sol <- compute_utility(dt_sol, coefs)
    se_sol <- tryCatch(
      compute_shares_and_elasticities(dt_sol, util_sol$V, lambda,
                                       benchmark_plan, plan_attrs, coefs,
                                       spec = STRUCTURAL_SPEC),
      error = function(e) NULL
    )
    shares_sol <- if (!is.null(se_sol)) se_sol$shares[plan_ids_cell] else setNames(rep(NA_real_, length(plan_ids_cell)), plan_ids_cell)

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
  build_scenario_data <- function(cell_data_base, comm_sc, tau = NULL) {
    cd <- as.data.table(copy(cell_data_base))

    for (pn in plan_ids_cell) {
      idx <- cd$plan_id == pn
      if (sum(idx) > 0 && "commission_broker" %in% names(cd)) {
        if ("any_agent" %in% names(cd)) {
          cd$commission_broker[idx] <- comm_sc[pn] * fifelse(cd$any_agent[idx] == 1L, cd$assisted[idx], 0L)
        } else {
          cd$commission_broker[idx] <- comm_sc[pn] * cd$assisted[idx]
        }
      }
    }

    if (!is.null(tau) && "any_agent" %in% names(cd)) {
      agent_hh <- cd[plan_id == "Uninsured" & any_agent == 1, .(household_number, p_nav)]
      if (nrow(agent_hh) == 0) {
        agent_hh <- cd[any_agent == 1, .(household_number, p_nav)]
        agent_hh <- unique(agent_hh, by = "household_number")
      }

      if (nrow(agent_hh) > 0) {
        agent_hh <- agent_hh[order(-p_nav)]
        n_switch <- ceiling(tau * nrow(agent_hh))
        switch_ids <- agent_hh$household_number[seq_len(n_switch)]

        cd[household_number %in% switch_ids, `:=`(
          commission_broker = 0,
          any_agent = 0L,
          channel_detail = "Navigator"
        )]

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

    cd[, `:=`(
      assisted_silver = assisted * silver,
      assisted_bronze = assisted * bronze,
      assisted_gold   = assisted * gold,
      assisted_plat   = assisted * platinum
    )]

    if ("v_hat" %in% names(cd) && "commission_broker" %in% names(cd)) {
      cd$v_hat_commission <- cd$v_hat * cd$commission_broker
    }

    cd
  }


  # Main solve loop ---------------------------------------------------------
  cat("Cell", r, y, "- plans:", length(plan_ids_cell), "\n")

  results_list <- list()

  # Scenario 1: Observed
  comm_obs_sc <- comm_obs[plan_ids_cell]
  cd_obs <- build_scenario_data(cell_data_base, comm_obs_sc)
  eq_obs <- solve_equilibrium(cd_obs, comm_obs_sc, p_obs)

  if (!is.null(eq_obs)) {
    cs_obs <- tryCatch(compute_consumer_surplus(eq_obs$dt_final %||% cd_obs, coefs),
                        error = function(e) NA_real_)
    results_list[[length(results_list) + 1]] <- tibble(
      region = r, year = y, scenario = "observed", tau = NA_real_,
      plan_id = plan_ids_cell,
      premium_obs = p_obs[plan_ids_cell],
      premium_cf = eq_obs$p[plan_ids_cell],
      premium_change = eq_obs$p[plan_ids_cell] - p_obs[plan_ids_cell],
      share_obs = setNames(sr_cell$share, sr_cell$plan_id)[plan_ids_cell],
      share_cf = eq_obs$shares[plan_ids_cell],
      mc = eq_obs$mc[plan_ids_cell],
      commission_pmpm = comm_obs_sc[plan_ids_cell],
      markup_cf = eq_obs$p[plan_ids_cell] - eq_obs$mc[plan_ids_cell],
      cs_weighted = cs_obs,
      nleqslv_termcd = eq_obs$sol$termcd,
      nleqslv_iter = eq_obs$sol$iter
    )
    cat("  observed - converged (nleqslv iter =", eq_obs$sol$iter, ")\n")
    p_warm <- eq_obs$p
  } else {
    cat("  observed - did not converge\n")
    p_warm <- p_obs
  }
  rm(cd_obs)


  # Scenario 2: Zero commission with tau gradient
  comm_zero <- setNames(rep(0, length(plan_ids_cell)), plan_ids_cell)

  for (tau in TAU_GRID) {
    sc_label <- paste0("zero_tau", sprintf("%.2f", tau))
    cd_tau <- build_scenario_data(cell_data_base, comm_zero, tau = tau)

    eq_tau <- solve_equilibrium(cd_tau, comm_zero, p_warm)

    if (!is.null(eq_tau)) {
      dt_cs <- eq_tau$dt_final
      if (is.null(dt_cs)) {
        dt_cs <- as.data.table(copy(cd_tau))
        for (pn in names(eq_tau$p)) {
          idx <- dt_cs$plan_id == pn
          if (sum(idx) == 0) next
          hh_prem_new <- (eq_tau$p[pn] / RATING_FACTOR_AGE40) * dt_cs$rating_factor[idx]
          dt_cs$premium[idx] <- hh_prem_new / dt_cs$hh_size[idx]
        }
        recompute_prem_interactions(dt_cs[plan_id != "Uninsured"], STRUCTURAL_SPEC)
      }
      cs_tau <- tryCatch(compute_consumer_surplus(dt_cs, coefs),
                          error = function(e) NA_real_)

      results_list[[length(results_list) + 1]] <- tibble(
        region = r, year = y, scenario = sc_label, tau = tau,
        plan_id = plan_ids_cell,
        premium_obs = p_obs[plan_ids_cell],
        premium_cf = eq_tau$p[plan_ids_cell],
        premium_change = eq_tau$p[plan_ids_cell] - p_obs[plan_ids_cell],
        share_obs = setNames(sr_cell$share, sr_cell$plan_id)[plan_ids_cell],
        share_cf = eq_tau$shares[plan_ids_cell],
        mc = eq_tau$mc[plan_ids_cell],
        commission_pmpm = comm_zero[plan_ids_cell],
        markup_cf = eq_tau$p[plan_ids_cell] - eq_tau$mc[plan_ids_cell],
        cs_weighted = cs_tau,
        nleqslv_termcd = eq_tau$sol$termcd,
        nleqslv_iter = eq_tau$sol$iter
      )
      cat("  ", sc_label, "- converged (nleqslv iter =", eq_tau$sol$iter, ")\n")
      p_warm <- eq_tau$p
    } else {
      cat("  ", sc_label, "- did not converge\n")
    }
    rm(cd_tau)
    gc(verbose = FALSE)
  }


  # Scenario 3: Uniform commission
  comm_uniform <- setNames(rep(mean_comm_pmpm, length(plan_ids_cell)), plan_ids_cell)
  cd_unif <- build_scenario_data(cell_data_base, comm_uniform)
  eq_unif <- solve_equilibrium(cd_unif, comm_uniform, p_obs)

  if (!is.null(eq_unif)) {
    cs_unif <- tryCatch(compute_consumer_surplus(eq_unif$dt_final %||% cd_unif, coefs),
                         error = function(e) NA_real_)
    results_list[[length(results_list) + 1]] <- tibble(
      region = r, year = y, scenario = "uniform", tau = NA_real_,
      plan_id = plan_ids_cell,
      premium_obs = p_obs[plan_ids_cell],
      premium_cf = eq_unif$p[plan_ids_cell],
      premium_change = eq_unif$p[plan_ids_cell] - p_obs[plan_ids_cell],
      share_obs = setNames(sr_cell$share, sr_cell$plan_id)[plan_ids_cell],
      share_cf = eq_unif$shares[plan_ids_cell],
      mc = eq_unif$mc[plan_ids_cell],
      commission_pmpm = comm_uniform[plan_ids_cell],
      markup_cf = eq_unif$p[plan_ids_cell] - eq_unif$mc[plan_ids_cell],
      cs_weighted = cs_unif,
      nleqslv_termcd = eq_unif$sol$termcd,
      nleqslv_iter = eq_unif$sol$iter
    )
    cat("  uniform - converged (nleqslv iter =", eq_unif$sol$iter, ")\n")
  } else {
    cat("  uniform - did not converge\n")
  }
  rm(cd_unif)

  # Return results
  if (length(results_list) > 0) {
    out <- bind_rows(results_list)
    cat("Cell", r, y, "- produced", nrow(out), "rows\n")
    out
  } else {
    cat("Cell", r, y, "- no scenarios converged\n")
    NULL
  }
}
