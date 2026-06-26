# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-23
## Date Edited:   2026-04-12
## Description:   Structural counterfactual simulation. run_cf_cell() solves the
##                pricing equilibrium for one region-year cell (endogenous MC(p):
##                demographics -> risk scores -> claims -> RA -> MC inside each FOC
##                evaluation, Saltzman RAND JE 2021; broker-to-navigator tau
##                gradient). The driver below loops it over all cells, in-process.

# run_cf_cell ---------------------------------------------------------------
#
# Runs counterfactual scenarios for one region-year cell.
# Returns a tibble of results, or NULL if the cell can't be processed.

run_cf_cell <- function(r, y, seed, sample_frac, hhs_raw,
                        plan_choice, supply_results, coefs,
                        commission_lookup, rs_coefs, claims_coefs,
                        reins_df, STRUCTURAL_SPEC) {

  TAU_GRID <- c(0, 0.25, 0.5, 0.75, 1.0)
  # Commission-level sweep: brokers stay brokers, commissions scaled down.
  SCALE_GRID <- c(0.25, 0.5, 0.75)
  # Commission utility terms — zeroed for the non-commission welfare metric
  # (cs_nocomm), so welfare comparisons don't rest on commission steering being a
  # genuine preference rather than a wedge.
  COMM_TERMS <- c("commission_broker")

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

  build_result <- build_structural(plans_cell, hhs_raw, sample_frac,
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
    AV          = unname(pa$av),
    HMO         = pa$hmo,
    trend       = y - 2014L,
    Anthem      = as.integer(str_detect(plan_ids_cell, "^ANT")),
    Blue_Shield = as.integer(str_detect(plan_ids_cell, "^BS")),
    Health_Net  = as.integer(str_detect(plan_ids_cell, "^HN")),
    Kaiser      = as.integer(str_detect(plan_ids_cell, "^KA")),
    Molina            = as.integer(str_detect(plan_ids_cell, "^MOL")),
    LA_Care           = as.integer(str_detect(plan_ids_cell, "^LA")),
    SHARP             = as.integer(str_detect(plan_ids_cell, "^SH")),
    Chinese_Community = as.integer(str_detect(plan_ids_cell, "^CC")),
    Oscar             = as.integer(str_detect(plan_ids_cell, "^OSC")),
    Western           = as.integer(str_detect(plan_ids_cell, "^WEST")),
    Valley            = as.integer(str_detect(plan_ids_cell, "^VAL"))
  )

  # Reinsurance factors for this year
  rf_year <- reins_df %>% filter(year == y)
  reins_vec <- sapply(plan_ids_cell, function(pn) {
    rf <- rf_year$reins_factor[rf_year$plan_id == pn]
    if (length(rf) == 0) return(0)
    mean(rf, na.rm = TRUE)
  })

  # omega cost residual ------------------------------------------------------
  # Plan-level structural cost shock (BLP/Nevo): MC(p) = compute_mc(p) + omega,
  # held FIXED across scenarios. omega is CALIBRATED below at the observed scenario
  # so the CF's own FOC holds exactly at observed prices (omega = -solve(Omega, f0)
  # with f0 = fn(p_obs) at omega = 0; equals mc_foc - mc_structural but from the CF's
  # own Omega, so it is self-consistent even though the cell is ill-conditioned). The
  # closures below read omega_vec by lexical scope, so it must exist before they run;
  # it is overwritten in the observed-scenario block. Counterfactual responses come
  # only from how the risk pool moves the compute_mc(p) part.
  omega_vec <- setNames(rep(0, length(plan_ids_cell)), plan_ids_cell)

  # Mean observed commission for uniform scenario
  mean_comm_pmpm <- mean(sr_cell$commission_pmpm[sr_cell$commission_pmpm > 0], na.rm = TRUE)
  if (is.na(mean_comm_pmpm)) mean_comm_pmpm <- 0

  # update_premiums ---------------------------------------------------------
  # Re-level the demand `premium` column for a candidate posted-premium vector,
  # reproducing the builder's NET premium (helpers/supply.R): posted -> age-40-
  # normalized HH premium -> OOP after subsidy and penalty offset -> per-member,
  # per-$100. Single source of truth for both the FOC evaluation and the
  # post-solution outcome recompute.
  #
  # ENDOGENOUS SUBSIDY (Saltzman RAND JE 2021, Eq. 8). The APTC moves with the
  # benchmark (2nd-cheapest silver) premium: subsidy = max(0, premiumSLC(p) - zeta),
  # where zeta_it = SLC_contribution is the fixed income cap (carried from the data
  # build; NA for subsidy-ineligible HHs). We anchor the HH benchmark premium to its
  # data-build value premiumSLC and add the HH-scaled change in the benchmark plan's
  # posted price, so the observed scenario reproduces the data subsidy exactly
  # (delta = 0 -> subsidy = pmax(0, premiumSLC - zeta), the data-build formula) while
  # counterfactual benchmark moves feed through. Benchmark IDENTITY is held fixed at
  # the baseline 2nd-cheapest silver; only its price moves. This makes the level
  # consistent with the 4-case derivative already in compute_shares_and_elasticities
  # and the FOC Jacobian (own-benchmark net price flat; other plans fall by rf_i as
  # the benchmark rises). p_obs and benchmark_plan are closed over from run_cf_cell.
  update_premiums <- function(dt, p_vec) {
    rf_i <- dt$rating_factor / RATING_FACTOR_AGE40
    if (!is.na(benchmark_plan) && benchmark_plan %in% names(p_vec)) {
      d_bench       <- p_vec[[benchmark_plan]] - p_obs[[benchmark_plan]]
      premiumSLC_cf <- dt$premiumSLC + rf_i * d_bench
      sub_endog     <- pmax(0, premiumSLC_cf - dt$SLC_contribution)
      # Gate on the SAME `subsidized` flag the 4-case elasticity derivative uses, so
      # the level and the derivative apply to the identical household set. subsidized==1L
      # now implies finite SLC_contribution, so sub_endog is finite where it's used.
      dt[, subsidy_cf := fifelse(subsidized == 1L, sub_endog, adj_subsidy)]
    } else {
      dt[, subsidy_cf := adj_subsidy]   # no benchmark silver -> fall back to baseline
    }

    for (pn in names(p_vec)) {
      idx <- which(dt$plan_id == pn)
      if (length(idx) == 0) next
      premium_hh <- (p_vec[pn] / RATING_FACTOR_AGE40) * dt$rating_factor[idx]
      oop <- pmax(premium_hh - dt$subsidy_cf[idx], 0) - dt$penalty[idx] / 12
      set(dt, i = idx, j = "premium", value = oop / dt$hh_size[idx] / 100)
    }
    recompute_prem_interactions(dt, STRUCTURAL_SPEC)
  }

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
    ins_own <- sub("_.*", "", plan_ids_cell)        # ownership matrix: 1 if same firm
    own_mat <- outer(ins_own, ins_own, "==") * 1L
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
      # Enrollment-weighted statewide average premium (ACA RA scale), not a plan mean.
      avg_p <- weighted.mean(p_vec, shares, na.rm = TRUE)
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
           mc_p = mc_res$mc[pn] + omega_vec[pn], rs_p = rs, ra_foc_p = ra_foc,
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

      # Multi-product Bertrand FOC: equation j contracts ds_k/dp_j = t(E)[j,k], so
      # transpose the elasticity before forming Omega (matches 2_pricing.R).
      Omega <- -own_mat[pn_solve, pn_solve] * t(q$elast_mat)

      # Cache for jac
      cache$dt <- dt
      cache$p_vec <- p_vec
      cache$pn_solve <- pn_solve
      cache$q <- q
      cache$Omega <- Omega

      # FOC residual
      if (!is.null(q$broker_elast)) {
        Omega_B <- -own_mat[pn_solve, pn_solve] * t(q$broker_elast)
        comm_vec <- comm_scenario[pn_solve]
        resid <- q$shares + q$ra_foc_p[pn_solve] -
          as.vector(Omega %*% (p_vec - q$mc_p)) +
          as.vector(Omega_B %*% comm_vec)
      } else {
        resid <- q$shares + q$ra_foc_p[pn_solve] -
          as.vector(Omega %*% (p_vec - q$mc_p))
      }

      # Track fn evaluations (per-iteration printing removed — it flooded the
      # console under the numerical Jacobian; per-scenario summaries below suffice).
      cache$iter <- cache$iter + 1L

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
          # A plan whose choice prob underflows to ~0 (very negative utility when the
          # solver pushes its price up) gives Q_vec = 0 and a 0/0 = NaN here, which
          # propagates to a non-finite Jacobian — this is what broke the analytical
          # jac on the larger (23-plan) cells. An empty plan has no enrollment-weighted
          # composition, so its demographic-share derivative is 0.
          ds_dk_dp_m[!is.finite(ds_dk_dp_m) | Q_vec < 1e-10] <- 0
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
        # Backstop: a non-finite MC derivative (e.g. from rs_vec underflow or an
        # RA denominator collapsing at an extreme trial price) would make the whole
        # Jacobian non-finite and abort the solve. Zero it instead so Newton can
        # step away from the degenerate point rather than crash.
        dmc_dp[!is.finite(dmc_dp[, m_idx]), m_idx] <- 0
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

      T1T2_mat <- J_mat                          # E - Omega so far

      # T3: Omega %*% dmc_dp
      T3_mat <- Omega %*% dmc_dp
      J_mat <- J_mat + T3_mat

      # T4: elasticity curvature
      J_mat <- J_mat + T4_mat

      # T6: d(ra_foc_l)/dp_m  (cheap FD using analytical dr_dp)
      T6_mat <- matrix(0, J_local, J_local, dimnames = list(pn_solve, pn_solve))
      eps_ra <- 1e-5
      for (m_idx in seq_along(pn_solve)) {
        shares_plus <- shares + E[, m_idx] * eps_ra
        rs_plus <- q$rs_p[pn_solve] + dr_dp_mat[, m_idx] * eps_ra
        ra_foc_plus <- compute_ra_foc(rs_plus, shares_plus, plan_avs, q$avg_prem,
                                       E, own_mat[pn_solve, pn_solve])
        T6_mat[, m_idx] <- (ra_foc_plus[pn_solve] - q$ra_foc_p[pn_solve]) / eps_ra
        J_mat[, m_idx] <- J_mat[, m_idx] + T6_mat[, m_idx]
      }

      if (isTRUE(getOption("cf.jac.debug", FALSE)))
        assign(".cf_jac_terms",
               list(T1T2 = T1T2_mat, T3 = T3_mat, T4 = T4_mat, T6 = T6_mat,
                    dmc_dp = dmc_dp, Omega = Omega), envir = globalenv())

      J_mat
    }

    list(fn = fn, jac = jac, cache = cache)
  }

  # Consumer surplus --------------------------------------------------------
  # welfare_drop: column names zeroed before utility is computed, so their
  # coefficients do not enter the welfare log-sum. Used to value scenarios on the
  # NON-commission part of utility (welfare_drop = the commission terms), which
  # makes the welfare comparison robust to whether commission steering reflects a
  # genuine preference or a behavioral wedge.
  # deterministic = TRUE replaces the logit log-sum with each household's best
  # available option (max over inside plans vs the outside good). Used for the
  # planner first-best: everyone in their best-fit plan, no steering, no logit
  # noise. With FALSE (default) this is the standard McFadden inclusive value.
  compute_consumer_surplus <- function(cell_data, coefs_cell, welfare_drop = character(),
                                       deterministic = FALSE) {
    coef_map_cs <- setNames(coefs_cell$estimate, coefs_cell$term)
    lambda_cs <- coef_map_cs[["lambda"]]

    if (length(welfare_drop) > 0) {
      cell_data <- as.data.table(copy(cell_data))
      for (cn in intersect(welfare_drop, names(cell_data))) cell_data[[cn]] <- 0
    }

    util <- compute_utility(cell_data, coefs_cell)
    V <- util$V

    dt <- as.data.table(cell_data)
    dt[, V := V]

    V0_by_hh <- dt[plan_id == "Uninsured", .(V_0 = V[1]), by = household_number]

    ins_dt <- dt[plan_id != "Uninsured"]
    if (deterministic) {
      # Best inside plan per HH (no nesting smoothing, no logit error).
      ins_dt[, log_D_lam := max(V), by = household_number]
    } else {
      ins_dt[, V_scaled := V / lambda_cs]
      ins_dt[, max_V_scaled := max(V_scaled), by = household_number]
      ins_dt[, exp_V := exp(V_scaled - max_V_scaled)]
      ins_dt[, sum_exp_V := sum(exp_V), by = household_number]
      ins_dt[, log_D := max_V_scaled + log(sum_exp_V)]
      ins_dt[, log_D_lam := lambda_cs * log_D]
    }

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
    # alpha_i is dV/dp (negative). The McFadden surplus divides the log-sum by the
    # marginal utility of income, which is the POSITIVE quantity -alpha_i. Use
    # abs(alpha_i) so CS is a positive dollar value and welfare differences carry
    # the intuitive sign (assistance / lower premiums raise CS).
    if (deterministic) {
      # Each HH realizes the better of its best inside plan and the outside option.
      hh_cs[, cs := (1 / abs(alpha_i)) * mx]
    } else {
      hh_cs[, cs := (1 / abs(alpha_i)) * (mx + log(exp(V_0 - mx) + exp(pmin(log_D_lam - mx, 500))))]
    }

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

    # --- Jacobian diagnostic (options(cf.jac.debug = TRUE)): compare the analytical
    # fns$jac against a central-difference numerical Jacobian at p_init, then stop. ---
    if (isTRUE(getOption("cf.jac.debug", FALSE))) {
      Ja <- fns$jac(p_init)                  # cache populated by f0 above (p_init)
      n <- length(p_init)
      Jn <- matrix(0, n, n, dimnames = list(names(p_init), names(p_init)))
      hh <- 1e-4 * pmax(abs(p_init), 1)
      for (j in seq_len(n)) {
        pp <- p_init; pp[j] <- pp[j] + hh[j]
        pm <- p_init; pm[j] <- pm[j] - hh[j]
        Jn[, j] <- (fns$fn(pp) - fns$fn(pm)) / (2 * hh[j])
      }
      D <- Ja - Jn
      cat("\n=== JAC DEBUG cell", r, y, " (J =", n, ") ===\n")
      cat("max|A-N| =", signif(max(abs(D)), 4),
          "  ||A-N||_F =", signif(sqrt(sum(D^2)), 4),
          "  ||N||_F =", signif(sqrt(sum(Jn^2)), 4), "\n")
      cat("per-column ||A-N|| (perturbed plan m):\n")
      for (j in seq_len(n))
        cat(sprintf("  col %2d %-10s  ||dA-N||=%10.4g   ||N||=%10.4g\n",
            j, names(p_init)[j], sqrt(sum(D[, j]^2)), sqrt(sum(Jn[, j]^2))))
      cat("top 12 worst entries [row,col]:\n")
      ord <- order(abs(D), decreasing = TRUE)[seq_len(min(12, n * n))]
      for (idx in ord) {
        i <- ((idx - 1) %% n) + 1; j <- ((idx - 1) %/% n) + 1
        cat(sprintf("  [%2d,%2d] %-9s<-%-9s  A=%11.4g  N=%11.4g  d=%11.4g\n",
            i, j, names(p_init)[i], names(p_init)[j], Ja[i, j], Jn[i, j], D[i, j]))
      }
      if (exists(".cf_jac_terms", envir = globalenv())) {
        tm <- get(".cf_jac_terms", envir = globalenv())
        cat("per-term contribution (true total diag mean ~", signif(mean(diag(Jn)), 3), "):\n")
        for (nm in c("T1T2", "T3", "T4", "T6")) {
          dg <- diag(tm[[nm]])
          cat(sprintf("  %-5s diag mean=%11.4g  ||diag||=%11.4g  ||mat||_F=%11.4g\n",
              nm, mean(dg), sqrt(sum(dg^2)), sqrt(sum(tm[[nm]]^2))))
        }
        cat("  dmc_dp: diag mean=", signif(mean(diag(tm$dmc_dp)), 4),
            " range [", signif(min(tm$dmc_dp), 4), ",", signif(max(tm$dmc_dp), 4), "]\n")
      }
      if (interactive()) stop("cf.jac.debug: comparison printed for cell ", r, " ", y)
      else quit(save = "no")
    }

    # Broyden quasi-Newton with a Levenberg-Marquardt hook step. The analytical
    # fns$jac is incorrect (the T4 curvature term is ~200x off — deferred), so we
    # build the Jacobian from fn evaluations instead. The cells are ill-conditioned
    # (near-substitute plans give cond(J) ~ 1e6, and the high-tau steering scenarios
    # are outright singular), so a plain Broyden step fails to move. global = "hook"
    # damps the step (solve (J'J + lambda I) dx), giving a minimum-norm step even
    # when J is singular — it picks a particular equilibrium along the economically
    # uninformative flat direction. maxit is capped low: the residual plateaus at the
    # conditioning floor well before 150 iters, and the |f| < 0.05 acceptance below
    # catches the plateaued solutions.
    sol <- tryCatch(
      nleqslv(x = p_init, fn = fns$fn, method = "Broyden", global = "hook",
              control = list(maxit = 100, xtol = 1e-6, ftol = 1e-8)),
      error = function(e) { cat("    nleqslv error:", conditionMessage(e), "\n"); NULL }
    )

    # --- Conditioning / smoothness diagnostic at the plateau (options(cf.cond.debug)) ---
    if (isTRUE(getOption("cf.cond.debug", FALSE)) && !is.null(sol)) {
      p_star <- sol$x; f_star <- fns$fn(p_star); n <- length(p_star)
      cat("\n=== COND DEBUG cell", r, y, " (J =", n, ") ===\n")
      cat("solver: termcd =", sol$termcd, " final |f| =", signif(sqrt(sum(f_star^2)), 4),
          " iters =", sol$iter, "\n")
      jac_at_h <- function(hrel) {
        J <- matrix(0, n, n); hv <- hrel * pmax(abs(p_star), 1)
        for (j in seq_len(n)) {
          pp <- p_star; pp[j] <- pp[j] + hv[j]
          pm <- p_star; pm[j] <- pm[j] - hv[j]
          J[, j] <- (fns$fn(pp) - fns$fn(pm)) / (2 * hv[j])
        }
        J
      }
      J2 <- jac_at_h(1e-2); J4 <- jac_at_h(1e-4); J6 <- jac_at_h(1e-6)
      cat("SMOOTHNESS (Jacobian vs FD step; large shift as h shrinks => non-smooth/kink):\n")
      cat("  max|J(1e-2)-J(1e-4)| =", signif(max(abs(J2 - J4)), 4),
          "   max|J(1e-4)-J(1e-6)| =", signif(max(abs(J4 - J6)), 4),
          "   ||J(1e-4)||_F =", signif(sqrt(sum(J4^2)), 4), "\n")
      sv <- svd(J4)$d
      cat("CONDITIONING: cond(J) =", signif(max(sv) / min(sv), 4),
          "  min sing =", signif(min(sv), 4), "  max sing =", signif(max(sv), 4), "\n")
      if (!is.na(benchmark_plan) && benchmark_plan %in% names(p_star)) {
        dtt <- as.data.table(cd_scenario)
        hhd <- dtt[, .(premiumSLC = first(premiumSLC), SLC = first(SLC_contribution),
                       rf = first(rating_factor)), by = household_number]
        d_bench <- p_star[[benchmark_plan]] - p_obs[[benchmark_plan]]
        hhd[, gap := premiumSLC + (rf / RATING_FACTOR_AGE40) * d_bench - SLC]  # subsidy = pmax(0, gap)
        elig <- hhd[is.finite(SLC)]
        cat("SUBSIDY KINK exposure at p* (eligible HH =", nrow(elig), "of", nrow(hhd),
            "; benchmark moved $", signif(d_bench, 4), "):\n")
        cat("  |gap| < $5/mo:", sum(abs(elig$gap) < 5),
            "   |gap| < $20/mo:", sum(abs(elig$gap) < 20),
            "   clipped (gap<=0):", sum(elig$gap <= 0),
            "   positive:", sum(elig$gap > 0), "\n")
      } else cat("SUBSIDY KINK: no benchmark silver in this cell\n")
      if (interactive()) stop("cf.cond.debug done for cell ", r, " ", y)
      else quit(save = "no")
    }

    if (!is.null(sol) && sol$termcd > 2) {
      f_norm <- sqrt(sum(sol$fvec^2))
      cat("    nleqslv termcd:", sol$termcd, ", |f|:", round(f_norm, 6), "\n")
      if (f_norm >= 0.05) return(NULL)
      cat("    Accepting with small residual\n")
    }
    if (is.null(sol)) return(NULL)

    p_sol <- sol$x

    dt_sol <- update_premiums(as.data.table(copy(cd_scenario)), p_sol)

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
                          demo_sol, shares_sol,
                          weighted.mean(p_sol, shares_sol, na.rm = TRUE),
                          plan_avs, reins_vec)
    # Same omega cost residual carried into the reported MC (see run_cf_cell head).
    mc_eff <- mc_sol$mc + omega_vec[names(mc_sol$mc)]

    list(sol = sol, p = p_sol, mc = mc_eff, shares = shares_sol,
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

    # Steering terms — same definition as build_structural: navigator (non-broker)
    # and broker each carry their own metal terms; broker-only commission (set
    # above); v_hat coalesced. Converting a broker to navigator (any_agent -> 0)
    # therefore moves its metal steering from broker_* onto assisted_*.
    if ("any_agent" %in% names(cd)) {
      cd[, nonbroker := assisted * fifelse(any_agent == 1L, 0L, 1L, na = 1L)]
      cd[, broker    := assisted * fifelse(any_agent == 1L, 1L, 0L, na = 0L)]
    } else {
      cd[, nonbroker := assisted]
      cd[, broker    := 0L]
    }
    cd[, `:=`(
      assisted_silver  = nonbroker * silver,
      assisted_bronze  = nonbroker * bronze,
      assisted_gold    = nonbroker * gold,
      assisted_plat    = nonbroker * platinum,
      broker_silver    = broker * silver,
      broker_bronze    = broker * bronze,
      assisted_premium = nonbroker * premium,
      broker_premium   = broker * premium
    )]
    # nonbroker / broker kept: raw_demo for the premium interactions, which
    # recompute_prem_interactions updates as premiums move in the solve.

    # Pareto-dominated flag rides on cd from build_structural (premium-independent,
    # so it never changes in the solve); rebuild the channel interactions for the
    # reassigned channels.
    if ("dominated_plan" %in% names(cd)) {
      cd[, nav_dominated    := nonbroker * dominated_plan]
      cd[, broker_dominated := broker    * dominated_plan]
    }

    cd
  }


  # Main solve loop ---------------------------------------------------------
  cat("Cell", r, y, "- plans:", length(plan_ids_cell), "\n")

  results_list <- list()

  # Scenario 1: Observed
  comm_obs_sc <- comm_obs[plan_ids_cell]
  cd_obs <- build_scenario_data(cell_data_base, comm_obs_sc)

  # Calibrate omega so the CF's own FOC holds exactly at observed prices. With
  # omega = 0, f0 = fn(p_obs); the FOC is linear in MC, so adding omega shifts the
  # residual by Omega %*% omega. omega = -solve(Omega, f0) zeroes fn(p_obs), making
  # p_obs a fixed point of the observed scenario; the cost shock then carries into the
  # counterfactual re-solves. Uses the CF's own Omega at p_obs, so it is self-consistent
  # even in ill-conditioned cells (borrowing mc_foc from 2_pricing left a ~0.05 residual
  # that the cond~2e6 geometry amplified into large price swings).
  fns_cal <- build_foc_function(cd_obs, coefs, comm_obs_sc, benchmark_plan, plan_attrs,
                                rs_coefs, claims_coefs, plan_chars_cell, plan_avs,
                                reins_vec, lambda, plan_ids_cell)
  f0 <- fns_cal$fn(p_obs[plan_ids_cell])
  om_sol <- tryCatch(as.numeric(-solve(fns_cal$cache$Omega, f0)),
                     error = function(e) rep(0, length(plan_ids_cell)))
  om_sol[!is.finite(om_sol)] <- 0
  omega_vec <- setNames(om_sol, plan_ids_cell)
  rm(fns_cal, f0, om_sol)

  eq_obs <- solve_equilibrium(cd_obs, comm_obs_sc, p_obs)

  if (!is.null(eq_obs)) {
    obs_dt <- eq_obs$dt_final %||% cd_obs
    cs_obs <- tryCatch(compute_consumer_surplus(obs_dt, coefs),
                        error = function(e) NA_real_)
    cs_obs_nc <- tryCatch(compute_consumer_surplus(obs_dt, coefs, welfare_drop = COMM_TERMS),
                          error = function(e) NA_real_)
    wf_obs <- tryCatch(scenario_welfare(obs_dt, coefs, lambda, y, CS_TABLE),
                       error = function(e) c(nav = NA_real_, obj = NA_real_))
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
      cs_nocomm = cs_obs_nc,
      cs_welfare_nav = wf_obs[["nav"]],
      cs_welfare_obj = wf_obs[["obj"]],
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
        dt_cs <- update_premiums(as.data.table(copy(cd_tau)), eq_tau$p)
      }
      cs_tau <- tryCatch(compute_consumer_surplus(dt_cs, coefs),
                          error = function(e) NA_real_)
      wf_tau <- tryCatch(scenario_welfare(dt_cs, coefs, lambda, y, CS_TABLE),
                         error = function(e) c(nav = NA_real_, obj = NA_real_))

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
        cs_welfare_nav = wf_tau[["nav"]],
        cs_welfare_obj = wf_tau[["obj"]],
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
    wf_unif <- tryCatch(scenario_welfare(eq_unif$dt_final %||% cd_unif, coefs, lambda, y, CS_TABLE),
                        error = function(e) c(nav = NA_real_, obj = NA_real_))
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
      cs_welfare_nav = wf_unif[["nav"]],
      cs_welfare_obj = wf_unif[["obj"]],
      nleqslv_termcd = eq_unif$sol$termcd,
      nleqslv_iter = eq_unif$sol$iter
    )
    cat("  uniform - converged (nleqslv iter =", eq_unif$sol$iter, ")\n")
  } else {
    cat("  uniform - did not converge\n")
  }
  rm(cd_unif)

  # Scenario 4: Commission-level sweep (brokers intact, commissions scaled down)
  for (sc in SCALE_GRID) {
    sc_label <- paste0("scale_", sprintf("%.2f", sc))
    comm_scaled <- setNames(comm_obs_sc * sc, plan_ids_cell)
    cd_sc <- build_scenario_data(cell_data_base, comm_scaled)
    eq_sc <- solve_equilibrium(cd_sc, comm_scaled, p_obs)
    if (!is.null(eq_sc)) {
      sc_dt <- eq_sc$dt_final %||% cd_sc
      cs_sc    <- tryCatch(compute_consumer_surplus(sc_dt, coefs),
                           error = function(e) NA_real_)
      cs_sc_nc <- tryCatch(compute_consumer_surplus(sc_dt, coefs, welfare_drop = COMM_TERMS),
                           error = function(e) NA_real_)
      wf_sc <- tryCatch(scenario_welfare(sc_dt, coefs, lambda, y, CS_TABLE),
                        error = function(e) c(nav = NA_real_, obj = NA_real_))
      results_list[[length(results_list) + 1]] <- tibble(
        region = r, year = y, scenario = sc_label, tau = NA_real_,
        plan_id = plan_ids_cell,
        premium_obs = p_obs[plan_ids_cell],
        premium_cf = eq_sc$p[plan_ids_cell],
        premium_change = eq_sc$p[plan_ids_cell] - p_obs[plan_ids_cell],
        share_obs = setNames(sr_cell$share, sr_cell$plan_id)[plan_ids_cell],
        share_cf = eq_sc$shares[plan_ids_cell],
        mc = eq_sc$mc[plan_ids_cell],
        commission_pmpm = comm_scaled[plan_ids_cell],
        markup_cf = eq_sc$p[plan_ids_cell] - eq_sc$mc[plan_ids_cell],
        cs_weighted = cs_sc,
        cs_nocomm = cs_sc_nc,
        cs_welfare_nav = wf_sc[["nav"]],
        cs_welfare_obj = wf_sc[["obj"]],
        nleqslv_termcd = eq_sc$sol$termcd,
        nleqslv_iter = eq_sc$sol$iter
      )
      cat("  ", sc_label, "- converged (nleqslv iter =", eq_sc$sol$iter, ")\n")
    } else {
      cat("  ", sc_label, "- did not converge\n")
    }
    rm(cd_sc); gc(verbose = FALSE)
  }

  # Scenario 5: Commissions aligned with consumer value (re-allocation, not level)
  # Value score = plan-level mean of the NON-commission indirect utility across the
  # cell's households (the model's own measure of consumer fit). Commission is set
  # proportional to that score and rescaled to hold the observed commission budget
  # fixed (sum comm * observed-share), so this isolates ALIGNMENT, not the level.
  # Brokers then steer toward high-fit plans; welfare is read on cs_nocomm.
  cd_base_nc <- as.data.table(copy(cell_data_base))
  for (cn in intersect(COMM_TERMS, names(cd_base_nc))) cd_base_nc[[cn]] <- 0
  cd_base_nc[, V_nc := compute_utility(cd_base_nc, coefs)$V]
  plan_val <- cd_base_nc[plan_id != "Uninsured", .(val = mean(V_nc, na.rm = TRUE)), by = plan_id]
  val_vec <- setNames(plan_val$val, plan_val$plan_id)[plan_ids_cell]
  val_vec[!is.finite(val_vec)] <- min(val_vec[is.finite(val_vec)], na.rm = TRUE)
  obs_share <- setNames(sr_cell$share, sr_cell$plan_id)[plan_ids_cell]
  obs_share[!is.finite(obs_share)] <- 0
  w_val  <- val_vec - min(val_vec)                         # shift to non-negative
  budget <- sum(comm_obs_sc * obs_share, na.rm = TRUE)     # observed commission budget
  denom  <- sum(w_val * obs_share, na.rm = TRUE)
  comm_aligned <- setNames(
    as.numeric(if (denom > 0) w_val * (budget / denom) else comm_obs_sc),
    plan_ids_cell)
  cd_al <- build_scenario_data(cell_data_base, comm_aligned)
  eq_al <- solve_equilibrium(cd_al, comm_aligned, p_obs)
  if (!is.null(eq_al)) {
    al_dt <- eq_al$dt_final %||% cd_al
    cs_al    <- tryCatch(compute_consumer_surplus(al_dt, coefs),
                         error = function(e) NA_real_)
    cs_al_nc <- tryCatch(compute_consumer_surplus(al_dt, coefs, welfare_drop = COMM_TERMS),
                         error = function(e) NA_real_)
    wf_al <- tryCatch(scenario_welfare(al_dt, coefs, lambda, y, CS_TABLE),
                      error = function(e) c(nav = NA_real_, obj = NA_real_))
    results_list[[length(results_list) + 1]] <- tibble(
      region = r, year = y, scenario = "aligned", tau = NA_real_,
      plan_id = plan_ids_cell,
      premium_obs = p_obs[plan_ids_cell],
      premium_cf = eq_al$p[plan_ids_cell],
      premium_change = eq_al$p[plan_ids_cell] - p_obs[plan_ids_cell],
      share_obs = obs_share,
      share_cf = eq_al$shares[plan_ids_cell],
      mc = eq_al$mc[plan_ids_cell],
      commission_pmpm = comm_aligned[plan_ids_cell],
      markup_cf = eq_al$p[plan_ids_cell] - eq_al$mc[plan_ids_cell],
      cs_weighted = cs_al,
      cs_nocomm = cs_al_nc,
      cs_welfare_nav = wf_al[["nav"]],
      cs_welfare_obj = wf_al[["obj"]],
      nleqslv_termcd = eq_al$sol$termcd,
      nleqslv_iter = eq_al$sol$iter
    )
    cat("   aligned - converged (nleqslv iter =", eq_al$sol$iter, ")\n")
  } else {
    cat("   aligned - did not converge\n")
  }
  # Scenario 6: Planner first-best (welfare frontier). Each household assigned its
  # best-fit plan on NON-commission utility at observed prices, valued
  # deterministically (no steering, no logit noise). The reference the aligned
  # commission CF tries to approach; reported on the non-commission metric only.
  cs_planner <- tryCatch(
    compute_consumer_surplus(cd_base_nc, coefs, welfare_drop = COMM_TERMS, deterministic = TRUE),
    error = function(e) NA_real_)
  # Planner-assignment shares: each HH -> argmax non-commission utility (or outside).
  pl_in <- cd_base_nc[plan_id != "Uninsured",
                      .(best_plan = plan_id[which.max(V_nc)], wt = first(hh_weight),
                        Vbest = max(V_nc, na.rm = TRUE)), by = household_number]
  pl_v0 <- cd_base_nc[plan_id == "Uninsured", .(household_number, V0 = V_nc)]
  pl_in <- merge(pl_in, pl_v0, by = "household_number", all.x = TRUE)
  pl_in[is.na(V0), V0 := 0]
  pl_in[, chosen := fifelse(Vbest >= V0, best_plan, "Uninsured")]
  pl_sh <- pl_in[, .(w = sum(wt)), by = chosen]
  planner_share <- setNames(pl_sh$w / sum(pl_sh$w), pl_sh$chosen)[plan_ids_cell]
  planner_share[!is.finite(planner_share)] <- 0
  results_list[[length(results_list) + 1]] <- tibble(
    region = r, year = y, scenario = "planner", tau = NA_real_,
    plan_id = plan_ids_cell,
    premium_obs = p_obs[plan_ids_cell],
    premium_cf = p_obs[plan_ids_cell],
    premium_change = 0,
    share_obs = setNames(sr_cell$share, sr_cell$plan_id)[plan_ids_cell],
    share_cf = planner_share,
    mc = NA_real_,
    commission_pmpm = 0,
    markup_cf = NA_real_,
    cs_weighted = NA_real_,
    cs_nocomm = cs_planner,
    nleqslv_termcd = NA_integer_,
    nleqslv_iter = NA_integer_
  )
  cat("   planner first-best - cs_nocomm =", round(cs_planner, 2), "\n")

  rm(cd_al, cd_base_nc); gc(verbose = FALSE)

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
tasks <- lapply(seq_len(nrow(cells)), function(i) {
  hhs <- hh_split_cf[[paste0(cells$region[i], ".", cells$year[i])]]
  list(r = cells$region[i], y = cells$year[i], seed = cell_seeds[i],
       hhs = if (is.null(hhs) || nrow(hhs) == 0) NULL else as.data.frame(hhs))
})
rm(hh_split_cf); gc(verbose = FALSE)

# Solve one cell. Returns NULL (not an error) for empty or failed cells.
run_one_cf <- function(task) {
  if (is.null(task$hhs)) return(NULL)
  tryCatch(
    run_cf_cell(task$r, task$y, task$seed, SAMPLE_FRAC, task$hhs,
                plan_choice, supply_results, coefs, commission_lookup,
                rs_coefs, claims_coefs, reins_df, STRUCTURAL_SPEC),
    error = function(e) { cat("  ERROR cell", task$r, task$y, ":", conditionMessage(e), "\n"); NULL }
  )
}

t_start <- Sys.time()

# Run cells in parallel (PSOCK, load-balanced); fall back to serial if the
# cluster can't be created. Each worker sources the helpers and receives
# run_cf_cell plus the small reference objects; household data rides with the task.
n_workers <- max(1L, parallel::detectCores() - 2L)
cl <- tryCatch(parallel::makeCluster(n_workers, type = "PSOCK"), error = function(e) NULL)

if (!is.null(cl)) {
  cat("  Parallel:", n_workers, "workers\n")
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
    "reins_df", "STRUCTURAL_SPEC", "CS_TABLE"))
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
comm_scenarios <- cf_results %>% filter(scenario %in% c("aligned", "planner") | str_detect(scenario, "^scale_"))
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
