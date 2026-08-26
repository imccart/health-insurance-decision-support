# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   run_cf_cell() — solves the pricing equilibrium for one
##                region-year cell (endogenous MC(p): demographics -> risk scores
##                -> claims -> RA -> MC inside each FOC evaluation;
##                broker-to-navigator tau gradient). Commissions carry
##                their own FOC (2026-07-23): each insurer scales its observed
##                schedule (pct schedules track candidate premiums) subject to a
##                per-insurer commission markup mu, specified as a function of
##                insurer covariates and estimated off the commission FOC. The
##                baseline scenario is the joint (p, k) fixed point. Endogenous
##                scenarios: endog_tau (expansion, brokers keep being paid),
##                flat_mandate (pct insurers forced flat, levels re-chosen),
##                defund (navigator-to-broker reverse conversion). Sourced in the
##                preamble; called by cf1_estimate.R (the all-cells driver) and
##                cf4_se-comm.R (sensitivity mode). Relies on the structural
##                helpers the caller has already loaded (supply.R, ra.R,
##                choice.R, welfare_*.R).

# run_cf_cell ---------------------------------------------------------------
#
# Runs counterfactual scenarios for one region-year cell.
# Returns a tibble of results, or NULL if the cell can't be processed.

run_cf_cell <- function(r, y, seed, sample_frac, hhs_raw,
                        plan_choice, supply_results, coefs,
                        commission_lookup, rs_coefs, claims_coefs,
                        reins_df, STRUCTURAL_SPEC, warm_start = NULL,
                        commission_mu = NULL, sens = NULL) {
  # warm_start: a per-scenario list keyed by scenario label, each element
  # list(p = premiums by plan_id, k = commission scale by insurer prefix) from the
  # cf1 solution. Used as the STARTING point of the endogenous scenario solves so a
  # re-run begins next to its answer and lands on the same spot in the (soft)
  # commission valley. NULL (cf1) = cold start.
  # sens (cf4): list(z = matrix of the commission-FOC covariates by "firm_year" row,
  # one column per coefficient; h_rel = relative FD step). Sensitivity mode: every
  # scenario is evaluated at its warm_start solution with no solve, and the function
  # returns d(premium)/d(delta) and d(commission)/d(delta) per plan and scenario by
  # the implicit function theorem instead of the equilibrium table.

  TAU_GRID <- c(0, 0.25, 0.5, 0.75, 1.0)
  # Endogenous-commission scenario grids, trimmed (read 0 -> 0.5 -> 1; tau = 0
  # duplicates the observed joint fixed point, endog_tau1.00 must match
  # zero_tau1.00 since the broker pool is empty)
  ENDOG_TAU_GRID <- c(0.5, 1.0)
  DEFUND_GRID    <- c(0.5, 1.0)
  # Commission-level sweep: brokers stay brokers, commissions scaled down.
  SCALE_GRID <- c(0.25, 0.5, 0.75)
  # Commission utility terms, zeroed for the non-commission welfare metric
  # (cs_nocomm), so welfare comparisons don't treat commission-driven steering as
  # a genuine household preference.
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
  # Percentage-schedule insurers (rate x premium): their dollar commissions track
  # candidate premiums in the endogenous-commission scenarios
  pct_prefixes <- comm_yr$insurer_prefix[!is.na(comm_yr$is_pct) & comm_yr$is_pct]
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

  # Insurer structure for the commission FOC: plan -> insurer prefix, ownership
  # matrix (hoisted from build_foc_function — the mu calibration needs it too),
  # pct flag and observed rate per plan (rho = eta_obs / p_obs on pct plans)
  plan_prefix <- sub("_.*", "", plan_ids_cell)
  own_mat <- outer(plan_prefix, plan_prefix, "==") * 1L
  dimnames(own_mat) <- list(plan_ids_cell, plan_ids_cell)
  pct_plan <- plan_prefix %in% pct_prefixes
  rho_obs  <- ifelse(pct_plan & p_obs[plan_ids_cell] > 0,
                     comm_obs[plan_ids_cell] / p_obs[plan_ids_cell], 0)

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
  # held FIXED across scenarios. omega is CALIBRATED below at the observed point
  # (p_obs, k = 1) so the premium FOC holds exactly at observed prices (omega = -solve(Omega, f0)
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
  # ENDOGENOUS SUBSIDY. The APTC moves with the
  # benchmark (2nd-cheapest silver) premium: subsidy = max(0, premiumSLC(p) - zeta),
  # where zeta_it = SLC_contribution is the fixed income cap (carried from the data
  # build; NA for subsidy-ineligible HHs). We anchor the HH benchmark premium to its
  # data-build value premiumSLC and add the HH-scaled change in the benchmark plan's
  # posted price, so at observed prices the subsidy reproduces the data-build value
  # exactly (delta = 0 -> subsidy = pmax(0, premiumSLC - zeta)) while
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
      # Interior subsidy (the derivative of the pmax above): the kernel's benchmark
      # column applies the subsidy chain only to these households
      dt[, sub_interior := as.numeric(subsidized == 1L & (premiumSLC_cf - SLC_contribution) > 0)]
    } else {
      dt[, subsidy_cf := adj_subsidy]   # no benchmark silver -> fall back to baseline
      dt[, sub_interior := 0]
    }

    # Floor indicator per row (the derivative of the pmax below), read by the kernel
    dt[, kink_m := 1]
    for (pn in names(p_vec)) {
      idx <- which(dt$plan_id == pn)
      if (length(idx) == 0) next
      premium_hh <- (p_vec[pn] / RATING_FACTOR_AGE40) * dt$rating_factor[idx]
      gap <- premium_hh - dt$subsidy_cf[idx]
      oop <- pmax(gap, 0) - dt$penalty[idx] / 12
      set(dt, i = idx, j = "premium", value = oop / dt$hh_size[idx] / 100)
      set(dt, i = idx, j = "kink_m", value = as.numeric(gap > 0))
    }
    recompute_prem_interactions(dt, STRUCTURAL_SPEC)
  }

  # apply_commissions --------------------------------------------------------
  # Rewrite the commission utility column for a per-plan eta vector. The
  # endogenous-commission FOC rewrites this per solver ITERATE (the write in
  # build_scenario_data is only the per-scenario baseline). Uses the `broker`
  # column, which build_scenario_data finalizes AFTER the tau conversion, so
  # converted households stay at zero; broker is NA-safe by construction.
  apply_commissions <- function(dt, eta_vec) {
    for (pn in names(eta_vec)) {
      idx <- which(dt$plan_id == pn)
      if (length(idx) == 0) next
      set(dt, i = idx, j = "commission_broker",
          value = eta_vec[[pn]] * dt$broker[idx])
    }
    dt
  }

  # FOC function and analytical Jacobian -------------------------------------
  # Returns list(fn, jac) sharing a cache. fn computes the FOC residual with
  # endogenous MC(p) and caches intermediates.
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
                                  plan_avs, reins_vec, lambda, plan_ids_cell,
                                  comm_endog = NULL) {
    dt_base <- as.data.table(cell_data_base)
    # own_mat closed over from run_cf_cell scope (hoisted; mu calibration uses it)
    J <- length(plan_ids_cell)

    # Endogenous commissions (comm_endog non-NULL): unknowns are (p, kappa) with
    # kappa_f = k_f * etabar_f in dollars PMPM; eta_j = k_f * w_j(p), where the
    # weight basis w_j is w_flat (flat schedules / mandate) or rho * p_j (pct
    # schedules track candidate premiums). eta_base holds non-endogenous insurers
    # at their scenario level. mu / MC_obs are the fixed observed-point calibration.
    idx_f <- if (!is.null(comm_endog))
      lapply(comm_endog$prefixes, function(f) which(plan_prefix == f)) else NULL

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

    # Demographics used in risk score regression (age, gender,
    # income). Map each predicted share to its raw per-HH column for the Eq. 17
    # dr/dp Jacobian below (the FPL shares don't follow the share_->perc_ rule).
    demo_names <- intersect(c("share_18to34", "share_35to54", "share_male",
                              "share_fpl250to400", "share_fpl400plus"),
                            names(rs_coefs))
    DEMO_RAWCOL <- c(share_18to34 = "perc_18to34", share_35to54 = "perc_35to54",
                     share_male = "perc_male",
                     share_fpl250to400 = "FPL_250to400",
                     share_fpl400plus = "FPL_400plus")

    # Shared cache between fn and jac
    cache <- new.env(parent = emptyenv())
    cache$iter <- 0L
    cache$p_vec_prev <- setNames(rep(0, J), plan_ids_cell)

    # --- Helper: compute all FOC quantities at a price vector ---
    eval_foc_quantities <- function(dt, p_vec) {
      util <- compute_utility(dt, coefs_cell)
      V <- util$V
      V_base <- util$V_base
      se <- tryCatch(
        compute_shares_and_elasticities(dt, V, lambda, benchmark_plan, plans_cell,
                                         coefs_cell, spec = STRUCTURAL_SPEC, V_base = V_base),
        error = function(e) NULL
      )
      if (is.null(se)) return(NULL)
      pn <- names(p_vec)
      shares <- se$shares[pn]
      elast <- se$elast_mat[pn, pn]
      # Enrollment-weighted statewide average premium (ACA RA scale), not a plan mean.
      avg_p <- weighted.mean(p_vec, shares, na.rm = TRUE)
      demo <- tryCatch(compute_demographic_shares(dt, V, lambda, V_base = V_base), error = function(e) NULL)
      if (is.null(demo)) return(NULL)
      mc_res <- compute_mc(rs_coefs, claims_coefs, plan_chars_cell,
                            demo, shares, avg_p, plan_avs, reins_local)
      rs <- mc_res$predicted_risk_scores[pn]
      ra_foc <- compute_ra_foc(rs, shares, plan_avs, avg_p,
                                elast, own_mat[pn, pn])
      br <- tryCatch(
        compute_broker_shares_and_elasticities(dt, V, lambda, benchmark_plan, plans_cell,
                                                coefs_cell, spec = STRUCTURAL_SPEC, V_base = V_base),
        error = function(e) NULL
      )
      br_elast <- if (!is.null(br)) br$broker_elast_mat[pn, pn] else NULL
      # Commission derivatives (endogenous-commission mode only): D = dqB/deta
      # over broker HHs, qB = broker enrollment, both in total-weight share units
      ck <- if (!is.null(comm_endog)) tryCatch(
        compute_commission_derivatives(dt, V, lambda, coefs_cell, V_base = V_base),
        error = function(e) NULL
      ) else NULL
      list(V = V, V_base = V_base, shares = shares, elast_mat = elast, mc_result = mc_res,
           mc_p = mc_res$mc[pn] + omega_vec[pn], rs_p = rs, ra_foc_p = ra_foc,
           demo_shares = demo, avg_prem = avg_p, broker_elast = br_elast,
           comm_D = if (!is.null(ck)) ck$D[pn, pn] else NULL,
           comm_qB = if (!is.null(ck)) ck$qB[pn] else NULL)
    }

    # =======================================================================
    # fn: FOC residual with caching
    # =======================================================================
    fn <- function(x_vec) {

      if (is.null(comm_endog)) {
        # ---- exogenous commissions (original path, unchanged) ----
        p_vec <- x_vec
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

        return(resid)
      }

      # ---- endogenous commissions: x = (p, kappa), split by POSITION ----
      p_vec <- x_vec[seq_len(J)]
      k_f   <- x_vec[-seq_len(J)] / comm_endog$etabar
      pn_solve <- names(p_vec)

      # eta_j = k_f * w_j(p); w = observed level (flat / mandate basis) or
      # observed rate x CURRENT premium (pct schedules track candidate premiums)
      w_full <- ifelse(comm_endog$pct, comm_endog$rho * p_vec, comm_endog$w_flat)
      eta_cur <- comm_endog$eta_base
      for (fi in seq_along(comm_endog$prefixes))
        eta_cur[idx_f[[fi]]] <- k_f[fi] * w_full[idx_f[[fi]]]

      dt <- apply_commissions(update_premiums(copy(dt_base), p_vec), eta_cur)

      q <- eval_foc_quantities(dt, p_vec)
      if (is.null(q) || is.null(q$broker_elast) || is.null(q$comm_D))
        return(rep(NA_real_, length(x_vec)))

      Omega <- -own_mat[pn_solve, pn_solve] * t(q$elast_mat)

      cache$dt <- dt
      cache$p_vec <- p_vec
      cache$pn_solve <- pn_solve
      cache$q <- q
      cache$Omega <- Omega

      # Premium FOC at the CURRENT eta. Pct plans of endogenous insurers carry
      # the direct outlay term k * rho * qB (raising the premium mechanically
      # raises the commission paid on the plan's broker enrollees).
      Omega_B <- -own_mat[pn_solve, pn_solve] * t(q$broker_elast)
      direct <- numeric(J)
      for (fi in seq_along(comm_endog$prefixes)) {
        ii <- idx_f[[fi]]
        direct[ii] <- ifelse(comm_endog$pct[ii],
                             k_f[fi] * comm_endog$rho[ii] * q$comm_qB[ii], 0)
      }
      resid_p <- q$shares + q$ra_foc_p[pn_solve] -
        as.vector(Omega %*% (p_vec - q$mc_p)) +
        as.vector(Omega_B %*% eta_cur[pn_solve]) - direct

      # Commission FOC per endogenous insurer: margin x dqB/dk + RA response
      # against (1 + mu) x inframarginal cost, divided by the FIXED calibration
      # normalizer so the equations are dimensionless (order relative-k-error).
      # D is NOT transposed: [D %*% w_f]_j = dqB_j/dk_f (rows respond, columns
      # move), the same orientation compute_ra_foc expects.
      ra_eta <- compute_ra_foc(q$rs_p, q$shares, plan_avs, q$avg_prem,
                               q$comm_D, own_mat[pn_solve, pn_solve])
      margin <- p_vec - q$mc_p - eta_cur[pn_solve]
      MB_f <- MC_f <- numeric(length(comm_endog$prefixes))
      for (fi in seq_along(comm_endog$prefixes)) {
        ii <- idx_f[[fi]]
        w_f <- numeric(J); w_f[ii] <- w_full[ii]
        dq <- as.numeric(q$comm_D %*% w_f)
        MB_f[fi] <- sum(margin[ii] * dq[ii]) + sum(w_full[ii] * ra_eta[ii])
        MC_f[fi] <- sum(q$comm_qB[ii] * w_full[ii])
      }
      resid_k <- (MB_f - (1 + comm_endog$mu) * MC_f) /
        ((1 + comm_endog$mu) * comm_endog$MC_obs)
      # Cached for the sensitivity mode (d resid_k / d mu needs MB at the solution)
      cache$MB_f <- MB_f
      cache$MC_f <- MC_f

      cache$iter <- cache$iter + 1L
      c(resid_p, resid_k)
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
          raw_col <- DEMO_RAWCOL[[d]]
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



  # Sensitivity of the solved equilibrium to the commission-FOC coefficients
  # (sensitivity mode). At the saved solution x* = (p*, kappa*), the implicit
  # function theorem gives dx*/d delta = -[dF/dx]^-1 dF/d delta. dF/dx is formed by
  # central differences of the FOC residual; dF/d delta runs through mu_f = z_f'
  # delta, and only insurer f's commission FOC depends on mu_f, with
  # d resid_k_f / d mu_f = -MB_f / ((1 + mu_f)^2 MC_obs_f). The commission
  # sensitivity follows from eta_j = k_f w_j(p). With exogenous commissions delta
  # does not enter, so the sensitivities are zero. Expects fns$cache at x*.
  sens_at_solution <- function(fns, x_star, comm_endog) {
    J <- length(plan_ids_cell)
    n <- length(x_star)
    zero <- matrix(0, J, ncol(sens$z), dimnames = list(plan_ids_cell, colnames(sens$z)))
    if (is.null(comm_endog)) return(list(dp = zero, deta = zero, cond = NA_real_))

    MB_f <- fns$cache$MB_f
    dF_dd <- matrix(0, n, ncol(sens$z))
    for (fi in seq_along(comm_endog$prefixes)) {
      key <- paste(comm_endog$prefixes[fi], y, sep = "_")
      if (!key %in% rownames(sens$z)) next      # fallback mu, not a function of delta
      dF_dd[J + fi, ] <- -MB_f[fi] / ((1 + comm_endog$mu[fi])^2 * comm_endog$MC_obs[fi]) *
        sens$z[key, ]
    }

    Jx <- matrix(0, n, n)
    hx <- sens$h_rel * pmax(abs(x_star), 1)
    for (j in seq_len(n)) {
      xp <- x_star; xp[j] <- xp[j] + hx[j]
      xm <- x_star; xm[j] <- xm[j] - hx[j]
      Jx[, j] <- (fns$fn(xp) - fns$fn(xm)) / (2 * hx[j])
    }
    if (any(!is.finite(Jx))) return(NULL)
    dx <- tryCatch(-qr.solve(Jx, dF_dd), error = function(e) -MASS::ginv(Jx) %*% dF_dd)
    sv <- svd(Jx)$d

    dp <- dx[seq_len(J), , drop = FALSE]
    dimnames(dp) <- dimnames(zero)
    dkappa <- dx[-seq_len(J), , drop = FALSE]
    p_star <- x_star[seq_len(J)]
    k_star <- x_star[-seq_len(J)] / comm_endog$etabar
    w_star <- ifelse(comm_endog$pct, comm_endog$rho * p_star, comm_endog$w_flat)
    deta <- zero
    for (fi in seq_along(comm_endog$prefixes)) {
      ii <- which(plan_prefix == comm_endog$prefixes[fi])
      dk <- dkappa[fi, ] / comm_endog$etabar[fi]
      deta[ii, ] <- outer(w_star[ii], dk) +
        (k_star[fi] * comm_endog$rho[ii] * comm_endog$pct[ii]) * dp[ii, , drop = FALSE]
    }
    list(dp = dp, deta = deta, cond = max(sv) / min(sv))
  }

  # Solve pricing equilibrium ------------------------------------------------
  # comm_endog non-NULL adds the per-insurer commission FOCs to the system:
  # unknowns (p, kappa), kappa_f = k_f * etabar_f in dollars PMPM (kappa_init on
  # that scale). Exogenous callers are unchanged (positional args). label names
  # the scenario (warm_start lookup in sensitivity mode).
  solve_equilibrium <- function(cd_scenario, comm_sc, p_init,
                                comm_endog = NULL, kappa_init = NULL, label = NULL) {

    fns <- build_foc_function(cd_scenario, coefs, comm_sc,
                               benchmark_plan, plan_attrs,
                               rs_coefs, claims_coefs, plan_chars_cell,
                               plan_avs, reins_vec, lambda, plan_ids_cell,
                               comm_endog = comm_endog)

    x_init <- if (is.null(comm_endog)) p_init else
      c(p_init, setNames(kappa_init, paste0("eta_", comm_endog$prefixes)))

    # Sensitivity mode: evaluate at the saved cf1 solution for this scenario.
    if (!is.null(sens)) {
      ws <- warm_start[[label]]
      if (is.null(ws)) { cat("    sens: no saved solution for", label, "\n"); return(NULL) }
      x_init <- if (is.null(comm_endog)) ws$p[plan_ids_cell] else
        c(ws$p[plan_ids_cell],
          setNames(ws$k[comm_endog$prefixes] * comm_endog$etabar,
                   paste0("eta_", comm_endog$prefixes)))
      if (anyNA(x_init)) { cat("    sens: incomplete saved solution for", label, "\n"); return(NULL) }
    }

    f0 <- fns$fn(x_init)
    cat("    initial |FOC| =", round(sqrt(sum(f0^2, na.rm=TRUE)), 6),
        ", any NA:", any(is.na(f0)), "\n")
    if (any(is.na(f0))) return(NULL)

    # --- Jacobian diagnostic (options(cf.jac.debug = TRUE)): compare the analytical
    # fns$jac against a central-difference numerical Jacobian at p_init, then stop.
    # Premium-only (J-shaped) — skipped in endogenous-commission mode. ---
    if (isTRUE(getOption("cf.jac.debug", FALSE)) && is.null(comm_endog)) {
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
    sol <- if (!is.null(sens)) {
      # No solve: the sensitivities at x_init (the saved solution), then a stub
      # solution object so the post-solve quantities below evaluate at x_init.
      sn <- sens_at_solution(fns, x_init, comm_endog)
      if (is.null(sn)) { cat("    sens: non-finite Jacobian for", label, "\n"); return(NULL) }
      sens_store[[label]] <<- bind_cols(
        tibble(region = r, year = y, scenario = label, plan_id = plan_ids_cell,
               cond_J = sn$cond),
        as_tibble(sn$dp)   %>% rename_with(~ paste0("dp_", .x)),
        as_tibble(sn$deta) %>% rename_with(~ paste0("deta_", .x)))
      list(x = x_init, fvec = f0, termcd = 1L, iter = 0L)
    } else tryCatch(
      nleqslv(x = x_init, fn = fns$fn, method = "Broyden", global = "hook",
              control = list(maxit = 100, xtol = 1e-6, ftol = 1e-8)),
      error = function(e) { cat("    nleqslv error:", conditionMessage(e), "\n"); NULL }
    )

    # --- Conditioning / smoothness diagnostic at the plateau (options(cf.cond.debug)).
    # Premium-only (J-shaped) — skipped in endogenous-commission mode. ---
    if (isTRUE(getOption("cf.cond.debug", FALSE)) && !is.null(sol) && is.null(comm_endog)) {
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

    p_sol <- sol$x[seq_along(p_init)]

    # Unstack the commission solution: kappa -> k -> per-plan eta at solved p
    if (!is.null(comm_endog)) {
      k_sol <- sol$x[-seq_along(p_init)] / comm_endog$etabar
      names(k_sol) <- comm_endog$prefixes
      w_sol <- ifelse(comm_endog$pct, comm_endog$rho * p_sol, comm_endog$w_flat)
      eta_sol <- comm_endog$eta_base
      for (fi in seq_along(comm_endog$prefixes)) {
        ii <- which(plan_prefix == comm_endog$prefixes[fi])
        eta_sol[ii] <- k_sol[fi] * w_sol[ii]
      }
    } else {
      k_sol <- NULL
      eta_sol <- NULL
    }

    dt_sol <- update_premiums(as.data.table(copy(cd_scenario)), p_sol)
    # dt_final must carry the SOLVED eta — welfare reads commission_broker
    # through utilities, and cf2 rebuilds from the persisted solved commissions
    if (!is.null(eta_sol)) dt_sol <- apply_commissions(dt_sol, eta_sol)

    util_sol <- compute_utility(dt_sol, coefs)
    se_sol <- tryCatch(
      compute_shares_and_elasticities(dt_sol, util_sol$V, lambda,
                                       benchmark_plan, plan_attrs, coefs,
                                       spec = STRUCTURAL_SPEC, V_base = util_sol$V_base),
      error = function(e) NULL
    )
    shares_sol <- if (!is.null(se_sol)) se_sol$shares[plan_ids_cell] else setNames(rep(NA_real_, length(plan_ids_cell)), plan_ids_cell)

    demo_sol <- tryCatch(
      compute_demographic_shares(dt_sol, util_sol$V, lambda, V_base = util_sol$V_base),
      error = function(e) NULL
    )
    mc_sol <- compute_mc(rs_coefs, claims_coefs, plan_chars_cell,
                          demo_sol, shares_sol,
                          weighted.mean(p_sol, shares_sol, na.rm = TRUE),
                          plan_avs, reins_vec)
    # Same omega cost residual carried into the reported MC (see run_cf_cell head).
    mc_eff <- mc_sol$mc + omega_vec[names(mc_sol$mc)]

    list(sol = sol, p = p_sol, mc = mc_eff, shares = shares_sol,
         dt_final = dt_sol, eta = eta_sol, k = k_sol)
  }


  # Build scenario cell data ------------------------------------------------
  # tau: share of broker households converted to navigators (highest p_nav
  #   first); broker_remain = TRUE keeps the non-switched (1 - tau) brokers as
  #   brokers (endogenous-commission scenarios) instead of converting them to
  #   Unassisted (the zero_tau ban design).
  # defund: share of NAVIGATOR households converted to brokers (lowest p_nav
  #   first — least navigator-attached switch first; the reverse of tau). Runs
  #   before the commission write so new brokers pick up the scenario schedule.
  build_scenario_data <- function(cell_data_base, comm_sc, tau = NULL,
                                  broker_remain = FALSE, defund = NULL) {
    cd <- as.data.table(copy(cell_data_base))

    if (!is.null(defund) && "any_agent" %in% names(cd)) {
      nav_hh <- cd[plan_id == "Uninsured" & assisted == 1L &
                     (is.na(any_agent) | any_agent != 1L),
                   .(household_number, p_nav)]
      if (nrow(nav_hh) == 0) {
        nav_hh <- unique(cd[assisted == 1L & (is.na(any_agent) | any_agent != 1L),
                            .(household_number, p_nav)], by = "household_number")
      }
      if (nrow(nav_hh) > 0) {
        nav_hh <- nav_hh[order(p_nav)]
        n_switch <- ceiling(defund * nrow(nav_hh))
        switch_ids <- nav_hh$household_number[seq_len(n_switch)]
        cd[household_number %in% switch_ids, any_agent := 1L]
      }
    }

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

        if (tau < 1 && !broker_remain) {
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
      assisted_av      = nonbroker * av,
      broker_av        = broker * av,
      assisted_premium = nonbroker * premium,
      broker_premium   = broker * premium
    )]
    # nonbroker / broker kept: raw_demo for the premium interactions, which
    # recompute_prem_interactions updates as premiums move in the solve.

    cd
  }


  # Main solve loop ---------------------------------------------------------
  cat("Cell", r, y, "- plans:", length(plan_ids_cell), "\n")

  results_list <- list()

  # Scenario 1: Baseline (joint premium + commission fixed point under the
  # estimated model; every counterfactual is differenced from it)
  comm_obs_sc <- comm_obs[plan_ids_cell]
  cd_obs <- build_scenario_data(cell_data_base, comm_obs_sc)

  # Calibrate omega so the CF's own FOC holds exactly at observed prices. With
  # omega = 0, f0 = fn(p_obs); the FOC is linear in MC, so adding omega shifts the
  # residual by Omega %*% omega. Uses the CF's own Omega at p_obs, so it is
  # self-consistent even in ill-conditioned cells (borrowing mc_foc from 2_pricing
  # left a ~0.05 residual that the cond~2e6 geometry amplified into large price
  # swings). With endogenous commissions the premium FOC subtracts the pct direct
  # outlay term at k = 1, so omega is calibrated against f0 - direct_obs, the
  # premium FOC at observed premiums and commissions. The baseline scenario then
  # re-solves (p, k) jointly under the estimated commission markup mu, so baseline
  # premiums and commissions are model-implied rather than observed.
  # Exogenous policy scenarios reuse this omega; their FOC carries no
  # direct term because those policies fix DOLLAR schedules (severing the pct
  # premium linkage is part of the policy, not an inconsistency).
  fns_cal <- build_foc_function(cd_obs, coefs, comm_obs_sc, benchmark_plan, plan_attrs,
                                rs_coefs, claims_coefs, plan_chars_cell, plan_avs,
                                reins_vec, lambda, plan_ids_cell)
  f0 <- fns_cal$fn(p_obs[plan_ids_cell])
  q_cal <- fns_cal$cache$q

  # Commission derivatives, broker enrollment, and the endogenous-insurer set at
  # the observed point. Gate: positive observed commissions and a broker pool at
  # or above the existing share floor (corner / degenerate FOCs held at observed).
  ck_cal <- if (!any(is.na(f0))) tryCatch(
    compute_commission_derivatives(fns_cal$cache$dt, q_cal$V, lambda, coefs, V_base = q_cal$V_base),
    error = function(e) NULL
  ) else NULL
  qB_cal <- if (!is.null(ck_cal)) ck_cal$qB[plan_ids_cell] else
    setNames(rep(0, length(plan_ids_cell)), plan_ids_cell)
  D_cal  <- if (!is.null(ck_cal)) ck_cal$D[plan_ids_cell, plan_ids_cell] else NULL

  endog_prefixes <- character(0)
  if (!is.null(D_cal)) {
    for (f in unique(plan_prefix)) {
      ii <- which(plan_prefix == f)
      if (sum(comm_obs_sc[ii]) > 0 && sum(qB_cal[ii]) >= SHARE_FLOOR_FOC)
        endog_prefixes <- c(endog_prefixes, f)
    }
  }

  # Pct direct outlay term at (p_obs, k = 1): rho_j * qB_j on endogenous pct plans
  direct_obs <- numeric(length(plan_ids_cell))
  for (f in endog_prefixes) {
    ii <- which(plan_prefix == f)
    direct_obs[ii] <- ifelse(pct_plan[ii], rho_obs[ii] * qB_cal[ii], 0)
  }

  om_sol <- tryCatch(as.numeric(-solve(fns_cal$cache$Omega, f0 - direct_obs)),
                     error = function(e) rep(0, length(plan_ids_cell)))
  om_sol[!is.finite(om_sol)] <- 0
  omega_vec <- setNames(om_sol, plan_ids_cell)

  # mu markup per endogenous insurer: the estimated commission markup
  # mu_ft = delta' z_ft from the commission FOC (insurer size, schedule type,
  # broker enrollment per available agent), held FIXED across scenarios, exactly
  # parallel to omega on the premium side. When the estimate is unavailable for an
  # insurer-year, fall back to the value that rationalizes the observed schedule
  # (MB = (1+mu) MC, MB including the RA response). Margins net of the newly
  # calibrated omega (q_cal$mc_p was evaluated at omega = 0).
  comm_mu <- comm_etabar <- comm_MCobs <- comm_qBsum <- setNames(numeric(0), character(0))
  if (length(endog_prefixes) > 0) {
    ra_eta_cal <- compute_ra_foc(q_cal$rs_p, q_cal$shares, plan_avs,
                                 q_cal$avg_prem, D_cal, own_mat)
    margin_cal <- p_obs[plan_ids_cell] - (q_cal$mc_p + omega_vec) - comm_obs_sc
    keep <- character(0)
    for (f in endog_prefixes) {
      ii <- which(plan_prefix == f)
      w_f <- numeric(length(plan_ids_cell)); w_f[ii] <- comm_obs_sc[ii]
      dq <- as.numeric(D_cal %*% w_f)
      MB_f <- sum(margin_cal[ii] * dq[ii]) + sum(comm_obs_sc[ii] * ra_eta_cal[ii])
      MC_f <- sum(qB_cal[ii] * comm_obs_sc[ii])
      if (is.finite(MB_f) && MC_f > 0) {
        mkey   <- paste(f, y, sep = "_")
        mu_est <- if (!is.null(commission_mu) && mkey %in% names(commission_mu))
                    commission_mu[[mkey]] else NA_real_
        comm_mu[f]     <- if (is.finite(mu_est)) mu_est else MB_f / MC_f - 1
        comm_etabar[f] <- MC_f / sum(qB_cal[ii])
        comm_MCobs[f]  <- MC_f
        comm_qBsum[f]  <- sum(qB_cal[ii])
        keep <- c(keep, f)
      }
    }
    endog_prefixes <- keep
  }
  mu_by_plan <- setNames(rep(NA_real_, length(plan_ids_cell)), plan_ids_cell)
  for (f in endog_prefixes) mu_by_plan[plan_prefix == f] <- comm_mu[[f]]
  if (length(endog_prefixes) > 0) {
    cat("  commission FOC:", length(endog_prefixes), "insurers endogenous; mu in [",
        round(min(comm_mu[endog_prefixes]), 2), ",",
        round(max(comm_mu[endog_prefixes]), 2), "]\n")
  } else {
    cat("  commission FOC: no endogenous insurers (gate/calibration)\n")
  }
  rm(fns_cal, f0, om_sol, q_cal, ck_cal)

  # comm_endog spec with the insurers' native schedule basis (flat level or pct
  # rate x current premium); shared by the observed / endog_tau / defund solves
  ce_native <- function(prefixes) {
    if (length(prefixes) == 0) return(NULL)
    list(eta_base = comm_obs_sc, w_flat = comm_obs_sc, rho = rho_obs,
         pct = pct_plan, prefixes = prefixes, mu = comm_mu[prefixes],
         etabar = comm_etabar[prefixes], MC_obs = comm_MCobs[prefixes])
  }
  # Per-plan k of the owning insurer (NA for held-fixed insurers), for the output
  k_by_plan <- function(k_vec) {
    kp <- setNames(rep(NA_real_, length(plan_ids_cell)), plan_ids_cell)
    if (!is.null(k_vec)) for (f in names(k_vec)) kp[plan_prefix == f] <- k_vec[[f]]
    kp
  }

  sens_store <- list()   # sensitivity mode: filled per scenario by solve_equilibrium

  ce_obs <- ce_native(endog_prefixes)
  eq_obs <- solve_equilibrium(cd_obs, comm_obs_sc, p_obs[plan_ids_cell],
                              comm_endog = ce_obs, label = "baseline",
                              kappa_init = if (is.null(ce_obs)) NULL else
                                unname(comm_etabar[endog_prefixes]))

  if (!is.null(eq_obs)) {
    results_list[[length(results_list) + 1]] <- tibble(
      region = r, year = y, scenario = "baseline", tau = NA_real_,
      plan_id = plan_ids_cell,
      premium_obs = p_obs[plan_ids_cell],
      premium_cf = eq_obs$p[plan_ids_cell],
      premium_change = eq_obs$p[plan_ids_cell] - p_obs[plan_ids_cell],
      share_obs = setNames(sr_cell$share, sr_cell$plan_id)[plan_ids_cell],
      share_cf = eq_obs$shares[plan_ids_cell],
      mc = eq_obs$mc[plan_ids_cell],
      commission_pmpm = if (!is.null(eq_obs$eta)) eq_obs$eta[plan_ids_cell] else comm_obs_sc[plan_ids_cell],
      comm_scale_cf = k_by_plan(eq_obs$k),
      mu_comm = mu_by_plan[plan_ids_cell],
      markup_cf = eq_obs$p[plan_ids_cell] - eq_obs$mc[plan_ids_cell],
      nleqslv_termcd = eq_obs$sol$termcd,
      nleqslv_iter = eq_obs$sol$iter
    )
    cat("  baseline - converged (nleqslv iter =", eq_obs$sol$iter, ")\n")
    if (!is.null(eq_obs$k))
      cat("    joint fixed point: max|k - 1| =",
          signif(max(abs(eq_obs$k - 1)), 3),
          " max|p - p_obs| =", signif(max(abs(eq_obs$p - p_obs[plan_ids_cell])), 3), "\n")
    p_warm <- eq_obs$p
  } else {
    cat("  baseline - did not converge\n")
    p_warm <- p_obs
  }
  # The endog/defund chains warm-start from the observed solution; the zero_tau
  # loop mutates p_warm down its own chain, so save the observed point here.
  p_obs_sol <- if (!is.null(eq_obs)) eq_obs$p else p_obs[plan_ids_cell]
  rm(cd_obs)

  # Baseline warm-start: the scenario premiums / commission dollars from cf1, or the
  # default start when warm_start is NULL (cf1) or lacks this scenario.
  ws_p <- function(label, default)
    if (is.null(warm_start[[label]])) default else warm_start[[label]]$p[plan_ids_cell]
  ws_kappa <- function(label, prefixes, etabar_local, default) {
    k <- if (is.null(warm_start[[label]])) NULL else warm_start[[label]]$k[prefixes]
    # A draw's endogenous set can differ from the baseline's (perturbed coefficients
    # move a borderline insurer across the threshold); cold-start k rather than feed
    # the solver an NA when the baseline doesn't cover this draw's insurers.
    if (is.null(k) || anyNA(k)) default else unname(k * etabar_local[prefixes])
  }


  # Scenario 2: Zero commission with tau gradient
  comm_zero <- setNames(rep(0, length(plan_ids_cell)), plan_ids_cell)

  for (tau in TAU_GRID) {
    sc_label <- paste0("zero_tau", sprintf("%.2f", tau))
    cd_tau <- build_scenario_data(cell_data_base, comm_zero, tau = tau)

    eq_tau <- solve_equilibrium(cd_tau, comm_zero, p_warm, label = sc_label)

    if (!is.null(eq_tau)) {
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
  eq_unif <- solve_equilibrium(cd_unif, comm_uniform, p_obs, label = "uniform")

  if (!is.null(eq_unif)) {
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
    eq_sc <- solve_equilibrium(cd_sc, comm_scaled, p_obs, label = sc_label)
    if (!is.null(eq_sc)) {
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
  eq_al <- solve_equilibrium(cd_al, comm_aligned, p_obs, label = "aligned")
  if (!is.null(eq_al)) {
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
      nleqslv_termcd = eq_al$sol$termcd,
      nleqslv_iter = eq_al$sol$iter
    )
    cat("   aligned - converged (nleqslv iter =", eq_al$sol$iter, ")\n")
  } else {
    cat("   aligned - did not converge\n")
  }

  rm(cd_al, cd_base_nc); gc(verbose = FALSE)

  # Scenario 6: Navigator expansion with ENDOGENOUS commissions (endog_tau) ----
  # Same tau conversion as zero_tau but the non-switched brokers REMAIN brokers
  # and keep being paid (broker_remain = TRUE); endogenous insurers re-choose
  # their commission scale k_f on the native schedule basis subject to the
  # estimated commission markup mu. Gate re-evaluated on the SCENARIO broker pool
  # at the warm point (the pool shrinks with tau; mu-existence at the baseline is
  # the precondition). tau = 0 is the baseline joint fixed point (not re-run);
  # endog_tau1.00 has an empty broker pool and must match zero_tau1.00.
  p_e_warm   <- p_obs_sol
  kappa_warm <- comm_etabar
  for (tau in ENDOG_TAU_GRID) {
    sc_label <- paste0("endog_tau", sprintf("%.2f", tau))
    cd_e <- build_scenario_data(cell_data_base, comm_obs_sc, tau = tau,
                                broker_remain = TRUE)

    endog_f <- character(0)
    if (length(endog_prefixes) > 0) {
      dt0 <- update_premiums(as.data.table(copy(cd_e)), p_e_warm)
      util0 <- compute_utility(dt0, coefs)
      ck0 <- tryCatch(
        compute_commission_derivatives(dt0, util0$V, lambda, coefs, V_base = util0$V_base),
        error = function(e) NULL
      )
      if (!is.null(ck0)) {
        qB0 <- ck0$qB[plan_ids_cell]
        for (f in endog_prefixes) {
          if (sum(qB0[plan_prefix == f]) >= SHARE_FLOOR_FOC) endog_f <- c(endog_f, f)
        }
      }
      rm(dt0, ck0)
    }

    ce_e <- ce_native(endog_f)
    eq_e <- if (is.null(ce_e)) {
      solve_equilibrium(cd_e, comm_obs_sc, ws_p(sc_label, p_e_warm), label = sc_label)
    } else {
      solve_equilibrium(cd_e, comm_obs_sc, ws_p(sc_label, p_e_warm), comm_endog = ce_e,
                        label = sc_label,
                        kappa_init = ws_kappa(sc_label, endog_f, comm_etabar,
                                              unname(kappa_warm[endog_f])))
    }

    if (!is.null(eq_e)) {
      results_list[[length(results_list) + 1]] <- tibble(
        region = r, year = y, scenario = sc_label, tau = tau,
        plan_id = plan_ids_cell,
        premium_obs = p_obs[plan_ids_cell],
        premium_cf = eq_e$p[plan_ids_cell],
        premium_change = eq_e$p[plan_ids_cell] - p_obs[plan_ids_cell],
        share_obs = setNames(sr_cell$share, sr_cell$plan_id)[plan_ids_cell],
        share_cf = eq_e$shares[plan_ids_cell],
        mc = eq_e$mc[plan_ids_cell],
        commission_pmpm = if (!is.null(eq_e$eta)) eq_e$eta[plan_ids_cell] else comm_obs_sc[plan_ids_cell],
        comm_scale_cf = k_by_plan(eq_e$k),
        markup_cf = eq_e$p[plan_ids_cell] - eq_e$mc[plan_ids_cell],
        nleqslv_termcd = eq_e$sol$termcd,
        nleqslv_iter = eq_e$sol$iter
      )
      cat("  ", sc_label, "- converged (nleqslv iter =", eq_e$sol$iter,
          "; endog insurers =", length(endog_f), ")\n")
      p_e_warm <- eq_e$p
      if (!is.null(eq_e$k))
        kappa_warm[names(eq_e$k)] <- eq_e$k * comm_etabar[names(eq_e$k)]
    } else {
      cat("  ", sc_label, "- did not converge\n")
    }
    rm(cd_e); gc(verbose = FALSE)
  }

  # Scenario 7: Flat-fee mandate ------------------------------------------------
  # Pct insurers forced onto flat schedules (the policy severs the premium
  # linkage, so no direct term and no rho anywhere); every endogenous insurer
  # re-chooses its LEVEL. Basis w = $1 per plan, so kappa IS the dollar level
  # (etabar = 1); start each insurer at its observed broker-weighted mean
  # commission. Held-fixed insurers keep observed dollar schedules.
  if (length(endog_prefixes) > 0) {
    cd_fm <- build_scenario_data(cell_data_base, comm_obs_sc)
    ce_fm <- list(
      eta_base = comm_obs_sc,
      w_flat   = setNames(rep(1, length(plan_ids_cell)), plan_ids_cell),
      rho      = rep(0, length(plan_ids_cell)),
      pct      = rep(FALSE, length(plan_ids_cell)),
      prefixes = endog_prefixes,
      mu       = comm_mu[endog_prefixes],
      etabar   = setNames(rep(1, length(endog_prefixes)), endog_prefixes),
      MC_obs   = comm_qBsum[endog_prefixes]
    )
    eq_fm <- solve_equilibrium(cd_fm, comm_obs_sc, ws_p("flat_mandate", p_obs_sol),
                               comm_endog = ce_fm, label = "flat_mandate",
                               kappa_init = ws_kappa("flat_mandate", endog_prefixes,
                                                     ce_fm$etabar,
                                                     unname(comm_etabar[endog_prefixes])))
    if (!is.null(eq_fm)) {
      results_list[[length(results_list) + 1]] <- tibble(
        region = r, year = y, scenario = "flat_mandate", tau = NA_real_,
        plan_id = plan_ids_cell,
        premium_obs = p_obs[plan_ids_cell],
        premium_cf = eq_fm$p[plan_ids_cell],
        premium_change = eq_fm$p[plan_ids_cell] - p_obs[plan_ids_cell],
        share_obs = setNames(sr_cell$share, sr_cell$plan_id)[plan_ids_cell],
        share_cf = eq_fm$shares[plan_ids_cell],
        mc = eq_fm$mc[plan_ids_cell],
        commission_pmpm = if (!is.null(eq_fm$eta)) eq_fm$eta[plan_ids_cell] else comm_obs_sc[plan_ids_cell],
        comm_scale_cf = k_by_plan(eq_fm$k),
        markup_cf = eq_fm$p[plan_ids_cell] - eq_fm$mc[plan_ids_cell],
        nleqslv_termcd = eq_fm$sol$termcd,
        nleqslv_iter = eq_fm$sol$iter
      )
      cat("   flat_mandate - converged (nleqslv iter =", eq_fm$sol$iter, ")\n")
    } else {
      cat("   flat_mandate - did not converge\n")
    }
    rm(cd_fm); gc(verbose = FALSE)
  }

  # Scenario 8: Navigator defunding (defund) -----------------------------------
  # Navigator households convert to brokers (reverse of the tau machinery; the
  # 2017-19 federal direction), commissions endogenous on native schedules. The
  # broker pool GROWS, so the observed-point endogenous set stays valid.
  p_d_warm     <- p_obs_sol
  kappa_d_warm <- comm_etabar
  for (df in DEFUND_GRID) {
    sc_label <- paste0("defund_", sprintf("%.2f", df))
    cd_d <- build_scenario_data(cell_data_base, comm_obs_sc, defund = df)

    ce_d <- ce_native(endog_prefixes)
    eq_d <- if (is.null(ce_d)) {
      solve_equilibrium(cd_d, comm_obs_sc, ws_p(sc_label, p_d_warm), label = sc_label)
    } else {
      solve_equilibrium(cd_d, comm_obs_sc, ws_p(sc_label, p_d_warm), comm_endog = ce_d,
                        label = sc_label,
                        kappa_init = ws_kappa(sc_label, endog_prefixes, comm_etabar,
                                              unname(kappa_d_warm[endog_prefixes])))
    }

    if (!is.null(eq_d)) {
      results_list[[length(results_list) + 1]] <- tibble(
        region = r, year = y, scenario = sc_label, tau = NA_real_,
        plan_id = plan_ids_cell,
        premium_obs = p_obs[plan_ids_cell],
        premium_cf = eq_d$p[plan_ids_cell],
        premium_change = eq_d$p[plan_ids_cell] - p_obs[plan_ids_cell],
        share_obs = setNames(sr_cell$share, sr_cell$plan_id)[plan_ids_cell],
        share_cf = eq_d$shares[plan_ids_cell],
        mc = eq_d$mc[plan_ids_cell],
        commission_pmpm = if (!is.null(eq_d$eta)) eq_d$eta[plan_ids_cell] else comm_obs_sc[plan_ids_cell],
        comm_scale_cf = k_by_plan(eq_d$k),
        markup_cf = eq_d$p[plan_ids_cell] - eq_d$mc[plan_ids_cell],
        nleqslv_termcd = eq_d$sol$termcd,
        nleqslv_iter = eq_d$sol$iter
      )
      cat("  ", sc_label, "- converged (nleqslv iter =", eq_d$sol$iter, ")\n")
      p_d_warm <- eq_d$p
      if (!is.null(eq_d$k))
        kappa_d_warm[names(eq_d$k)] <- eq_d$k * comm_etabar[names(eq_d$k)]
    } else {
      cat("  ", sc_label, "- did not converge\n")
    }
    rm(cd_d); gc(verbose = FALSE)
  }

  # Return results (sensitivity mode returns the per-scenario sensitivities instead)
  if (!is.null(sens)) {
    if (length(sens_store) == 0) {
      cat("Cell", r, y, "- no sensitivities\n")
      return(NULL)
    }
    out <- bind_rows(sens_store)
    cat("Cell", r, y, "- sensitivities for", length(sens_store), "scenarios\n")
    return(out)
  }
  if (length(results_list) > 0) {
    out <- bind_rows(results_list)
    cat("Cell", r, y, "- produced", nrow(out), "rows\n")
    out
  } else {
    cat("Cell", r, y, "- no scenarios converged\n")
    NULL
  }
}
