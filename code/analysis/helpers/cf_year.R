# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Year-level counterfactual solver (master side). Each plan is
##                priced once for the year: cell premium = plan-year base
##                premium P_jy x fixed regional factor g_jc. The pricing
##                condition for P_jy is the member- and factor-weighted sum of
##                the cells' first-order conditions, G_jy = sum_c N_c g_jc
##                resid_jc, solved to zero, so the baseline is the model's own
##                premium equilibrium. Commissions: where the rate is the
##                policy (zero, scaled, uniform, flat fee, aligned) the
##                scenario imposes the schedule and only premiums re-solve;
##                the baseline and the composition-shock scenarios solve each
##                insurer's rate from the estimated commission condition
##                MB/MC = (1 - beta_f) + wedge_f (solve_cf_commissions: a
##                damped per-firm update on the condition, with the premium
##                equilibrium re-solved inside each round — never a joint
##                root-finder over premiums and commissions). One function
##                evaluation = phase 1 on every cell of the year (parallel,
##                one worker per cell, helpers/cf_cell.R), the statewide
##                transfer sums, then phase 2. Called by cf1_estimate.R.

# cf_year_aggregate ---------------------------------------------------------
# Plan-year pricing conditions and insurer-year commission aggregates (MB, MC,
# qB; populated only when the scenario computes commission derivatives) from
# the cells' phase-2 pieces.
cf_year_aggregate <- function(pieces) {
  G_num <- G_den <- Ow <- setNames(numeric(0), character(0))
  MB <- MC <- qB <- setNames(numeric(0), character(0))
  for (ci in seq_along(pieces)) {
    pc <- pieces[[ci]]
    if (is.null(pc)) next
    pn <- pc$plan_ids
    w  <- pc$N * pc$g
    res <- pc$resid
    add <- function(v, pn, x) { v[setdiff(pn, names(v))] <- 0; v[pn] <- v[pn] + x; v }
    G_num <- add(G_num, pn, w * res)
    G_den <- add(G_den, pn, w)
    Ow    <- add(Ow, pn, w * pc$omega_own)
    for (f in names(pc$MB)) {
      MB[f] <- (if (is.na(MB[f])) 0 else MB[f]) + pc$N * pc$MB[[f]]
      MC[f] <- (if (is.na(MC[f])) 0 else MC[f]) + pc$N * pc$MC[[f]]
      qB[f] <- (if (is.na(qB[f])) 0 else qB[f]) + pc$N * pc$qB[[f]]
    }
  }
  G <- G_num / G_den
  omega_w <- Ow / G_den                      # own-price term: G / omega_w is the residual in dollars
  list(G = G, omega_w = omega_w, MB = MB, MC = MC, qB = qB)
}

# cf_year_evaluate ----------------------------------------------------------
# One full evaluation of the year at P: phase 1 on every worker, the statewide
# transfer sums, phase 2. Returns the pieces (NULL on a failed cell).
cf_year_evaluate <- function(cl, P) {
  recs <- parallel::clusterCall(cl, cf_cell_eval_p1, P)
  ok <- !vapply(recs, is.null, logical(1))
  if (!any(ok)) return(NULL)
  st <- ra_state_totals(recs[ok])
  pieces <- parallel::clusterCall(cl, cf_cell_eval_p2, st$totals, st$own)
  pieces
}

# cf_year_jacobian_P ---------------------------------------------------------
# Numerical derivative of the year's pricing conditions in the base premiums at
# P under the scenario set on the workers: forward differences of h dollars on
# each solved plan (one evaluation per plan). cf1 computes it once per year at
# the baseline and every scenario of the year starts its solve from it. The
# first-order analytic block (share derivatives and Omega) is not adequate
# here: the markups are as large as the price scale of the within-nest logit,
# so the curvature term (p - mc) d Omega / d p is of the same order as Omega
# itself.
cf_year_jacobian_P <- function(yr, solve_ids, P, h = 1) {
  f_at <- function(P) {
    pieces <- cf_year_evaluate(yr$cl, P)
    if (is.null(pieces) || !all(!vapply(pieces[yr$active], is.null, logical(1)))) return(NULL)
    ag <- cf_year_aggregate(pieces)
    ag$G[solve_ids]
  }
  t0 <- Sys.time()
  f0 <- f_at(P)
  if (is.null(f0)) return(NULL)
  J <- matrix(NA_real_, length(solve_ids), length(solve_ids), dimnames = list(solve_ids, solve_ids))
  for (j in seq_along(solve_ids)) {
    Ph <- P; Ph[solve_ids[j]] <- Ph[solve_ids[j]] + h
    fh <- f_at(Ph)
    if (!is.null(fh)) J[, j] <- (fh - f0) / h
    if (j %% 10 == 0 || j == length(solve_ids))
      cat(sprintf("    [%s jacobian] column %d of %d  %.1f min\n", yr$y, j, length(solve_ids),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  J
}

# solve_cf_year_fixed_point --------------------------------------------------
# The model's premium equilibrium from a distant start (the observed premiums):
# damped best-response iteration on the base premiums, P <- P + kappa f, with f
# the pricing residual in dollars per member-month (the gap between the markup
# the model wants and the markup at P), which converges to the stable fixed
# point from the far side of the fold where a Newton step from the observed
# point does not. The gap is measured with the own-price terms at the start (a
# fixed scale), the premium step is kappa times the gap (the iteration
# converges for own slopes down to -2 / kappa dollars of residual per dollar of
# premium; the steepest plans are near -6) and capped at step_cap dollars per
# iteration. Stops when every pricing condition is within tol_dollars; the
# result is then polished by solve_cf_year with the Jacobian computed at the
# fixed point.
solve_cf_year_fixed_point <- function(yr, label, solve_ids, P_init,
                                      kappa = 0.15, step_cap = 25, tol_dollars = 1,
                                      maxit_P = 60, om_base = NULL) {
  P <- P_init
  t0 <- Sys.time(); n <- 0L; last <- NULL
  # Residual scale: the baseline own-price terms when supplied (a scenario that
  # empties plans would otherwise blow up its own 1/omega scale), the start
  # point's terms otherwise
  om0 <- if (!is.null(om_base)) om_base[solve_ids] else NULL
  elapsed <- function() as.numeric(difftime(Sys.time(), t0, units = "mins"))
  eval_at <- function(P) {
    pieces <- cf_year_evaluate(yr$cl, P)
    if (is.null(pieces) || !all(!vapply(pieces[yr$active], is.null, logical(1)))) return(NULL)
    ag <- cf_year_aggregate(pieces)
    if (is.null(om0)) om0 <<- ag$omega_w[solve_ids]
    n <<- n + 1L
    list(pieces = pieces, f = ag$G[solve_ids] / om0)
  }
  best <- NULL; worse <- 0L
  for (it in seq_len(maxit_P)) {
    last <- eval_at(P)
    if (is.null(last)) { cat("    ", label, "- evaluation failed\n"); return(NULL) }
    m <- max(abs(last$f))
    cat(sprintf("    [%s %s] premiums %d  max |f| = %.2f $  %.1f min\n",
                yr$y, label, it, m, elapsed()))
    if (is.null(best) || m < max(abs(best$f))) {
      best <- list(P = P, pieces = last$pieces, f = last$f); worse <- 0L
    } else worse <- worse + 1L
    if (m < tol_dollars) break
    # A non-contracting map hands its best point to the Jacobian solve instead
    # of grinding to the iteration cap
    if (worse >= 5L) break
    P[solve_ids] <- P[solve_ids] + pmax(pmin(kappa * last$f, step_cap), -step_cap)
  }
  list(P = best$P, pieces = best$pieces, iter = n,
       converged = max(abs(best$f)) < tol_dollars, elapsed = elapsed())
}

# solve_cf_year -------------------------------------------------------------
# yr:       list(y, cl, active [logical per node]) from cf1
# solve_ids: plan ids priced in the solve (others held at observed)
# P_init:   named start (plan ids)
# J_P:      the year's premium Jacobian from cf_year_jacobian_P
# Returns list(sol, P, pieces [at the solution], n_eval, elapsed) or NULL.
solve_cf_year <- function(yr, label, solve_ids, P_init, J_P, tol_dollars = 1,
                          om_base = NULL) {
  nP <- length(solve_ids)
  st <- new.env(parent = emptyenv())
  st$n_eval <- 0L; st$t0 <- Sys.time(); st$pieces <- NULL
  # Residual scale from the baseline own-price terms when supplied (see
  # solve_cf_year_fixed_point); first-evaluation terms otherwise
  st$scale <- if (!is.null(om_base)) 1 / om_base[solve_ids] else NULL

  fn <- function(x) {
    P <- setNames(x, solve_ids)
    pieces <- cf_year_evaluate(yr$cl, P)
    st$n_eval <- st$n_eval + 1L
    if (is.null(pieces) || !all(!vapply(pieces[yr$active], is.null, logical(1))))
      return(rep(NA_real_, length(x)))
    ag <- cf_year_aggregate(pieces)
    # Units: the premium conditions in dollars per member-month (divided by the
    # own-price term at the start point), so the acceptance rule is max |f| <
    # tol_dollars.
    if (is.null(st$scale)) st$scale <- 1 / ag$omega_w[solve_ids]
    f <- unname(ag$G[solve_ids]) * st$scale
    st$pieces <- pieces
    cat(sprintf("    [%s %s] eval %d  |f| = %.3g  max %.2f  %.1f min\n", yr$y, label, st$n_eval,
                sqrt(sum(f^2)), max(abs(f)), as.numeric(difftime(Sys.time(), st$t0, units = "mins"))))
    f
  }
  jac <- function(x) J_P[solve_ids, solve_ids] * st$scale
  miss <- function(x) { f <- fn(x); max(abs(f)) }

  x_init <- unname(P_init[solve_ids])
  f0 <- fn(x_init)
  if (any(is.na(f0))) { cat("    ", label, "- evaluation failed at the start\n"); return(NULL) }
  sol <- tryCatch(
    nleqslv(x = x_init, fn = fn, jac = jac, method = "Broyden", global = "hook",
            xscalm = "auto",
            control = list(maxit = 150, xtol = 1e-6, ftol = 0.2 * tol_dollars, allowSingular = TRUE)),
    error = function(e) { cat("    nleqslv error:", conditionMessage(e), "\n"); NULL })
  if (is.null(sol)) return(NULL)
  m <- miss(sol$x)
  if (sol$termcd != 1 && !(is.finite(m) && m < tol_dollars)) {
    cat(sprintf("    nleqslv termcd: %d, |f|: %.4g, max miss %.2f $\n",
                sol$termcd, sqrt(sum(sol$fvec^2)), m))
    # Broyden stalled away from the root: Newton steps from the stalled point
    sol2 <- tryCatch(
      nleqslv(x = sol$x, fn = fn, jac = jac, method = "Newton", global = "dbldog",
              xscalm = "auto",
              control = list(maxit = 40, xtol = 1e-6, ftol = 0.2 * tol_dollars, allowSingular = TRUE)),
      error = function(e) NULL)
    if (!is.null(sol2)) {
      m2 <- miss(sol2$x)
      cat(sprintf("    retry termcd: %d, |f|: %.4g, max miss %.2f $\n",
                  sol2$termcd, sqrt(sum(sol2$fvec^2)), m2))
      if (is.finite(m2) && m2 < m) { sol <- sol2; m <- m2 }
    }
    if (!(is.finite(m) && m < tol_dollars)) { cat("    ", label, "- not converged, dropped\n"); return(NULL) }
  }
  # Pieces at the solution (the solver's last evaluation may not be at sol$x)
  invisible(fn(sol$x))
  cat(sprintf("    residual at the solution: max %.2f $/member-month on the premium conditions\n", m))
  list(sol = sol, P = setNames(sol$x, solve_ids), pieces = st$pieces,
       n_eval = st$n_eval, elapsed = as.numeric(difftime(Sys.time(), st$t0, units = "mins")))
}

# solve_cf_commissions -------------------------------------------------------
# The commission equilibrium for a scenario: each insurer in `firms` chooses
# its rate scale k_f on the observed schedule to satisfy the estimated
# condition MB/MC = (1 - beta_f) + wedge_f, with the premium equilibrium
# re-solved inside every round. The update is a damped proportional step on
# the condition's residual phi_f = MB/MC - (1 - beta_f) - wedge_f (phi > 0:
# the marginal commission dollar over-earns, raise the rate), capped per round
# and bounded in [k_min, k_max]; a firm pinned at k_min with phi < 0 is at its
# corner. The commission-derivative kernel runs only at the condition
# evaluations (cf_cell_set_calib), not during the premium iterations.
#
# yr / solve_ids / J_P / om_base as in solve_cf_year. spec_base carries the
# scenario's household conversions (tau / broker_remain / defund) and nothing
# about commissions. beta_f, wedge_f: named by firm. Returns list(P, pieces,
# k, phi, rounds, converged) or NULL.
solve_cf_commissions <- function(yr, label, solve_ids, P_init, J_P, spec_base,
                                 firms, beta_f, wedge_f, k_init = NULL,
                                 om_base = NULL, tol_phi = 0.05, max_rounds = 20,
                                 damp = 0.5, move_cap = 0.25,
                                 k_min = 0.02, k_max = 10) {
  k <- setNames(rep(1, length(firms)), firms)
  if (!is.null(k_init)) k[names(k_init)[names(k_init) %in% firms]] <-
    k_init[names(k_init) %in% firms]
  P <- P_init
  pieces_sol <- NULL
  phi <- setNames(rep(NA_real_, length(firms)), firms)
  t0 <- Sys.time()
  for (round in seq_len(max_rounds)) {
    spec <- spec_base
    spec$comm <- "firmscale"
    spec$k_firm <- k
    invisible(parallel::clusterCall(yr$cl, cf_cell_scenario, label, spec))

    # Premium equilibrium at the current schedules: pre-iterate when far, then
    # the Broyden polish with the year's Jacobian
    pieces0 <- cf_year_evaluate(yr$cl, P)
    if (is.null(pieces0) || !all(!vapply(pieces0[yr$active], is.null, logical(1)))) {
      cat("    ", label, "- evaluation failed in the commission loop\n"); return(NULL)
    }
    ag0 <- cf_year_aggregate(pieces0)
    if (round == 1) {
      # Gate the priced set on the scenario's own first-evaluation shares (a
      # household conversion can empty a plan, whose pricing condition is then
      # ill-conditioned); plans below the floor stay at their start premiums
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
      ids_ok <- solve_ids[solve_ids %in% names(share_scen)[share_scen >= SHARE_FLOOR_FOC]]
      if (length(ids_ok) == 0) { cat("    ", label, "- no plans above the share floor\n"); return(NULL) }
      solve_ids <- ids_ok
    }
    f0 <- ag0$G[solve_ids] /
      (if (!is.null(om_base)) om_base[solve_ids] else ag0$omega_w[solve_ids])
    if (max(abs(f0), na.rm = TRUE) > 25) {
      fp_s <- solve_cf_year_fixed_point(yr, label, solve_ids, P, om_base = om_base,
                                        maxit_P = 25, tol_dollars = 5)
      if (!is.null(fp_s)) P <- fp_s$P
    }
    # Premiums within the acceptance band from the last round's solution need
    # no polish; the condition evaluation follows either way
    if (max(abs(f0), na.rm = TRUE) >= 5) {
      res <- solve_cf_year(yr, label, solve_ids, P, J_P, tol_dollars = 5, om_base = om_base)
      if (is.null(res)) { cat("    ", label, "- premium solve failed in the commission loop\n"); return(NULL) }
      P[names(res$P)] <- res$P
    }

    # The commission condition at the solved premiums
    invisible(parallel::clusterCall(yr$cl, cf_cell_set_calib, TRUE))
    pieces_c <- cf_year_evaluate(yr$cl, P)
    invisible(parallel::clusterCall(yr$cl, cf_cell_set_calib, FALSE))
    if (is.null(pieces_c) || !all(!vapply(pieces_c[yr$active], is.null, logical(1)))) {
      cat("    ", label, "- condition evaluation failed\n"); return(NULL)
    }
    ag <- cf_year_aggregate(pieces_c)
    ok_f <- firms[firms %in% names(ag$MC) & ag$MC[firms] > 0 & is.finite(ag$MB[firms])]
    phi[] <- NA_real_
    phi[ok_f] <- ag$MB[ok_f] / ag$MC[ok_f] - (1 - beta_f[ok_f]) - wedge_f[ok_f]
    pieces_sol <- pieces_c

    at_floor <- k[ok_f] <= k_min * 1.001 & phi[ok_f] < 0
    active_f <- ok_f[!at_floor]
    worst <- if (length(active_f) > 0) max(abs(phi[active_f]), na.rm = TRUE) else 0
    cat(sprintf("    [%s %s] commissions round %d  max |phi| = %.3f  (%d firms, %d at floor)  %.1f min\n",
                yr$y, label, round, worst, length(ok_f), sum(at_floor),
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    if (worst < tol_phi) {
      return(list(P = P, pieces = pieces_sol, k = k, phi = phi,
                  rounds = round, converged = TRUE))
    }
    if (round < max_rounds) {
      step <- pmax(pmin(damp * phi[ok_f], move_cap), -move_cap)
      k[ok_f] <- pmax(pmin(k[ok_f] * (1 + step), k_max), k_min)
    }
  }
  cat("    ", label, "- commission loop hit max rounds; keeping the last point\n")
  list(P = P, pieces = pieces_sol, k = k, phi = phi,
       rounds = max_rounds, converged = FALSE)
}

# cf_year_firm_rows ----------------------------------------------------------
# Insurer-year profit and agent enrollment at a solution, summed over the
# cells' phase-2 pieces (monthly, sample units; scale by 12 / SAMPLE_FRAC for
# annual market dollars). Feeds the commission adjustment-cost analysis.
cf_year_firm_rows <- function(yr, label, pieces, k = NULL, phi = NULL) {
  prof <- qb <- setNames(numeric(0), character(0))
  for (pc in pieces) {
    if (is.null(pc) || is.null(pc$firm_profit)) next
    for (f in names(pc$firm_profit)) {
      prof[f] <- (if (is.na(prof[f])) 0 else prof[f]) + pc$firm_profit[[f]]
      qb[f]   <- (if (is.na(qb[f])) 0 else qb[f]) + pc$firm_qB[[f]]
    }
  }
  fn <- names(prof)
  tibble(year = yr$y, scenario = label, firm = fn,
         profit_month = unname(prof), agent_members = unname(qb[fn]),
         k_cf = if (is.null(k)) NA_real_ else unname(k[fn]),
         phi_cf = if (is.null(phi)) NA_real_ else unname(phi[fn]))
}

# cf_year_rows --------------------------------------------------------------
# Per-cell result rows in the counterfactual_results layout from the pieces at
# a solution. P_full: base premiums (solved plans; observed for the rest).
# comm_scale: the scenario's commission multiplier on the observed schedules
# (1 for point runs at observed commissions, the band multiplier for edge runs,
# 0 for the ban; NA when the scenario's schedule is not a multiple of observed).
cf_year_rows <- function(yr, label, tau, pieces, P_full, comm_scale = 1,
                         termcd, iter) {
  rows <- list()
  for (ci in seq_along(pieces)) {
    pc <- pieces[[ci]]; cs <- yr$cells[[ci]]
    if (is.null(pc) || is.null(cs)) next
    pn <- pc$plan_ids
    rows[[length(rows) + 1]] <- tibble(
      region = cs$r, year = cs$y, scenario = label, tau = tau, plan_id = pn,
      premium_obs = unname(cs$p_obs[pn]), premium_cf = unname(pc$p[pn]),
      premium_change = unname(pc$p[pn] - cs$p_obs[pn]),
      share_obs = unname(cs$share_obs[pn]), share_cf = unname(pc$shares[pn]),
      mc = unname(pc$mc[pn]), claims = unname(pc$claims[pn]),
      commission_pmpm = unname(pc$eta[pn]),
      markup_cf = unname(pc$p[pn] - pc$mc[pn]),
      nleqslv_termcd = termcd, nleqslv_iter = iter,
      comm_scale_cf = comm_scale,
      base_premium_cf = unname(P_full[pn]))
  }
  bind_rows(rows)
}
