# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Year-level counterfactual solver (master side). Each plan is
##                priced once for the year: cell premium = plan-year base
##                premium P_jy x fixed regional factor g_jc. The pricing
##                condition for P_jy is the member- and factor-weighted sum of
##                the cells' first-order conditions, G_jy = sum_c N_c g_jc
##                resid_jc; the commission condition is per insurer-year with
##                the cells' pieces summed with member weights. Both are held
##                at their observed-point residuals (e_jy and the shift b_obs
##                in the complementarity condition), so the observed premiums
##                and commissions are the baseline equilibrium; residual_scale
##                = 0 is the zero-residual robustness run. One function
##                evaluation = phase 1 on every cell of the year (parallel, one
##                worker per cell, helpers/cf_cell.R), the statewide transfer
##                sums, then phase 2. Called by cf1_estimate.R.

# cf_year_aggregate ---------------------------------------------------------
# Plan-year and insurer-year conditions from the cells' phase-2 pieces.
#   endog:      NULL or list(prefixes, comm_scale = 1 - beta, MC_obs, b_obs) by insurer
#   direct_obs: per-cell list of the pct direct term at the observed point
#   k:          insurer scales (endogenous scenarios)
cf_year_aggregate <- function(pieces, endog = NULL, direct_obs = NULL, k = NULL,
                              residual_scale = 1) {
  G_num <- G_den <- Jd <- Ow <- setNames(numeric(0), character(0))
  MB <- MC <- qB <- setNames(numeric(0), character(0))
  for (ci in seq_along(pieces)) {
    pc <- pieces[[ci]]
    if (is.null(pc)) next
    pn <- pc$plan_ids
    w  <- pc$N * pc$g
    res <- pc$resid
    if (!is.null(endog)) {
      d0 <- if (!is.null(direct_obs[[ci]])) direct_obs[[ci]][pn] else 0
      d0[is.na(d0)] <- 0
      res <- res - pc$direct + d0
    }
    add <- function(v, pn, x) { v[setdiff(pn, names(v))] <- 0; v[pn] <- v[pn] + x; v }
    G_num <- add(G_num, pn, w * res)
    G_den <- add(G_den, pn, w)
    Jd    <- add(Jd, pn, -2 * w * pc$g * pc$omega_own)
    Ow    <- add(Ow, pn, w * pc$omega_own)
    for (f in names(pc$MB)) {
      MB[f] <- (if (is.na(MB[f])) 0 else MB[f]) + pc$N * pc$MB[[f]]
      MC[f] <- (if (is.na(MC[f])) 0 else MC[f]) + pc$N * pc$MC[[f]]
      qB[f] <- (if (is.na(qB[f])) 0 else qB[f]) + pc$N * pc$qB[[f]]
    }
  }
  G <- G_num / G_den
  Jdiag <- Jd / G_den
  omega_w <- Ow / G_den                      # own-price term: G / omega_w is the residual in dollars
  b <- NULL
  if (!is.null(endog) && length(endog$prefixes) > 0) {
    f <- endog$prefixes
    MBf <- MB[f]; MCf <- MC[f]
    MBf[is.na(MBf)] <- 0; MCf[is.na(MCf)] <- 0
    # Commission condition MB = (1 - beta) MC, as a gap relative to the observed
    # net outlay, less its observed-point value (held): b > 0 means the marginal
    # commission dollar returns less than its net cost, so the insurer cuts.
    cs <- endog$comm_scale
    b <- setNames((cs * MCf - MBf) / (cs * endog$MC_obs[f]) - residual_scale * endog$b_obs[f], f)
  }
  list(G = G, Jdiag = Jdiag, omega_w = omega_w, MB = MB, MC = MC, qB = qB, b = b)
}

# cf_year_evaluate ----------------------------------------------------------
# One full evaluation of the year at (P, k): phase 1 on every worker, the
# statewide transfer sums, phase 2. Returns the pieces (NULL on a failed cell).
cf_year_evaluate <- function(cl, P, k = NULL) {
  recs <- parallel::clusterCall(cl, cf_cell_eval_p1, P, k)
  ok <- !vapply(recs, is.null, logical(1))
  if (!any(ok)) return(NULL)
  st <- ra_state_totals(recs[ok])
  pieces <- parallel::clusterCall(cl, cf_cell_eval_p2, st$totals, st$own)
  pieces
}

# solve_cf_year -------------------------------------------------------------
# yr:       list(y, cl, active [logical per node]) from cf1
# solve_ids: plan ids priced in the solve (others held at observed)
# P_init:   named start (plan ids), k_init: named start by insurer (endogenous)
# target:   held plan-year residuals e (named by plan id)
# Returns list(sol, P, k, pieces [at the solution], n_eval, elapsed) or NULL.
solve_cf_year <- function(yr, label, solve_ids, P_init, k_init = NULL, target,
                          endog = NULL, direct_obs = NULL, residual_scale = 1) {
  nP <- length(solve_ids)
  firms <- if (is.null(endog)) character(0) else endog$prefixes
  st <- new.env(parent = emptyenv())
  st$n_eval <- 0L; st$t0 <- Sys.time(); st$pieces <- NULL; st$Jdiag <- NULL; st$b <- NULL
  k_full <- if (length(firms)) setNames(unname(k_init[firms]), firms) else NULL

  solve_pass <- function(free) {
    nk <- length(free)
    unstack <- function(x) {
      k <- k_full
      if (nk > 0) k[free] <- x[nP + seq_len(nk)]
      list(P = setNames(x[seq_len(nP)], solve_ids), k = k)
    }
    fn <- function(x) {
      u <- unstack(x)
      pieces <- cf_year_evaluate(yr$cl, u$P, u$k)
      st$n_eval <- st$n_eval + 1L
      if (is.null(pieces) || !all(!vapply(pieces[yr$active], is.null, logical(1))))
        return(rep(NA_real_, length(x)))
      ag <- cf_year_aggregate(pieces, endog, direct_obs, u$k, residual_scale)
      st$pieces <- pieces; st$Jdiag <- ag$Jdiag; st$b <- ag$b
      f <- c(ag$G[solve_ids] - residual_scale * target[solve_ids], if (nk > 0) ag$b[free])
      cat(sprintf("    [%s %s] eval %d  |f| = %.3g  %.1f min\n", yr$y, label, st$n_eval,
                  sqrt(sum(f^2)), as.numeric(difftime(Sys.time(), st$t0, units = "mins"))))
      unname(f)
    }
    # Start-up Jacobian: the own-price term of each plan-year condition on the
    # premium block (diagonal), and forward-difference columns for the commission
    # scales (nk extra evaluations), since the commission conditions couple to
    # every plan through the steering shares and a diagonal start stalls there.
    # Broyden updates from this and returns to it on a restart.
    x_init <- c(unname(P_init[solve_ids]), if (nk > 0) unname(k_full[free]))
    f0 <- fn(x_init)
    if (any(is.na(f0))) { cat("    ", label, "- evaluation failed at the start\n"); return(NULL) }
    J0 <- diag(c(unname(st$Jdiag[solve_ids]), rep(1, nk)), nP + nk)
    if (nk > 0) for (j in seq_len(nk)) {
      h <- 1e-2
      xh <- x_init; xh[nP + j] <- xh[nP + j] + h
      fh <- fn(xh)
      if (!any(is.na(fh))) J0[, nP + j] <- (fh - f0) / h
    }
    jac <- function(x) J0
    sol <- tryCatch(
      nleqslv(x = x_init, fn = fn, jac = jac, method = "Broyden", global = "hook",
              xscalm = "auto",
              control = list(maxit = 150, xtol = 1e-6, ftol = 1e-8, allowSingular = TRUE)),
      error = function(e) { cat("    nleqslv error:", conditionMessage(e), "\n"); NULL })
    if (is.null(sol)) return(NULL)
    f_norm <- sqrt(sum(sol$fvec^2))
    if (sol$termcd > 2) {
      cat("    nleqslv termcd:", sol$termcd, ", |f|:", signif(f_norm, 4), "\n")
      if (f_norm >= 0.05) {
        # Broyden stalled away from the root: restart from the stalled point with
        # a numerical start-up Jacobian and a dogleg step
        sol2 <- tryCatch(
          nleqslv(x = sol$x, fn = fn, method = "Broyden", global = "dbldog", xscalm = "auto",
                  control = list(maxit = 150, xtol = 1e-6, ftol = 1e-8, allowSingular = TRUE)),
          error = function(e) NULL)
        if (!is.null(sol2)) {
          f2 <- sqrt(sum(sol2$fvec^2))
          cat("    retry termcd:", sol2$termcd, ", |f|:", signif(f2, 4), "\n")
          if (is.finite(f2) && f2 < f_norm) { sol <- sol2; f_norm <- f2 }
        }
      }
      if (f_norm >= 0.05) { cat("    ", label, "- not converged, dropped\n"); return(NULL) }
      if (sol$termcd > 2) cat("    Accepting with small residual\n")
    }
    # Pieces at the solution (the solver's last evaluation may not be at sol$x)
    f_sol <- fn(sol$x)
    u <- unstack(sol$x)
    list(sol = sol, P = u$P, k = u$k, b = st$b)
  }

  res <- solve_pass(firms)
  if (is.null(res)) return(NULL)
  list(sol = res$sol, P = res$P, k = res$k, pieces = st$pieces,
       n_eval = st$n_eval, elapsed = as.numeric(difftime(Sys.time(), st$t0, units = "mins")))
}

# cf_year_rows --------------------------------------------------------------
# Per-cell result rows in the counterfactual_results layout from the pieces at
# a solution. P: base premiums (solved plans; observed for the rest), k: insurer
# scales (NA -> commissions exogenous in the scenario).
cf_year_rows <- function(yr, label, tau, pieces, P_full, k = NULL, endog = NULL, mu = NULL,
                         termcd, iter) {
  rows <- list()
  for (ci in seq_along(pieces)) {
    pc <- pieces[[ci]]; cs <- yr$cells[[ci]]
    if (is.null(pc) || is.null(cs)) next
    pn <- pc$plan_ids
    kp <- setNames(rep(NA_real_, length(pn)), pn); mp <- kp
    if (!is.null(endog)) for (f in endog$prefixes) {
      kp[cs$prefix == f] <- if (!is.null(k)) k[[f]] else 1
      mp[cs$prefix == f] <- if (!is.null(mu) && f %in% names(mu)) mu[[f]] else 0
    }
    rows[[length(rows) + 1]] <- tibble(
      region = cs$r, year = cs$y, scenario = label, tau = tau, plan_id = pn,
      premium_obs = unname(cs$p_obs[pn]), premium_cf = unname(pc$p[pn]),
      premium_change = unname(pc$p[pn] - cs$p_obs[pn]),
      share_obs = unname(cs$share_obs[pn]), share_cf = unname(pc$shares[pn]),
      mc = unname(pc$mc[pn]), claims = unname(pc$claims[pn]),
      commission_pmpm = unname(pc$eta[pn]),
      markup_cf = unname(pc$p[pn] - pc$mc[pn]),
      nleqslv_termcd = termcd, nleqslv_iter = iter,
      comm_scale_cf = unname(kp), mu_comm = unname(mp),
      base_premium_cf = unname(P_full[pn]))
  }
  bind_rows(rows)
}
