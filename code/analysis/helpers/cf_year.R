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
#   endog:      NULL or list(prefixes, comm_scale = 1 - beta, b_obs) by insurer
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
    # Commission condition MB = (1 - beta) MC as the ratio gap 1 - MB / ((1 - beta) MC),
    # the same object as the estimation's M4 residual, less its observed-point
    # value (held). A ratio is invariant to the size of the broker pool, which the
    # scenarios change; b > 0 means the marginal commission dollar returns less
    # than its net cost, so the insurer cuts.
    cs <- unname(endog$comm_scale[f])            # (1 - beta) by insurer
    b <- setNames(1 - MBf / (cs * MCf) - residual_scale * endog$b_obs[f], f)
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

# cf_year_jacobian_P ---------------------------------------------------------
# Numerical derivative of the year's conditions in the base premiums at (P, k)
# under the scenario set on the workers: forward differences of h dollars on
# each solved plan (one evaluation per plan). Rows: the plan-year conditions
# (solve_ids), then the commission conditions of the endogenous insurers;
# columns: solve_ids. cf1 computes it once per year at the observed point and
# every scenario of the year starts its solve from it. The first-order analytic
# block (share derivatives and Omega) is not adequate here: the markups are as
# large as the price scale of the within-nest logit, so the curvature term
# (p - mc) d Omega / d p is of the same order as Omega itself.
cf_year_jacobian_P <- function(yr, solve_ids, P, k = NULL, target, endog = NULL,
                               direct_obs = NULL, residual_scale = 1, h = 1) {
  firms <- if (is.null(endog)) character(0) else endog$prefixes
  f_at <- function(P) {
    pieces <- cf_year_evaluate(yr$cl, P, k)
    if (is.null(pieces) || !all(!vapply(pieces[yr$active], is.null, logical(1)))) return(NULL)
    ag <- cf_year_aggregate(pieces, endog, direct_obs, k, residual_scale)
    c(ag$G[solve_ids] - residual_scale * target[solve_ids], if (length(firms)) ag$b[firms])
  }
  t0 <- Sys.time()
  f0 <- f_at(P)
  if (is.null(f0)) return(NULL)
  J <- matrix(NA_real_, length(f0), length(solve_ids), dimnames = list(c(solve_ids, firms), solve_ids))
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
# The model's equilibrium from a distant start (the observed premiums), with
# the residuals at zero: damped best-response iteration on the base premiums,
# P <- P + kappa f, with f the pricing residual in dollars per member-month
# (the gap between the markup the model wants and the markup at P), which
# converges to the stable fixed point from the far side of the fold where a
# Newton step from the observed point does not. The gap is measured with the
# own-price terms at the start (a fixed scale), the premium step is kappa times
# the gap (the iteration converges for own slopes down to -2 / kappa dollars of
# residual per dollar of premium; the steepest plans are near -6) and capped at
# step_cap dollars per iteration, and each commission scale moves by a damped
# proportional step, k <- k exp(-kappa_k b). Stops when every pricing
# condition is within tol_dollars and every commission condition within tol_b;
# the result is then polished by solve_cf_year with the Jacobian computed at
# the fixed point.
solve_cf_year_fixed_point <- function(yr, label, solve_ids, P_init, k_init = NULL, endog = NULL,
                                      kappa = 0.15, step_cap = 25, tol_dollars = 1, tol_b = 0.01,
                                      maxit_P = 60, rounds = 3) {
  firms <- if (is.null(endog)) character(0) else endog$prefixes
  P <- P_init
  k <- if (length(firms)) setNames(unname(k_init[firms]), firms) else NULL
  t0 <- Sys.time(); om0 <- NULL; n <- 0L; last <- NULL
  elapsed <- function() as.numeric(difftime(Sys.time(), t0, units = "mins"))
  # One evaluation at (P, k): the pricing gaps in dollars (fixed scale) and the
  # commission gaps
  eval_at <- function(P, k) {
    pieces <- cf_year_evaluate(yr$cl, P, k)
    if (is.null(pieces) || !all(!vapply(pieces[yr$active], is.null, logical(1)))) return(NULL)
    ag <- cf_year_aggregate(pieces, endog, NULL, k, residual_scale = 0)
    if (is.null(om0)) om0 <<- ag$omega_w[solve_ids]
    n <<- n + 1L
    list(pieces = pieces, f = ag$G[solve_ids] / om0, b = if (length(firms)) ag$b[firms] else numeric(0))
  }
  done <- function(e) max(abs(e$f)) < tol_dollars && (!length(e$b) || max(abs(e$b)) < tol_b)
  for (round in seq_len(rounds)) {
    # Premiums, commissions fixed: P <- P + kappa f, capped
    for (it in seq_len(maxit_P)) {
      last <- eval_at(P, k)
      if (is.null(last)) { cat("    ", label, "- evaluation failed\n"); return(NULL) }
      cat(sprintf("    [%s %s] round %d premiums %d  max |f| = %.2f $  max |b| = %.4f  %.1f min\n",
                  yr$y, label, round, it, max(abs(last$f)), if (length(last$b)) max(abs(last$b)) else 0, elapsed()))
      if (max(abs(last$f)) < tol_dollars) break
      P[solve_ids] <- P[solve_ids] + pmax(pmin(kappa * last$f, step_cap), -step_cap)
    }
    if (done(last)) break
    if (!length(firms)) next
    # Each insurer's commission scale on its own condition, premiums fixed: a
    # secant in log k from the current point and a 25 percent move, at most
    # 6 evaluations per insurer; a condition still positive as k -> 0 is a
    # corner and k stops at 0.02
    for (fm in firms) {
      k0 <- k[fm]; b0 <- last$b[fm]
      if (abs(b0) < tol_b) next
      k1 <- k0 * exp(-sign(b0) * 0.25); k_try <- k; k_try[fm] <- k1
      e1 <- eval_at(P, k_try); if (is.null(e1)) next
      b1 <- e1$b[fm]; n_f <- 1L
      while (abs(b1) >= tol_b && n_f < 6 && k1 > 0.02) {
        slope <- (b1 - b0) / (log(k1) - log(k0))
        k_new <- if (is.finite(slope) && slope > 1e-6) exp(log(k1) - b1 / slope) else k1 * exp(-sign(b1) * 0.5)
        k_new <- max(min(k_new, 4 * k1), k1 / 4, 0.02)
        k0 <- k1; b0 <- b1; k1 <- k_new; k_try[fm] <- k1
        e1 <- eval_at(P, k_try); if (is.null(e1)) break
        b1 <- e1$b[fm]; n_f <- n_f + 1L
      }
      k[fm] <- k1
      cat(sprintf("    [%s %s] round %d commission %s  k = %.3f  b = %.4f  (%d evaluations)  %.1f min\n",
                  yr$y, label, round, fm, k1, b1, n_f, elapsed()))
    }
    last <- eval_at(P, k)
    if (is.null(last)) { cat("    ", label, "- evaluation failed\n"); return(NULL) }
    cat(sprintf("    [%s %s] round %d end  max |f| = %.2f $  max |b| = %.4f  k: %s  %.1f min\n",
                yr$y, label, round, max(abs(last$f)), if (length(last$b)) max(abs(last$b)) else 0,
                paste(names(k), round(k, 3), collapse = " "), elapsed()))
    if (done(last)) break
  }
  list(P = P, k = k, pieces = last$pieces, iter = n, converged = done(last), elapsed = elapsed())
}

# solve_cf_year -------------------------------------------------------------
# yr:       list(y, cl, active [logical per node]) from cf1
# solve_ids: plan ids priced in the solve (others held at observed)
# P_init:   named start (plan ids), k_init: named start by insurer (endogenous)
# target:   held plan-year residuals e (named by plan id)
# J_P:      the year's premium Jacobian from cf_year_jacobian_P
# Returns list(sol, P, k, pieces [at the solution], n_eval, elapsed) or NULL.
solve_cf_year <- function(yr, label, solve_ids, P_init, k_init = NULL, target,
                          endog = NULL, direct_obs = NULL, residual_scale = 1, J_P,
                          tol_dollars = 1, tol_b = 0.01) {
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
      f <- c(ag$G[solve_ids] - residual_scale * target[solve_ids], if (nk > 0) ag$b[free])
      # Units: the premium conditions in dollars per member-month (divided by the
      # own-price term at the start point) and the commission conditions in
      # percent of the net outlay, so every condition is comparable in the norm
      # the solver works with and the acceptance rule is max |f| < 1.
      if (is.null(st$scale)) st$scale <- c(1 / ag$omega_w[solve_ids], rep(100, nk))
      f <- unname(f) * st$scale
      st$pieces <- pieces; st$Jdiag <- ag$Jdiag; st$b <- ag$b; st$omega_w <- ag$omega_w
      st$x_last <- x; st$f_last <- f
      cat(sprintf("    [%s %s] eval %d  |f| = %.3g  max %.2f  %.1f min\n", yr$y, label, st$n_eval,
                  sqrt(sum(f^2)), max(abs(f)), as.numeric(difftime(Sys.time(), st$t0, units = "mins"))))
      f
    }
    # Jacobian at x: the premium block and the commission rows' premium
    # derivatives from the year's numerical Jacobian (J_P, observed point, share
    # units, scaled to the units above); the commission-scale columns are
    # forward differences at x (nk extra evaluations). Broyden starts from it and
    # returns to it at a restart; the Newton retry uses it every iteration.
    jac <- function(x) {
      J <- matrix(0, nP + nk, nP + nk)
      J[seq_len(nP), seq_len(nP)] <- J_P[solve_ids, solve_ids] * st$scale[seq_len(nP)]
      if (nk > 0) {
        J[nP + seq_len(nk), seq_len(nP)] <- J_P[free, solve_ids] * 100
        if (is.null(st$x_last) || length(st$x_last) != length(x) || any(st$x_last != x))
          invisible(fn(x))
        keep <- mget(c("pieces", "Jdiag", "b", "omega_w", "x_last", "f_last"), envir = st)
        f0 <- keep$f_last
        for (j in seq_len(nk)) {
          h <- 1e-2
          xh <- x; xh[nP + j] <- xh[nP + j] + h
          fh <- fn(xh)
          if (!any(is.na(fh))) J[, nP + j] <- (fh - f0) / h
        }
        for (nm in names(keep)) assign(nm, keep[[nm]], envir = st)
      }
      J
    }
    # The miss at a point: the largest premium-condition residual in dollars per
    # member-month and the largest commission-condition residual (a ratio). A
    # solve is accepted when the first is under tol_dollars and the second under
    # tol_b (the plans whose condition is nearly flat in their own price, a few
    # small platinum plans, are where the last dollars of miss sit).
    miss <- function(x) {
      f <- fn(x)
      list(dollars = max(abs(f[seq_len(nP)])),
           b = if (nk > 0) max(abs(f[nP + seq_len(nk)])) / 100 else 0)
    }
    ok <- function(m) is.finite(m$dollars) && m$dollars < tol_dollars && m$b < tol_b
    x_init <- c(unname(P_init[solve_ids]), if (nk > 0) unname(k_full[free]))
    st$scale <- NULL
    f0 <- fn(x_init)
    if (any(is.na(f0))) { cat("    ", label, "- evaluation failed at the start\n"); return(NULL) }
    sol <- tryCatch(
      nleqslv(x = x_init, fn = fn, jac = jac, method = "Broyden", global = "hook",
              xscalm = "auto",
              control = list(maxit = 150, xtol = 1e-6, ftol = 1e-4, allowSingular = TRUE)),
      error = function(e) { cat("    nleqslv error:", conditionMessage(e), "\n"); NULL })
    if (is.null(sol)) return(NULL)
    m <- miss(sol$x)
    if (sol$termcd != 1 && !ok(m)) {
      cat(sprintf("    nleqslv termcd: %d, |f|: %.4g, max miss %.2f $ / %.4f\n",
                  sol$termcd, sqrt(sum(sol$fvec^2)), m$dollars, m$b))
      # Broyden stalled away from the root: Newton steps from the stalled point
      sol2 <- tryCatch(
        nleqslv(x = sol$x, fn = fn, jac = jac, method = "Newton", global = "dbldog",
                xscalm = "auto",
                control = list(maxit = 40, xtol = 1e-6, ftol = 1e-4, allowSingular = TRUE)),
        error = function(e) NULL)
      if (!is.null(sol2)) {
        m2 <- miss(sol2$x)
        cat(sprintf("    retry termcd: %d, |f|: %.4g, max miss %.2f $ / %.4f\n",
                    sol2$termcd, sqrt(sum(sol2$fvec^2)), m2$dollars, m2$b))
        if (is.finite(m2$dollars) && m2$dollars < m$dollars) { sol <- sol2; m <- m2 }
      }
      if (!ok(m)) { cat("    ", label, "- not converged, dropped\n"); return(NULL) }
    }
    # Pieces at the solution (the solver's last evaluation may not be at sol$x)
    f_sol <- fn(sol$x)
    cat(sprintf("    residual at the solution: max %.2f $/member-month on the premium conditions%s\n",
                m$dollars, if (nk > 0) sprintf(", max |b| %.4f on the commission conditions", m$b) else ""))
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
