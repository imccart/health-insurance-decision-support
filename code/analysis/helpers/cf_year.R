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

# Terminal output is a dot tracker: "." per solver evaluation, "|" per
# commission round, one line per finished or failed scenario. The full status
# lines go to a run log with timestamps, so a failed or skipped year's reason
# survives the terminal scrollback. quiet = TRUE writes to the log only.
CF_RUN_LOG <- file.path(TEMP_DIR, "cf_years", "run_log.txt")
cf_log <- function(msg, quiet = FALSE) {
  if (!quiet) cat(paste0("\n", msg))
  dir.create(dirname(CF_RUN_LOG), showWarnings = FALSE, recursive = TRUE)
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M] "), msg, file = CF_RUN_LOG, sep = "", append = TRUE)
}

# cf_year_aggregate ---------------------------------------------------------
# Plan-year pricing conditions and insurer-year commission aggregates (MB, MC,
# qB; populated only when the scenario computes commission derivatives) from
# the cells' phase-2 pieces.
cf_year_aggregate <- function(pieces) {
  G_num <- G_den <- Ow <- setNames(numeric(0), character(0))
  MB <- MC <- qB <- setNames(numeric(0), character(0))
  MBs <- MBr <- MBf <- setNames(numeric(0), character(0))
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
      MBs[f] <- (if (is.na(MBs[f])) 0 else MBs[f]) + pc$N * pc$MB_steer[[f]]
      MBr[f] <- (if (is.na(MBr[f])) 0 else MBr[f]) + pc$N * pc$MB_ra[[f]]
      MBf[f] <- (if (is.na(MBf[f])) 0 else MBf[f]) + pc$N * pc$MB_fb[[f]]
    }
  }
  G <- G_num / G_den
  omega_w <- Ow / G_den                      # own-price term at P: G / omega_w is the residual in dollars
  list(G = G, omega_w = omega_w, MB = MB, MB_steer = MBs, MB_ra = MBr, MB_fb = MBf, MC = MC, qB = qB)
}

# cf_year_evaluate ----------------------------------------------------------
# One full evaluation of the year at P: phase 1 on every worker, the statewide
# transfer sums, phase 2. Returns the pieces (NULL on a failed cell).
cf_year_evaluate <- function(cl, P) {
  recs <- parallel::clusterCall(cl, cf_cell_eval_p1, P)
  ok <- !vapply(recs, is.null, logical(1))
  if (!any(ok)) return(NULL)
  st <- ra_state_totals(recs[ok], RA_TP)
  pieces <- parallel::clusterCall(cl, cf_cell_eval_p2, st)
  pieces
}

# cf_year_jacobian_P ---------------------------------------------------------
# Numerical derivative of the year's pricing conditions, in dollars (each
# condition over its own-price term at the evaluated premiums), in the base premiums at
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
    ag$G[solve_ids] / ag$omega_w[solve_ids]
  }
  t0 <- Sys.time()
  f0 <- f_at(P)
  if (is.null(f0)) return(NULL)
  J <- matrix(NA_real_, length(solve_ids), length(solve_ids), dimnames = list(solve_ids, solve_ids))
  for (j in seq_along(solve_ids)) {
    Ph <- P; Ph[solve_ids[j]] <- Ph[solve_ids[j]] + h
    fh <- f_at(Ph)
    if (!is.null(fh)) J[, j] <- (fh - f0) / h
    cat(".")
  }
  J
}

# solve_cf_year_fixed_point --------------------------------------------------
# The premium equilibrium by best-response iteration on the base premiums,
# P <- P + k f, with f the pricing condition in dollars (over the own-price term
# at the evaluated premiums; under a fixed scale the condition goes to zero as
# enrollment does, and pricing a plan out of the market reads as a solution).
# k is chosen per plan each pass from kappa_grid, which tops out at 1 (the full
# best response); steps are capped at step_cap.
#
# The benchmark is the second-cheapest silver at the evaluated premiums, so the
# profit of a silver plan has a kink at each premium where the benchmark
# changes. Where the condition falls from positive to negative across a kink,
# the kink is the optimum and the condition has no root. Such a plan is one no
# step improves and whose condition changes sign within the smallest step; it
# is moved to the kink by bisection and left out of the convergence measure.
# Returns list(P, pieces, f, kink [logical over solve_ids], iter, kappa_plan,
# converged, max_miss, elapsed); NULL if an evaluation failed.
solve_cf_year_fixed_point <- function(yr, label, solve_ids, P_init,
                                      kappa_grid = c(0.15, 0.35, 0.6, 1), step_cap = 25,
                                      tol_dollars = 1, maxit_P = 40) {
  P <- P_init
  t0 <- Sys.time(); n <- 0L
  elapsed <- function() as.numeric(difftime(Sys.time(), t0, units = "mins"))
  eval_at <- function(P) {
    pieces <- cf_year_evaluate(yr$cl, P)
    if (is.null(pieces) || !all(!vapply(pieces[yr$active], is.null, logical(1)))) return(NULL)
    ag <- cf_year_aggregate(pieces)
    n <<- n + 1L
    list(pieces = pieces, f = ag$G[solve_ids] / ag$omega_w[solve_ids])
  }
  clamp <- function(k, f) pmax(pmin(k * f, step_cap), -step_cap)
  best <- NULL; worse <- 0L; k_plan <- NULL
  for (it in seq_len(maxit_P)) {
    last <- eval_at(P)
    if (is.null(last)) { cf_log(paste("    ", label, "- evaluation failed\n")); return(NULL) }
    cat(".")
    kink <- rep(FALSE, length(solve_ids))
    if (max(abs(last$f)) >= tol_dollars) {
      # The whole vector is tried at each fraction; every plan keeps the one that
      # shrank its own residual most, and 0 leaves it put.
      tried <- lapply(kappa_grid, function(k) {
        Pt <- P; Pt[solve_ids] <- Pt[solve_ids] + clamp(k, last$f)
        tr <- eval_at(Pt)
        if (is.null(tr)) rep(NA_real_, length(solve_ids)) else tr$f
      })
      a <- abs(cbind(last$f, do.call(cbind, tried))); a[is.na(a)] <- Inf
      pick <- max.col(-a, ties.method = "first")
      k_plan <- c(0, kappa_grid)[pick]
      kink <- pick == 1L & abs(last$f) >= tol_dollars & !is.na(tried[[1]]) &
        sign(tried[[1]]) == -sign(last$f)
    }
    m <- max(abs(last$f[!kink]), 0)
    if (is.null(best) || m < best$m) {
      best <- list(P = P, pieces = last$pieces, f = last$f, kink = kink, m = m); worse <- 0L
    } else worse <- worse + 1L
    # A non-contracting map hands its best point to the Jacobian solve instead
    # of grinding to the iteration cap
    if (m < tol_dollars || worse >= 5L) break
    P[solve_ids] <- P[solve_ids] + clamp(k_plan, last$f)
  }
  if (any(best$kink)) {
    kk <- which(best$kink); ids_k <- solve_ids[kk]
    s0 <- sign(best$f[kk])
    lo <- best$P[ids_k]; hi <- lo + clamp(kappa_grid[1], best$f[kk])
    for (b in 1:6) {
      Pt <- best$P; Pt[ids_k] <- (lo + hi) / 2
      tr <- eval_at(Pt)
      if (is.null(tr)) break
      same <- sign(tr$f[kk]) == s0
      lo[same] <- Pt[ids_k][same]; hi[!same] <- Pt[ids_k][!same]
    }
    best$P[ids_k] <- lo
    at_kink <- eval_at(best$P)
    if (!is.null(at_kink)) {
      best$pieces <- at_kink$pieces; best$f <- at_kink$f
      best$kink <- best$kink & abs(best$f) >= tol_dollars
      best$m <- max(abs(best$f[!best$kink]), 0)
    }
  }
  list(P = best$P, pieces = best$pieces, f = best$f, kink = best$kink, iter = n,
       kappa_plan = k_plan, converged = best$m < tol_dollars, max_miss = best$m,
       elapsed = elapsed())
}

# solve_cf_year -------------------------------------------------------------
# yr:       list(y, cl, active [logical per node]) from cf1
# solve_ids: plan ids priced in the solve (others held at their P_init values)
# P_init:   named start, every plan of the year
# J_P:      the year's premium Jacobian from cf_year_jacobian_P
# The best-response iteration runs first. If it stalls off tolerance, the plans
# not at a kink are solved from its best point by Broyden with J_P, and once
# more with the Jacobian recomputed at the stalled point.
# Returns list(P, pieces, converged, max_miss, resid, kink, termcd, iter,
# n_eval, elapsed), max_miss over the plans not at a kink; NULL only if an
# evaluation failed.
solve_cf_year <- function(yr, label, solve_ids, P_init, J_P, tol_dollars = 1) {
  t0 <- Sys.time()
  fp <- solve_cf_year_fixed_point(yr, label, solve_ids, P_init, tol_dollars = tol_dollars,
                                  maxit_P = 25)
  if (is.null(fp)) return(NULL)
  P <- fp$P
  st <- new.env(parent = emptyenv())
  st$n_eval <- fp$iter; st$pieces <- fp$pieces; st$f <- fp$f
  m <- fp$max_miss; termcd <- 1L; iter <- 0L
  if (!fp$converged) {
    ids <- solve_ids[!fp$kink]
    fn <- function(x) {
      Px <- P; Px[ids] <- x
      pieces <- cf_year_evaluate(yr$cl, Px)
      st$n_eval <- st$n_eval + 1L
      if (is.null(pieces) || !all(!vapply(pieces[yr$active], is.null, logical(1))))
        return(rep(NA_real_, length(x)))
      ag <- cf_year_aggregate(pieces)
      st$pieces <- pieces
      st$f <- ag$G[solve_ids] / ag$omega_w[solve_ids]
      cat(".")
      unname(st$f[ids])
    }
    miss <- function(x) max(abs(fn(x)))
    broyden <- function(x, J, maxit) tryCatch(
      nleqslv(x = x, fn = fn, jac = function(x) J[ids, ids], method = "Broyden", global = "hook",
              xscalm = "auto",
              control = list(maxit = maxit, xtol = 1e-6, ftol = 0.2 * tol_dollars, allowSingular = TRUE)),
      error = function(e) { cf_log(paste("    nleqslv error:", conditionMessage(e), "\n")); NULL })
    sol <- broyden(unname(P[ids]), J_P, 150)
    if (is.null(sol)) return(NULL)
    m <- miss(sol$x)
    if (!(is.finite(m) && m < tol_dollars)) {
      cf_log(sprintf("    nleqslv termcd: %d, |f|: %.4g, max miss %.2f $\n",
                     sol$termcd, sqrt(sum(sol$fvec^2)), m), quiet = TRUE)
      cf_log(sprintf("    [%s %s] recomputing jacobian at the stalled point\n", yr$y, label),
             quiet = TRUE)
      P_stall <- P; P_stall[ids] <- sol$x
      J_stall <- cf_year_jacobian_P(yr, ids, P_stall)
      sol2 <- if (is.null(J_stall)) NULL else broyden(sol$x, J_stall, 60)
      if (!is.null(sol2)) {
        m2 <- miss(sol2$x)
        cf_log(sprintf("    retry termcd: %d, |f|: %.4g, max miss %.2f $\n",
                       sol2$termcd, sqrt(sum(sol2$fvec^2)), m2), quiet = TRUE)
        if (is.finite(m2) && m2 < m) { sol <- sol2; m <- m2 }
      }
    }
    # Pieces at the solution (the last evaluation may not be at sol$x)
    m <- miss(sol$x)
    P[ids] <- sol$x
    termcd <- sol$termcd; iter <- sol$iter
  }
  ok <- is.finite(m) && m < tol_dollars
  at_kink <- if (any(fp$kink)) paste(", at a kink:", paste(solve_ids[fp$kink], collapse = " ")) else ""
  # A miss above tolerance is carried, not discarded; max_miss and resid record
  # how far off it is.
  cf_log(sprintf("    [%s %s] %s: max residual %.2f $/member-month%s\n", yr$y, label,
                 if (ok) "solved" else "accepted off tolerance", m, at_kink))
  list(P = P, pieces = st$pieces, converged = ok, max_miss = m,
       resid = st$f, kink = setNames(fp$kink, solve_ids), termcd = termcd, iter = iter,
       n_eval = st$n_eval, elapsed = as.numeric(difftime(Sys.time(), t0, units = "mins")))
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
# yr / solve_ids / J_P as in solve_cf_year. spec_base carries the
# scenario's household conversions (tau / broker_remain / defund) and nothing
# about commissions. beta_f, wedge_f: named by firm. Returns list(P, pieces,
# k, phi, rounds, converged) or NULL.
solve_cf_commissions <- function(yr, label, solve_ids, P_init, J_P, spec_base,
                                 firms, beta_f, wedge_f, k_init = NULL,
                                 tol_phi = 0.05, max_rounds = 20,
                                 damp = 0.5, move_cap = 1.0,
                                 k_min = 0.02, k_max = 10) {
  k <- setNames(rep(1, length(firms)), firms)
  if (!is.null(k_init)) k[names(k_init)[names(k_init) %in% firms]] <-
    k_init[names(k_init) %in% firms]
  P <- P_init
  pieces_sol <- NULL
  phi <- setNames(rep(NA_real_, length(firms)), firms)
  k_prev <- k; phi_prev <- phi
  kink <- setNames(rep(FALSE, length(solve_ids)), solve_ids)
  t0 <- Sys.time()
  for (round in seq_len(max_rounds)) {
    spec <- spec_base
    spec$comm <- "firmscale"
    spec$k_firm <- k
    invisible(parallel::clusterCall(yr$cl, cf_cell_scenario, label, spec))

    # Premium equilibrium at the current schedules
    pieces0 <- cf_year_evaluate(yr$cl, P)
    if (is.null(pieces0) || !all(!vapply(pieces0[yr$active], is.null, logical(1)))) {
      cf_log(paste("    ", label, "- evaluation failed in the commission loop\n")); return(NULL)
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
      if (length(ids_ok) == 0) { cf_log(paste("    ", label, "- no plans above the share floor\n")); return(NULL) }
      solve_ids <- ids_ok
    }
    f0 <- ag0$G[solve_ids] / ag0$omega_w[solve_ids]
    # Premiums within the acceptance band from the last round's solution (plans
    # at a kink aside) need no solve; the condition evaluation follows either way
    if (max(abs(f0[!kink[solve_ids]]), 0, na.rm = TRUE) >= 5) {
      res <- solve_cf_year(yr, paste0(label, " round ", round), solve_ids, P, J_P,
                           tol_dollars = 5)
      if (is.null(res)) { cf_log(paste("    ", label, "- premium solve failed in the commission loop\n")); return(NULL) }
      P[names(res$P)] <- res$P
      kink[names(res$kink)] <- res$kink
    }

    # The commission condition at the solved premiums
    invisible(parallel::clusterCall(yr$cl, cf_cell_set_calib, TRUE))
    pieces_c <- cf_year_evaluate(yr$cl, P)
    invisible(parallel::clusterCall(yr$cl, cf_cell_set_calib, FALSE))
    if (is.null(pieces_c) || !all(!vapply(pieces_c[yr$active], is.null, logical(1)))) {
      cf_log(paste("    ", label, "- condition evaluation failed\n")); return(NULL)
    }
    ag <- cf_year_aggregate(pieces_c)
    ok_f <- firms[firms %in% names(ag$MC) & ag$MC[firms] > 0 & is.finite(ag$MB[firms])]
    phi[] <- NA_real_
    phi[ok_f] <- ag$MB[ok_f] / ag$MC[ok_f] - (1 - beta_f[ok_f]) - wedge_f[ok_f]
    pieces_sol <- pieces_c

    at_floor <- k[ok_f] <= k_min * 1.001 & phi[ok_f] < 0
    active_f <- ok_f[!at_floor]
    worst <- if (length(active_f) > 0) max(abs(phi[active_f]), na.rm = TRUE) else 0
    cat("|")
    cf_log(sprintf("    [%s %s] commissions round %d  max |phi| = %.3f  k %.2f-%.2f  (%d firms, %d at floor)  %.1f min\n", yr$y, label, round, worst, min(k[ok_f]), max(k[ok_f]), length(ok_f), sum(at_floor),
                   as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    if (worst < tol_phi) {
      return(list(P = P, pieces = pieces_sol, k = k, phi = phi,
                  rounds = round, converged = TRUE))
    }
    if (round < max_rounds) {
      # Move k by phi over how much phi moved per unit of k last round.
      slope <- (phi[ok_f] - phi_prev[ok_f]) / (k[ok_f] - k_prev[ok_f])
      step <- ifelse(is.finite(slope) & slope < -1e-6,
                     -phi[ok_f] / slope / k[ok_f],
                     damp * phi[ok_f])
      step <- pmax(pmin(step, move_cap), -move_cap)
      k_prev <- k; phi_prev <- phi
      k[ok_f] <- pmax(pmin(k[ok_f] * (1 + step), k_max), k_min)
    }
  }
  cf_log(paste("    ", label, "- commission loop hit max rounds; keeping the last point\n"))
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
