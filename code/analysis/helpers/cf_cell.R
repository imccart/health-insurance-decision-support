# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Worker-side pieces of the counterfactual: one region-year
##                cell's frozen state (cf_cell_init), its scenario data
##                (cf_cell_scenario), and the two-phase evaluation of the
##                pricing first-order conditions at a candidate premium vector
##                (cf_cell_eval); the commission schedule is set by the
##                scenario. The solver itself lives in helpers/cf_year.R and
##                runs on the master: it
##                calls these on every cell of a year through the cluster, so
##                the statewide transfer pool is exact and each plan is priced
##                once for the year. Relies on the structural helpers the
##                caller has loaded (supply.R, ra.R, choice.R, covariates.R).
##
##                Worker state lives in the environment .cf: .cf$cell (the
##                frozen cell), .cf$scen (the current scenario), .cf$ev (the
##                last phase-1 evaluation, consumed by phase 2).

.cf <- new.env(parent = emptyenv())

# Commission utility terms, zeroed for the non-commission welfare metric
COMM_TERMS <- c("commission_broker")

# cf_cell_init --------------------------------------------------------------
# Builds the cell's frozen state: the structural choice data at the sampled
# households, plan attributes, the observed premiums and commissions, the
# insurer structure, the benchmark plan, the claims-equation plan
# characteristics, and the commission derivatives at the observed point.
# Returns a small summary for the master (NULL if the cell cannot be used).
cf_cell_init <- function(r, y, seed, sample_frac, hhs_raw,
                         plan_choice, supply_results, coefs, commission_lookup,
                         rs_coefs, claims_coefs, reins_df, STRUCTURAL_SPEC,
                         admin_lookup, beta_admin) {
  # admin_lookup: the insurer's non-commission administrative cost per member
  # (MLR), keyed prefix_year; beta_admin: the administrative saving per
  # commission dollar on a broker enrollee by insurer-year (s4), keyed
  # prefix_year. Both enter marginal cost.
  .cf$cell <- NULL; .cf$scen <- NULL; .cf$ev <- NULL

  sr_cell <- supply_results %>%
    filter(region == r, year == y, !is.na(posted_premium), !is.na(region_factor))
  if (nrow(sr_cell) == 0) { cat("No supply results for cell", r, y, "\n"); return(NULL) }
  plans_cell <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans_cell) == 0) { cat("No plans for cell", r, y, "\n"); return(NULL) }
  if (is.null(hhs_raw) || nrow(hhs_raw) == 0) { cat("No HH data for cell", r, y, "\n"); return(NULL) }
  set.seed(seed)

  # Commissions on the plans before the choice data are built. Percentage
  # schedules (rate x premium) are dollarized at the observed premiums; the
  # pct/rho flags carry their direct term in the pricing condition.
  comm_yr <- commission_lookup %>% filter(year == !!y) %>% select(-year)
  pct_prefixes <- comm_yr$insurer_prefix[!is.na(comm_yr$is_pct) & comm_yr$is_pct]
  plans_cell <- plans_cell %>%
    mutate(insurer_prefix = sub("_.*", "", plan_id)) %>%
    left_join(comm_yr, by = "insurer_prefix") %>%
    mutate(comm_pmpm = case_when(is.na(rate) ~ 0, is_pct ~ rate * premium, TRUE ~ rate)) %>%
    select(-insurer_prefix, -rate, -is_pct)

  build_result <- build_structural(plans_cell, hhs_raw, sample_frac, spec = STRUCTURAL_SPEC)
  if (is.null(build_result)) { cat("Empty cell data for", r, y, "\n"); return(NULL) }
  cell_data_base <- build_result$cell_data
  plan_attrs     <- build_result$plan_attrs
  rm(build_result)
  if (!"adj_subsidy" %in% names(cell_data_base))
    cell_data_base$adj_subsidy <- ifelse(is.na(cell_data_base$subsidy), 0, cell_data_base$subsidy)

  # Members in the cell (one Uninsured row per household; hh_weight = household size)
  N_cell <- sum(cell_data_base$hh_weight[cell_data_base$plan_id == "Uninsured"], na.rm = TRUE)

  plan_ids_cell <- intersect(sort(plan_attrs$plan_id), sr_cell$plan_id)
  if (length(plan_ids_cell) < 3) { cat("Too few plans for cell", r, y, "\n"); return(NULL) }

  pa <- plan_attrs[match(plan_ids_cell, plan_attrs$plan_id), ]
  p_obs    <- setNames(pa$premium_posted, pa$plan_id)
  plan_avs <- setNames(pa$av, pa$plan_id)
  comm_obs <- if ("comm_pmpm" %in% names(pa)) setNames(pa$comm_pmpm, pa$plan_id) else
    setNames(rep(0, length(plan_ids_cell)), plan_ids_cell)
  g_cell   <- setNames(sr_cell$region_factor[match(plan_ids_cell, sr_cell$plan_id)], plan_ids_cell)
  share_obs <- setNames(sr_cell$share[match(plan_ids_cell, sr_cell$plan_id)], plan_ids_cell)

  plan_prefix <- sub("_.*", "", plan_ids_cell)
  own_mat <- outer(plan_prefix, plan_prefix, "==") * 1L
  dimnames(own_mat) <- list(plan_ids_cell, plan_ids_cell)
  pct_plan <- plan_prefix %in% pct_prefixes
  rho_obs  <- ifelse(pct_plan & p_obs > 0, comm_obs / p_obs, 0)

  # 2nd cheapest Silver by observed posted premium (the ACA benchmark at the
  # observed point; the counterfactual re-picks it at the candidate premiums)
  silver <- plan_attrs[plan_attrs$metal == "Silver", ]
  silver <- silver[order(silver$premium_posted), ]
  benchmark_plan <- if (nrow(silver) == 0) NA_character_ else if (nrow(silver) == 1) silver$plan_id[1] else silver$plan_id[2]

  plan_chars_cell <- tibble(
    plan_id  = plan_ids_cell,
    Silver   = as.integer(pa$metal == "Silver"),
    Gold     = as.integer(pa$metal == "Gold"),
    Platinum = as.integer(pa$metal == "Platinum"),
    AV       = unname(pa$av),
    HMO      = pa$hmo,
    trend    = y - 2014L,
    !!!setNames(lapply(CLAIMS_REGION_TERMS, function(rc)
      as.numeric(ifelse(is.na(plans_cell[[rc]][match(plan_ids_cell, gsub("SIL(94|73|87)", "SIL", plans_cell$plan_id))]), 0,
                        plans_cell[[rc]][match(plan_ids_cell, gsub("SIL(94|73|87)", "SIL", plans_cell$plan_id))]))),
      CLAIMS_REGION_TERMS),
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

  rf_year <- reins_df %>% filter(year == y)
  reins_vec <- sapply(plan_ids_cell, function(pn) {
    rf <- rf_year$reins_factor[rf_year$plan_id == pn]
    if (length(rf) == 0) return(0)
    mean(rf, na.rm = TRUE)
  })
  reins_vec[is.na(reins_vec)] <- 0

  mean_comm_pmpm <- mean(sr_cell$commission_pmpm[sr_cell$commission_pmpm > 0], na.rm = TRUE)
  if (is.na(mean_comm_pmpm)) mean_comm_pmpm <- 0

  admin_vec <- setNames(admin_lookup[paste(plan_prefix, y, sep = "_")], plan_ids_cell)
  admin_vec[is.na(admin_vec)] <- 0
  beta_vec <- setNames(beta_admin[paste(plan_prefix, y, sep = "_")], plan_ids_cell)
  beta_vec[is.na(beta_vec)] <- mean(beta_admin)

  .cf$cell <- list(
    r = r, y = y, N = N_cell, plan_ids = plan_ids_cell, prefix = plan_prefix,
    admin = admin_vec, beta = unname(beta_vec),
    cell_data_base = cell_data_base, plan_attrs = plan_attrs, plans_cell = plans_cell,
    coefs = coefs, lambda = setNames(coefs$estimate, coefs$term)[["lambda"]],
    spec = STRUCTURAL_SPEC, rs_coefs = rs_coefs, claims_coefs = claims_coefs,
    p_obs = p_obs, plan_avs = plan_avs, comm_obs = comm_obs, g = g_cell, share_obs = share_obs,
    own_mat = own_mat, pct = pct_plan, rho = rho_obs, benchmark_plan = benchmark_plan,
    plan_chars = plan_chars_cell, reins_vec = reins_vec, gcf = ra_gcf(r, y),
    mean_comm_pmpm = mean_comm_pmpm
  )
  list(r = r, y = y, N = N_cell, plan_ids = plan_ids_cell, prefix = plan_prefix,
       p_obs = p_obs, comm_obs = comm_obs, g = g_cell, share_obs = share_obs,
       pct = setNames(pct_plan, plan_ids_cell), rho = setNames(rho_obs, plan_ids_cell))
}

# current_benchmark -------------------------------------------------------
# The ACA benchmark at a candidate posted-premium vector: the 2nd cheapest
# Silver plan in the cell at those premiums (the cheapest if there is one).
current_benchmark <- function(p_vec) {
  cl <- .cf$cell
  sil <- cl$plan_attrs$plan_id[cl$plan_attrs$metal == "Silver"]
  sil <- sil[sil %in% names(p_vec)]
  if (length(sil) == 0) return(NA_character_)
  sil <- sil[order(p_vec[sil])]
  if (length(sil) == 1) sil[1] else sil[2]
}

# update_premiums ---------------------------------------------------------
# Re-level the demand `premium` column for a candidate posted-premium vector,
# reproducing the builder's NET premium (helpers/supply.R): posted -> age-40-
# normalized HH premium -> OOP after subsidy and penalty offset -> per-member,
# per-$100. The APTC moves with the benchmark premium, the 2nd-cheapest silver
# re-picked at the candidate premiums: subsidy = max(0, premiumSLC(p) - zeta),
# anchored to the data-build value at the observed benchmark so the observed
# point reproduces it exactly. The benchmark in use is kept in .cf$bench_cur
# for the share derivatives.
update_premiums <- function(dt, p_vec) {
  cl <- .cf$cell
  rf_i <- dt$rating_factor / RATING_FACTOR_AGE40
  bench_cur <- current_benchmark(p_vec)
  .cf$bench_cur <- bench_cur
  if (!is.na(bench_cur) && !is.na(cl$benchmark_plan) &&
      !isTRUE(getOption("cf.fixed.subsidy", FALSE))) {
    d_bench       <- p_vec[[bench_cur]] - cl$p_obs[[cl$benchmark_plan]]
    premiumSLC_cf <- dt$premiumSLC + rf_i * d_bench
    sub_endog     <- pmax(0, premiumSLC_cf - dt$SLC_contribution)
    dt[, subsidy_cf := fifelse(subsidized == 1L, sub_endog, adj_subsidy)]
    dt[, sub_interior := as.numeric(subsidized == 1L & (premiumSLC_cf - SLC_contribution) > 0)]
  } else {
    dt[, subsidy_cf := adj_subsidy]
    dt[, sub_interior := 0]
  }
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
  recompute_prem_interactions(dt, cl$spec)
}

# build_scenario_data ------------------------------------------------------
# tau: share of broker households converted to navigators (highest p_nav
#   first); broker_remain = TRUE keeps the non-switched brokers as brokers
#   (the navigator-expansion scenarios) instead of converting them to Unassisted.
# defund: share of navigator households converted to brokers (lowest p_nav
#   first). Runs before the commission write so new brokers pick up the
#   scenario schedule.
build_scenario_data <- function(comm_sc, tau = NULL, broker_remain = FALSE, defund = NULL) {
  cl <- .cf$cell
  cd <- as.data.table(copy(cl$cell_data_base))

  if (!is.null(defund) && "any_agent" %in% names(cd)) {
    nav_hh <- cd[plan_id == "Uninsured" & assisted == 1L & (is.na(any_agent) | any_agent != 1L),
                 .(household_number, p_nav)]
    if (nrow(nav_hh) == 0)
      nav_hh <- unique(cd[assisted == 1L & (is.na(any_agent) | any_agent != 1L),
                          .(household_number, p_nav)], by = "household_number")
    if (nrow(nav_hh) > 0) {
      nav_hh <- nav_hh[order(p_nav)]
      switch_ids <- nav_hh$household_number[seq_len(ceiling(defund * nrow(nav_hh)))]
      cd[household_number %in% switch_ids, any_agent := 1L]
    }
  }

  for (pn in cl$plan_ids) {
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
    if (nrow(agent_hh) == 0)
      agent_hh <- unique(cd[any_agent == 1, .(household_number, p_nav)], by = "household_number")
    if (nrow(agent_hh) > 0) {
      agent_hh <- agent_hh[order(-p_nav)]
      switch_ids <- agent_hh$household_number[seq_len(ceiling(tau * nrow(agent_hh)))]
      cd[household_number %in% switch_ids, `:=`(commission_broker = 0, any_agent = 0L,
                                                channel_detail = "Navigator")]
      if (tau < 1 && !broker_remain) {
        remain_ids <- setdiff(agent_hh$household_number, switch_ids)
        cd[household_number %in% remain_ids, `:=`(assisted = 0L, commission_broker = 0,
                                                  any_agent = 0L, channel_detail = "Unassisted")]
      }
    }
  }

  # Steering terms, same definition as build_structural: navigator (non-broker)
  # and broker each carry their own metal terms
  if ("any_agent" %in% names(cd)) {
    cd[, nonbroker := assisted * fifelse(any_agent == 1L, 0L, 1L, na = 1L)]
    cd[, broker    := assisted * fifelse(any_agent == 1L, 1L, 0L, na = 0L)]
  } else {
    cd[, nonbroker := assisted]
    cd[, broker    := 0L]
  }
  cd[, `:=`(assisted_av = nonbroker * av, broker_av = broker * av,
            assisted_premium = nonbroker * premium, broker_premium = broker * premium)]
  cd
}

# cf_cell_scenario ----------------------------------------------------------
# Installs a scenario on the worker: its cell data and commission basis.
#   spec$comm   "observed", "zero", "uniform" (cell mean of observed positive
#               commissions), "scale" (observed x spec$sc), "flatbar" (a flat
#               fee per insurer at spec$levels, named by prefix), "aligned"
#               (proportional to the plan's mean non-commission utility, holding
#               the cell's commission budget)
#   spec$tau, spec$broker_remain, spec$defund: household conversions
# Returns the per-plan commission vector (for the master's records).
cf_cell_scenario <- function(label, spec) {
  cl <- .cf$cell
  .cf$scen <- NULL; .cf$ev <- NULL
  if (is.null(cl)) return(NULL)
  pn <- cl$plan_ids
  comm_sc <- switch(spec$comm,
    observed = cl$comm_obs,
    zero     = setNames(rep(0, length(pn)), pn),
    uniform  = setNames(rep(cl$mean_comm_pmpm, length(pn)), pn),
    scale    = setNames(cl$comm_obs * spec$sc, pn),
    flatbar  = {
      lv <- spec$levels[cl$prefix]
      lv[is.na(lv)] <- 0
      setNames(unname(lv), pn)
    },
    aligned  = {
      cd_nc <- as.data.table(copy(cl$cell_data_base))
      for (cn in intersect(COMM_TERMS, names(cd_nc))) cd_nc[[cn]] <- 0
      cd_nc[, V_nc := compute_utility(cd_nc, cl$coefs)$V]
      plan_val <- cd_nc[plan_id != "Uninsured", .(val = mean(V_nc, na.rm = TRUE)), by = plan_id]
      val_vec <- setNames(plan_val$val, plan_val$plan_id)[pn]
      val_vec[!is.finite(val_vec)] <- min(val_vec[is.finite(val_vec)], na.rm = TRUE)
      obs_share <- cl$share_obs; obs_share[!is.finite(obs_share)] <- 0
      w_val  <- val_vec - min(val_vec)
      budget <- sum(cl$comm_obs * obs_share, na.rm = TRUE)
      denom  <- sum(w_val * obs_share, na.rm = TRUE)
      setNames(as.numeric(if (denom > 0) w_val * (budget / denom) else cl$comm_obs), pn)
    },
    stop("unknown commission basis ", spec$comm))
  cd <- build_scenario_data(comm_sc, tau = spec$tau, broker_remain = isTRUE(spec$broker_remain),
                            defund = spec$defund)
  .cf$scen <- list(label = label, dt_base = cd, comm = comm_sc,
                   calib = isTRUE(spec$calib))
  comm_sc
}

# cf_cell_eval --------------------------------------------------------------
# Phase 1: at the cell premiums implied by the plan-year base premiums P (p_c =
# P g_c, observed for plans not in P), rebuild the choice data and compute
# shares, elasticities, demographic shares, predicted risk scores, the broker
# elasticities, and (calibration) the commission derivatives. Returns the cell
# record for ra_state_totals(); the rest stays in .cf$ev.
cf_cell_eval_p1 <- function(P) {
  cl <- .cf$cell; sc <- .cf$scen
  if (is.null(cl) || is.null(sc)) return(NULL)
  pn <- cl$plan_ids
  p_vec <- cl$p_obs
  inP <- pn %in% names(P)
  p_vec[inP] <- P[pn[inP]] * cl$g[inP]

  eta_cur <- sc$comm
  dt <- update_premiums(copy(sc$dt_base), p_vec)

  util <- compute_utility(dt, cl$coefs)
  bench <- .cf$bench_cur
  se <- tryCatch(
    compute_shares_and_elasticities(dt, util$V, cl$lambda, bench, cl$plan_attrs,
                                     cl$coefs, spec = cl$spec, V_base = util$V_base),
    error = function(e) NULL)
  if (is.null(se)) { .cf$ev <- NULL; return(NULL) }
  demo <- tryCatch(compute_demographic_shares(dt, util$V, cl$lambda, V_base = util$V_base),
                   error = function(e) NULL)
  if (is.null(demo)) { .cf$ev <- NULL; return(NULL) }
  br <- tryCatch(
    compute_broker_shares_and_elasticities(dt, util$V, cl$lambda, bench, cl$plan_attrs,
                                            cl$coefs, spec = cl$spec, V_base = util$V_base),
    error = function(e) NULL)
  ck <- if (sc$calib) tryCatch(
    compute_commission_derivatives(dt, util$V, cl$lambda, cl$coefs, V_base = util$V_base),
    error = function(e) NULL) else NULL
  rs <- predict_risk_scores(cl$rs_coefs, cl$plan_chars, demo)
  rs_vec <- setNames(rs$predicted_risk_score, rs$plan_id)[pn]

  .cf$ev <- list(p = p_vec, eta = eta_cur, dt = dt, V = util$V, V_base = util$V_base,
                 shares = se$shares[pn], elast = se$elast_mat[pn, pn], demo = demo, rs = rs_vec,
                 qB_plan = if (!is.null(br)) unname(br$broker_shares[pn]) else rep(0, length(pn)),
                 broker_elast = if (!is.null(br)) br$broker_elast_mat[pn, pn] else NULL,
                 comm_D = if (!is.null(ck)) ck$D[pn, pn] else NULL,
                 comm_qB = if (!is.null(ck)) ck$qB[pn] else NULL)
  list(region = cl$r, year = cl$y, N = cl$N, shares = .cf$ev$shares, rs = rs_vec,
       av = cl$plan_avs, arf = setNames(demo$arf, demo$plan_id), gcf = cl$gcf, premium = p_vec)
}

# Phase 2: with the year's statewide sums (totals, own) from the master, the
# transfers, marginal cost, the RA derivative, the per-plan pricing FOC residual
# (share units per member; exogenous form, with the pct direct term reported
# separately), and the per-insurer commission pieces MB_f, MC_f, qB_f in the
# cell's share units.
cf_cell_eval_p2 <- function(totals, own) {
  cl <- .cf$cell; sc <- .cf$scen; ev <- .cf$ev
  if (is.null(cl) || is.null(sc) || is.null(ev)) return(NULL)
  pn <- cl$plan_ids; J <- length(pn)
  ra_env <- ra_env_for_cell(cl$r, cl$y, cl$N, ev$demo, totals, own)
  mc_res <- compute_mc(cl$rs_coefs, cl$claims_coefs, cl$plan_chars, ev$demo, ev$shares,
                       ra_env, cl$plan_avs, cl$reins_vec)
  # Marginal cost: claims net of reinsurance and the transfer, plus the insurer's
  # administrative cost per member. A commission dollar on a broker enrollee
  # costs (1 - beta) net of the administrative work the agent takes over.
  mc <- mc_res$mc[pn] + cl$admin
  cs <- 1 - cl$beta                              # (1 - beta) by plan (its insurer-year)
  ra_foc <- compute_ra_foc(ev$rs, ev$shares, cl$plan_avs, ra_env, ev$elast, cl$own_mat)

  Omega <- -cl$own_mat * t(ev$elast)
  resid <- ev$shares + ra_foc - as.vector(Omega %*% (ev$p - mc))
  if (!is.null(ev$broker_elast)) {
    Omega_B <- -cl$own_mat * t(ev$broker_elast)
    resid <- resid + as.vector(Omega_B %*% (cs * ev$eta))
  }

  # Commission pieces per insurer: MB_f = margin x d qB / d k + RA response,
  # MC_f = outlay, qB_f = broker enrollment; weights w = the plan's commission
  # under the scenario's schedule. Pct direct term rho_j qB_j per plan.
  MB <- MC <- qB <- setNames(numeric(0), character(0))
  direct <- numeric(J)
  if (!is.null(ev$comm_D)) {
    ra_eta <- compute_ra_foc(ev$rs, ev$shares, cl$plan_avs, ra_env, ev$comm_D, cl$own_mat)
    margin <- ev$p - mc - cs * ev$eta
    w_basis <- sc$comm
    firms <- unique(cl$prefix[sc$comm > 0])
    for (f in firms) {
      ii <- which(cl$prefix == f)
      w_f <- numeric(J); w_f[ii] <- w_basis[ii]
      dq <- as.numeric(ev$comm_D %*% w_f)
      MB[f] <- sum(margin[ii] * dq[ii]) + sum(w_basis[ii] * ra_eta[ii])
      MC[f] <- sum(ev$comm_qB[ii] * w_basis[ii])
      qB[f] <- sum(ev$comm_qB[ii])
      direct[ii] <- ifelse(cl$pct[ii], cs[ii] * cl$rho[ii] * ev$comm_qB[ii], 0)
    }
  }
  # Insurer variable profit and agent enrollment in the cell (monthly, sample
  # units): margin on all members less the net commission outlay on agent members
  qBp <- if (!is.null(ev$qB_plan)) ev$qB_plan else rep(0, J)
  prof_plan <- cl$N * ((ev$p - mc) * ev$shares - cs * ev$eta * qBp)
  firm_profit <- tapply(prof_plan, cl$prefix, sum)
  firm_qB <- tapply(cl$N * qBp, cl$prefix, sum)

  list(plan_ids = pn, N = cl$N, g = cl$g, resid = setNames(resid, pn), direct = setNames(direct, pn),
       MB = MB, MC = MC, qB = qB, shares = ev$shares, mc = mc, claims = mc_res$predicted_claims[pn],
       firm_profit = firm_profit, firm_qB = firm_qB,
       eta = ev$eta, p = ev$p, omega_own = setNames(diag(Omega), pn))
}
