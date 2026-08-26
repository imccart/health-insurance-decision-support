# Meta --------------------------------------------------------------------
## Description:   Shared counterfactual welfare scorer, used by cf2 (point estimates)
##                and cf3 (bootstrap draws). Given a cell's solved premiums and
##                commissions per scenario (cf_cell: region, year, scenario, plan_id,
##                premium_cf, commission_pmpm, tau), it reloads the cached structural
##                choice data, rebuilds each scenario, re-levels premiums, applies the
##                spending schedule, and returns cell-level welfare, producer surplus,
##                and government subsidy (and writes per-household welfare to hh_dir).
##                Reads CELL_DIR, supply_results,
##                coefs, lambda, STRUCTURAL_SPEC, COMM_TERMS, CS_TABLE, and
##                SPENDING_SCHEDULE from the caller's environment. The scenario builder
##                and consumer-surplus function are frozen copies of helpers/cf_cell.R.

score_cf_cell <- function(r, y, cf_cell, hh_dir, coefs, lambda) {
  fp <- file.path(CELL_DIR, sprintf("cell_%s_%s_data.csv", r, y))
  if (!file.exists(fp)) return(NULL)
  cell_data_base <- as.data.frame(fread(fp))
  sr_cell <- supply_results[supply_results$region == r & supply_results$year == y, ]
  if (nrow(sr_cell) == 0 || nrow(cf_cell) == 0) return(NULL)

  inside <- cell_data_base[cell_data_base$plan_id != "Uninsured", ]
  pa <- inside[!duplicated(inside$plan_id), ]
  plan_ids_cell <- sort(intersect(unique(inside$plan_id), sr_cell$plan_id))
  if (length(plan_ids_cell) < 3) return(NULL)
  pa <- pa[match(plan_ids_cell, pa$plan_id), ]
  p_obs    <- setNames(pa$premium_posted, plan_ids_cell)
  sil <- pa[pa$silver == 1, ]; sil <- sil[order(sil$premium_posted), ]
  benchmark_plan <- if (nrow(sil) < 2) sil$plan_id[1] else sil$plan_id[2]

  # --- frozen closures (copies of helpers/cf_cell.R) ---
  update_premiums <- function(dt, p_vec) {
    rf_i <- dt$rating_factor / RATING_FACTOR_AGE40
    if (!is.na(benchmark_plan) && benchmark_plan %in% names(p_vec)) {
      d_bench       <- p_vec[[benchmark_plan]] - p_obs[[benchmark_plan]]
      premiumSLC_cf <- dt$premiumSLC + rf_i * d_bench
      sub_endog     <- pmax(0, premiumSLC_cf - dt$SLC_contribution)
      dt[, subsidy_cf := fifelse(subsidized == 1L, sub_endog, adj_subsidy)]
      dt[, sub_interior := as.numeric(subsidized == 1L & (premiumSLC_cf - SLC_contribution) > 0)]
    } else { dt[, subsidy_cf := adj_subsidy]; dt[, sub_interior := 0] }
    dt[, kink_m := 1]
    for (pn in names(p_vec)) {
      idx <- which(dt$plan_id == pn); if (length(idx) == 0) next
      premium_hh <- (p_vec[pn] / RATING_FACTOR_AGE40) * dt$rating_factor[idx]
      gap <- premium_hh - dt$subsidy_cf[idx]
      oop <- pmax(gap, 0) - dt$penalty[idx] / 12
      set(dt, i = idx, j = "premium", value = oop / dt$hh_size[idx] / 100)
      set(dt, i = idx, j = "kink_m", value = as.numeric(gap > 0))
    }
    recompute_prem_interactions(dt, STRUCTURAL_SPEC)
  }

  build_scenario_data <- function(cell_data_base, comm_sc, tau = NULL,
                                  broker_remain = FALSE, defund = NULL) {
    cd <- as.data.table(copy(cell_data_base))
    if (!is.null(defund) && "any_agent" %in% names(cd)) {
      nav_hh <- cd[plan_id == "Uninsured" & assisted == 1L &
                     (is.na(any_agent) | any_agent != 1L), .(household_number, p_nav)]
      if (nrow(nav_hh) == 0)
        nav_hh <- unique(cd[assisted == 1L & (is.na(any_agent) | any_agent != 1L),
                            .(household_number, p_nav)], by = "household_number")
      if (nrow(nav_hh) > 0) {
        nav_hh <- nav_hh[order(p_nav)]
        switch_ids <- nav_hh$household_number[seq_len(ceiling(defund * nrow(nav_hh)))]
        cd[household_number %in% switch_ids, any_agent := 1L]
      }
    }
    for (pn in plan_ids_cell) {
      idx <- cd$plan_id == pn
      if (sum(idx) > 0 && "commission_broker" %in% names(cd)) {
        if ("any_agent" %in% names(cd))
          cd$commission_broker[idx] <- comm_sc[pn] * fifelse(cd$any_agent[idx] == 1L, cd$assisted[idx], 0L)
        else cd$commission_broker[idx] <- comm_sc[pn] * cd$assisted[idx]
      }
    }
    if (!is.null(tau) && "any_agent" %in% names(cd)) {
      agent_hh <- cd[plan_id == "Uninsured" & any_agent == 1, .(household_number, p_nav)]
      if (nrow(agent_hh) == 0) { agent_hh <- cd[any_agent == 1, .(household_number, p_nav)]; agent_hh <- unique(agent_hh, by = "household_number") }
      if (nrow(agent_hh) > 0) {
        agent_hh <- agent_hh[order(-p_nav)]
        n_switch <- ceiling(tau * nrow(agent_hh))
        switch_ids <- agent_hh$household_number[seq_len(n_switch)]
        cd[household_number %in% switch_ids, `:=`(commission_broker = 0, any_agent = 0L, channel_detail = "Navigator")]
        if (tau < 1 && !broker_remain) {
          remain_ids <- setdiff(agent_hh$household_number, switch_ids)
          cd[household_number %in% remain_ids, `:=`(assisted = 0L, commission_broker = 0, any_agent = 0L, channel_detail = "Unassisted")]
        }
      }
    }
    if ("any_agent" %in% names(cd)) {
      cd[, nonbroker := assisted * fifelse(any_agent == 1L, 0L, 1L, na = 1L)]
      cd[, broker    := assisted * fifelse(any_agent == 1L, 1L, 0L, na = 0L)]
    } else { cd[, nonbroker := assisted]; cd[, broker := 0L] }
    cd[, `:=`(assisted_av = nonbroker*av, broker_av = broker*av,
              assisted_premium = nonbroker*premium, broker_premium = broker*premium)]
    cd
  }

  compute_consumer_surplus <- function(cell_data, coefs_cell, welfare_drop = character()) {
    lambda_cs <- setNames(coefs_cell$estimate, coefs_cell$term)[["lambda"]]
    if (length(welfare_drop) > 0) { cell_data <- as.data.table(copy(cell_data)); for (cn in intersect(welfare_drop, names(cell_data))) cell_data[[cn]] <- 0 }
    # Two-part nested logit: the enrollment log-sum uses the base inclusive value
    # I_base (assistance terms excluded); the within-nest gain from assistance,
    # lambda (I_full - I_base), is added back so the inclusive-value surplus is
    #   CS = (1/alpha) [ lambda (I_full - I_base) + log(exp(V_0) + exp(lambda I_base)) ],
    # which is the ordinary log-sum when I_full = I_base.
    util <- compute_utility(cell_data, coefs_cell)
    dt <- as.data.table(cell_data); dt[, V := util$V]; dt[, V_base := util$V_base]
    V0_by_hh <- dt[plan_id == "Uninsured", .(V_0 = V[1]), by = household_number]
    ins_dt <- dt[plan_id != "Uninsured"]
    lse <- function(v) { m <- max(v); m + log(sum(exp(v - m))) }
    hh_iv <- ins_dt[, .(I_full = lse(V / lambda_cs), I_base = lse(V_base / lambda_cs),
                        hh_weight = first(hh_weight), hh_size = first(hh_size)), by = household_number]
    # Utils-to-dollars denominator: each household's own base price sensitivity
    # (per raw household dollar; the channel premium slopes excluded). Fixed across
    # scenarios, since it does not depend on premiums or on the household's channel.
    ins_dt[, alpha_base := compute_alpha_i(ins_dt, coefs_cell, STRUCTURAL_SPEC, base = TRUE)]
    hh_a  <- ins_dt[, .(alpha_base = first(alpha_base)), by = household_number]
    hh_cs <- merge(hh_iv, V0_by_hh, by = "household_number", all.x = TRUE); hh_cs[is.na(V_0), V_0 := 0]
    hh_cs <- merge(hh_cs, hh_a, by = "household_number", all.x = TRUE)
    hh_cs[, log_D_lam := lambda_cs * I_base]
    hh_cs[, mx := pmax(V_0, log_D_lam)]
    # per member per year (matches the objective): / hh_size, x 12 (premium is monthly)
    hh_cs[, cs := (1 / abs(alpha_base)) * (lambda_cs * (I_full - I_base) +
                                           mx + log(exp(V_0 - mx) + exp(pmin(log_D_lam - mx, 500)))) / hh_size * 12]
    sum(hh_cs$hh_weight * hh_cs$cs) / sum(hh_cs$hh_weight)
  }

  # Scenario flags recovered from the label: endog_tau -> tau, brokers remain;
  # defund_<f> -> reverse conversion at fraction f; zero_tau -> tau. Commissions
  # come from the persisted commission_pmpm.
  scen_labels <- unique(cf_cell$scenario)

  per <- lapply(scen_labels, function(lab) {
    rows <- cf_cell[scenario == lab]
    if (nrow(rows) == 0) return(NULL)
    comm <- setNames(rows$commission_pmpm, rows$plan_id)[plan_ids_cell]
    comm[is.na(comm)] <- 0
    names(comm) <- plan_ids_cell
    tt <- rows$tau[1]; if (is.na(tt)) tt <- NULL
    df <- if (grepl("^defund_", lab)) as.numeric(sub("^defund_", "", lab)) else NULL
    cd <- build_scenario_data(cell_data_base, comm, tau = tt,
                              broker_remain = grepl("^endog_tau", lab), defund = df)
    p_vec <- setNames(rows$premium_cf, rows$plan_id)[plan_ids_cell]
    if (any(is.na(p_vec))) return(NULL)
    names(p_vec) <- plan_ids_cell
    dt <- update_premiums(as.data.table(copy(cd)), p_vec)
    espend <- household_spending(dt, SPENDING_SCHEDULE)   # per-row expected spending

    # Producer surplus (insurer margin net of commissions) and government cost, per
    # member per year. Government cost = premium subsidies (APTC paid, capped at
    # the premium) + cost-sharing reductions (government share of predicted claims
    # for CSR enrollees, CSR_GOV_SHARE) + uncompensated care for the uninsured
    # (UC_PER_UNINSURED per uninsured member, scaled below by the uninsured pool's
    # predicted risk score relative to the baseline scenario) - mandate penalty
    # revenue from the uninsured.
    psg <- tryCatch({
      mc_vec <- setNames(rows$mc, rows$plan_id)[plan_ids_cell]
      cl_vec <- if ("claims" %in% names(rows)) setNames(rows$claims, rows$plan_id)[plan_ids_cell] else setNames(rep(NA_real_, length(plan_ids_cell)), plan_ids_cell)
      pr <- choice_probs(dt, coefs, lambda)
      dd <- as.data.table(copy(dt)); dd[, p_ch := pr]
      M  <- dd[, .(w = first(hh_weight)), by = household_number][, sum(w)]
      ins <- dd[plan_id != "Uninsured"]
      ins[, `:=`(mem   = p_ch * hh_weight,
                 mem_b = p_ch * hh_weight * fifelse(is.na(broker), 0, broker))]
      enr <- ins[, .(mem = sum(mem), mem_b = sum(mem_b)), by = plan_id]
      enr[, `:=`(p = p_vec[plan_id], mc = mc_vec[plan_id], eta = comm[plan_id])]
      enr[is.na(eta), eta := 0]
      ps_month <- enr[, sum((p - mc) * mem - eta * mem_b, na.rm = TRUE)]
      ins[, sub_paid := pmin((p_vec[plan_id] / RATING_FACTOR_AGE40) * rating_factor, subsidy_cf)]
      ins[is.na(sub_paid), sub_paid := 0]
      gov_month <- ins[, sum(p_ch * sub_paid, na.rm = TRUE)]
      # CSR: silver rows of CSR-eligible households, government share by variant
      ins[, csr_var := fifelse(csr94_elig == 1L, "94",
                        fifelse(csr87_elig == 1L, "87",
                         fifelse(FPL > 2 & FPL <= 2.5 & fcoalesce(as.numeric(subsidized_members), 0) > 0, "73", "")))]
      ins[, csr_share := fifelse(silver == 1L & csr_var != "", CSR_GOV_SHARE[csr_var], 0)]
      ins[is.na(csr_share), csr_share := 0]
      csr_month <- ins[, sum(mem * csr_share * cl_vec[plan_id], na.rm = TRUE)]
      # Uninsured: penalty revenue (annual household penalty) and uncompensated care
      out <- dd[plan_id == "Uninsured"]
      pen_month <- out[, sum(p_ch * fcoalesce(penalty, 0) / 12, na.rm = TRUE)]
      unins_mem <- out[, sum(p_ch * hh_weight)]
      uc_rate <- if (exists("UC_PER_UNINSURED") && as.character(y) %in% names(UC_PER_UNINSURED)) UC_PER_UNINSURED[[as.character(y)]] else NA_real_
      uc_month <- unins_mem * uc_rate / 12
      # Predicted risk score of the uninsured pool (risk-score equation at the
      # pool's demographic shares, bronze base), for the cross-scenario scaling
      rsg <- if (exists("RS_COEFS_GOV")) RS_COEFS_GOV else {
        rc <- read.csv(file.path(TEMP_DIR, "ra_rs_coefs_gmm.csv"), stringsAsFactors = FALSE); setNames(rc$estimate, rc$term) }
      g <- function(n) if (n %in% names(rsg)) rsg[[n]] else 0
      pool <- out[, .(s1834 = sum(p_ch * hh_weight * perc_18to34) / sum(p_ch * hh_weight),
                      s3554 = sum(p_ch * hh_weight * perc_35to54) / sum(p_ch * hh_weight),
                      shisp = sum(p_ch * hh_weight * perc_hispanic) / sum(p_ch * hh_weight))]
      rs_unins <- exp(g("(Intercept)") + g("share_18to34") * pool$s1834 + g("share_35to54") * pool$s3554 + g("share_hispanic") * pool$shisp)
      list(ps = ps_month / M * 12, gov = gov_month / M * 12, csr = csr_month / M * 12,
           pen = pen_month / M * 12, uc = uc_month / M * 12, rs_unins = rs_unins)
    }, error = function(e) list(ps = NA_real_, gov = NA_real_, csr = NA_real_, pen = NA_real_, uc = NA_real_, rs_unins = NA_real_))

    # Revealed-preference CS with the commission term in the inclusive value
    # (cs_weighted) and without it (cs_nocomm, the reported measure: the
    # commission is the broker's incentive, not a household valuation).
    cs    <- tryCatch(compute_consumer_surplus(dt, coefs), error = function(e) NA_real_)
    cs_nc <- tryCatch(compute_consumer_surplus(dt, coefs, welfare_drop = COMM_TERMS), error = function(e) NA_real_)
    # per_hh = TRUE returns per-household nav / obj / components; agg() below is the
    # household-weighted mean.
    whh <- tryCatch(scenario_welfare(dt, coefs, lambda, y, CS_TABLE, mean_spending = espend, per_hh = TRUE,
                                     unins_sched = if (exists("UNINS_SCHED")) UNINS_SCHED else NULL), error = function(e) NULL)
    if (is.null(whh)) {
      cell <- data.table(region = r, year = y, scenario = lab, cs_weighted = cs, cs_nocomm = cs_nc,
                         cs_welfare_nav = NA_real_, cs_welfare_obj = NA_real_,
                         obj_prem = NA_real_, obj_eoop = NA_real_, obj_risk = NA_real_,
                         obj_insured = NA_real_, share_unins = NA_real_,
                         unins_oop = NA_real_, unins_mort = NA_real_, unins_cat = NA_real_,
                         producer_surplus = psg$ps, gov_subsidy = psg$gov, gov_csr = psg$csr,
                         gov_penalty = psg$pen, gov_uc_raw = psg$uc, rs_unins = psg$rs_unins)
      return(list(cell = cell, hh = NULL))
    }
    W <- sum(whh$w); agg <- function(x) sum(x * whh$w) / W
    # cs_welfare_obj is the central-scenario objective; the cost-band components let
    # sum2/cf3 rebuild the low/high band.
    cell <- data.table(region = r, year = y, scenario = lab, cs_weighted = cs, cs_nocomm = cs_nc,
                       cs_welfare_nav = agg(whh$nav), cs_welfare_obj = agg(whh$obj),
                       obj_prem = agg(whh$obj_prem), obj_eoop = agg(whh$obj_eoop), obj_risk = agg(whh$obj_risk),
                       obj_insured = agg(whh$obj_insured), share_unins = agg(whh$share_unins),
                       unins_oop = agg(whh$unins_oop), unins_mort = agg(whh$unins_mort), unins_cat = agg(whh$unins_cat),
                       producer_surplus = psg$ps, gov_subsidy = psg$gov, gov_csr = psg$csr,
                       gov_penalty = psg$pen, gov_uc_raw = psg$uc, rs_unins = psg$rs_unins)
    list(cell = cell, hh = data.table(region = r, year = y, scenario = lab, whh))
  })
  # Uncompensated care scaled by the uninsured pool's risk score relative to the
  # baseline scenario; government cost total = subsidies + CSR + uncompensated care
  # - penalty revenue.
  cells_dt <- rbindlist(lapply(per, function(x) if (is.null(x)) NULL else x$cell))
  if (nrow(cells_dt) > 0) {
    rs_base <- cells_dt[scenario == "baseline", rs_unins]
    rs_base <- if (length(rs_base) == 1 && is.finite(rs_base)) rs_base else NA_real_
    cells_dt[, gov_uc := gov_uc_raw * (if (is.finite(rs_base)) rs_unins / rs_base else 1)]
    cells_dt[, gov_total := gov_subsidy + gov_csr + gov_uc - gov_penalty]
    for (i in seq_along(per)) if (!is.null(per[[i]])) {
      per[[i]]$cell <- cells_dt[scenario == per[[i]]$cell$scenario[1]]
    }
  }
  per <- per[!vapply(per, is.null, logical(1))]
  if (length(per) == 0) return(NULL)
  hh_all <- rbindlist(lapply(per, `[[`, "hh"))
  if (nrow(hh_all) > 0) fwrite(hh_all, file.path(hh_dir, sprintf("cell_%s_%s.csv", r, y)))
  rbindlist(lapply(per, `[[`, "cell"))
}
