# Meta --------------------------------------------------------------------
## Description:   Shared counterfactual WELFARE SCORER, used by cf2 (point
##                estimates) and cf3 (bootstrap draws). Given a cell's solved
##                premiums + commissions per scenario (cf_cell: region, year,
##                scenario, plan_id, premium_cf, commission_pmpm, tau), it reloads
##                the cached structural choice data, rebuilds each scenario, re-levels
##                premiums, applies the age/income spending schedule, and returns the
##                cell-level welfare (+ writes per-household welfare to hh_dir). Reads
##                the rest from the caller's environment: CELL_DIR, supply_results,
##                coefs, lambda, STRUCTURAL_SPEC, COMM_TERMS, CS_TABLE,
##                SPENDING_SCHEDULE. Frozen copies of cf_cell.R's scenario builder +
##                consumer-surplus fn (isolate-experimental-builds convention): the
##                scorer never sources the solver. cf2 passes cfres slices; cf3 passes
##                each draw's solved premiums.

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
    } else dt[, subsidy_cf := adj_subsidy]
    for (pn in names(p_vec)) {
      idx <- which(dt$plan_id == pn); if (length(idx) == 0) next
      premium_hh <- (p_vec[pn] / RATING_FACTOR_AGE40) * dt$rating_factor[idx]
      oop <- pmax(premium_hh - dt$subsidy_cf[idx], 0) - dt$penalty[idx] / 12
      set(dt, i = idx, j = "premium", value = oop / dt$hh_size[idx] / 100)
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
    cd[, `:=`(assisted_silver = nonbroker*silver, assisted_bronze = nonbroker*bronze,
              assisted_gold = nonbroker*gold, assisted_plat = nonbroker*platinum,
              broker_silver = broker*silver, broker_bronze = broker*bronze,
              assisted_premium = nonbroker*premium, broker_premium = broker*premium)]
    if ("dominated_plan" %in% names(cd)) {
      cd[, nav_dominated := nonbroker*dominated_plan]; cd[, broker_dominated := broker*dominated_plan]
    }
    cd
  }

  compute_consumer_surplus <- function(cell_data, coefs_cell, welfare_drop = character()) {
    lambda_cs <- setNames(coefs_cell$estimate, coefs_cell$term)[["lambda"]]
    if (length(welfare_drop) > 0) { cell_data <- as.data.table(copy(cell_data)); for (cn in intersect(welfare_drop, names(cell_data))) cell_data[[cn]] <- 0 }
    V <- compute_utility(cell_data, coefs_cell)$V
    dt <- as.data.table(cell_data); dt[, V := V]
    V0_by_hh <- dt[plan_id == "Uninsured", .(V_0 = V[1]), by = household_number]
    ins_dt <- dt[plan_id != "Uninsured"]
    ins_dt[, V_scaled := V / lambda_cs]; ins_dt[, max_V_scaled := max(V_scaled), by = household_number]
    ins_dt[, exp_V := exp(V_scaled - max_V_scaled)]; ins_dt[, sum_exp_V := sum(exp_V), by = household_number]
    ins_dt[, log_D := max_V_scaled + log(sum_exp_V)]; ins_dt[, log_D_lam := lambda_cs * log_D]
    ins_dt[, alpha_i := compute_alpha_i(ins_dt, coefs_cell, STRUCTURAL_SPEC)]
    ins_dt <- merge(ins_dt, V0_by_hh, by = "household_number", all.x = TRUE); ins_dt[is.na(V_0), V_0 := 0]
    hh_cs <- ins_dt[, .(log_D_lam = first(log_D_lam), V_0 = first(V_0), alpha_i = first(alpha_i), hh_weight = first(hh_weight)), by = household_number]
    hh_cs[, mx := pmax(V_0, log_D_lam)]
    hh_cs[, cs := (1 / abs(alpha_i)) * (mx + log(exp(V_0 - mx) + exp(pmin(log_D_lam - mx, 500))))]
    sum(hh_cs$hh_weight * hh_cs$cs) / sum(hh_cs$hh_weight)
  }

  # Scenarios are enumerated from cf1's output; the commission vector per
  # scenario is read from the persisted commission_pmpm exactly like premium_cf
  # (for the endogenous-commission scenarios that column carries the SOLVED eta,
  # so cf2 needs no scenario-construction knowledge and cannot drift from cf1).
  # Scenario data flags recovered from the label: endog_tau -> tau + brokers
  # remain; defund_<f> -> reverse conversion at fraction f; zero_tau -> tau.
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
    espend <- household_spending(dt, SPENDING_SCHEDULE)   # per-row; flat until MEPS filled
    cs    <- tryCatch(compute_consumer_surplus(dt, coefs), error = function(e) NA_real_)
    cs_nc <- if (!grepl("^zero_tau", lab) && lab != "uniform")
               tryCatch(compute_consumer_surplus(dt, coefs, welfare_drop = COMM_TERMS), error = function(e) NA_real_) else NA_real_
    # per_hh = TRUE gives per-household nav / obj / components; the cell-level number
    # is the household-weight-weighted mean of those, identical to the old aggregate.
    whh <- tryCatch(scenario_welfare(dt, coefs, lambda, y, CS_TABLE, mean_spending = espend, per_hh = TRUE), error = function(e) NULL)
    if (is.null(whh)) {
      cell <- data.table(region = r, year = y, scenario = lab, cs_weighted = cs, cs_nocomm = cs_nc,
                         cs_welfare_nav = NA_real_, cs_welfare_obj = NA_real_,
                         obj_prem = NA_real_, obj_eoop = NA_real_, obj_risk = NA_real_)
      return(list(cell = cell, hh = NULL))
    }
    W <- sum(whh$w); agg <- function(x) sum(x * whh$w) / W
    cell <- data.table(region = r, year = y, scenario = lab, cs_weighted = cs, cs_nocomm = cs_nc,
                       cs_welfare_nav = agg(whh$nav), cs_welfare_obj = agg(whh$obj),
                       obj_prem = agg(whh$obj_prem), obj_eoop = agg(whh$obj_eoop), obj_risk = agg(whh$obj_risk))
    list(cell = cell, hh = data.table(region = r, year = y, scenario = lab, whh))
  })
  per <- per[!vapply(per, is.null, logical(1))]
  if (length(per) == 0) return(NULL)
  hh_all <- rbindlist(lapply(per, `[[`, "hh"))
  if (nrow(hh_all) > 0) fwrite(hh_all, file.path(hh_dir, sprintf("cell_%s_%s.csv", r, y)))
  rbindlist(lapply(per, `[[`, "cell"))
}
