# welfare_engine.R — Welfare scored at ACTUAL choices under two normative rules.
#
# Sourced by the structural/CF pipeline. Relies on compute_utility / compute_alpha_i
# (supply.R) and oop_moments + the calibration scalars (welfare_objective.R) being
# loaded, plus the CA cost-sharing table.
#
# The welfare object is, per household, the expected NORMATIVE utility of the plan
# the household actually ends up choosing: sum_j P_ij * V^N_ij, where P_ij is the
# full-model (steered) nested-logit choice probability and V^N is one of:
#   - navigator: impose the navigator/informed decision rule on everyone (informed
#     price slope and metal valuations folded into the base variables; commission
#     and broker terms dropped). Utility units -> dollars via the informed alpha.
#   - objective: a money-metric value, -(annual individual premium + certainty-
#     equivalent OOP), with OOP from the plan's actual cost-sharing (welfare_objective.R).
#
# FIRST-CUT: several documented defaults (MEAN_SPENDING, cell-level spend, navigator
# folding). Expect first-run shake-out; the descriptive CF outputs do not depend on
# any of this.

# Representative annual individual total health spending ($) — the scale for the
# objective metric's OOP distribution. The cost model is cell-level (no household
# spending), so this is a single representative level. Calibrate (MEPS mean ~ this).
MEAN_SPENDING <- 6000

# --- Per-row nested-logit choice probabilities (inside plans + outside) --------
choice_probs <- function(cell_data, coefs, lambda) {
  V <- compute_utility(cell_data, coefs)$V
  dt <- as.data.table(cell_data)
  dt[, `:=`(.V = V, .rid = .I, .is_out = (plan_id == "Uninsured"))]

  ins <- dt[.is_out == FALSE]
  ins[, vs := .V / lambda]
  ins[, mx := max(vs), by = household_number]
  ins[, p_in_nest := exp(vs - mx) / sum(exp(vs - mx)), by = household_number]
  ins[, logD := first(mx + log(sum(exp(vs - mx)))), by = household_number]

  hh <- ins[, .(logD = first(logD)), by = household_number]
  v0 <- dt[.is_out == TRUE, .(household_number, V0 = .V)]
  hh <- merge(hh, v0, by = "household_number", all.x = TRUE)
  hh[is.na(V0), V0 := 0]
  hh[, mxt := pmax(V0, lambda * logD)]
  hh[, p_inside := exp(lambda * logD - mxt) /
        (exp(V0 - mxt) + exp(lambda * logD - mxt))]

  ins <- merge(ins, hh[, .(household_number, p_inside)], by = "household_number")
  ins[, p := p_inside * p_in_nest]
  out <- merge(dt[.is_out == TRUE], hh[, .(household_number, p_inside)],
               by = "household_number")
  out[, p := 1 - p_inside]

  probs <- rbind(ins[, .(.rid, p)], out[, .(.rid, p)])
  setorder(probs, .rid)
  probs$p
}

# --- Navigator (informed-rule) normative utility, in utils --------------------
# Folds the navigator coefficients into the base variables so the informed rule is
# applied to everyone, and zeroes the interaction/distortion columns.
vN_navigator_coefs <- function(coefs) {
  cm <- setNames(coefs$estimate, coefs$term)
  g  <- function(n) if (n %in% names(cm)) cm[[n]] else 0
  if ("premium" %in% names(cm)) cm["premium"] <- g("premium") + g("assisted_premium")
  if ("silver"  %in% names(cm)) cm["silver"]  <- g("silver")  + g("assisted_silver")
  if ("bronze"  %in% names(cm)) cm["bronze"]  <- g("bronze")  + g("assisted_bronze")
  for (z in c("assisted_premium", "assisted_silver", "assisted_bronze",
              "assisted_gold", "assisted_plat", "broker_premium",
              "broker_silver", "broker_bronze", "commission_broker",
              "v_hat_commission"))
    if (z %in% names(cm)) cm[z] <- 0
  data.frame(term = names(cm), estimate = as.numeric(cm), stringsAsFactors = FALSE)
}

# --- Objective money-metric normative value per row ($), -(premium + OOP CE) ---
# Effective tier per row from the metal dummies, hsa, and (for silver) the
# household CSR variant by FPL; uninsured pays full spending (no cap).
vN_objective <- function(cell_data, year, cs_table, cv, rho, mean_spending) {
  d <- as.data.table(cell_data)

  # household CSR variant for silver (mirrors build_structural csr_* by FPL)
  fpl <- if ("FPL" %in% names(d)) d$FPL else rep(NA_real_, nrow(d))
  csr <- fifelse(fpl <= 1.5, "94",
          fifelse(fpl <= 2.0, "87",
           fifelse(fpl <= 2.5, "73", "std")))
  csr[is.na(csr)] <- "std"

  metal <- fifelse(d$plan_id == "Uninsured", "Uninsured",
            fifelse(d$silver == 1L, paste0("Silver", fifelse(csr == "std", "", paste0("_", csr))),
             fifelse(d$bronze == 1L, "Bronze",
              fifelse(d$gold == 1L, "Gold",
               fifelse(d$platinum == 1L, "Platinum", "Bronze")))))
  hsa <- if ("hsa" %in% names(d)) as.integer(d$hsa) else rep(0L, nrow(d))

  # map metal label -> the cost-sharing table's (metal, hsa) key
  cs_y <- cs_table[cs_table$year == year, ]
  key  <- function(m, h) {
    base <- switch(sub("_.*", "", m),
                   Silver = fcase(grepl("_94", m), "Silver - Enhanced 94",
                                  grepl("_87", m), "Silver - Enhanced 87",
                                  grepl("_73", m), "Silver - Enhanced 73",
                                  default = "Silver"),
                   Bronze = "Bronze", Gold = "Gold", Platinum = "Platinum",
                   m)
    base
  }
  # precompute OOP certainty equivalent per distinct (metal-label, hsa)
  labs <- unique(data.frame(metal = metal, hsa = hsa, stringsAsFactors = FALSE))
  labs$ce <- NA_real_
  for (i in seq_len(nrow(labs))) {
    if (labs$metal[i] == "Uninsured") {
      m <- oop_moments(mean_spending, cv, deductible = Inf, coinsurance = 1, moop = Inf)
    } else {
      row <- cs_y[cs_y$metal == key(labs$metal[i]) & cs_y$hsa == labs$hsa[i], ]
      if (nrow(row) == 0) row <- cs_y[cs_y$metal == key(labs$metal[i]), ][1, ]
      m <- oop_moments(mean_spending, cv, row$deductible[1], row$coinsurance[1], row$moop[1])
    }
    labs$ce[i] <- m["mean"] + (rho / 2) * m["var"]
  }
  ce <- labs$ce[match(paste(metal, hsa), paste(labs$metal, labs$hsa))]

  # annual individual premium: `premium` is net premium in $100/member/month
  annual_indiv_premium <- fifelse(d$plan_id == "Uninsured", 0, d$premium * 100 * 12)
  -(annual_indiv_premium + ce)
}

# --- Welfare at actual choices: sum_j P_ij V^N_ij, weighted --------------------
# vN_vec in dollars already (objective) -> alpha_vec = NULL. vN_vec in utils
# (navigator) -> pass alpha_vec (per row) to divide by |alpha|.
welfare_at_choices <- function(cell_data, probs, vN_vec, hh_weight, alpha_vec = NULL) {
  contrib <- probs * vN_vec
  if (!is.null(alpha_vec)) contrib <- contrib / abs(alpha_vec)
  d <- data.table(hh = cell_data$household_number, w = hh_weight, c = contrib)
  per_hh <- d[, .(cs = sum(c), w = first(w)), by = hh]
  sum(per_hh$cs * per_hh$w) / sum(per_hh$w)
}

# --- Both welfare metrics at actual choices for one scenario's equilibrium -----
# Returns c(nav = ..., obj = ...). nav is in dollars (utils / informed |alpha|);
# obj is the money-metric. Wrap in tryCatch at the call site.
scenario_welfare <- function(dt_final, coefs, lambda, year, cs_table,
                             cv = SPENDING_CV, rho = RHO_RISK_AVERSION,
                             mean_spending = MEAN_SPENDING) {
  p   <- choice_probs(dt_final, coefs, lambda)
  nc  <- vN_navigator_coefs(coefs)
  vnv <- compute_utility(dt_final, nc)$V
  anv <- compute_alpha_i(dt_final, nc, spec = nc$term)
  vob <- vN_objective(dt_final, year, cs_table, cv, rho, mean_spending)
  c(nav = welfare_at_choices(dt_final, p, vnv, dt_final$hh_weight, alpha_vec = anv),
    obj = welfare_at_choices(dt_final, p, vob, dt_final$hh_weight))
}
