# Objective (money-metric) normative utility V^N.
#
# A second normative benchmark to set alongside the navigator-based V^N. Values
# each plan in dollars as premium plus the certainty-equivalent burden of
# out-of-pocket (OOP) spending, with OOP built from the plan's ACTUAL cost-sharing
# (deductible -> coinsurance -> MOOP) rather than approximated by (1 - AV). This is
# feasible here because Covered California uses standardized benefit designs, so
# cost-sharing is fixed by metal/CSR tier (data/input/ca_standard_cost_sharing.csv).
#
# NOTHING below is filled with invented numbers. The CA cost-sharing values must be
# entered from Covered California's published Patient-Centered Benefit Designs, and
# the two calibration scalars must be set from the literature. The functions error
# loudly on NA so nothing can silently run on placeholders.

# --- Calibration scalars (literature values; confirm before final results) ----
# Coefficient of variation of annual individual health spending. Right-skewed;
# from AHRQ MEPS 2022 concentration tabulation (Stat. Brief #560), CV >= 2.5 as a
# strict lower bound, plausibly 3-4 with within-group dispersion. Using the lower
# bound; sensitivity to 3 and 4 is reported in the appendix. Gives the lognormal spending
# distribution its spread, which is what makes risk aversion bite.
SPENDING_CV <- 2.5
# CARA absolute risk aversion, PER DOLLAR. Handel (2013, AER) mean ~2.3e-4/$ is
# the common ESI/marketplace benchmark; defensible range ~1e-4 to 1e-3. Units:
# objective_vN works in raw annual dollars (premium, OOP, MOOP all in $), so this
# per-dollar value is used directly — no /100 rescaling (unlike the demand alpha,
# which is per-$100).
RHO_RISK_AVERSION <- 2.3e-4

# Assumed dollar cost of financial distress per uninsured person who crosses the
# catastrophic-expenditure line (spending > 40% of income) — debt, damaged credit,
# collections, forgone care. This is the one deliberately DIALABLE assumption in
# the uninsured valuation; the objective scoring emits the catastrophic SHARE
# separately so this multiplier can be varied after the fact without re-solving.
DISTRESS_COST <- 10000

# --- CA standardized cost-sharing lookup -------------------------------------
# Returns a data.frame keyed by (year, metal, hsa) with av + deductible,
# coinsurance, moop. Join to cell_data on (year, metal, hsa).
load_ca_cost_sharing <- function(path = "data/input/ca_standard_cost_sharing.csv") {
  cs <- read.csv(path, stringsAsFactors = FALSE)
  if (anyNA(cs$deductible) || anyNA(cs$coinsurance) || anyNA(cs$moop))
    warning("ca_standard_cost_sharing.csv has unfilled cost-sharing cells — ",
            "fill them from Covered California's standard benefit designs.")
  cs
}

# --- OOP schedule and moments ------------------------------------------------
# Standard schedule: pay 100% up to the deductible, then coinsurance, capped at
# MOOP (and never more than total spending).
oop_schedule <- function(s, deductible, coinsurance, moop) {
  paid <- pmin(s, deductible) + coinsurance * pmax(s - deductible, 0)
  pmin(paid, moop)
}

# E[OOP] and Var[OOP] over a lognormal annual-spending distribution with mean
# `espend` and coefficient of variation `cv`. Deterministic quadrature (no RNG)
# so it is exactly reproducible.
oop_moments <- function(espend, cv, deductible, coinsurance, moop, n_grid = 400L) {
  if (anyNA(c(cv, deductible, coinsurance, moop)))
    stop("oop_moments: NA input — fill the CA cost-sharing table and set SPENDING_CV.")
  if (espend <= 0) return(c(mean = 0, var = 0))
  s2    <- log(1 + cv^2)
  sdlog <- sqrt(s2)
  mlog  <- log(espend) - s2 / 2
  p     <- (seq_len(n_grid) - 0.5) / n_grid          # midpoint quantiles
  s     <- qlnorm(p, meanlog = mlog, sdlog = sdlog)
  oop   <- oop_schedule(s, deductible, coinsurance, moop)
  m     <- mean(oop)
  c(mean = m, var = mean((oop - m)^2))
}

# --- Objective normative value of a plan (annual dollars) ---------------------
# premium_annual must be the annual premium the household pays (net premium x 12).
# Returns V^N in dollars; higher is better (lower total cost of coverage). This is
# the negative of premium + the mean-variance certainty equivalent of OOP.
objective_vN <- function(premium_annual, espend, cv, rho, deductible, coinsurance, moop) {
  m <- oop_moments(espend, cv, deductible, coinsurance, moop)
  unname(-(premium_annual + m["mean"] + (rho / 2) * m["var"]))
}

# --- Validation helper -------------------------------------------------------
# Implied actuarial value = 1 - E[OOP]/E[spend]. Should track the statutory `av`
# column; a check on the table entries and on SPENDING_CV before using results.
implied_av <- function(espend, cv, deductible, coinsurance, moop) {
  m <- oop_moments(espend, cv, deductible, coinsurance, moop)
  unname(1 - m["mean"] / espend)
}

# --- Age/income spending schedule (optional) ---------------------------------
# Replaces the flat MEAN_SPENDING with household-specific expected annual individual
# spending drawn from an EXTERNAL schedule (e.g. MEPS by age x income). This is a
# MEASURED input, not a calibration -- it swaps a guessed constant for numbers a
# large survey pins down, and it introduces no moral hazard because spending depends
# on the household's characteristics, never on the plan.
#
# load_spending_schedule returns NULL if the file is missing or any mean_spend cell
# is unfilled, so the caller falls back to the flat scalar and the pipeline still
# runs. Fill data/input/meps_spending_by_demographics.csv from MEPS to turn it on;
# nothing here is populated with invented numbers.
load_spending_schedule <- function(path = "data/input/meps_spending_by_demographics.csv") {
  if (!file.exists(path)) return(NULL)
  s <- read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("age_group", "income", "mean_spend") %in% names(s))) return(NULL)
  if (anyNA(s$mean_spend)) {
    warning("meps_spending_by_demographics.csv has unfilled mean_spend -- ",
            "using flat MEAN_SPENDING until it is filled from MEPS.")
    return(NULL)
  }
  s
}

# Per-ROW expected annual individual spending from the household's age mix and income
# bracket. Household spending follows the person (age composition + income), not the
# plan, so it is constant across a household's plan rows and carries no moral hazard.
# schedule = NULL -> flat `default` (reproduces the pre-schedule behavior exactly).
household_spending <- function(cell_data, schedule = NULL, default = MEAN_SPENDING) {
  n <- nrow(cell_data)
  if (is.null(schedule)) return(rep(default, n))
  d   <- as.data.table(cell_data)
  p0  <- if ("perc_0to17"  %in% names(d)) d$perc_0to17  else rep(0, n)
  p18 <- if ("perc_18to34" %in% names(d)) d$perc_18to34 else rep(0, n)
  p35 <- if ("perc_35to54" %in% names(d)) d$perc_35to54 else rep(0, n)
  p55 <- pmax(0, 1 - p0 - p18 - p35)
  inc <- fifelse(("FPL_400plus"  %in% names(d)) & d$FPL_400plus  == 1, "400plus",
          fifelse(("FPL_250to400" %in% names(d)) & d$FPL_250to400 == 1, "250to400", "lt250"))
  look <- function(age, income) {
    m <- schedule$mean_spend[match(paste(age, income), paste(schedule$age_group, schedule$income))]
    fifelse(is.na(m), default, m)
  }
  p0 * look("0to17", inc) + p18 * look("18to34", inc) +
    p35 * look("35to54", inc) + p55 * look("55plus", inc)
}

# --- Uninsured out-of-pocket schedule (measured) -----------------------------
# What uninsured people ACTUALLY pay out of pocket, by age x income, from MEPS
# (build-meps-uninsured-oop.R -> data/input/meps_uninsured_oop.csv). Replaces the
# old assumption that an uninsured person pays their full, uncapped medical
# spending — which put the uninsured value near -$30k/yr and dominated the
# objective. Returns NULL if the file is missing/unfilled so the caller can fall
# back. Columns: age_group, income, mean_oop, var_oop, catastrophic_rate.
load_uninsured_oop <- function(path = "data/input/meps_uninsured_oop.csv") {
  if (!file.exists(path)) return(NULL)
  s <- read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("age_group", "income", "mean_oop", "catastrophic_rate") %in% names(s))) return(NULL)
  if (anyNA(s$mean_oop) || anyNA(s$catastrophic_rate)) return(NULL)
  s
}

# Per-ROW expected realized out-of-pocket and catastrophic rate for the uninsured
# option, from the household's age mix and income bracket (same construction as
# household_spending). Returns a list(oop, cat); schedule = NULL -> oop = default,
# cat = 0 (so the caller can fall back to the full-spending valuation).
household_uninsured_oop <- function(cell_data, schedule = NULL, default = MEAN_SPENDING) {
  n <- nrow(cell_data)
  if (is.null(schedule)) return(list(oop = rep(default, n), cat = rep(0, n)))
  d   <- as.data.table(cell_data)
  p0  <- if ("perc_0to17"  %in% names(d)) d$perc_0to17  else rep(0, n)
  p18 <- if ("perc_18to34" %in% names(d)) d$perc_18to34 else rep(0, n)
  p35 <- if ("perc_35to54" %in% names(d)) d$perc_35to54 else rep(0, n)
  p55 <- pmax(0, 1 - p0 - p18 - p35)
  inc <- fifelse(("FPL_400plus"  %in% names(d)) & d$FPL_400plus  == 1, "400plus",
          fifelse(("FPL_250to400" %in% names(d)) & d$FPL_250to400 == 1, "250to400", "lt250"))
  look <- function(age, income, col) {
    v <- schedule[[col]][match(paste(age, income), paste(schedule$age_group, schedule$income))]
    fifelse(is.na(v), if (col == "mean_oop") default else 0, v)
  }
  age_mix <- function(col)
    p0 * look("0to17", inc, col) + p18 * look("18to34", inc, col) +
    p35 * look("35to54", inc, col) + p55 * look("55plus", inc, col)
  list(oop = age_mix("mean_oop"), cat = age_mix("catastrophic_rate"))
}

# --- Social cost of being uninsured beyond realized OOP + distress ------------
# Two literature-anchored channels, added per person-year to realized out-of-pocket
# and the catastrophic-distress overlay:
#   (1) risk protection / consumption smoothing -- Finkelstein-Hendren-Luttmer
#       (JPE 2019) pure-insurance component from the Oregon experiment ($133-$1,106).
#   (2) mortality -- age-specific baseline annual mortality x the proportional
#       reduction in mortality from coverage (Miller-Johnson-Wherry QJE 2021,
#       Goldin-Lurie-McCubbin QJE 2021: ~10-20%; Oregon null) x the common federal
#       value of a statistical life (~$13M, HHS 2024). We apply a single VSL to all
#       incomes by design, so the poor are not valued less for being unable to pay.
# Reported as a LOW/CENTRAL/HIGH band; the mortality EFFECT SIZE is the main
# uncertainty. The whole conclusion leans on channel (2), which dominates the others.
UNINS_COST_SCENARIO <- "central"                                  # low | central | high
UNINS_RISK_PROT  <- c(low = 150,    central = 625,    high = 1500)      # $/person-year (FHL)
UNINS_VSL        <- c(low = 11e6,   central = 13e6,   high = 13e6)      # value of statistical life ($)
UNINS_MORT_REDUX <- c(low = 0.05,   central = 0.15,   high = 0.20)      # proportional mortality reduction
# US age-specific baseline annual all-cause mortality, non-elderly (approx., CDC).
UNINS_BASE_MORT  <- c("0to17" = 0.0003, "18to34" = 0.0012, "35to54" = 0.003, "55plus" = 0.009)

# Per-ROW age-weighted baseline annual mortality for the household. Split out so
# the mortality dollar cost can be reconstructed from bootstrapped shares under any
# (mort_redux, VSL) scenario without re-scoring.
uninsured_base_mortality <- function(cell_data) {
  n <- nrow(cell_data)
  d   <- as.data.table(cell_data)
  p0  <- if ("perc_0to17"  %in% names(d)) d$perc_0to17  else rep(0, n)
  p18 <- if ("perc_18to34" %in% names(d)) d$perc_18to34 else rep(0, n)
  p35 <- if ("perc_35to54" %in% names(d)) d$perc_35to54 else rep(0, n)
  p55 <- pmax(0, 1 - p0 - p18 - p35)
  p0 * UNINS_BASE_MORT[["0to17"]]  + p18 * UNINS_BASE_MORT[["18to34"]] +
    p35 * UNINS_BASE_MORT[["35to54"]] + p55 * UNINS_BASE_MORT[["55plus"]]
}

# Per-ROW social cost of being uninsured for the household's age mix. Returns
# annual dollars; add to realized OOP and distress in the uninsured valuation.
uninsured_social_cost <- function(cell_data, scenario = UNINS_COST_SCENARIO) {
  bm <- uninsured_base_mortality(cell_data)
  UNINS_RISK_PROT[[scenario]] + bm * UNINS_MORT_REDUX[[scenario]] * UNINS_VSL[[scenario]]
}
# Welfare engine — welfare scored at ACTUAL choices under two normative rules.
#
# Sourced by the structural/CF pipeline. Relies on compute_utility / compute_alpha_i
# (supply.R) and oop_moments + the calibration scalars (above) being
# loaded, plus the CA cost-sharing table.
#
# The welfare object is, per household, the expected NORMATIVE utility of the plan
# the household actually ends up choosing: sum_j P_ij * V^N_ij, where P_ij is the
# full-model (steered) nested-logit choice probability and V^N is one of:
#   - navigator: impose the navigator/informed decision rule on everyone (informed
#     price slope and metal valuations folded into the base variables; commission
#     and broker terms dropped). Utility units -> dollars via the informed alpha.
#   - objective: a money-metric value, -(annual individual premium + certainty-
#     equivalent OOP), with OOP from the plan's actual cost-sharing (above).
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
  # Two-part nested logit (supply.R kernels): P(insured) from the base
  # inclusive value, P(j | insured) from the full utility.
  util <- compute_utility(cell_data, coefs)
  dt <- as.data.table(cell_data)
  dt[, .rid := .I]
  ins <- nest_inside_rows(dt, util$V, util$V_base, lambda)
  hh <- ins[, .(p_inside = first(s_g)), by = household_number]
  out <- merge(dt[plan_id == "Uninsured", .(.rid, household_number)], hh,
               by = "household_number", all.x = TRUE)
  out[is.na(p_inside), p_inside := 0]
  out[, p := 1 - p_inside]
  probs <- rbind(ins[, .(.rid, p = q_j)], out[, .(.rid, p)])
  setorder(probs, .rid)
  probs$p
}

# --- Navigator (informed-rule) normative utility, in utils --------------------
# Imposes the navigator (informed) decision rule on everyone: the navigator's price
# slope and generosity (AV) valuation are folded onto the base variables, and the
# broker / commission distortions are dropped. The demographic x AV terms are base
# preferences, not a channel distortion, so they pass through unchanged.
# Experienced welfare = these coefficients applied to the plan the household
# actually (steered) ends up in.
vN_navigator_coefs <- function(coefs) {
  cm <- setNames(coefs$estimate, coefs$term)
  g  <- function(n) if (n %in% names(cm)) cm[[n]] else 0
  if ("premium" %in% names(cm)) cm["premium"] <- g("premium") + g("assisted_premium")
  if ("av"      %in% names(cm)) cm["av"]      <- g("av")      + g("assisted_av")
  for (z in c("assisted_premium", "assisted_av", "broker_premium", "broker_av",
              "commission_broker"))
    if (z %in% names(cm)) cm[z] <- 0
  data.frame(term = names(cm), estimate = as.numeric(cm), stringsAsFactors = FALSE)
}

# --- Objective money-metric normative value per row ($) ------------------------
# Returns a data.table with the money metric DECOMPOSED into its three components
# (all in annual dollars, all <= 0, and v_total = their sum):
#   v_prem  = -(annual premium the household pays)
#   v_eoop  = -(expected out-of-pocket)          [no assumptions beyond cost-sharing]
#   v_risk  = -(rho/2) * Var(out-of-pocket)      [the calibrated risk-aversion piece]
# Splitting these lets us report how much of a counterfactual's welfare change is
# assumption-driven (v_risk) versus data-pinned (v_prem, v_eoop).
#
# `mean_spending` may be a SCALAR (flat annual individual spending, the default) or
# a per-ROW vector of household-specific spending (e.g. from an age/gender/income
# schedule). Rows are grouped by (spending bucket, metal, hsa) so the quadrature
# runs once per distinct combination. Effective tier per row from the metal dummies,
# hsa, and (for silver) the household CSR variant by FPL; uninsured pays full
# spending (no cap).
vN_objective <- function(cell_data, year, cs_table, cv, rho, mean_spending,
                         unins_sched = NULL, distress = DISTRESS_COST,
                         unins_scenario = UNINS_COST_SCENARIO) {
  d <- as.data.table(cell_data)
  n <- nrow(d)
  espend <- if (length(mean_spending) == 1L) rep(mean_spending, n) else mean_spending

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
  # precompute OOP mean + risk per distinct (spending bucket, metal, hsa). A scalar
  # espend collapses to one bucket, so this reproduces the flat-spending case exactly.
  ekey <- round(espend / 500) * 500
  labs <- unique(data.frame(ek = ekey, metal = metal, hsa = hsa, stringsAsFactors = FALSE))
  labs$ce_mean <- NA_real_; labs$ce_risk <- NA_real_
  for (i in seq_len(nrow(labs))) {
    es <- labs$ek[i]
    if (labs$metal[i] == "Uninsured") {
      m <- oop_moments(es, cv, deductible = Inf, coinsurance = 1, moop = Inf)
    } else {
      row <- cs_y[cs_y$metal == key(labs$metal[i]) & cs_y$hsa == labs$hsa[i], ]
      if (nrow(row) == 0) row <- cs_y[cs_y$metal == key(labs$metal[i]), ][1, ]
      m <- oop_moments(es, cv, row$deductible[1], row$coinsurance[1], row$moop[1])
    }
    labs$ce_mean[i] <- m["mean"]; labs$ce_risk[i] <- (rho / 2) * m["var"]
  }
  idx       <- match(paste(ekey, metal, hsa), paste(labs$ek, labs$metal, labs$hsa))
  eoop_mean <- labs$ce_mean[idx]
  eoop_risk <- labs$ce_risk[idx]

  # Uninsured option: value it by what uninsured people ACTUALLY pay (MEPS realized
  # out-of-pocket by age/income) rather than their full, uncapped spending. The tail
  # is handled explicitly as a catastrophic rate (share crossing 40% of income),
  # priced by the dialable distress cost, and the rate is also returned so the
  # multiplier can be varied after the fact. Falls back to the full-spending
  # uncapped valuation when the schedule is absent (unins_sched = NULL).
  is_unins <- d$plan_id == "Uninsured"
  cat_rate <- rep(0, nrow(d))
  if (!is.null(unins_sched)) {
    uo   <- household_uninsured_oop(d, unins_sched)
    soc  <- uninsured_social_cost(d, unins_scenario)     # risk protection + mortality
    eoop_mean[is_unins] <- uo$oop[is_unins]
    # risk term for the uninsured collects the catastrophic-distress overlay and the
    # risk-protection + mortality social cost
    eoop_risk[is_unins] <- distress * uo$cat[is_unins] + soc[is_unins]
    cat_rate[is_unins]  <- uo$cat[is_unins]
  }

  # annual individual premium: `premium` is net premium in $100/member/month
  annual_indiv_premium <- fifelse(d$plan_id == "Uninsured", 0, d$premium * 100 * 12)
  data.table(v_prem  = -annual_indiv_premium,
             v_eoop  = -eoop_mean,
             v_risk  = -eoop_risk,
             v_total = -(annual_indiv_premium + eoop_mean + eoop_risk),
             cat_rate = cat_rate,
             is_unins = as.integer(is_unins))
}

# --- Welfare at actual choices: sum_j P_ij V^N_ij, weighted --------------------
# vN_vec in dollars already (objective) -> alpha_vec = NULL. vN_vec in utils
# (navigator) -> pass alpha_vec (per row) to divide by |alpha|. return_per_hh = TRUE
# returns the per-household surplus data.table (hh, cs, w) instead of the weighted
# mean, so the caller can build the distribution of effects across households.
welfare_at_choices <- function(cell_data, probs, vN_vec, hh_weight, alpha_vec = NULL,
                               return_per_hh = FALSE) {
  contrib <- probs * vN_vec
  if (!is.null(alpha_vec)) contrib <- contrib / abs(alpha_vec)
  d <- data.table(hh = cell_data$household_number, w = hh_weight, c = contrib)
  per_hh <- d[, .(cs = sum(c), w = first(w)), by = hh]
  if (return_per_hh) return(per_hh)
  sum(per_hh$cs * per_hh$w) / sum(per_hh$w)
}

# --- Both welfare metrics at actual choices for one scenario's equilibrium -----
# per_hh = FALSE (default): returns the named vector the sequential pipeline expects,
#   c(nav, obj, obj_prem, obj_eoop, obj_risk) -- nav is dollars (utils / informed
#   |alpha|); obj is the money metric; obj_prem/eoop/risk decompose obj and sum to it.
# per_hh = TRUE: returns a per-household data.table (household_number, w, nav, obj,
#   obj_prem, obj_eoop, obj_risk) for the distribution of effects across households.
# Wrap in tryCatch at the call site.
scenario_welfare <- function(dt_final, coefs, lambda, year, cs_table,
                             cv = SPENDING_CV, rho = RHO_RISK_AVERSION,
                             mean_spending = MEAN_SPENDING, per_hh = FALSE,
                             unins_sched = NULL, unins_scenario = UNINS_COST_SCENARIO) {
  p   <- choice_probs(dt_final, coefs, lambda)
  nc  <- vN_navigator_coefs(coefs)
  vnv <- compute_utility(dt_final, nc)$V
  anv <- compute_alpha_i(dt_final, nc, spec = nc$term)
  vob <- vN_objective(dt_final, year, cs_table, cv, rho, mean_spending,
                      unins_sched = unins_sched, unins_scenario = unins_scenario)
  w   <- dt_final$hh_weight
  # Cost-band component inputs (all driven by the estimated parameters through the
  # choice probabilities p; the uninsured-cost DOLLARS are NOT baked in here). The
  # final objective welfare under any (risk_prot, mort_redux, VSL, distress) scenario
  # is reconstructed as
  #   obj_insured - unins_oop - risk_prot*share_unins
  #               - mort_redux*VSL*unins_mort - distress*unins_cat
  # so the bootstrap carries these five and the cost band is applied afterward.
  is_unins <- as.numeric(dt_final$plan_id == "Uninsured")
  uo   <- if (!is.null(unins_sched)) household_uninsured_oop(dt_final, unins_sched)
          else list(oop = rep(0, nrow(dt_final)), cat = rep(0, nrow(dt_final)))
  bm   <- uninsured_base_mortality(dt_final)
  v_ins <- vob$v_total * (1 - is_unins); oop_u <- uo$oop * is_unins
  bm_u  <- bm * is_unins;                cat_u <- uo$cat * is_unins
  if (per_hh) {
    ph <- function(v, a = NULL) welfare_at_choices(dt_final, p, v, w, alpha_vec = a, return_per_hh = TRUE)$cs
    nav_h <- welfare_at_choices(dt_final, p, vnv, w, alpha_vec = anv, return_per_hh = TRUE)
    return(data.table(household_number = nav_h$hh, w = nav_h$w,
                      nav = nav_h$cs, obj = ph(vob$v_total),
                      obj_prem = ph(vob$v_prem), obj_eoop = ph(vob$v_eoop), obj_risk = ph(vob$v_risk),
                      obj_insured = ph(v_ins), share_unins = ph(is_unins),
                      unins_oop = ph(oop_u), unins_mort = ph(bm_u), unins_cat = ph(cat_u)))
  }
  wc <- function(v) welfare_at_choices(dt_final, p, v, w)
  c(nav = welfare_at_choices(dt_final, p, vnv, w, alpha_vec = anv),
    obj = wc(vob$v_total), obj_prem = wc(vob$v_prem), obj_eoop = wc(vob$v_eoop), obj_risk = wc(vob$v_risk),
    obj_insured = wc(v_ins), share_unins = wc(is_unins),
    unins_oop = wc(oop_u), unins_mort = wc(bm_u), unins_cat = wc(cat_u))
}
# Headline counterfactual statistics from one CF welfare table.
#
# summarize_cf_headline(cf) takes a welfare table with one row per (region, year,
# scenario) carrying the cf2 welfare columns and returns the named vector of
# headline statistics the paper reports: each is a mean over cells of a scenario's
# welfare column minus the model-baseline value. Revealed-preference CS is the
# no-commission measure (cs_nocomm) throughout. Shared by cf3_se (bootstrap
# draws), so both difference exactly the
# statistics sum2 reports.

summarize_cf_headline <- function(cf) {
  cf  <- as.data.frame(cf)
  obs <- unique(cf[cf$scenario == "baseline",
                   c("region", "year", "cs_weighted", "cs_nocomm",
                     "cs_welfare_nav", "cs_welfare_obj",
                     "obj_prem", "obj_eoop", "obj_risk",
                     "obj_insured", "share_unins", "unins_oop", "unins_mort", "unins_cat")])
  mdelta <- function(scen, col) {                  # mean over cells of (col[scen] - col[baseline])
    s <- unique(cf[cf$scenario == scen, c("region", "year", col)])
    s <- s[!duplicated(s[c("region", "year")]), ]
    o <- obs[, c("region", "year", col)]; names(o)[3] <- "obsval"
    m <- merge(s, o, by = c("region", "year"))
    if (nrow(m) == 0) return(NA_real_)
    mean(m[[col]] - m$obsval, na.rm = TRUE)
  }
  taus <- c(0, 0.25, 0.5, 0.75, 1.0)
  grad <- vapply(taus, function(t) mdelta(sprintf("zero_tau%.2f", t), "cs_nocomm"), numeric(1))
  names(grad) <- paste0("grad_cs_tau", sprintf("%.2f", taus))
  # Endogenous-commission scenarios (endog_tau0 = baseline, not carried).
  taus_e <- c(0.5, 1.0)
  grad_e <- vapply(taus_e, function(t) mdelta(sprintf("endog_tau%.2f", t), "cs_nocomm"), numeric(1))
  names(grad_e) <- paste0("grad_cs_endog_tau", sprintf("%.2f", taus_e))
  # Cost-band components per scenario (coverage share, insured composition, and the
  # uninsured-weighted OOP / baseline mortality / catastrophic pieces). sum2 rebuilds
  # the objective band from these.
  comp_scen <- c("zero_tau0.00", "zero_tau1.00", "uniform", "aligned",
                 "endog_tau1.00", "flat_mandate", "defund_1.00")
  comp <- unlist(lapply(comp_scen, function(s)
    setNames(c(mdelta(s, "share_unins"), mdelta(s, "obj_insured"), mdelta(s, "unins_oop"),
               mdelta(s, "unins_mort"),  mdelta(s, "unins_cat")),
             paste0(c("dshare_", "dobjins_", "doop_", "dmort_", "dcat_"), s))))
  # obj decomposed into premium / expected-OOP / risk.
  c(va_cs            = unname(grad["grad_cs_tau1.00"] - grad["grad_cs_tau0.00"]),
    grad,
    va_nav           = mdelta("zero_tau1.00", "cs_welfare_nav") - mdelta("zero_tau0.00", "cs_welfare_nav"),
    va_obj           = mdelta("zero_tau1.00", "cs_welfare_obj") - mdelta("zero_tau0.00", "cs_welfare_obj"),
    va_obj_prem      = mdelta("zero_tau1.00", "obj_prem") - mdelta("zero_tau0.00", "obj_prem"),
    va_obj_eoop      = mdelta("zero_tau1.00", "obj_eoop") - mdelta("zero_tau0.00", "obj_eoop"),
    va_obj_risk      = mdelta("zero_tau1.00", "obj_risk") - mdelta("zero_tau0.00", "obj_risk"),
    grad_e,
    va_cs_endog      = unname(grad_e["grad_cs_endog_tau1.00"]),
    va_nav_endog     = mdelta("endog_tau1.00", "cs_welfare_nav"),
    va_obj_endog     = mdelta("endog_tau1.00", "cs_welfare_obj"),
    flatmand_dcs     = mdelta("flat_mandate", "cs_nocomm"),
    flatmand_obj     = mdelta("flat_mandate", "cs_welfare_obj"),
    defund_dcs       = mdelta("defund_1.00", "cs_nocomm"),
    defund_obj       = mdelta("defund_1.00", "cs_welfare_obj"),
    aligned_dcs      = mdelta("aligned", "cs_nocomm"),
    aligned_dcs_comm = mdelta("aligned", "cs_weighted"),
    aligned_nav      = mdelta("aligned", "cs_welfare_nav"),
    aligned_obj      = mdelta("aligned", "cs_welfare_obj"),
    aligned_obj_prem = mdelta("aligned", "obj_prem"),
    aligned_obj_eoop = mdelta("aligned", "obj_eoop"),
    aligned_obj_risk = mdelta("aligned", "obj_risk"),
    comp)
}
