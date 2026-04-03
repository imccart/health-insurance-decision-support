# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-26
## Description:   Risk adjustment helper functions for structural estimation.
##                Estimates risk score and claims regressions from rate filings,
##                predicts plan-level risk scores from demographic composition,
##                computes endogenous RA transfers for counterfactuals.

# Risk score and claims regressions ---------------------------------------

#' Estimate risk score and claims regressions from rate filing data.
#' Called once before the supply cell loop.
#'
#' @param rsdata  Rate filing data (from rate_filing_rsdata.csv)
#' @return List with rs_reg (risk score lm), claims_reg (claims lm),
#'         rs_coefs (named vector), claims_coefs (named vector)

estimate_ra_regressions <- function(rsdata) {

  # Drop invalid risk scores
  rs_valid <- rsdata %>%
    filter(!is.na(log_risk_score), is.finite(log_risk_score), EXP_MM > 0)

  # Risk score regression (Saltzman spec)
  # Metal tier captures adverse selection; demographics capture within-tier
  # composition effects. Demographics come from observed enrollment (merged
  # from plan_demographics.csv before calling this function).
  has_demo <- all(c("share_18to34", "share_35to54", "share_hispanic") %in% names(rs_valid))
  if (has_demo) {
    # Drop rows missing demographics (unmatched plan-years)
    rs_valid <- rs_valid %>%
      filter(!is.na(share_18to34), !is.na(share_35to54), !is.na(share_hispanic))
    rs_reg <- lm(log_risk_score ~ Silver + Gold + Platinum +
                   share_18to34 + share_35to54 + share_hispanic,
                 data = rs_valid, weights = rs_valid$EXP_MM)
  } else {
    rs_reg <- lm(log_risk_score ~ Silver + Gold + Platinum,
                 data = rs_valid, weights = rs_valid$EXP_MM)
  }

  cat("  Risk score regression: N =", nrow(rs_valid),
      ", demographics =", has_demo, "\n")
  cat("  R² =", round(summary(rs_reg)$r.squared, 4), "\n")

  # Claims regression
  claims_valid <- rs_valid %>%
    filter(!is.na(log_cost), is.finite(log_cost))

  claims_reg <- lm(log_cost ~ log_risk_score + HMO + trend +
                      Anthem + Blue_Shield + Health_Net + Kaiser,
                    data = claims_valid, weights = claims_valid$EXP_MM)

  cat("  Claims regression: N =", nrow(claims_valid), "\n")
  cat("  R² =", round(summary(claims_reg)$r.squared, 4), "\n")

  list(
    rs_reg      = rs_reg,
    claims_reg  = claims_reg,
    rs_coefs    = coef(rs_reg),
    claims_coefs = coef(claims_reg)
  )
}


# Demographic shares from choice probabilities ----------------------------

#' Compute plan-level demographic shares from HH-level choice probabilities.
#' This is the function that makes RA endogenous — when enrollment shifts,
#' demographic shares change, which changes predicted risk scores.
#'
#' @param cell_data   Long-format HH × plan data with demographics and utility
#' @param V           Utility vector (same length as cell_data rows)
#' @param lambda      Nesting parameter
#' @return Tibble: plan_name, share_18to34, share_35to54, share_hispanic, demand

compute_demographic_shares <- function(cell_data, V, lambda) {

  dt <- as.data.table(cell_data)
  dt[, V := V]

  # Nested logit choice probabilities (same formula as compute_shares_and_elasticities)
  ins_dt <- dt[plan_name != "Uninsured"]
  out_dt <- dt[plan_name == "Uninsured"]

  # Within-nest: exp(V_j / lambda)
  ins_dt[, V_scaled := V / lambda]
  ins_dt[, max_V_scaled := max(V_scaled), by = household_number]
  ins_dt[, exp_V := exp(V_scaled - max_V_scaled)]
  ins_dt[, sum_exp_V := sum(exp_V), by = household_number]

  # Log inclusive value
  ins_dt[, log_IV := max_V_scaled + log(sum_exp_V)]

  # Outside option utility
  out_dt[, V_out := V]
  hh_V_out <- out_dt[, .(V_out = first(V_out)), by = household_number]
  ins_dt <- merge(ins_dt, hh_V_out, by = "household_number", all.x = TRUE)
  ins_dt[is.na(V_out), V_out := 0]

  # P(insured) = exp(lambda * log_IV) / (exp(V_out) + exp(lambda * log_IV))
  ins_dt[, log_num_ins := lambda * log_IV]
  ins_dt[, max_top := pmax(V_out, log_num_ins)]
  ins_dt[, P_ins := exp(log_num_ins - max_top) /
           (exp(V_out - max_top) + exp(log_num_ins - max_top))]

  # P(j | insured) = exp(V_j/lambda) / sum(exp(V_k/lambda))
  ins_dt[, P_j_ins := exp_V / sum_exp_V]

  # P(j) = P(insured) * P(j | insured)
  ins_dt[, prob := P_ins * P_j_ins]

  # Weight by ipweight
  w <- if ("ipweight" %in% names(ins_dt)) ins_dt$ipweight else rep(1, nrow(ins_dt))
  ins_dt[, w := w]
  ins_dt[, wp := w * prob]

  # Aggregate demographic shares by plan
  # Requires perc_18to34 = perc_18to25 + perc_26to34 if only components available
  if (!"perc_18to34" %in% names(ins_dt) && "perc_18to25" %in% names(ins_dt)) {
    ins_dt[, perc_18to34 := perc_18to25 + perc_26to34]
  }

  demo_shares <- ins_dt[, .(
    share_18to34  = sum(wp * perc_18to34, na.rm = TRUE) / sum(wp),
    share_35to54  = sum(wp * perc_35to54, na.rm = TRUE) / sum(wp),
    share_hispanic = sum(wp * perc_hispanic, na.rm = TRUE) / sum(wp),
    demand        = sum(wp)
  ), by = plan_name]

  as_tibble(demo_shares)
}


# Predict risk scores -----------------------------------------------------

#' Predict plan-level risk scores given RA regression coefficients,
#' plan characteristics, and demographic shares.
#'
#' @param rs_coefs     Named coefficient vector from risk score regression
#' @param plan_chars   Tibble with plan_name, Silver, Gold, Platinum
#' @param demo_shares  Tibble from compute_demographic_shares (or NULL for base model)
#' @return Tibble: plan_name, predicted_risk_score

predict_risk_scores <- function(rs_coefs, plan_chars, demo_shares = NULL) {

  pred_data <- plan_chars

  # If we have demographic shares, merge and use full model
  if (!is.null(demo_shares)) {
    pred_data <- pred_data %>%
      left_join(demo_shares %>% select(plan_name, share_18to34, share_35to54, share_hispanic),
                by = "plan_name")
  }

  # Predict log risk score
  log_rs <- rs_coefs[["(Intercept)"]] +
    rs_coefs[["Silver"]] * pred_data$Silver +
    rs_coefs[["Gold"]] * pred_data$Gold +
    rs_coefs[["Platinum"]] * pred_data$Platinum

  # Add demographic terms if available and in the model
  if (!is.null(demo_shares) && "share_18to34" %in% names(rs_coefs)) {
    log_rs <- log_rs +
      rs_coefs[["share_18to34"]] * pred_data$share_18to34 +
      rs_coefs[["share_35to54"]] * pred_data$share_35to54 +
      rs_coefs[["share_hispanic"]] * pred_data$share_hispanic
  }

  tibble(
    plan_name = pred_data$plan_name,
    predicted_risk_score = exp(log_rs),
    log_risk_score_hat = log_rs
  )
}


# RA transfers ------------------------------------------------------------

#' Compute budget-neutral RA transfers based on predicted risk scores
#' and current market shares. Plans with above-average risk receive
#' positive transfers; plans below pay in.
#'
#' @param predicted_risk_scores  Named vector or tibble with plan_name + predicted_risk_score
#' @param plan_shares            Named vector: plan_name → market share (among insured)
#' @param avg_premium            Scalar: average premium PMPM in market
#' @param plan_avs               Named vector: plan_name → actuarial value
#' @return Named vector of PMPM RA transfers per plan

compute_ra_transfers <- function(predicted_risk_scores, plan_shares, avg_premium, plan_avs) {

  # If tibble, extract as named vector
  if (is.data.frame(predicted_risk_scores)) {
    rs_vec <- setNames(predicted_risk_scores$predicted_risk_score,
                       predicted_risk_scores$plan_name)
  } else {
    rs_vec <- predicted_risk_scores
  }

  pn <- names(plan_shares)
  rs <- rs_vec[pn]
  sh <- plan_shares[pn]
  av <- plan_avs[pn]

  # Moral hazard factors by AV (same as MH_FACTOR in data build)
  MH_LOOKUP <- c("0.6" = 1.00, "0.7" = 1.03, "0.8" = 1.08, "0.9" = 1.15)
  av_rounded <- as.character(round(av, 1))
  mh <- MH_LOOKUP[av_rounded]
  mh[is.na(mh)] <- 1.0

  # ACA RA formula (Pope et al. 2014):
  # Total RA for plan j = (risk_share_j - util_share_j) × total_premium_revenue
  # PMPM RA = total RA / member_months = (risk_share_j - util_share_j) × avg_premium / share_j
  #
  # risk_share_j = (rs_j × share_j) / Σ(rs_k × share_k)
  # util_share_j = (util_adj_j × share_j) / Σ(util_adj_k × share_k)
  #
  # Simplifying: PMPM RA = (rs_j / Σ(rs·sh) - util_adj_j / Σ(util_adj·sh)) × avg_premium
  # This is budget-neutral: Σ(PMPM_j × share_j) = 0 by construction.

  sum_rs_sh <- sum(rs * sh, na.rm = TRUE)
  util_adj <- av * mh
  sum_util_sh <- sum(util_adj * sh, na.rm = TRUE)

  ra_pmpm <- (rs / sum_rs_sh - util_adj / sum_util_sh) * avg_premium

  setNames(ra_pmpm, pn)
}


# Predict claims ----------------------------------------------------------

#' Predict plan-level claims from risk scores and claims regression.
#'
#' @param claims_coefs  Named coefficient vector from claims regression
#' @param plan_chars    Tibble with plan_name, HMO, trend, insurer dummies
#' @param log_rs        Named vector of log predicted risk scores
#' @return Named vector of predicted claims PMPM

predict_claims <- function(claims_coefs, plan_chars, log_rs) {

  pn <- plan_chars$plan_name
  log_cost <- claims_coefs[["(Intercept)"]] +
    claims_coefs[["log_risk_score"]] * log_rs[pn]

  # Add HMO if in model
  if ("HMO" %in% names(claims_coefs)) {
    log_cost <- log_cost + claims_coefs[["HMO"]] * plan_chars$HMO
  }
  # Add trend if in model
  if ("trend" %in% names(claims_coefs)) {
    log_cost <- log_cost + claims_coefs[["trend"]] * plan_chars$trend
  }
  # Add insurer FEs (NA coefficient = reference category, treat as 0)
  for (ins in c("Anthem", "Blue_Shield", "Health_Net", "Kaiser")) {
    if (ins %in% names(claims_coefs) && ins %in% names(plan_chars)) {
      coef_val <- claims_coefs[[ins]]
      if (!is.na(coef_val)) {
        log_cost <- log_cost + coef_val * plan_chars[[ins]]
      }
    }
  }

  setNames(exp(log_cost), pn)
}


# Structural MC -----------------------------------------------------------

#' Compute structural marginal cost from predicted claims and RA transfers.
#'
#' @param predicted_claims  Named vector of predicted claims PMPM
#' @param ra_transfers      Named vector of RA transfers PMPM
#' @param reins_factors     Named vector of reinsurance factors (0 after 2016)
#' @return Named vector of MC PMPM

predict_mc_structural <- function(predicted_claims, ra_transfers, reins_factors) {
  pn <- names(predicted_claims)
  rf <- reins_factors[pn]
  rf[is.na(rf)] <- 0

  # MC = claims * (1 - reinsurance) - RA transfer
  mc <- predicted_claims * (1 - rf) - ra_transfers[pn]
  setNames(mc, pn)
}


# compute_mc ---------------------------------------------------------------

#' Single entry point for the full MC chain: demographics → risk scores →
#' claims → RA transfers → structural MC. Called identically by 2_pricing.R,
#' 3_cost_gmm.R, and 4a_cf-worker.R.
#'
#' @param rs_coefs      Named vector of risk score regression coefficients
#' @param claims_coefs  Named vector of claims regression coefficients
#' @param plan_chars    Tibble with plan_name, Silver, Gold, Platinum, HMO,
#'                      trend, Anthem, Blue_Shield, Health_Net, Kaiser
#' @param demo_shares   Tibble with plan_name, share_18to34, share_35to54,
#'                      share_hispanic (or NULL for metal-only model)
#' @param shares        Named vector of market shares (among insured)
#' @param avg_premium   Scalar average premium in market
#' @param plan_avs      Named vector of actuarial values
#' @param reins_vec     Named vector of reinsurance factors
#' @return List with mc (named vector), predicted_claims, predicted_risk_scores,
#'         ra_transfers, log_risk_score_hat

compute_mc <- function(rs_coefs, claims_coefs, plan_chars, demo_shares,
                       shares, avg_premium, plan_avs, reins_vec) {

  rs_pred <- predict_risk_scores(rs_coefs, plan_chars, demo_shares)
  log_rs <- setNames(rs_pred$log_risk_score_hat, rs_pred$plan_name)
  pred_claims <- predict_claims(claims_coefs, plan_chars, log_rs)
  ra_transfers <- compute_ra_transfers(rs_pred, shares, avg_premium, plan_avs)
  mc <- predict_mc_structural(pred_claims, ra_transfers, reins_vec)

  list(
    mc                    = mc,
    predicted_claims      = pred_claims,
    predicted_risk_scores = setNames(rs_pred$predicted_risk_score, rs_pred$plan_name),
    log_risk_score_hat    = log_rs,
    ra_transfers          = ra_transfers
  )
}


# RA derivative for FOC ----------------------------------------------------

#' Compute the RA contribution to the pricing FOC.
#'
#' When a firm changes price p_l, market shares shift, which changes the
#' budget-neutral RA transfers for all plans. The firm internalizes this
#' for the plans it owns. This function computes the J-vector ra_foc where:
#'
#'   ra_foc_l = sum_k O[l,k] * s_k * (dRA_k / dp_l)
#'
#' Channel 1 only: holds risk scores fixed, captures the mechanical effect
#' of share changes on RA denominators.
#'
#' @param risk_scores  Named vector of predicted risk scores (levels, not log)
#' @param shares       Named vector of market shares
#' @param plan_avs     Named vector of actuarial values
#' @param avg_premium  Scalar: average premium in market
#' @param elast_mat    J x J elasticity matrix (ds_j/dp_l)
#' @param own_mat      J x J ownership matrix (1 if same firm)
#' @return Named J-vector of RA FOC contributions

compute_ra_foc <- function(risk_scores, shares, plan_avs, avg_premium,
                           elast_mat, own_mat) {

  pn <- names(shares)
  J <- length(pn)
  rs <- unname(risk_scores[pn])
  sh <- unname(shares[pn])
  av <- unname(plan_avs[pn])

  # Moral hazard factors
  MH_LOOKUP <- c("0.6" = 1.00, "0.7" = 1.03, "0.8" = 1.08, "0.9" = 1.15)
  av_r <- as.character(round(av, 1))
  mh <- MH_LOOKUP[av_r]; mh[is.na(mh)] <- 1.0
  util_adj <- av * mh

  S_rs <- sum(rs * sh, na.rm = TRUE)
  S_u  <- sum(util_adj * sh, na.rm = TRUE)

  # dRA_k/ds_m = (-rs_k * rs_m / S_rs^2 + u_k * u_m / S_u^2) * avg_premium
  dRA_ds <- (-outer(rs, rs) / S_rs^2 + outer(util_adj, util_adj) / S_u^2) * avg_premium

  # dRA_k/dp_l = sum_m dRA_k/ds_m * ds_m/dp_l = dRA_ds %*% elast_mat
  # elast_mat and own_mat are J x J, already ordered by plan_names_cell
  dRA_dp <- dRA_ds %*% elast_mat

  # ra_foc_l = sum_k O[l,k] * s_k * dRA_dp[k,l]
  ra_foc <- colSums(own_mat * (sh * dRA_dp))

  setNames(ra_foc, pn)
}


# Expected OOP ------------------------------------------------------------

#' Compute expected out-of-pocket costs for welfare decomposition.
#' E[OOP_j] = (1 - AV_j) * predicted_claims_j
#' Population-weighted: E[OOP] = sum(shares_j * E[OOP_j])
#'
#' @param shares            Named vector of plan shares
#' @param plan_avs          Named vector of actuarial values
#' @param predicted_claims  Named vector of predicted claims PMPM
#' @return List with oop_by_plan (named vector) and oop_weighted (scalar)

compute_expected_oop <- function(shares, plan_avs, predicted_claims) {
  pn <- names(shares)
  oop <- (1 - plan_avs[pn]) * predicted_claims[pn]
  oop_weighted <- sum(shares[pn] * oop, na.rm = TRUE)

  list(
    oop_by_plan = setNames(oop, pn),
    oop_weighted = oop_weighted
  )
}
