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

  # Risk score regression: Saltzman Eq. 16 (AV + age/gender demographic shares) PLUS
  # insurer fixed effects, which is our one deviation from his equation. It is
  # data-motivated and earned by a test. Under his exact Eq. 16 the low-cost carriers
  # (Molina foremost) are over-scored — the equation has no carrier term, so it
  # cannot see that Molina runs a lower-risk book than its AV/demographics imply. It
  # over-predicts Molina's risk, hands it a transfer exceeding its (low) claims, and
  # drives its MC negative, even though the raw filings show Molina is a net RA payer,
  # so that negative is a mislabel. The insurer FE lets the equation learn the
  # carrier-level risk and pulls Molina's score (and transfer) back down. Same
  # INS_COST set as the claims equation, but Kaiser folds into the baseline here (no
  # HMO term on the risk side). Income/FPL shares stay OUT — they, not the insurer
  # FEs, drove the earlier coefficient degeneracy. AV enters here and is EXCLUDED
  # from claims; AV_METAL is renamed AV to match predict_risk_scores. To run his
  # exact spec instead, drop the insurer terms from the formula below —
  # predict_risk_scores applies whatever terms rs_coefs holds.
  rs_valid <- rs_valid %>% mutate(AV = AV_METAL)
  demo_terms <- c("share_18to34", "share_35to54", "share_male")
  has_demo <- all(demo_terms %in% names(rs_valid))
  if (has_demo) {
    rs_valid <- rs_valid %>%
      filter(!is.na(share_18to34), !is.na(share_35to54), !is.na(share_male))
    rs_reg <- lm(log_risk_score ~ AV + share_18to34 + share_35to54 + share_male +
                   Anthem + Blue_Shield + Health_Net + Molina + LA_Care +
                   SHARP + Chinese_Community + Oscar + Western + Valley,
                 data = rs_valid, weights = rs_valid$EXP_MM)
  } else {
    rs_reg <- lm(log_risk_score ~ AV, data = rs_valid, weights = rs_valid$EXP_MM)
  }

  cat("  Risk score regression: N =", nrow(rs_valid),
      ", demographics =", has_demo, "\n")
  cat("  R² =", round(summary(rs_reg)$r.squared, 4), "\n")

  # Claims regression (Saltzman Eq. 18). Claims are regressed on the PREDICTED
  # risk score (the fitted values from rs_reg), not the observed one, so the
  # elasticity is estimated on the same object the FOC and counterfactual apply
  # it to; on observed data the score is noisy and the pass-through attenuates
  # below one. AV is OMITTED — it is collinear with the risk score and, if
  # included, collapses the pass-through; AV still enters the RA transfer's
  # utilization factor, so generosity is not lost. Insurer FEs are OMITTED here
  # too: they live in the risk-score equation instead (see above). The insurer
  # dummies are collinear across the two equations through the pass-through, so
  # carrying them in both left the cost-side GMM rank-deficient (NA standard
  # errors). We keep them only in the risk score, where they also move the RA
  # transfer and correct the Molina mislabel — in claims they would move predicted
  # claims but not the transfer, so they cannot do that work.
  claims_valid <- rs_valid %>%
    filter(!is.na(log_cost), is.finite(log_cost), !is.na(AV_METAL))
  claims_valid <- claims_valid %>%
    mutate(log_risk_score = predict(rs_reg, newdata = claims_valid))

  claims_reg <- lm(log_cost ~ log_risk_score + HMO + trend,
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
#' @return Tibble: plan_id, share_18to34, share_35to54, share_male,
#'   share_fpl250to400, share_fpl400plus, share_hispanic, demand

compute_demographic_shares <- function(cell_data, V, lambda) {

  dt <- as.data.table(cell_data)
  dt[, V := V]

  # Nested logit choice probabilities (same formula as compute_shares_and_elasticities)
  ins_dt <- dt[plan_id != "Uninsured"]
  out_dt <- dt[plan_id == "Uninsured"]

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

  # HH-level weight (hh_size from the choice-data builder)
  w <- if ("hh_weight" %in% names(ins_dt)) ins_dt$hh_weight else rep(1, nrow(ins_dt))
  ins_dt[, w := w]
  ins_dt[, wp := w * prob]

  # Aggregate demographic shares by plan
  # Requires perc_18to34 = perc_18to25 + perc_26to34 if only components available
  if (!"perc_18to34" %in% names(ins_dt) && "perc_18to25" %in% names(ins_dt)) {
    ins_dt[, perc_18to34 := perc_18to25 + perc_26to34]
  }

  # Predicted shares from the choice model. The risk-score spec (Saltzman Eq. 16)
  # uses age and gender (share_18to34/35to54/male); income (FPL) and hispanic are
  # still emitted for other consumers but do not enter the risk-score equation.
  demo_shares <- ins_dt[, .(
    share_18to34      = sum(wp * perc_18to34, na.rm = TRUE) / sum(wp),
    share_35to54      = sum(wp * perc_35to54, na.rm = TRUE) / sum(wp),
    share_male        = sum(wp * perc_male, na.rm = TRUE) / sum(wp),
    share_fpl250to400 = sum(wp * FPL_250to400, na.rm = TRUE) / sum(wp),
    share_fpl400plus  = sum(wp * FPL_400plus, na.rm = TRUE) / sum(wp),
    share_hispanic    = sum(wp * perc_hispanic, na.rm = TRUE) / sum(wp),
    demand            = sum(wp)
  ), by = plan_id]

  as_tibble(demo_shares)
}


# Predict risk scores -----------------------------------------------------

#' Predict plan-level risk scores given RA regression coefficients,
#' plan characteristics, and demographic shares.
#'
#' @param rs_coefs     Named coefficient vector from risk score regression
#' @param plan_chars   Tibble with plan_id, Silver, Gold, Platinum
#' @param demo_shares  Tibble from compute_demographic_shares (or NULL for base model)
#' @return Tibble: plan_id, predicted_risk_score

predict_risk_scores <- function(rs_coefs, plan_chars, demo_shares = NULL) {

  pred_data <- plan_chars

  # Merge whatever demographic shares the risk-score spec actually uses, matched by
  # name against rs_coefs, so the applied terms track estimate_ra_regressions.
  if (!is.null(demo_shares)) {
    share_cols <- intersect(names(rs_coefs), names(demo_shares))
    if (length(share_cols) > 0) {
      pred_data <- pred_data %>%
        left_join(demo_shares %>% select(plan_id, all_of(share_cols)),
                  by = "plan_id")
    }
  }

  # ln r = intercept + gamma_av * AV + every other term in rs_coefs present as a
  # column (age/gender shares under Eq. 16, plus insurer FEs if the spec adds them).
  # An aliased (NA) coefficient contributes nothing.
  log_rs <- rs_coefs[["(Intercept)"]] + rs_coefs[["AV"]] * pred_data$AV
  for (term in setdiff(names(rs_coefs), c("(Intercept)", "AV"))) {
    coef_t <- rs_coefs[[term]]
    if (!is.na(coef_t) && term %in% names(pred_data)) {
      log_rs <- log_rs + coef_t * pred_data[[term]]
    }
  }

  tibble(
    plan_id = pred_data$plan_id,
    predicted_risk_score = exp(log_rs),
    log_risk_score_hat = log_rs
  )
}


# RA transfers ------------------------------------------------------------

#' Compute budget-neutral RA transfers based on predicted risk scores
#' and current market shares. Plans with above-average risk receive
#' positive transfers; plans below pay in.
#'
#' @param predicted_risk_scores  Named vector or tibble with plan_id + predicted_risk_score
#' @param plan_shares            Named vector: plan_id → market share (among insured)
#' @param avg_premium            Scalar: average premium PMPM in market
#' @param plan_avs               Named vector: plan_id → actuarial value
#' @return Named vector of PMPM RA transfers per plan

compute_ra_transfers <- function(predicted_risk_scores, plan_shares, avg_premium, plan_avs) {

  # If tibble, extract as named vector
  if (is.data.frame(predicted_risk_scores)) {
    rs_vec <- setNames(predicted_risk_scores$predicted_risk_score,
                       predicted_risk_scores$plan_id)
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
#' @param plan_chars    Tibble with plan_id, AV, HMO, trend, insurer dummies
#' @param log_rs        Named vector of log predicted risk scores
#' @return Named vector of predicted claims PMPM

predict_claims <- function(claims_coefs, plan_chars, log_rs) {

  pn <- plan_chars$plan_id
  log_cost <- claims_coefs[["(Intercept)"]] +
    claims_coefs[["log_risk_score"]] * log_rs[pn]

  # Add AV (generosity / moral hazard) if in model
  if ("AV" %in% names(claims_coefs) && "AV" %in% names(plan_chars)) {
    log_cost <- log_cost + claims_coefs[["AV"]] * plan_chars$AV
  }
  # Add HMO if in model
  if ("HMO" %in% names(claims_coefs)) {
    log_cost <- log_cost + claims_coefs[["HMO"]] * plan_chars$HMO
  }
  # Add trend if in model
  if ("trend" %in% names(claims_coefs)) {
    log_cost <- log_cost + claims_coefs[["trend"]] * plan_chars$trend
  }
  # Add insurer FEs (NA coefficient = reference category, treat as 0). Big four +
  # the seven larger regionals; Other_Small carries no dummy (baseline).
  for (ins in c("Anthem", "Blue_Shield", "Health_Net", "Kaiser",
                "Molina", "LA_Care", "SHARP", "Chinese_Community",
                "Oscar", "Western", "Valley")) {
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
#' @param plan_chars    Tibble with plan_id, Silver, Gold, Platinum, HMO,
#'                      trend, Anthem, Blue_Shield, Health_Net, Kaiser
#' @param demo_shares   Tibble with plan_id and predicted demographic shares
#'                      (share_18to34, share_35to54, share_male,
#'                      share_fpl250to400, share_fpl400plus); NULL for AV-only
#' @param shares        Named vector of market shares (among insured)
#' @param avg_premium   Scalar average premium in market
#' @param plan_avs      Named vector of actuarial values
#' @param reins_vec     Named vector of reinsurance factors
#' @return List with mc (named vector), predicted_claims, predicted_risk_scores,
#'         ra_transfers, log_risk_score_hat

compute_mc <- function(rs_coefs, claims_coefs, plan_chars, demo_shares,
                       shares, avg_premium, plan_avs, reins_vec) {

  rs_pred <- predict_risk_scores(rs_coefs, plan_chars, demo_shares)
  log_rs <- setNames(rs_pred$log_risk_score_hat, rs_pred$plan_id)
  pred_claims <- predict_claims(claims_coefs, plan_chars, log_rs)
  ra_transfers <- compute_ra_transfers(rs_pred, shares, avg_premium, plan_avs)
  mc <- predict_mc_structural(pred_claims, ra_transfers, reins_vec)

  list(
    mc                    = mc,
    predicted_claims      = pred_claims,
    predicted_risk_scores = setNames(rs_pred$predicted_risk_score, rs_pred$plan_id),
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
  # elast_mat and own_mat are J x J, already ordered by plan_ids_cell
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