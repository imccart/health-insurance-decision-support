# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-26
## Description:   Cost-side helpers for the structural model: the risk-score and
##                claims regressions, plan-level risk scores predicted from the
##                demographic composition, the HHS risk adjustment transfer
##                formula pooled across the state (with each cell's own
##                contribution moving in the FOC and the counterfactual), its
##                derivative with respect to premiums and commissions, and
##                marginal cost.

# Risk score and claims regressions ---------------------------------------

#' Estimate risk score and claims regressions. Called once before the supply
#' cell loop (OLS starting values for the cost GMM).
#'
#' @param rsdata   Rate filing PUF rows (plan-year): claims, member months, metal
#'                 dummies, HMO, trend, insurer dummies, and the observed
#'                 plan-year demographic shares
#' @param rs_srrt  SRRT plan risk scores (insurer x metal x region x year) with
#'                 the observed enrollment demographic shares at that level
#' @return List with rs_reg (risk score lm), claims_reg (claims lm),
#'         rs_coefs (named vector), claims_coefs (named vector)

# Risk-score demographics: the plan's enrollment shares of members aged 0 to
# 34, male, in a family household, and non-white, each mapped to the
# per-household column the counterfactual differentiates
# (dr/dp) and the uninsured-pool score uses.
RS_DEMO_RAWCOL <- c(share_0to34 = "perc_0to34", share_male = "perc_male",
                    share_family = "family", share_minority = "perc_minority")
RS_DEMO_TERMS <- names(RS_DEMO_RAWCOL)
# Claims equation (Eq. 9): HMO, trend, big-four insurer indicators, and the
# rating-area shares of the plan-year's enrollment (region 1 the base), which
# stand in for market fixed effects.
CLAIMS_REGION_TERMS <- paste0("share_ra", 2:19)
CLAIMS_EXOG_TERMS <- c("HMO", "trend", "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
                       CLAIMS_REGION_TERMS)

estimate_ra_regressions <- function(rsdata, rs_srrt) {

  # Risk-score rows: SRRT scores, valid and weighted by member months
  rs_valid <- rs_srrt %>%
    filter(!is.na(log_risk_score), is.finite(log_risk_score), member_months > 0)

  # Risk score regression: metal tier fixed effects (bronze the base) plus the
  # plan's predicted demographic shares (RS_DEMO_TERMS). No insurer terms.
  # predict_risk_scores applies whatever terms rs_coefs holds.
  has_demo <- all(RS_DEMO_TERMS %in% names(rs_valid))
  if (has_demo) {
    rs_valid <- rs_valid %>% filter(if_all(all_of(RS_DEMO_TERMS), ~ !is.na(.x)))
    rs_reg <- lm(reformulate(c("Silver", "Gold", "Platinum", RS_DEMO_TERMS), "log_risk_score"),
                 data = rs_valid, weights = rs_valid$member_months)
  } else {
    rs_reg <- lm(log_risk_score ~ Silver + Gold + Platinum, data = rs_valid, weights = rs_valid$member_months)
  }

  cat("  Risk score regression: N =", nrow(rs_valid),
      ", demographics =", has_demo, "\n")
  cat("  R² =", round(summary(rs_reg)$r.squared, 4), "\n")

  # Claims regression: log claims on the PREDICTED log risk score (the fitted
  # values from rs_reg, the object the FOC and counterfactual apply the
  # pass-through to) and the exogenous terms in CLAIMS_EXOG_TERMS. AV is
  # omitted (collinear with the risk score; generosity still enters the RA
  # transfer through the utilization factor).
  claims_valid <- rsdata %>%
    filter(!is.na(log_cost), is.finite(log_cost), EXP_MM > 0,
           if_all(all_of(RS_DEMO_TERMS), ~ !is.na(.x)))
  for (rc in CLAIMS_REGION_TERMS) if (!rc %in% names(claims_valid)) claims_valid[[rc]] <- 0
  claims_valid <- claims_valid %>%
    mutate(across(all_of(CLAIMS_REGION_TERMS), ~ ifelse(is.na(.x), 0, .x)),
           log_risk_score = predict(rs_reg, newdata = claims_valid))

  claims_reg <- lm(reformulate(c("log_risk_score", CLAIMS_EXOG_TERMS), "log_cost"),
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

compute_demographic_shares <- function(cell_data, V, lambda, V_base = NULL) {

  # Two-part nested-logit choice probabilities (same kernel as
  # compute_shares_and_elasticities): P(insured) from the base inclusive value,
  # P(j | insured) from the full utility.
  ins_dt <- nest_inside_rows(cell_data, V, V_base, lambda)
  ins_dt[, prob := q_j]

  # HH-level weight (hh_size from the choice-data builder)
  w <- if ("hh_weight" %in% names(ins_dt)) ins_dt$hh_weight else rep(1, nrow(ins_dt))
  ins_dt[, w := w]
  ins_dt[, wp := w * prob]

  # Aggregate demographic shares by plan
  # Requires perc_18to34 = perc_18to25 + perc_26to34 if only components available
  if (!"perc_18to34" %in% names(ins_dt) && "perc_18to25" %in% names(ins_dt)) {
    ins_dt[, perc_18to34 := perc_18to25 + perc_26to34]
  }

  # Predicted shares from the choice model, one per RS_DEMO_TERMS entry (the
  # per-household column named in RS_DEMO_RAWCOL, choice-probability weighted).
  if (!"family" %in% names(ins_dt)) ins_dt[, family := as.integer(hh_size > 1L)]
  if (!"perc_0to34" %in% names(ins_dt)) ins_dt[, perc_0to34 := perc_0to17 + perc_18to34]
  if (!"perc_minority" %in% names(ins_dt))
    ins_dt[, perc_minority := perc_asian + perc_black + perc_hispanic + perc_other]
  # Per-member age rating factor of the plan's enrollees (the ARF of the
  # transfer formula): household rating factor over household size, weighted by
  # predicted members.
  if (!"rf_member" %in% names(ins_dt)) ins_dt[, rf_member := rating_factor / hh_size]
  demo_shares <- ins_dt[, c(lapply(RS_DEMO_RAWCOL, function(col) sum(wp * .SD[[col]], na.rm = TRUE) / sum(wp)),
                            list(arf = sum(wp * rf_member, na.rm = TRUE) / sum(wp), demand = sum(wp))),
                        by = plan_id, .SDcols = unname(RS_DEMO_RAWCOL)]

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

  # ln r = intercept + every term in rs_coefs present as a column of pred_data
  # (metal dummies, demographic shares). An aliased (NA) coefficient contributes
  # nothing.
  log_rs <- rep(rs_coefs[["(Intercept)"]], nrow(pred_data))
  for (term in setdiff(names(rs_coefs), "(Intercept)")) {
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

# Risk adjustment transfers ----------------------------------------------
#
# HHS transfer formula (Pope et al. 2014; 2014 Payment Notice), per member-month
# for plan j in rating region m:
#
#   T_j = Pbar * M * ( x_j / R - y_j / A )
#   x_j = r_j * IDF_j * G_m,   y_j = AV_j * ARF_j * IDF_j * G_m
#   R = sum_i n_i x_i,  A = sum_i n_i y_i,  M = sum_i n_i     (statewide, members)
#
# with Pbar the statewide average premium per member-month (less the
# administrative-cost share from 2018), r the plan liability risk score, AV the
# metal actuarial value, ARF the plan's average age rating factor, IDF the
# induced demand factor by metal, and G the region's geographic cost factor.
# The statewide sums are split into the cell's own contribution, recomputed at
# the current shares and scores, and the rest of the state held at its baseline
# (ra_env$rest). The formula is zero-sum across the state.
#
# ra_env: list(gcf, arf [named by plan], N [cell members], rest = list(R, A, M),
#              pbar) from ra_env_for_cell().

ra_cell_xy <- function(rs, plan_avs, ra_env) {
  pn <- names(rs)
  av  <- unname(plan_avs[pn])
  idf <- unname(RA_IDF_BY_AV[as.character(round(av, 1))]); idf[is.na(idf)] <- 1
  arf <- unname(ra_env$arf[pn]); arf[is.na(arf)] <- 1
  list(x = unname(rs) * idf * ra_env$gcf, y = av * arf * idf * ra_env$gcf)
}

compute_ra_transfers <- function(predicted_risk_scores, plan_shares, ra_env, plan_avs) {
  rs_vec <- if (is.data.frame(predicted_risk_scores))
    setNames(predicted_risk_scores$predicted_risk_score, predicted_risk_scores$plan_id) else predicted_risk_scores
  pn <- names(plan_shares)
  rs <- rs_vec[pn]; sh <- unname(plan_shares[pn]); N <- ra_env$N
  xy <- ra_cell_xy(rs, plan_avs, ra_env)
  R <- ra_env$rest$R + N * sum(sh * xy$x, na.rm = TRUE)
  A <- ra_env$rest$A + N * sum(sh * xy$y, na.rm = TRUE)
  M <- ra_env$rest$M + N * sum(sh, na.rm = TRUE)
  setNames(ra_env$pbar * M * (xy$x / R - xy$y / A), pn)
}

# Statewide sums from a list of cell records (year, N, shares, rs, av, arf, gcf,
# premium), and each cell's own contribution. Pbar is the member-weighted average
# posted premium, less the administrative-cost share of the year.
ra_state_totals <- function(cells) {
  own <- lapply(cells, function(cl) {
    env <- list(gcf = cl$gcf, arf = cl$arf, N = cl$N)
    xy <- ra_cell_xy(cl$rs, cl$av, env)
    sh <- unname(cl$shares[names(cl$rs)])
    data.frame(region = cl$region, year = cl$year,
               R = cl$N * sum(sh * xy$x, na.rm = TRUE), A = cl$N * sum(sh * xy$y, na.rm = TRUE),
               M = cl$N * sum(sh, na.rm = TRUE),
               PM = cl$N * sum(sh * unname(cl$premium[names(cl$rs)]), na.rm = TRUE))
  })
  own <- do.call(rbind, own)
  tot <- aggregate(cbind(R, A, M, PM) ~ year, data = own, FUN = sum)
  # Pbar: the statewide average premium CMS used (net of the admin share from
  # 2018); the model's own share-weighted posted premium is the fallback
  tot$pbar <- vapply(tot$year, ra_pbar_cms, numeric(1))
  miss <- is.na(tot$pbar)
  tot$pbar[miss] <- (tot$PM / tot$M * (1 - RA_ADMIN_SHARE[as.character(tot$year)]))[miss]
  list(totals = tot, own = own)
}

# ra_env for one cell: its GCF, ARF (from the demographic shares), members, the
# rest-of-state sums (totals less the cell's baseline contribution), and Pbar.
ra_env_for_cell <- function(region, year, N, demo_shares, totals, own = NULL) {
  tot <- totals[totals$year == year, ]
  o <- if (is.null(own)) NULL else own[own$region == region & own$year == year, ]
  rest <- if (is.null(o) || nrow(o) == 0) list(R = tot$R, A = tot$A, M = tot$M) else
    list(R = tot$R - o$R, A = tot$A - o$A, M = tot$M - o$M)
  list(gcf = ra_gcf(region, year), N = N, pbar = tot$pbar, rest = rest,
       arf = setNames(demo_shares$arf, demo_shares$plan_id))
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

  # Every other term in claims_coefs present as a column of plan_chars (HMO,
  # trend, insurer indicators, rating-area shares). An aliased (NA) coefficient
  # contributes nothing.
  for (term in setdiff(names(claims_coefs), c("(Intercept)", "log_risk_score"))) {
    coef_t <- claims_coefs[[term]]
    if (!is.na(coef_t) && term %in% names(plan_chars)) {
      log_cost <- log_cost + coef_t * plan_chars[[term]]
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
#' @param ra_env        Transfer-formula environment for the cell (ra_env_for_cell)
#' @param plan_avs      Named vector of actuarial values
#' @param reins_vec     Named vector of reinsurance factors
#' @return List with mc (named vector), predicted_claims, predicted_risk_scores,
#'         ra_transfers, log_risk_score_hat

compute_mc <- function(rs_coefs, claims_coefs, plan_chars, demo_shares,
                       shares, ra_env, plan_avs, reins_vec) {

  rs_pred <- predict_risk_scores(rs_coefs, plan_chars, demo_shares)
  log_rs <- setNames(rs_pred$log_risk_score_hat, rs_pred$plan_id)
  pred_claims <- predict_claims(claims_coefs, plan_chars, log_rs)
  # the cell's current ARF travels with its demographic shares
  ra_env$arf <- setNames(demo_shares$arf, demo_shares$plan_id)
  ra_transfers <- compute_ra_transfers(rs_pred, shares, ra_env, plan_avs)
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
#' @param ra_env       Transfer-formula environment for the cell (ra_env_for_cell)
#' @param elast_mat    J x J elasticity matrix (ds_j/dp_l)
#' @param own_mat      J x J ownership matrix (1 if same firm)
#' @return Named J-vector of RA FOC contributions

compute_ra_foc <- function(risk_scores, shares, plan_avs, ra_env,
                           elast_mat, own_mat) {

  pn <- names(shares)
  J <- length(pn)
  rs <- unname(risk_scores[pn])
  sh <- unname(shares[pn])
  N <- ra_env$N
  xy <- ra_cell_xy(setNames(rs, pn), plan_avs, ra_env)
  x <- xy$x; y <- xy$y
  R <- ra_env$rest$R + N * sum(sh * x, na.rm = TRUE)
  A <- ra_env$rest$A + N * sum(sh * y, na.rm = TRUE)
  M <- ra_env$rest$M + N * sum(sh, na.rm = TRUE)
  P <- ra_env$pbar

  # T_k = P M (x_k/R - y_k/A) with R, A, M moving through the cell's own shares:
  # dT_k/ds_m = P [ N (x_k/R - y_k/A) - M N (x_k x_m / R^2 - y_k y_m / A^2) ]
  dRA_ds <- P * (N * outer(x / R - y / A, rep(1, J)) -
                 M * N * (outer(x, x) / R^2 - outer(y, y) / A^2))

  # dRA_k/dp_l = sum_m dRA_k/ds_m * ds_m/dp_l
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