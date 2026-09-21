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
#'                 dummies, HMO, year dummies, insurer dummies, and the observed
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
# Household characteristics whose plan-level means enter the cost side: the
# risk-score shares and the per-member rating factor behind the transfer
# formula's ARF. The kernel weights its derivative matrices by the same columns,
# so a plan's mean of column z and the derivative of that mean line up.
MIX_COLS <- c(RS_DEMO_RAWCOL, arf = "rf_member")
add_mix_columns <- function(dt) {
  if (!"perc_18to34" %in% names(dt) && "perc_18to25" %in% names(dt))
    dt[, perc_18to34 := perc_18to25 + perc_26to34]
  if (!"family" %in% names(dt)) dt[, family := as.integer(hh_size > 1L)]
  if (!"perc_0to34" %in% names(dt)) dt[, perc_0to34 := perc_0to17 + perc_18to34]
  if (!"perc_minority" %in% names(dt))
    dt[, perc_minority := perc_asian + perc_black + perc_hispanic + perc_other]
  if (!"rf_member" %in% names(dt)) dt[, rf_member := rating_factor / hh_size]
  dt
}
# Metal effects are insurer-specific; a single platinum dummy cannot span
# observed platinum scores of 1.2 (Kaiser) to 7.3 (Health Net PPO). The
# demographic slopes stay common so the equation still responds to composition.
RS_IM_PREFIX <- c("ANT", "BS", "CC", "HN", "KA", "LA", "MOL", "OSC", "SH", "VAL", "WEST")
RS_IM_METAL  <- c("Bronze", "Silver", "Gold", "Platinum")
RS_IM_BASE   <- "im_KA_Silver"   # the largest cell, omitted for identification
RS_IM_TERMS  <- setdiff(paste0("im_", rep(RS_IM_PREFIX, each = length(RS_IM_METAL)), "_",
                               RS_IM_METAL), RS_IM_BASE)
# The supply side (s3 through cf3) runs on the years with realized claims in
# the rate filings, 2014-2018; demand is estimated on 2014-2019.
SUPPLY_YEARS <- 2014:2018
# Claims equation (Eq. 9): HMO, year dummies (2014 the base), big-four insurer
# indicators, and the rating-area shares of the plan-year's enrollment (region
# 1 the base), which stand in for market fixed effects. A carrier indicator for
# every insurer is not identified alongside the rating-area shares: a
# single-region carrier's indicator is a linear combination of them (R^2 above
# 0.99 for Chinese Community, Western, Valley and SHARP).
CLAIMS_REGION_TERMS <- paste0("share_ra", 2:19)
CLAIMS_YEAR_TERMS <- paste0("year_", SUPPLY_YEARS[-1])
CLAIMS_EXOG_TERMS <- c("HMO", CLAIMS_YEAR_TERMS, "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
                       CLAIMS_REGION_TERMS)

estimate_ra_regressions <- function(rsdata, rs_srrt) {

  # Risk-score rows: SRRT scores, valid and weighted by member months
  rs_valid <- rs_srrt %>%
    filter(!is.na(log_risk_score), is.finite(log_risk_score), member_months > 0)

  # One effect per insurer-metal cell (Kaiser silver the base) plus the plan's
  # demographic shares. An unobserved cell carries a zero, not an NA.
  rs_key <- paste0("im_", rs_valid$insurer_prefix, "_", rs_valid$metal)
  for (nm in RS_IM_TERMS) rs_valid[[nm]] <- as.integer(rs_key == nm)
  has_demo <- all(RS_DEMO_TERMS %in% names(rs_valid))
  if (has_demo) {
    rs_valid <- rs_valid %>% filter(if_all(all_of(RS_DEMO_TERMS), ~ !is.na(.x)))
    rs_reg <- lm(reformulate(c(RS_IM_TERMS, RS_DEMO_TERMS), "log_risk_score"),
                 data = rs_valid, weights = rs_valid$member_months)
  } else {
    rs_reg <- lm(reformulate(RS_IM_TERMS, "log_risk_score"),
                 data = rs_valid, weights = rs_valid$member_months)
  }
  rs_reg$coefficients[is.na(rs_reg$coefficients)] <- 0


  # Claims regression: log claims on the PREDICTED log risk score (the fitted
  # values from rs_reg, the object the FOC and counterfactual apply the
  # pass-through to) and the exogenous terms in CLAIMS_EXOG_TERMS. AV is
  # omitted (collinear with the risk score; generosity still enters the RA
  # transfer through the utilization factor).
  claims_valid <- rsdata %>%
    filter(!is.na(log_cost), is.finite(log_cost), EXP_MM > 0,
           if_all(all_of(RS_DEMO_TERMS), ~ !is.na(.x)))
  for (rc in CLAIMS_REGION_TERMS) if (!rc %in% names(claims_valid)) claims_valid[[rc]] <- 0
  cl_key <- paste0("im_", sub("_.*", "", claims_valid$plan_id), "_",
                   if ("METAL" %in% names(claims_valid)) claims_valid$METAL else claims_valid$metal)
  for (nm in RS_IM_TERMS) claims_valid[[nm]] <- as.integer(cl_key == nm)
  claims_valid <- claims_valid %>%
    mutate(across(all_of(CLAIMS_REGION_TERMS), ~ ifelse(is.na(.x), 0, .x)),
           log_risk_score = predict(rs_reg, newdata = claims_valid))

  claims_reg <- lm(reformulate(c("log_risk_score", CLAIMS_EXOG_TERMS), "log_cost"),
                   data = claims_valid, weights = claims_valid$EXP_MM)


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

compute_demographic_shares <- function(cell_data, V, lambda, V_base = NULL,
                                       add_N = NULL, add_A = NULL) {

  # Two-part nested-logit choice probabilities (same kernel as
  # compute_shares_and_elasticities): P(insured) from the expected inclusive
  # value over channel states, P(j | insured) from the full utility.
  ins_dt <- nest_inside_rows(cell_data, V, V_base, lambda, add_N, add_A)
  ins_dt[, prob := q_j]

  # HH-level weight (hh_size from the choice-data builder)
  w <- if ("hh_weight" %in% names(ins_dt)) ins_dt$hh_weight else rep(1, nrow(ins_dt))
  ins_dt[, w := w]
  ins_dt[, wp := w * prob]

  # Plan-level means of the risk-score characteristics and of the per-member
  # rating factor (the ARF of the transfer formula), weighted by predicted members
  ins_dt <- add_mix_columns(ins_dt)
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
# ra_env: list(gcf, arf [named by plan], N [cell members], nu, tpn [nu times the
#              year's premium total], rest = list(R, A), firm_rest = list(X, Y)
#              [the carrier's sums outside the cell]) from ra_env_for_cell().

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
  setNames(ra_env$tpn * (xy$x / R - xy$y / A), pn)
}

# The year's premium total from cell records at the observed premiums: premiums
# collected, posted premium times the rating-weighted share, summed over cells.
ra_premium_total <- function(cells) {
  tp <- vapply(cells, function(cl) cl$N * sum(unname(cl$premium[names(cl$rshares)]) * unname(cl$rshares),
                                              na.rm = TRUE), numeric(1))
  yr <- vapply(cells, function(cl) as.character(cl$year), character(1))
  tapply(tp, yr, sum)
}

# Statewide sums from a list of cell records (year, N, shares, rs, av, arf, gcf),
# each cell's own contribution, and the same by carrier. tp is the year's
# premium total, named by year: premiums collected at the observed premiums
# (s3 writes it), held at that value wherever the transfers are evaluated.
ra_state_totals <- function(cells, tp) {
  own <- lapply(cells, function(cl) {
    env <- list(gcf = cl$gcf, arf = cl$arf, N = cl$N)
    xy <- ra_cell_xy(cl$rs, cl$av, env)
    sh <- unname(cl$shares[names(cl$rs)])
    sx <- cl$N * sh * xy$x; sx[is.na(sx)] <- 0
    sy <- cl$N * sh * xy$y; sy[is.na(sy)] <- 0
    firm <- sub("_.*", "", names(cl$rs))
    f <- unique(firm)
    list(cell = data.frame(region = cl$region, year = cl$year, R = sum(sx), A = sum(sy)),
         firm = data.frame(region = cl$region, year = cl$year, firm = f,
                           X = as.numeric(tapply(sx, firm, sum)[f]),
                           Y = as.numeric(tapply(sy, firm, sum)[f])))
  })
  own_cell <- do.call(rbind, lapply(own, `[[`, "cell"))
  own_firm <- do.call(rbind, lapply(own, `[[`, "firm"))
  tot <- aggregate(cbind(R, A) ~ year, data = own_cell, FUN = sum)
  tot$nu <- unname(1 - RA_ADMIN_SHARE[as.character(tot$year)])
  tot$tp <- unname(tp[as.character(tot$year)])
  if (anyNA(tot$tp)) stop("ra_state_totals: no premium total for year ", paste(tot$year[is.na(tot$tp)], collapse = ", "))
  list(totals = tot, own = own_cell,
       totals_firm = aggregate(cbind(X, Y) ~ year + firm, data = own_firm, FUN = sum),
       own_firm = own_firm)
}

# ra_env for one cell: its GCF, ARF (from the demographic shares), members, the
# premium total, and the statewide and carrier sums outside the cell (totals
# less the cell's own contribution in st).
ra_env_for_cell <- function(region, year, N, demo_shares, st) {
  tot <- st$totals[st$totals$year == year, ]
  o <- st$own[st$own$region == region & st$own$year == year, ]
  rest <- if (nrow(o) == 0) list(R = tot$R, A = tot$A) else list(R = tot$R - o$R, A = tot$A - o$A)
  tf <- st$totals_firm[st$totals_firm$year == year, ]
  of <- st$own_firm[st$own_firm$region == region & st$own_firm$year == year, ]
  X <- setNames(tf$X, tf$firm); Y <- setNames(tf$Y, tf$firm)
  X[of$firm] <- X[of$firm] - of$X
  Y[of$firm] <- Y[of$firm] - of$Y
  list(gcf = ra_gcf(region, year), N = N, nu = tot$nu, tpn = tot$nu * tot$tp,
       rest = rest, firm_rest = list(X = X, Y = Y),
       arf = setNames(demo_shares$arf, demo_shares$plan_id))
}


# Predict claims ----------------------------------------------------------

#' Predict plan-level claims from risk scores and claims regression.
#'
#' @param claims_coefs  Named coefficient vector from claims regression
#' @param plan_chars    Tibble with plan_id, AV, HMO, year dummies, insurer dummies
#' @param log_rs        Named vector of log predicted risk scores
#' @return Named vector of predicted claims PMPM

predict_claims <- function(claims_coefs, plan_chars, log_rs) {

  pn <- plan_chars$plan_id
  log_cost <- claims_coefs[["(Intercept)"]] +
    claims_coefs[["log_risk_score"]] * log_rs[pn]

  # Every other term in claims_coefs present as a column of plan_chars (HMO,
  # year dummies, insurer indicators, rating-area shares). An aliased (NA) coefficient
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
#' claims → RA transfers → structural MC. Called identically by s3_pricing.R,
#' s4_cost-gmm.R, and the counterfactual cell kernel in cf_cell.R.
#'
#' @param rs_coefs      Named vector of risk score regression coefficients
#' @param claims_coefs  Named vector of claims regression coefficients
#' @param plan_chars    Tibble with plan_id, the insurer-by-metal indicators
#'                      (RS_IM_TERMS), HMO, year dummies, and the big-four
#'                      brand dummies
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

# How a plan's enrollee mix moves with each column's instrument (a premium or a
# commission scale). G[m, l] = s_m dlog r_m / dl through the risk-score shares;
# A[m, l] = s_m dlog ARF_m / dl. E is the member-weighted derivative matrix and
# zE the characteristic-weighted ones from the same kernel (supply.R).
mix_response <- function(E, zE, demo_shares, rs_coefs) {
  pn <- rownames(E)
  ds <- demo_shares[match(pn, demo_shares$plan_id), ]
  G <- matrix(0, nrow(E), ncol(E), dimnames = dimnames(E))
  for (d in RS_DEMO_TERMS) {
    g <- if (d %in% names(rs_coefs)) rs_coefs[[d]] else NA_real_
    if (is.na(g)) next
    sd <- ds[[d]]; sd[is.na(sd)] <- 0
    G <- G + g * (zE[[d]][pn, pn] - sd * E)
  }
  arf <- ds$arf; arf[is.na(arf) | arf <= 0] <- 1
  list(G = G, A = (zE[["arf"]][pn, pn] - arf * E) / arf)
}

# Claims moving with the enrollee mix, the term that separates marginal from
# average cost: cc_l = sum_k O[l,k] (1 - reins_k) claims_k mu s_k dlog r_k / dl,
# with mu the pass-through of the risk score into claims.
compute_claims_comp <- function(claims, reins, mix, own_mat, mu) {
  pn <- rownames(mix$G)
  r <- reins[pn]; r[is.na(r)] <- 0
  w <- unname((1 - r) * claims[pn]) * mu
  w[is.na(w)] <- 0
  setNames(colSums(own_mat * (w * mix$G)), pn)
}

#' The transfer's contribution to a first-order condition, per unit of the
#' cell's member weight.
#'
#' The carrier's transfer is T_f = tpn (X_f / R - Y_f / A), with X_f and Y_f its
#' statewide risk and utilization sums and tpn the premium total net of the
#' administrative share. Moving column l's instrument moves the cell's shares,
#' the risk scores and average rating factors of its plans (the mix response),
#' and with them X_f, Y_f, R and A, so the carrier's transfers in every cell
#' respond. The premium total enters the level as a constant and the derivative
#' through mkt_rev, the instrument's effect on premiums collected in the cell.
#' The part already carried by the marginal cost in Omega mc (the level T_k
#' times the share response) is netted out, so the pricing residual keeps the
#' form rshares + ra_foc - (Omega_r p - Omega mc).
#'
#' @param mix      list(G, A) from mix_response for the same derivative matrix
#' @param mkt_rev  J-vector: d(premiums collected in the cell)/dl per unit weight
#' @return list(total, feedback), named J-vectors; feedback is the mkt_rev part
compute_ra_foc <- function(risk_scores, shares, plan_avs, ra_env, elast_mat, own_mat,
                           mix, mkt_rev) {
  pn <- names(shares)
  rs <- unname(risk_scores[pn]); sh <- unname(shares[pn]); N <- ra_env$N
  xy <- ra_cell_xy(setNames(rs, pn), plan_avs, ra_env)
  x <- xy$x; x[is.na(x)] <- 0
  y <- xy$y; y[is.na(y)] <- 0
  sh[is.na(sh)] <- 0
  R <- ra_env$rest$R + N * sum(sh * x)
  A <- ra_env$rest$A + N * sum(sh * y)

  firm <- sub("_.*", "", pn)
  Xr <- ra_env$firm_rest$X[firm]; Xr[is.na(Xr)] <- 0
  Yr <- ra_env$firm_rest$Y[firm]; Yr[is.na(Yr)] <- 0
  Xf <- unname(Xr) + N * as.numeric(tapply(sh * x, firm, sum)[firm])
  Yf <- unname(Yr) + N * as.numeric(tapply(sh * y, firm, sum)[firm])

  E <- elast_mat[pn, pn]; O <- own_mat            # own_mat is in plan order, as in Omega
  dX <- x * (E + mix$G[pn, pn])          # [m, l]: plan m's part of dR / dl, per unit weight
  dY <- y * (E + mix$A[pn, pn])
  dR <- colSums(dX); dA <- colSums(dY)
  dXf <- colSums(O * dX); dYf <- colSums(O * dY)

  gap <- Xf / R - Yf / A
  dT <- ra_env$tpn * (dXf / R - Xf * dR / R^2 - dYf / A + Yf * dA / A^2)
  feedback <- ra_env$nu * mkt_rev[pn] * gap
  Tk <- ra_env$tpn * (x / R - y / A)
  level_part <- colSums(O * (Tk * E))

  list(total = setNames(dT + feedback - level_part, pn), feedback = setNames(feedback, pn))
}