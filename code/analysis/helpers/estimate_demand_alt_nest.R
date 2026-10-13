# estimate_demand_alt_nest.R — three-level nested logit (enroll, channel, plan)
#
# The alternative to the two-part model in estimate_demand.R: the household
# chooses its channel on the channel's menu value as well as its covariates.
#   V^c = V_base + add_c                      (state utilities as in estimate_demand.R)
#   I^c = logsumexp(V^c / lambda)
#   U_0 = lambda I^0;  U_N = z'gamma_N + lambda I^N;  U_A = z'gamma_A + lambda I^A
#     z = the first-stage covariates (agent density, demographic shares, FPL
#         group, household size, year effects), household level
#   P(c | enroll) = softmax(U_c / lambda_C)
#   I_E = lambda_C logsumexp(U / lambda_C)                 (standard nest)
#   I_E = lambda_C logsumexp(U / lambda_C) - lambda_C logsumexp(W / lambda_C)
#                                                           (normalize = TRUE)
#     W = (0, z'gamma_N, z'gamma_A)
#   P(enroll) = exp(I_E) / (exp(I_E) + exp(V_0))
# With normalize = TRUE the channel covariates set channel use but are not a
# source of value from enrolling, so the enrollment equation is the two-part
# model's: I_E = lambda_C ln sum_c pi_c exp(lambda I^c / lambda_C), pi_c the
# channel logit in z, which tends to lambda sum_c pi_c I^c as lambda_C grows.
# Enrollment shifters inside_a10k (agent density) and inside_yrYYYY (year
# effects) are built from the inside indicator when the covariates name them.
# Enrollee likelihood: P(enroll) P(c* | enroll) P(j* | c*); uninsured:
# 1 - P(enroll).
#
# theta = (beta[1..K], lambda, lambda_C, gamma_N[1..Q], gamma_A[1..Q]).
# Uses hh_max, nest_iv, normalize_weights, CH_TERMS, CH_TERMS_OPT from
# estimate_demand.R.

AN_YEAR_TERMS <- paste0("yr", 2015:2019)
AN_Z_TERMS <- c("one", "agents10k", "perc_0to17", "perc_18to34", "perc_35to54",
                "perc_male", "perc_black", "perc_hispanic", "perc_asian", "perc_other",
                "FPL_250to400", "FPL_400plus", "hh_size", AN_YEAR_TERMS)
AN_Q <- length(AN_Z_TERMS)
AN_LAMC_MAX <- 100

an_load_one_cell <- function(path, covars, a10k, year, sub_frac, seed) {
  raw_cols <- c("inside", "av", "premium", "comm_hh", "broker", "nonbroker",
                "p_none_hat", "p_nav_hat", "p_agent_hat",
                setdiff(AN_Z_TERMS, c("one", "agents10k", AN_YEAR_TERMS)))
  needed <- unique(c("household_number", "plan_id", "choice", "hh_weight", covars, raw_cols))
  header <- names(fread(path, nrows = 0L))
  df <- fread(path, select = intersect(needed, header))
  setorder(df, household_number, plan_id)
  if ("inside_a10k" %in% covars) df[, inside_a10k := inside * a10k]
  for (yt in intersect(paste0("inside_", AN_YEAR_TERMS), covars))
    df[, (yt) := inside * as.numeric(paste0("inside_yr", year) == yt)]

  K <- length(covars)
  n_rows <- nrow(df)
  X_full <- matrix(0, nrow = n_rows, ncol = K)
  for (k in seq_along(covars)) {
    if (covars[k] %in% names(df)) {
      vals <- as.numeric(df[[covars[k]]])
      vals[is.na(vals)] <- 0
      X_full[, k] <- vals
    }
  }
  raw <- sapply(c("av", "premium", "comm_hh"), function(cn) {
    v <- if (cn %in% names(df)) as.numeric(df[[cn]]) else numeric(n_rows)
    v[is.na(v)] <- 0
    v
  })
  for (cn in c("broker", "nonbroker")) if (!cn %in% names(df)) df[, (cn) := 0]

  df[, row_idx := .I]
  df[, is_unins := (plan_id == "Uninsured")]
  hh_summary <- df[, .(
    has_ins    = any(!is_unins),
    has_unins  = any(is_unins),
    has_choice = any(choice == 1L),
    unins_idx  = row_idx[is_unins][1],
    chosen_idx = row_idx[choice == 1L][1],
    chose_insured = (plan_id[choice == 1L][1] != "Uninsured"),
    weight = hh_weight[1],
    p0 = p_none_hat[1], pN = p_nav_hat[1], pA = p_agent_hat[1],
    ch_state = if (any(broker == 1, na.rm = TRUE)) 2L
               else if (any(nonbroker == 1, na.rm = TRUE)) 1L else 0L
  ), by = household_number]

  valid_hh <- hh_summary[has_ins == TRUE & has_unins == TRUE & has_choice == TRUE]
  if (sub_frac < 1 && nrow(valid_hh) > 0) {
    set.seed(seed)
    keep <- sort(sample.int(nrow(valid_hh), max(1L, round(sub_frac * nrow(valid_hh)))))
    valid_hh <- valid_hh[keep]
  }
  n_hh <- nrow(valid_hh)
  if (n_hh == 0) return(NULL)
  valid_hh[, hh_idx := .I]

  ins_mask <- df$plan_id != "Uninsured" & df$household_number %in% valid_hh$household_number
  hh_lookup <- setNames(valid_hh$hh_idx, as.character(valid_hh$household_number))

  # Channel covariates from the uninsured row, where the household demographics
  # are unchanged
  Z <- matrix(0, n_hh, AN_Q)
  Z[, 1] <- 1
  Z[, 2] <- a10k
  for (q in 3:AN_Q) {
    cn <- AN_Z_TERMS[q]
    if (cn %in% AN_YEAR_TERMS) {
      Z[, q] <- as.numeric(paste0("yr", year) == cn)
    } else if (cn %in% names(df)) {
      v <- as.numeric(df[[cn]][valid_hh$unins_idx])
      v[is.na(v)] <- 0
      Z[, q] <- v
    }
  }

  list(X_ins = X_full[ins_mask, , drop = FALSE],
       X_0 = X_full[valid_hh$unins_idx, , drop = FALSE],
       X_ch = X_full[valid_hh$chosen_idx, , drop = FALSE],
       hh_id = as.integer(hh_lookup[as.character(df$household_number[ins_mask])]),
       n_hh = n_hh,
       chose_ins = valid_hh$chose_insured, wt = valid_hh$weight,
       av_ins = raw[ins_mask, "av"], prem_ins = raw[ins_mask, "premium"],
       comm_ins = raw[ins_mask, "comm_hh"],
       ch_state = valid_hh$ch_state,
       p0 = valid_hh$p0, pN = valid_hh$pN, pA = valid_hh$pA,
       Z = Z)
}

# Cells as in s2_demand.R, sub_frac of the households in each; agent density
# (standardized) by region-year
an_load_cells <- function(cell_dir, covars, ext_exclude, a10k_lookup, sub_frac, seed0) {
  files <- sort(list.files(cell_dir, pattern = "^cell_.*_data\\.csv$", full.names = TRUE))
  cells <- list()
  for (i in seq_along(files)) {
    m <- regmatches(basename(files[i]), regexec("^cell_(.+)_(\\d{4})_data\\.csv$", basename(files[i])))[[1]]
    key <- paste(m[2], m[3], sep = "_")
    cl <- an_load_one_cell(files[i], covars, a10k = a10k_lookup[[key]], year = as.integer(m[3]),
                           sub_frac = sub_frac, seed = seed0 + i)
    if (!is.null(cl)) cells[[length(cells) + 1]] <- cl
  }
  cells <- normalize_weights(cells)
  excl_idx <- match(ext_exclude, covars)
  excl_idx <- excl_idx[!is.na(excl_idx)]
  ch_idx <- c(setNames(match(CH_TERMS, covars), names(CH_TERMS)),
              setNames(match(CH_TERMS_OPT, covars), names(CH_TERMS_OPT)))
  for (ci in seq_along(cells)) {
    cells[[ci]]$excl_idx <- excl_idx
    cells[[ci]]$ch_idx <- ch_idx
  }
  cells
}

an_split_theta <- function(theta, K) {
  list(beta = theta[1:K], lambda = theta[K + 1], lamC = theta[K + 2],
       gN = theta[K + 2 + seq_len(AN_Q)],
       gA = theta[K + 2 + AN_Q + seq_len(AN_Q)])
}

an_cell_ll_pieces <- function(tp, cell, normalize) {
  n_hh <- cell$n_hh; hh_id <- cell$hh_id
  excl <- cell$excl_idx; ch <- cell$ch_idx
  beta <- tp$beta; lambda <- tp$lambda; lamC <- tp$lamC

  V_ins <- as.numeric(cell$X_ins %*% beta)
  V_0   <- as.numeric(cell$X_0 %*% beta)
  V_ch  <- as.numeric(cell$X_ch %*% beta)
  V_base <- V_ins - as.numeric(cell$X_ins[, excl, drop = FALSE] %*% beta[excl])
  full <- nest_iv(V_ins, lambda, hh_id, n_hh)

  add_N <- beta[ch["nav_av"]] * cell$av_ins + beta[ch["nav_pr"]] * cell$prem_ins
  add_A <- beta[ch["brk_av"]] * cell$av_ins + beta[ch["brk_pr"]] * cell$prem_ins +
           beta[ch["comm"]] * cell$comm_ins
  if (!is.na(ch["comm2"])) add_A <- add_A + beta[ch["comm2"]] * cell$comm_ins^2 / 100
  st0 <- nest_iv(V_base, lambda, hh_id, n_hh)
  stN <- nest_iv(V_base + add_N, lambda, hh_id, n_hh)
  stA <- nest_iv(V_base + add_A, lambda, hh_id, n_hh)

  WN <- as.numeric(cell$Z %*% tp$gN)
  WA <- as.numeric(cell$Z %*% tp$gA)
  U_0 <- lambda * st0$I
  U_N <- WN + lambda * stN$I
  U_A <- WA + lambda * stA$I

  a0 <- U_0 / lamC; aN <- U_N / lamC; aA <- U_A / lamC
  mxU <- pmax(a0, aN, aA)
  e0 <- exp(a0 - mxU); eN <- exp(aN - mxU); eA <- exp(aA - mxU)
  sumU <- e0 + eN + eA
  g0 <- e0 / sumU; gN <- eN / sumU; gA <- eA / sumU
  I_E_raw <- lamC * (mxU + log(sumU))

  if (normalize) {
    wN <- WN / lamC; wA <- WA / lamC
    mxW <- pmax(0, wN, wA)
    f0 <- exp(-mxW); fN <- exp(wN - mxW); fA <- exp(wA - mxW)
    sumW <- f0 + fN + fA
    hN <- fN / sumW; hA <- fA / sumW
    S <- lamC * (mxW + log(sumW))
    Wbar <- hN * WN + hA * WA
  } else {
    hN <- hA <- S <- Wbar <- 0
  }
  I_E <- I_E_raw - S

  mx_d <- pmax(I_E, V_0)
  log_denom <- mx_d + log(exp(I_E - mx_d) + exp(V_0 - mx_d))
  P_ins <- exp(I_E - log_denom)
  log_g_star <- ifelse(cell$ch_state == 2L, log(pmax(gA, 1e-300)),
                ifelse(cell$ch_state == 1L, log(pmax(gN, 1e-300)), log(pmax(g0, 1e-300))))
  ll_ins <- V_ch / lambda - full$I + log_g_star + I_E - log_denom
  ll_unins <- V_0 - log_denom

  list(V_ins = V_ins, V_base = V_base, V_ch = V_ch, V_0 = V_0,
       add_N = add_N, add_A = add_A,
       s_f = full$s, s_0 = st0$s, s_N = stN$s, s_A = stA$s,
       I_full = full$I, I_0 = st0$I, I_N = stN$I, I_A = stA$I,
       WN = WN, WA = WA, U_0 = U_0, U_N = U_N, U_A = U_A,
       g0 = g0, gN = gN, gA = gA, I_E = I_E, I_E_raw = I_E_raw,
       hN = hN, hA = hA, S = S, Wbar = Wbar,
       P_ins = P_ins, ll_h = ifelse(cell$chose_ins, ll_ins, ll_unins))
}

an_cell_grad_pieces <- function(tp, cell, pc) {
  hh_id <- cell$hh_id; excl <- cell$excl_idx; ch <- cell$ch_idx
  lambda <- tp$lambda; lamC <- tp$lamC
  P_ins <- pc$P_ins; ins <- cell$chose_ins; cs <- cell$ch_state

  x_bar_f <- rowsum(pc$s_f * cell$X_ins, hh_id, reorder = FALSE)
  V_bar_f <- as.numeric(rowsum(pc$s_f * pc$V_ins, hh_id, reorder = FALSE))

  xb0 <- rowsum(pc$s_0 * cell$X_ins, hh_id, reorder = FALSE)
  xb0[, excl] <- 0
  xbN <- rowsum(pc$s_N * cell$X_ins, hh_id, reorder = FALSE)
  xbN[, excl] <- 0
  xbN[, ch["nav_av"]] <- rowsum(pc$s_N * cell$av_ins, hh_id, reorder = FALSE)
  xbN[, ch["nav_pr"]] <- rowsum(pc$s_N * cell$prem_ins, hh_id, reorder = FALSE)
  xbA <- rowsum(pc$s_A * cell$X_ins, hh_id, reorder = FALSE)
  xbA[, excl] <- 0
  xbA[, ch["brk_av"]] <- rowsum(pc$s_A * cell$av_ins, hh_id, reorder = FALSE)
  xbA[, ch["brk_pr"]] <- rowsum(pc$s_A * cell$prem_ins, hh_id, reorder = FALSE)
  xbA[, ch["comm"]]   <- rowsum(pc$s_A * cell$comm_ins, hh_id, reorder = FALSE)
  if (!is.na(ch["comm2"]))
    xbA[, ch["comm2"]] <- rowsum(pc$s_A * cell$comm_ins^2 / 100, hh_id, reorder = FALSE)

  xbar_bar <- pc$g0 * xb0 + pc$gN * xbN + pc$gA * xbA
  x_bar_star <- xb0
  x_bar_star[cs == 1L, ] <- xbN[cs == 1L, , drop = FALSE]
  x_bar_star[cs == 2L, ] <- xbA[cs == 2L, , drop = FALSE]

  Vb0 <- as.numeric(rowsum(pc$s_0 * pc$V_base, hh_id, reorder = FALSE))
  VbN <- as.numeric(rowsum(pc$s_N * (pc$V_base + pc$add_N), hh_id, reorder = FALSE))
  VbA <- as.numeric(rowsum(pc$s_A * (pc$V_base + pc$add_A), hh_id, reorder = FALSE))
  dU0_lam <- pc$I_0 - Vb0 / lambda
  dUN_lam <- pc$I_N - VbN / lambda
  dUA_lam <- pc$I_A - VbA / lambda
  dIE_lam <- pc$g0 * dU0_lam + pc$gN * dUN_lam + pc$gA * dUA_lam
  dU_star_lam <- ifelse(cs == 2L, dUA_lam, ifelse(cs == 1L, dUN_lam, dU0_lam))

  U_bar <- pc$g0 * pc$U_0 + pc$gN * pc$U_N + pc$gA * pc$U_A
  U_star <- ifelse(cs == 2L, pc$U_A, ifelse(cs == 1L, pc$U_N, pc$U_0))
  diff_xbar_x0 <- xbar_bar - cell$X_0

  g_beta_ins <- (cell$X_ch - x_bar_f) / lambda + (x_bar_star - xbar_bar) / lamC +
                (1 - P_ins) * diff_xbar_x0
  g_beta_h <- ifelse(ins, 1, 0) * g_beta_ins + ifelse(!ins, 1, 0) * (-P_ins * diff_xbar_x0)

  g_lam_h <- ifelse(ins,
                    -(pc$V_ch - V_bar_f) / lambda^2 + (dU_star_lam - dIE_lam) / lamC +
                      (1 - P_ins) * dIE_lam,
                    -P_ins * dIE_lam)

  dIE_lc <- (pc$I_E_raw - U_bar) / lamC - (pc$S - pc$Wbar) / lamC
  g_lc_h <- ifelse(ins, -(U_star - U_bar) / lamC^2 + (1 - P_ins) * dIE_lc, -P_ins * dIE_lc)

  # d I_E / d gamma_c = (g_c - h_c) z
  sN_h <- ifelse(ins, (as.numeric(cs == 1L) - pc$gN) / lamC + (1 - P_ins) * (pc$gN - pc$hN),
                 -P_ins * (pc$gN - pc$hN))
  sA_h <- ifelse(ins, (as.numeric(cs == 2L) - pc$gA) / lamC + (1 - P_ins) * (pc$gA - pc$hA),
                 -P_ins * (pc$gA - pc$hA))

  list(g_beta_h = g_beta_h, g_lam_h = g_lam_h, g_lc_h = g_lc_h,
       g_gN_h = sN_h * cell$Z, g_gA_h = sA_h * cell$Z)
}

an_accumulate <- function(theta, cells, K, normalize, compute_grad = TRUE) {
  tp <- an_split_theta(theta, K)
  negll <- 0
  grad <- if (compute_grad) numeric(length(theta)) else NULL
  for (cell in cells) {
    pc <- an_cell_ll_pieces(tp, cell, normalize)
    negll <- negll - sum(cell$wt * pc$ll_h)
    if (compute_grad) {
      gp <- an_cell_grad_pieces(tp, cell, pc)
      grad <- grad - c(as.numeric(crossprod(cell$wt, gp$g_beta_h)), sum(cell$wt * gp$g_lam_h),
                       sum(cell$wt * gp$g_lc_h), as.numeric(crossprod(cell$wt, gp$g_gN_h)),
                       as.numeric(crossprod(cell$wt, gp$g_gA_h)))
    }
  }
  list(negll = negll, grad = grad)
}

an_bfgs_bhhh <- function(theta, cells, K, normalize, max_iter = 500, ftol = 1e-8, stptol = 1e-10) {
  tp <- an_split_theta(theta, K)
  negll <- 0; g <- numeric(length(theta))
  bhhh <- matrix(0, length(theta), length(theta))
  for (cell in cells) {
    pc <- an_cell_ll_pieces(tp, cell, normalize)
    gp <- an_cell_grad_pieces(tp, cell, pc)
    gi <- cbind(gp$g_beta_h, gp$g_lam_h, gp$g_lc_h, gp$g_gN_h, gp$g_gA_h) * (-cell$wt)
    negll <- negll - sum(cell$wt * pc$ll_h)
    g <- g + colSums(gi)
    bhhh <- bhhh + crossprod(gi)
  }
  Hm1 <- tryCatch(solve(bhhh), error = function(e) diag(length(theta)))

  for (iter in seq_len(max_iter)) {
    old_negll <- negll; old_g <- g
    d <- as.numeric(-Hm1 %*% g)
    step <- 2
    repeat {
      step <- step / 2
      if (step < stptol) break
      theta_try <- theta + step * d
      if (theta_try[K + 1] <= 0.001 || theta_try[K + 1] >= 5) next
      if (theta_try[K + 2] <= 0.001 || theta_try[K + 2] >= AN_LAMC_MAX) next
      if (an_accumulate(theta_try, cells, K, normalize, FALSE)$negll <= old_negll) break
    }
    if (step < stptol) break
    theta <- theta + step * d
    acc <- an_accumulate(theta, cells, K, normalize, TRUE)
    negll <- acc$negll; g <- acc$grad
    incr <- step * d; y <- g - old_g; sy <- sum(incr * y)
    if (sy > 1e-10) {
      Hy <- as.numeric(Hm1 %*% y)
      Hm1 <- Hm1 + (sy + sum(y * Hy)) / sy^2 * outer(incr, incr) -
        (outer(Hy, incr) + outer(incr, Hy)) / sy
    }
    chi2 <- -sum(d * old_g)
    if (iter %% 5 == 0)
      cat(sprintf("  alt nest iter %3d: negLL %.2f  lambda %.4f  lambda_C %.4f  chi2 %.2f\n",
                  iter, negll, theta[K + 1], theta[K + 2], chi2))
    if (abs(negll - old_negll) < ftol || abs(chi2) < 1e-6) break
  }
  list(theta = theta, negll = negll, iter = iter)
}

# Warm start: plan terms and lambda from the two-part estimates; channel
# intercepts from the enrollee channel shares
estimate_demand_alt_nest <- function(cells, covars, start_coefs, normalize, lamC0 = 0.5) {
  K <- length(covars)
  n_by <- c(0, 0, 0)
  for (cl in cells) for (s in 0:2) n_by[s + 1] <- n_by[s + 1] + sum(cl$chose_ins & cl$ch_state == s)
  theta <- c(rep(0, K), 1, lamC0, rep(0, 2 * AN_Q))
  m <- match(covars, start_coefs$term)
  theta[1:K][!is.na(m)] <- start_coefs$estimate[m[!is.na(m)]]
  theta[K + 1] <- start_coefs$estimate[start_coefs$term == "lambda"]
  theta[K + 3] <- log(n_by[2] / n_by[1]) * lamC0
  theta[K + 3 + AN_Q] <- log(n_by[3] / n_by[1]) * lamC0
  fit <- an_bfgs_bhhh(theta, cells, K, normalize)
  data.frame(term = c(covars, "lambda", "lambda_C", paste0("chN_", AN_Z_TERMS), paste0("chA_", AN_Z_TERMS)),
             estimate = fit$theta)
}
