# objective.R — Joint demand+supply GMM: stacked moments, weighting, gradient.
#
# theta layout: [ beta (K = length(DEMAND_COVARS)), lambda, alpha (6 risk-score),
#                 gamma_cl (7 claims) ].  K is set by the orchestrator.
# Moment blocks (Saltzman eq. 20):
#   M0 demand score  (K+1)  — gradient of weight-normalized NLL / N_hh  (= 0 at MLE)
#   M1 risk score    (6)    — frozen rate-filing WLS moment (warm-start age shares)
#   M2 claims        (6)    — frozen rate-filing WLS moment
#   M3 pricing FOC   (9)    — recomputed at current (beta,lambda); shares/elast/demo move
# M1/M2/M3 logic is copied verbatim from 3_cost_gmm.R::compute_g_bar; M0 from
# kernels_demand.R::cell_negll_gradi. See plan jazzy-snuggling-beaver.md.

MH_LOOKUP_JOINT <- c("0.6" = 1.00, "0.7" = 1.03, "0.8" = 1.08, "0.9" = 1.15)

# tibble(term, estimate) coefs from a named beta vector + lambda (for the kernels)
make_coefs <- function(beta, lambda, demand_covars) {
  tibble(term = c(demand_covars, "lambda"), estimate = c(unname(beta), lambda))
}

# ---- per-cell parameter-free cache -----------------------------------------
build_one_cell_cache <- function(r, y, seed, plan_choice, hh_split, sample_frac,
                                  spec_build, demand_covars, reins_df, commission_lookup) {
  set.seed(seed)
  hhs <- hh_split[[paste0(r, ".", y)]]
  if (is.null(hhs) || nrow(hhs) == 0) return(NULL)
  hhs <- as.data.frame(hhs)
  plans <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans) == 0) return(NULL)

  # commission PMPM (same as 2_pricing.R:90-98)
  if (!"comm_pmpm" %in% names(plans)) {
    comm_yr <- commission_lookup %>% filter(year == !!y) %>% select(-year)
    plans <- plans %>%
      mutate(insurer_prefix = sub("_.*", "", plan_id)) %>%
      left_join(comm_yr, by = "insurer_prefix") %>%
      mutate(comm_pmpm = case_when(is.na(rate) ~ 0, is_pct ~ rate * premium, TRUE ~ rate)) %>%
      select(-insurer_prefix, -rate, -is_pct)
  }

  br <- build_structural(plans, hhs, sample_frac, spec = spec_build)
  if (is.null(br)) return(NULL)
  cell_data  <- br$cell_data
  plan_attrs <- br$plan_attrs
  plan_ids   <- sort(plan_attrs$plan_id)
  J <- length(plan_ids); if (J < 2) return(NULL)

  pa <- plan_attrs[match(plan_ids, plan_attrs$plan_id), ]
  posted_premium <- setNames(pa$premium_posted, pa$plan_id)
  plan_avs       <- setNames(pa$av, pa$plan_id)
  comm_vec       <- if ("comm_pmpm" %in% names(pa)) setNames(pa$comm_pmpm, pa$plan_id) else setNames(rep(0, J), plan_ids)

  # 2nd-cheapest silver benchmark (2_pricing.R:139-143)
  sbp <- plan_attrs[plan_attrs$metal == "Silver", ]
  sbp <- sbp[order(sbp$premium_posted), ]
  benchmark_plan <- if (nrow(sbp) == 0) NA_character_ else if (nrow(sbp) == 1) sbp$plan_id[1] else sbp$plan_id[2]

  metal <- setNames(pa$metal, pa$plan_id)[plan_ids]
  Silver <- as.integer(metal == "Silver"); Gold <- as.integer(metal == "Gold"); Platinum <- as.integer(metal == "Platinum")
  HMO <- as.integer(grepl("^KA", plan_ids)); trend <- y - 2014L
  Anthem <- as.integer(grepl("^ANT", plan_ids)); Blue_Shield <- as.integer(grepl("^BS", plan_ids)); Health_Net <- as.integer(grepl("^HN", plan_ids))

  ins_own <- sub("_.*", "", plan_ids)
  own_mat <- outer(ins_own, ins_own, "==") * 1L
  dimnames(own_mat) <- list(plan_ids, plan_ids)
  Z_cell <- cbind(1, Silver, Gold, Platinum, HMO, trend, Anthem, Blue_Shield, Health_Net)

  rf <- reins_df %>% filter(year == y)
  reins_vec <- sapply(plan_ids, function(pn) {
    v <- rf$reins_factor[rf$plan_id == pn]; if (length(v) == 0) 0 else mean(v, na.rm = TRUE)
  })

  dense <- cell_data_to_matrices(cell_data, demand_covars)
  if (is.null(dense)) return(NULL)

  list(region = r, year = y, cell_data = cell_data, plan_attrs = plan_attrs, plan_ids = plan_ids,
       benchmark_plan = benchmark_plan, posted_premium = posted_premium, plan_avs = plan_avs,
       comm_vec = comm_vec, Silver = Silver, Gold = Gold, Platinum = Platinum, HMO = HMO,
       trend = trend, Anthem = Anthem, Blue_Shield = Blue_Shield, Health_Net = Health_Net,
       own_mat = own_mat, Z_cell = Z_cell, reins_vec = reins_vec, dense = dense)
}

# ---- full cache: per-cell caches + frozen M1/M2 rate-filing matrices --------
build_joint_cache <- function(cells, cell_seeds, plan_choice, hh_split, sample_frac,
                              spec_build, demand_covars, structural_spec, reins_df,
                              commission_lookup, beta0, lambda0,
                              rate_filing_path, which_cells = NULL) {
  idx <- if (is.null(which_cells)) seq_len(nrow(cells)) else which_cells
  cat("Building joint cache for", length(idx), "cells...\n")
  cc_list <- vector("list", length(idx))
  for (j in seq_along(idx)) {
    i <- idx[j]
    cc_list[[j]] <- build_one_cell_cache(cells$region[i], cells$year[i], cell_seeds[i],
                                         plan_choice, hh_split, sample_frac, spec_build,
                                         demand_covars, reins_df, commission_lookup)
    if (j %% 20 == 0) cat("  ...", j, "/", length(idx), "\n")
  }
  cc_list <- Filter(Negate(is.null), cc_list)
  cat("  built", length(cc_list), "valid cells\n")

  # normalize demand weights globally over the built cells (estimate_demand.R::normalize_weights)
  dense_list <- lapply(cc_list, `[[`, "dense")
  dense_list <- normalize_weights(dense_list)
  for (k in seq_along(cc_list)) cc_list[[k]]$dense <- dense_list[[k]]
  N_hh <- sum(sapply(cc_list, function(cc) cc$dense$n_hh))

  # --- frozen M1/M2 rate-filing matrices (3_cost_gmm.R:22-91) ---
  # predicted plan-year age shares at warm-start (beta0,lambda0), aggregated over built cells
  pred_rows <- lapply(cc_list, function(cc) {
    V <- compute_utility(cc$cell_data, make_coefs(beta0, lambda0, demand_covars))$V
    ds <- as.data.table(compute_demographic_shares(cc$cell_data, V, lambda0))
    ds[, year := cc$year]; ds
  })
  pred_all <- rbindlist(pred_rows, fill = TRUE)
  pred_py <- pred_all[, .(share_18to34 = sum(share_18to34 * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE),
                          share_35to54 = sum(share_35to54 * demand, na.rm = TRUE) / sum(demand, na.rm = TRUE)),
                      by = .(plan_id, year)]

  rsdata <- read_csv(rate_filing_path, show_col_types = FALSE) %>%
    filter(!is.na(log_risk_score), is.finite(log_risk_score),
           !is.na(log_cost), is.finite(log_cost), EXP_MM > 0) %>%
    left_join(as.data.frame(pred_py), by = c("plan_id", "year")) %>%
    filter(!is.na(share_18to34), !is.na(share_35to54), !is.na(HMO))

  w_rf <- sqrt(rsdata$EXP_MM)
  X_rs <- as.matrix(rsdata %>% select(Silver, Gold, Platinum, share_18to34, share_35to54))
  y_rs <- rsdata$log_risk_score
  X_cl_exog <- as.matrix(rsdata %>% select(HMO, trend, Anthem, Blue_Shield, Health_Net))
  y_cl <- rsdata$log_cost
  Z_rs <- cbind(1, X_rs); Z_cl <- cbind(1, X_cl_exog)
  cat("  M1/M2 rate-filing rows:", nrow(rsdata), "\n")

  list(cells = cc_list, N_hh = N_hh, structural_spec = structural_spec, demand_covars = demand_covars,
       K = length(demand_covars),
       w_rf = w_rf, X_rs = X_rs, y_rs = y_rs, X_cl_exog = X_cl_exog, y_cl = y_cl, Z_rs = Z_rs, Z_cl = Z_cl)
}

# ---- parse theta ------------------------------------------------------------
parse_theta <- function(theta, K) {
  list(beta = theta[1:K], lambda = theta[K + 1],
       alpha = theta[(K + 2):(K + 7)], gamma_cl = theta[(K + 8):(K + 14)])
}

# ---- EXPENSIVE per-cell kernel outputs at (beta,lambda) ---------------------
kernel_outputs <- function(beta, lambda, JC, cell_idx = NULL) {
  cells <- JC$cells; if (!is.null(cell_idx)) cells <- cells[cell_idx]
  coefs_full <- make_coefs(beta, lambda, JC$demand_covars)
  lapply(cells, function(cc) {
    pid <- cc$plan_ids
    V <- compute_utility(cc$cell_data, coefs_full)$V
    se <- tryCatch(compute_shares_and_elasticities(cc$cell_data, V, lambda, cc$benchmark_plan,
                                                   cc$plan_attrs, coefs_full, spec = JC$structural_spec),
                   error = function(e) NULL)
    if (is.null(se)) return(NULL)
    br <- tryCatch(compute_broker_shares_and_elasticities(cc$cell_data, V, lambda, cc$benchmark_plan,
                                                          cc$plan_attrs, coefs_full, spec = JC$structural_spec),
                   error = function(e) NULL)
    demo <- as.data.frame(compute_demographic_shares(cc$cell_data, V, lambda))
    d18 <- sapply(pid, function(p){v<-demo$share_18to34[demo$plan_id==p]; if(length(v)==0) mean(demo$share_18to34,na.rm=TRUE) else v[1]})
    d35 <- sapply(pid, function(p){v<-demo$share_35to54[demo$plan_id==p]; if(length(v)==0) mean(demo$share_35to54,na.rm=TRUE) else v[1]})
    belast <- if (!is.null(br)) br$broker_elast_mat[pid, pid] else matrix(0, length(pid), length(pid), dimnames=list(pid,pid))
    list(shares = se$shares[pid], elast = se$elast_mat[pid, pid], belast = belast, d18 = d18, d35 = d35)
  })
}

# ---- M0 demand score (uses dense matrices; cheap relative to kernels) -------
m0_score <- function(beta, lambda, JC, cell_idx = NULL) {
  cells <- JC$cells; if (!is.null(cell_idx)) cells <- cells[cell_idx]
  K1 <- JC$K + 1L
  g <- numeric(K1)
  for (cc in cells) {
    res <- cell_negll_gradi(beta, lambda, cc$dense)
    g <- g + res$grad
  }
  g / JC$N_hh
}

# ---- stacked moments given precomputed kernel outputs -----------------------
moments_from_kernels <- function(theta, JC, kout, cell_idx = NULL, include_m0 = TRUE) {
  p <- parse_theta(theta, JC$K)
  cells <- JC$cells; if (!is.null(cell_idx)) cells <- cells[cell_idx]

  # M0
  g0 <- if (include_m0) m0_score(p$beta, p$lambda, JC, cell_idx) else numeric(JC$K + 1L)

  # M1 / M2 (frozen rate-filing matrices) — 3_cost_gmm.R:193-201
  a <- p$alpha; gcl <- p$gamma_cl
  pred_log_rs_rf <- a[1] + JC$X_rs %*% a[2:6]
  g_rs <- colMeans(JC$Z_rs * as.vector((JC$y_rs - pred_log_rs_rf) * JC$w_rf))
  pred_log_cl_rf <- gcl[1] + gcl[2] * pred_log_rs_rf + JC$X_cl_exog %*% gcl[3:7]
  g_cl <- colMeans(JC$Z_cl * as.vector((JC$y_cl - pred_log_cl_rf) * JC$w_rf))

  # M3 FOC — 3_cost_gmm.R:208-261
  g_foc_sum <- rep(0, 9L); n_foc <- 0L
  for (m in seq_along(cells)) {
    cc <- cells[[m]]; ko <- kout[[m]]; if (is.null(ko)) next
    pid <- cc$plan_ids; J <- length(pid)
    shares <- ko$shares; elast <- ko$elast; own_mat <- cc$own_mat
    Omega <- -own_mat * t(elast)
    Omega_broker <- -own_mat * t(ko$belast)
    pred_log_rs <- a[1] + a[2]*cc$Silver + a[3]*cc$Gold + a[4]*cc$Platinum + a[5]*ko$d18 + a[6]*ko$d35
    pred_log_cl <- gcl[1] + gcl[2]*pred_log_rs + gcl[3]*cc$HMO + gcl[4]*cc$trend +
                   gcl[5]*cc$Anthem + gcl[6]*cc$Blue_Shield + gcl[7]*cc$Health_Net
    pred_claims <- exp(pred_log_cl); pred_rs <- exp(pred_log_rs)
    avg_p <- weighted.mean(cc$posted_premium, shares, na.rm = TRUE)
    mh <- MH_LOOKUP_JOINT[as.character(round(cc$plan_avs, 1))]; mh[is.na(mh)] <- 1.0
    util_adj <- cc$plan_avs * mh
    sum_rs_sh <- sum(pred_rs * shares, na.rm = TRUE); sum_util_sh <- sum(util_adj * shares, na.rm = TRUE)
    ra <- (pred_rs / sum_rs_sh - util_adj / sum_util_sh) * avg_p
    mc <- pred_claims * (1 - cc$reins_vec) - ra
    ra_foc <- compute_ra_foc(setNames(pred_rs, pid), shares, cc$plan_avs, avg_p, elast, own_mat)
    foc_resid <- shares + ra_foc - as.vector(Omega %*% (cc$posted_premium - mc)) +
                 as.vector(Omega_broker %*% cc$comm_vec)
    g_foc_sum <- g_foc_sum + colSums(cc$Z_cell * foc_resid); n_foc <- n_foc + J
  }
  g_foc <- if (n_foc > 0) g_foc_sum / n_foc else g_foc_sum

  c(g0, g_rs, g_cl, g_foc)
}

# ---- full moment vector (recomputes kernels) --------------------------------
g_joint <- function(theta, JC, cell_idx = NULL, include_m0 = TRUE) {
  p <- parse_theta(theta, JC$K)
  kout <- kernel_outputs(p$beta, p$lambda, JC, cell_idx)
  moments_from_kernels(theta, JC, kout, cell_idx, include_m0)
}

# ---- block-diagonal Step-1 weighting (g0 at theta0) -------------------------
BLOCK_WEIGHTS <- c(1, 1, 1, 1)
block_sizes <- function(JC) c(JC$K + 1L, 6L, 6L, 9L)
build_W1 <- function(g_theta0, JC, block_weights = BLOCK_WEIGHTS) {
  bs <- block_sizes(JC); ends <- cumsum(bs); starts <- c(1, head(ends, -1) + 1)
  blocks <- lapply(seq_along(bs), function(b) {
    gb <- g_theta0[starts[b]:ends[b]]; v <- max(sum(gb^2), 1e-20)
    diag(bs[b]) * (block_weights[b] / v)
  })
  as.matrix(Matrix::bdiag(blocks))
}

Q_joint <- function(theta, JC, W, cell_idx = NULL, include_m0 = TRUE) {
  g <- g_joint(theta, JC, cell_idx, include_m0)
  as.numeric(t(g) %*% W %*% g)
}

# ---- hand-split numerical gradient of Q -------------------------------------
# gamma-direction (alpha, gamma_cl) perturbations reuse the base kernel outputs
# (shares/elast/demo depend only on beta,lambda); beta/lambda directions re-run kernels.
grad_Q_split <- function(theta, JC, W, cell_idx = NULL, eps = 1e-6) {
  K <- JC$K
  p <- parse_theta(theta, K)
  kout0 <- kernel_outputs(p$beta, p$lambda, JC, cell_idx)
  g0 <- moments_from_kernels(theta, JC, kout0, cell_idx)
  Q0 <- as.numeric(t(g0) %*% W %*% g0)
  np <- length(theta); gr <- numeric(np)
  betalambda_idx <- 1:(K + 1)
  for (i in seq_len(np)) {
    th <- theta; th[i] <- th[i] + eps
    if (i %in% betalambda_idx) {
      pp <- parse_theta(th, K)
      ko <- kernel_outputs(pp$beta, pp$lambda, JC, cell_idx)   # expensive
      gi <- moments_from_kernels(th, JC, ko, cell_idx)
    } else {
      gi <- moments_from_kernels(th, JC, kout0, cell_idx)      # cheap: reuse kernels
    }
    Qi <- as.numeric(t(gi) %*% W %*% gi)
    gr[i] <- (Qi - Q0) / eps
  }
  gr
}

# ---- optimal block-diagonal weighting: W = bdiag(bw_b * solve(S_b)) ----------
# S_b = sampling variance of moment block b at theta:
#   M0: BHHH (outer product of per-HH demand scores) / N_hh^2
#   M1/M2: robust (Z*eps)'(Z*eps) / n^2  (Z'diag(eps^2)Z / n^2)
#   M3: cluster-by-cell OPG of the FOC moment contributions / n_foc^2
# block_weights multiplies each inverse block (down-weight M0 to let beta
# co-adjust; default c(1,1,1,1) = pure two-step optimal GMM). Unlike build_W1
# (which normalizes by the theta0 moment value and explodes when a block is ~0
# at a warm start), this weights by statistical precision.
build_W2 <- function(theta, JC, kout0 = NULL, block_weights = BLOCK_WEIGHTS, ridge = 1e-8) {
  p <- parse_theta(theta, JC$K); a <- p$alpha; gcl <- p$gamma_cl

  # S0 — demand score variance (BHHH / OPG)
  K1 <- JC$K + 1L; S0 <- matrix(0, K1, K1)
  for (cc in JC$cells) {
    gi <- cell_negll_gradi(p$beta, p$lambda, cc$dense)$gradi
    S0 <- S0 + crossprod(gi)
  }
  S0 <- S0 / (JC$N_hh^2)

  # S1, S2 — robust rate-filing moment variances
  prx <- a[1] + JC$X_rs %*% a[2:6]; e1 <- as.vector((JC$y_rs - prx) * JC$w_rf)
  S1 <- crossprod(JC$Z_rs * e1) / (length(e1)^2)
  pcl <- gcl[1] + gcl[2] * prx + JC$X_cl_exog %*% gcl[3:7]; e2 <- as.vector((JC$y_cl - pcl) * JC$w_rf)
  S2 <- crossprod(JC$Z_cl * e2) / (length(e2)^2)

  # S3 — FOC moment variance, clustered by cell
  if (is.null(kout0)) kout0 <- kernel_outputs(p$beta, p$lambda, JC)
  S3 <- matrix(0, 9, 9); n_foc <- 0L
  for (m in seq_along(JC$cells)) {
    cc <- JC$cells[[m]]; ko <- kout0[[m]]; if (is.null(ko)) next
    pid <- cc$plan_ids; shares <- ko$shares; elast <- ko$elast; own_mat <- cc$own_mat
    Omega <- -own_mat * t(elast); Omega_broker <- -own_mat * t(ko$belast)
    plr  <- a[1] + a[2]*cc$Silver + a[3]*cc$Gold + a[4]*cc$Platinum + a[5]*ko$d18 + a[6]*ko$d35
    pcl_c <- gcl[1] + gcl[2]*plr + gcl[3]*cc$HMO + gcl[4]*cc$trend +
             gcl[5]*cc$Anthem + gcl[6]*cc$Blue_Shield + gcl[7]*cc$Health_Net
    pred_claims <- exp(pcl_c); pred_rs <- exp(plr)
    avg_p <- weighted.mean(cc$posted_premium, shares, na.rm = TRUE)
    mh <- MH_LOOKUP_JOINT[as.character(round(cc$plan_avs, 1))]; mh[is.na(mh)] <- 1.0
    util_adj <- cc$plan_avs * mh
    ra <- (pred_rs / sum(pred_rs * shares, na.rm = TRUE) -
           util_adj / sum(util_adj * shares, na.rm = TRUE)) * avg_p
    mc <- pred_claims * (1 - cc$reins_vec) - ra
    ra_foc <- compute_ra_foc(setNames(pred_rs, pid), shares, cc$plan_avs, avg_p, elast, own_mat)
    foc_resid <- shares + ra_foc - as.vector(Omega %*% (cc$posted_premium - mc)) +
                 as.vector(Omega_broker %*% cc$comm_vec)
    m_c <- colSums(cc$Z_cell * foc_resid)
    S3 <- S3 + tcrossprod(m_c); n_foc <- n_foc + length(pid)
  }
  S3 <- S3 / (n_foc^2)

  invb <- function(S, bw) bw * solve(S + ridge * diag(nrow(S)))
  as.matrix(Matrix::bdiag(invb(S0, block_weights[1]), invb(S1, block_weights[2]),
                          invb(S2, block_weights[3]), invb(S3, block_weights[4])))
}
