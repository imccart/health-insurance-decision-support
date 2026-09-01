# se.R — Analytical sandwich standard errors for the structural estimators.
#
# Two pure functions, called from s5_se.R: demand_sandwich_se for the two-part
# nested logit and cost_gmm_sandwich_se for the cost GMM. Each returns the SE
# table AND the full parameter covariance matrix, the latter consumed by the CF
# parametric bootstrap (cf3_se.R) to draw correlated parameter vectors.
#
# Dependencies are resolved at CALL time, not source time:
#   demand_sandwich_se   needs estimate_demand.R (accumulate, cell_negll_gradi)
#   cost_gmm_sandwich_se needs only a moment function passed in as gbar_fn

# --- Demand: Huber-White sandwich for the nested-logit MLE -----------------
# V = H^-1 B H^-1, H = Hessian of the weighted negLL (central FD of the analytical
# gradient), B = the HH-level robust meat crossprod(gradi). Households are the
# sampling unit (cells are the population, within-cell choices roughly independent
# -- matches the RF bootstrap design). theta is a NAMED vector c(covars..., lambda)
# aligned to the cell-matrix columns.
demand_sandwich_se <- function(cells, theta, fd_rel = 1e-5) {
  K <- length(theta) - 1L
  beta_hat   <- theta[seq_len(K)]
  lambda_hat <- theta[K + 1L]

  # Meat at theta_hat: HH-level score outer product
  B       <- matrix(0, K + 1, K + 1)
  g_total <- numeric(K + 1)
  for (ci in seq_along(cells)) {
    res     <- cell_negll_gradi(beta_hat, lambda_hat, cells[[ci]])
    B       <- B + crossprod(res$gradi)
    g_total <- g_total + res$grad
  }

  # Bread: Hessian of negLL by central FD of the analytical gradient
  H     <- matrix(0, K + 1, K + 1)
  hstep <- fd_rel * pmax(abs(theta), 1)
  for (j in seq_len(K + 1)) {
    tp <- theta; tp[j] <- tp[j] + hstep[j]
    tm <- theta; tm[j] <- tm[j] - hstep[j]
    gp <- accumulate(tp, cells, compute_grad = TRUE)$grad
    gm <- accumulate(tm, cells, compute_grad = TRUE)$grad
    H[, j] <- (gp - gm) / (2 * hstep[j])
  }
  H <- (H + t(H)) / 2

  Hinv <- tryCatch(solve(H), error = function(e) MASS::ginv(H))
  V    <- Hinv %*% B %*% Hinv

  d <- diag(V); d[d < 0] <- NA_real_
  se_df <- data.frame(term = names(theta), estimate = as.numeric(theta),
                      se = sqrt(d), stringsAsFactors = FALSE)
  se_df$z       <- se_df$estimate / se_df$se
  se_df$p_value <- 2 * pnorm(-abs(se_df$z))
  rownames(se_df) <- NULL

  dimnames(V) <- list(names(theta), names(theta))
  list(se = se_df, vcov = V, max_grad = max(abs(g_total)))
}

# --- Cost GMM: sandwich for the two-step estimator -------------------------
# V = (G'WG)^-1 G'W S W G (G'WG)^-1. gbar_fn(theta) returns averaged moments;
# gbar_fn(theta, return_contributions = TRUE) returns the contribution blocks
# (see moment_cov_blocks). Meat S is block-diagonal: M2 (PUF rows) and
# M3 (plan-year FOC rows), with the cross-block covariance set to zero
# (distinct data sources). CONDITIONAL on the demand estimates (no
# Newey-McFadden first-step correction; full propagation is in the CF
# bootstrap). param_names labels the rows in (alpha, gamma) order.
# Block-diagonal moment covariance from the per-observation contribution blocks
# (each element list(mat, n); rows are the block's observations, columns its
# moments, blocks in moment order and treated as independent).
moment_cov_blocks <- function(blocks, n_mom) {
  S <- matrix(0, n_mom, n_mom); k <- 0L
  for (b in blocks) {
    m <- ncol(b$mat); idx <- (k + 1):(k + m)
    S[idx, idx] <- crossprod(b$mat) / b$n^2
    k <- k + m
  }
  stopifnot(k == n_mom)
  S
}

cost_gmm_sandwich_se <- function(theta_hat, W, gbar_fn, N_ALPHA, N_GAMMA,
                                 n_mom, param_names, fd_rel = 1e-5) {
  n_par <- length(theta_hat)

  # Jacobian G = d g_bar / d theta' (n_mom x n_par)
  G     <- matrix(0, n_mom, n_par)
  hstep <- fd_rel * pmax(abs(theta_hat), 1)
  for (j in seq_len(n_par)) {
    tp <- theta_hat; tp[j] <- tp[j] + hstep[j]
    tm <- theta_hat; tm[j] <- tm[j] - hstep[j]
    G[, j] <- (gbar_fn(tp) - gbar_fn(tm)) / (2 * hstep[j])
  }

  contr <- gbar_fn(theta_hat, return_contributions = TRUE)
  Vg <- moment_cov_blocks(contr$blocks, n_mom)

  A    <- t(G) %*% W %*% G
  Ainv <- tryCatch(solve(A), error = function(e) MASS::ginv(A))
  V    <- Ainv %*% (t(G) %*% W %*% Vg %*% W %*% G) %*% Ainv

  d <- diag(V); d[d < 0] <- NA_real_
  se_df <- data.frame(
    param    = param_names,
    equation = c(rep("risk_score", N_ALPHA), rep("claims", N_GAMMA),
                 rep("commission", n_par - N_ALPHA - N_GAMMA)),
    estimate = as.numeric(theta_hat),
    se       = sqrt(d),
    stringsAsFactors = FALSE
  )
  se_df$z       <- se_df$estimate / se_df$se
  se_df$p_value <- 2 * pnorm(-abs(se_df$z))
  rownames(se_df) <- NULL

  dimnames(V) <- list(param_names, param_names)
  list(se = se_df, vcov = V)
}

