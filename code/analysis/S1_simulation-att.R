# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-25
## Description:   Monte Carlo simulation comparing ATT estimators for
##                discrete choice with endogenous treatment (assistance).
##                Tests whether a control function residual in the
##                unassisted choice model corrects selection bias.
##
## DGP:
##   Selection: D_i = 1{gamma_0 + gamma_z * Z_i + v_i > 0}
##   Utility:   U_ij = alpha_j + v_i * eta_j + D_i * delta_j + eps_ij
##   eps_ij ~ Type I Extreme Value (MNL)
##
##   Endogeneity: v_i drives both D and plan preferences (via eta_j)
##   Treatment effect: delta_j (assistance shifts utility by plan)
##   Instrument: Z_i (broker density, excluded from utility)

source("code/0-setup.R")

# =========================================================================
# Parameters
# =========================================================================

N <- 5000
J <- 4        # plans (j=1..4), plus outside option j=0
R <- 200

# Plan intercepts (outside option normalized to 0)
alpha <- c(0, 1.2, 1.5, 1.0, 0.8)   # j=0,1,2,3,4

# Endogeneity: v_i * eta_j (how unobservable shifts plan preferences)
eta <- c(0, 0.3, -0.2, 0.5, -0.1)   # j=0,1,2,3,4 (outside option eta=0)

# Treatment effect: delta_j + X_i * kappa_j (heterogeneous by plan and HH)
delta <- c(0, 0.4, 0.6, -0.2, 0.1)  # j=0,1,2,3,4 (no effect on outside)
kappa <- c(0, 0.3, -0.2, 0.4, -0.1) # j=0,1,2,3,4 (HTE: X_i shifts TE)

# Selection equation
gamma_0 <- -0.2
gamma_z <-  0.6


# =========================================================================
# Simulate data
# =========================================================================

simulate_data <- function(seed) {
  set.seed(seed)

  X <- rnorm(N)
  Z <- rnorm(N)
  v <- rnorm(N)
  D <- as.integer(gamma_0 + gamma_z * Z + 0.4 * X + v > 0)

  # Long format: N * (J+1) rows
  dt <- data.table(
    hh_id = rep(1:N, each = J + 1),
    alt   = rep(0:J, N),
    X_i   = rep(X, each = J + 1),
    Z_i   = rep(Z, each = J + 1),
    v_i   = rep(v, each = J + 1),
    D_i   = rep(D, each = J + 1)
  )

  # Utility: alpha_j + v_i*eta_j + D_i*(delta_j + X_i*kappa_j)
  dt[, alpha_j := alpha[alt + 1]]
  dt[, eta_j   := eta[alt + 1]]
  dt[, delta_j := delta[alt + 1]]
  dt[, kappa_j := kappa[alt + 1]]

  dt[, V := alpha_j + v_i * eta_j + D_i * (delta_j + X_i * kappa_j)]

  # MNL probabilities
  dt[, exp_V := exp(V)]
  dt[, sum_exp_V := sum(exp_V), by = hh_id]
  dt[, prob := exp_V / sum_exp_V]

  # Draw choices
  dt[, choice := 0L]
  chosen <- dt[, .(chosen_alt = sample(alt, 1, prob = prob)), by = hh_id]
  dt <- merge(dt, chosen, by = "hh_id")
  dt[, choice := as.integer(alt == chosen_alt)]
  dt[, chosen_alt := NULL]

  dt
}


# =========================================================================
# True ATT
# =========================================================================

compute_true_att <- function(dt) {
  a <- copy(dt[D_i == 1])

  # P(j) under D=1 (observed)
  p1 <- a[, .(p1 = mean(prob)), by = alt]

  # P(j) under D=0 counterfactual (remove treatment effect)
  a[, V0 := alpha_j + v_i * eta_j]
  a[, exp_V0 := exp(V0)]
  a[, sum_exp_V0 := sum(exp_V0), by = hh_id]
  a[, prob0 := exp_V0 / sum_exp_V0]
  p0 <- a[, .(p0 = mean(prob0)), by = alt]

  merge(p1, p0, by = "alt")[, att := p1 - p0][]
}


# =========================================================================
# MNL prediction helper
# =========================================================================

predict_mnl <- function(coefs, newdata, covar_names) {
  X_mat <- as.matrix(newdata[, ..covar_names])
  newdata[, V_hat := as.numeric(X_mat %*% coefs)]
  newdata[, exp_V_hat := exp(V_hat)]
  newdata[, sum_exp_V_hat := sum(exp_V_hat), by = hh_id]
  newdata[, pred := exp_V_hat / sum_exp_V_hat]
  newdata
}


# =========================================================================
# Estimator 1: Naive
# =========================================================================

estimate_naive <- function(dt) {
  unass <- copy(dt[D_i == 0])
  asst  <- copy(dt[D_i == 1])

  # Plan dummies (alt 1..4, outside option is baseline)
  for (j in 1:J) {
    unass[, paste0("d", j) := as.numeric(alt == j)]
    asst[, paste0("d", j) := as.numeric(alt == j)]
  }

  covars <- paste0("d", 1:J)

  ml_data <- mlogit.data(as.data.frame(unass),
                          choice = "choice", shape = "long",
                          alt.var = "alt", id.var = "hh_id")

  fit <- tryCatch(
    mlogit(as.formula(paste("choice ~", paste(covars, collapse = " + "), "| 0")),
           data = ml_data),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  coefs <- coef(fit)
  names(coefs) <- covars

  asst <- predict_mnl(coefs, asst, covars)

  obs  <- asst[, .(obs = mean(choice)), by = alt]
  pred <- asst[, .(pred = mean(pred)), by = alt]
  merge(obs, pred, by = "alt")[, att := obs - pred][]
}


# =========================================================================
# Estimator 2: Dummy CF (pooled)
# =========================================================================

estimate_dummy_cf <- function(dt) {
  d <- copy(dt)

  # First stage (all HH)
  fs_data <- d[, .(D_i = first(D_i), Z_i = first(Z_i), X_i = first(X_i)), by = hh_id]
  fs <- lm(D_i ~ Z_i + X_i, data = fs_data)
  fs_data[, v_hat := residuals(fs)]
  d <- merge(d, fs_data[, .(hh_id, v_hat)], by = "hh_id")

  # Plan dummies, D*plan dummies, v_hat*plan dummies
  for (j in 1:J) {
    d[, paste0("d", j) := as.numeric(alt == j)]
    d[, paste0("dd", j) := D_i * as.numeric(alt == j)]
    d[, paste0("cf", j) := v_hat * as.numeric(alt == j)]
  }

  covars <- c(paste0("d", 1:J), paste0("dd", 1:J), paste0("cf", 1:J))

  ml_data <- mlogit.data(as.data.frame(d),
                          choice = "choice", shape = "long",
                          alt.var = "alt", id.var = "hh_id")

  fit <- tryCatch(
    mlogit(as.formula(paste("choice ~", paste(covars, collapse = " + "), "| 0")),
           data = ml_data),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  coefs <- coef(fit)
  names(coefs) <- covars

  # Predict for assisted under D=1
  asst1 <- copy(d[D_i == 1])
  p1 <- predict_mnl(coefs, asst1, covars)
  p1_agg <- p1[, .(p1 = mean(pred)), by = alt]

  # Predict under D=0 (zero out dd columns)
  asst0 <- copy(d[D_i == 1])
  for (j in 1:J) asst0[, paste0("dd", j) := 0]
  p0 <- predict_mnl(coefs, asst0, covars)
  p0_agg <- p0[, .(p0 = mean(pred)), by = alt]

  merge(p1_agg, p0_agg, by = "alt")[, att := p1 - p0][]
}


# =========================================================================
# Estimator 3: PO + CF
# =========================================================================

estimate_po_cf <- function(dt) {
  d <- copy(dt)

  # First stage (all HH)
  fs_data <- d[, .(D_i = first(D_i), Z_i = first(Z_i), X_i = first(X_i)), by = hh_id]
  fs <- lm(D_i ~ Z_i + X_i, data = fs_data)
  fs_data[, v_hat := residuals(fs)]
  d <- merge(d, fs_data[, .(hh_id, v_hat)], by = "hh_id")

  unass <- copy(d[D_i == 0])
  asst  <- copy(d[D_i == 1])

  # Plan dummies + v_hat * plan dummies
  for (j in 1:J) {
    unass[, paste0("d", j) := as.numeric(alt == j)]
    unass[, paste0("cf", j) := v_hat * as.numeric(alt == j)]
    asst[, paste0("d", j) := as.numeric(alt == j)]
    asst[, paste0("cf", j) := v_hat * as.numeric(alt == j)]
  }

  covars <- c(paste0("d", 1:J), paste0("cf", 1:J))

  ml_data <- mlogit.data(as.data.frame(unass),
                          choice = "choice", shape = "long",
                          alt.var = "alt", id.var = "hh_id")

  fit <- tryCatch(
    mlogit(as.formula(paste("choice ~", paste(covars, collapse = " + "), "| 0")),
           data = ml_data),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  coefs <- coef(fit)
  names(coefs) <- covars

  asst <- predict_mnl(coefs, asst, covars)

  obs  <- asst[, .(obs = mean(choice)), by = alt]
  pred <- asst[, .(pred = mean(pred)), by = alt]
  merge(obs, pred, by = "alt")[, att := obs - pred][]
}


# =========================================================================
# Monte Carlo
# =========================================================================

cat("Running", R, "replications...\n")

# Diagnostics on first rep
dt_check <- simulate_data(42)
cat("  Assisted share:", mean(dt_check[alt == 0]$D_i), "\n")
fs_check <- dt_check[, .(D_i = first(D_i), Z_i = first(Z_i), X_i = first(X_i)), by = hh_id]
fs_fit <- lm(D_i ~ Z_i + X_i, data = fs_check)
cat("  First-stage F:", round(summary(fs_fit)$fstatistic[1], 1), "\n")
fs_check[, v_hat := residuals(fs_fit)]
cat("  v_hat mean (D=0):", round(mean(fs_check[D_i == 0]$v_hat), 4), "\n")
cat("  v_hat mean (D=1):", round(mean(fs_check[D_i == 1]$v_hat), 4), "\n")

set.seed(20260325)
seeds <- sample.int(1e7, R)
results <- list()

for (r in 1:R) {
  dt <- simulate_data(seeds[r])

  true_att <- compute_true_att(dt)
  naive    <- estimate_naive(dt)
  dummy_cf <- estimate_dummy_cf(dt)
  po_cf    <- estimate_po_cf(dt)

  if (!is.null(naive) && !is.null(dummy_cf) && !is.null(po_cf)) {
    results[[length(results) + 1]] <- data.table(
      rep = r, alt = true_att$alt,
      true_att = true_att$att,
      naive_att = naive$att,
      dummy_cf_att = dummy_cf$att,
      po_cf_att = po_cf$att
    )
  }

  if (r %% 20 == 0) cat("  Rep", r, "of", R, "\n")
}

results <- rbindlist(results)
cat("\nDone:", length(unique(results$rep)), "successful reps\n")


# =========================================================================
# Results
# =========================================================================

summary_stats <- results[, .(
  bias_naive    = mean(naive_att - true_att),
  bias_dummy_cf = mean(dummy_cf_att - true_att),
  bias_po_cf    = mean(po_cf_att - true_att),
  rmse_naive    = sqrt(mean((naive_att - true_att)^2)),
  rmse_dummy_cf = sqrt(mean((dummy_cf_att - true_att)^2)),
  rmse_po_cf    = sqrt(mean((po_cf_att - true_att)^2))
), by = alt]

cat("\n=== Bias ===\n")
print(summary_stats[, .(alt, bias_naive, bias_dummy_cf, bias_po_cf)])

cat("\n=== RMSE ===\n")
print(summary_stats[, .(alt, rmse_naive, rmse_dummy_cf, rmse_po_cf)])

overall <- results[alt > 0, .(
  bias_naive    = mean(naive_att - true_att),
  bias_dummy_cf = mean(dummy_cf_att - true_att),
  bias_po_cf    = mean(po_cf_att - true_att),
  rmse_naive    = sqrt(mean((naive_att - true_att)^2)),
  rmse_dummy_cf = sqrt(mean((dummy_cf_att - true_att)^2)),
  rmse_po_cf    = sqrt(mean((po_cf_att - true_att)^2))
)]

cat("\n=== Overall (plans only) ===\n")
cat("  Naive:    bias =", round(overall$bias_naive, 4),
    " RMSE =", round(overall$rmse_naive, 4), "\n")
cat("  Dummy CF: bias =", round(overall$bias_dummy_cf, 4),
    " RMSE =", round(overall$rmse_dummy_cf, 4), "\n")
cat("  PO + CF:  bias =", round(overall$bias_po_cf, 4),
    " RMSE =", round(overall$rmse_po_cf, 4), "\n")

# Plot
plot_long <- results[alt > 0] %>%
  pivot_longer(cols = c(naive_att, dummy_cf_att, po_cf_att),
               names_to = "estimator", values_to = "att_hat") %>%
  mutate(bias = att_hat - true_att,
         estimator = factor(estimator,
           levels = c("naive_att", "dummy_cf_att", "po_cf_att"),
           labels = c("Naive", "Dummy CF", "PO + CF")))

p <- ggplot(plot_long, aes(x = estimator, y = bias)) +
  geom_boxplot(outlier.size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~paste("Plan", alt)) +
  labs(y = "ATT Bias (estimate - truth)", x = NULL) +
  theme_bw()
ggsave("results/figures/simulation_att_bias.png", p, width = 8, height = 5)
cat("\nFigure saved: results/figures/simulation_att_bias.png\n")
