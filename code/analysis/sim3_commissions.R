# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Monte Carlo validation, tier 3: demand, optimal premiums,
##                AND commissions set by the model's own first-order condition
##                with the cross-market wedge. Households carry a risk score
##                correlated with the channel-selection covariate, claims cost
##                is mc_j x risk, and a budget-neutral risk-adjustment
##                transfer compensates each plan to the market-average risk
##                mix, so margins have the structure of the estimated model.
##                Each firm-market draws its administrative offset beta_f
##                (known to the estimator, as the filings-based beta is) and
##                exchange leverage lev_f = (1 - w)/w as a PAIR from the
##                empirical carrier-year pairs (commission_beta_carrier
##                joined with commission_filings' on-exchange shares of the
##                individual book, leverage jittered) --
##                beta and leverage are positively related in the filings,
##                and independent draws manufacture carrier types with
##                near-zero effective commission cost that corner; the true
##                wedge is delta0 + delta1 * lev_f at the empirical
##                estimates. Firms choose premiums and their dollar
##                commission scale jointly each year to maximize exchange
##                profit B minus ((1 - beta_f) + wedge_f) per commission
##                dollar, so the commission FOC
##                MB/MC = (1 - beta_f) + wedge_f holds at any interior
##                optimum. Marginal costs drift across years. Each rep
##                estimates demand exactly as on real data, then recovers
##                (delta0, delta1) the way s4's M4 block does: the FOC gap
##                phi = MB/MC - 1 + beta_f computed at the ESTIMATED demand
##                parameters per firm-market-year, fit on (1, leverage).
##                Firm-years at a search bound are excluded from the wedge
##                fit, as M4 gates its conditions; the share at a bound is
##                printed after the panel solve. The steering coefficient is
##                set below the production estimate so the stylized market's
##                equilibrium schedules sit at the empirical scale (a market
##                with 4-8 plans concentrates the same steering response far
##                more than a 30-plan cell); the estimator estimates it, so
##                recovery is tested at the truth that generated the data.
##                Simplification: flat per-member commission bases in
##                dollars pmpm (premiums are in hundred-dollar units; the
##                /100 in the outlay converts). Standalone; not sourced by
##                the driver.
## Output:        results/simulations/sim3_estimates.csv (demand recovery),
##                results/simulations/sim3_wedge.csv (delta recovery)

suppressMessages({ library(tidyverse); library(data.table); library(nnet) })
setwd("C:/Users/immccar/SynologyDrive/work/research-projects/health-insurance-decision-support")
TEMP_DIR <- "D:/temp-research-data/health-insurance-decision-support"
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/estimate_demand.R")
source("code/analysis/helpers/se.R")

N_REPS  <- as.integer(Sys.getenv("MC_REPS", "20"))
N_CELLS <- as.integer(Sys.getenv("SIM_CELLS", "20"))
N_HH    <- 2000L               # households per market-year (panel is T x larger)
N_REF   <- 4000L
J_RANGE <- 4:8
T_YEARS <- 6L
MC_DRIFT_SD <- 0.15            # year-to-year log drift of marginal costs
K_MAX <- 25                    # commission-scale search ceiling; MB/MC falls
                               # below any drawn effective cost well inside it
DELTA_TRUE  <- c(delta0 = -0.457, delta1 = 0.090)
MC_SEED <- 81520L
MC_DIR  <- file.path(TEMP_DIR, "sim3_cells")
OUT_DIR <- "results/simulations"
COEF_DIR <- file.path(TEMP_DIR, "sim3_coefs")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
if (!dir.exists(COEF_DIR)) dir.create(COEF_DIR, recursive = TRUE)

theta_true <- c(
  inside = 0.45, premium = -0.05, av = 0.55, hmo = -0.03, brand1 = 0.05,
  hh_size_prem = -0.004,
  assisted_av = 0.171, broker_av = 0.088,
  assisted_premium = -0.014, broker_premium = -0.002,
  commission_broker = 0.0005,
  lambda = 0.0534
)
ASST <- c("assisted_av", "broker_av", "assisted_premium", "broker_premium",
          "commission_broker")
BASE <- setdiff(names(theta_true), c(ASST, "lambda"))
SPEC_PATH <- file.path(TEMP_DIR, "demand_spec_sim.csv")
write_demand_spec(BASE, ASST, SPEC_PATH)

FS_TRUE <- list(nav = c(int = -0.6, z = 1.5, x = 0.5),
                agt = c(int = -0.9, z = 3.0, x = 0.8))

lse <- function(v) { mx <- max(v); mx + log(sum(exp(v - mx))) }

draw_hh <- function(n, z, id0) {
  hh <- data.table(
    household_number = seq_len(n) + id0,
    hh_size = sample(1:4, n, replace = TRUE, prob = c(.5, .25, .15, .1)),
    x = as.integer(runif(n) < 0.5),
    rf = runif(n, 0.7, 2.5)
  )
  hh[, r := exp(rnorm(n, 0, 0.4)) * (1 + 0.3 * x)]
  uN <- FS_TRUE$nav["int"] + FS_TRUE$nav["z"] * z + FS_TRUE$nav["x"] * hh$x
  uA <- FS_TRUE$agt["int"] + FS_TRUE$agt["z"] * z + FS_TRUE$agt["x"] * hh$x
  den <- 1 + exp(uN) + exp(uA)
  hh[, `:=`(p0 = 1 / den, pN = exp(uN) / den, pA = exp(uA) / den)]
  hh
}

# Firm-market primitives: (beta_f, leverage) pairs from the empirical
# carrier-years, so the effective commission cost has the joint distribution
# the estimates imply
bb <- fread("data/output/commission_beta_carrier.csv")
ws <- fread("data/output/commission_filings.csv")
emp_pairs <- merge(ws[on_share > 0 & on_share <= 1, .(insurer_prefix, on_share)],
                   bb[, .(insurer_prefix, beta)], by = "insurer_prefix")
emp_pairs[, lev := (1 - on_share) / on_share]
set.seed(MC_SEED)
markets <- lapply(seq_len(N_CELLS), function(m) {
  z <- runif(1, 0.1, 0.9)
  Jm <- sample(J_RANGE, 1)
  firm <- rep_len(c("A", "B", "C"), Jm)
  plans <- data.table(
    plan_id = sprintf("P%d_%d", m, seq_len(Jm)),
    firm    = firm,
    av      = sample(c(0.6, 0.7, 0.8, 0.9), Jm, replace = TRUE),
    hmo     = as.integer(runif(Jm) < 0.4),
    brand1  = as.integer(firm == "A"),
    mc      = runif(Jm, 0.5, 2.0),
    basis   = ifelse(firm == "C", 0, runif(Jm, 8, 20))
  )
  repeat {
    pick   <- emp_pairs[sample(.N, 2)]
    beta_f <- setNames(pick$beta, c("A", "B"))
    lev_f  <- setNames(pick$lev * exp(rnorm(2, 0, 0.2)), c("A", "B"))
    wedge_f <- DELTA_TRUE[["delta0"]] + DELTA_TRUE[["delta1"]] * lev_f
    cost_mult <- (1 - beta_f) + wedge_f
    if (all(cost_mult > 0.10)) break
  }
  list(m = m, z = z, plans = plans, lev_f = lev_f, beta_f = beta_f,
       cost_mult = c(cost_mult, C = 0))
})

state_shares <- function(theta, hh, pl, P, Cvec) {
  lam <- theta[["lambda"]]
  Jm <- nrow(pl)
  P_mat <- outer(hh$rf, P)
  Vb <- theta[["inside"]] + theta[["premium"]] * P_mat +
    matrix(theta[["av"]] * pl$av + theta[["hmo"]] * pl$hmo +
             theta[["brand1"]] * pl$brand1, nrow(hh), Jm, byrow = TRUE) +
    theta[["hh_size_prem"]] * hh$hh_size * P_mat
  addN <- matrix(theta[["assisted_av"]] * pl$av, nrow(hh), Jm, byrow = TRUE) +
    theta[["assisted_premium"]] * P_mat
  addA <- matrix(theta[["broker_av"]] * pl$av +
                   theta[["commission_broker"]] * Cvec, nrow(hh), Jm, byrow = TRUE) +
    theta[["broker_premium"]] * P_mat
  sh <- function(V) { W <- exp(V / lam - apply(V / lam, 1, max)); W / rowSums(W) }
  s0 <- sh(Vb); sN <- sh(Vb + addN); sA <- sh(Vb + addA)
  I0 <- apply(Vb / lam, 1, lse); IN <- apply((Vb + addN) / lam, 1, lse)
  IA <- apply((Vb + addA) / lam, 1, lse)
  Ibar <- hh$p0 * I0 + hh$pN * IN + hh$pA * IA
  s_g <- { l <- lam * Ibar; m0 <- pmax(l, 0); exp(l - m0) / (exp(l - m0) + exp(-m0)) }
  list(q  = s_g * (hh$p0 * s0 + hh$pN * sN + hh$pA * sA),
       qB = s_g * hh$pA * sA,
       Vb = Vb, addN = addN, addA = addA, P_mat = P_mat, s_g = s_g)
}

# Per-firm margin part B (revenue minus risk-scaled claims plus the budget-
# neutral risk-adjustment transfer) and raw commission outlay O; the DGP
# objective is B - cost_mult_f * O, whose interior optimum satisfies
# dB/dk = cost_mult_f * dO/dk, i.e. MB/MC = (1 - beta_f) + wedge_f
firm_BO <- function(theta, hh, pl, mc_t, P, kvec, Cvec = NULL) {
  if (is.null(Cvec)) Cvec <- unname(kvec[pl$firm]) * pl$basis
  ss <- state_shares(theta, hh, pl, P, Cvec)
  w <- hh$hh_size
  q_j   <- colSums(w * ss$q)
  rq_j  <- colSums(w * hh$r * ss$q)
  rev_j <- P * colSums(w * hh$rf * ss$q)
  # Each plan is compensated to the market-average risk mix at the market-
  # average premium; transfers sum to zero by construction
  rs_mkt <- sum(rq_j) / sum(q_j)
  p_mkt  <- sum(rev_j) / sum(q_j)
  T_j <- p_mkt * (rq_j / pmax(q_j, 1e-12) - rs_mkt) / rs_mkt
  B_j <- rev_j - mc_t * rq_j + T_j * q_j
  O_j <- Cvec / 100 * colSums(w * ss$qB)
  list(B = tapply(B_j, pl$firm, sum), O = tapply(O_j, pl$firm, sum))
}

firm_profit_dgp <- function(hh, pl, mc_t, P, kvec, cost_mult) {
  bo <- firm_BO(theta_true, hh, pl, mc_t, P, kvec)
  bo$B - cost_mult[names(bo$B)] * bo$O
}

# Damped plan-by-plan Newton on each plan's own first-order condition, with
# premiums capped in [mc, mc + 5]: a raw root-finder wanders into the region
# where shares collapse and every gradient vanishes, and declares that a root.
solve_P <- function(refhh, pl, mc_t, kvec, P_start, cost_mult) {
  Jm <- nrow(pl); h <- 0.01
  P <- pmin(pmax(P_start, mc_t), mc_t + 2)
  conv <- FALSE
  for (it in 1:200) {
    P_old <- P
    for (j in seq_len(Jm)) {
      f <- pl$firm[j]
      Pp <- P; Pp[j] <- Pp[j] + h
      Pm <- P; Pm[j] <- Pm[j] - h
      pi0 <- firm_profit_dgp(refhh, pl, mc_t, P,  kvec, cost_mult)[[f]]
      pip <- firm_profit_dgp(refhh, pl, mc_t, Pp, kvec, cost_mult)[[f]]
      pim <- firm_profit_dgp(refhh, pl, mc_t, Pm, kvec, cost_mult)[[f]]
      g  <- (pip - pim) / (2 * h)
      g2 <- (pip - 2 * pi0 + pim) / h^2
      step <- if (is.finite(g2) && g2 < -1e-9) -g / g2 else sign(g) * 0.1
      P[j] <- min(max(P[j] + max(min(step, 0.25), -0.25), mc_t[j]), mc_t[j] + 5)
    }
    if (max(abs(P - P_old)) < 1e-4) { conv <- TRUE; break }
  }
  list(x = P, termcd = if (conv) 1L else 9L)
}

# Continuous commission optimum for firm f given premiums (the FOC holds at an
# interior point rather than a grid corner because the wedge raises the
# effective cost of a commission dollar)
best_k <- function(refhh, pl, mc_t, P, kvec, f, cost_mult) {
  optimize(function(k) {
    kv <- kvec; kv[f] <- k
    firm_profit_dgp(refhh, pl, mc_t, P, kv, cost_mult)[[f]]
  }, interval = c(0, K_MAX), maximum = TRUE, tol = 1e-4)$maximum
}

# Panel equilibrium: premiums and commissions jointly, each year at that
# year's costs (the FOC holds every firm-year; no stickiness in the model)
cat("Solving the", N_CELLS, "market panels at the truth...\n")
t0 <- Sys.time()
eq <- lapply(markets, function(mk) {
  set.seed(MC_SEED + mk$m * 37L)
  refhh <- draw_hh(N_REF, mk$z, 0L)
  pl <- mk$plans
  years <- vector("list", T_YEARS)
  mc_t <- pl$mc
  kvec <- c(A = 1, B = 1, C = 0)
  P <- mc_t + 1
  for (t in seq_len(T_YEARS)) {
    if (t > 1) mc_t <- pmin(pmax(mc_t * exp(rnorm(length(mc_t), 0, MC_DRIFT_SD)), 0.3), 3)
    for (round in 1:6) {
      sol <- solve_P(refhh, pl, mc_t, kvec, P, mk$cost_mult); P <- sol$x
      k_old <- kvec
      for (f in c("A", "B")) kvec[f] <- best_k(refhh, pl, mc_t, P, kvec, f, mk$cost_mult)
      if (max(abs(kvec - k_old)) < 1e-3) break
    }
    sol <- solve_P(refhh, pl, mc_t, kvec, P, mk$cost_mult); P <- sol$x
    years[[t]] <- list(t = t, mc_t = mc_t, P = P, kvec = kvec, termcd = sol$termcd)
  }
  cat(sprintf("  market %d: lev = (%.2f, %.2f), k range A [%.2f, %.2f] B [%.2f, %.2f]\n",
              mk$m, mk$lev_f[["A"]], mk$lev_f[["B"]],
              min(vapply(years, function(y) y$kvec[["A"]], numeric(1))),
              max(vapply(years, function(y) y$kvec[["A"]], numeric(1))),
              min(vapply(years, function(y) y$kvec[["B"]], numeric(1))),
              max(vapply(years, function(y) y$kvec[["B"]], numeric(1)))))
  list(years = years, refhh = refhh)
})
kk <- unlist(lapply(eq, function(e)
  lapply(e$years, function(y) y$kvec[c("A", "B")])))
cat(sprintf("panels done, %.1f min; k in [%.2f, %.2f], %.1f%% of firm-years at a search bound\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins")),
            min(kk), max(kk), 100 * mean(kk < 0.05 | kk > K_MAX - 0.05)))

simulate_rep <- function(r) {
  out <- list()
  for (i in seq_along(markets)) {
    mk <- markets[[i]]; pl <- mk$plans; Jm <- nrow(pl)
    for (yy in eq[[i]]$years) {
      set.seed(MC_SEED + r * 10000L + mk$m * 100L + yy$t)
      hh <- draw_hh(N_HH, mk$z, mk$m * 1e7 + yy$t * 1e6)
      Cvec <- unname(yy$kvec[pl$firm]) * pl$basis
      ss <- state_shares(theta_true, hh, pl, yy$P, Cvec)
      hh[, enroll := as.integer(runif(.N) < ss$s_g)]
      u <- runif(N_HH)
      hh[, channel := fifelse(u < p0, "Unassisted",
                       fifelse(u < p0 + pN, "Navigator", "Agent"))]
      lam <- theta_true[["lambda"]]
      Vc <- ss$Vb + as.integer(hh$channel == "Navigator") * ss$addN +
                    as.integer(hh$channel == "Agent") * ss$addA
      W <- exp(Vc / lam - apply(Vc / lam, 1, max))
      pr <- W / rowSums(W)
      pick <- max.col(matrix(runif(N_HH), N_HH, Jm) < t(apply(pr, 1, cumsum)),
                      ties.method = "first")
      hh[, plan_pick := pick]
      out[[length(out) + 1]] <- list(mk = mk, t = yy$t, Cvec = Cvec,
                                     hh = hh, P_mat = ss$P_mat)
    }
  }
  out
}

write_cells <- function(cells, p_hat) {
  if (dir.exists(MC_DIR)) unlink(MC_DIR, recursive = TRUE)
  dir.create(MC_DIR, recursive = TRUE)
  for (cc in cells) {
    hh <- merge(cc$hh, p_hat, by = "household_number")
    pl <- cc$mk$plans
    Jm <- nrow(pl)
    inside_rows <- hh[rep(seq_len(nrow(hh)), each = Jm)]
    inside_rows[, `:=`(plan_id = rep(pl$plan_id, nrow(hh)),
                       premium = as.vector(t(cc$P_mat)),
                       av = rep(pl$av, nrow(hh)), hmo = rep(pl$hmo, nrow(hh)),
                       brand1 = rep(pl$brand1, nrow(hh)),
                       comm_pmpm = rep(cc$Cvec, nrow(hh)),
                       jidx = rep(seq_len(Jm), nrow(hh)), inside = 1L)]
    out_rows <- hh[, .(household_number, hh_size, x, enroll, channel, plan_pick,
                       p_none_hat, p_nav_hat, p_agent_hat)]
    out_rows[, `:=`(plan_id = "Uninsured", premium = 0, av = 0, hmo = 0L,
                    brand1 = 0L, comm_pmpm = 0, jidx = 0L, inside = 0L)]
    cd <- rbind(inside_rows, out_rows, fill = TRUE)
    cd[, choice := fifelse(enroll == 1L, as.integer(jidx == plan_pick),
                           as.integer(plan_id == "Uninsured"))]
    ch <- fifelse(cd$enroll == 1L, cd$channel, "Unassisted")
    cd[, `:=`(nonbroker = as.integer(ch == "Navigator" & inside == 1L),
              broker    = as.integer(ch == "Agent" & inside == 1L))]
    cd[, `:=`(hh_weight = hh_size,
              hh_size_prem = hh_size * premium,
              assisted_av = nonbroker * av, broker_av = broker * av,
              assisted_premium = nonbroker * premium, broker_premium = broker * premium,
              commission_broker = broker * comm_pmpm,
              assisted = as.integer(nonbroker == 1L | broker == 1L))]
    fwrite(cd[, .(household_number, plan_id, choice, hh_weight, assisted,
                  inside, premium, av, hmo, brand1, hh_size_prem,
                  assisted_av, broker_av, assisted_premium, broker_premium,
                  commission_broker, comm_pmpm,
                  p_none_hat, p_nav_hat, p_agent_hat)],
           file.path(MC_DIR, sprintf("cell_%d_%d_data.csv", cc$mk$m, 2013L + cc$t)))
  }
}

# Rep loop -----------------------------------------------------------------
res_path   <- file.path(OUT_DIR, "sim3_estimates.csv")
wedge_path <- file.path(OUT_DIR, "sim3_wedge.csv")
lambda_true <- theta_true[["lambda"]]
for (r in seq_len(N_REPS)) {
  done <- if (file.exists(res_path)) unique(read.csv(res_path)$rep) else integer(0)
  if (r %in% done) next
  t0 <- Sys.time()
  cat(sprintf("\n=== tier 3 rep %d of %d ===\n", r, N_REPS))
  cells <- simulate_rep(r)

  fs <- rbindlist(lapply(cells, function(cc)
    cc$hh[, .(household_number, hh_size, x, enroll, channel, z = cc$mk$z)]))
  fs[, channel_f := factor(channel, levels = c("Unassisted", "Navigator", "Agent"))]
  fit <- multinom(channel_f ~ z + x + hh_size, data = fs[enroll == 1L],
                  weights = hh_size, maxit = 300, trace = FALSE)
  p <- predict(fit, newdata = fs, type = "probs")
  p_hat <- data.table(household_number = fs$household_number,
                      p_none_hat = p[, "Unassisted"], p_nav_hat = p[, "Navigator"],
                      p_agent_hat = p[, "Agent"])

  write_cells(cells, p_hat)

  est <- estimate_demand(cell_dir = MC_DIR, spec_path = SPEC_PATH,
                         out_path = file.path(COEF_DIR, sprintf("coefs_sim3_rep%02d.csv", r)),
                         ext_exclude = ASST)

  loaded <- load_all_cells(MC_DIR, c(BASE, ASST))
  cells_e <- prepare_cells(normalize_weights(loaded$cells), c(BASE, ASST), ASST)
  rm(loaded)
  theta_r <- setNames(est$estimate, est$term)
  dse <- demand_sandwich_se(cells_e, theta_r)
  rm(cells_e); gc(verbose = FALSE)

  row <- merge(data.frame(term = names(theta_true), truth = unname(theta_true)),
               dse$se[, c("term", "estimate", "se")], by = "term")
  row$rep <- r
  if (file.exists(res_path)) write.table(row, res_path, append = TRUE, sep = ",",
                                         col.names = FALSE, row.names = FALSE)
  else write.csv(row, res_path, row.names = FALSE)

  # Wedge recovery, the M4 logic: the FOC gap phi = MB/MC - 1 + beta_f at the
  # ESTIMATED demand parameters, true costs, and the equilibrium premiums and
  # commissions, per firm-market-year; (delta0, delta1) from the fit of phi on
  # (1, leverage), weighted by the firm-year commission base
  h <- 0.02
  wrows <- rbindlist(lapply(seq_along(markets), function(i) {
    mk <- markets[[i]]; e <- eq[[i]]; pl <- mk$plans
    rbindlist(lapply(e$years, function(yy) {
      rbindlist(lapply(c("A", "B"), function(f) {
        kp <- yy$kvec; kp[f] <- kp[f] + h
        km <- yy$kvec; km[f] <- km[f] - h
        bop <- firm_BO(theta_r, e$refhh, pl, yy$mc_t, yy$P, kp)
        bom <- firm_BO(theta_r, e$refhh, pl, yy$mc_t, yy$P, km)
        MB <- (bop$B[[f]] - bom$B[[f]]) / (2 * h)
        MCd <- (bop$O[[f]] - bom$O[[f]]) / (2 * h)
        bo0 <- firm_BO(theta_r, e$refhh, pl, yy$mc_t, yy$P, yy$kvec)
        data.table(market = mk$m, t = yy$t, firm = f,
                   phi = MB / MCd - (1 - mk$beta_f[[f]]),
                   lev = mk$lev_f[[f]], w_o = bo0$O[[f]], k_eq = yy$kvec[[f]])
      }))
    }))
  }))
  # corner guard, mirroring M4's gating: a firm-year at a bound of the search
  # interval does not satisfy the FOC and is excluded from the wedge fit
  wrows <- wrows[is.finite(phi) & w_o > 0 & k_eq > 0.05 & k_eq < K_MAX - 0.05]
  wfit <- lm(phi ~ lev, data = wrows, weights = wrows$w_o)
  wrow <- data.frame(rep = r, delta0 = unname(coef(wfit)[1]), delta1 = unname(coef(wfit)[2]),
                     delta0_true = DELTA_TRUE[["delta0"]], delta1_true = DELTA_TRUE[["delta1"]],
                     n_conditions = nrow(wrows))
  if (file.exists(wedge_path)) write.table(wrow, wedge_path, append = TRUE, sep = ",",
                                           col.names = FALSE, row.names = FALSE)
  else write.csv(wrow, wedge_path, row.names = FALSE)

  cat(sprintf("  rep %d done in %.1f min: lambda_hat = %.4f (truth %.4f); delta = (%.3f, %.3f) vs (%.2f, %.2f)\n",
              r, as.numeric(difftime(Sys.time(), t0, units = "mins")),
              theta_r[["lambda"]], lambda_true, wrow$delta0, wrow$delta1,
              DELTA_TRUE[["delta0"]], DELTA_TRUE[["delta1"]]))
}

sim3 <- read_csv(res_path, show_col_types = FALSE)
summ3 <- sim3 %>% group_by(term) %>%
  summarize(truth = first(truth), mean_est = mean(estimate),
            bias = mean(estimate - truth),
            rmse = sqrt(mean((estimate - truth)^2)),
            coverage = mean(abs(estimate - truth) <= 1.96 * se, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
cat("\n=== tier 3 demand recovery (full model, FOC-with-wedge commissions) ===\n")
print(as.data.frame(summ3), row.names = FALSE)
wp <- read_csv(wedge_path, show_col_types = FALSE)
cat(sprintf("\nwedge recovery over %d reps: delta0 mean %.3f (truth %.2f, rmse %.3f); delta1 mean %.3f (truth %.2f, rmse %.3f)\n",
            nrow(wp), mean(wp$delta0), DELTA_TRUE[["delta0"]],
            sqrt(mean((wp$delta0 - wp$delta0_true)^2)),
            mean(wp$delta1), DELTA_TRUE[["delta1"]],
            sqrt(mean((wp$delta1 - wp$delta1_true)^2))))
