# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Monte Carlo validation, tier 2: demand plus optimal
##                premiums, commissions exogenous. Firms own plans and face
##                marginal-cost primitives; each market's premiums solve the
##                Bertrand first-order conditions under the expected-
##                inclusive-value demand at the true parameters, with
##                commission schedules held at exogenous levels. Households
##                choose within that equilibrium, and each rep estimates
##                demand exactly as on real data. Simplifications, stated: no
##                risk adjustment (a coherent special case), beta known and
##                common, flat per-member commission bases. Standalone; not
##                sourced by the driver. Tier 1 is sim1_demand.R; tier 3 is
##                sim3_commissions.R.
## Output:        results/simulations/sim2_estimates.csv

suppressMessages({ library(tidyverse); library(data.table); library(nnet); library(nleqslv) })
setwd("C:/Users/immccar/SynologyDrive/work/research-projects/health-insurance-decision-support")
TEMP_DIR <- "D:/temp-research-data/health-insurance-decision-support"
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/estimate_demand.R")
source("code/analysis/helpers/se.R")

N_REPS  <- as.integer(Sys.getenv("MC_REPS", "20"))
N_CELLS <- 20L
N_HH    <- 5000L
N_REF   <- 4000L               # reference population for the equilibrium solve
J_RANGE <- 4:8
BETA_F  <- 0.3                 # administrative offset per commission dollar
MC_SEED <- 70414L
MC_DIR  <- file.path(TEMP_DIR, "sim2_cells")
OUT_DIR <- "results/simulations"
COEF_DIR <- file.path(TEMP_DIR, "sim2_coefs")
if (!dir.exists(COEF_DIR)) dir.create(COEF_DIR, recursive = TRUE)
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

theta_true <- c(
  inside = 0.45, premium = -0.05, av = 0.55, hmo = -0.03, brand1 = 0.05,
  hh_size_prem = -0.004,
  assisted_av = 0.17, broker_av = 0.07,
  assisted_premium = -0.014, broker_premium = -0.002,
  commission_broker = 0.001,
  lambda = 0.054
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
  uN <- FS_TRUE$nav["int"] + FS_TRUE$nav["z"] * z + FS_TRUE$nav["x"] * hh$x
  uA <- FS_TRUE$agt["int"] + FS_TRUE$agt["z"] * z + FS_TRUE$agt["x"] * hh$x
  den <- 1 + exp(uN) + exp(uA)
  hh[, `:=`(p0 = 1 / den, pN = exp(uN) / den, pA = exp(uA) / den)]
  hh
}

# Markets: menus with firms, cost and commission-basis primitives
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
    mc      = runif(Jm, 0.5, 2.0),            # $100/member/month
    basis   = ifelse(firm == "C", 0, runif(Jm, 8, 20))   # $/member/month at k = 1
  )
  list(m = m, z = z, plans = plans)
})

# Demand at theta: state shares, enrollment, expected members and agent members
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
       Vb = Vb, addN = addN, addA = addA, P_mat = P_mat, Ibar = Ibar, s_g = s_g)
}

firm_profit <- function(theta, hh, pl, P, kvec) {
  Cvec <- unname(kvec[pl$firm]) * pl$basis
  ss <- state_shares(theta, hh, pl, P, Cvec)
  w <- hh$hh_size
  rev_j  <- P * colSums(w * hh$rf * ss$q)
  cost_j <- pl$mc * colSums(w * ss$q)
  comm_j <- (1 - BETA_F) * Cvec / 100 * colSums(w * ss$qB)
  tapply(rev_j - cost_j - comm_j, pl$firm, sum)
}

# Equilibrium: premiums from the FOCs at the exogenous commission schedules.
# Damped plan-by-plan Newton on each plan's own first-order condition, with
# premiums capped in [mc, mc + 5]: a raw root-finder wanders into the region
# where shares collapse and every gradient vanishes, and declares that a root.
solve_P_eq <- function(refhh, pl, kvec, P_start) {
  Jm <- nrow(pl)
  h <- 0.01
  P <- pmin(pmax(P_start, pl$mc), pl$mc + 2)
  conv <- FALSE
  for (it in 1:200) {
    P_old <- P
    for (j in seq_len(Jm)) {
      f <- pl$firm[j]
      Pp <- P; Pp[j] <- Pp[j] + h
      Pm <- P; Pm[j] <- Pm[j] - h
      pi0 <- firm_profit(theta_true, refhh, pl, P,  kvec)[[f]]
      pip <- firm_profit(theta_true, refhh, pl, Pp, kvec)[[f]]
      pim <- firm_profit(theta_true, refhh, pl, Pm, kvec)[[f]]
      g  <- (pip - pim) / (2 * h)
      g2 <- (pip - 2 * pi0 + pim) / h^2
      step <- if (is.finite(g2) && g2 < -1e-9) -g / g2 else sign(g) * 0.1
      P[j] <- min(max(P[j] + max(min(step, 0.25), -0.25), pl$mc[j]), pl$mc[j] + 5)
    }
    if (max(abs(P - P_old)) < 1e-4) { conv <- TRUE; break }
  }
  list(x = P, termcd = if (conv) 1L else 9L)
}

solve_market <- function(mk) {
  set.seed(MC_SEED + mk$m * 37L)
  refhh <- draw_hh(N_REF, mk$z, 0L)
  pl <- mk$plans
  kvec <- c(A = 1, B = 1, C = 0)
  sol <- solve_P_eq(refhh, pl, kvec, pl$mc + 1)
  list(P = sol$x, kvec = kvec, Cvec = unname(kvec[pl$firm]) * pl$basis,
       termcd = sol$termcd)
}

cat("Solving the", N_CELLS, "market equilibria at the truth...\n")
t0 <- Sys.time()
eq <- lapply(markets, function(mk) {
  s <- solve_market(mk)
  cat(sprintf("  market %d: termcd %d, mean P = %.2f\n", mk$m, s$termcd, mean(s$P)))
  s
})
cat(sprintf("equilibria done, %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

# Simulate one rep of household choices at the equilibrium ------------------
simulate_rep <- function(r) {
  lapply(seq_along(markets), function(i) {
    mk <- markets[[i]]; e <- eq[[i]]; pl <- mk$plans; Jm <- nrow(pl)
    set.seed(MC_SEED + r * 1000L + mk$m)
    hh <- draw_hh(N_HH, mk$z, mk$m * 1e6)
    ss <- state_shares(theta_true, hh, pl, e$P, e$Cvec)
    P_ins <- ss$s_g
    hh[, enroll := as.integer(runif(.N) < P_ins)]
    u <- runif(N_HH)
    hh[, channel := fifelse(u < p0, "Unassisted", fifelse(u < p0 + pN, "Navigator", "Agent"))]
    Vc <- ss$Vb + as.integer(hh$channel == "Navigator") * ss$addN +
                  as.integer(hh$channel == "Agent") * ss$addA
    lam <- theta_true[["lambda"]]
    W <- exp(Vc / lam - apply(Vc / lam, 1, max))
    pr <- W / rowSums(W)
    pick <- max.col(matrix(runif(N_HH), N_HH, Jm) < t(apply(pr, 1, cumsum)),
                    ties.method = "first")
    hh[, plan_pick := pick]
    list(mk = mk, e = e, hh = hh, P_mat = ss$P_mat)
  })
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
                       comm_pmpm = rep(cc$e$Cvec, nrow(hh)),
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
           file.path(MC_DIR, sprintf("cell_%d_2014_data.csv", cc$mk$m)))
  }
}

# Rep loop -----------------------------------------------------------------
res_path    <- file.path(OUT_DIR, "sim2_estimates.csv")
lambda_true <- theta_true[["lambda"]]
for (r in seq_len(N_REPS)) {
  done <- if (file.exists(res_path)) unique(read.csv(res_path)$rep) else integer(0)
  if (r %in% done) next
  t0 <- Sys.time()
  cat(sprintf("\n=== full-model MC rep %d of %d ===\n", r, N_REPS))
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
                         out_path = file.path(COEF_DIR, sprintf("coefs_sim2_rep%02d.csv", r)),
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

  cat(sprintf("  rep %d done in %.1f min: lambda_hat = %.4f (truth %.4f)\n",
              r, as.numeric(difftime(Sys.time(), t0, units = "mins")),
              theta_r[["lambda"]], lambda_true))
}

sim2 <- read_csv(res_path, show_col_types = FALSE)
summ2 <- sim2 %>% group_by(term) %>%
  summarize(truth = first(truth), mean_est = mean(estimate),
            bias = mean(estimate - truth),
            rmse = sqrt(mean((estimate - truth)^2)),
            coverage = mean(abs(estimate - truth) <= 1.96 * se, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
cat("\n=== version 2 recovery (premiums optimal, commissions exogenous) ===\n")
print(as.data.frame(summ2), row.names = FALSE)
