# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Monte Carlo validation, tier 1: demand-model parameter
##                recovery on fully synthetic data. Each rep generates
##                households and choices from the model's own timing
##                (enrollment from the expected inclusive value over channel
##                states, a channel drawn from a true first-stage multinomial,
##                a plan from the within-nest logit at the realized channel),
##                then estimates exactly as on real data: the channel
##                multinomial refit on the SIMULATED enrollees only, and the
##                production estimator and sandwich run on cell CSVs in the
##                production format. MC_SEL sets the unobserved
##                channel-selection strength (0 = correctly specified); the
##                summary tail reports bias, RMSE, and coverage per term
##                across every strength run so far. Standalone; not sourced by
##                the driver.
## Output:        results/simulations/sim1_estimates*.csv, sim1_summary.csv

suppressMessages({ library(tidyverse); library(data.table); library(nnet) })
setwd("C:/Users/immccar/SynologyDrive/work/research-projects/health-insurance-decision-support")
TEMP_DIR <- "D:/temp-research-data/health-insurance-decision-support"
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/estimate_demand.R")
source("code/analysis/helpers/se.R")

N_REPS  <- as.integer(Sys.getenv("MC_REPS", "50"))
# Unobserved channel selection (the sensitivity dial): a household type nu,
# unobserved by the estimator, raises both the agent-channel propensity
# (u_agent += s*nu) and the taste for generous plans (V += s*nu*av). At 0 the
# model is correctly specified; the sweep traces which estimates degrade as
# selection strengthens.
SEL_STRENGTH <- as.numeric(Sys.getenv("MC_SEL", "0"))
N_CELLS <- 20L     # markets
N_HH    <- 5000L   # households per market
J_RANGE <- 4:8     # inside plans per market (varied: with a common menu size
                   # the lambda and inside-constant scores are collinear at the
                   # optimizer's zero start and the BHHH matrix is singular)
MC_SEED <- 60318L
MC_DIR  <- file.path(TEMP_DIR, "sim1_cells")
OUT_DIR <- "results/simulations"
COEF_DIR <- file.path(TEMP_DIR, "sim1_coefs")
if (!dir.exists(COEF_DIR)) dir.create(COEF_DIR, recursive = TRUE)
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# Truth (near the real estimates; premium in $100/member/month, commission in
# $/member/month as in the real cells)
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
lambda_true <- theta_true[["lambda"]]
SPEC_PATH <- file.path(TEMP_DIR, "demand_spec_sim.csv")
write_demand_spec(BASE, ASST, SPEC_PATH)

# True first stage: channel utilities from market agent density z and a
# household demographic x (softmax over Unassisted / Navigator / Agent)
FS_TRUE <- list(nav = c(int = -0.6, z = 1.5, x = 0.5),
                agt = c(int = -0.9, z = 3.0, x = 0.8))

# Markets: density and menus (fixed across reps; the choices are what vary)
set.seed(MC_SEED)
markets <- lapply(seq_len(N_CELLS), function(m) {
  z <- runif(1, 0.1, 0.9)
  Jm <- sample(J_RANGE, 1)
  plans <- data.table(
    plan_id  = sprintf("P%d_%d", m, seq_len(Jm)),
    premium  = runif(Jm, 0.8, 3.0),          # net premium, $100/member/month
    av       = sample(c(0.6, 0.7, 0.8, 0.9), Jm, replace = TRUE),
    hmo      = as.integer(runif(Jm) < 0.4),
    brand1   = as.integer(seq_len(Jm) <= 2),
    comm_pmpm = ifelse(runif(Jm) < 0.3, 0, runif(Jm, 5, 25))
  )
  list(m = m, z = z, plans = plans)
})

lse <- function(v) { mx <- max(v); mx + log(sum(exp(v - mx))) }

simulate_rep <- function(r) {
  set.seed(MC_SEED + r * 1000L)
  cells <- lapply(markets, function(mk) {
    hh <- data.table(
      household_number = seq_len(N_HH) + mk$m * 1e6,
      hh_size = sample(1:4, N_HH, replace = TRUE, prob = c(.5, .25, .15, .1)),
      x = as.integer(runif(N_HH) < 0.5),
      # Rating factor: household-specific premium scaling (the synthetic analog
      # of age rating and subsidies; without within-market premium variation
      # the price terms are only weakly identified)
      rf = runif(N_HH, 0.7, 2.5),
      # Unobserved type for the selection dial
      nu = rnorm(N_HH)
    )
    # True channel probabilities
    uN <- FS_TRUE$nav["int"] + FS_TRUE$nav["z"] * mk$z + FS_TRUE$nav["x"] * hh$x
    uA <- FS_TRUE$agt["int"] + FS_TRUE$agt["z"] * mk$z + FS_TRUE$agt["x"] * hh$x +
      SEL_STRENGTH * hh$nu
    den <- 1 + exp(uN) + exp(uA)
    hh[, `:=`(p0 = 1 / den, pN = exp(uN) / den, pA = exp(uA) / den)]

    # Household-specific premiums and utilities (V0 = 0 on the outside row)
    pl <- mk$plans
    Jm <- nrow(pl)
    P_mat <- outer(hh$rf, pl$premium)                        # N_HH x Jm
    Vb <- theta_true["inside"] + theta_true["premium"] * P_mat +
      matrix(theta_true["av"] * pl$av + theta_true["hmo"] * pl$hmo +
               theta_true["brand1"] * pl$brand1,
             nrow(hh), Jm, byrow = TRUE) +
      theta_true["hh_size_prem"] * hh$hh_size * P_mat +
      SEL_STRENGTH * outer(hh$nu, pl$av)
    addN <- matrix(theta_true["assisted_av"] * pl$av, nrow(hh), Jm, byrow = TRUE) +
      theta_true["assisted_premium"] * P_mat
    addA <- matrix(theta_true["broker_av"] * pl$av +
                     theta_true["commission_broker"] * pl$comm_pmpm,
                   nrow(hh), Jm, byrow = TRUE) +
      theta_true["broker_premium"] * P_mat
    I0 <- apply(Vb / lambda_true, 1, lse)
    IN <- apply((Vb + addN) / lambda_true, 1, lse)
    IA <- apply((Vb + addA) / lambda_true, 1, lse)
    Ibar <- hh$p0 * I0 + hh$pN * IN + hh$pA * IA
    P_ins <- { l <- lambda_true * Ibar; m0 <- pmax(l, 0); exp(l - m0) / (exp(l - m0) + exp(-m0)) }
    hh[, enroll := as.integer(runif(.N) < P_ins)]
    u <- runif(N_HH)
    hh[, channel := fifelse(u < p0, "Unassisted", fifelse(u < p0 + pN, "Navigator", "Agent"))]

    # Plan choice at the realized channel
    Vc <- Vb + as.integer(hh$channel == "Navigator") * addN +
               as.integer(hh$channel == "Agent") * addA
    W <- exp(Vc / lambda_true - apply(Vc / lambda_true, 1, max))
    pr <- W / rowSums(W)
    pick <- max.col(matrix(runif(nrow(hh)), nrow(hh), Jm) < t(apply(pr, 1, cumsum)),
                    ties.method = "first")
    hh[, plan_pick := pick]

    list(mk = mk, hh = hh, P_mat = P_mat)
  })
  cells
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
                       comm_pmpm = rep(pl$comm_pmpm, nrow(hh)),
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
res_path <- if (SEL_STRENGTH == 0) file.path(OUT_DIR, "sim1_estimates.csv") else
  file.path(OUT_DIR, sprintf("sim1_estimates_sel%.2f.csv", SEL_STRENGTH))
cat("selection strength:", SEL_STRENGTH, "->", res_path, "\n")
for (r in seq_len(N_REPS)) {
  done <- if (file.exists(res_path)) unique(read.csv(res_path)$rep) else integer(0)
  if (r %in% done) next
  t0 <- Sys.time()
  cat(sprintf("\n=== MC rep %d of %d ===\n", r, N_REPS))
  cells <- simulate_rep(r)

  # First stage refit on the simulated ENROLLEES only, predicted for everyone
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
                         out_path = file.path(COEF_DIR, sprintf("coefs_sel%.2f_rep%02d.csv",
                                                               SEL_STRENGTH, r)),
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

# Summary across every run so far: bias, RMSE, and 95% coverage per term, for
# the baseline and each selection strength
files <- list.files(OUT_DIR, pattern = "^sim1_estimates.*\\.csv$", full.names = TRUE)
key <- c("lambda", "inside", "premium", "av", "assisted_av", "broker_av",
         "assisted_premium", "broker_premium", "commission_broker")
all_summ <- list()
for (f in files) {
  sel <- if (grepl("_sel", f)) as.numeric(sub(".*_sel([0-9.]+)\\.csv$", "\\1", f)) else 0
  mcres <- read_csv(f, show_col_types = FALSE)
  summ <- mcres %>%
    group_by(term) %>%
    summarize(truth    = first(truth),
              mean_est = mean(estimate),
              bias     = mean(estimate - truth),
              rmse     = sqrt(mean((estimate - truth)^2)),
              coverage = mean(abs(estimate - truth) <= 1.96 * se, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(selection = sel, reps = length(unique(mcres$rep)))
  all_summ[[f]] <- summ
  cat(sprintf("
=== selection strength %.2f (%d reps), key terms ===
",
              sel, length(unique(mcres$rep))))
  print(summ %>% filter(term %in% key) %>% arrange(match(term, key)) %>%
          select(-selection, -reps) %>%
          mutate(across(where(is.numeric), ~ round(.x, 4))) %>% as.data.frame(),
        row.names = FALSE)
}
write_csv(bind_rows(all_summ), file.path(OUT_DIR, "sim1_summary.csv"))
