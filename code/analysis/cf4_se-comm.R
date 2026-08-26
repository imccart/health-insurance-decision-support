# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Delta-method standard errors for the counterfactual welfare
##                statistics with respect to the commission-FOC coefficients
##                (delta, estimated in s4_cost-gmm; covariance from s5_se).
##                Phase 1 runs run_cf_cell in sensitivity mode at the saved cf1
##                solutions: per cell and scenario, d premium / d delta and
##                d commission / d delta by the implicit function theorem (numerical
##                Jacobian of the FOC at the solution, no solve). Phase 2 re-scores
##                welfare (score_cf_cell, the frozen kernels cf2/cf3 use) at premiums
##                and commissions moved along each sensitivity direction, +/- h, and
##                differences the headline statistics to get their gradient G in
##                delta. Var = G V_delta G'. Combined with cf3's demand-parameter
##                bootstrap SE (independent stages, the sequential convention) into
##                a total SE. Run after cf3, through the driver.
##
## Dependencies: preamble loaded by _analysis.R; cf1 (counterfactual_results.csv),
##               cf2 (counterfactual_welfare.csv), cf3 (cf_bootstrap_se.csv),
##               s4 (commission_foc_fit.csv), s5 (commission_foc_coefs_vcov.csv).

H_REL_JAC <- 1e-4          # relative FD step for the FOC Jacobian at the solution
H_SE_FRAC <- 0.5           # welfare FD step per coefficient, as a fraction of its SE
SENS_PATH <- file.path(TEMP_DIR, "cf_sensitivities.csv")
GRAD_PATH <- file.path(TEMP_DIR, "cf_delta_gradient.csv")
SE_PATH   <- "results/cf_delta_se.csv"

cat("=== CF delta-method SEs (commission-FOC coefficients) ===\n")

# Shared structural inputs (cells, cell_seeds, plan_choice, commission_lookup)
source("code/analysis/s1_inputs.R")
rm(hh_split); gc(verbose = FALSE)

# Static CF inputs (as cf1) -------------------------------------------------
coefs          <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE, lazy = FALSE)
lambda         <- setNames(coefs$estimate, coefs$term)[["lambda"]]
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE, lazy = FALSE)
rs_coefs_df    <- read_csv(file.path(TEMP_DIR, "ra_rs_coefs_gmm.csv"), show_col_types = FALSE)
claims_coefs_df <- read_csv(file.path(TEMP_DIR, "ra_claims_coefs_gmm.csv"), show_col_types = FALSE)
reins_df       <- read_csv(file.path(TEMP_DIR, "reinsurance_factors.csv"), show_col_types = FALSE, lazy = FALSE)
rs_coefs       <- setNames(rs_coefs_df$estimate, rs_coefs_df$term)
claims_coefs   <- setNames(claims_coefs_df$estimate, claims_coefs_df$term)
demand_spec    <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
STRUCTURAL_SPEC <- demand_spec$all
CS_TABLE       <- read.csv("data/input/ca_standard_cost_sharing.csv", stringsAsFactors = FALSE)

comm_foc_df   <- read_csv(file.path(TEMP_DIR, "commission_foc_fit.csv"), show_col_types = FALSE)
commission_mu <- setNames(comm_foc_df$mu_fit, paste(comm_foc_df$firm, comm_foc_df$year, sep = "_"))

# Commission-FOC coefficients: terms, covariance, and the covariate matrix z by
# firm-year (columns in the coefficient order; d1..d4 keeps the sensitivity
# column names plain).
delta_df    <- read_csv(file.path(TEMP_DIR, "commission_foc_coefs.csv"), show_col_types = FALSE)
delta_terms <- delta_df$term
stopifnot(identical(delta_terms, c("(Intercept)", "large", "pct", "broker_per_agent")))
read_vcov <- function(path) {
  d <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  M <- as.matrix(d[, -1]); rownames(M) <- d[[1]]; colnames(M) <- d[[1]]
  (M + t(M)) / 2
}
V_delta  <- read_vcov("results/commission_foc_coefs_vcov.csv")[delta_terms, delta_terms]
se_delta <- sqrt(diag(V_delta))
K <- length(delta_terms)
Z <- cbind(1, comm_foc_df$large, comm_foc_df$pct, comm_foc_df$broker_per_agent)
dimnames(Z) <- list(paste(comm_foc_df$firm, comm_foc_df$year, sep = "_"), paste0("d", seq_len(K)))
SENS <- list(z = Z, h_rel = H_REL_JAC)
cat("  coefficients:", paste(delta_terms, collapse = ", "), "\n")

# Saved cf1 solutions, per cell then per scenario: premiums by plan and the
# commission scale k by insurer prefix (comm_scale_cf is per plan, NA where the
# insurer's commission was held fixed).
cf_base <- as.data.table(read_csv("results/counterfactual_results.csv", show_col_types = FALSE, lazy = FALSE))
build_warm_start <- function(cfb) {
  out <- list()
  for (lab in unique(cfb$scenario)) {
    s <- cfb[scenario == lab]
    k_plan <- s[!is.na(comm_scale_cf), .(k = comm_scale_cf[1]), by = .(prefix = sub("_.*", "", plan_id))]
    out[[lab]] <- list(p = setNames(s$premium_cf, s$plan_id),
                       k = setNames(k_plan$k, k_plan$prefix))
  }
  out
}
warm_start_all <- lapply(split(cf_base, by = c("region", "year")), build_warm_start)

# =========================================================================
# PHASE 1: sensitivities dp/d delta, d eta/d delta per cell and scenario
# =========================================================================
cat("\nPhase 1: equilibrium sensitivities at the cf1 solutions...\n")
hh_all <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split_cf <- split(hh_all, by = c("region", "year"))
rm(hh_all); gc(verbose = FALSE)

n_cells_total <- nrow(cells)
tasks <- lapply(seq_len(n_cells_total), function(i) {
  key <- paste0(cells$region[i], ".", cells$year[i])
  hhs <- hh_split_cf[[key]]
  list(r = cells$region[i], y = cells$year[i], seed = cell_seeds[i], idx = i,
       n_total = n_cells_total, ws = warm_start_all[[key]],
       hhs = if (is.null(hhs) || nrow(hhs) == 0) NULL else as.data.frame(hhs))
})
rm(hh_split_cf); gc(verbose = FALSE)

SENS_CELL_DIR <- file.path(TEMP_DIR, "cf_sens")
if (dir.exists(SENS_CELL_DIR)) unlink(SENS_CELL_DIR, recursive = TRUE)
dir.create(SENS_CELL_DIR, recursive = TRUE)

run_one_sens <- function(task) {
  if (is.null(task$hhs) || is.null(task$ws)) return(NULL)
  t0 <- Sys.time()
  res <- tryCatch(
    run_cf_cell(task$r, task$y, task$seed, SAMPLE_FRAC, task$hhs,
                plan_choice, supply_results, coefs, commission_lookup,
                rs_coefs, claims_coefs, reins_df, STRUCTURAL_SPEC,
                warm_start = task$ws, commission_mu = commission_mu, sens = SENS),
    error = function(e) { cat(sprintf("  [cell %d/%d] r%s y%s ERROR: %s\n",
      task$idx, task$n_total, task$r, task$y, conditionMessage(e))); NULL }
  )
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (!is.null(res)) {
    data.table::fwrite(res, file.path(SENS_CELL_DIR, sprintf("cell_%s_%s.csv", task$r, task$y)))
    cat(sprintf("  [cell %d/%d] r%s y%s: %d scenarios, max cond(J) %.2g, %.0fs\n",
      task$idx, task$n_total, task$r, task$y, length(unique(res$scenario)),
      max(res$cond_J, na.rm = TRUE), el))
  }
  res
}

n_workers <- max(1L, parallel::detectCores() - 2L)
cl <- tryCatch(parallel::makeCluster(n_workers, type = "PSOCK", outfile = ""),
               error = function(e) NULL)
if (!is.null(cl)) {
  cat("  Parallel:", n_workers, "workers\n")
  parallel::clusterEvalQ(cl, {
    suppressMessages({ library(tidyverse); library(data.table); library(nleqslv) })
    source("code/data-build/_helpers.R")
    source("code/analysis/helpers/constants.R")
    source("code/analysis/helpers/covariates.R")
    source("code/analysis/helpers/choice.R")
    source("code/analysis/helpers/supply.R")
    source("code/analysis/helpers/ra.R")
    source("code/analysis/helpers/estimate_demand.R")
    source("code/analysis/helpers/welfare_objective.R")
    source("code/analysis/helpers/welfare_engine.R")
    data.table::setDTthreads(1)
  })
  parallel::clusterExport(cl, c("run_cf_cell", "SAMPLE_FRAC", "plan_choice",
    "supply_results", "coefs", "commission_lookup", "rs_coefs", "claims_coefs",
    "reins_df", "STRUCTURAL_SPEC", "CS_TABLE", "SENS_CELL_DIR", "commission_mu", "SENS"))
  sens_list <- parallel::parLapplyLB(cl, tasks, run_one_sens)
  parallel::stopCluster(cl)
} else {
  cat("  Cluster unavailable, running serial\n")
  sens_list <- lapply(tasks, run_one_sens)
}
rm(tasks); gc(verbose = FALSE)

sens_all <- rbindlist(sens_list[!vapply(sens_list, is.null, logical(1))])
write_csv(sens_all, SENS_PATH)
cat("  Sensitivities:", nrow(sens_all), "plan-scenario rows over",
    nrow(unique(sens_all[, .(region, year)])), "cells\n")

# =========================================================================
# PHASE 2: welfare gradient by frozen re-scoring along each sensitivity
# =========================================================================
cat("\nPhase 2: welfare gradients...\n")
source("code/analysis/helpers/welfare_objective.R")
source("code/analysis/helpers/welfare_engine.R")
source("code/analysis/helpers/score_cf.R")
COMM_TERMS        <- c("commission_broker")
SPENDING_SCHEDULE <- load_spending_schedule()
UNINS_SCHED       <- load_uninsured_oop()
CELL_DIR          <- file.path(TEMP_DIR, "choice_cells")
DELTA_HH_DIR      <- file.path(TEMP_DIR, "cf_delta_hh")   # per-household sink (transient)

# Scenario rows without a sensitivity (scenario absent from the cf1 solution set
# or a non-finite Jacobian) carry a zero sensitivity; reported.
cf_pert_base <- merge(cf_base[, .(region, year, scenario, plan_id, premium_cf, commission_pmpm, tau, mc, claims)],
                      sens_all, by = c("region", "year", "scenario", "plan_id"), all.x = TRUE)
dcols <- c(paste0("dp_d", seq_len(K)), paste0("deta_d", seq_len(K)))
n_missing <- sum(is.na(cf_pert_base[[dcols[1]]]))
for (cc in dcols) cf_pert_base[is.na(get(cc)), (cc) := 0]
cat("  rows without a sensitivity (set to zero):", n_missing, "of", nrow(cf_pert_base), "\n")

cells_cf <- unique(cf_base[, .(region, year)])
tasks <- lapply(seq_len(nrow(cells_cf)), function(i) list(r = cells_cf$region[i], y = cells_cf$year[i]))

score_one <- function(task) {
  tryCatch(score_cf_cell(task$r, task$y, cf_pert[region == task$r & year == task$y],
                         DELTA_HH_DIR, coefs, lambda),
           error = function(e) { cat("  ERR r", task$r, "y", task$y, ":", conditionMessage(e), "\n"); NULL })
}

cl <- tryCatch(parallel::makeCluster(n_workers, type = "PSOCK", outfile = ""), error = function(e) NULL)
if (!is.null(cl)) {
  parallel::clusterEvalQ(cl, {
    suppressMessages({ library(tidyverse); library(data.table) })
    source("code/data-build/_helpers.R"); source("code/analysis/helpers/constants.R")
    source("code/analysis/helpers/covariates.R"); source("code/analysis/helpers/choice.R")
    source("code/analysis/helpers/supply.R"); source("code/analysis/helpers/ra.R")
    source("code/analysis/helpers/estimate_demand.R")
    source("code/analysis/helpers/welfare_objective.R"); source("code/analysis/helpers/welfare_engine.R")
    data.table::setDTthreads(1)
  })
  parallel::clusterExport(cl, c("score_cf_cell", "coefs", "lambda", "supply_results",
    "STRUCTURAL_SPEC", "COMM_TERMS", "CS_TABLE", "SPENDING_SCHEDULE", "UNINS_SCHED",
    "CELL_DIR", "DELTA_HH_DIR", "TEMP_DIR"))
}

# One headline vector per (coefficient, sign): premiums and commissions moved by
# +/- h_k along the k-th sensitivity, welfare re-scored, statistics summarized.
headline_at <- function(k, sign) {
  h <- H_SE_FRAC * se_delta[k]
  cf_pert <<- copy(cf_pert_base)[, `:=`(
    premium_cf      = premium_cf      + sign * h * get(paste0("dp_d", k)),
    commission_pmpm = commission_pmpm + sign * h * get(paste0("deta_d", k)))]
  if (dir.exists(DELTA_HH_DIR)) unlink(DELTA_HH_DIR, recursive = TRUE)
  dir.create(DELTA_HH_DIR, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(cl)) {
    parallel::clusterExport(cl, "cf_pert", envir = environment())
    wl <- parallel::parLapplyLB(cl, tasks, score_one)
  } else wl <- lapply(tasks, score_one)
  w <- rbindlist(wl[!vapply(wl, is.null, logical(1))])
  summarize_cf_headline(w)
}

t0 <- Sys.time()
G <- NULL
for (k in seq_len(K)) {
  hp <- headline_at(k, +1)
  hm <- headline_at(k, -1)
  gk <- (hp - hm) / (2 * H_SE_FRAC * se_delta[k])
  G  <- if (is.null(G)) matrix(gk, ncol = 1, dimnames = list(names(gk), NULL)) else cbind(G, gk)
  cat(sprintf("  %-18s scored (+/-), %.1f min elapsed\n", delta_terms[k],
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
colnames(G) <- delta_terms
if (!is.null(cl)) parallel::stopCluster(cl)
unlink(DELTA_HH_DIR, recursive = TRUE)
write.csv(data.frame(statistic = rownames(G), G, check.names = FALSE), GRAD_PATH, row.names = FALSE)

# =========================================================================
# SEs: commission channel, then combined with the demand bootstrap
# =========================================================================
V_comm  <- G %*% V_delta %*% t(G)
dimnames(V_comm) <- list(rownames(G), rownames(G))
se_comm <- sqrt(pmax(diag(V_comm), 0))
# Full commission-channel covariance over the headline statistics, so sum2 can
# form the SE of any linear combination (the coverage and objective effects).
write.csv(data.frame(statistic = rownames(V_comm), V_comm, check.names = FALSE),
          "results/cf_delta_vcov.csv", row.names = FALSE)

pt <- tryCatch(summarize_cf_headline(read_csv("results/counterfactual_welfare.csv",
                                              show_col_types = FALSE, lazy = FALSE)),
               error = function(e) NULL)
boot <- if (file.exists("results/cf_bootstrap_se.csv"))
  read.csv("results/cf_bootstrap_se.csv", stringsAsFactors = FALSE) else NULL
se_demand <- if (is.null(boot)) rep(NA_real_, nrow(G)) else
  boot$se[match(rownames(G), boot$statistic)]

summ <- data.frame(
  statistic = rownames(G),
  point     = if (!is.null(pt)) pt[rownames(G)] else NA_real_,
  se_demand = se_demand,
  se_comm   = se_comm,
  se_total  = sqrt(se_demand^2 + se_comm^2),
  stringsAsFactors = FALSE
)
rownames(summ) <- NULL
write.csv(summ, SE_PATH, row.names = FALSE)

cat("\n  ->", SENS_PATH, "\n  ->", GRAD_PATH, "\n  ->", SE_PATH,
    "\n  -> results/cf_delta_vcov.csv\n\n")
print(summ %>% mutate(across(where(is.numeric), ~ round(., 3))), row.names = FALSE)
cat("\nCF delta-method SEs complete.\n")
