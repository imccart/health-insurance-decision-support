# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Parametric bootstrap SEs for the counterfactual welfare
##                statistics. Each draw perturbs the demand parameters
##                (N(theta_d, V_d), V_d from s5_se.R) and re-scores welfare at the
##                cf1 premiums, held fixed. Writes per-draw values to
##                cf_bootstrap_draws.csv; sum2 reconstructs the coverage effect and
##                the objective band and applies the bias correction. Run after cf2,
##                through the driver:
##                  source("code/analysis/cf3_se.R")
##                Why frozen scoring rather than a re-solve: notes/decisions.md.

# Preamble and helpers come from _analysis.R; workers re-source the helpers in
# clusterEvalQ below. MASS::mvrnorm is called qualified, never attached.

# TEMP_DIR, N_BOOT_CF, etc. come from _analysis.R.
BOOT_SEED  <- 987654321L
DRAWS_PATH <- "results/cf_bootstrap_draws.csv"
SE_PATH    <- "results/cf_bootstrap_se.csv"
HH_SINK    <- file.path(TEMP_DIR, "cf_boot_hh")   # per-draw per-household welfare (transient)

cat("=== CF parametric bootstrap ===\n  draws:", N_BOOT_CF, "\n")

# Shared structural inputs (cells, cell_seeds, hh_split, plan_choice, commission)
source("code/analysis/s1_inputs.R")

# Scorer config (score_cf_cell is loaded by the driver, re-sourced per worker):
CELL_DIR          <- file.path(TEMP_DIR, "choice_cells")
COMM_TERMS        <- c("commission_broker")
SPENDING_SCHEDULE <- load_spending_schedule()
UNINS_SCHED       <- load_uninsured_oop()   # uninsured valued at realized OOP + social cost

# Static CF inputs. lazy = FALSE forces eager reads (readr ALTREP connections
# break on the cluster workers).
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE, lazy = FALSE)
reins_df       <- read_csv(file.path(TEMP_DIR, "reinsurance_factors.csv"), show_col_types = FALSE, lazy = FALSE)
demand_spec    <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
STRUCTURAL_SPEC <- demand_spec$all
CS_TABLE       <- read.csv("data/input/ca_standard_cost_sharing.csv", stringsAsFactors = FALSE)

# Baseline cf1 equilibrium per cell; cf_base premiums feed the frozen re-score.
cf_base <- as.data.table(read_csv("results/counterfactual_results.csv",
                                  show_col_types = FALSE, lazy = FALSE))
# Point estimates + sandwich covariances ----------------------------------
read_vcov <- function(path) {
  d <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  M <- as.matrix(d[, -1]); rownames(M) <- d[[1]]; colnames(M) <- d[[1]]
  (M + t(M)) / 2                                   # symmetrize FD asymmetry
}
coefs_hat <- read.csv("results/choice_coefficients_structural.csv", stringsAsFactors = FALSE)
Vd        <- read_vcov("results/choice_coefficients_structural_vcov.csv")
mu_d      <- setNames(coefs_hat$estimate, coefs_hat$term)[rownames(Vd)]

# Headline statistics from one CF result set: summarize_cf_headline() in
# helpers/cf_headline.R (shared with cf4_se-comm).

# Per-draw share worse off (money + navigator rulers) for the key scenarios, from the
# per-household files this draw wrote. Fixed-length named vector, NA where missing.
DIST_SCEN <- c("zero_tau0.00", "zero_tau1.00", "aligned", "endog_tau0.50")
dist_headline <- function(hh_dir) {
  nm  <- c(paste0("shareworse_obj_", DIST_SCEN), paste0("shareworse_nav_", DIST_SCEN))
  out <- setNames(rep(NA_real_, length(nm)), nm)
  files <- list.files(hh_dir, full.names = TRUE)
  if (length(files) == 0) return(out)
  d <- tryCatch(data.table::rbindlist(lapply(files, function(f) {
    h   <- data.table::fread(f)
    obs <- h[scenario == "baseline", .(region, year, household_number, o_obj = obj, o_nav = nav)]
    m   <- merge(h[scenario != "baseline"], obs, by = c("region", "year", "household_number"))
    m[, .(scenario, w, e_obj = obj - o_obj, e_nav = nav - o_nav)]
  })), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(out)
  for (s in DIST_SCEN) {
    ds <- d[scenario == s]; if (nrow(ds) == 0) next
    out[paste0("shareworse_obj_", s)] <- sum(ds$w * (ds$e_obj < 0)) / sum(ds$w)
    out[paste0("shareworse_nav_", s)] <- sum(ds$w * (ds$e_nav < 0)) / sum(ds$w)
  }
  out
}

# Point estimates from cf2's saved welfare (counterfactual_welfare.csv; has the
# component columns).
pt <- tryCatch(summarize_cf_headline(read_csv("results/counterfactual_welfare.csv",
                                              show_col_types = FALSE, lazy = FALSE)),
               error = function(e) NULL)

# Tasks, one per cell. Frozen re-score (no solve): score_cf_cell reloads the cached
# cell data and takes premiums from cf_base.
n_cells_total <- nrow(cells)
tasks <- lapply(seq_len(nrow(cells)), function(i)
  list(r = cells$region[i], y = cells$year[i], idx = i, n_total = n_cells_total))
rm(hh_split); gc(verbose = FALSE)

# Score one cell at the draw's demand parameters, cf1 premiums fixed; writes
# per-household welfare to HH_SINK.
run_one_boot <- function(task) {
  cfb <- cf_base[region == task$r & year == task$y,
                 .(region, year, scenario, plan_id, premium_cf, commission_pmpm, tau, mc)]
  if (nrow(cfb) == 0) return(NULL)
  t0  <- Sys.time()
  out <- tryCatch(score_cf_cell(task$r, task$y, cfb, HH_SINK, coefs_b, lambda_b),
                  error = function(e) NULL)
  el  <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  db  <- if (exists("draw_b")) draw_b else NA
  cat(sprintf("  [draw %s | cell %d/%d] r%s y%s: %s, %.1fs\n",
      as.character(db), task$idx, task$n_total, task$r, task$y,
      if (is.null(out)) "FAILED" else sprintf("%d rows", nrow(out)), el))
  out
}

# Cluster (set up once; static objects exported once, params per draw) -----
n_workers <- max(1L, parallel::detectCores() - 2L)
# outfile = "" streams each worker's per-cell progress to the console.
cl <- parallel::makeCluster(n_workers, type = "PSOCK", outfile = "")
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
  source("code/analysis/helpers/score_cf.R")
  data.table::setDTthreads(1)
})
parallel::clusterExport(cl, c("run_one_boot", "cf_base", "supply_results",
  "STRUCTURAL_SPEC", "CS_TABLE", "HH_SINK", "CELL_DIR", "COMM_TERMS",
  "SPENDING_SCHEDULE", "UNINS_SCHED"))
message("  Parallel: ", n_workers, " workers; ", length(tasks), " cells/draw")

# Draw loop ---------------------------------------------------------------
set.seed(BOOT_SEED)
n_clamp <- 0L
t0 <- Sys.time()
draws <- vector("list", N_BOOT_CF)
# finally stops the cluster even if a draw errors.
tryCatch(
for (b in seq_len(N_BOOT_CF)) {
  # Demand draw only; lambda clamped to the RUM interior.
  d_b <- MASS::mvrnorm(1, mu_d, Vd, tol = 1e-6)
  if (!is.na(d_b["lambda"])) {
    lam <- min(max(d_b["lambda"], 0.05), 0.999)
    if (lam != d_b["lambda"]) n_clamp <- n_clamp + 1L
    d_b["lambda"] <- lam
  }
  coefs_b <- data.frame(term = names(d_b), estimate = as.numeric(d_b),
                        stringsAsFactors = FALSE)
  lambda_b <- setNames(coefs_b$estimate, coefs_b$term)[["lambda"]]

  draw_b <- b
  parallel::clusterExport(cl, c("coefs_b", "draw_b", "lambda_b"), envir = environment())
  # Fresh per-household sink for this draw (workers write per cell; pooled below).
  unlink(HH_SINK, recursive = TRUE); dir.create(HH_SINK, recursive = TRUE, showWarnings = FALSE)
  message(sprintf("  --- draw %d/%d: scoring %d cells ---", b, N_BOOT_CF, length(tasks)))
  res  <- parallel::parLapplyLB(cl, tasks, run_one_boot)
  cf_b <- bind_rows(res[!vapply(res, is.null, logical(1))])
  stats <- if (nrow(cf_b) > 0) c(summarize_cf_headline(cf_b), dist_headline(HH_SINK)) else NULL

  if (!is.null(stats)) {
    draws[[b]] <- stats
    row <- data.frame(draw = b, as.list(stats), check.names = FALSE)
    if (b == 1L || !file.exists(DRAWS_PATH))
      write.csv(row, DRAWS_PATH, row.names = FALSE)
    else
      write.table(row, DRAWS_PATH, append = TRUE, sep = ",",
                  col.names = FALSE, row.names = FALSE)
  }

  if (b == 1L) {
    per <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    message(sprintf("  draw 1 done in %.1f min; projected total ~ %.1f min (%.1f h)",
                per, per * N_BOOT_CF, per * N_BOOT_CF / 60))
  } else {
    message(sprintf("  draw %d/%d done (%.1f min elapsed)",
                b, N_BOOT_CF, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
},
finally = try(parallel::stopCluster(cl), silent = TRUE))

# Summary -----------------------------------------------------------------
D <- do.call(rbind, draws[!vapply(draws, is.null, logical(1))])
if (is.null(D) || nrow(D) == 0) stop("No successful bootstrap draws.")

summ <- data.frame(
  statistic  = colnames(D),
  point      = if (!is.null(pt)) pt[colnames(D)] else NA_real_,
  boot_mean  = colMeans(D, na.rm = TRUE),
  se         = apply(D, 2, sd, na.rm = TRUE),
  ci_lo      = apply(D, 2, quantile, 0.025, na.rm = TRUE),
  ci_hi      = apply(D, 2, quantile, 0.975, na.rm = TRUE),
  n_draws    = apply(D, 2, function(x) sum(!is.na(x))),
  stringsAsFactors = FALSE
)
rownames(summ) <- NULL
write.csv(summ, SE_PATH, row.names = FALSE)

cat("\n  lambda draws clamped:", n_clamp, "of", N_BOOT_CF, "\n")
cat("  ->", DRAWS_PATH, "\n  ->", SE_PATH, "\n\n")
print(summ %>% mutate(across(where(is.numeric), ~round(., 3))), row.names = FALSE)
cat("\nCF bootstrap complete.\n")
