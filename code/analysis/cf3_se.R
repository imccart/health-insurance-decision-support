# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Parametric bootstrap for the counterfactual welfare statistics.
##                Each draw perturbs the structural parameters by their estimated
##                sampling distribution and re-solves the CF, so the spread across
##                draws is the SE of the welfare outcomes. Run SEPARATELY from the
##                main pipeline -- it re-runs the CF per draw and is slow.
##                  source("code/analysis/cf3_se.R")
##
## Design:
##   * Demand params ~ N(theta_d, V_d) and cost params ~ N(theta_c, V_c) are drawn
##     INDEPENDENTLY. Justified because the cost-GMM down-weights the FOC block
##     (M3) ~5000x, so the cost estimates barely depend on demand -- the two
##     blocks are effectively orthogonal, and no joint covariance is needed.
##     V_d / V_c are the sandwich vcovs written by s5_se.R.
##   * Observed premiums and shares (supply_results) are DATA, held fixed across
##     draws; only the parameters move, propagating into the CF equilibria.
##   * lambda is clamped to (0.05, 0.999) to stay RUM-consistent (rarely binds).
##   * Per-draw headline statistics are checkpointed to cf_bootstrap_draws.csv
##     (BSOD-recoverable); the summary (SE + 2.5/97.5 percentile CI) goes to
##     cf_bootstrap_se.csv, alongside the point estimates from the saved CF.

# Packages + helpers (master + the set each worker re-sources). MASS is NOT
# attached -- it masks dplyr::select, which run_cf_cell uses; call MASS::mvrnorm
# qualified instead.
pacman::p_load(tidyverse, data.table, nleqslv, Matrix)
source("code/data-build/_helpers.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")
source("code/analysis/helpers/ra.R")
source("code/analysis/helpers/estimate_demand.R")
source("code/analysis/helpers/welfare_objective.R")
source("code/analysis/helpers/welfare_engine.R")

# Parameters. TEMP_DIR/SAMPLE_FRAC/MASTER_SEED/N_BOOT_CF flow from _analysis.R when
# sourced there; defaulted here for a standalone run.
if (!exists("TEMP_DIR"))    TEMP_DIR    <- "D:/temp-research-data/health-insurance-decision-support"
if (!exists("SAMPLE_FRAC")) SAMPLE_FRAC <- 0.05
if (!exists("MASTER_SEED")) MASTER_SEED <- 20260224
if (!exists("N_BOOT_CF"))   N_BOOT_CF   <- 30L
BOOT_SEED  <- 987654321L
DRAWS_PATH <- "results/cf_bootstrap_draws.csv"
SE_PATH    <- "results/cf_bootstrap_se.csv"
HH_SINK    <- file.path(TEMP_DIR, "cf_boot_hh")   # per-draw per-household welfare (transient)

cat("=== CF parametric bootstrap ===\n  draws:", N_BOOT_CF, "\n")

# Shared structural inputs (cells, cell_seeds, hh_split, plan_choice, commission)
source("code/analysis/s1_inputs.R")

# The per-cell CF solver, shared with cf1_estimate.R
source("code/analysis/helpers/cf_cell.R")

# Static CF inputs the skipped driver would have loaded. lazy = FALSE forces
# eager reads -- exported objects must not carry readr's ALTREP file connection,
# which is invalid on the cluster workers (serialize error otherwise).
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE, lazy = FALSE)
reins_df       <- read_csv(file.path(TEMP_DIR, "reinsurance_factors.csv"), show_col_types = FALSE, lazy = FALSE)
demand_spec    <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
STRUCTURAL_SPEC <- demand_spec$all
CS_TABLE       <- read.csv("data/input/ca_standard_cost_sharing.csv", stringsAsFactors = FALSE)

# Point estimates + sandwich covariances ----------------------------------
read_vcov <- function(path) {
  d <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  M <- as.matrix(d[, -1]); rownames(M) <- d[[1]]; colnames(M) <- d[[1]]
  (M + t(M)) / 2                                   # symmetrize FD asymmetry
}
coefs_hat <- read.csv("results/choice_coefficients_structural.csv", stringsAsFactors = FALSE)
Vd        <- read_vcov("results/choice_coefficients_structural_vcov.csv")
mu_d      <- setNames(coefs_hat$estimate, coefs_hat$term)[rownames(Vd)]

rs_hat <- read.csv(file.path(TEMP_DIR, "ra_rs_coefs_gmm.csv"), stringsAsFactors = FALSE)
cl_hat <- read.csv(file.path(TEMP_DIR, "ra_claims_coefs_gmm.csv"), stringsAsFactors = FALSE)
alpha_names <- rs_hat$term; gamma_names <- cl_hat$term
Vc   <- read_vcov("results/cost_coefficients_gmm_vcov.csv")
mu_c <- setNames(c(rs_hat$estimate, cl_hat$estimate), c(alpha_names, gamma_names))[rownames(Vc)]

# Headline statistics from one CF result set (mirrors 4_counterfactuals Phase 4)
summarize_cf_headline <- function(cf) {
  cf  <- as.data.frame(cf)
  obs <- unique(cf[cf$scenario == "observed",
                   c("region", "year", "cs_weighted", "cs_nocomm",
                     "cs_welfare_nav", "cs_welfare_obj",
                     "obj_prem", "obj_eoop", "obj_risk")])
  mdelta <- function(scen, col) {                  # mean over cells of (col[scen] - col[observed])
    s <- unique(cf[cf$scenario == scen, c("region", "year", col)])
    s <- s[!duplicated(s[c("region", "year")]), ]
    o <- obs[, c("region", "year", col)]; names(o)[3] <- "obsval"
    m <- merge(s, o, by = c("region", "year"))
    if (nrow(m) == 0) return(NA_real_)
    mean(m[[col]] - m$obsval, na.rm = TRUE)
  }
  taus <- c(0, 0.25, 0.5, 0.75, 1.0)
  grad <- vapply(taus, function(t) mdelta(sprintf("zero_tau%.2f", t), "cs_weighted"), numeric(1))
  names(grad) <- paste0("grad_cs_tau", sprintf("%.2f", taus))
  # obj decomposed into premium / expected-OOP / risk (the same columns cf2 reports;
  # here they get bootstrap SEs, so the assumption-driven risk piece is inferable too).
  c(va_cs            = unname(grad["grad_cs_tau1.00"] - grad["grad_cs_tau0.00"]),
    grad,
    va_nav           = mdelta("zero_tau1.00", "cs_welfare_nav") - mdelta("zero_tau0.00", "cs_welfare_nav"),
    va_obj           = mdelta("zero_tau1.00", "cs_welfare_obj") - mdelta("zero_tau0.00", "cs_welfare_obj"),
    va_obj_prem      = mdelta("zero_tau1.00", "obj_prem") - mdelta("zero_tau0.00", "obj_prem"),
    va_obj_eoop      = mdelta("zero_tau1.00", "obj_eoop") - mdelta("zero_tau0.00", "obj_eoop"),
    va_obj_risk      = mdelta("zero_tau1.00", "obj_risk") - mdelta("zero_tau0.00", "obj_risk"),
    aligned_dcs      = mdelta("aligned", "cs_weighted"),
    aligned_dcs_nc   = mdelta("aligned", "cs_nocomm"),
    aligned_nav      = mdelta("aligned", "cs_welfare_nav"),
    aligned_obj      = mdelta("aligned", "cs_welfare_obj"),
    aligned_obj_prem = mdelta("aligned", "obj_prem"),
    aligned_obj_eoop = mdelta("aligned", "obj_eoop"),
    aligned_obj_risk = mdelta("aligned", "obj_risk"))
}

# Distributional headline stats for one draw: pool the per-household welfare that
# run_cf_cell(hh_sink=...) wrote this draw, form each household's effect vs its own
# observed choice, and return the share worse off (money + navigator rulers) for the
# key scenarios. Always returns the same fixed-length named vector (NA where a
# scenario is missing) so the per-draw rows stack cleanly.
DIST_SCEN <- c("zero_tau0.00", "zero_tau1.00", "aligned")
dist_headline <- function(hh_dir) {
  nm  <- c(paste0("shareworse_obj_", DIST_SCEN), paste0("shareworse_nav_", DIST_SCEN))
  out <- setNames(rep(NA_real_, length(nm)), nm)
  files <- list.files(hh_dir, full.names = TRUE)
  if (length(files) == 0) return(out)
  d <- tryCatch(data.table::rbindlist(lapply(files, function(f) {
    h   <- data.table::fread(f)
    obs <- h[scenario == "observed", .(region, year, household_number, o_obj = obj, o_nav = nav)]
    m   <- merge(h[scenario != "observed"], obs, by = c("region", "year", "household_number"))
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

# Point estimates from the saved (full) CF, for reference
pt <- tryCatch(summarize_cf_headline(read_csv("results/counterfactual_results.csv",
                                              show_col_types = FALSE, lazy = FALSE)),
               error = function(e) NULL)

# Tasks (one per cell, household slice attached) ---------------------------
n_cells_total <- nrow(cells)
tasks <- lapply(seq_len(nrow(cells)), function(i) {
  key <- paste0(cells$region[i], ".", cells$year[i])
  hhs <- hh_split[[key]]
  list(r = cells$region[i], y = cells$year[i], seed = cell_seeds[i],
       idx = i, n_total = n_cells_total,
       hhs = if (is.null(hhs) || nrow(hhs) == 0) NULL else as.data.frame(hhs))
})
rm(hh_split); gc(verbose = FALSE)

# Worker that runs one cell at the CURRENT draw's parameters. run_cf_cell's own
# per-cell chatter is captured (swallowed) so only one concise progress line streams
# per cell; with the cluster's outfile="" these show live through the multi-hour
# draw, so the run reports where it is (draw, cell index, rows, convergence, seconds)
# instead of sitting on a blank cursor.
run_one_boot <- function(task) {
  if (is.null(task$hhs)) return(NULL)
  t0  <- Sys.time()
  out <- NULL
  tryCatch(
    capture.output(
      out <- run_cf_cell(task$r, task$y, task$seed, SAMPLE_FRAC, task$hhs,
                         plan_choice, supply_results, coefs_b, commission_lookup,
                         rs_coefs_b, claims_coefs_b, reins_df, STRUCTURAL_SPEC,
                         hh_sink = HH_SINK)),
    error = function(e) NULL)
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  db <- if (exists("draw_b")) draw_b else NA
  info <- if (is.null(out)) "FAILED" else {
    conv <- if ("nleqslv_termcd" %in% names(out))
      round(100 * mean(out$nleqslv_termcd <= 2, na.rm = TRUE)) else NA
    sprintf("%d rows, %s%% conv", nrow(out), conv)
  }
  cat(sprintf("  [draw %s | cell %d/%d] r%s y%s: %s, %.0fs\n",
      as.character(db), task$idx, task$n_total, task$r, task$y, info, el))
  out
}

# Cluster (set up once; static objects exported once, params per draw) -----
n_workers <- max(1L, parallel::detectCores() - 2L)
# outfile = "" lets each worker's per-cell progress line stream to the console live
# (a draw solves all 114 cells over hours; without this the console sits blank).
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
  data.table::setDTthreads(1)
})
parallel::clusterExport(cl, c("run_cf_cell", "run_one_boot", "SAMPLE_FRAC",
  "plan_choice", "supply_results", "commission_lookup", "reins_df",
  "STRUCTURAL_SPEC", "CS_TABLE", "HH_SINK"))
message("  Parallel: ", n_workers, " workers; ", length(tasks), " cells/draw")

# Draw loop ---------------------------------------------------------------
set.seed(BOOT_SEED)
n_clamp <- 0L
t0 <- Sys.time()
draws <- vector("list", N_BOOT_CF)
# tryCatch/finally guarantees the cluster is stopped even if a draw errors
# (top-level on.exit misbehaves in a sourced script, so it is not used here).
tryCatch(
for (b in seq_len(N_BOOT_CF)) {
  # Demand draw (clamp lambda into the RUM-consistent interior)
  d_b <- MASS::mvrnorm(1, mu_d, Vd, tol = 1e-6)
  if (!is.na(d_b["lambda"])) {
    lam <- min(max(d_b["lambda"], 0.05), 0.999)
    if (lam != d_b["lambda"]) n_clamp <- n_clamp + 1L
    d_b["lambda"] <- lam
  }
  coefs_b <- data.frame(term = names(d_b), estimate = as.numeric(d_b),
                        stringsAsFactors = FALSE)
  # Cost draw (split into risk-score alpha + claims gamma by name)
  c_b <- MASS::mvrnorm(1, mu_c, Vc, tol = 1e-6)
  rs_coefs_b     <- c_b[alpha_names]
  claims_coefs_b <- c_b[gamma_names]

  draw_b <- b
  parallel::clusterExport(cl, c("coefs_b", "rs_coefs_b", "claims_coefs_b", "draw_b"),
                          envir = environment())
  # Fresh per-household sink for this draw (workers write per cell; pooled below).
  unlink(HH_SINK, recursive = TRUE); dir.create(HH_SINK, recursive = TRUE, showWarnings = FALSE)
  message(sprintf("  --- draw %d/%d: solving %d cells ---", b, N_BOOT_CF, length(tasks)))
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
