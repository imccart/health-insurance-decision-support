# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Parametric bootstrap for the counterfactual welfare statistics,
##                FROZEN-EQUILIBRIUM version. Each draw perturbs the demand
##                parameters by their estimated sampling distribution and RE-SCORES
##                welfare at the cf1 premiums (held fixed), rather than re-solving
##                the equilibrium. The spread across draws is the SE of the welfare
##                components. Run SEPARATELY from the main pipeline, after cf2.
##                  source("code/analysis/cf3_se.R")
##
## Design:
##   * Frozen scoring. Holding the cf1 premiums fixed and re-scoring at each demand
##     draw is validated: across draws the re-solved premiums move only a few dollars,
##     so the omitted premium-response channel is small, and it avoids the multiple-
##     equilibria noise the full re-solve carries for the commission-ban scenario.
##   * Demand params ~ N(theta_d, V_d) only. Cost params enter welfare through
##     premiums, which are frozen here, so they drop out of the re-scoring.
##     V_d is the demand sandwich vcov written by s5_se.R.
##   * lambda is clamped to (0.05, 0.999) to stay RUM-consistent (rarely binds).
##   * Per-draw component values are checkpointed to cf_bootstrap_draws.csv; sum2
##     reconstructs the coverage effect and the objective band from them and applies
##     the bias correction, so cf3 stays the raw draw generator.

# Packages and helpers are loaded by _analysis.R (top section + helpers) before this
# step runs, so the master does not re-source them. The cluster workers are separate
# processes and DO re-source the helpers, in clusterEvalQ below. MASS is called
# qualified (MASS::mvrnorm), never attached, to avoid masking dplyr::select.

# TEMP_DIR, SAMPLE_FRAC, MASTER_SEED, and N_BOOT_CF come from _analysis.R. cf3 is
# always run through the driver (top section + helpers), so there is no fallback.
BOOT_SEED  <- 987654321L
DRAWS_PATH <- "results/cf_bootstrap_draws.csv"
SE_PATH    <- "results/cf_bootstrap_se.csv"
HH_SINK    <- file.path(TEMP_DIR, "cf_boot_hh")   # per-draw per-household welfare (transient)

cat("=== CF parametric bootstrap ===\n  draws:", N_BOOT_CF, "\n")

# Shared structural inputs (cells, cell_seeds, hh_split, plan_choice, commission)
source("code/analysis/s1_inputs.R")

# The shared welfare scorer (score_cf_cell) and helpers are already loaded by the
# driver for the master, and re-sourced per worker in clusterEvalQ below. The frozen
# bootstrap never solves, so cf_cell.R is not needed. Scorer config:
CELL_DIR          <- file.path(TEMP_DIR, "choice_cells")
COMM_TERMS        <- c("commission_broker")
SPENDING_SCHEDULE <- load_spending_schedule()
UNINS_SCHED       <- load_uninsured_oop()   # uninsured valued at realized OOP + social cost

# Static CF inputs the skipped driver would have loaded. lazy = FALSE forces
# eager reads -- exported objects must not carry readr's ALTREP file connection,
# which is invalid on the cluster workers (serialize error otherwise).
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE, lazy = FALSE)
reins_df       <- read_csv(file.path(TEMP_DIR, "reinsurance_factors.csv"), show_col_types = FALSE, lazy = FALSE)
demand_spec    <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
STRUCTURAL_SPEC <- demand_spec$all
CS_TABLE       <- read.csv("data/input/ca_standard_cost_sharing.csv", stringsAsFactors = FALSE)

# Baseline cf1 equilibrium, per cell, to warm-start each draw's endogenous-scenario
# solves (helpers/cf_cell.R argument warm_start). A perturbed draw sits next to the
# baseline, so each cell starts near its answer and lands on the same spot in the
# soft commission valley -- faster, and it keeps the draws coherent so the SE spread
# reflects parameter uncertainty rather than where the solver happened to stop.
cf_base <- as.data.table(read_csv("results/counterfactual_results.csv",
                                  show_col_types = FALSE, lazy = FALSE))
build_warm <- function(r, y) {
  d <- cf_base[region == r & year == y & scenario != "observed"]
  if (nrow(d) == 0) return(NULL)
  ws <- list()
  for (s in unique(d$scenario)) {
    ds  <- d[scenario == s]
    kdt <- ds[is.finite(comm_scale_cf),
              .(k = first(comm_scale_cf)), by = .(pfx = sub("_.*", "", plan_id))]
    ws[[s]] <- list(p = setNames(ds$premium_cf, ds$plan_id),
                    k = if (nrow(kdt) > 0) setNames(kdt$k, kdt$pfx) else NULL)
  }
  ws
}

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
                     "obj_prem", "obj_eoop", "obj_risk",
                     "obj_insured", "share_unins", "unins_oop", "unins_mort", "unins_cat")])
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
  # Endogenous-commission scenario families (trimmed grids; endog_tau0 = observed,
  # so its delta is 0 by construction and is not carried).
  taus_e <- c(0.5, 1.0)
  grad_e <- vapply(taus_e, function(t) mdelta(sprintf("endog_tau%.2f", t), "cs_weighted"), numeric(1))
  names(grad_e) <- paste0("grad_cs_endog_tau", sprintf("%.2f", taus_e))
  # Cost-band components per scenario (all parameter-driven): coverage effect
  # (share_unins), insured-side composition (obj_insured), and the uninsured-weighted
  # OOP / baseline-mortality / catastrophic pieces. The objective welfare and its
  # low/central/high band are rebuilt from these in reporting with the uninsured cost
  # applied post-hoc, so bootstrapping these carries the parameter SE into the band.
  comp_scen <- c("zero_tau0.00", "zero_tau1.00", "uniform", "aligned",
                 "endog_tau1.00", "flat_mandate", "defund_1.00")
  comp <- unlist(lapply(comp_scen, function(s)
    setNames(c(mdelta(s, "share_unins"), mdelta(s, "obj_insured"), mdelta(s, "unins_oop"),
               mdelta(s, "unins_mort"),  mdelta(s, "unins_cat")),
             paste0(c("dshare_", "dobjins_", "doop_", "dmort_", "dcat_"), s))))
  # obj decomposed into premium / expected-OOP / risk (the same columns cf2 reports;
  # here they get bootstrap SEs, so the assumption-driven risk piece is inferable too).
  c(va_cs            = unname(grad["grad_cs_tau1.00"] - grad["grad_cs_tau0.00"]),
    grad,
    va_nav           = mdelta("zero_tau1.00", "cs_welfare_nav") - mdelta("zero_tau0.00", "cs_welfare_nav"),
    va_obj           = mdelta("zero_tau1.00", "cs_welfare_obj") - mdelta("zero_tau0.00", "cs_welfare_obj"),
    va_obj_prem      = mdelta("zero_tau1.00", "obj_prem") - mdelta("zero_tau0.00", "obj_prem"),
    va_obj_eoop      = mdelta("zero_tau1.00", "obj_eoop") - mdelta("zero_tau0.00", "obj_eoop"),
    va_obj_risk      = mdelta("zero_tau1.00", "obj_risk") - mdelta("zero_tau0.00", "obj_risk"),
    grad_e,
    va_cs_endog      = unname(grad_e["grad_cs_endog_tau1.00"]),
    va_nav_endog     = mdelta("endog_tau1.00", "cs_welfare_nav"),
    va_obj_endog     = mdelta("endog_tau1.00", "cs_welfare_obj"),
    flatmand_dcs     = mdelta("flat_mandate", "cs_weighted"),
    flatmand_obj     = mdelta("flat_mandate", "cs_welfare_obj"),
    defund_dcs       = mdelta("defund_1.00", "cs_weighted"),
    defund_obj       = mdelta("defund_1.00", "cs_welfare_obj"),
    aligned_dcs      = mdelta("aligned", "cs_weighted"),
    aligned_dcs_nc   = mdelta("aligned", "cs_nocomm"),
    aligned_nav      = mdelta("aligned", "cs_welfare_nav"),
    aligned_obj      = mdelta("aligned", "cs_welfare_obj"),
    aligned_obj_prem = mdelta("aligned", "obj_prem"),
    aligned_obj_eoop = mdelta("aligned", "obj_eoop"),
    aligned_obj_risk = mdelta("aligned", "obj_risk"),
    comp)
}

# Distributional headline stats for one draw: pool the per-household welfare that
# run_cf_cell(hh_sink=...) wrote this draw, form each household's effect vs its own
# observed choice, and return the share worse off (money + navigator rulers) for the
# key scenarios. Always returns the same fixed-length named vector (NA where a
# scenario is missing) so the per-draw rows stack cleanly.
DIST_SCEN <- c("zero_tau0.00", "zero_tau1.00", "aligned", "endog_tau0.50")
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

# Point estimates from cf2's saved welfare (counterfactual_welfare.csv), which carries
# the cost-band components; counterfactual_results.csv holds only premiums + cf1's
# provisional welfare and lacks the component columns.
pt <- tryCatch(summarize_cf_headline(read_csv("results/counterfactual_welfare.csv",
                                              show_col_types = FALSE, lazy = FALSE)),
               error = function(e) NULL)

# Tasks (one per cell). Frozen bootstrap: we do NOT re-solve the equilibrium. Each
# draw re-scores welfare at the drawn demand parameters holding the cf1 premiums
# fixed, so a cell needs only its id; score_cf_cell reloads the cached cell data and
# the premiums come from cf_base. This is far faster than re-solving per draw, and it
# is justified because the equilibrium premiums move only a few dollars across draws
# (validated), so the premium-response channel it omits is small.
n_cells_total <- nrow(cells)
tasks <- lapply(seq_len(nrow(cells)), function(i)
  list(r = cells$region[i], y = cells$year[i], idx = i, n_total = n_cells_total))
rm(hh_split); gc(verbose = FALSE)

# Worker that SCORES one cell at the current draw's demand parameters, holding the
# cf1 equilibrium premiums (from cf_base) fixed. No solve. Writes per-household
# welfare to HH_SINK, exactly as cf2 does. outfile="" streams one line per cell.
run_one_boot <- function(task) {
  cfb <- cf_base[region == task$r & year == task$y,
                 .(region, year, scenario, plan_id, premium_cf, commission_pmpm, tau)]
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
# tryCatch/finally guarantees the cluster is stopped even if a draw errors
# (top-level on.exit misbehaves in a sourced script, so it is not used here).
tryCatch(
for (b in seq_len(N_BOOT_CF)) {
  # Demand draw only (clamp lambda into the RUM-consistent interior). Frozen scoring
  # holds premiums fixed, so the cost parameters enter welfare only through premiums
  # and drop out of the re-scoring; we therefore do not draw them.
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
