# _joint_supply_demand.R — Joint demand+supply structural estimator (self-contained).
#
# Estimates (beta, lambda, alpha_risk, gamma_claims) jointly by GMM, stacking the
# demand score (M0) with the risk/claims/FOC moments (M1/M2/M3). Sources ONLY the
# frozen copies in this folder; the sequential pipeline is untouched.
#
# Runs the full estimation over all cells and writes joint coefficients side by
# side with the sequential ones. To experiment (subset cells, tweak weighting),
# use scratch/joint_smoke_test.R — keep this script clean and full-run-only.

pacman::p_load(tidyverse, data.table, Matrix, nleqslv, mlogit, fixest, parallel)
setDTthreads(1)

TEMP_DIR    <- "D:/temp-research-data/health-insurance-decision-support"
SAMPLE_FRAC <- 0.05
MASTER_SEED <- 20260224

jdir <- "code/analysis/joint-estimation"
for (f in c("kernels_constants.R","kernels_covariates.R","kernels_datahelpers.R",
            "kernels_demand.R","kernels_supply.R","kernels_ra.R",
            "cell_matrices.R","objective.R"))
  source(file.path(jdir, f))

# ---- shared inputs (copied from _inputs.R; nothing sourced) -----------------
cat("Loading shared structural data...\n")
hh_all   <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split <- split(hh_all, by = c("region", "year"))
cells    <- unique(hh_all[, .(region, year)])[order(region, year)]
rm(hh_all); gc(verbose = FALSE)
plan_choice       <- fread(file.path(TEMP_DIR, "plan_choice.csv")) %>% as_tibble()
commission_lookup <- fread("data/output/commission_lookup.csv")    %>% as_tibble()
set.seed(MASTER_SEED); cell_seeds <- sample.int(1e7, nrow(cells))
cat("  Cells:", nrow(cells), "\n")

# ---- spec ------------------------------------------------------------------
ds <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
STRUCTURAL_SPEC <- ds$base
DEMAND_COVARS   <- ds$all              # base + assisted; X-matrix column order
SPEC_BUILD      <- ds$all
K <- length(DEMAND_COVARS)
reins_df <- read_csv(file.path(TEMP_DIR, "reinsurance_factors.csv"), show_col_types = FALSE)

# ---- warm start (sequential demand MLE + sequential GMM cost coefs) ---------
ALPHA_NAMES <- c("(Intercept)", "Silver", "Gold", "Platinum", "share_18to34", "share_35to54")
GAMMA_NAMES <- c("(Intercept)", "log_risk_score", "HMO", "trend", "Anthem", "Blue_Shield", "Health_Net")
dem  <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
demv <- setNames(dem$estimate, dem$term)
beta0   <- unname(demv[DEMAND_COVARS]); beta0[is.na(beta0)] <- 0
lambda0 <- unname(demv["lambda"])
a_in <- read_csv(file.path(TEMP_DIR, "ra_rs_coefs_gmm.csv"), show_col_types = FALSE)
g_in <- read_csv(file.path(TEMP_DIR, "ra_claims_coefs_gmm.csv"), show_col_types = FALSE)
alpha0    <- unname(setNames(a_in$estimate, a_in$term)[ALPHA_NAMES])
gamma_cl0 <- unname(setNames(g_in$estimate, g_in$term)[GAMMA_NAMES]); gamma_cl0[is.na(gamma_cl0)] <- 0
theta0 <- c(beta0, lambda0, alpha0, gamma_cl0)
cat("  K (demand covars):", K, " | total params:", length(theta0),
    " | lambda0:", round(lambda0, 4), "\n")
stopifnot(length(theta0) == K + 14)

# ---- build cache (all cells) -----------------------------------------------
JC <- build_joint_cache(cells, cell_seeds, plan_choice, hh_split, SAMPLE_FRAC,
                        SPEC_BUILD, DEMAND_COVARS, STRUCTURAL_SPEC, reins_df,
                        commission_lookup, beta0, lambda0,
                        "data/output/rate_filing_rsdata.csv", which_cells = NULL)

# ---- parallel cluster: source kernels on workers, export JC once ------------
# kernel_outputs sweeps all cells per objective/gradient eval; that loop is the
# cost. Workers source the frozen kernels and hold one copy of JC (exported, not
# re-shipped per call); .par_kernel_one indexes worker-global JC by cell index.
n_cores  <- max(1L, detectCores() - 2L)
cl       <- makeCluster(n_cores)
jdir_abs <- normalizePath(jdir)
clusterExport(cl, "jdir_abs")
invisible(clusterEvalQ(cl, {
  suppressMessages(pacman::p_load(tidyverse, data.table, Matrix, nleqslv, mlogit, fixest))
  for (f in c("kernels_constants.R","kernels_covariates.R","kernels_datahelpers.R",
              "kernels_demand.R","kernels_supply.R","kernels_ra.R",
              "cell_matrices.R","objective.R"))
    source(file.path(jdir_abs, f))
  setDTthreads(1)
  TRUE
}))
clusterExport(cl, "JC")
cat("  parallel cluster:", n_cores, "workers\n")

# ---- warm-start diagnostic (M0 should be ~0 at the full-sample MLE) ---------
pp0   <- parse_theta(theta0, K)
kout0 <- kernel_outputs(pp0$beta, pp0$lambda, JC, cl = cl)
g0 <- moments_from_kernels(theta0, JC, kout0)
W  <- build_W2(theta0, JC, kout0 = kout0)   # optimal inverse-variance weighting (BHHH + robust)
bs <- block_sizes(JC); ends <- cumsum(bs); stx <- c(1, head(ends, -1) + 1)
cat(sprintf("theta0 blocks: M0 ||.||=%.4g (max|.|=%.4g)  M1=%.4g  M2=%.4g  M3=%.4g\n",
            sqrt(sum(g0[stx[1]:ends[1]]^2)), max(abs(g0[stx[1]:ends[1]])),
            sqrt(sum(g0[stx[2]:ends[2]]^2)), sqrt(sum(g0[stx[3]:ends[3]]^2)), sqrt(sum(g0[stx[4]:ends[4]]^2))))
cat("  (large M0 => cell mismatch; abort if so)\n")

# ---- optimize (parallel kernel sweep; per-eval trace checkpoint; SEs deferred) ----
lb <- rep(-Inf, length(theta0)); ub <- rep(Inf, length(theta0))
lb[K + 1] <- 0.001; ub[K + 1] <- 5
trace_path <- file.path(TEMP_DIR, "joint_trace.csv")
if (file.exists(trace_path)) file.remove(trace_path)   # fresh trace each run
cat("Optimizing (L-BFGS-B, hand-split gradient)... trace ->", trace_path, "\n")
fit <- optim(theta0, Q_joint, gr = grad_Q_split_flat, JC = JC, W = W, cl = cl, trace_file = trace_path,
             method = "L-BFGS-B", lower = lb, upper = ub,
             control = list(maxit = 200, factr = 1e7, pgtol = 1e-6, trace = 1, REPORT = 1))
cat("  convergence code:", fit$convergence, " | Q:", format(fit$value, digits = 6), "\n")

# ---- write outputs (side by side; never overwrite sequential) --------------
p <- parse_theta(fit$par, K)
write_csv(tibble(term = c(DEMAND_COVARS, "lambda"), estimate = c(p$beta, p$lambda)),
          "results/choice_coefficients_joint.csv")
write_csv(tibble(term = ALPHA_NAMES, estimate = p$alpha),    file.path(TEMP_DIR, "ra_rs_coefs_joint.csv"))
write_csv(tibble(term = GAMMA_NAMES, estimate = p$gamma_cl), file.path(TEMP_DIR, "ra_claims_coefs_joint.csv"))
cat("Joint estimation complete; coefficients written.\n")
cat(sprintf("  claims pass-through (joint): %.3f   (sequential GMM was 0.76; target ~1)\n", p$gamma_cl[2]))
cat(sprintf("  lambda (joint): %.4f vs warm start %.4f   premium beta: %.4f vs %.4f\n",
            p$lambda, lambda0, p$beta[1], beta0[1]))

# ---- solution diagnostics: FOC reduction, risk coefs, negative-MC count -----
g_hat <- g_joint(fit$par, JC, cl = cl)
cat(sprintf("  FOC residual ||M3||: %.4g (theta0) -> %.4g (joint)\n",
            sqrt(sum(g0[stx[4]:ends[4]]^2)), sqrt(sum(g_hat[stx[4]:ends[4]]^2))))
cat(sprintf("  risk-score age coefs (joint): %.3f, %.3f   (data-only ~ -1.8, -1.0; FOC-pulled-frozen was -2.6, -2.6)\n",
            p$alpha[5], p$alpha[6]))
kh <- kernel_outputs(p$beta, p$lambda, JC, cl = cl); a <- p$alpha; gcl <- p$gamma_cl
negc <- 0L; totc <- 0L; plat_min <- Inf
for (m in seq_along(JC$cells)) {
  cc <- JC$cells[[m]]; ko <- kh[[m]]; if (is.null(ko)) next
  plr <- a[1]+a[2]*cc$Silver+a[3]*cc$Gold+a[4]*cc$Platinum+a[5]*ko$d18+a[6]*ko$d35
  pcl <- gcl[1]+gcl[2]*plr+gcl[3]*cc$HMO+gcl[4]*cc$trend+gcl[5]*cc$Anthem+gcl[6]*cc$Blue_Shield+gcl[7]*cc$Health_Net
  shares <- ko$shares; prs <- exp(plr); pclm <- exp(pcl)
  avgp <- weighted.mean(cc$posted_premium, shares, na.rm = TRUE)
  mh <- MH_LOOKUP_JOINT[as.character(round(cc$plan_avs,1))]; mh[is.na(mh)] <- 1; util <- cc$plan_avs*mh
  ra <- (prs/sum(prs*shares,na.rm=TRUE) - util/sum(util*shares,na.rm=TRUE))*avgp
  mc <- pclm*(1-cc$reins_vec) - ra
  negc <- negc + sum(mc < 0); totc <- totc + length(mc)
  if (any(cc$Platinum==1)) plat_min <- min(plat_min, min(mc[cc$Platinum==1]))
}
cat(sprintf("  negative MC at joint solution: %d / %d   (min platinum mc: %.0f)\n", negc, totc, plat_min))

stopCluster(cl)
