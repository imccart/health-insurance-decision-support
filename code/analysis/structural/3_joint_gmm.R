# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-09
## Description:   Joint demand-supply GMM estimation.
##                Stacks demand score equations with cost/FOC moments.
##                The FOC moments depend on both demand parameters (through
##                shares, elasticities, Omega) and cost parameters (through MC).
##                This creates cross-equation restrictions that discipline
##                beta_premium: the pricing FOC constrains demand elasticities.
##
##                Four moment blocks:
##                  M_D: Demand score (gradient of log-likelihood)
##                  M1:  Risk score moments (rate filing data)
##                  M2:  Claims moments (rate filing data)
##                  M3:  FOC moments (recomputed each evaluation)
##
##                Two-step feasible GMM with block-diagonal weighting.
##
##                Standalone script — does not modify 3_cost_gmm.R or
##                _structural.R. Run manually to test.

# Setup (idempotent — safe to re-source) -----------------------------------
source("code/0-setup.R")
library(arrow)
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")
source("code/analysis/helpers/ra.R")
source("code/analysis/helpers/estimate_demand.R")

SAMPLE_FRAC   <- as.numeric(Sys.getenv("SAMPLE_FRAC"))
MASTER_SEED   <- as.integer(Sys.getenv("MASTER_SEED"))
TEMP_DIR      <- Sys.getenv("TEMP_DIR")
CELL_DIR      <- file.path(TEMP_DIR, "choice_cells")
PARTITION_DIR <- file.path(TEMP_DIR, "hh_choice_partitions")

demand_spec <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
STRUCTURAL_SPEC <- demand_spec$base
STRUCTURAL_ASST <- demand_spec$assisted
ALL_COVARS <- c(STRUCTURAL_SPEC, STRUCTURAL_ASST)

cat("=== Joint Demand-Supply GMM ===\n")

# =========================================================================
# PHASE A: Load demand cells (compact structures for score computation)
# =========================================================================

cat("\nPhase A: Loading demand cells...\n")
loaded <- load_all_cells(CELL_DIR, ALL_COVARS, filter_assisted = -1L)
demand_cells <- loaded$cells
total_hh <- loaded$total_hh
rm(loaded)
demand_cells <- normalize_weights(demand_cells)

K <- length(ALL_COVARS)
cat("  Demand: K =", K, "covariates,", total_hh, "HH,", length(demand_cells), "cells\n")

# =========================================================================
# PHASE B: Load supply cells (full cell_data for elasticity recomputation)
# =========================================================================

cat("\nPhase B: Loading supply cells...\n")

plan_choice <- read_csv(file.path(TEMP_DIR, "plan_choice.csv"), show_col_types = FALSE)
commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

# Reinsurance factors
rsdata_reins <- read_csv("data/output/rate_filing_rsdata.csv", show_col_types = FALSE)
reins_df <- rsdata_reins %>%
  select(plan_name, year, reins_factor) %>%
  filter(!is.na(reins_factor))
rm(rsdata_reins)

partition_files <- list.files(PARTITION_DIR, pattern = "^hh_\\d+_\\d+\\.parquet$",
                               full.names = FALSE)
cells_meta <- tibble(file = partition_files) %>%
  mutate(
    region = as.integer(str_extract(file, "(?<=hh_)\\d+")),
    year   = as.integer(str_extract(file, "(?<=_)\\d{4}"))
  ) %>%
  arrange(region, year)

set.seed(MASTER_SEED)
cell_seeds <- sample.int(1e7, nrow(cells_meta))

supply_cells <- vector("list", nrow(cells_meta))
n_loaded <- 0L

for (i in seq_len(nrow(cells_meta))) {
  r <- cells_meta$region[i]
  y <- cells_meta$year[i]

  set.seed(cell_seeds[i])
  hhs <- tryCatch(
    read_parquet(file.path(PARTITION_DIR, paste0("hh_", r, "_", y, ".parquet"))),
    error = function(e) NULL
  )
  if (is.null(hhs) || nrow(hhs) == 0) next

  plans <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans) == 0) { rm(hhs); next }

  if (!"comm_pmpm" %in% names(plans)) {
    cv <- get_commission_pmpm(unique(plans$plan_name), plans, y, commission_lookup)
    plans <- plans %>% left_join(tibble(plan_name = names(cv), comm_pmpm = unname(cv)),
                                  by = "plan_name")
  }

  build_result <- build_supply_choice_data(plans, hhs, SAMPLE_FRAC,
                                            spec = STRUCTURAL_SPEC)
  rm(hhs, plans)
  if (is.null(build_result)) next

  cd <- build_result$cell_data
  pa <- build_result$plan_attrs
  rm(build_result)

  # Assisted x metal interactions
  if (!"assisted_silver" %in% names(cd)) {
    cd$assisted_silver <- cd$assisted * cd$silver
    cd$assisted_bronze <- cd$assisted * cd$bronze
    if ("gold" %in% names(cd)) cd$assisted_gold <- cd$assisted * cd$gold
    if ("platinum" %in% names(cd)) cd$assisted_plat <- cd$assisted * cd$platinum
  }
  if (!"commission_broker" %in% names(cd)) {
    if ("any_agent" %in% names(cd)) {
      cd$commission_broker <- cd$comm_pmpm * ifelse(cd$any_agent == 1L, cd$assisted, 0L)
    } else {
      cd$commission_broker <- cd$comm_pmpm * cd$assisted
    }
  }
  if ("v_hat" %in% names(cd) && "commission_broker" %in% names(cd)) {
    cd$v_hat_commission <- cd$v_hat * cd$commission_broker
  }

  # CF interactions (v_hat x plan indicators)
  if ("v_hat" %in% names(cd)) {
    menu <- get_covariate_menu()
    for (cf_term in intersect(ALL_COVARS, names(menu))) {
      if (identical(menu[[cf_term]]$type, "cf_interaction")) {
        raw_col <- menu[[cf_term]]$raw_demo
        if (raw_col %in% names(cd)) cd[[cf_term]] <- cd$v_hat * cd[[raw_col]]
      }
    }
  }

  plan_names_cell <- sort(pa$plan_name)
  J <- length(plan_names_cell)
  if (J < 2) { rm(cd, pa); next }

  pa_sorted <- pa[match(plan_names_cell, pa$plan_name), ]
  posted_premium <- setNames(pa_sorted$premium_posted, pa_sorted$plan_name)
  plan_metal     <- setNames(pa_sorted$metal, pa_sorted$plan_name)
  plan_avs       <- setNames(pa_sorted$av, pa_sorted$plan_name)
  comm_vec       <- if ("comm_pmpm" %in% names(pa_sorted)) {
    setNames(pa_sorted$comm_pmpm, pa_sorted$plan_name)
  } else {
    setNames(rep(0, J), plan_names_cell)
  }

  rf_cell <- reins_df %>% filter(year == y)
  reins_vec <- sapply(plan_names_cell, function(pn) {
    rf <- rf_cell$reins_factor[rf_cell$plan_name == pn]
    if (length(rf) == 0) return(0)
    mean(rf, na.rm = TRUE)
  })

  own_mat <- build_ownership_matrix(plan_names_cell)
  benchmark_plan <- identify_benchmark(pa)

  plan_chars <- tibble(
    plan_name   = plan_names_cell,
    Silver      = as.integer(unname(plan_metal) == "Silver"),
    Gold        = as.integer(unname(plan_metal) == "Gold"),
    Platinum    = as.integer(unname(plan_metal) == "Platinum"),
    HMO         = unname(setNames(pa_sorted$hmo, pa_sorted$plan_name)[plan_names_cell]),
    trend       = y - 2014L,
    Anthem      = as.integer(grepl("^ANT", plan_names_cell)),
    Blue_Shield = as.integer(grepl("^BS", plan_names_cell)),
    Health_Net  = as.integer(grepl("^HN", plan_names_cell)),
    Kaiser      = as.integer(grepl("^KA", plan_names_cell))
  )

  n_loaded <- n_loaded + 1L
  supply_cells[[n_loaded]] <- list(
    cell_data      = cd,
    plan_attrs     = pa,
    plan_names     = plan_names_cell,
    own_mat        = own_mat,
    benchmark_plan = benchmark_plan,
    posted_premium = posted_premium,
    comm_vec       = comm_vec,
    plan_avs       = plan_avs,
    reins_vec      = reins_vec,
    plan_chars     = plan_chars,
    region         = r,
    year           = y
  )

  rm(cd, pa, pa_sorted, posted_premium, plan_metal, plan_avs, comm_vec,
     reins_vec, own_mat, benchmark_plan, plan_chars, rf_cell)
  if (i %% 20 == 0) { gc(verbose = FALSE); cat("    Cell", i, "/", nrow(cells_meta), "\n") }
}

supply_cells <- supply_cells[seq_len(n_loaded)]
rm(plan_choice, commission_lookup, reins_df)
gc(verbose = FALSE)
cat("  Supply cells loaded:", n_loaded, "\n")

# =========================================================================
# PHASE C: Load rate filing data (M1, M2 moments)
# =========================================================================

cat("\nPhase C: Loading rate filing data...\n")

rsdata <- read_csv("data/output/rate_filing_rsdata.csv", show_col_types = FALSE)
plan_demo <- read_csv(file.path(TEMP_DIR, "plan_demographics.csv"), show_col_types = FALSE)
rsdata <- rsdata %>% left_join(plan_demo, by = c("plan_name", "year"))
rm(plan_demo)

rsdata <- rsdata %>%
  filter(!is.na(log_risk_score), is.finite(log_risk_score),
         !is.na(log_cost), is.finite(log_cost),
         EXP_MM > 0,
         !is.na(share_18to34), !is.na(share_35to54), !is.na(share_hispanic))

cat("  Rate filing observations:", nrow(rsdata), "\n")

w_rf <- sqrt(rsdata$EXP_MM)
X_rs <- as.matrix(rsdata %>% select(Silver, Gold, Platinum,
                                      share_18to34, share_35to54, share_hispanic))
y_rs <- rsdata$log_risk_score
X_cl_exog <- as.matrix(rsdata %>% select(HMO, trend, Anthem, Blue_Shield, Health_Net))
y_cl <- rsdata$log_cost
Z_rs <- cbind(1, X_rs)
Z_cl <- cbind(1, X_cl_exog)
rm(rsdata)

MH_LOOKUP <- c("0.6" = 1.00, "0.7" = 1.03, "0.8" = 1.08, "0.9" = 1.15)

N_ALPHA <- 7L
N_GAMMA <- 7L
N_Z_FOC <- 9L

alpha_names <- c("(Intercept)", "Silver", "Gold", "Platinum",
                 "share_18to34", "share_35to54", "share_hispanic")
gamma_names <- c("(Intercept)", "log_risk_score", "HMO", "trend",
                 "Anthem", "Blue_Shield", "Health_Net")

N_DEMAND <- K + 1L  # K betas + lambda
N_COST   <- N_ALPHA + N_GAMMA
N_PARAMS <- N_DEMAND + N_COST
N_MOMENTS <- N_DEMAND + ncol(Z_rs) + ncol(Z_cl) + N_Z_FOC

cat("  Parameters:", N_PARAMS, "(demand:", N_DEMAND, " cost:", N_COST, ")\n")
cat("  Moments:", N_MOMENTS, "(M_D:", N_DEMAND,
    " M1:", ncol(Z_rs), " M2:", ncol(Z_cl), " M3:", N_Z_FOC, ")\n")

# =========================================================================
# STARTING VALUES (from sequential estimation)
# =========================================================================

cat("\nLoading starting values...\n")

coefs_demand <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
beta0 <- coefs_demand$estimate[coefs_demand$term != "lambda"]
lambda0 <- coefs_demand$estimate[coefs_demand$term == "lambda"]
cat("  Demand: beta_premium =", round(beta0[1], 6), " lambda =", round(lambda0, 4), "\n")

rs_coefs_start <- read_csv(file.path(TEMP_DIR, "ra_rs_coefs.csv"), show_col_types = FALSE)
cl_coefs_start <- read_csv(file.path(TEMP_DIR, "ra_claims_coefs.csv"), show_col_types = FALSE)
alpha0 <- unname(setNames(rs_coefs_start$estimate, rs_coefs_start$term)[alpha_names])
gamma0_raw <- setNames(cl_coefs_start$estimate, cl_coefs_start$term)[gamma_names]
gamma0_raw[is.na(gamma0_raw)] <- 0
gamma0 <- unname(gamma0_raw)
cat("  Cost: theta_r (OLS) =", round(gamma0[2], 4), "\n")

theta0 <- c(beta0, lambda0, alpha0, gamma0)

# Helper: build coefs tibble from beta vector + lambda
make_coefs_tibble <- function(beta, lambda) {
  tibble(term = c(ALL_COVARS, "lambda"),
         estimate = c(beta, lambda))
}

# =========================================================================
# JOINT MOMENT FUNCTION
# =========================================================================

cat("\nBuilding joint moment function...\n")

compute_joint_g_bar <- function(theta) {

  beta   <- theta[1:K]
  lambda <- theta[K + 1]
  alpha  <- theta[(K + 2):(K + 1 + N_ALPHA)]
  gamma  <- theta[(K + 2 + N_ALPHA):(K + 1 + N_ALPHA + N_GAMMA)]

  # Lambda sanity check (L-BFGS-B enforces bounds, but just in case)
  if (lambda <= 0 || lambda >= 10) return(rep(1e6, N_MOMENTS))

  # --- M_D: Demand score (gradient of NLL, should be 0 at optimum) ---
  acc <- tryCatch(
    accumulate(c(beta, lambda), demand_cells, compute_grad = TRUE),
    error = function(e) NULL
  )
  if (is.null(acc) || !is.finite(acc$negll)) return(rep(1e6, N_MOMENTS))
  g_demand <- acc$grad / total_hh

  # --- M1: Risk score residuals ---
  pred_log_rs_rf <- alpha[1] + X_rs %*% alpha[2:N_ALPHA]
  eps_rs <- as.vector(y_rs - pred_log_rs_rf) * w_rf
  g_rs <- colMeans(Z_rs * eps_rs)

  # --- M2: Claims residuals ---
  pred_log_cl_rf <- gamma[1] + gamma[2] * pred_log_rs_rf + X_cl_exog %*% gamma[3:N_GAMMA]
  eps_cl <- as.vector(y_cl - pred_log_cl_rf) * w_rf
  g_cl <- colMeans(Z_cl * eps_cl)

  # --- M3: FOC residuals (recomputed with current beta, lambda) ---
  coefs_current <- make_coefs_tibble(beta, lambda)
  rs_coefs_vec <- setNames(alpha, alpha_names)
  cl_coefs_vec <- setNames(gamma, gamma_names)

  g_foc_sum <- rep(0, N_Z_FOC)
  n_foc <- 0L

  for (sc in supply_cells) {
    J <- length(sc$plan_names)
    pn <- sc$plan_names

    # Recompute shares and elasticities with current demand params
    V <- tryCatch(compute_utility(sc$cell_data, coefs_current)$V,
                  error = function(e) NULL)
    if (is.null(V)) next

    se <- tryCatch(
      compute_shares_and_elasticities(
        sc$cell_data, V, lambda, sc$benchmark_plan, sc$plan_attrs,
        coefs_current, spec = STRUCTURAL_SPEC),
      error = function(e) NULL
    )
    if (is.null(se)) next

    Omega <- -sc$own_mat * se$elast_mat

    br <- tryCatch(
      compute_broker_shares_and_elasticities(
        sc$cell_data, V, lambda, sc$benchmark_plan, sc$plan_attrs,
        coefs_current, spec = STRUCTURAL_SPEC),
      error = function(e) NULL
    )
    Omega_B <- if (!is.null(br)) {
      -sc$own_mat * br$broker_elast_mat
    } else {
      matrix(0, J, J)
    }

    # Demographic shares → risk scores → claims → RA → MC
    demo <- tryCatch(compute_demographic_shares(sc$cell_data, V, lambda),
                     error = function(e) NULL)
    mc_res <- tryCatch(
      compute_mc(rs_coefs_vec, cl_coefs_vec, sc$plan_chars,
                  demo, se$shares, mean(sc$posted_premium),
                  sc$plan_avs, sc$reins_vec),
      error = function(e) NULL
    )
    if (is.null(mc_res)) next

    # RA FOC (adverse selection channel)
    rs_levels <- mc_res$predicted_risk_scores
    ra_foc <- tryCatch(
      compute_ra_foc(rs_levels, se$shares, sc$plan_avs,
                      mean(sc$posted_premium), se$elast_mat, sc$own_mat),
      error = function(e) rep(0, J)
    )

    # FOC residual (correct sign: MINUS Omega*(p-mc))
    foc_resid <- se$shares[pn] + ra_foc[pn] -
      as.vector(Omega %*% (sc$posted_premium[pn] - mc_res$mc[pn])) +
      as.vector(Omega_B %*% sc$comm_vec[pn])

    if (any(!is.finite(foc_resid))) next

    Z_cell <- cbind(1, sc$plan_chars$Silver, sc$plan_chars$Gold,
                    sc$plan_chars$Platinum, sc$plan_chars$HMO,
                    sc$plan_chars$trend, sc$plan_chars$Anthem,
                    sc$plan_chars$Blue_Shield, sc$plan_chars$Health_Net)
    g_foc_sum <- g_foc_sum + colSums(Z_cell * foc_resid)
    n_foc <- n_foc + J
  }

  if (n_foc == 0) return(rep(1e6, N_MOMENTS))
  g_foc <- g_foc_sum / n_foc

  c(g_demand, g_rs, g_cl, g_foc)
}

# =========================================================================
# GMM OBJECTIVE
# =========================================================================

gmm_objective <- function(theta, W) {
  g <- compute_joint_g_bar(theta)
  if (any(!is.finite(g))) return(1e20)
  as.numeric(t(g) %*% W %*% g)
}

# =========================================================================
# INITIAL DIAGNOSTICS
# =========================================================================

cat("\nChecking initial moments at sequential estimates...\n")
g_init <- compute_joint_g_bar(theta0)
cat("  any NA/NaN/Inf:", any(!is.finite(g_init)), "\n")

if (any(!is.finite(g_init))) {
  cat("  Non-finite moments — cannot proceed.\n")
  cat("  M_D (demand):", sum(!is.finite(g_init[1:N_DEMAND])), "non-finite\n")
  cat("  M1:", sum(!is.finite(g_init[(N_DEMAND+1):(N_DEMAND+ncol(Z_rs))])), "\n")
  cat("  M2:", sum(!is.finite(g_init[(N_DEMAND+ncol(Z_rs)+1):(N_DEMAND+ncol(Z_rs)+ncol(Z_cl))])), "\n")
  cat("  M3:", sum(!is.finite(g_init[(N_MOMENTS-N_Z_FOC+1):N_MOMENTS])), "\n")
  stop("Non-finite initial moments")
}

# Moment norms by block
md_end <- N_DEMAND
m1_end <- md_end + ncol(Z_rs)
m2_end <- m1_end + ncol(Z_cl)
cat("  ||M_D|| =", round(sqrt(sum(g_init[1:md_end]^2)), 6), "\n")
cat("  ||M1||  =", round(sqrt(sum(g_init[(md_end+1):m1_end]^2)), 4), "\n")
cat("  ||M2||  =", round(sqrt(sum(g_init[(m1_end+1):m2_end]^2)), 4), "\n")
cat("  ||M3||  =", round(sqrt(sum(g_init[(m2_end+1):N_MOMENTS]^2)), 6), "\n")

# =========================================================================
# STEP 1: BLOCK-DIAGONAL WEIGHTED GMM
# =========================================================================

cat("\n--- Joint GMM Step 1 (identity weighting) ---\n")

# Identity weighting for Step 1. The demand score is ~0 at MLE starting
# values, so inverse-variance weighting gives it near-infinite weight and
# the optimizer blows up. Identity lets all moments contribute naturally;
# Step 2 will use proper block-diagonal weighting once parameters have moved.
W1 <- diag(N_MOMENTS)

cat("  Using identity weighting matrix.\n")

cat("  Starting optim(BFGS)...\n"); flush.console()

# Track iteration count for reporting
iter_count <- 0L
gmm_obj_trace <- function(theta, W) {
  iter_count <<- iter_count + 1L
  val <- gmm_objective(theta, W)
  if (iter_count %% 10 == 0 || iter_count <= 3) {
    beta_p <- theta[1]
    lam <- theta[K + 1]
    theta_r <- theta[K + 1 + N_ALPHA + 2]  # gamma[2] = log_risk_score coef
    cat(sprintf("    iter %3d: obj = %.6f  beta_prem = %.6f  lambda = %.4f  theta_r = %.4f\n",
                iter_count, val, beta_p, lam, theta_r))
    flush.console()
  }
  val
}

# Parameter scaling for proper finite differences and step sizes
pscale <- pmax(abs(theta0), 1e-4)

result1 <- optim(
  par = theta0,
  fn = gmm_obj_trace,
  W = W1,
  method = "BFGS",
  control = list(maxit = 500, reltol = 1e-10,
                 parscale = pscale, trace = 0)
)

cat("  Converged:", result1$convergence == 0, "\n")
cat("  Objective:", format(result1$value, digits = 6), "\n")

beta1   <- result1$par[1:K]
lambda1 <- result1$par[K + 1]
alpha1  <- result1$par[(K + 2):(K + 1 + N_ALPHA)]
gamma1  <- result1$par[(K + 2 + N_ALPHA):(K + 1 + N_ALPHA + N_GAMMA)]

cat("  beta_premium (Step 1):", round(beta1[1], 6), "\n")
cat("  lambda (Step 1):", round(lambda1, 4), "\n")
cat("  theta_r (Step 1):", round(gamma1[2], 4), "\n")

g1 <- compute_joint_g_bar(result1$par)
cat("  Moment norms (Step 1):\n")
cat("    ||M_D|| =", round(sqrt(sum(g1[1:md_end]^2)), 6), "\n")
cat("    ||M1||  =", round(sqrt(sum(g1[(md_end+1):m1_end]^2)), 6), "\n")
cat("    ||M2||  =", round(sqrt(sum(g1[(m1_end+1):m2_end]^2)), 6), "\n")
cat("    ||M3||  =", round(sqrt(sum(g1[(m2_end+1):N_MOMENTS]^2)), 6), "\n")

# =========================================================================
# STEP 2: OPTIMAL WEIGHTING
# =========================================================================

cat("\n--- Joint GMM Step 2 ---\n")

v2_demand <- max(sum(g1[1:md_end]^2), 1e-20)
v2_rs     <- max(sum(g1[(md_end+1):m1_end]^2), 1e-20)
v2_cl     <- max(sum(g1[(m1_end+1):m2_end]^2), 1e-20)
v2_foc    <- max(sum(g1[(m2_end+1):N_MOMENTS]^2), 1e-20)

W2 <- as.matrix(Matrix::bdiag(
  diag(N_DEMAND) / v2_demand,
  diag(ncol(Z_rs)) / v2_rs,
  diag(ncol(Z_cl)) / v2_cl,
  diag(N_Z_FOC) / v2_foc
))

cat("  Block norms: M_D =", format(sqrt(v2_demand), digits = 4),
    " M1 =", format(sqrt(v2_rs), digits = 4),
    " M2 =", format(sqrt(v2_cl), digits = 4),
    " M3 =", format(sqrt(v2_foc), digits = 4), "\n")

iter_count <- 0L
pscale2 <- pmax(abs(result1$par), 1e-4)
result2 <- optim(
  par = result1$par,
  fn = gmm_obj_trace,
  W = W2,
  method = "BFGS",
  control = list(maxit = 500, reltol = 1e-10,
                 parscale = pscale2, trace = 0)
)

cat("  Converged:", result2$convergence == 0, "\n")
cat("  Objective:", format(result2$value, digits = 6), "\n")

beta_joint   <- result2$par[1:K]
lambda_joint <- result2$par[K + 1]
alpha_joint  <- result2$par[(K + 2):(K + 1 + N_ALPHA)]
gamma_joint  <- result2$par[(K + 2 + N_ALPHA):(K + 1 + N_ALPHA + N_GAMMA)]

# =========================================================================
# DIAGNOSTICS
# =========================================================================

cat("\n--- Joint GMM Diagnostics ---\n")

g2 <- compute_joint_g_bar(result2$par)
cat("  Final moment norms:\n")
cat("    ||M_D|| =", round(sqrt(sum(g2[1:md_end]^2)), 6), "\n")
cat("    ||M1||  =", round(sqrt(sum(g2[(md_end+1):m1_end]^2)), 6), "\n")
cat("    ||M2||  =", round(sqrt(sum(g2[(m1_end+1):m2_end]^2)), 6), "\n")
cat("    ||M3||  =", round(sqrt(sum(g2[(m2_end+1):N_MOMENTS]^2)), 6), "\n")
cat("    M3 moments:", round(g2[(m2_end+1):N_MOMENTS], 6), "\n")

# Parameter comparison: sequential → joint
cat("\n  Key parameter comparison (sequential → joint):\n")
cat(sprintf("    beta_premium:  %.6f → %.6f  (change: %+.6f)\n",
            beta0[1], beta_joint[1], beta_joint[1] - beta0[1]))
cat(sprintf("    lambda:        %.4f → %.4f  (change: %+.4f)\n",
            lambda0, lambda_joint, lambda_joint - lambda0))
cat(sprintf("    theta_r:       %.4f → %.4f  (change: %+.4f)\n",
            gamma0[2], gamma_joint[2], gamma_joint[2] - gamma0[2]))

cat("\n  Full demand coefficients (joint):\n")
for (k in seq_along(ALL_COVARS)) {
  cat(sprintf("    %-25s = %12.6f  (was %12.6f)\n",
              ALL_COVARS[k], beta_joint[k], beta0[k]))
}
cat(sprintf("    %-25s = %12.6f  (was %12.6f)\n", "lambda", lambda_joint, lambda0))

cat("\n  Cost parameters (joint):\n")
comp <- data.frame(
  param = c(alpha_names, gamma_names),
  equation = c(rep("risk_score", N_ALPHA), rep("claims", N_GAMMA)),
  sequential = round(c(alpha0, gamma0), 4),
  joint = round(c(alpha_joint, gamma_joint), 4),
  change = round(c(alpha_joint - alpha0, gamma_joint - gamma0), 4)
)
print(comp, row.names = FALSE)

# =========================================================================
# SAVE (to separate files — does NOT overwrite sequential estimates)
# =========================================================================

cat("\nSaving joint GMM estimates...\n")

# Demand coefficients
coefs_joint <- tibble(term = c(ALL_COVARS, "lambda"),
                       estimate = c(beta_joint, lambda_joint))
write_csv(coefs_joint, "results/choice_coefficients_joint.csv")
cat("  Demand → results/choice_coefficients_joint.csv\n")

# Cost coefficients
rs_coefs_joint <- tibble(term = alpha_names, estimate = alpha_joint)
cl_coefs_joint <- tibble(term = gamma_names, estimate = gamma_joint)
write_csv(rs_coefs_joint, file.path(TEMP_DIR, "ra_rs_coefs_joint.csv"))
write_csv(cl_coefs_joint, file.path(TEMP_DIR, "ra_claims_coefs_joint.csv"))
cat("  Cost → TEMP_DIR/ra_*_coefs_joint.csv\n")

cat("\nJoint demand-supply GMM complete.\n")
