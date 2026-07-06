# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Analytical standard errors for the structural parameters.
##                Demand: HH-robust sandwich (reloads the cells + reads the saved
##                coefficients). Cost: GMM sandwich, reusing the machinery left in
##                the session by 9_cost-gmm (result2 / W2 / compute_g_bar); if this
##                step is run on its own it sources 9_cost-gmm first. Writes the SE
##                tables and the full vcov matrices (the latter feed the CF
##                bootstrap, step 12). Sandwich math in helpers/se.R.
##
## Dependencies: preamble loaded by _analysis.R; 7_demand (choice_cells + demand
##               coefficients) and 9_cost-gmm run first.

# --- Demand SE (HH-robust sandwich) --------------------------------------
cat("\n--- Demand standard errors (sandwich) ---\n"); flush.console()
covars  <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))$all
dcoef   <- read.csv("results/choice_coefficients_structural.csv", stringsAsFactors = FALSE)
theta_d <- setNames(dcoef$estimate, dcoef$term)

loaded   <- load_all_cells(file.path(TEMP_DIR, "choice_cells"), covars, filter_assisted = -1L)
cells_se <- normalize_weights(loaded$cells)
rm(loaded); gc(verbose = FALSE)

dse <- demand_sandwich_se(cells_se, theta_d)
cat(sprintf("  max |gradient| at optimum = %.3g\n", dse$max_grad))
write.csv(dse$se, "results/choice_coefficients_structural_se.csv", row.names = FALSE)
write.csv(data.frame(term = rownames(dse$vcov), dse$vcov, check.names = FALSE),
          "results/choice_coefficients_structural_vcov.csv", row.names = FALSE)
rm(cells_se); gc(verbose = FALSE)

# --- Cost SE (GMM sandwich) ----------------------------------------------
# Reuse the GMM machinery left by 9_cost-gmm; source it if running standalone.
if (!exists("compute_g_bar")) source("code/analysis/9_cost-gmm.R")
cat("\n--- Cost standard errors (sandwich) ---\n"); flush.console()
cse <- cost_gmm_sandwich_se(
  theta_hat   = result2$par, W = W2, gbar_fn = compute_g_bar,
  N_ALPHA     = N_ALPHA, N_GAMMA = N_GAMMA, n_mom = N_MOMENTS,
  n12         = ncol(Z_rs) + ncol(Z_cl),
  param_names = c(alpha_names, gamma_names))
write.csv(cse$se, "results/cost_coefficients_gmm_se.csv", row.names = FALSE)
write.csv(data.frame(param = rownames(cse$vcov), cse$vcov, check.names = FALSE),
          "results/cost_coefficients_gmm_vcov.csv", row.names = FALSE)

cat("  -> results/choice_coefficients_structural_se.csv (+ vcov)\n")
cat("  -> results/cost_coefficients_gmm_se.csv (+ vcov)\n")
cat("\nStructural standard errors complete.\n")
