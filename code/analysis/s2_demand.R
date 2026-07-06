# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Date Edited:   2026-03-24
## Description:   Structural demand estimation.
##                Phase 1 (R): build cell CSVs from parquet partitions.
##                Phase 2 (R): L-BFGS-B nested logit via estimate_demand.R.
##                See docs/optimizer.md for algorithm details.

# Dependencies: preamble + s1_inputs.R (cells, seeds, plan_choice) loaded
# by _analysis.R before this step.

# Structural specification ------------------------------------------------

STRUCTURAL_SPEC <- c(
  "premium",
  "silver", "bronze", "hmo", "hsa",
  # Big-four brand dummies only. The seven regionals are kept as SEPARATE plans
  # (own premium, commission, and cost), but carry NO brand fixed effect — adding
  # one per regional pushes the nesting parameter lambda from 0.47 to 4.27
  # (non-RUM). They sit in the dummy-less small baseline. Per-regional
  # commission/cost key off the plan_id prefix, not these dummies.
  "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
  "hh_size_prem", "perc_0to17_prem", "perc_18to34_prem", "perc_35to54_prem",
  "perc_male_prem", "perc_black_prem", "perc_hispanic_prem", "perc_asian_prem", "perc_other_prem",
  "FPL_250to400_prem", "FPL_400plus_prem",
  # Age x metal: the metal preference varies by household age mix so the young tilt
  # into bronze beyond the common price effect (fixes the inverted age-by-metal
  # sorting). Premium-independent; see covariates.R / build_structural.
  "perc_0to17_silver", "perc_0to17_bronze",
  "perc_18to34_silver", "perc_18to34_bronze",
  "perc_35to54_silver", "perc_35to54_bronze"
)

STRUCTURAL_ASST <- c(
  "assisted_silver", "assisted_bronze",
  # Broker metal steering, estimated rather than assumed zero (symmetric with the
  # navigator assisted_* terms). Brokers also carry commission_broker; navigators
  # do not (institutional, not a behavioral assumption).
  "broker_silver", "broker_bronze",
  # Channel-specific price response: navigator and broker each shift the premium
  # coefficient (price interactions, raw_demo = nonbroker / broker).
  "assisted_premium", "broker_premium",
  # Channel x Pareto-dominated plan (RF definition): a CSR-eligible household's
  # Gold/Platinum alternatives, dominated by the enhanced Silver it qualifies for.
  # Premium-independent (CSR x metal). See covariates.R / cf_cell.R.
  "nav_dominated", "broker_dominated",
  # Commission steering enters as a level term only. v_hat is dropped from the
  # structural side (a household constant cannot enter the logit except via a
  # plan-varying interaction, and that interaction was collinear with the level).
  "commission_broker"
)

write_demand_spec(STRUCTURAL_SPEC, STRUCTURAL_ASST,
                  file.path(TEMP_DIR, "demand_spec.csv"))

CELL_DIR <- file.path(TEMP_DIR, "choice_cells")

cat("Region-year cells:", nrow(cells), "\n")

# Clean and recreate cell directory to ensure fresh data
if (dir.exists(CELL_DIR)) unlink(CELL_DIR, recursive = TRUE)
dir.create(CELL_DIR, recursive = TRUE)

cat("\nPhase 1: Building cell CSVs...\n")
n_built <- 0L
n_skip  <- 0L

for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]

  out_file <- file.path(CELL_DIR, paste0("cell_", r, "_", y, "_data.csv"))
  if (file.exists(out_file)) { n_skip <- n_skip + 1L; next }

  set.seed(cell_seeds[i])
  cell_key <- paste0(r, ".", y)
  hhs <- hh_split[[cell_key]]
  if (is.null(hhs) || nrow(hhs) == 0) { n_skip <- n_skip + 1L; next }
  hhs <- as.data.frame(hhs)

  plans <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans) == 0) { n_skip <- n_skip + 1L; next }

  cd <- build_structural(plans, hhs, SAMPLE_FRAC,
                         spec = c(STRUCTURAL_SPEC, STRUCTURAL_ASST))$cell_data
  rm(hhs, plans)

  if (!is.null(cd)) {
    cd$region <- r
    cd$year <- y

    fwrite(cd, out_file)
    n_built <- n_built + 1L
  } else {
    n_skip <- n_skip + 1L
  }
  rm(cd)

  if (i %% 20 == 0) {
    gc(verbose = FALSE)
    cat("  Cell", i, "of", nrow(cells), "\n")
  }
}

gc(verbose = FALSE)
cat("  Built:", n_built, "  Skipped:", n_skip, "\n")

# Free hh_split before estimation (estimate_demand loads cells from CSVs)
rm(hh_split); gc(full = TRUE, verbose = FALSE)

# =========================================================================
# PHASE 2: Estimate demand (R)
# =========================================================================

cat("\nPhase 2: Running demand estimation...\n")

estimate_demand(
  cell_dir        = CELL_DIR,
  spec_path       = file.path(TEMP_DIR, "demand_spec.csv"),
  out_path        = "results/choice_coefficients_structural.csv",
  filter_assisted = -1L,  # all HH for structural
  temp_dir        = TEMP_DIR  # cache MNL starting values
)


# =========================================================================
# PHASE 3: Read results
# =========================================================================

cat("\nPhase 3: Reading coefficient estimates...\n")

coefs_path <- "results/choice_coefficients_structural.csv"

if (file.exists(coefs_path)) {
  coefs_structural <- read_csv(coefs_path, show_col_types = FALSE)
  cat("  Pooled model:", nrow(coefs_structural), "terms\n")
  print(coefs_structural, n = Inf)

  # Headline: commission-premium ratio and assisted x metal effects
  beta_p <- coefs_structural$estimate[coefs_structural$term == "premium"]
  beta_c <- coefs_structural$estimate[coefs_structural$term == "commission_broker"]
  if (length(beta_p) == 1 && length(beta_c) == 1 && abs(beta_p) > 1e-10) {
    cat(sprintf("\n  beta_commission / |beta_premium| = %.4f\n", beta_c / abs(beta_p)))
  }
  for (m in c("assisted_silver", "assisted_bronze", "assisted_gold", "assisted_plat",
              "broker_silver", "broker_bronze", "assisted_premium", "broker_premium",
              "nav_dominated", "broker_dominated")) {
    b <- coefs_structural$estimate[coefs_structural$term == m]
    if (length(b) == 1) cat(sprintf("  %s = %.6f\n", m, b))
  }
} else {
  cat("  Coefficients not found.\n")
}

cat("\nStructural demand estimation complete.\n")
