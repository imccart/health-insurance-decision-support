# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Date Edited:   2026-03-24
## Description:   Structural demand estimation.
##                Phase 1 (R): build cell CSVs from parquet partitions.
##                Phase 2 (R): two-part nested logit via estimate_demand.R.
##                See notes/optimizer.md for algorithm details.

# Dependencies: preamble + s1_inputs.R (cells, seeds, plan_choice) loaded
# by _analysis.R before this step.

# Structural specification ------------------------------------------------

STRUCTURAL_SPEC <- c(
  "premium",
  # Coverage generosity: the household-specific actuarial value of the plan
  # (CSR-aware for silver), continuous, in place of metal-tier dummies.
  "av", "hmo",
  # Big-four brand dummies only; the seven regionals carry no brand fixed effect
  # (their commission/cost key off the plan_id prefix).
  "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
  # Demographics x premium and the same demographics x AV (built in build_structural).
  "hh_size_prem", "perc_0to17_prem", "perc_18to34_prem", "perc_35to54_prem",
  "perc_male_prem", "perc_black_prem", "perc_hispanic_prem", "perc_asian_prem", "perc_other_prem",
  "FPL_250to400_prem", "FPL_400plus_prem",
  "hh_size_av", "perc_0to17_av", "perc_18to34_av", "perc_35to54_av",
  "perc_male_av", "perc_black_av", "perc_hispanic_av", "perc_asian_av", "perc_other_av",
  "FPL_250to400_av", "FPL_400plus_av"
)

# Assistance terms. These enter plan choice within the insured nest only: the
# enrollment decision uses the inclusive value without them (two-part nested
# logit, estimate_demand.R), since assistance is observed only conditional on
# enrolling and its effect on enrollment is not identified.
STRUCTURAL_ASST <- c(
  # Channel x generosity steering (navigator, broker).
  "assisted_av", "broker_av",
  # Channel-specific price response (raw_demo = nonbroker / broker).
  "assisted_premium", "broker_premium",
  # Commission steering, level term (brokers only).
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
  temp_dir        = TEMP_DIR,
  ext_exclude     = STRUCTURAL_ASST   # excluded from the enrollment inclusive value
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
  for (m in c("av", "assisted_av", "broker_av", "assisted_premium", "broker_premium")) {
    b <- coefs_structural$estimate[coefs_structural$term == m]
    if (length(b) == 1) cat(sprintf("  %s = %.6f\n", m, b))
  }
} else {
  cat("  Coefficients not found.\n")
}

cat("\nStructural demand estimation complete.\n")
