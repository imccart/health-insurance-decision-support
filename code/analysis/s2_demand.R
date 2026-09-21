# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Date Edited:   2026-03-24
## Description:   Structural demand estimation.
##                Phase 1 (R): build cell CSVs from the analysis data.
##                Phase 2 (R): two-part nested logit via estimate_demand.R.
##                See notes/optimizer.md for algorithm details.

# Dependencies: preamble + s1_inputs.R (cells, seeds, plan_choice) loaded
# by _analysis.R before this step.

# Structural specification ------------------------------------------------

STRUCTURAL_SPEC <- c(
  # Inside-good intercept (1 on every insured plan, 0 on the outside option).
  "inside",
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
  "FPL_250to400_av", "FPL_400plus_av",
  # Demographic enrollment intercepts: demographics x the inside-good
  # indicator, shifting the propensity to enroll at all.
  "hh_size_insured", "perc_0to17_insured", "perc_18to34_insured", "perc_35to54_insured",
  "perc_male_insured", "perc_black_insured", "perc_hispanic_insured", "perc_asian_insured",
  "perc_other_insured", "FPL_250to400_insured", "FPL_400plus_insured"
)

# Assistance terms. They enter plan choice at the household's realized channel
# and the enrollment margin through the expected inclusive value over the three
# channel states, weighted by build3's first-stage probabilities
# (estimate_demand.R). This list names the state add-on terms.
STRUCTURAL_ASST <- c(
  # Channel x generosity steering (navigator, broker).
  "assisted_av", "broker_av",
  # Channel-specific price response (raw_demo = nonbroker / broker).
  "assisted_premium", "broker_premium",
  # Commission steering (brokers only): the household commission and its
  # square over 100, so the response to a commission dollar can fall with the rate.
  "commission_broker", "commission_broker_sq"
)

write_demand_spec(STRUCTURAL_SPEC, STRUCTURAL_ASST,
                  file.path(TEMP_DIR, "demand_spec.csv"))

CELL_DIR <- file.path(TEMP_DIR, "choice_cells")


# Clean and recreate cell directory to ensure fresh data
if (dir.exists(CELL_DIR)) unlink(CELL_DIR, recursive = TRUE)
dir.create(CELL_DIR, recursive = TRUE)

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

# Free hh_split before estimation (estimate_demand loads cells from CSVs)
rm(hh_split); gc(full = TRUE, verbose = FALSE)

# =========================================================================
# PHASE 2: Estimate demand (R)
# =========================================================================


estimate_demand(
  cell_dir        = CELL_DIR,
  spec_path       = file.path(TEMP_DIR, "demand_spec.csv"),
  out_path        = "results/choice_coefficients_structural.csv",
  filter_assisted = -1L,  # all HH for structural
  temp_dir        = TEMP_DIR,
  ext_exclude     = STRUCTURAL_ASST   # the channel-state add-on terms
)

