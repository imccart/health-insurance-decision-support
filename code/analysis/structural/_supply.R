# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Structural supply stage. Reads the demand coefficients and the
##                0_data-prep outputs from disk, then recovers markups via the
##                pricing FOC, estimates cost-side parameters by GMM, and runs
##                counterfactuals. Requires _demand.R to have run first (this
##                run or a prior one) so the demand coefficients, demand_spec,
##                and 0_data-prep outputs exist on disk. Sourced after _demand.R
##                by _analysis.R; can also be re-run on its own to iterate the
##                supply side without re-estimating demand.

# Preconditions -----------------------------------------------------------

needed <- c(
  file.path(TEMP_DIR, "hh_choice.csv"),
  file.path(TEMP_DIR, "demand_spec.csv"),
  "results/choice_coefficients_structural.csv"
)
missing <- needed[!file.exists(needed)]
if (length(missing) > 0) {
  stop("Supply stage is missing demand outputs; run _demand.R first.\n  Missing: ",
       paste(missing, collapse = ", "))
}

# Specification (single-sourced from what _demand.R wrote) ----------------

demand_spec     <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
# Full spec (base + assisted): on the supply side STRUCTURAL_SPEC only feeds the
# price-interaction machinery (compute_alpha_i, recompute_prem_interactions, raw-
# demo export), which must see the channel premium interactions assisted_premium /
# broker_premium (assigned to the assisted group). No estimation happens here, so
# the base/assisted split is irrelevant.
STRUCTURAL_SPEC <- demand_spec$all

# Shared structural inputs ------------------------------------------------

source("code/analysis/structural/_inputs.R")

# Pricing -----------------------------------------------------------------

cat("\n--- Pricing (markups, FOC inputs) ---\n")
source("code/analysis/structural/2_pricing.R")
gc(full = TRUE, verbose = FALSE)

# Cost-side GMM -----------------------------------------------------------

cat("\n--- Cost-side GMM ---\n")
source("code/analysis/structural/3_cost_gmm.R")
gc(full = TRUE, verbose = FALSE)

# Counterfactuals ---------------------------------------------------------

# Free hh_split before counterfactuals (CF reads per-cell from disk)
rm(hh_split)
gc(full = TRUE, verbose = FALSE)

cat("\n--- Counterfactual simulation ---\n")
source("code/analysis/structural/4_counterfactuals.R")

rm(cells, cell_seeds, plan_choice, commission_lookup)
gc(full = TRUE, verbose = FALSE)

cat("\n=== Structural supply stage complete ===\n")
