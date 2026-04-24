# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Date Edited:   2026-03-24
## Description:   Structural demand estimation.
##                Phase 1 (R): build cell CSVs from parquet partitions.
##                Phase 2 (R): L-BFGS-B nested logit via estimate_demand.R.
##                See docs/optimizer.md for algorithm details.

# Dependencies: all loaded by _structural.R

# Tuning parameters -------------------------------------------------------

SAMPLE_FRAC   <- as.numeric(Sys.getenv("SAMPLE_FRAC"))
MASTER_SEED   <- as.integer(Sys.getenv("MASTER_SEED"))
TEMP_DIR      <- Sys.getenv("TEMP_DIR")
CELL_DIR      <- file.path(TEMP_DIR, "choice_cells")

# hh_split, cells, cell_seeds, plan_choice loaded by _structural.R

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

  cd <- build_choice_data(plans, hhs, SAMPLE_FRAC,
                          spec = c(STRUCTURAL_SPEC, STRUCTURAL_ASST),
                          premium_type = "net")
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
  for (m in c("assisted_silver", "assisted_bronze", "assisted_gold", "assisted_plat")) {
    b <- coefs_structural$estimate[coefs_structural$term == m]
    if (length(b) == 1) cat(sprintf("  %s = %.6f\n", m, b))
  }
} else {
  cat("  Coefficients not found.\n")
}

# Reload hh_split for downstream scripts (pricing, counterfactuals)
cat("Reloading shared HH data...\n")
hh_all <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split <- split(hh_all, by = c("region", "year"), keep.by = FALSE)
rm(hh_all); gc(verbose = FALSE)

cat("\nStructural demand estimation complete.\n")
