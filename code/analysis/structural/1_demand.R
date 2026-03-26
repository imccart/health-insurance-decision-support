# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-16
## Date Edited:   2026-03-24
## Description:   Structural demand estimation.
##                Phase 1 (R): build cell CSVs from parquet partitions.
##                Phase 2 (Julia): BFGS-BHHH nested logit via estimate_demand_v3.jl.
##                See docs/optimizer.md for algorithm details.

# Dependencies (arrow for read_parquet, not in 0-setup.R) -----------------
library(arrow)

# Tuning parameters -------------------------------------------------------

SAMPLE_FRAC   <- 0.20
CELL_DIR      <- "data/output/choice_cells"
PARTITION_DIR <- "data/output/hh_choice_partitions"

# =========================================================================
# PHASE 1: Build cell CSVs (R)
# =========================================================================

plan_choice <- read_csv("data/output/plan_choice.csv", show_col_types = FALSE)

partition_files <- list.files(PARTITION_DIR, pattern = "^hh_\\d+_\\d+\\.parquet$",
                              full.names = FALSE)
cells <- tibble(file = partition_files) %>%
  mutate(
    region = as.integer(str_extract(file, "(?<=hh_)\\d+")),
    year   = as.integer(str_extract(file, "(?<=_)\\d{4}"))
  ) %>%
  arrange(region, year)

cat("Region-year cells:", nrow(cells), "\n")

set.seed(20260224)
cell_seeds <- sample.int(1e7, nrow(cells))

if (!dir.exists(CELL_DIR)) dir.create(CELL_DIR, recursive = TRUE)

cat("\nPhase 1: Building cell CSVs...\n")
n_built <- 0L
n_skip  <- 0L

for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]

  out_file <- file.path(CELL_DIR, paste0("cell_", r, "_", y, "_data.csv"))
  if (file.exists(out_file)) { n_skip <- n_skip + 1L; next }

  set.seed(cell_seeds[i])
  hhs <- tryCatch(
    read_parquet(file.path(PARTITION_DIR, paste0("hh_", r, "_", y, ".parquet"))),
    error = function(e) NULL
  )
  if (is.null(hhs) || nrow(hhs) == 0) { n_skip <- n_skip + 1L; next }

  plans <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans) == 0) { n_skip <- n_skip + 1L; next }

  cd <- build_choice_data(plans, hhs, SAMPLE_FRAC)
  rm(hhs, plans)

  if (!is.null(cd)) {
    cd$region <- r
    cd$year <- y
    write_csv(cd, out_file)
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

rm(plan_choice)
gc(verbose = FALSE)
cat("  Built:", n_built, "  Skipped:", n_skip, "\n")


# =========================================================================
# PHASE 2: Run Julia estimator
# =========================================================================

cat("\nPhase 2: Running Julia demand estimation...\n")

julia_script <- "code/julia/estimate_demand_v3.jl"

julia_exe <- Sys.which("julia")
if (julia_exe == "") {
  julia_candidates <- c(
    "C:/Users/immccar/AppData/Local/Microsoft/WindowsApps/julia.exe",
    "C:/Users/immccar/.juliaup/bin/julia.exe"
  )
  for (jc in julia_candidates) {
    if (file.exists(jc)) { julia_exe <- jc; break }
  }
}

if (julia_exe == "") {
  cat("  Julia not found. Run manually:\n")
  cat("    julia --threads=auto", julia_script, "\n")
} else {
  cat("  Julia executable:", julia_exe, "\n")
  exit_code <- system2(julia_exe, args = c("+release", "--threads=auto", julia_script),
                        stdout = "", stderr = "")
  if (exit_code != 0) cat("  Julia exited with code", exit_code, "\n")
}


# =========================================================================
# PHASE 3: Read results
# =========================================================================

cat("\nPhase 3: Reading coefficient estimates...\n")

coefs_path <- "data/output/choice_coefficients_structural.csv"

if (file.exists(coefs_path)) {
  coefs_structural <- read_csv(coefs_path, show_col_types = FALSE)
  cat("  Pooled model:", nrow(coefs_structural), "terms\n")
  print(coefs_structural, n = Inf)

  # Headline: commission-premium ratio
  beta_p <- coefs_structural$estimate[coefs_structural$term == "premium"]
  beta_c <- coefs_structural$estimate[coefs_structural$term == "commission_broker"]
  if (length(beta_p) == 1 && length(beta_c) == 1 && abs(beta_p) > 1e-10) {
    cat(sprintf("\n  beta_commission / |beta_premium| = %.4f\n", beta_c / abs(beta_p)))
  }
} else {
  cat("  Coefficients not found.\n")
}

cat("\nStructural demand estimation complete.\n")
