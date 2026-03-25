# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-24
## Date Edited:   2026-03-06
## Description:   Pooled nested logit discrete choice estimation.
##                Builds cell data in-process, concatenates all 114 cells,
##                estimates ONE pooled mlogit, then computes per-cell
##                predictions for ATT analysis.

# Dependencies (arrow for read_parquet, not in 0-setup.R) -----------------
library(arrow)

# Tuning parameters -------------------------------------------------------

SAMPLE_FRAC <- 0.20    # fraction of HH sampled per cell
MAX_BOOT    <- 0       # bootstrap iterations (deferred; use analytic SEs)

CELL_DIR      <- "data/output/choice_cells"
PARTITION_DIR <- "data/output/hh_choice_partitions"

if (!dir.exists(CELL_DIR)) dir.create(CELL_DIR, recursive = TRUE)

# Load plan_choice from disk (freed by _structural.R before sourcing here)
plan_choice <- read_csv("data/output/plan_choice.csv", show_col_types = FALSE)

# Discover cells from partitioned parquet files ----------------------------
partition_files <- list.files(PARTITION_DIR, pattern = "^hh_\\d+_\\d+\\.parquet$",
                              full.names = FALSE)

cells <- tibble(file = partition_files) %>%
  mutate(
    region = as.integer(str_extract(file, "(?<=hh_)\\d+")),
    year   = as.integer(str_extract(file, "(?<=_)\\d{4}"))
  ) %>%
  arrange(region, year)

cat("Region-year cells:", nrow(cells), "\n")


# Phase 1: Build cell data ------------------------------------------------
cat("Phase 1: Building cell data (", nrow(cells), "cells)...\n")

set.seed(20260224)
cell_seeds <- sample.int(1e7, nrow(cells))

cell_list <- list()
n_built <- 0L
n_skip  <- 0L

for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]

  set.seed(cell_seeds[i])
  hhs <- tryCatch(
    read_parquet(file.path(PARTITION_DIR, paste0("hh_", r, "_", y, ".parquet"))),
    error = function(e) NULL
  )
  if (is.null(hhs) || nrow(hhs) == 0) {
    n_skip <- n_skip + 1L
    next
  }

  plans <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans) == 0) {
    n_skip <- n_skip + 1L
    next
  }

  cd <- build_choice_data(plans, hhs, SAMPLE_FRAC)
  rm(hhs, plans)

  if (!is.null(cd)) {
    cd$region <- r
    cd$year <- y
    cell_list[[length(cell_list) + 1]] <- cd
    n_built <- n_built + 1L

    # Save cell data for future bootstrap
    write_csv(cd, file.path(CELL_DIR, paste0("cell_", r, "_", y, "_data.csv")))
  } else {
    n_skip <- n_skip + 1L
  }
  rm(cd)

  if (i %% 20 == 0) {
    gc(verbose = FALSE)
    cat("  Cell", i, "of", nrow(cells), "\n")
  }
}

cat("  Built:", n_built, "  Skipped:", n_skip, "\n")

pooled <- bind_rows(cell_list)
rm(cell_list)
gc(verbose = FALSE)

cat("  Pooled dataset:", nrow(pooled), "rows,",
    length(unique(pooled$household_number)), "unique HH IDs\n")


# Phase 2: Estimate pooled model ------------------------------------------
cat("Phase 2: Estimating pooled nested logit...\n")

# Household numbers must be unique across cells (they're only unique within cell)
# Create a globally unique ID: paste region_year_household_number
pooled$hh_global <- paste(pooled$region, pooled$year, pooled$household_number, sep = "_")

# Re-encode as integer for mlogit
hh_map <- data.frame(
  hh_global = unique(pooled$hh_global),
  hh_id = seq_along(unique(pooled$hh_global))
)
pooled <- merge(pooled, hh_map, by = "hh_global")
pooled$household_number <- pooled$hh_id
pooled$hh_global <- NULL
pooled$hh_id <- NULL

# Split by treatment status
data_est <- pooled %>% filter(assisted == 0)
data_oos <- pooled %>% filter(assisted == 1)
rm(pooled)
gc(verbose = FALSE)

cat("  Estimation (unassisted):", nrow(data_est), "rows,",
    length(unique(data_est$household_number)), "HH\n")
cat("  OOS (assisted):", nrow(data_oos), "rows,",
    length(unique(data_oos$household_number)), "HH\n")

# Nest names from pooled data
nest_names <- unique(data_est$plan_name)
nest1 <- nest_names[nest_names != "Uninsured"]

# Estimate pooled model
mod <- tryCatch(
  estimate_nested_logit(d = data_est, nest_names = nest1, pooled = TRUE),
  error = function(e) {
    cat("  Pooled estimation failed:", conditionMessage(e), "\n")
    NULL
  }
)

if (is.null(mod)) {
  cat("Pooled model did not converge. Stopping.\n")
  stop("Pooled nested logit estimation failed.", call. = FALSE)
}

# Extract and save coefficients
coefs <- tidy(mod)
coefs$term[coefs$term == "iv"] <- "lambda"  # mlogit calls it "iv"; helpers expect "lambda"
cat("  Coefficients:\n")
print(coefs %>% select(term, estimate, std.error, p.value), n = Inf)

write_csv(coefs, "data/output/choice_coefficients.csv")
cat("  Coefficients written -> data/output/choice_coefficients.csv\n")


# Phase 3: Per-cell predictions -------------------------------------------
cat("Phase 3: Computing per-cell predictions...\n")

pred_list <- list()

# Predict on OOS (assisted) data for each cell
for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]

  cell_oos <- data_oos %>% filter(region == r, year == y)
  if (nrow(cell_oos) == 0) next

  # Predict using manual formula (avoids predict.mlogit issues)
  preds <- predict_nested_logit(cell_oos, coefs, nest1)

  # Aggregate to plan-level summaries
  cell_oos_dt <- as.data.table(cell_oos[, c("household_number", "plan_name", "choice")])
  cell_oos_dt <- merge(cell_oos_dt, preds, by = c("household_number", "plan_name"), all.x = TRUE)

  plan_summary <- cell_oos_dt[, .(
    tot_nonmiss   = sum(!is.na(pred)),
    obs_purchase  = sum(choice, na.rm = TRUE),
    pred_purchase = sum(pred, na.rm = TRUE)
  ), by = plan_name]
  plan_summary[, `:=`(region = r, year = y)]

  pred_list[[length(pred_list) + 1]] <- plan_summary
}

all_prob <- bind_rows(pred_list)
write_csv(all_prob, "data/output/choice_point_estimates.csv")

cat("  Predictions:", nrow(all_prob), "rows -> data/output/choice_point_estimates.csv\n")

rm(data_est, data_oos)
gc(verbose = FALSE)


# Bootstrap (deferred) ----------------------------------------------------
# MAX_BOOT = 0: skip bootstrap for now. Pooled bootstrap is expensive
# (~10 min per iteration). Use analytic SEs from mlogit for coefficient
# uncertainty. Write empty bootstrap files so 6_choice-summary.R handles
# gracefully.

sim_bs_pred <- tibble(
  plan_name = character(0), tot_nonmiss = integer(0),
  obs_purchase = numeric(0), pred_purchase = numeric(0),
  region = integer(0), year = integer(0), boot = integer(0)
)
sim_bs_coef <- tibble(
  term = character(0), estimate = numeric(0),
  std.error = numeric(0), statistic = numeric(0),
  p.value = numeric(0), boot = integer(0)
)
write_csv(sim_bs_pred, "data/output/choice_bootstrap_pred.csv")
write_csv(sim_bs_coef, "data/output/choice_bootstrap_coef.csv")

cat("Choice model estimation complete.\n")
cat("  Point coefficients:", nrow(coefs), "terms\n")
cat("  Point predictions:", nrow(all_prob), "rows\n")
cat("  Bootstrap: deferred (MAX_BOOT = 0)\n")
