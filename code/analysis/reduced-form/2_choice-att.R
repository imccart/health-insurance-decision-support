# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-24
## Date Edited:   2026-03-25
## Description:   Pooled nested logit choice model ATT with control function.
##                Estimate on unassisted HH (with v_hat CF correction),
##                predict counterfactual for assisted HH, compute ATT.
##                Expects plan_choice, partitioned parquets, and v_hat
##                available from _reduced-form.R runner.

# Dependencies ------------------------------------------------------------
library(arrow)

# Tuning ------------------------------------------------------------------

SAMPLE_FRAC   <- 0.20
CELL_DIR      <- "data/output/choice_cells"
PARTITION_DIR <- "data/output/hh_choice_partitions"

# Read plan data (runner already saved plan_choice.csv)
plan_choice <- read_csv("data/output/plan_choice.csv", show_col_types = FALSE)

if (!dir.exists(CELL_DIR)) dir.create(CELL_DIR, recursive = TRUE)

# =========================================================================
# Phase 1: Build cell data
# =========================================================================

partition_files <- list.files(PARTITION_DIR, pattern = "^hh_\\d+_\\d+\\.parquet$",
                              full.names = FALSE)
cells <- tibble(file = partition_files) %>%
  mutate(
    region = as.integer(str_extract(file, "(?<=hh_)\\d+")),
    year   = as.integer(str_extract(file, "(?<=_)\\d{4}"))
  ) %>%
  arrange(region, year)

cat("Phase 1: Building cell data (", nrow(cells), "cells)...\n")

set.seed(20260224)
cell_seeds <- sample.int(1e7, nrow(cells))

cell_list <- list()
n_built <- 0L
n_skip  <- 0L

for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]

  out_file <- file.path(CELL_DIR, paste0("cell_", r, "_", y, "_data.csv"))
  if (file.exists(out_file)) {
    cell_list[[length(cell_list) + 1]] <- read_csv(out_file, show_col_types = FALSE)
    cell_list[[length(cell_list)]]$region <- r
    cell_list[[length(cell_list)]]$year <- y
    n_built <- n_built + 1L
    next
  }

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
    cell_list[[length(cell_list) + 1]] <- cd
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

cat("  Built:", n_built, "  Skipped:", n_skip, "\n")

pooled <- bind_rows(cell_list)
rm(cell_list, plan_choice)
gc(verbose = FALSE)

cat("  Pooled dataset:", nrow(pooled), "rows\n")


# =========================================================================
# Phase 2: Estimate pooled model with CF
# =========================================================================

cat("Phase 2: Estimating pooled nested logit (with CF)...\n")

# Globally unique HH IDs
pooled$hh_global <- paste(pooled$region, pooled$year, pooled$household_number, sep = "_")
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

# Add CF interactions: v_hat * plan indicators
# (v_hat is already in the data from partitioned parquets)
# Get major insurer names present in estimation data
insurer_dummies <- c("Anthem", "Blue_Shield", "Kaiser", "Health_Net")
metal_dummies <- c("silver", "bronze")

cf_vars <- c()
for (var in c(insurer_dummies, metal_dummies)) {
  cf_name <- paste0("cf_", tolower(var))
  if (var %in% names(data_est)) {
    data_est[[cf_name]] <- data_est$v_hat * data_est[[var]]
    data_oos[[cf_name]] <- data_oos$v_hat * data_oos[[var]]
    cf_vars <- c(cf_vars, cf_name)
  }
}

# Also add v_hat * premium interaction
data_est$cf_premium <- data_est$v_hat * data_est$premium
data_oos$cf_premium <- data_oos$v_hat * data_oos$premium
cf_vars <- c(cf_vars, "cf_premium")

cat("  CF variables added:", paste(cf_vars, collapse = ", "), "\n")

# Nest names
nest_names <- unique(data_est$plan_name)
nest1 <- nest_names[nest_names != "Uninsured"]

# Estimate with CF covariates appended
# We call estimate_nested_logit but need to add CF vars to the formula.
# Easiest: add them to the data and modify the function call.

# Build formula manually (same covariates as estimate_nested_logit pooled mode + CF)
base_covars <- c("premium", "penalty_own", "premium_sq",
                  "silver", "bronze", "hh_size_prem",
                  "any_0to17_prem", "FPL_250to400_prem", "FPL_400plus_prem",
                  "any_black_prem", "any_hispanic_prem",
                  "hmo", "hsa",
                  "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
                  "Anthem_silver", "BS_silver", "Kaiser_silver", "HN_silver",
                  "Anthem_bronze", "BS_bronze", "Kaiser_bronze", "HN_bronze")

# Drop cf_resid (we use CF from broker density first stage instead)
all_covars <- c(base_covars, cf_vars)

# Keep only covariates that exist in data
all_covars <- all_covars[all_covars %in% names(data_est)]

nested_data <- mlogit.data(data_est, choice = "choice", shape = "long",
                            chid.var = "household_number",
                            alt.var = "plan_name")

nested_formula <- as.formula(
  paste("choice ~", paste(all_covars, collapse = " + "), "| 0")
)

mod <- tryCatch(
  mlogit(nested_formula, data = nested_data, weights = ipweight,
         nests = list(insured = nest1, uninsured = "Uninsured"),
         un.nest.el = TRUE),
  error = function(e) {
    cat("  Nested logit failed:", conditionMessage(e), "\n")
    cat("  Trying with GLM starting values...\n")
    tryCatch({
      logit_formula <- as.formula(
        paste("choice ~", paste(all_covars, collapse = " + "))
      )
      logit_start <- glm(logit_formula, data = data_est, family = "binomial")
      start_vals <- logit_start$coefficients[-1]
      mlogit(nested_formula, data = nested_data, weights = ipweight,
             nests = list(insured = nest1, uninsured = "Uninsured"),
             un.nest.el = TRUE, start = start_vals)
    }, error = function(e2) {
      cat("  GLM fallback also failed:", conditionMessage(e2), "\n")
      NULL
    })
  }
)

if (is.null(mod)) {
  cat("Pooled model did not converge. Stopping.\n")
  stop("Nested logit estimation failed.", call. = FALSE)
}

coefs <- tidy(mod)
coefs$term[coefs$term == "iv"] <- "lambda"
cat("  Coefficients:\n")
print(coefs %>% select(term, estimate, std.error, p.value), n = Inf)

# Report CF coefficient significance
cf_coefs <- coefs %>% filter(grepl("^cf_", term))
if (nrow(cf_coefs) > 0) {
  cat("\n  CF coefficients (selection correction):\n")
  print(cf_coefs %>% select(term, estimate, std.error, p.value), n = Inf)
}

write_csv(coefs, "results/choice_coefficients.csv")


# =========================================================================
# Phase 3: Per-cell predictions
# =========================================================================

cat("Phase 3: Computing per-cell predictions...\n")

pred_list <- list()

for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]

  cell_oos <- data_oos %>% filter(region == r, year == y)
  if (nrow(cell_oos) == 0) next

  preds <- predict_nested_logit(cell_oos, coefs, nest1)

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
write_csv(all_prob, "results/choice_point_estimates.csv")

cat("  Predictions:", nrow(all_prob), "rows -> results/choice_point_estimates.csv\n")

rm(data_est, data_oos)
gc(verbose = FALSE)

cat("Choice model ATT estimation complete.\n")
