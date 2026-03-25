# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-04
## Description:   Subprocess worker for nested logit estimation on one
##                region-year cell. Called by 5_choice-model.R via system2().
##
## Usage:         Rscript code/analysis/5a_cell-worker.R <region> <year> <sample_frac> <seed>
## Outputs:       data/output/choice_cells/cell_{r}_{y}_data.csv
##                data/output/choice_cells/cell_{r}_{y}_pred.csv
##                data/output/choice_cells/cell_{r}_{y}_coef.csv

# Parse CLI arguments -----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("Usage: Rscript 5a_cell-worker.R <region> <year> <sample_frac> <seed>")
}
r           <- as.integer(args[1])
y           <- as.integer(args[2])
sample_frac <- as.numeric(args[3])
seed        <- as.integer(args[4])

set.seed(seed)

# Setup (packages + helpers) ----------------------------------------------
source("renv/activate.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(mlogit)
  library(arrow)
  library(broom)
})

source("code/data-build/_helpers-enrollment.R")
source("code/analysis/_helpers-analysis.R")
source("code/analysis/_helpers-choice.R")

# Read data for this cell -------------------------------------------------
hhs <- read_parquet(paste0("data/output/hh_choice_partitions/hh_", r, "_", y, ".parquet"))

plan_choice <- read_csv("data/output/plan_choice.csv", show_col_types = FALSE) %>%
  filter(region == r, year == y)

if (nrow(hhs) == 0 || nrow(plan_choice) == 0) {
  cat("No data for region", r, "year", y, "\n")
  quit(save = "no", status = 0)
}

# Build choice data -------------------------------------------------------
cell_data <- build_choice_data(
  plans       = plan_choice,
  hhs         = hhs,
  sample_frac = sample_frac
)
rm(hhs, plan_choice)

if (is.null(cell_data)) {
  cat("Insufficient data for region", r, "year", y, "\n")
  quit(save = "no", status = 0)
}

# Estimate nested logit ---------------------------------------------------
data_est <- filter(cell_data, assisted == 0)
data_oos <- filter(cell_data, assisted == 1)

nest_names <- unique(data_est$plan_name)
nest1 <- nest_names[nest_names != "Uninsured"]

mod <- tryCatch(
  estimate_nested_logit(d = data_est, nest_names = nest1),
  error = function(e) NULL
)

if (is.null(mod)) {
  cat("Model did not converge for region", r, "year", y, "\n")
  quit(save = "no", status = 0)
}

# Predict on OOS data -----------------------------------------------------
pred_data <- bind_rows(data_oos, data_est)
rm(data_est)

oos_idx <- tryCatch(
  mlogit.data(pred_data, choice = "choice", shape = "long",
              chid.var = "household_number", alt.var = "plan_name"),
  error = function(e) NULL
)
rm(pred_data)

# Write outputs -----------------------------------------------------------
cell_dir <- "data/output/choice_cells"
if (!dir.exists(cell_dir)) dir.create(cell_dir, recursive = TRUE)

# 1. Cell data (for bootstrap)
write_csv(cell_data, file.path(cell_dir, paste0("cell_", r, "_", y, "_data.csv")))
rm(cell_data)

# 2. Predictions
if (!is.null(oos_idx)) {
  nested_pred <- tryCatch(predict(mod, newdata = oos_idx), error = function(e) NULL)
  rm(oos_idx)

  if (!is.null(nested_pred)) {
    nested_pred <- as_tibble(nested_pred, rownames = "household_number") %>%
      mutate(household_number = as.numeric(household_number)) %>%
      pivot_longer(!household_number, names_to = "plan_name",
                   values_to = "pred_purchase")

    treated_dat <- data_oos %>%
      left_join(nested_pred, by = c("household_number", "plan_name")) %>%
      group_by(plan_name) %>%
      summarize(
        tot_nonmiss   = sum(!is.na(pred_purchase)),
        obs_purchase  = sum(choice, na.rm = TRUE),
        pred_purchase = sum(pred_purchase, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(region = r, year = y)

    write_csv(treated_dat, file.path(cell_dir, paste0("cell_", r, "_", y, "_pred.csv")))
  }
}

# 3. Coefficients
coefs <- tryCatch(tidy(mod), error = function(e) NULL)
if (!is.null(coefs)) {
  coefs <- coefs %>% mutate(region = r, year = y)
  write_csv(coefs, file.path(cell_dir, paste0("cell_", r, "_", y, "_coef.csv")))
}

cat("Cell", r, y, "complete.\n")
