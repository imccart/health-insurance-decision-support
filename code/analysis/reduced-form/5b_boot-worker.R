# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-04
## Description:   Subprocess worker for one bootstrap iteration of the nested
##                logit choice model. Reads all converged cell CSVs sequentially,
##                resamples HH, re-estimates, predicts. Called by 5_choice-model.R
##                via system2().
##
## Usage:         Rscript code/analysis/5b_boot-worker.R <boot_iter> <seed>
## Outputs:       data/output/choice_cells/boot_{b}_pred.csv
##                data/output/choice_cells/boot_{b}_coef.csv

# Parse CLI arguments -----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript 5b_boot-worker.R <boot_iter> <seed>")
}
b    <- as.integer(args[1])
seed <- as.integer(args[2])

set.seed(seed)

# Setup (packages + helpers) ----------------------------------------------
source("renv/activate.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(mlogit)
  library(broom)
})

source("code/data-build/_helpers-enrollment.R")
source("code/analysis/_helpers-analysis.R")
source("code/analysis/_helpers-choice.R")

poss_estimate <- possibly(estimate_nested_logit, otherwise = NULL)

# Discover converged cells ------------------------------------------------
cell_dir <- "data/output/choice_cells"
data_files <- list.files(cell_dir, pattern = "^cell_\\d+_\\d+_data\\.csv$",
                         full.names = TRUE)

if (length(data_files) == 0) {
  cat("No cell data files found.\n")
  quit(save = "no", status = 0)
}

# Extract region/year from filenames
cell_info <- tibble(file = data_files) %>%
  mutate(
    basename = basename(file),
    region = as.integer(str_extract(basename, "(?<=cell_)\\d+")),
    year   = as.integer(str_extract(basename, "(?<=_)\\d{4}"))
  )

# Process each cell sequentially ------------------------------------------
bs_pred_iter <- list()
bs_coef_iter <- list()

for (i in seq_len(nrow(cell_info))) {
  cr <- cell_info[i, ]

  d_full <- fread(cr$file, showProgress = FALSE)

  # Resample HH with replacement
  hh_ids <- unique(d_full$household_number)
  hh_resamp <- data.table(
    household_number_orig = sample(hh_ids, length(hh_ids), replace = TRUE),
    new_hh_number = seq_along(hh_ids)
  )

  data_bs <- d_full[hh_resamp, on = c("household_number" = "household_number_orig"),
                     allow.cartesian = TRUE]
  data_bs[, household_number := new_hh_number]
  data_bs[, new_hh_number := NULL]
  rm(d_full, hh_ids, hh_resamp)

  data_est_bs <- data_bs[assisted == 0]
  data_oos_bs <- data_bs[assisted == 1]
  data_oos_bs <- data_oos_bs[plan_name %in% unique(data_est_bs$plan_name)]
  rm(data_bs)

  nest_names_bs <- unique(data_est_bs$plan_name)
  nest1_bs <- nest_names_bs[nest_names_bs != "Uninsured"]

  nest_names <- nest1_bs  # mlogit::predict looks for this in calling env
  mod_bs <- poss_estimate(d = as_tibble(data_est_bs), nest_names = nest1_bs)
  if (is.null(mod_bs)) {
    rm(data_est_bs, data_oos_bs)
    gc(verbose = FALSE)
    next
  }

  # Predict
  pred_data <- rbind(data_oos_bs, data_est_bs)
  rm(data_est_bs)
  oos_idx <- tryCatch(
    mlogit.data(as_tibble(pred_data), choice = "choice", shape = "long",
                chid.var = "household_number", alt.var = "plan_name"),
    error = function(e) NULL
  )
  rm(pred_data)
  if (is.null(oos_idx)) {
    rm(data_oos_bs, mod_bs)
    gc(verbose = FALSE)
    next
  }

  nested_pred <- tryCatch(predict(mod_bs, newdata = oos_idx), error = function(e) NULL)
  rm(oos_idx)
  if (is.null(nested_pred)) {
    rm(data_oos_bs, mod_bs)
    gc(verbose = FALSE)
    next
  }

  nested_pred <- as_tibble(nested_pred, rownames = "household_number") %>%
    mutate(household_number = as.numeric(household_number)) %>%
    pivot_longer(!household_number, names_to = "plan_name",
                 values_to = "pred_purchase")

  treated_dat <- as_tibble(data_oos_bs) %>%
    left_join(nested_pred, by = c("household_number", "plan_name")) %>%
    group_by(plan_name) %>%
    summarize(
      tot_nonmiss   = sum(!is.na(pred_purchase)),
      obs_purchase  = sum(choice, na.rm = TRUE),
      pred_purchase = sum(pred_purchase, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(region = cr$region, year = cr$year)
  rm(data_oos_bs, nested_pred)

  bs_pred_iter[[length(bs_pred_iter) + 1]] <- treated_dat

  coefs_bs <- tryCatch(tidy(mod_bs), error = function(e) NULL)
  if (!is.null(coefs_bs)) {
    coefs_bs <- coefs_bs %>% mutate(region = cr$region, year = cr$year)
    bs_coef_iter[[length(bs_coef_iter) + 1]] <- coefs_bs
  }

  rm(mod_bs)
  gc(verbose = FALSE)
}

# Aggregate and write results ---------------------------------------------
if (length(bs_pred_iter) > 0) {
  bs_pred <- bind_rows(bs_pred_iter) %>%
    group_by(plan_name) %>%
    summarize(
      tot_nonmiss   = sum(tot_nonmiss, na.rm = TRUE),
      obs_purchase  = sum(obs_purchase, na.rm = TRUE),
      pred_purchase = sum(pred_purchase, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(boot = b)
  write_csv(bs_pred, file.path(cell_dir, paste0("boot_", b, "_pred.csv")))
}

if (length(bs_coef_iter) > 0) {
  bs_coef <- bind_rows(bs_coef_iter) %>% mutate(boot = b)
  write_csv(bs_coef, file.path(cell_dir, paste0("boot_", b, "_coef.csv")))
}

cat("Bootstrap iteration", b, "complete.\n")
