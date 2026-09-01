# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   cf2 welfare scoring. Reads cf1's solved premiums and commissions
##                (results/counterfactual_results.csv) and re-scores welfare from them,
##                so welfare definitions can be iterated without re-solving. Per cell it
##                reloads the cached choice data, rebuilds each scenario, re-levels
##                premiums to the cf1 solution, and scores (via score_cf.R). Sourced by
##                _analysis.R after cf1; standalone-safe (reads its own inputs).

cat("\n=== cf2: welfare scoring from solved equilibria ===\n")

# Inputs ------------------------------------------------------------------
coefs <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
lambda <- setNames(coefs$estimate, coefs$term)[["lambda"]]
supply_results <- read_csv("results/supply_results.csv", show_col_types = FALSE)
cfres <- as.data.table(read_csv("results/counterfactual_results.csv", show_col_types = FALSE))

demand_spec <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
STRUCTURAL_SPEC <- demand_spec$all
COMM_TERMS <- c("commission_broker")

source("code/analysis/helpers/welfare.R")
CS_TABLE <- read.csv("data/input/ca_standard_cost_sharing.csv", stringsAsFactors = FALSE)
# Age/income spending schedule (NULL falls back to flat MEAN_SPENDING).
SPENDING_SCHEDULE <- load_spending_schedule()
# MEPS uninsured OOP schedule (NULL falls back to the full-spending valuation).
UNINS_SCHED <- load_uninsured_oop()

CELL_DIR <- file.path(TEMP_DIR, "choice_cells")
# Per-household welfare written here, one file per cell (feeds the distribution below).
CF_WELFARE_HH_DIR <- file.path(TEMP_DIR, "cf_welfare_hh")
if (dir.exists(CF_WELFARE_HH_DIR)) unlink(CF_WELFARE_HH_DIR, recursive = TRUE)
dir.create(CF_WELFARE_HH_DIR, recursive = TRUE)
cells_cf <- unique(cfres[, .(region, year)])
cat("  Cells to score:", nrow(cells_cf), "\n")

# Per-cell scorer (shared with cf3) ---------------------------------------
source("code/analysis/helpers/score_cf.R")

# Driver (parallel) -------------------------------------------------------
tasks <- lapply(seq_len(nrow(cells_cf)), function(i) list(r = cells_cf$region[i], y = cells_cf$year[i]))
n_workers <- max(1L, parallel::detectCores() - 2L)
cl <- tryCatch(parallel::makeCluster(n_workers, type = "PSOCK", outfile = ""), error = function(e) NULL)

score_one <- function(task) {
  res <- tryCatch(score_cf_cell(task$r, task$y, cfres[region == task$r & year == task$y], CF_WELFARE_HH_DIR, coefs, lambda),
                  error = function(e) { cat("  ERR r", task$r, "y", task$y, ":", conditionMessage(e), "\n"); NULL })
  if (!is.null(res)) cat(sprintf("  scored r%s y%s (%d scenarios)\n", task$r, task$y, nrow(res)))
  res
}

if (!is.null(cl)) {
  cat("  Parallel:", n_workers, "workers\n")
  parallel::clusterEvalQ(cl, {
    suppressMessages({ library(tidyverse); library(data.table) })
    source("code/data-build/_helpers.R"); source("code/analysis/helpers/constants.R")
    source("code/analysis/helpers/covariates.R"); source("code/analysis/helpers/choice.R")
    source("code/analysis/helpers/supply.R"); source("code/analysis/helpers/ra.R")
    source("code/analysis/helpers/estimate_demand.R")
    source("code/analysis/helpers/cf_cell.R")
    source("code/analysis/helpers/welfare.R")
    data.table::setDTthreads(1)
  })
  parallel::clusterExport(cl, c("score_cf_cell", "coefs", "lambda", "supply_results", "cfres",
    "STRUCTURAL_SPEC", "COMM_TERMS", "CS_TABLE", "SPENDING_SCHEDULE", "UNINS_SCHED", "CELL_DIR", "CF_WELFARE_HH_DIR", "TEMP_DIR"))
  welfare_list <- parallel::parLapplyLB(cl, tasks, score_one)
  parallel::stopCluster(cl)
} else {
  cat("  Serial\n"); welfare_list <- lapply(tasks, score_one)
}

cf_welfare <- rbindlist(welfare_list)
write_csv(cf_welfare, "results/counterfactual_welfare.csv")
cat("  Written", nrow(cf_welfare), "rows to results/counterfactual_welfare.csv\n")

# Internal check: the objective decomposes into its three components ----------
cat("\n  --- decomposition check (spending schedule ",
    if (!is.null(SPENDING_SCHEDULE)) "ON" else "OFF", ") ---\n", sep = "")
cat("    obj = prem+eoop+risk (max |resid|):",
    round(max(abs(cf_welfare$cs_welfare_obj - (cf_welfare$obj_prem + cf_welfare$obj_eoop + cf_welfare$obj_risk)), na.rm = TRUE), 6), "\n")

# Distribution of effects across households (point estimate) -----------------
# Each household's effect vs its own observed choice, summarized per scenario (share
# worse off, mean, p10/50/90) for the money (obj) and navigator (nav) rulers.
cat("\n  Building distribution of household effects...\n")
wq <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]; x[which(cumsum(w) / sum(w) >= p)[1]] }
dist_rows <- lapply(list.files(CF_WELFARE_HH_DIR, full.names = TRUE), function(f) {
  h <- fread(f)
  obs <- h[scenario == "baseline", .(household_number, o_obj = obj, o_nav = nav)]
  m <- merge(h[scenario != "baseline"], obs, by = "household_number")
  m[, .(scenario, w, e_obj = obj - o_obj, e_nav = nav - o_nav)]
})
dist <- rbindlist(dist_rows)
if (nrow(dist) > 0) {
  dsumm <- dist[, .(
    share_worse_obj = sum(w * (e_obj < 0)) / sum(w),
    mean_obj = sum(w * e_obj) / sum(w),
    p10_obj = wq(e_obj, w, .10), p50_obj = wq(e_obj, w, .50), p90_obj = wq(e_obj, w, .90),
    share_worse_nav = sum(w * (e_nav < 0)) / sum(w),
    mean_nav = sum(w * e_nav) / sum(w),
    p10_nav = wq(e_nav, w, .10), p50_nav = wq(e_nav, w, .50), p90_nav = wq(e_nav, w, .90)
  ), by = scenario][order(scenario)]
  write_csv(dsumm, "results/counterfactual_welfare_dist.csv")
  cat("  Written", nrow(dsumm), "scenario rows to results/counterfactual_welfare_dist.csv\n")
}

cat("\ncf2 welfare scoring complete.\n")
