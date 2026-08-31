# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Commission revealed-preference bounds. Per insurer-year,
##                variable profit at commission scales k on the insurer's own
##                observed schedule, premiums and rival schedules held at
##                observed, demand from the estimated two-part nested logit
##                (cached choice cells), mc at the structural values, beta the
##                per-carrier substitution rate. The commission term is
##                excluded from the enrollment stage, so P(insured) is fixed in
##                k and only agent-assisted households' within-nest shares
##                move. Reports the deviation inequalities the commission model
##                imposes: the observed schedule against paying nothing and
##                against +/- 25 percent, and the local profit band. Writes
##                results/commission_profit_curves.csv and
##                results/commission_bounds.csv. Sourced by _analysis.R after
##                s5; standalone-safe given the driver preamble.

cat("\n=== s6: commission revealed-preference bounds ===\n")

coefs_s6  <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
cmap_s6   <- setNames(coefs_s6$estimate, coefs_s6$term)
LAMBDA_S6 <- cmap_s6[["lambda"]]
BETA_COMM_S6 <- cmap_s6[["commission_broker"]]
beta_carrier_s6 <- read_csv("data/output/commission_beta_carrier.csv", show_col_types = FALSE)
BETA_F <- setNames(beta_carrier_s6$beta, beta_carrier_s6$insurer_prefix)
BETA_DEFAULT_S6 <- read_csv("data/output/mlr_admin_beta.csv", show_col_types = FALSE)$beta0[1]
sr_s6 <- read_csv("results/supply_results.csv", show_col_types = FALSE) %>%
  select(region, year, plan_id, share, mc_structural)

K_GRID_S6 <- c(0, 0.5, 0.75, 0.9, 1, 1.1, 1.25, 1.5)

# Schedule revision history, for the adjustment-cost brackets: a firm-year is a
# revision when its schedule differs from the prior year's; k_keep is the scale
# that reproduces the prior schedule in the current year (NA when the schedule
# type switched). 2014 has no prior year and enters neither bracket.
comm_hist <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE) %>%
  arrange(insurer_prefix, year) %>%
  group_by(insurer_prefix) %>%
  mutate(rate_prev = lag(rate), pct_prev = lag(is_pct),
         known = !is.na(rate_prev),
         revised = known & (rate != rate_prev | is_pct != pct_prev),
         k_keep = if_else(revised & is_pct == pct_prev & rate > 0, rate_prev / rate, NA_real_)) %>%
  ungroup() %>%
  select(firm = insurer_prefix, year, known, revised, k_keep)
K_EXTRA <- comm_hist %>% filter(!is.na(k_keep)) %>%
  { setNames(as.list(.$k_keep), paste(.$firm, .$year, sep = "_")) }

CELL_DIR_S6 <- file.path(TEMP_DIR, "choice_cells")
cell_files_s6 <- list.files(CELL_DIR_S6, pattern = "^cell_.*_data\\.csv$", full.names = TRUE)
cat("  cells:", length(cell_files_s6), "| lambda", round(LAMBDA_S6, 4),
    "| commission coefficient", round(BETA_COMM_S6, 4), "\n")

profit_cell_s6 <- function(fp) {
  m <- str_match(basename(fp), "^cell_(.+)_(\\d{4})_data\\.csv$")
  r <- m[2]; y <- as.integer(m[3])
  cd <- fread(fp)
  if (!"hh_weight" %in% names(cd)) cd[, hh_weight := weight]

  sr_cell <- sr_s6 %>% filter(region == r, year == y)
  util <- compute_utility(cd, coefs_s6)
  ins <- nest_inside_rows(cd, util$V, util$V_base, LAMBDA_S6)
  ins[, prefix := sub("_.*", "", plan_id)]
  ins <- merge(ins, as.data.table(sr_cell)[, .(plan_id, mc_structural)], by = "plan_id", all.x = TRUE)

  # Check: model shares at k = 1 against s3's stored shares
  chk <- ins[, .(model_share = sum(hh_weight * q_j) /
                   cd[, sum(first(hh_weight)), by = household_number][, sum(V1)]), by = plan_id]
  chk <- merge(chk, as.data.table(sr_cell)[, .(plan_id, share)], by = "plan_id")
  max_share_gap <- chk[, max(abs(model_share - share), na.rm = TRUE)]

  # Agent-assisted households move with k; everyone else is fixed
  ins[, is_b := broker == 1L]
  nb_mem <- ins[is_b == FALSE, .(mem_nb = sum(hh_weight * q_j)), by = plan_id]
  B <- ins[is_b == TRUE, .(household_number, plan_id, prefix, V, s_g, hh_weight,
                           commission_broker, comm_pmpm, premium_posted, mc_structural)]
  firms <- sort(unique(ins[comm_pmpm > 0, prefix]))
  plan_info <- unique(ins[, .(plan_id, prefix, premium_posted, comm_pmpm, mc_structural)])

  out <- list()
  for (f in firms) {
    beta_f <- if (f %in% names(BETA_F)) BETA_F[[f]] else BETA_DEFAULT_S6
    k_set <- K_GRID_S6
    ek <- K_EXTRA[[paste(f, y, sep = "_")]]
    if (!is.null(ek)) k_set <- sort(unique(c(k_set, ek)))
    for (k in k_set) {
      if (nrow(B) > 0) {
        B[, V_k := V + fifelse(prefix == f, BETA_COMM_S6 * (k - 1) * commission_broker, 0)]
        B[, w := exp(V_k / LAMBDA_S6 - max(V_k / LAMBDA_S6)), by = household_number]
        B[, s_jg_k := w / sum(w), by = household_number]
        b_mem <- B[, .(mem_b = sum(hh_weight * s_jg_k * s_g)), by = plan_id]
      } else b_mem <- data.table(plan_id = character(0), mem_b = numeric(0))
      pf <- merge(plan_info[prefix == f], nb_mem, by = "plan_id", all.x = TRUE)
      pf <- merge(pf, b_mem, by = "plan_id", all.x = TRUE)
      pf[is.na(mem_nb), mem_nb := 0]; pf[is.na(mem_b), mem_b := 0]
      pi_m <- pf[, sum((premium_posted - mc_structural) * (mem_nb + mem_b) -
                       (1 - beta_f) * k * comm_pmpm * mem_b)]
      out[[length(out) + 1]] <- data.table(region = r, year = y, firm = f, k = k,
        profit_month = pi_m, members = pf[, sum(mem_nb + mem_b)], members_b = pf[, sum(mem_b)])
    }
  }
  res <- rbindlist(out)
  cat(sprintf("  cell %s %s: %d firms, max share gap %.5f\n", r, y, length(firms), max_share_gap))
  res
}

n_workers_s6 <- max(1L, parallel::detectCores() - 2L)
cl_s6 <- parallel::makeCluster(min(10L, n_workers_s6), type = "PSOCK", outfile = "")
parallel::clusterEvalQ(cl_s6, {
  suppressMessages({ library(tidyverse); library(data.table) })
  source("code/data-build/_helpers.R"); source("code/analysis/helpers/constants.R")
  source("code/analysis/helpers/covariates.R"); source("code/analysis/helpers/choice.R")
  source("code/analysis/helpers/supply.R"); setDTthreads(1)
})
parallel::clusterExport(cl_s6, c("profit_cell_s6", "coefs_s6", "LAMBDA_S6", "BETA_COMM_S6",
                                 "BETA_F", "BETA_DEFAULT_S6", "sr_s6", "K_GRID_S6", "K_EXTRA"))
res_s6 <- rbindlist(parallel::parLapplyLB(cl_s6, cell_files_s6, function(fp)
  tryCatch(profit_cell_s6(fp), error = function(e) {
    cat("  ERR", basename(fp), ":", conditionMessage(e), "\n"); NULL })))
parallel::stopCluster(cl_s6)
cat("  cells returned:", uniqueN(res_s6[, .(region, year)]), "of", length(cell_files_s6), "\n")

# Insurer-year curves, annual market dollars (12 months, / SAMPLE_FRAC)
curves <- res_s6[, .(profit = sum(profit_month) * 12 / SAMPLE_FRAC,
                     members = sum(members), members_b = sum(members_b)), by = .(firm, year, k)]
write_csv(curves, "results/commission_profit_curves.csv")

# The deviation inequalities: observed against zero and the band edges, and the
# local profit band as a share of variable profit
wide <- curves %>% select(firm, year, k, profit) %>%
  pivot_wider(names_from = k, values_from = profit, names_prefix = "k")
bounds <- wide %>%
  transmute(firm, year,
            profit_obs = k1,
            d_zero = k0 - k1,
            d_dn25 = k0.75 - k1,
            d_up25 = k1.25 - k1,
            band_pct = 100 * (pmax(k0.75, k1, k1.25) - pmin(k0.75, k1, k1.25)) / abs(k1),
            obs_beats_zero = k0 < k1,
            obs_beats_band = k1 >= pmax(k0.75, k1.25))
write_csv(bounds, "results/commission_bounds.csv")

cat("\n  deviation inequalities at the observed schedules:\n")
cat("    observed preferred to zero:", sum(bounds$obs_beats_zero), "of", nrow(bounds), "insurer-years\n")
cat("    observed preferred to both band edges:", sum(bounds$obs_beats_band), "of", nrow(bounds), "\n")
cat("    local band (profit range over k in [0.75, 1.25], % of variable profit): median",
    round(median(bounds$band_pct), 2), " max", round(max(bounds$band_pct), 2), "\n")
print(bounds %>% group_by(firm) %>%
        summarise(n = n(), beats_zero = sum(obs_beats_zero),
                  mean_dzero_m = round(mean(d_zero) / 1e6, 1),
                  mean_band_pct = round(mean(band_pct), 2), .groups = "drop") %>%
        arrange(mean_dzero_m) %>% as.data.frame(), row.names = FALSE)
cat("  -> results/commission_profit_curves.csv, commission_bounds.csv\n")

# Adjustment-cost brackets. An unchanged schedule reveals that the best
# feasible deviation was not worth the revision cost, so kappa is at least the
# forgone gain; an actual revision reveals that the move beat the cost, so
# kappa is at most the gain of the move over keeping the prior schedule.
# Feasible moves are scales in [0.5, 1.5], the range of observed revisions.
base_k1 <- curves %>% filter(k == 1) %>% select(firm, year, profit1 = profit)
feas <- curves %>% filter(k >= 0.5, k <= 1.5) %>%
  inner_join(base_k1, by = c("firm", "year")) %>%
  group_by(firm, year) %>%
  summarise(best_gain = max(profit - profit1), .groups = "drop")
rev_gain <- curves %>%
  inner_join(comm_hist %>% filter(revised, !is.na(k_keep)), by = c("firm", "year")) %>%
  filter(near(k, k_keep)) %>%
  inner_join(base_k1, by = c("firm", "year")) %>%
  transmute(firm, year, gain_of_move = profit1 - profit)
kappa <- full_join(
  feas %>% inner_join(comm_hist %>% filter(known, !revised), by = c("firm", "year")) %>%
    group_by(firm) %>%
    summarise(kappa_lo = max(pmax(best_gain, 0)), n_inaction = n(), .groups = "drop"),
  rev_gain %>% group_by(firm) %>%
    summarise(kappa_hi = min(gain_of_move), n_revisions = n(), .groups = "drop"),
  by = "firm") %>%
  mutate(consistent = is.na(kappa_lo) | is.na(kappa_hi) | kappa_lo <= kappa_hi)
write_csv(kappa, "results/commission_kappa.csv")
cat("\n  adjustment-cost brackets (annual market dollars):\n")
print(kappa %>% mutate(across(c(kappa_lo, kappa_hi), ~ round(.x / 1e6, 1))) %>%
        rename(kappa_lo_m = kappa_lo, kappa_hi_m = kappa_hi) %>%
        arrange(firm) %>% as.data.frame(), row.names = FALSE)
cat("  firms with kappa_lo <= kappa_hi (menu-cost model consistent):",
    sum(kappa$consistent, na.rm = TRUE), "of", nrow(kappa), "\n")
cat("  -> results/commission_kappa.csv\n")
