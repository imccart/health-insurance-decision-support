# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Commission revealed-preference bounds. Per insurer-year,
##                variable profit at commission scales k on the insurer's own
##                observed schedule, premiums and rival schedules held at
##                observed, demand from the estimated two-part nested logit
##                (cached choice cells), mc at the GMM solution (s4's
##                mc_gmm.csv, the same marginal cost the commission conditions
##                use), beta the per-carrier substitution rate. A k-deviation
##                moves broker households' within-nest shares and every
##                household's enrollment through the agent state's inclusive
##                value (fixed enrollment when the cells lack the channel
##                probabilities). Reports the deviation inequalities the
##                commission model imposes: the observed schedule against
##                paying nothing and against +/- 25 percent, and the local
##                profit band. Writes results/commission_profit_curves.csv and
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
  select(region, year, plan_id, share) %>%
  inner_join(read_csv(file.path(TEMP_DIR, "mc_gmm.csv"), show_col_types = FALSE),
             by = c("region", "year", "plan_id"))

K_GRID_S6 <- c(0, 0.5, 0.75, 0.9, 1, 1.1, 1.25, 1.5)

# Commission-setting units, as in s4's M4: a carrier-year whose filed schedule
# has distinct network rates deviates one network's schedule at a time
SPLIT_KEYS_S6 <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE) %>%
  group_by(insurer_prefix, year) %>%
  summarize(split = n_distinct(rate) > 1, .groups = "drop") %>%
  filter(split) %>%
  { paste(.$insurer_prefix, .$year, sep = "_") }

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
  ins <- nest_inside_rows(cd, util$V, util$V_base, LAMBDA_S6, util$add_N, util$add_A)
  ins[, prefix := sub("_.*", "", plan_id)]
  ins <- merge(ins, as.data.table(sr_cell)[, .(plan_id, mc_gmm)], by = "plan_id", all.x = TRUE)
  if (anyNA(ins$mc_gmm)) stop("s6: mc_gmm missing for plans in cell ", r, " ", y,
                              " -- re-run s4 so mc_gmm.csv covers every cell")

  # Check: model shares at k = 1 against s3's stored shares
  chk <- ins[, .(model_share = sum(hh_weight * q_j) /
                   cd[, sum(first(hh_weight)), by = household_number][, sum(V1)]), by = plan_id]
  chk <- merge(chk, as.data.table(sr_cell)[, .(plan_id, share)], by = "plan_id")
  max_share_gap <- chk[, max(abs(model_share - share), na.rm = TRUE)]

  # A k-deviation on firm f moves (a) broker households' realized within-nest
  # shares (the commission term in V) and (b) EVERY household's enrollment,
  # through the agent state's inclusive value in the expected-IV margin. The
  # shop-alone and navigator state IVs (iv_0, iv_N) are fixed in k; the agent
  # state is recomputed at the deviated commissions. Cells built without the
  # channel probabilities fall back to fixed enrollment.
  ins[, is_b := broker == 1L]
  use_states_s6 <- all(c("iv_0", "iv_N", "iv_A", "add_A") %in% names(ins))
  cell_has_split <- any(paste(unique(ins$prefix), y, sep = "_") %in% SPLIT_KEYS_S6)
  if (cell_has_split && !"network_type" %in% names(ins))
    stop("s6: cell ", r, " ", y, " lacks network_type, needed for the network units")
  ins[, unit := if (cell_has_split)
    fifelse(paste(prefix, y, sep = "_") %in% SPLIT_KEYS_S6,
            paste0(prefix, fifelse(!is.na(network_type) &
                                     network_type %in% c("HMO", "HSP"),
                                   ".HMO", ".PPO")), prefix) else prefix]
  firms <- sort(unique(ins[comm_pmpm > 0, unit]))
  plan_info <- unique(ins[, .(plan_id, prefix, unit, premium_posted, comm_pmpm, mc_gmm)])

  out <- list()
  for (f in firms) {
    f_carrier <- sub("[.].*$", "", f)
    beta_f <- if (f_carrier %in% names(BETA_F)) BETA_F[[f_carrier]] else BETA_DEFAULT_S6
    for (k in K_GRID_S6) {
      ins[, d_k := fifelse(unit == f, BETA_COMM_S6 * (k - 1) * comm_pmpm, 0)]
      # Broker households' within-nest shares at the deviated commissions
      # (commission_broker = comm_pmpm on their rows)
      ins[, s_jg_k := s_jg]
      ins[is_b == TRUE, s_jg_k := {
        w <- exp((V + d_k) / LAMBDA_S6 - max((V + d_k) / LAMBDA_S6))
        w / sum(w)
      }, by = household_number]
      if (use_states_s6) {
        ins[, VA_k := (V_base + add_A + d_k) / LAMBDA_S6]
        ins[, iv_A_k := { m <- max(VA_k); m + log(sum(exp(VA_k - m))) },
            by = household_number]
        ins[, lI_k := LAMBDA_S6 * (p_none_hat * iv_0 + p_nav_hat * iv_N +
                                     p_agent_hat * iv_A_k)]
        ins[, mx_k := pmax(lI_k, V_0)]
        ins[, s_g_k := exp(lI_k - mx_k) / (exp(lI_k - mx_k) + exp(V_0 - mx_k))]
      } else {
        ins[, s_g_k := s_g]
      }
      # Profit over the carrier's plans; the deviated scale applies only to the
      # unit's schedule, the sister network stays at its observed rates
      mem <- ins[prefix == f_carrier,
                 .(mem = sum(hh_weight * s_jg_k * s_g_k),
                   mem_b = sum(hh_weight * s_jg_k * s_g_k * is_b)), by = plan_id]
      pf <- merge(plan_info[prefix == f_carrier], mem, by = "plan_id", all.x = TRUE)
      pf[is.na(mem), mem := 0]; pf[is.na(mem_b), mem_b := 0]
      pi_m <- pf[, sum((premium_posted - mc_gmm) * mem -
                       (1 - beta_f) * fifelse(unit == f, k, 1) * comm_pmpm * mem_b)]
      out[[length(out) + 1]] <- data.table(region = r, year = y, firm = f, k = k,
        profit_month = pi_m, members = pf[, sum(mem)], members_b = pf[, sum(mem_b)])
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
                                 "BETA_F", "BETA_DEFAULT_S6", "sr_s6", "K_GRID_S6",
                                 "SPLIT_KEYS_S6"))
res_s6 <- rbindlist(parallel::parLapplyLB(cl_s6, cell_files_s6, function(fp)
  tryCatch(profit_cell_s6(fp), error = function(e) {
    cat("  ERR", basename(fp), ":", conditionMessage(e), "\n"); NULL })))
parallel::stopCluster(cl_s6)
if (nrow(res_s6) == 0) stop("s6: no cells returned; check the worker error messages above")
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

