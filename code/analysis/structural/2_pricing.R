# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-06
## Date Edited:   2026-03-26
## Description:   Supply-side markup recovery (pure R).
##                Reads parquet partitions, computes nested logit shares and
##                elasticities, recovers markups via Bertrand FOC with
##                broker-commission correction. Also estimates structural RA
##                regressions and validates FOC-implied MC against predicted MC.

# Dependencies: tidyverse, data.table, arrow, helpers (loaded by _structural.R)

# Tuning parameters -------------------------------------------------------

SAMPLE_FRAC   <- as.numeric(Sys.getenv("SAMPLE_FRAC"))
MASTER_SEED   <- as.integer(Sys.getenv("MASTER_SEED"))
TEMP_DIR      <- Sys.getenv("TEMP_DIR")
PARTITION_DIR <- file.path(TEMP_DIR, "hh_choice_partitions")

# =========================================================================
# Load coefficients and reference data
# =========================================================================

cat("\nLoading demand coefficients and reference data...\n")

coefs <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
lambda <- coefs %>% filter(term == "lambda") %>% pull(estimate)
cat("  lambda =", round(lambda, 4), "\n")
cat("  Coefficients:", nrow(coefs), "terms\n")

plan_choice <- read_csv(file.path(TEMP_DIR, "plan_choice.csv"), show_col_types = FALSE)
commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

# =========================================================================
# Estimate RA regressions from rate filing data
# =========================================================================

cat("\nEstimating RA regressions...\n")
rsdata <- read_csv("data/output/rate_filing_rsdata.csv", show_col_types = FALSE)

# Merge plan-level demographics from observed enrollment
plan_demo <- read_csv(file.path(TEMP_DIR, "plan_demographics.csv"), show_col_types = FALSE)
rsdata <- rsdata %>%
  left_join(plan_demo, by = c("plan_name", "year"))
n_matched <- sum(!is.na(rsdata$share_18to34))
cat("  Demographics merged:", n_matched, "of", nrow(rsdata), "plan-years matched\n")
rm(plan_demo)

ra_regs <- estimate_ra_regressions(rsdata)

# Save coefficients for counterfactual worker
rs_coefs_df <- tibble(term = names(ra_regs$rs_coefs), estimate = ra_regs$rs_coefs)
claims_coefs_df <- tibble(term = names(ra_regs$claims_coefs), estimate = ra_regs$claims_coefs)
write_csv(rs_coefs_df, file.path(TEMP_DIR, "ra_rs_coefs.csv"))
write_csv(claims_coefs_df, file.path(TEMP_DIR, "ra_claims_coefs.csv"))

# Reinsurance factors by plan-year (for counterfactuals)
reins_df <- rsdata %>%
  select(plan_name, year, reins_factor) %>%
  filter(!is.na(reins_factor))
write_csv(reins_df, file.path(TEMP_DIR, "reinsurance_factors.csv"))

cat("  RA coefficients and reinsurance factors saved.\n")
rm(rsdata)

# =========================================================================
# Identify cells and set seeds (same as demand)
# =========================================================================

partition_files <- list.files(PARTITION_DIR, pattern = "^hh_\\d+_\\d+\\.parquet$",
                              full.names = FALSE)
cells <- tibble(file = partition_files) %>%
  mutate(
    region = as.integer(str_extract(file, "(?<=hh_)\\d+")),
    year   = as.integer(str_extract(file, "(?<=_)\\d{4}"))
  ) %>%
  arrange(region, year)

cat("  Region-year cells:", nrow(cells), "\n")

set.seed(MASTER_SEED)
cell_seeds <- sample.int(1e7, nrow(cells))

# =========================================================================
# Loop over cells: build data, compute markups
# =========================================================================

cat("\nComputing supply-side markups...\n")

results_list <- vector("list", nrow(cells))
n_done <- 0L
n_skip <- 0L

for (i in seq_len(nrow(cells))) {
  tryCatch({
  r <- cells$region[i]
  y <- cells$year[i]

  set.seed(cell_seeds[i])
  hhs <- tryCatch(
    read_parquet(file.path(PARTITION_DIR, paste0("hh_", r, "_", y, ".parquet"))),
    error = function(e) NULL
  )
  if (is.null(hhs) || nrow(hhs) == 0) { n_skip <- n_skip + 1L; next }

  plans <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans) == 0) { n_skip <- n_skip + 1L; rm(hhs); next }

  # Add commission PMPM if not already present
  if (!"comm_pmpm" %in% names(plans)) {
    plan_names_unique <- unique(plans$plan_name)
    comm_vec <- get_commission_pmpm(plan_names_unique, plans, y, commission_lookup)
    comm_df <- tibble(plan_name = names(comm_vec), comm_pmpm = unname(comm_vec))
    plans <- plans %>%
      left_join(comm_df, by = "plan_name")
  }

  # Build supply choice data (same seed/sample as demand)
  build_result <- build_supply_choice_data(plans, hhs, SAMPLE_FRAC, spec = STRUCTURAL_SPEC)
  rm(hhs)

  if (is.null(build_result)) { n_skip <- n_skip + 1L; rm(plans); next }
  cell_data  <- build_result$cell_data
  plan_attrs <- build_result$plan_attrs
  rm(build_result)

  # Assisted x metal interactions (if not already present)
  if (!"assisted_silver" %in% names(cell_data)) {
    cell_data$assisted_silver <- cell_data$assisted * cell_data$silver
    cell_data$assisted_bronze <- cell_data$assisted * cell_data$bronze
    cell_data$assisted_gold   <- cell_data$assisted * cell_data$gold
    cell_data$assisted_plat   <- cell_data$assisted * cell_data$platinum
  }
  # Commission x broker interaction (broker/agent only, not navigators)
  if (!"commission_broker" %in% names(cell_data)) {
    if ("any_agent" %in% names(cell_data)) {
      cell_data$commission_broker <- cell_data$comm_pmpm * ifelse(cell_data$any_agent == 1L, cell_data$assisted, 0L)
    } else {
      cell_data$commission_broker <- cell_data$comm_pmpm * cell_data$assisted
    }
  }

  # Plan names and attributes from plan_attrs (post-collapse, always consistent)
  plan_names_cell <- sort(plan_attrs$plan_name)
  J <- length(plan_names_cell)

  if (J < 2) { n_skip <- n_skip + 1L; rm(cell_data, plans, plan_attrs); next }

  # Read attributes directly from plan_attrs — no lookups against plans_cell
  pa <- plan_attrs[match(plan_names_cell, plan_attrs$plan_name), ]
  posted_premium <- setNames(pa$premium_posted, pa$plan_name)
  plan_metal     <- setNames(pa$metal, pa$plan_name)
  plan_issuer    <- setNames(pa$issuer, pa$plan_name)
  plan_avs       <- setNames(pa$av, pa$plan_name)
  comm_vec       <- if ("comm_pmpm" %in% names(pa)) setNames(pa$comm_pmpm, pa$plan_name) else setNames(rep(0, J), plan_names_cell)

  benchmark_plan <- identify_benchmark(plan_attrs)

  # -----------------------------------------------------------------------
  # Step 1: Compute utility
  # -----------------------------------------------------------------------
  util_result <- compute_utility(cell_data, coefs)
  V <- util_result$V

  # -----------------------------------------------------------------------
  # Step 2: Compute shares and elasticities (all HH)
  # -----------------------------------------------------------------------
  se_result <- compute_shares_and_elasticities(
    cell_data, V, lambda, benchmark_plan, plan_attrs, coefs,
    spec = STRUCTURAL_SPEC
  )
  shares    <- se_result$shares
  elast_mat <- se_result$elast_mat

  # -----------------------------------------------------------------------
  # Step 3: Ownership matrix and Omega
  # -----------------------------------------------------------------------
  own_mat <- build_ownership_matrix(plan_names_cell)
  Omega <- -own_mat * elast_mat  # positive diagonal

  # -----------------------------------------------------------------------
  # Step 4: Broker shares and elasticities (assisted HH only)
  # -----------------------------------------------------------------------
  broker_result <- compute_broker_shares_and_elasticities(
    cell_data, V, lambda, benchmark_plan, plan_attrs, coefs,
    spec = STRUCTURAL_SPEC
  )
  broker_elast_mat <- broker_result$broker_elast_mat
  Omega_broker <- -own_mat * broker_elast_mat

  # -----------------------------------------------------------------------
  # Step 5: Risk scores and RA (needed for FOC RA derivative)
  # -----------------------------------------------------------------------
  plan_chars_cell <- tibble(
    plan_name   = plan_names_cell,
    Silver      = as.integer(unname(plan_metal) == "Silver"),
    Gold        = as.integer(unname(plan_metal) == "Gold"),
    Platinum    = as.integer(unname(plan_metal) == "Platinum"),
    HMO         = unname(setNames(pa$hmo, pa$plan_name)[plan_names_cell]),
    trend       = y - 2014L,
    Anthem      = as.integer(grepl("^ANT", plan_names_cell)),
    Blue_Shield = as.integer(grepl("^BS", plan_names_cell)),
    Health_Net  = as.integer(grepl("^HN", plan_names_cell)),
    Kaiser      = as.integer(grepl("^KA", plan_names_cell))
  )

  demo_shares <- tryCatch(
    compute_demographic_shares(cell_data, V, lambda),
    error = function(e) NULL
  )

  avg_prem <- mean(posted_premium, na.rm = TRUE)
  rf_cell <- reins_df %>% filter(year == y)
  reins_vec <- sapply(plan_names_cell, function(pn) {
    rf <- rf_cell$reins_factor[rf_cell$plan_name == pn]
    if (length(rf) == 0) return(0)
    mean(rf, na.rm = TRUE)
  })

  mc_result <- compute_mc(ra_regs$rs_coefs, ra_regs$claims_coefs, plan_chars_cell,
                           demo_shares, shares, avg_prem, plan_avs, reins_vec)
  mc_structural  <- mc_result$mc
  pred_claims    <- mc_result$predicted_claims
  rs_pred        <- tibble(plan_name = names(mc_result$predicted_risk_scores),
                           predicted_risk_score = unname(mc_result$predicted_risk_scores),
                           log_risk_score_hat = unname(mc_result$log_risk_score_hat))
  ra_transfers   <- mc_result$ra_transfers

  # -----------------------------------------------------------------------
  # Step 6: RA derivative for FOC (adverse selection channel)
  # -----------------------------------------------------------------------
  # Insurers internalize that price changes shift enrollment composition,
  # changing RA transfers. This term was previously omitted from the FOC.
  rs_levels <- setNames(rs_pred$predicted_risk_score, rs_pred$plan_name)
  ra_foc <- compute_ra_foc(rs_levels, shares, plan_avs, avg_prem,
                            elast_mat, own_mat)

  # -----------------------------------------------------------------------
  # Step 7: Markup (with RA derivative and commission correction)
  # -----------------------------------------------------------------------
  rhs <- shares + ra_foc + as.numeric(Omega_broker %*% comm_vec)

  markup <- tryCatch(
    solve(Omega, rhs),
    error = function(e) rep(NA_real_, J)
  )

  ra_factor_static <- get_ra_transfer(plan_names_cell, plan_attrs)

  mc_foc <- posted_premium - markup
  lerner <- ifelse(posted_premium > 0, markup / posted_premium, NA_real_)

  # -----------------------------------------------------------------------
  # Step 8: Save FOC inputs for cost-side GMM
  # -----------------------------------------------------------------------
  foc_inputs_dir <- file.path(Sys.getenv("TEMP_DIR"), "foc_inputs")
  if (!dir.exists(foc_inputs_dir)) dir.create(foc_inputs_dir, recursive = TRUE)

  saveRDS(list(
    region         = r,
    year           = y,
    plan_names     = plan_names_cell,
    Omega          = Omega,
    Omega_broker   = Omega_broker,
    shares         = shares,
    comm_vec       = comm_vec,
    posted_premium = posted_premium,
    reins_vec      = reins_vec,
    plan_avs       = plan_avs,
    ra_foc         = ra_foc
  ), file.path(foc_inputs_dir, paste0("foc_", r, "_", y, ".rds")))

  # -----------------------------------------------------------------------
  # Step 8: Collect results
  # -----------------------------------------------------------------------
  cell_result <- tibble(
    region          = r,
    year            = y,
    plan_name       = plan_names_cell,
    issuer          = unname(plan_issuer),
    metal           = unname(plan_metal),
    share           = unname(shares),
    posted_premium  = unname(posted_premium),
    markup          = unname(markup),
    mc_foc          = unname(mc_foc),
    mc_structural   = unname(mc_structural),
    ra_factor_static = unname(ra_factor_static),
    ra_transfer     = unname(ra_transfers),
    predicted_claims = unname(pred_claims),
    predicted_risk_score = unname(rs_pred$predicted_risk_score),
    lerner_index    = unname(lerner),
    commission_pmpm = unname(comm_vec)
  )

  results_list[[i]] <- cell_result
  n_done <- n_done + 1L

  rm(cell_data, plans, plan_attrs, pa, V, se_result, broker_result,
     shares, elast_mat, own_mat, Omega, broker_elast_mat, Omega_broker,
     comm_vec, rhs, markup, posted_premium, ra_factor_static, mc_foc, lerner,
     plan_metal, plan_issuer, cell_result, util_result,
     plan_chars_cell, rs_pred, pred_claims, plan_avs,
     ra_transfers, reins_vec, mc_structural, demo_shares,
     ra_foc, rs_levels, avg_prem, rf_cell)
  gc(verbose = FALSE)

  if (i %% 20 == 0) {
    cat("  Cell", i, "of", nrow(cells), "(done:", n_done, " skip:", n_skip, ")\n")
  }
  }, error = function(e) {
    cat("  ERROR at cell", i, "(region", r, "year", y, "):", conditionMessage(e), "\n")
    cat("  Traceback:\n")
    traceback(4)
    stop(e)
  })
}

rm(plan_choice, commission_lookup)
gc(verbose = FALSE)

cat("  Completed:", n_done, "  Skipped:", n_skip, "\n")

# =========================================================================
# Combine and write results
# =========================================================================

supply_results <- bind_rows(results_list)
rm(results_list)

write_csv(supply_results, "results/supply_results.csv")
cat("\nSupply results:", nrow(supply_results), "rows -> results/supply_results.csv\n")

# =========================================================================
# Diagnostics
# =========================================================================

cat("\n--- Supply Diagnostics ---\n")
cat("  Median Lerner index:", round(median(supply_results$lerner_index, na.rm = TRUE), 3), "\n")
cat("  Markup range: [", round(min(supply_results$markup, na.rm = TRUE), 1),
    ",", round(max(supply_results$markup, na.rm = TRUE), 1), "]\n")
cat("  Median markup:", round(median(supply_results$markup, na.rm = TRUE), 1), "$/month\n")
cat("  Negative MC (FOC) count:", sum(supply_results$mc_foc < 0, na.rm = TRUE),
    "of", nrow(supply_results), "\n")
cat("  Commission FOC summary (commission_pmpm):\n")
print(summary(supply_results$commission_pmpm))

# MC comparison: FOC-implied vs structural
cat("\n--- MC Validation: FOC vs Structural ---\n")
mc_valid <- supply_results %>% filter(!is.na(mc_foc), !is.na(mc_structural))
if (nrow(mc_valid) > 0) {
  mc_cor <- cor(mc_valid$mc_foc, mc_valid$mc_structural, use = "complete.obs")
  cat("  Correlation:", round(mc_cor, 4), "\n")
  cat("  MC (FOC) median:", round(median(mc_valid$mc_foc), 1), "\n")
  cat("  MC (structural) median:", round(median(mc_valid$mc_structural), 1), "\n")
  cat("  RA transfer median:", round(median(mc_valid$ra_transfer, na.rm = TRUE), 1), "\n")
  cat("  Predicted claims median:", round(median(mc_valid$predicted_claims, na.rm = TRUE), 1), "\n")
}

# =========================================================================
# Figures
# =========================================================================

if (!dir.exists("results/figures")) dir.create("results/figures", recursive = TRUE)

plot_data <- supply_results

# 1. Markup distribution by insurer
p_markup_insurer <- plot_data %>%
  filter(!is.na(issuer)) %>%
  ggplot(aes(x = reorder(issuer, markup, median), y = markup)) +
  geom_boxplot(outlier.size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(x = NULL, y = "Markup ($/month)") +
  theme_bw()
ggsave("results/figures/supply_markup_insurer.png", p_markup_insurer, width = 6, height = 4)

# 2. Marginal cost vs posted premium
p_mc_premium <- plot_data %>%
  ggplot(aes(x = posted_premium, y = mc_foc, color = metal)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Posted Premium ($/month)", y = "Marginal Cost ($/month)", color = "Metal") +
  theme_bw()
ggsave("results/figures/supply_mc_vs_premium.png", p_mc_premium, width = 7, height = 5)

# 3. Commission cost vs margin by insurer
p_comm_margin <- plot_data %>%
  filter(!is.na(issuer)) %>%
  group_by(issuer) %>%
  summarize(
    avg_markup = mean(markup, na.rm = TRUE),
    avg_commission = mean(commission_pmpm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = avg_commission, y = avg_markup, label = issuer)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.8, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Average Commission ($/month)", y = "Average Markup ($/month)") +
  theme_bw()
ggsave("results/figures/supply_comm_vs_margin.png", p_comm_margin, width = 6, height = 5)

# 4. Lerner index by metal tier
p_lerner_metal <- plot_data %>%
  filter(!is.na(metal), !is.na(lerner_index)) %>%
  mutate(metal = factor(metal, levels = c("Platinum", "Gold", "Silver",
                                           "Bronze", "Minimum Coverage"))) %>%
  ggplot(aes(x = metal, y = lerner_index)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(x = "Metal Tier", y = "Lerner Index") +
  theme_bw()
ggsave("results/figures/supply_lerner_metal.png", p_lerner_metal, width = 6, height = 4)

# 5. MC validation: FOC vs structural
p_mc_compare <- plot_data %>%
  filter(!is.na(mc_foc), !is.na(mc_structural)) %>%
  ggplot(aes(x = mc_structural, y = mc_foc, color = metal)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "MC (Structural RA Model)", y = "MC (FOC Inversion)", color = "Metal") +
  theme_bw()
ggsave("results/figures/supply_mc_foc_vs_structural.png", p_mc_compare, width = 7, height = 5)

cat("Figures saved to results/figures/.\n")
cat("Supply-side estimation complete.\n")
