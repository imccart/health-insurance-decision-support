# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-06
## Date Edited:   2026-03-24
## Description:   Supply-side markup recovery (pure R).
##                Reads parquet partitions, computes nested logit shares and
##                elasticities, recovers markups via Bertrand FOC with
##                broker-commission correction, writes results and figures.

# Setup (idempotent — safe to re-source) -----------------------------------
source("code/0-setup.R")
source("code/data-build/_helpers-enrollment.R")
source("code/analysis/_helpers-analysis.R")
source("code/analysis/_helpers-choice.R")
source("code/analysis/_helpers-supply.R")
library(arrow)

# Tuning parameters -------------------------------------------------------

SAMPLE_FRAC   <- 0.20
PARTITION_DIR <- "data/output/hh_choice_partitions"

# =========================================================================
# Load coefficients and reference data
# =========================================================================

cat("\nLoading demand coefficients and reference data...\n")

coefs <- read_csv("data/output/choice_coefficients_structural.csv", show_col_types = FALSE)
lambda <- coefs %>% filter(term == "lambda") %>% pull(estimate)
cat("  lambda =", round(lambda, 4), "\n")
cat("  Coefficients:", nrow(coefs), "terms\n")

plan_choice <- read_csv("data/output/plan_choice.csv", show_col_types = FALSE)
commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

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

set.seed(20260224)
cell_seeds <- sample.int(1e7, nrow(cells))

# =========================================================================
# Loop over cells: build data, compute markups
# =========================================================================

cat("\nComputing supply-side markups...\n")

results_list <- vector("list", nrow(cells))
n_done <- 0L
n_skip <- 0L

for (i in seq_len(nrow(cells))) {
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
  cell_data <- build_supply_choice_data(plans, hhs, SAMPLE_FRAC)
  rm(hhs)

  if (is.null(cell_data)) { n_skip <- n_skip + 1L; rm(plans); next }

  # Add commission_broker interaction
  if (!"commission_broker" %in% names(cell_data)) {
    cell_data$commission_broker <- cell_data$comm_pmpm * cell_data$assisted
  }

  # Identify plans in this cell (excluding Uninsured)
  plans_cell <- plans
  plan_names_cell <- sort(unique(cell_data$plan_name[cell_data$plan_name != "Uninsured"]))
  J <- length(plan_names_cell)

  if (J < 2) { n_skip <- n_skip + 1L; rm(cell_data, plans, plans_cell); next }

  # Identify benchmark plan
  benchmark_plan <- identify_benchmark(plans_cell)

  # -----------------------------------------------------------------------
  # Step 1: Compute utility
  # -----------------------------------------------------------------------
  util_result <- compute_utility(cell_data, coefs)
  V <- util_result$V

  # -----------------------------------------------------------------------
  # Step 2: Compute shares and elasticities (all HH)
  # -----------------------------------------------------------------------
  se_result <- compute_shares_and_elasticities(
    cell_data, V, lambda, benchmark_plan, plans_cell, coefs
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
    cell_data, V, lambda, benchmark_plan, plans_cell, coefs
  )
  broker_elast_mat <- broker_result$broker_elast_mat
  Omega_broker <- -own_mat * broker_elast_mat

  # -----------------------------------------------------------------------
  # Step 5: Commission vector
  # -----------------------------------------------------------------------
  comm_vec <- sapply(plan_names_cell, function(pn) {
    vals <- cell_data$comm_pmpm[cell_data$plan_name == pn]
    if (length(vals) == 0) return(0)
    mean(vals, na.rm = TRUE)
  })

  # -----------------------------------------------------------------------
  # Step 6: Commission-corrected markup
  # -----------------------------------------------------------------------
  # markup = solve(Omega, shares + Omega_broker * comm_vec) — POSITIVE shares
  rhs <- shares + as.numeric(Omega_broker %*% comm_vec)

  markup <- tryCatch(
    solve(Omega, rhs),
    error = function(e) rep(NA_real_, J)
  )

  # -----------------------------------------------------------------------
  # Step 7: Posted premiums, MC, RA, Lerner
  # -----------------------------------------------------------------------
  posted_premium <- sapply(plan_names_cell, function(pn) {
    mean(plans_cell$premium[plans_cell$plan_name == pn], na.rm = TRUE)
  })

  ra_factor <- get_ra_transfer(plan_names_cell, plans_cell)

  mc <- posted_premium - markup
  lerner <- ifelse(posted_premium > 0, markup / posted_premium, NA_real_)

  # Metal and issuer for each plan
  plan_metal <- sapply(plan_names_cell, function(pn) {
    m <- plans_cell$metal[plans_cell$plan_name == pn]
    if (length(m) == 0) return(NA_character_)
    m[1]
  })
  plan_issuer <- sapply(plan_names_cell, function(pn) {
    ins <- plans_cell$issuer[plans_cell$plan_name == pn]
    if (length(ins) == 0) return(NA_character_)
    ins[1]
  })

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
    mc              = unname(mc),
    ra_factor     = unname(ra_factor),
    lerner_index    = unname(lerner),
    commission_pmpm = unname(comm_vec)
  )

  results_list[[i]] <- cell_result
  n_done <- n_done + 1L

  rm(cell_data, plans, plans_cell, V, se_result, broker_result,
     shares, elast_mat, own_mat, Omega, broker_elast_mat, Omega_broker,
     comm_vec, rhs, markup, posted_premium, ra_factor, mc, lerner,
     plan_metal, plan_issuer, cell_result, util_result)
  gc(verbose = FALSE)

  if (i %% 20 == 0) {
    cat("  Cell", i, "of", nrow(cells), "(done:", n_done, " skip:", n_skip, ")\n")
  }
}

rm(plan_choice, commission_lookup)
gc(verbose = FALSE)

cat("  Completed:", n_done, "  Skipped:", n_skip, "\n")

# =========================================================================
# Combine and write results
# =========================================================================

supply_results <- bind_rows(results_list)
rm(results_list)

write_csv(supply_results, "data/output/supply_results.csv")
cat("\nSupply results:", nrow(supply_results), "rows -> data/output/supply_results.csv\n")

# =========================================================================
# Diagnostics
# =========================================================================

cat("\n--- Supply Diagnostics ---\n")
cat("  Median Lerner index:", round(median(supply_results$lerner_index, na.rm = TRUE), 3), "\n")
cat("  Markup range: [", round(min(supply_results$markup, na.rm = TRUE), 1),
    ",", round(max(supply_results$markup, na.rm = TRUE), 1), "]\n")
cat("  Median markup:", round(median(supply_results$markup, na.rm = TRUE), 1), "$/month\n")
cat("  Negative MC count:", sum(supply_results$mc < 0, na.rm = TRUE),
    "of", nrow(supply_results), "\n")
cat("  Commission FOC summary (commission_pmpm):\n")
print(summary(supply_results$commission_pmpm))

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
  ggplot(aes(x = posted_premium, y = mc, color = metal)) +
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

cat("Figures saved to results/figures/.\n")
cat("Supply-side estimation complete.\n")
