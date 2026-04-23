# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Standalone demand sensitivity analysis. Builds the cell-
##                level data for the nested logit estimator. Estimation
##                logic added separately on top of this foundation.

# Setup -------------------------------------------------------------------

source("code/0-setup.R")
source("code/data-build/_helpers.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/estimate_demand.R")

# Hyperparameters ---------------------------------------------------------

TEMP_DIR       <- "D:/temp-research-data/health-insurance-decision-support"
SAMPLE_FRAC    <- 0.05
MASTER_SEED    <- 20260224
TARGET_REGIONS <- c(1L, 4L, 8L, 13L, 16L)
TARGET_YEARS   <- c(2014L, 2016L, 2018L)
TARGET_CELLS   <- expand.grid(region = TARGET_REGIONS, year = TARGET_YEARS)

# Data prep ---------------------------------------------------------------

# Regenerate hh_full.csv from the current data-build outputs.
source("code/analysis/1_decision-analysis.R")
rm(hh_full, hh_ins)
gc(verbose = FALSE)

hh_full <- read_csv("data/output/hh_full.csv", show_col_types = FALSE)
plan_data <- read_csv("data/input/Covered California/plan_data.csv")
broker_density <- read_csv("data/output/broker_density.csv", show_col_types = FALSE)
commission_lookup <- read_csv("data/output/commission_lookup.csv", show_col_types = FALSE)

# Control-function residual for assistance (broker-density first stage).
# `assisted` is observed only on insured (CC) rows; v_hat = NA for ACS.
hh_full <- hh_full %>%
  left_join(broker_density %>% select(region, year, n_agents),
            by = c("region", "year"))
ins_idx  <- which(hh_full$insured == 1L)
fs_model <- lm(assisted ~ n_agents + FPL + perc_0to17 + perc_18to25 +
                  perc_65plus + perc_black + perc_hispanic + perc_asian +
                  perc_male + household_size + factor(year),
                data = hh_full %>% filter(insured == 1L))
hh_full$v_hat <- NA_real_
hh_full$v_hat[ins_idx] <- residuals(fs_model)
rm(fs_model, broker_density, ins_idx)
gc(verbose = FALSE)


# Plan choice — one row per plan_id. Hausman IV uses mean of OTHER plans'
# premiums in same (issuer × base_metal × year) group.
plan_choice <- plan_data %>%
  select(region, year = ENROLLMENT_YEAR, Issuer_Name,
         metal = metal_level, plan_id = Plan_Name2,
         network_type = PLAN_NETWORK_TYPE,
         premium = Premium, msp = MSP, hsa = HSA) %>%
  mutate(region = as.integer(region), year = as.integer(year),
         issuer = standardize_insurer(Issuer_Name),
         base_metal = sub(" - Enhanced.*", "", metal)) %>%
  select(-Issuer_Name) %>%
  filter(metal != "Minimum Coverage") %>%
  group_by(issuer, base_metal, year) %>%
  mutate(n_other    = n() - 1L,
         hausman_iv = (sum(premium) - premium) / pmax(n_other, 1L)) %>%
  ungroup() %>%
  mutate(hausman_iv = if_else(n_other == 0, premium, hausman_iv)) %>%
  select(-n_other, -base_metal)

first_stage <- lm(premium ~ hausman_iv + metal + network_type + factor(year),
                  data = plan_choice)
plan_choice$cf_resid <- residuals(first_stage)
rm(first_stage)
gc(verbose = FALSE)

plan_choice <- plan_choice %>%
  mutate(insurer_prefix = sub("_.*", "", plan_id)) %>%
  left_join(commission_lookup, by = c("insurer_prefix", "year")) %>%
  mutate(comm_pmpm = case_when(is.na(rate) ~ 0,
                                is_pct      ~ rate * premium,
                                TRUE        ~ rate)) %>%
  select(-insurer_prefix, -rate, -is_pct)

# HH choice — drop catastrophic enrollees; ACS HHs (plan_id NA) pass.
hh_choice <- hh_full %>%
  filter(!str_detect(plan_id, "_CAT$") | is.na(plan_id)) %>%
  mutate(region = as.integer(region), year = as.integer(year))
rm(hh_full)
gc(verbose = FALSE)


# Estimation -------------------------------------------------------------
## Simple MNL with just premium

library(mlogit)

set.seed(MASTER_SEED)

# Build per-cell long-format choice data (sampling stratified by CC/ACS
# happens inside build_choice_data) and pool across target cells.
cell_data <- bind_rows(lapply(seq_len(nrow(TARGET_CELLS)), function(i) {                                                                                                                                                                                                                           
  r <- TARGET_CELLS$region[i]                                                                                                                                                                                                                                                                      
  y <- TARGET_CELLS$year[i]                                                                                                                                                                                                                                                                        
  build_choice_data(                                                                                                                                                                                                                                                                               
    plans        = plan_choice %>% filter(region == r, year == y),
    hhs          = hh_choice   %>% filter(region == r, year == y),
    sample_frac  = SAMPLE_FRAC,
    spec         = "premium",
    premium_type = "evan"
  ) %>% mutate(household_number = paste(household_number, r, y, sep = "_"))
}))

# Fit MNL: choice ~ alternative-specific premium only.
mldata  <- mlogit.data(cell_data, choice = "choice", shape = "long",
                       alt.var = "plan_id", chid.var = "household_number")
inside_plans <- setdiff(unique(cell_data$plan_id), "Uninsured")                       
rm(cell_data)                       


mnl_fit   <- mlogit(choice ~ premium | 0 | 0, data = mldata)
print(summary(mnl_fit))

mnl_fit_w <- mlogit(choice ~ premium | 0 | 0, data = mldata, weights = hh_weight)
print(summary(mnl_fit_w))

nl_fit <- mlogit(choice ~ premium | 0 | 0, data = mldata,
                 nests = list(insured = inside_plans, uninsured = "Uninsured"),
                 un.nest.el = TRUE)
print(summary(nl_fit))


# Nested logit via custom BFGS-BHHH (mlogit doesn't scale to this size).
# First fit with premium only — should match nl_fit above as a sanity check.
covars <- c("premium")
cell_dir <- file.path(TEMP_DIR, "demand_cells")
unlink(cell_dir, recursive = TRUE)
dir.create(cell_dir, recursive = TRUE)

for (i in seq_len(nrow(TARGET_CELLS))) {
  r <- TARGET_CELLS$region[i]
  y <- TARGET_CELLS$year[i]
  cd <- build_choice_data(
    plans        = plan_choice %>% filter(region == r, year == y),
    hhs          = hh_choice   %>% filter(region == r, year == y),
    sample_frac  = SAMPLE_FRAC,
    spec         = covars,
    premium_type = "evan"
  )
  cd$household_number <- paste(cd$household_number, r, y, sep = "_")
  fwrite(cd, file.path(cell_dir, sprintf("cell_%d_%d_data.csv", r, y)))
}

cells <- normalize_weights(load_all_cells(cell_dir, covars, filter_assisted = -1L)$cells)
theta_opt <- bfgs_bhhh(c(rep(0, length(covars)), 1.0), cells)
names(theta_opt) <- c(covars, "lambda")
print(round(theta_opt, 4))


# Add assistance-commission terms (commission_broker = comm_pmpm × assisted;
# v_hat_commission = broker-density CF × commission_broker).
covars2   <- c("premium", "commission_broker", "v_hat_commission")
cell_dir2 <- file.path(TEMP_DIR, "demand_cells_comm")
unlink(cell_dir2, recursive = TRUE); dir.create(cell_dir2, recursive = TRUE)

for (i in seq_len(nrow(TARGET_CELLS))) {
  r <- TARGET_CELLS$region[i]; y <- TARGET_CELLS$year[i]
  cd <- build_choice_data(
    plans        = plan_choice %>% filter(region == r, year == y),
    hhs          = hh_choice   %>% filter(region == r, year == y),
    sample_frac  = SAMPLE_FRAC,
    spec         = covars2,
    premium_type = "evan"
  )
  cd$household_number <- paste(cd$household_number, r, y, sep = "_")
  fwrite(cd, file.path(cell_dir2, sprintf("cell_%d_%d_data.csv", r, y)))
}

cells2 <- normalize_weights(load_all_cells(cell_dir2, covars2, filter_assisted = -1L)$cells)
theta_opt2 <- bfgs_bhhh(c(rep(0, length(covars2)), 1.0), cells2)
names(theta_opt2) <- c(covars2, "lambda")
print(round(theta_opt2, 4))


# Add premium × demographic interactions (heterogeneous price sensitivity).
covars3 <- c(covars2,
             "hh_size_prem", "perc_0to17_prem", "perc_18to34_prem",
             "perc_35to54_prem", "perc_male_prem", "perc_black_prem",
             "perc_hispanic_prem", "perc_asian_prem", "perc_other_prem",
             "FPL_250to400_prem", "FPL_400plus_prem")
cell_dir3 <- file.path(TEMP_DIR, "demand_cells_demo")
unlink(cell_dir3, recursive = TRUE); dir.create(cell_dir3, recursive = TRUE)

for (i in seq_len(nrow(TARGET_CELLS))) {
  r <- TARGET_CELLS$region[i]; y <- TARGET_CELLS$year[i]
  cd <- build_choice_data(
    plans        = plan_choice %>% filter(region == r, year == y),
    hhs          = hh_choice   %>% filter(region == r, year == y),
    sample_frac  = SAMPLE_FRAC,
    spec         = covars3,
    premium_type = "evan"
  )
  cd$household_number <- paste(cd$household_number, r, y, sep = "_")
  fwrite(cd, file.path(cell_dir3, sprintf("cell_%d_%d_data.csv", r, y)))
}

cells3 <- normalize_weights(load_all_cells(cell_dir3, covars3, filter_assisted = -1L)$cells)
theta_opt3 <- bfgs_bhhh(c(rep(0, length(covars3)), 1.0), cells3)
names(theta_opt3) <- c(covars3, "lambda")
print(round(theta_opt3, 4))


# Add Petrin-Train control function for premium endogeneity (cf_resid =
# first-stage residual of `premium ~ hausman_iv + metal + network_type + year`).
covars4   <- c(covars3, "cf_resid")
cell_dir4 <- file.path(TEMP_DIR, "demand_cells_cf")
unlink(cell_dir4, recursive = TRUE); dir.create(cell_dir4, recursive = TRUE)

for (i in seq_len(nrow(TARGET_CELLS))) {
  r <- TARGET_CELLS$region[i]; y <- TARGET_CELLS$year[i]
  cd <- build_choice_data(
    plans        = plan_choice %>% filter(region == r, year == y),
    hhs          = hh_choice   %>% filter(region == r, year == y),
    sample_frac  = SAMPLE_FRAC,
    spec         = covars4,
    premium_type = "evan"
  )
  cd$household_number <- paste(cd$household_number, r, y, sep = "_")
  fwrite(cd, file.path(cell_dir4, sprintf("cell_%d_%d_data.csv", r, y)))
}

cells4 <- normalize_weights(load_all_cells(cell_dir4, covars4, filter_assisted = -1L)$cells)
theta_opt4 <- bfgs_bhhh(c(rep(0, length(covars4)), 1.0), cells4)
names(theta_opt4) <- c(covars4, "lambda")
print(round(theta_opt4, 4))
