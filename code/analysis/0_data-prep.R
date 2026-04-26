# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Single source of truth for analysis-side data prep.
##                Sourced by both _reduced-form.R and _structural.R so the
##                two paths can never drift on filters, joins, or columns.
##
##                Inputs (all required):
##                  data/output/hh_full.csv         (from 1_decision-analysis.R)
##                  data/output/ipweights.csv       (from 2_ipw.R)
##                  data/output/broker_density.csv  (data-build)
##                  data/output/commission_lookup.csv (data-build)
##                  data/input/Covered California/plan_data.csv
##
##                Outputs (TEMP_DIR):
##                  plan_choice.csv         — region × year × plan, w/ cf_resid, comm_pmpm
##                  plan_demographics.csv   — plan × year shares (structural)
##                  hh_choice.csv           — HH-year panel for cell estimation
##
##                Side effect: leaves `hh_full` in caller's environment.
##                The RF runner needs it for 1_dominated-choices.R; the
##                structural runner can rm() it after sourcing.
##
##                hh_full.csv is assumed already filtered to market-eligible
##                rows by 1_decision-analysis.R. No re-filter here.

cat("=== Data prep ===\n")

# Read inputs -------------------------------------------------------------

hh_full <- fread("data/output/hh_full.csv") %>% as_tibble() %>%
  mutate(region = as.integer(region), year = as.integer(year))

ipweights <- fread("data/output/ipweights.csv") %>% as_tibble()

broker_density <- fread("data/output/broker_density.csv") %>% as_tibble() %>%
  mutate(region = as.integer(region), year = as.integer(year))

commission_lookup <- fread("data/output/commission_lookup.csv") %>% as_tibble()

plan_data <- read_csv("data/input/Covered California/plan_data.csv",
                      show_col_types = FALSE, name_repair = "minimal")

cat("  hh_full:", nrow(hh_full), "rows\n")

# HH-side prep ------------------------------------------------------------
# Join IPW, broker density, drop catastrophic, compute v_hat and p_nav.

hh_full <- hh_full %>%
  left_join(ipweights, by = "household_year") %>%
  left_join(broker_density %>% select(region, year, n_agents),
            by = c("region", "year"))

n_before <- nrow(hh_full)
hh_full <- hh_full %>% filter(!str_detect(plan_id, "_CAT$") | is.na(plan_id))
cat("  Dropped catastrophic HH:", n_before - nrow(hh_full), "\n")

# v_hat: first-stage LPM on insured rows (assisted is undefined for uninsured)
ins_idx <- which(hh_full$insured == 1L)
fs_model <- lm(assisted ~ n_agents + FPL + perc_0to17 + perc_18to25 +
                 perc_65plus + perc_black + perc_hispanic + perc_asian +
                 perc_male + household_size + factor(year),
               data = hh_full[ins_idx, ])
hh_full$v_hat <- NA_real_
hh_full$v_hat[ins_idx] <- residuals(fs_model)
cat("  v_hat first-stage F:", round(summary(fs_model)$fstatistic[1], 1), "\n")
rm(fs_model, ins_idx)

# Navigator propensity (structural-side; harmless extra column for RF)
nav_model <- glm(
  (channel_detail == "Navigator") ~ FPL + perc_hispanic + perc_black +
    perc_0to17 + perc_65plus + household_size + perc_male + factor(year),
  data = hh_full %>% filter(assisted == 1), family = binomial
)
hh_full$p_nav <- predict(nav_model, newdata = hh_full, type = "response")
cat("  p_nav range:", round(range(hh_full$p_nav, na.rm = TRUE), 3), "\n")
rm(nav_model)

# Plan-side prep ----------------------------------------------------------

plan_choice <- plan_data %>%
  select(region, year = ENROLLMENT_YEAR, Issuer_Name,
         metal = metal_level, plan_id = Plan_Name2,
         network_type = PLAN_NETWORK_TYPE,
         premium = Premium, msp = MSP, hsa = `HSA`) %>%
  mutate(region = as.integer(region), year = as.integer(year),
         issuer = standardize_insurer(Issuer_Name),
         base_metal = sub(" - Enhanced.*", "", metal)) %>%
  select(-Issuer_Name) %>%
  filter(metal != "Minimum Coverage")

# Hausman IV: leave-one-out mean premium within (issuer, base_metal, year)
plan_choice <- plan_choice %>%
  group_by(issuer, base_metal, year) %>%
  mutate(n_other = n() - 1L,
         hausman_iv = (sum(premium) - premium) / pmax(n_other, 1L)) %>%
  ungroup() %>%
  mutate(hausman_iv = ifelse(n_other == 0, NA_real_, hausman_iv)) %>%
  group_by(issuer, base_metal, year) %>%
  mutate(hausman_iv = ifelse(is.na(hausman_iv),
                             mean(premium, na.rm = TRUE), hausman_iv)) %>%
  ungroup() %>%
  select(-n_other, -base_metal)

first_stage <- lm(premium ~ hausman_iv + metal + network_type + factor(year),
                  data = plan_choice)
plan_choice$cf_resid <- residuals(first_stage)
cat("  Premium first-stage F:", round(summary(first_stage)$fstatistic[1], 1), "\n")
rm(first_stage)

plan_choice <- plan_choice %>%
  mutate(insurer_prefix = sub("_.*", "", plan_id)) %>%
  left_join(commission_lookup, by = c("insurer_prefix", "year")) %>%
  mutate(comm_pmpm = case_when(
    is.na(rate) ~ 0,
    is_pct      ~ rate * premium,
    TRUE        ~ rate
  )) %>%
  select(-insurer_prefix, -rate, -is_pct)

fwrite(plan_choice, file.path(TEMP_DIR, "plan_choice.csv"))
cat("  plan_choice:", nrow(plan_choice), "rows -> plan_choice.csv\n")

# Plan demographics (insurer × year shares; structural-side input) --------

plan_demographics <- hh_full %>%
  filter(!is.na(plan_id), plan_id != "Uninsured") %>%
  mutate(plan_id = gsub("SIL(94|73|87)", "SIL", plan_id),
         wt = household_size) %>%
  group_by(plan_id, year) %>%
  summarize(share_18to34   = weighted.mean(perc_18to34,   wt, na.rm = TRUE),
            share_35to54   = weighted.mean(perc_35to54,   wt, na.rm = TRUE),
            share_hispanic = weighted.mean(perc_hispanic, wt, na.rm = TRUE),
            .groups = "drop")
fwrite(plan_demographics, file.path(TEMP_DIR, "plan_demographics.csv"))
cat("  plan_demographics:", nrow(plan_demographics), "rows\n")
rm(plan_demographics)

# HH choice file ----------------------------------------------------------
# Single file consumed by both 2_choice-att.R (RF) and 1_demand.R (structural).
# Carries both ipweight (RF needs) and channel_detail/any_agent/p_nav
# (structural needs). Either side ignores columns it doesn't use.

hh_choice <- hh_full %>%
  select(region, year, household_id, FPL, subsidized_members, rating_factor,
         plan_id, oldest_member, cheapest_premium, subsidy, penalty,
         poverty_threshold, household_size, weight, ipweight, v_hat,
         perc_0to17, perc_18to34, perc_35to54,
         perc_black, perc_hispanic, perc_asian, perc_other, perc_male,
         channel, channel_detail, any_agent, p_nav)

fwrite(hh_choice, file.path(TEMP_DIR, "hh_choice.csv"))
n_cells <- length(unique(paste0(hh_choice$region, "_", hh_choice$year)))
cat("  hh_choice:", nrow(hh_choice), "rows,", ncol(hh_choice),
    "cols,", n_cells, "cells -> hh_choice.csv\n")
rm(hh_choice, ipweights, broker_density, plan_data)
gc(verbose = FALSE)

cat("=== Data prep complete ===\n\n")
