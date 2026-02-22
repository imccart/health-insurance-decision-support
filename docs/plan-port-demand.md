# Plan: Port `process.SIPP.R` + `prepare.demand.data.R`

## Context

The enrollment build (scripts 1-5) is complete and producing clean outputs. The next pipeline phase prepares demand estimation data by:
1. Imputing immigration status and employer offer from SIPP onto ACS records (prerequisite)
2. Building the ACS-based outside option (uninsured population) with subsidies and demographics
3. Merging exchange enrollees with ACS, extending the panel, computing penalties, and tagging decision-support channels

The two source scripts total ~2,880 lines of base R. The port converts to tidyverse, consolidates year-by-year repetition, and splits along natural boundaries.

## Pre-requisite: Add `service_channel` and `language_spoken` to enrollment pipeline

These two columns exist in the raw FOIA data but are not currently preserved through scripts 1-5. The demand prep needs them for broker/navigator channel analysis and language controls. Changes:
- `1_load-clean-enrollment.R`: Keep `service_channel` and `language_spoken` through the rename/select chain
- `5_assemble-enrollment.R`: Add both to the final `enroll_clean` select statement

## Pre-requisite: Add packages to `0-setup.R`

Add `SAScii` (for parsing SAS fixed-width input statements) and `readr` (for `read_fwf`) to the groundhog package list. `readr` is part of tidyverse so may already be attached, but `SAScii` is new.

## File Structure

```
code/data-build/
  _helpers-demand.R          (~100 lines)  shared functions for demand prep
  6_process-sipp.R           (~250 lines)  SIPP immigration + employer offer imputation
  7_build-acs-market.R       (~350 lines)  load ACS, households, geography, subsidies, filter
  8_merge-exchange-acs.R     (~300 lines)  merge, plan numbering, dynamic choices, demographics
  9_extend-panel.R           (~200 lines)  panel extension for missing years
  10_finalize-demand.R       (~200 lines)  penalties, channels, final assembly
```

5 new scripts + 1 helper file. Total ~1,400 lines (down from ~2,880).

## Script-by-Script Plan

### `_helpers-demand.R` (~100 lines)

Functions reused across scripts 7-10:

| Function | Purpose |
|----------|---------|
| `compute_demographic_pcts(df, id_col, ...)` | Compute age-group and race/ethnicity percentages per household. Used 3+ times (ACS initial, ACS filtered, exchange). |
| `assign_puma_county(puma, sahie, pumas_lookup)` | Probabilistic PUMA-to-county assignment using SAHIE uninsured distribution. Handles multi-county PUMAs. |
| `compute_cheapest_bronze(plan_data, county_avail, year)` | Cheapest bronze premium per county/rating area. Used for affordability exemption + exchange final. |

Note: `compute_slc_premium()` and `calculate_aca_contribution()` already exist in `_helpers-enrollment.R` and are available since the master runner sources it.

### `6_process-sipp.R` (~250 lines)

**Port of `process.SIPP.R` (629 lines).**

Two sections using the 2008 SIPP panel:

**Section A — Immigration Status (~150 lines):**
- Parse `inputSIPP_core.sas` + `inputSIPP_tm2.sas` via `SAScii::parse.SAScii()`
- Read `l08puw2.zip` (core wave 2) and `p08putm2.zip` (topical module 2) as fixed-width
- Identify unauthorized immigrants: remove citizens, permanent residents, federal benefit recipients, college students
- Harmonize SIPP and ACS covariates (age, sex, race, Hispanic, education, industry, FPL, etc.)
- Load ACS via `input20142.sas` + `usa_00011.dat.gz`
- Fit logit for unauthorized status, predict + sample onto ACS
- Fit logit for permanent resident (among authorized), predict + sample
- Save `data/output/acs_immigration.csv`

**Section B — Employer Offer (~100 lines):**
- Parse `inputSIPP_tm6.sas` + read `l08puw6.zip` and `p08putm6.zip`
- Create household-level employer offer indicator
- Harmonize covariates, fit logit, predict + sample onto ACS
- Save `data/output/acs_emp_offer.csv`

**Input:** SIPP 2008 zips + SAS input files, ACS microdata
**Output:** `acs_immigration.csv`, `acs_emp_offer.csv`

### `7_build-acs-market.R` (~350 lines)

**Port of lines 1-1031 of `prepare.demand.data.R`.**

- Load ACS microdata (`usa_00011.dat.gz` via `input20143.sas` + `read_fwf`)
- Merge SIPP-imputed immigration and employer offer from step 6
- Extend ACS to 2018-2019 by duplicating 2017 records
- Normalize weights (`PERWT / 100`)
- Build household object:
  - Size, weight, uninsured counts
  - FPL via poverty guidelines lookup
  - Subsidy bracket assignment via `findInterval()`
  - PUMA-to-county-to-rating-area via `assign_puma_county()` helper
  - Demographics via `compute_demographic_pcts()` helper
- Income imputation regression (FPL ~ rating_area + demographics for >400% FPL HHs)
  - Save coefficients + residual distribution as CSV (for later use in script 8)
- Filter to market population:
  - Remove undocumented immigrants
  - Remove households with employer offer access
  - Keep only uninsured members
- Determine subsidy eligibility (Medicaid/CHIP thresholds, income)
- Calculate ACA subsidies:
  - SLC premium via county files + `compute_slc_premium()`
  - Contribution via `calculate_aca_contribution()`
  - Handle mixed-subsidy households
- Determine mandate exemptions (American Indian, below filing threshold, unaffordable offer)
- Recalculate demographics after filtering
- Renumber individual IDs (start at 100M to avoid collision with exchange IDs)

**Input:** ACS microdata, step 6 outputs, reference CSVs, county files
**Output:** `data/output/acs_individuals.csv`, `data/output/acs_households.csv`, `data/output/income_model.csv`

### `8_merge-exchange-acs.R` (~300 lines)

**Port of lines 1044-1410 of `prepare.demand.data.R`.**

- Load enrollment clean data + households clean data
- Standardize variable names (household_size, rating_area, FPL rescaling)
- Plan numbering: create `plan_number`, `plan_number_nocsr`, `plan_number_small` from plan names
- Assign head plan per household
- Build dynamic choice matrix (household × year):
  - Track `previous_plan_number`, `next_plan_number`, `previous_plan_offered`
  - Check plan availability across years via `get_available_plans()` join pattern
- Enrollment timing variables (`dep_midyear`, `SEP`)
- Exchange household demographics via `compute_demographic_pcts()`
- Impute FPL for unsubsidized exchange households:
  - Apply income model from step 7 (load coefficients, predict, rank-sample from ACS distribution)
  - For subsidized with missing income: sample within bracket

**Input:** `enrollment_clean.csv`, `households_clean.csv`, step 7 outputs, plan_data
**Output:** `data/output/exchange_individuals.csv`, `data/output/exchange_households.csv`

### `9_extend-panel.R` (~200 lines)

**Port of lines 1413-2017 of `prepare.demand.data.R`. Biggest compression (600→200 lines).**

The old code repeats the reference-finding logic for offsets -1 through ±5 as separate code blocks. The new version uses a single loop:

```r
offsets <- c(-1, 1, -2, 2, -3, 3, -4, 4, -5, 5)
for (offset in offsets) {
  # find unmatched household-years that have a reference at this offset
  # copy reference household, adjust year/age/demographics
}
```

- Expand household grid to all 6 years (2014-2019)
- Find closest reference year for each missing household-year
- Replicate household and individual records from reference
- Adjust ages based on year difference
- Drop households with members 65+ or age < 0
- Recalculate subsidies for extended years (reuse `calculate_aca_contribution()`)
- Compute SLC premiums for new years
- Flag split households across all years
- Merge extended records with exchange data

**Input:** Step 8 outputs, step 7 outputs (ACS market data)
**Output:** `data/output/panel_individuals.csv`, `data/output/panel_households.csv`

### `10_finalize-demand.R` (~200 lines)

**Port of lines 2000-2254 of `prepare.demand.data.R`.**

- Final exchange household variables:
  - Cheapest bronze via `compute_cheapest_bronze()` helper
  - Tax filing status (single/married/household_head from gender composition)
  - Filing threshold by year and status
  - Poverty threshold lookup
  - Household weight (sum of individual weights)
- Penalty calculation:
  - Count adults/children subject to mandate
  - Flat penalty + percentage penalty, capped at national average bronze × 12
  - 2019 penalty = 0
- Broker/navigator channel variables:
  - `agent` (CIA), `broker` (CIA + PBE), `navigator` (SCR + CEW + CEC)
  - `forward_*` (current year or any future year)
  - `ever_*` (any year)
- Final flag consistency between individuals and households
- Merge ACS + exchange into unified datasets
- Column selection matching downstream analysis interface

**Input:** Step 9 outputs
**Output:** `data/output/demand_individuals.csv`, `data/output/demand_households.csv`

## Downstream Interface

The final outputs must satisfy the column requirements of the demand estimation scripts:

**demand_individuals.csv**: individual_id, household_id, household_year, year, age, gender, race, plan_id, plan_name, plan_number, plan_number_nocsr, rating_factor, weight, penalty, subsidy, premiumSLC, SLC_contribution, FPL, subsidy_fpl_bracket, region, zip3, language_spoken, service_channel, flagged

**demand_households.csv**: household_year, household_id, year, members, FPL, subsidized_members, unsubsidized_members, rating_factor, subsidy, premiumSLC, SLC_contribution, penalty, weight, plan_number_nocsr, previous_plan_number, previous_plan_offered, agent, broker, navigator, forward_agent/broker/navigator, ever_agent/broker/navigator, dep_midyear, SEP, perc_male, perc_0to17..perc_65plus, perc_white..perc_other, mean_age, rating_area, zip3, flagged, exempt.belowfiling, exempt.unaffordable, cheapest_premium, filing_threshold

## Key Design Decisions

1. **Stick with 2008 SIPP** — variable names match the old code exactly; 2014 SIPP has different column names and some topical module variables may be missing.
2. **Income model as CSV** — save OLS coefficients + residual distribution as CSV (not RDS). Manually reconstruct prediction in script 8.
3. **`set.seed()` per script** — each script that uses randomness sets its own seed for reproducibility.
4. **ACS years 2018-2019** — duplicate 2017 ACS records (same as old code). No new ACS extract needed.
5. **Reuse existing helpers** — `calculate_aca_contribution()`, `compute_slc_premium()`, `RATING_FACTOR_AGE40` from `_helpers-enrollment.R`.

## Implementation Order

1. Pre-requisites: add `service_channel`/`language_spoken` to scripts 1 and 5, add `SAScii` to packages
2. `_helpers-demand.R`
3. `6_process-sipp.R`
4. `7_build-acs-market.R`
5. `8_merge-exchange-acs.R`
6. `9_extend-panel.R`
7. `10_finalize-demand.R`
8. Update `_data-build.R` master runner

## Verification

After each script, run in VS Code and check:
- Row counts (ACS: ~200K CA households; exchange: ~6.6M household-years; combined panel: larger)
- Subsidy distribution aligns between ACS and exchange populations
- No NA values in required output columns
- Flagged rate remains ~3-4%
- Mandate penalty is 0 for 2019
- Channel variables (agent/broker/navigator) have reasonable prevalence
- Final outputs have all columns needed by downstream demand estimation
