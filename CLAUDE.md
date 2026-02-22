# Health Insurance Decision Support

## Project Overview

Academic research on decision assistance and insurer steering in the ACA health insurance marketplace (Covered California). Two main parts:

1. **Demand-side (reduced form):** Design-based analysis showing decision support tools affect plan choice. Includes dominated choice analysis and nested logit discrete choice estimation.
2. **Supply-side (structural):** Structural model of insurer premium setting and commission setting, building on demand estimates. The commission-setting model is the core contribution.

Rebuild of an earlier project (original repo: https://github.com/imccart/aca-decision-support). The old repo is poorly organized and undocumented. This rebuild starts fresh with a clean, reproducible pipeline.

## Research Questions

- How do decision support tools (navigators, agents/brokers) affect health insurance plan choice?
- Do insurers steer consumers toward higher-margin plans via broker commissions?
- What is the welfare effect of commission-based steering?

## Data Sources

- **Enrollments:** FOIA from Covered California (pra_07192019.csv)
- **Plans:** Covered California website, manually appended (plan_data.csv + product_definitions.csv)
- **Choices:** 2017-2019 downloaded, 2014-2016 FOIA, manually appended (zip3_choices.csv)
- **Age rating:** CCIIO age rating curves from CMS (age_rating_factors.csv)
- **Poverty thresholds:** poverty_guidelines.csv
- **ACA contribution percentages:** contribution_percentages.csv
- **California rating areas:** rating_areas.csv
- **ACS and SIPP:** Multiple files for outside option prediction
- **PUMAs:** Census Bureau (PUMAs.csv)
- **SAHIE:** Census Bureau (sahie_2014.csv)
- **County files:** counties_2014 through counties_2019
- **Rate filings:** CMS website, merged into 2014-2020.RData
- **MLR:** CMS website, separate folders by year (2014-2018)
- **RA/RI:** CMS PDFs, manually extracted to ra_reins.csv

## Identification Strategy

Decision support effect identified by variation in how consumers access the marketplace (direct vs. navigator-assisted vs. agent/broker-assisted). Dominated choice analysis provides reduced-form evidence. Nested logit discrete choice model for structural demand estimation.

Supply side: structural model of insurer premium and commission setting, using demand estimates as inputs.

## Code Pipeline

### Data Build (code/data-build/)

Master runner: `_data-build.R` sources all scripts in order (Phase 1: enrollment, Phase 2: demand prep). Each script also works standalone (reads prior step's CSV from `data/output/`).

**Phase 1: Enrollment pipeline (scripts 1-5)**

| Script | Purpose | Input | Output |
|--------|---------|-------|--------|
| `_helpers-enrollment.R` | Shared functions (insurer standardization, subsidy inversion, SLC premium, ACA contribution) | — | — |
| `1_load-clean-enrollment.R` | Load FOIA CSV, standardize variables, create plan IDs, fix regions | Raw CSVs | `enrollment_step1.csv` |
| `2_validate-geography-income.R` | Geographic choice set validation, income/FPL bracket fixes | step1 | `enrollment_step2.csv` |
| `3_construct-households.R` | Detect anomalies, split multi-household cases, assign HH IDs | step2 | `enrollment_step3.csv` |
| `4_reconcile-income.R` | Invert ACA subsidy formula to recover FPL for singles and families | step3 | `enrollment_step4.csv`, `households_step4.csv` |
| `5_assemble-enrollment.R` | Verify income, reassign ACS plans, rebuild plan IDs, export clean data | step4 | `enrollment_clean.csv`, `households_clean.csv` |

**Phase 2: Demand estimation data (scripts 6-10)**

| Script | Purpose | Input | Output |
|--------|---------|-------|--------|
| `_helpers-demand.R` | Shared functions (demographics, PUMA-county assignment, cheapest bronze, available plans) | — | — |
| `6_process-sipp.R` | SIPP immigration + employer offer imputation onto ACS | SIPP 2008, ACS | `acs_immigration.csv`, `acs_emp_offer.csv` |
| `7_build-acs-market.R` | ACS market population: geography, subsidies, filtering, income model | ACS, step 6 | `acs_individuals.csv`, `acs_households.csv`, `income_model_coefs.csv`, `income_distribution.csv` |
| `8_merge-exchange-acs.R` | Plan numbering, dynamic choices, timing, demographics, FPL imputation | steps 5+7 | `exchange_individuals.csv`, `exchange_households.csv`, `dynamic_choices.csv` |
| `9_extend-panel.R` | Panel extension to all 6 years (2014-2019), subsidy recalculation | step 8 | `panel_individuals.csv`, `panel_households.csv` |
| `10_finalize-demand.R` | Penalties, channels, tax filing, flag consistency, final merge | steps 7+9 | `demand_individuals.csv`, `demand_households.csv` |

**Phase 3: SIPP market transitions (script 11)**

| Script | Purpose | Input | Output |
|--------|---------|-------|--------|
| `11_sipp-market-transitions.R` | SIPP 2014 panel: household demographics, nongroup spells, market transition indicators | `pu2014w1-3.csv`, `status2013-2015.csv` | `sipp_households.csv` |

Bug fix from old code: waves 2-3 used `|` instead of `&` for age brackets (18-34, 35-54), marking everyone as middle-aged. Fixed with `&`. Uses `process_sipp_wave()` function to replace 400+ lines of copy-pasted wave processing.

All scripts source `code/0-setup.R` for packages (renv). The master runner sources both `_helpers-enrollment.R` and `_helpers-demand.R`. Phase 1 scripts (2-5) use conditional reads (`if (!exists("enroll"))`) so they skip redundant CSV I/O when sourced sequentially but still work standalone.

### Analysis (code/analysis/)

Master runner: `_analysis.R` sources setup, loads analysis packages (fixest, kableExtra, cobalt, modelsummary, broom), sets `modelsummary_factory_default = "kableExtra"`, then sources numbered scripts.

**Reduced-form analysis (scripts 1-4)**

| Script | Purpose | Key Outputs |
|--------|---------|-------------|
| `_helpers-analysis.R` | Constants (affordability thresholds), channel/plan derivation functions | — |
| `1_decision-analysis.R` | Load demand data, derive analysis variables, apply SIPP out-of-market filter, create `hh_full`/`hh_clean`/`hh_ins` | — |
| `2_summary-stats.R` | Crosstabs, stacked bars, enrollment time series, PS estimation (on insured), IPW, cobalt balance | `results/tables/summary_stats.tex`, `results/figures/` (7 plots) |
| `3_dominated-choices.R` | Dominated choice FE regressions + potential outcomes bootstrap (200 iter) | `results/tables/dominated_choice_regression.tex`, `results/figures/dom_choice.png` |
| `4_switching.R` | Switching regressions (region FE, HH FE, dom choice among switchers) | `results/tables/switching_regression.tex`, `results/figures/switching_regression.png` |

Key design decisions:
- IPW propensity scores estimated on **insured only** (`hh_ins`), not full panel — uninsured panel observations shouldn't contribute to treated/control weighting
- `dominated_choice` requires insured + CSR-eligible (NA otherwise); `switch` requires continuing enrollee (NA for new enrollees)
- Bootstrap pre-filters to CSR-eligible insured (`hh_po`) before resampling; uses indexed row lookup (no joins)
- **SIPP out-of-market filter** applied in `1_decision-analysis.R`: logit trained on SIPP transition data predicts P(in-market) for uninsured HH; stochastic filter drops out-of-market uninsured (pred_prob < uniform draw). Insured HH are never dropped.

## Reference Materials (Temporary)

These folders are temporary staging areas. The goal is to extract all relevant content (code logic, data files, paper text, email context) into the proper project folders, then delete these sources entirely.

- `_old-repo/` — complete copy of old project (gitignored). Original at `research-projects/aca-decision-support/` deleted 2026-02-20. Includes Evan's Julia code (`analysis/archive/evan-code/`).
- `_emails/` — 119 exported project emails as .msg files + converted .txt (gitignored)
- `_overleaf/` — exported Overleaf project (gitignored). Contains paper draft, appendix, tables, figures, old presentations, funding proposal

**Nothing in these folders should be needed once the rebuild is complete.** All data goes to `data/input/`, all code logic gets rewritten into `code/`, paper content goes to `paper/`, etc.

## Next Steps

1. ~~**Run and debug the full data build**~~ — DONE (2026-02-20). All 10 scripts pass.
2. ~~**Port reduced-form analysis**~~ — DONE (2026-02-22). Scripts 1-4 pass, tables and figures generated.
3. ~~**Port SIPP market transition logit**~~ — DONE (2026-02-22). Script 11 + analysis filter implemented.
4. **Port nested logit choice model** into `code/analysis/` — discrete choice estimation (old code in `choice_data_function_map.R` + `_ChoiceModel.R`). Includes choice set construction, premium calculation, insurer grouping, MNL/mixed logit estimation.
5. **Develop supply-side structural model** in `code/analysis/` (Section 6.3 of paper is empty).
6. **Set up new Overleaf project** linked to `paper/`.

## Open Questions

See `docs/open-questions.md` once the archaeology phase is complete.

## Data Organization

Raw data lives in `research-data/` (central) and is symlinked into `data/input/`:
- `data/input/Covered California` → `research-data/Covered California/` (FOIA enrollments, plans, ratings, counties, rate filings)
- `data/input/SIPP Data` → `research-data/SIPP Data/` (2008 & 2014 panel waves)
- `data/input/ACS Data` → `research-data/ACS Data/` (IPUMS extracts, SAHIE)
- `data/input/Medical Loss Ratio Data` → `research-data/Insurance Market Data/Medical Loss Ratio Data/` (2011-2018)

Symlinks created via `.bat` file in `scratch/create_symlinks.bat`.

## Last Session

Date: 2026-02-22

- **Ported SIPP market transition logit** (script 11): `process_sipp_wave()` replaces 400+ lines of copy-pasted wave processing. Fixed age bracket bug (waves 2-3 used `|` instead of `&`). Output: 6,462 household-year observations.
- **Integrated SIPP out-of-market filter** into `1_decision-analysis.R`: logit on `transitioned`, stochastic filter for uninsured HH.
- Still need to run full `_analysis.R` to verify ATT estimates decrease toward ~1 pp.
- Next: run analysis scripts, then port nested logit choice model.
