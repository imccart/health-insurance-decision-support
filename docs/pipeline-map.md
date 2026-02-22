# Pipeline Map: Old Repository (`_old-repo/`)

## Overview

Two parallel pipelines: (1) demand-side data build and decision-support analysis, (2) supply-side data build for structural steering model. Demand side is more complete; supply side was still in progress.

Two entry points:
- `data-code/build-data.R` -- original master script (sources `paths.R`, runs demand pipeline, then analysis)
- `analysis/decision-support/decision-analysis.R` -- Ian's later rewrite (tidyverse-based, cleaner)

**Important**: Both entry points `source('paths.R')`, which does not exist in the repo. It set absolute paths (Evan's version pointed to `/home/imccart/` on Linux).

---

## Dependency Graph

```
process.SIPP.R ──────────┐
                          ├──> prepare.demand.data.R ──> enroll_data, household_data, plan_data
impute.SIPP.R ───────────┤                                          |
                          │                                          |
process.COVCAL.data.R ───┘                                          |
                                                                     ├──> decision-analysis.R
                                                                     |      ├─ _SummaryStats.R
                                                                     |      ├─ _DominatedChoices.R
                                                                     |      ├─ _ChoiceModel.R
                                                                     |      ├─ _ChoiceSummary.R
                                                                     |      └─ _Switching.R
                                                                     |
                                                                     └──> run.make.data.objects.small.R
                                                                               |
                                                                               v
                                                                         make.julia.learning.R
                                                                               |
                                                                               v
                                                                     J_demand_est.jl (archived)
                                                                               |
                                                                               v
process.MLR.data.R ──> process.rate.data.R ──┐
                                              ├──> prepare.supply.data.R
                              [Julia demand results] ──────────────────┘
                                                              |
                                                              v
                                                    [Structural supply model -- NOT IN REPO]
```

---

## Data Build Pipeline

### Stage 1: ACS/SIPP Processing

**1a. `process.SIPP.R`** -- Process 2008 SIPP to impute unauthorized immigrants and employer offers in ACS
- Inputs: SIPP topical modules 2 & 6, ACS IPUMS extract, SAS layouts
- Outputs: `acs_immigration`, `acs_emp_offer` (RData)
- Key: Logit on SIPP predicts unauthorized status; calibrated to 2.82M undocumented in CA per DHS

**1b. `impute.SIPP.R`** -- Process 2014 SIPP panel (waves 1-3) for outside-option prediction
- Inputs: `pu2014w1-w3.csv`, `status2013-2015.csv`
- Outputs: `sipp_logit` (combined transition logit, used downstream)
- Key: Tracks nongroup insurance spells, determines entry/exit patterns

### Stage 2: Covered California Enrollment

**2. `process.COVCAL.data.R`** (~2900 lines, most complex script)
- Inputs: `pra_07192019.csv` (FOIA), plan_data, zip3_choices, age_rating_factors, etc.
- Outputs: `enroll_temp`, `household_temp`, `family_enroll_temp` (RData)
- Key transformations:
  - Standardizes insurer names
  - Resolves ambiguous region/zip assignments insurer-by-insurer
  - Fills missing FPL brackets using multiple fallback strategies
  - Splits cases into proper households (3 rounds, ~1.4% of cases)

### Stage 3: Demand Data Assembly

**3. `prepare.demand.data.R`** (~2250 lines, second most complex)
- Inputs: ACS, immigration/offer imputations, PUMAs, SAHIE, plan data, county files
- Outputs: `enroll_data`, `household_data`, `plan_data`, `dynamic_choices` (RData)
- Key: Duplicates 2017 ACS for 2018-2019 (should be updated with actual data)

### Stage 4: Supply-Side Data

**4a. `process.MLR.data.R`** -- CMS Medical Loss Ratio files, 2014-2018
- Outputs: `mlr_data.csv`

**4b. `process.rate.data.R`** (~2500 lines) -- CMS rate filings 2014-2020
- Outputs: `convergence.csv`, `ra_by_metal_year.csv`, `rs_reg` (RData)
- Note: Comment says "I need to ask Sam how he constructed the merged file" re: `2014-2020.RData`

**4c. `prepare.supply.data.R`** -- Combines demand results + cost/pricing data
- Inputs: Julia demand output, MLR, rate data, `commission_input.csv`
- Outputs: Supply-side CSVs for structural model

### Stage 5: Julia Data Preparation

**5a. `run.make.data.objects.small.R`** -- Matrix-format data objects (55K household sample)
**5b. `make.julia.learning.R`** -- Flattens to CSV for Julia

---

## Analysis Pipeline

### Decision Support Analysis (Ian's rewrite: `decision-analysis.R`)

| Script | Purpose | Outputs |
|--------|---------|---------|
| Inline (lines 26-137) | Data prep, dominated choice flags, channel variables | `hh.full`, `hh.clean` |
| `_SummaryStats.R` | Summary statistics | `metal_assistance.tex`, `summary_stats.tex`, 8 figures |
| `_DominatedChoices.R` | Reduced-form evidence | `dominated_choice_regression.tex`, 2 figures, 200 bootstrap reps |
| `_ChoiceModel.R` | Nested logit by region-year on unassisted HHs | In-memory choice probability objects |
| `_ChoiceSummary.R` | ATT by metal tier and insurer | `choice_metals.png`, `choice_insurer.png` |
| `_Switching.R` | Switching regressions | `switching_regression.tex`, 1 figure |

### Julia Demand Estimation (Archived)
- `J_demand_est.jl` -- Nested logit via Nelder-Mead
- `J_demand_fnc.jl` -- Analytical gradient and Hessian
- `J_demand_bs.jl` -- Bootstrap with LBFGS + autodiff fallback

---

## Missing Files (Referenced but Not in Repo)

- `paths.R` -- set working directory and path variables
- `common_functions.R` -- referenced via `source(paste0(common,"/common_functions.R"))`
- `commission_input.csv` -- broker/agent commission data by insurer

## Dead Code

- `.nav` suffix files: Evan's versions with his local OneDrive paths (ignore)
- `data-code/archive/`: `_BuildFinalData.R`, `individual.R`, `zip_choices.R` (all superseded)
- `analysis/steering/` duplicates of `data-code/` scripts

---

## Notes for Rebuild

1. **`process.COVCAL.data.R` is the most complex and fragile script** (2900+ lines base R). Port carefully.
2. **`prepare.demand.data.R`** duplicates 2017 ACS for 2018-2019; update with actual data if available.
3. **`decision-analysis.R` (Ian's rewrite)** is much cleaner than `build-data.R`; follow this pattern.
4. **Julia code** contains analytical nested logit gradient/Hessian. R's `mlogit` handles demand-side but Julia was needed for supply-side speed.
5. **Supply-side structural model does not exist in this repo.** Only data preparation exists.
6. **`commission_input.csv`** is referenced but undocumented. Appears to be broker/agent commission schedule data by insurer and year (sourced from Kevin Knauss).
