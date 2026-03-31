# Health Insurance Decision Support

## Project Overview

Academic research on decision assistance and insurer steering in the ACA health insurance marketplace (Covered California). Two main parts:

1. **Demand-side (reduced form):** Design-based analysis showing decision support tools affect plan choice. Includes dominated choice analysis and nested logit discrete choice estimation with control function correction using broker density as IV.
2. **Supply-side (structural):** Structural model of insurer premium setting with commission-based steering. Commissions treated as exogenous (empirically sticky); pricing FOC only (no commission FOC). Sequential estimation with bootstrapped SEs.

Rebuild of an earlier project (original repo: https://github.com/imccart/aca-decision-support).

## Data Sources

- **Enrollments:** FOIA from Covered California (pra_07192019.csv)
- **Plans:** Covered California website, manually appended (plan_data.csv + product_definitions.csv)
- **Choices:** 2017-2019 downloaded, 2014-2016 FOIA, manually appended (zip3_choices.csv)
- **Age rating, poverty thresholds, contribution percentages, rating areas:** Various CMS/Census files
- **ACS and SIPP:** Multiple files for outside option prediction
- **Rate filings:** CMS website, merged into 2014-2020.RData
- **MLR:** CMS website, separate folders by year (2014-2018)
- **RA/RI:** CMS PDFs, manually extracted to ra_reins.csv (metal-tier factors only)
- **Commissions:** 29 rows, 12 insurers, 2014-2020. Mixed flat PMPM / percentage.
- **Broker density:** CPRA FOIA (RatingRegionAgentEnrollment), 19 regions × 6 years.

Raw data symlinked into `data/input/` from D: drive research-data.

## Code Pipeline

### Data Build (code/data-build/)

Master runner: `_data-build.R`. Phase 1: enrollment (scripts 1-5), Phase 2: demand prep (scripts 6-10), Phase 3: SIPP transitions (script 11), Phase 4: broker density (script 12), Phase 5: rate filings for RA regressions (script 13).

### Analysis (code/analysis/)

Master runner: `_analysis.R` (calls scripts 1-2, then reduced-form and structural).

**Shared:** `_helpers-analysis.R`, `_helpers-choice.R`, `_helpers-supply.R`, `_helpers-ra.R`
- `1_decision-analysis.R` — combines raw data into analysis dataset, saves `hh_full.csv`, `hh_clean.csv`, `hh_ins.csv`
- `2_summary-stats.R` — descriptive stats, propensity scores, IPW weights, grayscale figures (updates saved CSVs)

**Reduced-form (code/analysis/reduced-form/):** Runner `_reduced-form.R`. Reads data from disk, computes CF residual (broker density IV), partitions data.
- `1_dominated-choices.R` — dominated choice regressions + PO ATT with CF
- `2_choice-att.R` — pooled nested logit choice model ATT with CF
- `3_summary.R` — ATT by metal/insurer, figures

**Structural (code/analysis/structural/):** Runner `_structural.R`. Reads from disk, computes CF, partitions.

| Script | Purpose | Key Outputs |
|--------|---------|-------------|
| `1_demand.R` | Builds cell CSVs (R), calls Julia `estimate_demand_v3.jl` | `results/choice_coefficients_structural.csv` |
| `2_supply.R` | Builds choice data, computes markups via Bertrand FOC | `results/supply_results.csv` |
| `3_counterfactuals.R` | Dispatches R subprocess workers (`3a_cf-worker.R`) | `results/counterfactual_results.csv` |

### Julia (code/julia/)

Only demand estimation runs in Julia (1.12.5 via `+release`). Supply and counterfactuals run in R.

**Demand model design:**
- Pooled model: all HH, single coefficient set (31 parameters: 26 base + 4 assisted×metal + commission_broker + v_hat_commission + λ)
- Base covariates: premium, penalty_own, premium_sq, silver, bronze, hh_size_prem, demographic×premium interactions, hmo, hsa, insurer dummies, insurer×metal interactions
- `assisted_silver`, `assisted_bronze`, `assisted_gold`, `assisted_plat` — general effect of any assistance on metal preferences (brokers and navigators)
- `commission_broker = comm_pmpm * any_agent` — commission steering for broker-assisted HH only (zero for navigators)
- `v_hat_commission = v_hat * commission_broker` (CF correction for endogenous broker assistance)
- No `v_hat_assisted` — v_hat is from broker density IV and doesn't vary across plans when interacted with assisted
- V_0 = β'X_0 (NOT normalized to 0)
- Household-size weights (not IPW) — makes predicted shares member-level for supply FOC
- Premiums treated as exogenous (regulated market)

**Supply model design:**
- Markup = solve(Omega, shares + Omega_broker * comm_vec)
- Omega_broker filters on `any_agent == 1` (not `assisted == 1`) — navigators don't respond to commissions
- RA: risk score regression (metal + demographics) + claims regression from rate filings (`_helpers-ra.R`), endogenous in counterfactuals via `compute_demographic_shares`
- MC decomposition: `mc_foc` (FOC inversion) and `mc_structural` (predicted claims - RA transfer - reinsurance)
- Counterfactuals: outer RA iteration loop, broker-to-navigator tau gradient, endogenous subsidies (benchmark premium linkage), corrected CS formula (V_0 ≠ 0)

## Identification Strategy

**Broker density as IV for assistance:** broker availability (n_agents by region-year) affects whether HH seeks help but doesn't directly affect plan choice quality.

**Reduced-form:** IPW weights from `P(assisted | demographics)` without the instrument (Bhattacharya & Vogt 2007). CF residual from first-stage LPM `assisted ~ broker_density + X` corrects for unobservable selection. CF enters choice model interacted with plan indicators (simulation-validated).

**Structural:** CF residual enters demand via `v_hat_commission` to correct the commission coefficient only. The assisted × metal interactions don't need CF correction — they're demand shifters, not the causal parameter of interest.

## Reference Materials (Temporary)

- `_old-repo/` — complete copy of old project (gitignored)
- `_emails/` — exported project emails (gitignored)
- `_overleaf/` — exported Overleaf project (gitignored)
- `_archive/` — old reduced-form scripts (gitignored)

## Paper

Paper at `paper/paper.tex`. Bib and bst in `paper/`. Compile from project root:
1. `xelatex -output-directory=paper paper/paper.tex`
2. `cd paper && bibtex paper && cd ..`
3. `xelatex -output-directory=paper paper/paper.tex` (twice)

Structure: (1) Intro, (2) Background, (3) Data, (4) Does Decision Assistance Improve Plan Choice?, (5) Insurer Premium Setting with Commission-Based Steering, (6) Welfare Effects of Broker Intermediation, (7) Conclusion. Co-authored with Evan Saltzman.

**Output conventions:**
- `data/output/` — intermediate/cleaned datasets only
- `results/` — final estimation CSVs + tables + figures
- `results/tables/` — generated .tex tables (bare tabular, `\hline\hline` style, no booktabs)
- `results/figures/` — generated figures (grayscale, no titles, `theme_bw()` base)
- `background/` — static institutional images (CA rating regions map)
- `presentations/` — old slide decks (creed-202105, lunch-learn-202103)

**Results generation:** `code/analysis/4_paper-results.R` reads estimation outputs from `results/` and intermediate data from `data/output/`, generates all tables and figures.

## Next Steps

1. **Debug CF convergence** — counterfactual tau scenarios and some cells don't converge with nleqslv after RA fix. Observed/uniform scenarios also fail for some cells. Need better starting values or solver tuning.
2. **Paper results** — run `4_paper-results.R` once CF outputs exist; write results text for Sections 5.4, 6.4
3. **Bootstrap SEs** — Hessian-based for demand, parametric bootstrap for supply/CF. Python backup estimator at `code/python/estimate_demand.py`.
4. **New-enrollee sensitivity** — appendix analysis restricting structural demand to `hh_clean`

## Last Session

Date: 2026-03-31

- **Demand estimation completed** via Julia with `BLAS.set_num_threads(1)`. Coefficients match Mar 27 (λ=0.8575, premium=-0.00149). Retry loop in `1_demand.R` handles intermittent LLVM JIT crashes.
- **RA bug fixed** — Kaiser coefficient NA in `ra_claims_coefs.csv` caused all-NA mc_new. Fixed in `predict_claims` (`_helpers-ra.R`) by skipping NA coefficients. RA now iterates properly (iter=10 vs iter=1 before).
- **Tau bug diagnosed** — tau=0≈tau=1 in old results was from an earlier code version, not a current bug. Current `build_scenario_data` is correct (verified with diagnostic showing $8.60 price difference).
- **`_structural.R` restructured** — skips data prep when intermediate files exist, aggressive gc between stages.
- **CF convergence issues** — tau scenarios and some cells fail to converge after RA fix changes MC values. Needs debugging.
- **Infrastructure**: Intel UHD 770 driver updated (was causing BSODs). Arrow package conflicts with readRDS (detach after Phase 1). Python estimator written as backup (`code/python/estimate_demand.py`).
