# Health Insurance Decision Support

## Project Overview

Academic research on decision assistance and insurer steering in the ACA health insurance marketplace (Covered California). Two main parts:

1. **Demand-side (reduced form):** Design-based analysis showing decision support tools affect plan choice. Includes dominated choice analysis and nested logit discrete choice estimation with control function correction using broker density as IV.
2. **Supply-side (structural):** Structural model of insurer premium setting with commission-based steering. Commissions treated as exogenous (empirically sticky); pricing FOC with RA derivative (∂RA/∂p). Sequential estimation with bootstrapped SEs.

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

**Helpers (code/analysis/helpers/):**
- `covariates.R` — centralized covariate spec: `get_covariate_menu()`, `write/read_demand_spec()`, `get_prem_interactions()`, `recompute_prem_interactions()`
- `constants.R` — affordability thresholds, rating factor constants, channel/plan derivation functions
- `choice.R` — `build_choice_data(spec=)`, `predict_nested_logit()`, `estimate_nested_logit(spec=)`
- `supply.R` — `build_supply_choice_data(spec=)` (returns `list(cell_data, plan_attrs)`), generic `compute_alpha_i(spec=)`, elasticities, ownership
- `ra.R` — risk scores, claims, RA transfers, `compute_mc()`, `compute_ra_foc()`
- `cf_worker.R` — CF subprocess per cell, loads spec from `data/output/demand_spec.csv`

**Top-level scripts:**
- `1_decision-analysis.R` — combines raw data into analysis dataset
- `2_summary-stats.R` — descriptive stats, propensity scores, IPW weights
- `3_paper-results.R` — tables and figures from estimation outputs

**Reduced-form (code/analysis/reduced-form/):** Runner `_reduced-form.R`.

**Structural (code/analysis/structural/):** Runner `_structural.R`. Full cleanup between stages (rm + gc).

| Script | Purpose | Key Outputs |
|--------|---------|-------------|
| `1_demand.R` | Builds cell CSVs (R), calls Julia estimator | `results/choice_coefficients_structural.csv` |
| `2_pricing.R` | Shares, elasticities, markups via Bertrand FOC | `results/supply_results.csv`, `data/output/foc_inputs/` |
| `3_cost_gmm.R` | GMM estimation of cost parameters (moments: risk scores, claims, FOC) | `data/output/ra_*_coefs_gmm.csv` |
| `4_counterfactuals.R` | Dispatches `cf_worker.R` subprocesses | `results/counterfactual_results.csv` |

### Key Architecture: plan_attrs

`build_supply_choice_data()` returns `list(cell_data, plan_attrs)`. The `plan_attrs` table has post-collapse plan names with all attributes (metal, issuer, AV, HMO, premium_posted, comm_pmpm). Downstream code reads from `plan_attrs` — no lookups against `plans_cell`.

### Julia (code/julia/)

Only demand estimation runs in Julia (1.12.5 via `+release`). Supply and counterfactuals run in R.

**Demand model design:**
- Pooled model: 36 parameters + λ. Spec defined in `_analysis.R`, written to `data/output/demand_spec.csv` for Julia/cf_worker.
- Posted premium (no subsidy deduction). Subsidy effects via FPL×premium interactions.
- Premium = 0 on uninsured row. `penalty_own` = penalty/12/hh_size separately identifies outside option.
- Base: premium, penalty_own, silver, bronze, hmo, hsa, cf_resid, 4 insurer FEs, 8 insurer×metal
- Demo×premium: hh_size, perc_{0to17,18to34,35to54,male,black,hispanic,asian,other}, FPL_{250to400,400plus}
- Assisted: assisted×{silver,bronze,gold,plat}, commission_broker, v_hat_commission
- V_0 = β'X_0 (NOT normalized to 0)
- Household-size weights (not IPW)

**Supply model design:**
- FOC includes RA derivative (`compute_ra_foc`): insurers internalize adverse selection in pricing
- Markup = solve(Omega, shares + ra_foc + Omega_broker * comm_vec)
- Cost-side GMM: joint estimation of risk score and claims regression parameters with FOC moments
- CFs currently use fixed mc_foc (no RA iteration) pending demand re-estimation

## Identification Strategy

**Broker density as IV for assistance.** Reduced-form uses IPW + CF residual. Structural uses `v_hat_commission` to correct commission coefficient only.

## Paper

Paper at `paper/paper.tex`. Co-authored with Evan Saltzman. Compile with XeLaTeX from project root.

## Next Steps

1. **Demand re-estimation** — current spec (36 params + λ) with posted premium, premium=0 on outside option. Last run reached λ≈0.56, β_premium≈-0.0006 before BSOD crash. Premium negative but small; λ and β_premium trading off. Need to evaluate converged results.
2. **After demand**: re-run pricing → GMM → CFs. Pipeline confirmed working.
3. **Paper results** — run `3_paper-results.R` once CF outputs exist
4. **Bootstrap SEs** — Hessian-based for demand, parametric bootstrap for supply/CF

## Last Session

Date: 2026-04-03

- **BSOD root cause identified**: WinDbg analysis of MEMORY.DMP confirms `csagent.sys` (CrowdStrike Falcon) is the faulting module, not Intel GPU driver. CrowdStrike's kernel-mode file filter likely crashes when intercepting Julia's heavy mmap I/O (114 cell_*.bin files, ~12 GB).
- **Proposed fix**: Remove `offload_to_mmap()` and hold all X matrices in regular Julia arrays. Machine has 64 GB RAM — 12 GB heap is fine. Eliminates the random-access file I/O pattern that triggers CrowdStrike. Not yet implemented.
- **Minidumps enabled**: Future crashes will write individual .dmp files to `C:\WINDOWS\Minidump`. WinDbg installed via Microsoft Store.
