# Current Approach (post plan-id refactor)

Snapshot of the working tree as of 2026-04-22, the day after the plan-id
refactor and the workers-only ESI / silver-grepl / `eff_cheapest` fixes.

This document mirrors `docs/pre-refactor-approach.md` so the two can be diffed
section by section.

## 1. Pipeline overview

Two layers:

1. **Data build** — `code/data-build/` (orchestrator `_data-build.R`).
   Seven numbered scripts produce CSV outputs in `data/output/`.
2. **Analysis** — `code/analysis/` (orchestrator `_analysis.R`).
   Loads helpers once, builds analysis datasets, then runs reduced-form,
   structural, and paper-results pipelines.

### Data build (`code/data-build/`)

Reference files loaded once in `_data-build.R:22-44`:
`plan_data.csv`, `product_definitions.csv`, `zip3_choices.csv`,
`rating_areas.csv`, `age_rating_factors.csv`, `poverty_guidelines.csv`
(also pivoted to `poverty_guidelines_long`), `contribution_percentages.csv`.
`plan_data$Issuer_Name` is standardized via `_helpers.R:standardize_insurer`.

| Step | Script | Reads | Writes | Purpose |
|------|--------|-------|--------|---------|
| 1 | `1_clean-enrollment.R` | `pra_07192019.csv` + reference tables | `enrollment_individual.csv` | Clean CC individual rows; CSR-aware metal fill from plan_name; OEP cutoff; mid-year dedup; plan validity check; FPL bracket repair; HN HSP overrides |
| 2 | `2_aggregate-to-hh.R` | `enrollment_individual.csv` | `enrollment_hh.csv` (overwrites individual) | Attach canonical `plan_id` from `plan_data$Plan_Name2`; HH grouping (split mixed cases); SLC benchmark per `(zip3, region, year)`; chunked DT aggregation to HH-year |
| 3 | `3_process-sipp.R` | SIPP fixed-width archives | `sipp_immigration_logit.rds`, `sipp_emp_offer_logit.rds` | Two SIPP logits (immigration + ESI offer). ESI now workers-only fit (`3_process-sipp.R:164`) |
| 4 | `4_build-acs.R` | ACS extracts + SIPP logits | `acs_individuals.csv`, `acs_households.csv`, `income_model_coefs.csv`, `income_distribution.csv` | Apply SIPP logits to ACS; two-flag uninsured filter (`ACS_unins_flag` ∩ `SIPP_unins_flag`); compute formula APTC; fit FPL imputation OLS (>400% sample) |
| 5 | `5_merge-and-finalize.R` | `enrollment_hh.csv` + `acs_households.csv` (and individuals) | `demand_households.csv`, `demand_individuals.csv` | Bind CC + ACS; rank-match income for CC ≥400% with missing FPL; compute cheapest bronze; mandate penalty |
| 6 | `6_broker-density.R` | `RatingRegionAgentEnrollment_*.xlsx` | `broker_density.csv` | Region-year broker counts + HHI |
| 7 | `7_rate-filings.R` | `2014-2020.RData` (CMS PUFs) | `rate_filing_rsdata.csv` | Plan-year claims/RA panel; risk score derivation; reinsurance factor |

Old scripts preserved in `code/data-build/_archive/`.

### Analysis flow (`code/analysis/_analysis.R`)

1. Set `TEMP_DIR`, `SAMPLE_FRAC=0.02`, `MASTER_SEED=20260224`; export via
   `Sys.setenv()`.
2. Source `code/0-setup.R` (renv + packages).
3. Source helpers in fixed order (`_analysis.R:24-32`):
   `data-build/_helpers.R`, `helpers/constants.R`, `helpers/covariates.R`,
   `helpers/choice.R`, `helpers/supply.R`, `helpers/ra.R`,
   `helpers/estimate_demand.R`, `helpers/cf_worker.R`, `S2-demand-specs.R`.
4. Build analysis data (`1_decision-analysis.R`, `2_ipw.R`, `3_summary-stats.R`).
5. Free big objects, run `reduced-form/_reduced-form.R` then
   `structural/_structural.R`, then `4_paper-results.R`.

`S2-demand-specs.R` is a standalone driver that can be sourced from a cold
session — it activates renv, regenerates `hh_full.csv` from
`1_decision-analysis.R` if missing, and runs the spec sweep.

## 2. Canonical schema (post plan-id refactor)

Single plan identifier across the whole pipeline:

- **`plan_id`** — short code derived from `plan_data$Plan_Name2`
  (e.g. `"ANT_SIL87"`, `"BS_G3"`, `"KA_BR"`). Reserved values introduced only
  inside the demand cell builder:
  - `"Uninsured"` — the outside-option row added in
    `helpers/choice.R:65-72` and `helpers/supply.R:55-62`.
  - `"Small_<base_metal>"` — the collapsed small-insurer row written in
    `helpers/choice.R:206-213` (`Small_BR/SIL/G/P/CAT`).
- **`metal`** — CSR-aware. Possible values:
  `"Bronze"`, `"Silver"`, `"Silver - Enhanced 73/87/94"`, `"Gold"`,
  `"Platinum"`, `"Minimum Coverage"`, or `NA` for the outside option.
  Where the *base* tier is needed (small-insurer collapse, AV lookups for
  collapsed plans), it is derived inline:
  `sub(" - Enhanced.*", "", metal)` (`helpers/choice.R:184`,
  `helpers/supply.R:154`).
- **`network_type`** — renamed from `plan_network_type` in
  `1_clean-enrollment.R:197`. Used by `helpers/choice.R:249` to set the
  `hmo` indicator.
- **`insurer`** — standardized via `_helpers.R:INSURER_MAP`.

Dropped throughout the pipeline (do not reintroduce):

- `hios_id_16`, `hios_id_14` — dropped at `1_clean-enrollment.R:211`.
- `HIOS` — dropped from individuals at `2_aggregate-to-hh.R:271`.
- `plan_name` — dropped at `2_aggregate-to-hh.R:40` after plan_id crosswalk.
- `plan_unique_id`, `plan_id_HIOS`, `plan_number_nocsr`,
  `previous_plan_number`, `metal_level_enhanced` — never reach later steps.

"Insured" detection downstream uses `!is.na(plan_id)` (data-build)
or `insured == 1L` (analysis HH dataset, set in
`5_merge-and-finalize.R:83-84`). Never use `!is.na(plan_number_nocsr)`.

## 3. Outside option

- **HH-level**: an HH is uninsured if it came from the ACS pool. Set in
  `5_merge-and-finalize.R:83-84` — CC HHs receive `insured = 1L` and ACS
  HHs receive `insured = 0L` plus `plan_id = NA`, `metal = NA`,
  `insurer = NA`, `network_type = NA`, and
  `agent = broker = navigator = 0L` (today's fix to the previous bug
  where these were left NA and the channel field went missing).
- **Plan-row level inside the cell**: `helpers/choice.R:65-72` and
  `helpers/supply.R:55-62` add a single Uninsured row per cell with
  `plan_id = "Uninsured"`, `issuer = "Outside_Option"`, `network_type = NA`,
  `metal = NA`, `premium = NA`, `msp = NA`, `hsa = NA`, `cf_resid = 0`
  (and `comm_pmpm = 0` if commissions are present). The `Uninsured` row
  receives `premium_oop` overrides per `premium_type` (see §4).
- **Choice indicator on the Uninsured row**: in `helpers/choice.R:239-243`,
  `plan_choice == 1` if and only if the HH had `is.na(hh_plan_id)` and
  `insured == 0L` and the row is the Uninsured row. The HH-level `insured`
  column is set as `max(plan_choice)` over the HH (`helpers/choice.R:128`).
- **Attribute indicators on the Uninsured row** are 0 by construction:
  `silver/bronze/gold/platinum`, `Anthem/...`, `hmo`, `hsa`, `av` are all
  set from `metal`, `issuer`, and `network_type` which are NA / "Outside_Option".
  See `helpers/choice.R:159-170` (AV lookup gives the Outside row `av = 0`).
- **`uninsured_plan` indicator**: set as `plan_id == "Uninsured"`
  (`helpers/choice.R:253`).
- **Insured intercept**: `insured_intercept = 1` for any non-Uninsured row,
  0 otherwise (`helpers/choice.R:281-282`).

## 4. Premium types

`build_choice_data(premium_type = ...)` controls four variants of `premium_oop`
(`helpers/choice.R:138-158`). All are computed at the HH level
(after multiplying the age-21-base posted premium by the HH's
`rating_factor / RATING_FACTOR_AGE40 = rating_factor / 1.278`).

Define `premium_hh = (premium / RATING_FACTOR_AGE40) * rating_factor`
and `adj_subsidy = coalesce(subsidy, 0)`. Then:

| `premium_type` | Outside row | Inside row |
|----------------|------------|-----------|
| `"posted"`     | 0          | `premium_hh` |
| `"oop"`        | 0          | `pmax(premium_hh - adj_subsidy, 0)` |
| `"net"`        | 0          | `pmax(premium_hh - adj_subsidy, 0) - penalty / 12` |
| `"evan"`       | `penalty / 12` | `pmax(premium_hh - adj_subsidy, 0)` |

`"net"` and `"evan"` differ only by an HH-constant shift (`penalty/12`) that
is added to the inside premium versus to the outside premium; in nested logit
this is algebraically identical (same `λ`, `β`, negLL). Currently the
structural and S2 pipelines pass `premium_type = "net"`
(`structural/1_demand.R:50`; the S2 specs hard-code `"net"`).

`build_supply_choice_data` only uses the posted premium — it does not take a
`premium_type` argument (`helpers/supply.R:122-125`).

After `premium_oop` is set, `helpers/choice.R:248` divides by `hh_size`
to produce the per-member `net_premium = premium / hh_size` used in the
demand model. Demographic × premium interactions (`hh_size_prem` etc.) are
built from this per-member premium (`helpers/choice.R:266-278`).

## 5. Subsidy

- **CC HHs** (`source = "CC"`, step 2 output): `subsidy` is computed as the
  observed gross-minus-net difference per HH:
  `subsidy = pmax(0, gross_premium_amt_int - net_premium_amt_int)`
  (`2_aggregate-to-hh.R:255`). `SLC_contribution` is then
  `pmax(0, premiumSLC - subsidy)` (`2_aggregate-to-hh.R:256`).
- **ACS HHs** (`source = "ACS"`, step 4 output): formula APTC. Bracket-specific
  contribution percentages from `contribution_percentages.csv` are
  interpolated by `_helpers.R:aca_contribution()` (FPL in *ratio* form,
  e.g. 1.5 for 150%). Then
  `subsidy = pmax(0, premiumSLC - SLC_contribution)` for HHs with
  `subsidy_eligible = 1` (`4_build-acs.R:336-340`). Eligibility =
  `1.38 ≤ FPL ≤ 4.0` (`4_build-acs.R:285`).
- **After step 5's bind**, both sources have `subsidy` set on the HH row;
  ACS HHs also have `plan_id = NA` (so they are uninsured for the choice
  model). Today's `eff_cheapest` fix (`5_merge-and-finalize.R:172`)
  restored symmetric handling: previously
  `eff_cheapest` was conditional on `subsidized_members > 0`, which is an
  enrollment field (NA for ACS), causing the affordability-based mandate
  exemption to break for ACS rows. New definition:
  `eff_cheapest = pmax(0, cheapest_premium - coalesce(subsidy, 0))`,
  which works for both CC and ACS.
- **`subsidy_eligible` in analysis** (`1_decision-analysis.R:37`) is now
  `coalesce(subsidy, 0) > 0`. The previous version was based on
  `subsidized_members > 0`, which is NA for ACS rows. With the fix, ACS HHs
  with positive formula subsidy now correctly receive `subsidy_eligible = 1`,
  and `csr_eligible = subsidy_eligible & FPL ≤ 2.50` follows.

## 6. Penalty

Computed in `5_merge-and-finalize.R:128-191` for **all** HHs (CC and ACS). The
IRS individual-mandate formula:

1. Determine `n_adults` from `demand_individuals` (>=18) and `n_children`
   from the residual.
2. `tax_unit_type` = `"single"` if `household_size == 1`, `"married"` if
   `n_adults >= 2`, else `"household_head"`.
3. `filing_threshold` from year × tax_unit_type lookup (`5_merge-and-finalize.R:139-148`).
4. `eff_cheapest = pmax(0, cheapest_premium - coalesce(subsidy, 0))` (today's fix).
5. **Exemption**: `(FPL * poverty_threshold < filing_threshold) |
    (eff_cheapest * 12 > afford_pct[year] * FPL * poverty_threshold)`.
   `afford_pct` from a year-specific lookup
   (`5_merge-and-finalize.R:150`). NB the constants in this lookup differ
   from `helpers/constants.R:AFFORD_THRESHOLDS` (used downstream as the
   catastrophic-eligibility cutoff); both intentional.
6. Otherwise: `pmin(adult+child cap × annual cap_pmpm,
                     pmax(per-person flat, percent of taxable income))`
   floored at 0.

`penalty` is then the dollar penalty per HH per year. The choice model
uses `penalty / 12` (per-month).

## 7. ACS uninsured filter (two flags)

`4_build-acs.R:160-182` builds two parallel flags and keeps the **intersection**:

- `ACS_unins_flag` = uninsured in ACS variables AND citizen
  (`uninsured == 1 & CITIZEN %in% c(0, 1, 2)`).
- `SIPP_unins_flag` = uninsured AND not predicted-undocumented
  AND no household ESI access
  (`uninsured == 1 & undocumented == 0 & hh_access_ESI == 0`).

`hh_access_ESI` is the HH-level max of any individual's `access_ESI`,
where `access_ESI = has_ESI | imputed_ESI`. After today's fixes:

- The ESI logit in step 3 is fit on workers only
  (`3_process-sipp.R:160-165`, `filter(employed == 1)`).
- ACS prediction in step 4 (`4_build-acs.R:131-142`) computes `pred_ESI`
  for `employed == 1` only, then imputes via Bernoulli draw:
  `imputed_ESI = as.integer(pred_ESI - runif() > 0)`. Non-workers stay at
  `imputed_ESI = 0` (they cannot be offered ESI). Previously the code
  applied a 0.5 threshold to all individuals, which over-imputed ESI
  among non-workers and shrank the uninsured pool unrealistically.

The intersection-kept count is printed at
`4_build-acs.R:170-180`.

## 8. Weighting

- **Inside the cell** (`helpers/choice.R:262`): `hh_weight = as.numeric(hh_size)`
  always. There is no `weight_var` parameter — the IPW-based ATT weighting
  used previously has been removed from the structural and S2 paths.
  `helpers/supply.R:235` does the same.
- **Globally**: `helpers/estimate_demand.R:154-168` (`normalize_weights`)
  scales every cell's weight vector by the global mean across all loaded
  cells, so the post-normalization mean weight is 1. This matches mlogit's
  behaviour (see `optimizer.md`).
- The reduced-form pipeline still has its own IPW step (`2_ipw.R`), which
  feeds `hh_ins_ps` for non-structural analyses; that path is untouched
  by the refactor.

## 9. Sampling

`SAMPLE_FRAC` (env-var, default 0.02) is applied **per channel**, before the
HH × choice-set cross-join:

```r
untreated_ids <- hhs_dt[channel == "Unassisted", unique(household_id)]
treated_ids   <- hhs_dt[channel != "Unassisted", unique(household_id)]
n_untreated <- max(1L, as.integer(length(untreated_ids) * sample_frac))
n_treated   <- max(1L, as.integer(length(treated_ids)   * sample_frac))
keep_ids    <- c(sample(untreated_ids, n_untreated, replace = FALSE),
                 sample(treated_ids,   n_treated,   replace = FALSE))
hhs_dt <- hhs_dt[household_id %in% keep_ids]
```

(`helpers/choice.R:32-42`, identical at `helpers/supply.R:21-31`.)

If the resulting sample has fewer than 50 unique HHs the cell is dropped
(`helpers/choice.R:44`). Note: `channel != "Unassisted"` includes both
broker/agent-assisted HHs and navigator HHs as "treated".

Per-cell seeds are drawn once from `MASTER_SEED` in `_structural.R:188-189`
and `S2-demand-specs.R:269-270`.

## 10. CSR-enhanced silver handling

The pipeline carries the CSR-enhanced silver labels intact through cell
construction so that an HH only sees the silver variant they're eligible
for, then collapses the short codes only after attribute indicators are set:

1. Step 1 fills missing metal from `plan_name` regexes that include the
   `Silver - Enhanced 73/87/94` cases (`1_clean-enrollment.R:43-46`).
2. Step 2 carries the enhanced label through the HH-level row.
3. In `helpers/choice.R:90-103`, a CSR filter keeps only the right silver
   variant per HH:
   - `csr_94` for `FPL ≤ 1.5 & subsidized_members > 0`
   - `csr_87` for `1.5 < FPL ≤ 2 & subsidized_members > 0`
   - `csr_73` for `2 < FPL ≤ 2.5 & subsidized_members > 0`
   - All other HHs keep base `"Silver"`; non-silver and Uninsured rows
     pass through (`!grepl("^Silver", metal) | is.na(metal)`).
   The `^Silver` grepl is today's fix — previously this was
   `metal != "Silver"`, which incorrectly retained enhanced silver rows
   for the wrong HHs because `"Silver - Enhanced 87" != "Silver"` is `TRUE`.
4. AV is then assigned by exact metal match (`helpers/choice.R:159-170`).
5. **After AV is set**, the short codes are collapsed via
   `dt[, plan_id := gsub("SIL(94|73|87)", "SIL", plan_id)]`
   (`helpers/choice.R:304`, `helpers/supply.R:270`). The demand model
   treats the CSR variants as one alternative.
6. The silver indicator used in interactions is built post-collapse via
   `silver = grepl("^Silver", metal)` (`helpers/choice.R:256`,
   `helpers/supply.R:229`) — today's fix; previously
   `metal == "Silver"` excluded the still-labelled
   `"Silver - Enhanced 73/87/94"` rows from the silver indicator on the
   non-collapsed plans.

`structural/_structural.R:90-105` and `S2-demand-specs.R:106-122` apply the
same `gsub` collapse when computing plan-level demographics
(so SIL73/87/94 demographics roll into SIL).

## 11. Small insurer collapse

`helpers/choice.R:175-217` (and analogously `helpers/supply.R:144-196`):

1. Split the cell rows into `large` (Anthem, Blue_Shield, Kaiser, Health_Net,
   Outside_Option) and `small_raw` (everyone else).
2. `small_raw[, base_metal := sub(" - Enhanced.*", "", metal)]` — collapse
   any CSR-enhanced silvers in the small group to base "Silver".
3. Aggregate `small_raw` by `(household_id, base_metal)` taking
   `min(premium_oop)`, `max(plan_choice)`, mean attributes.
4. Set `issuer = "Small_Insurer"`, `metal = base_metal`, and
   `plan_id = "Small_<P/G/SIL/BR/CAT>"` per base_metal.
5. `rbind` large + small with `fill = TRUE` for missing columns.

The `gsub("SIL(94|73|87)", "SIL", plan_id)` on line 304 then collapses
the large-insurer enhanced-silver short codes to a single `SIL`.

`build_supply_choice_data` does the same collapse but additionally retains
`rating_factor`, `adj_subsidy`, `subsidized`, `premium_hh`, and
`premium_posted` per HH × base_metal (used for the supply-side benchmark
elasticity split).

The plan attribute table `plan_attrs` (returned alongside `cell_data`)
is built post-collapse from the supply cell data
(`helpers/supply.R:273-288`); base-metal AV is taken via `min(av)` (so
collapsed Small_SIL gets `av = 0.70`, the base Silver AV).

## 12. S2 demand-specs

`code/analysis/S2-demand-specs.R` — standalone driver, currently three specs.

**Building blocks** (`S2-demand-specs.R:205-223`):

- `PLAN_ATTRS = c("silver", "bronze", "hmo", "hsa")`
- `INSURER_FE = c("Anthem", "Blue_Shield", "Kaiser", "Health_Net")`
- `ASST_TERMS = c("assisted_silver", "assisted_bronze",
                  "commission_broker", "v_hat_commission")`
- `DEMO_PREM` = 11 demographic × premium interactions (`hh_size_prem`,
  `perc_*_prem`, `FPL_*_prem`).
- `INSURER_MARKET_FE` = 24 dummies of the form `ix_<INS>_ra<REGION>` for
  insurer ∈ {Ant, BS, Kai, HN, Sm} × region ∈ {1, 4, 8, 13, 16},
  with Anthem × region 1 as the reference cell.

**Specs** (`S2-demand-specs.R:225-243`):

| Spec | base | asst | premium_type |
|------|------|------|--------------|
| `A1_net_exch` | `premium`, `PLAN_ATTRS`, `INSURER_FE` | `ASST_TERMS` | `"net"` |
| `A2_net_exch_dprem` | A1 + `DEMO_PREM` | `ASST_TERMS` | `"net"` |
| `L2_net_exch_imxFE_dprem` | `premium`, `PLAN_ATTRS`, `INSURER_MARKET_FE`, `DEMO_PREM` | `ASST_TERMS` | `"net"` |

**Targeted cells**: regions {1, 4, 8, 13, 16} × years {2014, 2016, 2018}
(`S2-demand-specs.R:193-196`), 15 cells max per spec.

**Workflow per spec** (`S2-demand-specs.R:281-427`):

1. Skip if `coefs_<spec>.csv` and `<spec>_summary.txt` already exist.
2. For each `(premium_type, outside_option)` combination, build the cell
   CSVs once into `SENS_DIR/cells_<combo_key>/`. The `INSURER_MARKET_FE`
   dummies are constructed inside this loop (`S2-demand-specs.R:336-353`):
   the cell row's insurer is mapped to a short code, and the dummy fires
   only if the insurer matches and the cell's region matches the FE's
   region.
3. Load all cells (`load_all_cells`) — restricted to the spec's covariates.
4. Normalize weights (mean 1).
5. Initialize `theta0 = c(rep(0, K), 1.0)` — optionally warm-start from a
   previous spec's CSV if `sp$warm_start` is set (no spec currently uses it).
6. Run `bfgs_bhhh(theta0, demand_cells, max_iter = 500, print_every = 100)`.
7. Write coefficients to `SENS_DIR/coefs_<spec>.csv` and a one-line summary
   (`b_prem`, `lambda`, `negLL`) to `_summary.txt`.

## 13. Estimation (BFGS-BHHH)

`helpers/estimate_demand.R` is a pure-R nested-logit estimator. Key choices
are documented in `code/analysis/structural/optimizer.md`:

- **`V_0 = β'X_0`, NOT zero** — uninsured rows are kept in the X matrix and
  enter both the likelihood and the gradient. Setting `V_0 = 0` produced a
  ~3400-log-unit likelihood mismatch versus mlogit at the same parameters.
- **Weight normalization** is global (`normalize_weights`,
  `helpers/estimate_demand.R:154-168`), not per-cell.
- **BFGS with BHHH initialization** is the only optimizer that works.
  `optim()`'s L-BFGS-B, BFGS, and `nlminb` all fail (either λ drifts to
  bounds or convergence stalls). `bfgs_bhhh` (`helpers/estimate_demand.R:354-443`)
  initializes the inverse Hessian as `solve(crossprod(per-HH gradi))`,
  uses descent direction `d = -Hm1 %*% g` with halving line search,
  bounds λ to (0.001, 5.0), and converges when `|negLL_new - negLL_old| < ftol`
  or `chi2 = -d'g < 1e-6`.
- **NA → 0 in covariate matrix**: `helpers/estimate_demand.R:55-62` fills
  any `NA` values in X columns with 0 before the matrix product. Missing
  spec covariates are silently zeroed (intersected with the file header
  in `load_one_cell`, `helpers/estimate_demand.R:38-40`).
- **Starting values**: `theta0 = c(rep(0, K), 1.0)` from
  `_structural.R`/`S2-demand-specs.R`. The structural pipeline always
  starts from zeros + λ=1 (no warm start).

The cell loader (`load_one_cell`) requires every HH to have at least one
insured row, the Uninsured row, and a chosen row; HHs without all three
are dropped (`helpers/estimate_demand.R:87-90`).

`accumulate(theta, cells, compute_grad = TRUE)` sums negLL and gradient
across cells; never builds a pooled matrix.

## 14. Counterfactuals

`helpers/cf_worker.R::run_cf_cell()` runs all counterfactual scenarios for
one region-year cell **in-process** (no subprocess). Called by
`structural/4_counterfactuals.R:73-81` inside a `tryCatch` over each
`(region, year)`.

Per-cell flow:

1. Filter `supply_results` and `plan_choice` to the cell; bail if empty.
2. Attach commissions to the plan rows from `commission_lookup`
   (`cf_worker.R:49-55`).
3. Call `build_supply_choice_data(plans_cell, hhs_raw, sample_frac,
   spec = STRUCTURAL_SPEC)` to produce `cell_data` and `plan_attrs`.
4. Read all plan attributes from `plan_attrs` (no lookups against
   `plans_cell`); identify benchmark plan as second-cheapest base Silver
   by posted premium.
5. Build the FOC function + analytical Jacobian via `build_foc_function`
   (`cf_worker.R:134-466`):
   - `fn(p)` — endogenous-MC FOC residual:
     `s + ra_foc - Omega %*% (p - mc) + Omega_broker %*% comm`.
     MC is recomputed at each price via demographics → risk score → claims
     → RA → MC (Saltzman RAND JE 2021).
   - `jac(p)` — analytical Jacobian with terms T1+T2 (E - Ω), T3 (∂mc/∂p
     including ∂RA/∂p analytically), T4 (elasticity curvature, full
     subsidized vs unsubsidized split for the benchmark column), and T6
     (cheap finite-difference of `compute_ra_foc` perturbed by analytical
     ∂shares/∂p and ∂rs/∂p). T5 (broker-elasticity curvature) is omitted.
6. `solve_equilibrium` calls `nleqslv::nleqslv(method = "Newton")` with
   `xtol = 1e-6`, `ftol = 1e-8`, `maxit = 100`. Solutions with
   `termcd > 2` and `|f| < 0.05` are accepted.
7. Three scenario types per cell (`cf_worker.R:631-743`):
   - `"observed"` — observed commissions, warm-start from `p_obs`.
   - `"zero_tau<rate>"` for `tau ∈ {0, 0.25, 0.5, 0.75, 1.0}` — zero
     commissions; `tau` controls the share of any-agent HHs that switch
     to navigator-assisted (the rest become unassisted). Warm-started
     from previous scenario's `p`.
   - `"uniform"` — every plan gets the same `mean_comm_pmpm`.
8. Consumer surplus per scenario via `compute_consumer_surplus`
   (closed-form nested-logit log-sum scaled by per-HH `1/alpha_i`).

Outputs aggregated to `results/counterfactual_results.csv`. Tau-gradient
welfare summary written by `4_counterfactuals.R:147-193`.

## 15. Cost-side GMM (`structural/3_cost_gmm.R`)

Two-step feasible GMM for `(α, γ)` — the risk-score and claims regression
parameters — using three blocks of moments:

- **M1** — risk-score moments from rate filings (intercept + Silver, Gold,
  Platinum, share_18to34, share_35to54, share_hispanic), weighted by
  `sqrt(EXP_MM)`.
- **M2** — claims moments from rate filings (intercept + HMO, trend, Anthem,
  Blue_Shield, Health_Net), weighted by `sqrt(EXP_MM)`.
- **M3** — pricing FOC moments evaluated **directly** (not inverted):
  `s + ra_foc - Ω(p - MC(α, γ)) + Ω_broker · comm` averaged across
  plan-cell observations, instrumented by the same plan characteristics.

Step 1 uses identity weighting; step 2 uses inverse-block-norm weighting
of M1, M2, M3. Outputs: `ra_rs_coefs_gmm.csv` and `ra_claims_coefs_gmm.csv`
in `TEMP_DIR`. Counterfactuals consume the GMM versions
(`4_counterfactuals.R:29-30`).

## 16. Pricing (`structural/2_pricing.R`)

Loops over cells, builds supply choice data with same seed as demand,
computes shares + elasticities + ownership matrix + RA derivative, then
recovers markups from
`shares + ra_foc + Ω_broker · comm = Ω · markup`. RA regressions are
fit via `helpers/ra.R::estimate_ra_regressions` from
`rate_filing_rsdata.csv` joined to `plan_demographics.csv`. FOC inputs
(Omega, shares, plan_avs, etc.) are saved per cell as
`TEMP_DIR/foc_inputs/foc_<r>_<y>.rds` for the GMM step.

`helpers/ra.R::compute_ra_foc` (line 347) computes the ∂RA/∂p contribution
to the FOC. The function is exact under the quotient rule
(`dRA_k/ds_m = (-rs_k * rs_m / S_rs^2 + util_k * util_m / S_u^2) * avg_p`)
then chain-ruled to `dRA_k/dp_l = dRA_ds %*% elast_mat` and aggregated as
`ra_foc_l = Σ_k O[l,k] * s_k * dRA_dp[k,l]`.

## 17. Helpers reference

| File | Key exports |
|------|-------------|
| `data-build/_helpers.R` | `RATING_FACTOR_AGE40 = 1.278`, `INSURER_MAP`, `standardize_insurer`, `aca_contribution` (FPL ratio form), `FPL_BRACKETS`, `assign_bracket` (uses `findInterval`), `read_fwf_sas` |
| `helpers/constants.R` | `AFFORD_THRESHOLDS` (year → cutoff for catastrophic eligibility) |
| `helpers/covariates.R` | `get_covariate_menu()`, `write/read_demand_spec`, `get_prem_interactions`, `recompute_prem_interactions` |
| `helpers/choice.R` | `build_choice_data` (returns one cell tibble), `predict_nested_logit` |
| `helpers/supply.R` | `build_supply_choice_data` (returns `list(cell_data, plan_attrs)`), `compute_utility`, `compute_alpha_i`, `compute_shares_and_elasticities`, `build_ownership_matrix`, `compute_broker_shares_and_elasticities` |
| `helpers/ra.R` | `estimate_ra_regressions`, `compute_demographic_shares`, `predict_risk_scores`, `compute_ra_transfers`, `predict_claims`, `predict_mc_structural`, `compute_mc`, `compute_ra_foc` |
| `helpers/estimate_demand.R` | `load_one_cell`, `load_all_cells`, `normalize_weights`, `cell_negll_grad`, `accumulate`, `bfgs_bhhh`, `estimate_demand` |
| `helpers/cf_worker.R` | `run_cf_cell` (in-process counterfactual driver) |

## 18. Known issues / open

1. **λ > 1 at SAMPLE_FRAC = 0.02** — yesterday's S2 run produced
   λ ∈ (1.45, 2.10) across A1/A2/L2 with β_premium ∈ (-0.85, -0.71) per $100
   on ~7,270 HHs. Likely small-sample. Today's silver-grepl, ESI workers-only,
   `eff_cheapest`, and `subsidy_eligible` fixes should expand the ACS
   uninsured pool and improve identification — to be verified after a fresh
   data-build + S2 run.
2. **Silver indicator fix (`grepl("^Silver", metal)`)** in `helpers/choice.R:256`
   and `helpers/supply.R:229`: previously `metal == "Silver"`. The CSR
   labels (e.g. `"Silver - Enhanced 87"`) are present on the cell rows
   *before* the post-AV `gsub("SIL(94|73|87)", "SIL", plan_id)` collapse.
   The `metal` column is *never* collapsed back to base Silver in the cell;
   only `plan_id` short codes are. So `metal == "Silver"` was missing all
   CSR-enhanced silver rows when constructing the silver indicator and
   the `Anthem_silver`-style interactions.
3. **`subsidized_members` was the wrong driver** for `subsidy_eligible`
   in analysis (an enrollment field, NA for ACS rows). Now uses the
   computed `subsidy` value directly. Note that the CSR filter inside
   `helpers/choice.R:91-93` still uses `subsidized_members`, which is
   correct for CC HHs but skips CSR for ACS HHs (whose subsidized_members
   is NA). For ACS HHs the `Uninsured` row is the chosen alternative
   anyway, so CSR plan rows being filtered out via the `subsidized_members > 0`
   gate is essentially harmless — but worth verifying once the rebuilt
   sample lands.
4. **`renv` 1.2.1 startup race** (`file.exists(path) : invalid 'file' argument`)
   — partially mitigated by `RENV_PROJECT` in `.Renviron`. Sometimes
   re-running once works.
5. **data.table grouped aggregation triggers locked-binding errors** under
   load. Step 2 mitigates by chunking by year, single-threading
   (`setDTthreads(1)`), and inlining the `dt[, .(...), by = ...]` at top
   level (no function wrapper). See `2_aggregate-to-hh.R:194-251`.
6. **VS Code R extension**: keep `r.sessionWatcher = false`. Long
   estimation loops can crash the R terminal; `S2-demand-specs.R` writes
   a per-step checkpoint log to `SENS_DIR/checkpoint.log` so progress
   survives a crash.
7. **`graphics.off()` before ggplot** is required in `2_pricing.R:359`
   after the heavy elasticity loop (otherwise ggplot's `valid.viewport`
   errors).

## 19. Pipeline parameter summary

Set in `_analysis.R:9-17` and propagated via env vars:

| Variable | Value | Set in |
|----------|-------|--------|
| `TEMP_DIR` | `D:/temp-research-data/health-insurance-decision-support` | `_analysis.R:10` |
| `SAMPLE_FRAC` | `0.02` (current; `0.20` for final runs) | `_analysis.R:11` |
| `MASTER_SEED` | `20260224` | `_analysis.R:12` |
| `STRUCTURAL_SPEC` | 20-term spec defined in `_structural.R:13-20` | `_structural.R` |
| `STRUCTURAL_ASST` | 4-term assisted block (`_structural.R:22-25`) | `_structural.R` |
| Tau grid for counterfactuals | `c(0, 0.25, 0.5, 0.75, 1.0)` | `cf_worker.R:23` |

Outputs land in `results/`:
- `choice_coefficients_structural.csv`
- `supply_results.csv`
- `counterfactual_results.csv`
- `figures/*.png`

Sensitivity outputs land in `D:/temp-research-data/.../demand_sensitivity/`:
- `coefs_<spec>.csv`, `<spec>_summary.txt`
- `sensitivity_summary.csv`
- `checkpoint.log`
