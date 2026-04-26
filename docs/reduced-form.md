# Reduced-form specifications

The reduced-form pipeline (`code/analysis/reduced-form/`) estimates two
separate ATT objects on the new-enrollee CC sample:

1. **Dominated-choice ATT** — does assistance reduce the probability of picking
   a Pareto-dominated plan? (`1_dominated-choices.R`)
2. **Plan-attribute ATT** — conditional on enrolling, does assistance shift
   *which* plan an HH picks? (`2_choice-att.R`)

Both ATTs are conditional on enrollment. Synthetic off-year (uninsured) rows
do not enter either estimation — for these HH-years the treatment was not
delivered, so the assisted/unassisted contrast is undefined.

---

## 1. Dominated-choice ATT (`1_dominated-choices.R`)

Sample: CSR-eligible new enrollees (FPL ≤ 2.00), where `dominated_choice` is
non-NA. Two complementary identification strategies:

### 1a. Single-equation LPM (mod1, mod2, mod3)

One IPW-weighted linear-probability regression on the pooled sample (both
groups present in the data). Coefficient on `assisted` is the ATT.

```r
# mod1 — all enrollees, no CF
dominated_choice ~ assisted + X + new_enrollee | region + year + insurer

# mod2 — new enrollees, no CF
dominated_choice ~ assisted + X | region + year + insurer

# mod3 — new enrollees, with CF
dominated_choice ~ assisted + v_hat + X | region + year + insurer
```

`v_hat` (broker-density CF residual) is fine here because the sample is
pooled. v_hat ranges from -1 to +1 across the data, the coefficient is
identified from within-group variation, no extrapolation.

### 1b. Potential-outcomes ATT (`compute_att`)

Two-step:
1. Fit IPW-weighted logit of `dominated_choice ~ X | FE` on **unassisted only**.
2. Predict `dominated_choice` for **assisted** observations using that fit.
3. ATT = mean(observed assisted) − mean(predicted counterfactual).

`v_hat` is **deliberately omitted** from this spec. The reason is structural,
not a robustness choice:

- v_hat is the residual from the LPM first stage `assisted ~ broker_density + X`.
- Because `assisted` is binary (0/1) and the fitted probability lives in
  [0, 1], the residual is mechanically `-fitted ∈ [-1, 0]` for unassisted
  HHs and `1 - fitted ∈ [0, 1]` for assisted HHs.
- The two groups have **non-overlapping v_hat support by construction**.
- Fitting on unassisted (negative v_hat only) and predicting on assisted
  (positive v_hat only) is pure extrapolation. The binomial logit's link
  saturates that extrapolation to ~1 and forces ATT ≈ -0.97. A 2026-04-25
  diagnostic confirmed this is the mechanism.
- The IPW weighting in `compute_att` is already a selection-on-observables
  correction; the CF residual would be a second correction, and it can't
  do that work cleanly across non-overlapping support anyway.

So the rule is: **v_hat in pooled regressions (mod3), but not in
fit-then-predict counterfactuals (PO ATT, MNL choice-att if logit-binomial
were used)**.

---

## 2. Plan-attribute ATT (`2_choice-att.R`)

Sample: insured + unassisted enrollees (a chid is a (region, year, HH) tuple).
The outside good (uninsured) is dropped from the choice set entirely — see
"Why no outside good" below.

### Estimation

Plain MNL via `mlogit::mlogit`:

```r
choice ~ premium + plan dummies + insurer dummies +
         (demographics × premium) + (v_hat × insurer / metal)  | 0 | 0
```

Coefficients are estimated on insured + unassisted; counterfactual choice
probabilities are then computed for insured + assisted by hand
(`exp(Xβ) / Σ exp(Xβ)` per chid). ATT for plan attribute Y =
`mean_assisted(Y_observed) − mean_assisted(Y_predicted_if_unassisted)`.

### Why no outside good

Earlier versions had the synthetic off-year rows in the data and used a
nested logit (insured nest + uninsured outside good, governed by λ). With
the off-year synthesis approach, the synthetic uninsured rows inherit
`assisted = 0` from `5_merge-and-finalize.R` regardless of the HH's
treatment status in its actual CC-enrolled years. That contaminates the
unassisted sample with off-year rows from HHs that *were* assisted, drives
the unassisted-sample insured share toward zero, and pushes λ to
non-physical values (~2.6 in the run that exposed this).

The estimand we care about is "given enrollment, did assistance change the
plan?" — that is conditional on enrolling, doesn't need the outside good,
and is a natural fit for an MNL on enrollees. Whether assistance changes
the *enrollment decision* itself is a separate question that this
identification can't answer well, and isn't what the structural model asks
the reduced form to nail down.

### Why v_hat is fine in MNL

Same support gap as the PO ATT in §1b, but the mechanics differ:

- The binomial-logit link in §1b saturates near 1 on extrapolation, which
  drives a binary-outcome prediction off a cliff.
- The MNL softmax produces probabilities that move around but stay
  bounded; extrapolation biases coefficients somewhat but doesn't produce
  the same numerical pathology. CF in MNL is the standard approach for
  selection on unobservables in discrete choice (Petrin & Train 2010).

So the cf_* terms (v_hat × insurer, v_hat × metal) stay in the spec.

---

## Selection corrections at a glance

| Source of selection           | Correction                          |
|-------------------------------|-------------------------------------|
| Selection-on-observables      | IPW (propensity score in `2_ipw.R`) |
| Selection-on-unobservables    | CF residual `v_hat` from broker-density first stage |

The IPW propensity score uses **only confounders** — no broker density
(Bhattacharya & Vogt 2007). Broker density enters the pipeline only as the
IV in the CF first stage, never in the propensity score.

Where each correction is used:

| Spec                            | IPW | v_hat |
|---------------------------------|-----|-------|
| mod1, mod2 (LPM, pooled)        | yes | no    |
| mod3 (LPM, pooled, CF)          | yes | yes   |
| PO ATT logit (`compute_att`)    | yes | no    |
| MNL plan-choice ATT             | no (uses population weights) | yes (cf_* interactions) |

---

## Things that exist for historical / structural reasons

- `predict_nested_logit` in `helpers/choice.R` is no longer used by the
  reduced-form. The structural side still uses it.
- `estimate_demand` (BFGS-BHHH nested logit estimator) is the structural
  estimator. The reduced-form goes through `mlogit` directly.
- The cell-CSV format produced by `build_choice_data` is shared between
  RF and structural. RF reads back the cells in `2_choice-att.R` Phase 2
  and pools them into a single mlogit fit; structural reads them per cell
  for cell-by-cell BFGS-BHHH.
