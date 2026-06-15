# Endogenizing the Premium Subsidy (APTC) in the Pricing Equilibrium

Two parts. Task 1 documents how Saltzman (RAND JE 2021) handles the price-linked
subsidy in his pricing and counterfactual model. Task 2 is a concrete,
file-specific plan to endogenize the subsidy in our supply-side code.

Source PDFs are local under `background/`:
- `The RAND J of Economics - 2021 - Saltzman - Managing adverse selection  underinsurance versus underenrollment.pdf` (main paper)
- `rand12372-sup-0001-onlineappendix.pdf` (web appendices A-D)

---

## Task 1 — How Saltzman handles the subsidy

### 1.1 The subsidy is endogenous to posted prices (the headline answer)

In the structural pricing model the APTC subsidy **moves with the benchmark
premium**. It is not held fixed at baseline. When an insurer changes a premium
and the second-lowest-cost-silver (SLCSP) benchmark moves, the subsidy moves
with it for every subsidized household.

The household premium is RAND Eq. (8):

> p_ijt(p) = max{ σ_it·p_jmt − max{ σ_it·p_bmt − ζ_it, 0 }, 0 }

where σ_it is the rating factor, p_jmt is plan j's **base (posted) premium**,
p_bmt is the **base premium of the benchmark plan** (the second-cheapest silver,
which "varies between consumers because of heterogeneous firm entry"), and ζ_it
is the household's income contribution cap. The subsidy is the middle term,
`max{σ_it·p_bmt − ζ_it, 0}`. Because the subsidy is a function of `p_bmt`, and
`p_bmt` is itself one of the prices in the vector `p` that firms choose, the
subsidy is endogenous to the equilibrium price vector. Saltzman states this
explicitly (p. 368):

> "The complex relationship between insurer and consumer premiums, endogenous
> determination of the benchmark premium, and variation in the benchmark plan
> across consumers due to heterogeneous entry create significant computational
> challenges. I carefully model the endogenous subsidy design despite the high
> computational cost because of the critical role premium subsidies play in
> addressing adverse selection."

So the level p_ijt(p) re-prices through the benchmark term whenever p_bmt moves.

### 1.2 The demand derivative — the 4-case rule (RAND Eq. 9)

Saltzman derives the pass-through directly. For a subsidized consumer the chain
rule is

> ∂q_ikt/∂p_jmt = Σ_l (∂q_ikt/∂p_ilt)·(∂p_ilt/∂p_jmt)

and "assuming a strictly positive subsidy that does not exceed the full,
unsubsidized premium," Eq. (9) gives the four cases for ∂p_ilt/∂p_jmt:

| case | l, j relation | ∂p_ilt/∂p_jmt |
|---|---|---|
| 1 | l = j and j = b (own price, plan is benchmark) | 0 |
| 2 | l = j and j ≠ b (own price, plan not benchmark) | σ_it |
| 3 | l ≠ j and j = b (other plan's net price, j is benchmark) | −σ_it |
| 4 | l ≠ j and j ≠ b | 0 |

His own gloss (p. 368):

> "For a non-benchmark plan, an infinitesimal premium increase results in
> consumers paying more for that plan only. An infinitesimal increase in the
> benchmark premium does not affect what subsidized consumers pay for the
> benchmark plan, but reduces what consumers pay for all other plans because of
> the larger subsidy."

The level (Eq. 8) and the derivative (Eq. 9) are the **same object** —
Eq. 9 is just Eq. 8 differentiated. There is no level/derivative split in
Saltzman; the subsidy is endogenous in both.

### 1.3 The firm FOC carries the benchmark channel into marginal revenue

The firm FOC is RAND Eq. (13):

> MR_jmt(p) = (1 − ι_ft)·MC_jmt(p) − MRA_jmt(p) + v_ft·[∂q_ft/∂p_jmt] / [∂q_jmt/∂p_jmt]

Marginal revenue MR_jmt is defined in web Appendix B, Eq. (6):

> MR_jmt(p) = (∂q_jmt/∂p_jmt)^{-1} · Σ_{i,k∈J_fmt} σ_it·( q_ijt + p_kmt·∂q_ikt/∂p_jmt )

The `∂q_ikt/∂p_jmt` term inside MR is exactly the cross-derivative that, when
expanded through Eq. (9), carries the benchmark channel. So the benchmark
pass-through is structurally inside the FOC residual via marginal revenue, not
bolted on. When a firm prices the benchmark plan, raising it lowers everyone's
net premium on all other plans through the larger subsidy, which MR internalizes.

### 1.4 Counterfactuals re-solve the equilibrium benchmark

Saltzman runs six counterfactuals (combinations of repealing risk adjustment,
repealing the mandate, and replacing the endogenous subsidy with a voucher),
each solved by re-solving the FOCs (Eq. 13 or Eq. 21). In the **base/ACA
scenarios the benchmark is re-solved as part of the equilibrium** — the subsidy
adjusts to the new equilibrium prices.

The voucher counterfactual is the telling contrast (p. 376):

> "I replace ACA subsidies with vouchers by making the benchmark premium in
> formula (8) a constant equal to the observed benchmark premium and then
> resolving the first-order conditions. Fixing the benchmark premium replaces
> formula (9) with [Eq. 22]:"

> ∂p_ilt/∂p_jmt = σ_it if l = j, else 0.

That is, the voucher scenario degenerates the 4-case rule to the simple 2-case
rule (own-price only), because with a fixed benchmark there is no cross-plan
subsidy channel. Table 4's scenario-definition panel literally has a row
"Endogenous subsidy ✓" — present in the base scenario and scenarios 1-3, absent
in scenarios 4-6 (where the subsidy is frozen at its baseline level). The notes
to Table 4 confirm scenario (3) is "fixing the subsidy at its level in the base
scenario so that the subsidy does not adjust to premiums." The welfare reading
is that freezing the subsidy keeps premium-subsidy spending high (compare
scenarios 5 vs 6), whereas the endogenous subsidy falls when the benchmark falls
(repealing risk adjustment cuts benchmark silver, so subsidy spending drops
$113/yr — p. 376).

### 1.5 Fixed-point structure

The benchmark depends on equilibrium prices and prices depend on the benchmark,
so the benchmark is part of the equilibrium fixed point. Saltzman does not
iterate a separate benchmark loop. He solves the full price vector p from the
system of FOCs (Eq. 13 across all plans j) with a root-finder, and the subsidy
(through Eq. 8) and its derivative (through Eq. 9) are evaluated inside each FOC
evaluation at the candidate p. The benchmark is therefore solved jointly with
all other prices in one nonlinear system — not a nested/outer fixed point. He
notes resolving the FOCs is "very computationally burdensome" and runs each
counterfactual once per year on a subsample of ns = 500 households, averaging
across years. He does **not** discuss what happens when the benchmark plan's
identity (which plan is the 2nd-cheapest silver) switches as prices move; with a
joint root-find this is handled implicitly by whatever plan is 2nd-cheapest at
the candidate p, but the non-differentiability at switch points is not addressed
in the paper.

### 1.6 What is fixed vs endogenous in Saltzman

- **Endogenous to p:** the benchmark base premium p_bmt, hence the subsidy
  level and all its derivatives; plan risk scores r_jmt(p); average claims
  c_jmt(p); risk-adjustment transfers RA_ft(p).
- **Fixed (exogenous) per household:** the rating factor σ_it, the income
  contribution cap ζ_it (a function of FPL only, not of prices), subsidy
  eligibility, and which silver plans are CSR-enhanced. Geographic rating
  factors are also held exogenous in the simulations.

The contribution cap ζ_it is the analogue of our `SLC_contribution`: a fixed
per-household income object. Only the benchmark premium becomes endogenous.

---

## Task 2 — Plan to endogenize the subsidy in our code

### 2.0 Current state: a level/derivative mismatch

This is the crux. Our code **already encodes the endogenous-subsidy derivative
but freezes the subsidy in the level**. Concretely:

- **Level (frozen).** `update_premiums()` in
  `code/analysis/structural/4_counterfactuals.R:126-135` re-levels the demand
  `premium` column for a candidate posted-price vector but subtracts
  `dt$adj_subsidy[idx]` — the **baseline** subsidy carried as a frozen column:
  ```r
  premium_hh <- (p_vec[pn] / RATING_FACTOR_AGE40) * dt$rating_factor[idx]
  oop <- pmax(premium_hh - dt$adj_subsidy[idx], 0) - dt$penalty[idx] / 12
  ```
  When the benchmark silver plan's posted price moves in `p_vec`, the subsidy
  does not move. `build_structural()` constructs the same way
  (`helpers/supply.R:130-135`, `adj_subsidy := fifelse(is.na(subsidy), 0, subsidy)`
  then `pmax(premium_hh - adj_subsidy, 0)`).

- **Derivative (endogenous).** `compute_shares_and_elasticities()` in
  `helpers/supply.R:448-581` implements Saltzman's 4-case rule exactly. For the
  benchmark column and subsidized households (lines 552-559) it uses the
  closed-form derivative in which V_l is unchanged and all other V_k move:
  ```r
  sub[, common_factor := (1 - s_lg) * ((lambda_i - 1) / lambda_i - s_g)]
  sub[, dq_dposted := fifelse(plan_id == l,
        alpha_i * (-rf_i) * q_j * common_factor,
        alpha_i * (-rf_i) * q_j * (1 / lambda_i + common_factor))]
  ```
  This is the analytic content of Eq. 9 cases 1 and 3 (own-benchmark price has
  no net-price effect; other plans' net price falls by σ_it as the benchmark
  rises). The same benchmark logic is repeated in
  `compute_broker_shares_and_elasticities()` (lines 659-711) and in the FOC
  Jacobian's benchmark branch (`4_counterfactuals.R:354-364`).

So the FOC residual is internally inconsistent: its **shares and elasticities
assume a subsidy that moves with the benchmark**, but the **net premium it feeds
into utility uses a subsidy frozen at baseline**. At the observed prices (where
posted = baseline) the level is correct by construction, so the observed-scenario
FOC is consistent. But any counterfactual that moves the benchmark plan's posted
price away from baseline introduces a level error of σ_it·(p_b − p_b^obs) in the
net premium of every other plan for subsidized households, while the Jacobian
correctly anticipates that movement. The fix is to make the level consistent
with the derivative — i.e., endogenize the subsidy in `update_premiums()`.

### 2.1 Where the SLCSP benchmark must be recomputed

The benchmark identity is already computed once per cell, from baseline posted
premiums, in `4_counterfactuals.R:88-91`:

```r
silver <- plan_attrs[plan_attrs$metal == "Silver", ]
silver <- silver[order(silver$premium_posted), ]
benchmark_plan <- if (nrow(silver) == 0) NA else if (nrow(silver) == 1) silver$plan_id[1] else silver$plan_id[2]
```

`benchmark_plan` is the 2nd-cheapest silver by **baseline** posted premium and
is held fixed for the whole solve. That fixed identity is fine for a first
implementation (see 2.5). What is missing is recomputing the benchmark **price**
— and hence the subsidy — from the candidate `p_vec` inside `update_premiums()`.

The per-household subsidy as a function of the candidate price vector is

> subsidy_i(p) = max{ 0, rf_i · p_benchmark(p) − SLC_contribution_i }

where `rf_i = rating_factor / RATING_FACTOR_AGE40` (already computed as `rf_i`
in the share code), `p_benchmark(p)` is the posted premium of the benchmark
plan in the candidate vector, and `SLC_contribution_i` is the fixed per-HH
income contribution cap (= ζ_it).

This matches the data-build construction in
`code/data-build/5_merge-and-finalize.R:64-65,106-109`:

```r
premiumSLC = slc_base / RATING_FACTOR_AGE40 * rating_factor   # = rf_i-scaled benchmark
subsidy    = pmax(0, premiumSLC - SLC_contribution)           # for FPL 1.38–4.0
```

and the canonical old-repo formula in
`_old-repo/data-code/prepare.demand.data.R:732`:

```r
households$subsidy <- pmax(0, premiumSLC - premiumSLC_unsubsidized - SLC_contribution)
```

So our baseline definition is exactly Saltzman Eq. 8's subsidy term; only
`slc_base` (→ `premiumSLC`) becomes endogenous when we let it track `p_vec`.

**Data gap to fix first.** `SLC_contribution` is computed in the data build but
**dropped** before saving (`5_merge-and-finalize.R:110-111`), and the analysis
HH file `hh_choice.csv` does not carry it (`0_data-prep.R:144-150` selects
`subsidy` and `poverty_threshold` but not `SLC_contribution` or `premiumSLC`).
To endogenize, we must carry `SLC_contribution` (the fixed cap ζ_it) forward:
stop dropping it in step 5, and add it to the `hh_choice` select in
`0_data-prep.R`, then thread it through `build_structural()`'s `hh_slim`
(`helpers/supply.R:73-79`) and the small-insurer collapse `first()` aggregation
(lines 163-179) so each HH row carries `SLC_contribution`. A robust fallback
when `SLC_contribution` is absent is to back it out from baseline objects:
`SLC_contribution_i = rf_i·p_benchmark^obs − subsidy_i^obs` for subsidized HHs
with positive subsidy, and `+Inf` (no subsidy) otherwise. This recovers the cap
exactly at baseline and is cheap, avoiding a data-build re-run for a first cut.

### 2.2 How `update_premiums()` changes

Replace the frozen `adj_subsidy` with a subsidy recomputed from `p_vec`.
Current (`4_counterfactuals.R:126-135`):

```r
update_premiums <- function(dt, p_vec) {
  for (pn in names(p_vec)) {
    idx <- which(dt$plan_id == pn)
    if (length(idx) == 0) next
    premium_hh <- (p_vec[pn] / RATING_FACTOR_AGE40) * dt$rating_factor[idx]
    oop <- pmax(premium_hh - dt$adj_subsidy[idx], 0) - dt$penalty[idx] / 12
    set(dt, i = idx, j = "premium", value = oop / dt$hh_size[idx] / 100)
  }
  recompute_prem_interactions(dt, STRUCTURAL_SPEC)
}
```

Endogenized:

```r
update_premiums <- function(dt, p_vec) {
  # 1. Endogenous benchmark price from the candidate vector (fixed identity)
  p_b <- if (!is.na(benchmark_plan) && benchmark_plan %in% names(p_vec))
           p_vec[[benchmark_plan]] else NA_real_

  # 2. Per-HH subsidy = max(0, rf_i * p_b - SLC_contribution_i), subsidized only
  if (!is.na(p_b)) {
    rf_i <- dt$rating_factor / RATING_FACTOR_AGE40
    sub_endog <- pmax(0, rf_i * p_b - dt$SLC_contribution)
    dt[, subsidy_cf := fifelse(subsidized == 1L & is.finite(SLC_contribution),
                               sub_endog, 0)]
  } else {
    dt[, subsidy_cf := adj_subsidy]   # no silver/benchmark -> fall back
  }

  # 3. Re-level net premium with the endogenous subsidy
  for (pn in names(p_vec)) {
    idx <- which(dt$plan_id == pn)
    if (length(idx) == 0) next
    premium_hh <- (p_vec[pn] / RATING_FACTOR_AGE40) * dt$rating_factor[idx]
    oop <- pmax(premium_hh - dt$subsidy_cf[idx], 0) - dt$penalty[idx] / 12
    set(dt, i = idx, j = "premium", value = oop / dt$hh_size[idx] / 100)
  }
  recompute_prem_interactions(dt, STRUCTURAL_SPEC)
}
```

Notes:
- `subsidized` and `rating_factor` are already on `cell_data` (kept by
  `build_structural`). `SLC_contribution` is the new column from 2.1.
- This reproduces Saltzman Eq. 8 exactly: own benchmark net premium is
  `max(rf_i·p_b − subsidy_cf, 0) = max(rf_i·p_b − (rf_i·p_b − cap), 0) = cap`
  when the subsidy is interior, i.e., the benchmark net price is pinned to the
  contribution cap (Eq. 9 case 1, the zero derivative). Non-benchmark plans get
  the larger/smaller subsidy through `subsidy_cf` (Eq. 9 cases 2-3).
- The post-solution outcome recompute (`solve_equilibrium`,
  `4_counterfactuals.R:547`) calls the same `update_premiums`, so shares,
  consumer surplus, and reported subsidy spending all become consistent
  automatically. Government subsidy spending in any future welfare accounting
  should sum `subsidy_cf`, not `adj_subsidy`.

### 2.3 Consequence for the analytical Jacobian

With the level endogenized to match, **the existing Jacobian is already
correct** — that is the point of 2.0. The benchmark branch of the Jacobian
(`4_counterfactuals.R:354-364`) and of `compute_shares_and_elasticities`
(`helpers/supply.R:552-559`) is the derivative-side of the endogenous subsidy.
The 4-case rule is precisely "dNetPremium_j/dPosted_l has an extra channel
through the benchmark for all subsidized households whenever l is the benchmark
plan." So endogenizing the level does not require new Jacobian terms; it
**removes** an inconsistency between residual and Jacobian. After the change:

- At baseline prices, residual and Jacobian are unchanged (subsidy_cf =
  adj_subsidy there), so the observed scenario is unaffected.
- Away from baseline, the residual now uses the same subsidy the Jacobian
  assumes, so Newton steps become consistent and convergence should be at least
  as good (the Jacobian was previously the gradient of a slightly different
  function than the residual being zeroed).

One caveat. T3 (endogenous MC) and T4 (elasticity curvature) in the Jacobian
were derived for the existing share/elasticity functions, which already include
the benchmark channel; they remain valid. The only thing that changes is the
level fed into `compute_utility` via `update_premiums`. No T-term needs
rederivation. (If we later also endogenize the **benchmark identity** — 2.4 —
that would introduce non-differentiability, not new smooth terms.)

### 2.4 Can the benchmark identity switch as prices move?

Yes. The benchmark is the 2nd-cheapest silver by posted premium; the argmin /
2nd-argmin is non-differentiable at the prices where two silver plans cross. If
the benchmark identity is allowed to float with `p_vec`, the FOC residual has
kinks and the analytical Jacobian (which assumes a fixed benchmark column) is
wrong at and near switch points, which can stall Newton.

Saltzman does not address this; his joint root-find implicitly re-selects the
benchmark at each candidate p. For our analytical-Jacobian solver the clean
choice is to **hold the benchmark identity fixed within a solve** and let only
its price move. This keeps the residual smooth and the Jacobian exact, at the
cost of a possibly-stale benchmark identity if the equilibrium reorders silver
plans substantially. Two ways to manage the staleness:

1. **Fixed within solve, refresh across solves (recommended).** Solve with the
   baseline benchmark identity; after convergence, recompute which silver plan
   is 2nd-cheapest at the solution; if it changed, re-solve once with the new
   identity, warm-started at the previous solution. Usually one or two outer
   passes. This is an outer fixed point on the benchmark *identity* only, with
   the smooth price system solved exactly inside.
2. **Smooth the benchmark price.** Replace the hard 2nd-min with a softmin over
   silver posted premiums (e.g., `p_b = Σ_s w_s p_s` with
   `w_s ∝ exp(−p_s/τ)` tuned so weight concentrates on the two cheapest). This
   makes the benchmark differentiable everywhere but changes the object and
   requires extending the Jacobian's benchmark column to a weighted combination
   across silver plans. More work; only worth it if identity-switching turns out
   to be frequent.

### 2.5 Recommendation: direct substitution, fixed identity, first cut

Recommended minimal first implementation:

1. **Carry `SLC_contribution` forward** (2.1), or use the baseline-backout
   fallback to avoid a data-build re-run on the first pass.
2. **Endogenize the level in `update_premiums()`** via direct substitution
   (2.2): subsidy = `max(0, rf_i·p_benchmark(p_vec) − SLC_contribution_i)`,
   benchmark **identity fixed** at the baseline 2nd-cheapest silver.
3. **Leave the Jacobian as is** (2.3) — it is already the endogenous-subsidy
   derivative.
4. **Do not** float the benchmark identity inside the solve (2.4); optionally
   add the one-shot outer refresh (option 1) only if post-hoc the benchmark
   identity at the solution differs from baseline in a non-trivial share of
   cells.

Direct substitution (recompute subsidy from `p_vec` inside each residual
evaluation) is preferred over a nested benchmark fixed point because the
benchmark price is a single coordinate of the same `p_vec` the root-finder
already solves — there is no separate object to iterate. This mirrors Saltzman's
joint solve while keeping our analytical Jacobian valid by holding the identity
fixed.

Validation checks after implementing:
- Observed scenario: shares, premiums, CS, and `subsidy_cf` should match the
  pre-change run bit-for-bit (subsidy_cf = adj_subsidy at baseline prices).
- Benchmark plan's net premium for interior-subsidy HHs should equal the
  contribution cap at the solution (Eq. 8 / Eq. 9 case 1).
- Counterfactual that lowers the benchmark silver (e.g., the zero-commission or
  navigator scenarios if they compress silver) should now **reduce** subsidy
  spending and raise net premiums on non-benchmark plans for subsidized HHs,
  the direction Saltzman documents (p. 376).
- FOC convergence should be no worse; if Newton stalls in cells where the
  benchmark identity changes a lot at the solution, add the outer-refresh pass.

---

## Quick cross-reference

| Object | Saltzman | Our code |
|---|---|---|
| HH net premium (Eq. 8) | `max{σp_j − max{σp_b − ζ,0},0}` | `helpers/supply.R:130-135`; `4_counterfactuals.R:126-135` |
| Benchmark = 2nd-cheapest silver | p. 365 | `4_counterfactuals.R:88-91` |
| 4-case derivative (Eq. 9) | p. 368 | `helpers/supply.R:530-577` (shares); `4_counterfactuals.R:354-364` (Jacobian) |
| Firm FOC (Eq. 13) | p. 368 | `4_counterfactuals.R:219-258` (`fn`) |
| MR with benchmark channel (App. B Eq. 6) | web App. B | inside `compute_shares_and_elasticities` cross-derivs |
| Voucher = fix benchmark (Eq. 22) | p. 376 | not implemented; this is the *baseline* behavior in our current frozen-subsidy level |
| Income contribution cap ζ_it | Eq. 8 | `SLC_contribution` (data build, currently dropped) |
| Counterfactual re-solves FOCs | Eq. 13/21 | `solve_equilibrium` via `nleqslv` |

The most important single observation: our pricing code currently behaves like
Saltzman's **voucher** counterfactual in the level (subsidy frozen, Eq. 22) but
like his **ACA base** scenario in the derivative (endogenous subsidy, Eq. 9).
Endogenizing `update_premiums()` aligns the two and makes the base scenario a
true price-linked-subsidy equilibrium.
