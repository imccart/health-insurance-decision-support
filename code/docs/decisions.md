# Analysis decisions log

Reasoning behind non-obvious choices in the analysis code. Code comments say what a
block does; the "why" lives here. Newest first.

## 2026-08 — Producer surplus and government cost in the counterfactuals

score_cf now also returns producer surplus and government subsidy per member per
year, so the counterfactuals report a total-surplus picture rather than consumer
welfare alone.

- Producer surplus is the insurer margin, posted premium less marginal cost,
  summed over enrolled members and net of commissions paid to brokers. Commission
  payments are a transfer from insurers to brokers, so they are subtracted from
  insurer profit and not reported as broker surplus on their own. Net broker
  welfare would need an effort-cost estimate we do not have, and the value of
  broker guidance is already on the consumer side through coverage.
- Government subsidy is the advance premium tax credit actually paid, capped at
  the household premium, on enrolled households. It is the policy-relevant fiscal
  term missing from the consumer measures, and it tracks coverage because the
  subsidy is paid only on enrolled households.
- Both are computed in the cf2 scoring pass (from cf1's solved premiums, mc, and
  commissions), so no re-solve is needed. They sit in a separate table from the
  consumer-welfare results to avoid overflowing the headline table.

## 2026-08 — Consumer surplus: common fixed alpha, per member per year

**Marginal utility of income (alpha).** Consumer surplus is `inclusive value / alpha`.
We use a single common alpha per cell — the household-size-weighted mean of the base
per-dollar price sensitivity, with the assistance-channel premium interactions
(`assisted_premium`, `broker_premium`) zeroed — held fixed across all scenarios.

- Alpha only converts utils to dollars; it is not a welfare weight, so it should not
  differ across a policy that leaves the household's income unchanged.
- The old code recomputed a per-household alpha per scenario. Because `assisted_premium`
  puts assistance into the price coefficient, alpha then changed between the observed
  and no-assistance states, and the CS "change" re-priced the whole baseline inclusive
  value instead of measuring the change. It also blew up `1/alpha` for the ~1% of
  households whose demographic premium terms nearly cancel the base coefficient.
- A single fixed alpha removes both problems. This is Saltzman's fixed-denominator
  logic; we take the demographic mean rather than a person-specific alpha to stay
  robust to that near-zero tail.
- Effect: the remove-assistance CS change fell from about −$3,400 to about −$540 per
  member per year, roughly 1.3x the objective measure rather than ~9x, and the
  tau-gradient became monotonic.

**Scale.** CS is reported per member per year to match the objective money metric:
divide the household compensating variation by household size and multiply by 12 (the
premium underneath is monthly). `Delta` Premium in the results table is annualized (x12)
for the same reason.

## 2026-08 — cf3 standard errors: frozen equilibrium

cf3 bootstraps the counterfactual welfare SEs by redrawing the demand parameters and
re-scoring welfare at cf1's solved premiums, held fixed, rather than re-solving the
equilibrium on each draw. Reasons:

- Re-solving inside every draw ran for days.
- It hit the multiple-equilibria instability on the commission-ban (tau=0) scenario.
- The premium-response channel it omits is small — across draws the re-solved premiums
  move only a few dollars per plan — so holding them fixed changes the welfare spread
  little.
- Cost parameters enter welfare only through premiums, which are fixed here, so they
  drop out; only the demand parameters are drawn.

The resulting SE treats premiums as known, so it is a lower bound, reported as such.
The only reason to un-freeze would be a referee wanting full-uncertainty SEs, as a
one-off robustness check. (The solver warm-start and cost-parameter scaffolding for
that version was removed in 2026-08; restore from git if ever needed.)

## 2026-08 — Metal reshuffle: structural vs reduced form

The structural remove-assistance counterfactual and the reduced-form prediction-based
ATT agree that assistance moves ~9-12pp into silver (structural +9.1 conditional on
insured, RF +12.3). They disagree on bronze: the structural moves people into bronze
(+4.3 conditional) while the design moves them out (−9.5). The unconditional structural
silver effect (+23.3) was inflated by the uninsured margin; conditioning on insured
brings it in line, since the RF plan-choice model has no outside option. The bronze gap
is small and the direction is not pinned by theory (assistance can steer toward silver
for CSR-eligibles or toward cheap bronze for the premium-minded), so it is treated as
second-order. The objective and CS are the reported welfare measures.

**Rejected along the way:**
- `csr_silver` (`csr_eligible x silver`): added to test whether CSR-eligibility
  selection drove the large `assisted_silver`. It did not — `assisted_silver` barely
  moved when it was included — so it was reverted.
- Dropping the channel price interactions (`assisted_premium`, `broker_premium`): tested
  to see if they drove the bad no-assistance extrapolation. They did not fix the metal
  ATT, so the spec was kept as is.

## Naming

`assisted` (the column) means any assistance, navigator or broker. The interaction
prefixes do not track it: `nonbroker` = navigator, and the `assisted_*` interactions are
built from `nonbroker`, so `assisted_*` = navigator and `broker_*` = broker.

## Cross-approach comparisons

Comparing an effect across the reduced-form (design) and structural approaches requires
matching the weighting first. The RF prediction-based ATT is IPW-weighted; the structural
diagnostics here used household-size weights. The treatment definition is consistent
(both `assisted == 1`), but redo the structural side with IPW weights before reading any
effect-size gap.
