# Nested Logit Optimizer Notes

## Problem

mlogit (R package) handles nested logit estimation but can't scale beyond ~25M rows due to dense model matrix + Hessian. The structural estimation needs to pool across 114 region-year cells (~60M+ rows at 20% sample), requiring a custom cell-by-cell accumulator.

## What Failed

1. **L-BFGS-B, nlminb, BFGS with logit-transform**: All fail from any starting point. L-BFGS-B's approximate Hessian can't capture the curvature between β and λ directions. lambda drifts to bounds.

2. **V_0 = 0 normalization**: The original accumulator set V_uninsured = 0, excluding uninsured rows from the X matrix. mlogit computes V_0 = β'X_0 where X_0 contains the mandate penalty and demographic interactions. This caused a likelihood mismatch of ~3400 log-units at the same parameters. With the wrong likelihood, no optimizer could find the right solution.

3. **IPW weight normalization**: mlogit normalizes weights to mean 1 (`weights / mean(weights)`). Our initial implementation used raw weights (sum to N), causing another LL mismatch.

## What Works

**BFGS with BHHH initialization** (replicating `mlogit:::mlogit.optim`):

- Initialize inverse Hessian from outer product of per-HH gradients: `Hm1 = solve(crossprod(gradi))`
- Search direction: `d = -Hm1 %*% g` (descent for minimizing negLL)
- Halving line search: start step=1, halve until negLL improves
- BFGS rank-2 update of `Hm1` after each step
- Converge when `chi2 = -d'g < tol`

This replicates mlogit's exact optimization path and converges from zeros + λ=1 in ~46 iterations to match mlogit's solution exactly.

## Key Implementation Details

1. **V_0 != 0**: Include uninsured rows in the X matrix. Compute `V_0 = β'X_0` for each HH. The nest probability becomes `P(ins) = exp(λI) / (exp(λI) + exp(V_0))`, not `exp(λI) / (1 + exp(λI))`.

2. **Gradient with V_0 != 0**:
   - Insured chooser: `∂ll/∂β_k = (X_ch_k - x̄_k)/λ + (1-P_ins)(x̄_k - X_0_k)`
   - Uninsured chooser: `∂ll/∂β_k = -P_ins(x̄_k - X_0_k)`
   - Lambda gradient unchanged from V_0=0 case.

3. **Weight normalization**: `ipw = ipw_raw / mean(ipw_raw)` (mean 1, matching mlogit).

4. **Per-HH gradi**: The BHHH initialization requires per-HH gradient contributions (n_hh × K+1 matrix), not just the total gradient vector. `crossprod(gradi)` approximates the Hessian.

5. **data.table `copy()`**: Required when creating data.tables inside the LL function to avoid locked binding errors on repeated calls.

## Diagnostic Scripts

- `diagnostic_bfgs_v0fix.R` — the working diagnostic that verified the fix. Compares mlogit, custom BFGS from mlogit.start, and custom BFGS from zeros+λ=1. All three match.
