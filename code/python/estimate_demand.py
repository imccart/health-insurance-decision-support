"""
Nested logit demand estimation — Python port of Julia estimate_demand_v3.jl.

Two-stage:
  Stage 1: BFGS with BHHH initialization (β only, λ=1 fixed)
  Stage 2: L-BFGS-B joint optimization (β + λ)

Key design:
  V_0 = β'X_0 (NOT normalized to 0)
  Weights normalized to mean 1 globally
  Dict-based HH grouping (no sorting)
  Single-threaded BLAS for determinism
"""

import os
os.environ["OPENBLAS_NUM_THREADS"] = "1"
os.environ["MKL_NUM_THREADS"] = "1"

import glob
import csv
import time
import numpy as np
from scipy.optimize import minimize
from datetime import datetime

# =========================================================================
# Covariates (must match Julia BASE_COVARS + structural covars)
# =========================================================================

COVARS = [
    "premium", "penalty_own", "premium_sq",
    "silver", "bronze", "hh_size_prem",
    "any_0to17_prem", "FPL_250to400_prem", "FPL_400plus_prem",
    "any_black_prem", "any_hispanic_prem", "hmo", "hsa",
    "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
    "Anthem_silver", "BS_silver", "Kaiser_silver", "HN_silver",
    "Anthem_bronze", "BS_bronze", "Kaiser_bronze", "HN_bronze",
    "assisted_silver", "assisted_bronze", "assisted_gold", "assisted_plat",
    "commission_broker", "v_hat_commission",
]
K = len(COVARS)


# =========================================================================
# Data structures
# =========================================================================

class HHGroup:
    __slots__ = ["ins_rows", "unins_row", "chosen_row", "chose_insured", "weight"]
    def __init__(self, ins_rows, unins_row, chosen_row, chose_insured, weight):
        self.ins_rows = ins_rows        # numpy int array of insured row indices
        self.unins_row = unins_row      # int
        self.chosen_row = chosen_row    # int
        self.chose_insured = chose_insured  # bool
        self.weight = weight            # float


class CellData:
    __slots__ = ["X", "groups", "n_hh"]
    def __init__(self, X, groups, n_hh):
        self.X = X              # (n_rows, K) float64
        self.groups = groups    # list of HHGroup
        self.n_hh = n_hh        # int


# =========================================================================
# Build cell data from CSV (dict-based grouping, no sort)
# =========================================================================

def build_cell_data(filepath):
    with open(filepath, "r") as f:
        reader = csv.DictReader(f)
        rows = list(reader)

    n = len(rows)
    if n == 0:
        return None

    # Build X matrix
    X = np.zeros((n, K), dtype=np.float64)
    for k, var in enumerate(COVARS):
        for i in range(n):
            val = rows[i].get(var, "")
            try:
                X[i, k] = float(val) if val != "" else 0.0
            except (ValueError, TypeError):
                X[i, k] = 0.0

    # Group rows by household using dict
    hh_rows_dict = {}
    for i in range(n):
        hh = int(float(rows[i]["household_number"]))
        if hh not in hh_rows_dict:
            hh_rows_dict[hh] = []
        hh_rows_dict[hh].append(i)

    groups = []
    for hh, row_indices in hh_rows_dict.items():
        ins_rows = []
        unins_row = -1
        chosen_row = -1

        for r in row_indices:
            if rows[r]["plan_name"] == "Uninsured":
                unins_row = r
            else:
                ins_rows.append(r)
            if int(float(rows[r]["choice"])) == 1:
                chosen_row = r

        if len(ins_rows) == 0 or unins_row == -1 or chosen_row == -1:
            continue

        chose_insured = rows[chosen_row]["plan_name"] != "Uninsured"
        weight = float(rows[row_indices[0]].get("ipweight", "1"))

        groups.append(HHGroup(
            ins_rows=np.array(ins_rows, dtype=np.int64),
            unins_row=unins_row,
            chosen_row=chosen_row,
            chose_insured=chose_insured,
            weight=weight,
        ))

    if len(groups) == 0:
        return None

    return CellData(X=X, groups=groups, n_hh=len(groups))


# =========================================================================
# Load all cells
# =========================================================================

def load_cells(cell_dir):
    pattern = os.path.join(cell_dir, "cell_*_data.csv")
    csv_files = sorted(glob.glob(pattern))
    print(f"  Found {len(csv_files)} cell files")

    cells = []
    total_hh = 0
    for i, f in enumerate(csv_files):
        cell = build_cell_data(f)
        if cell is not None:
            cells.append(cell)
            total_hh += cell.n_hh
        if (i + 1) % 20 == 0:
            print(f"    Loaded {i + 1} / {len(csv_files)}")

    print(f"  Loaded {len(cells)} cells, {total_hh} HH")
    return cells, total_hh


# =========================================================================
# Normalize weights globally to mean 1
# =========================================================================

def normalize_weights(cells):
    total_w = 0.0
    total_n = 0
    for cell in cells:
        for g in cell.groups:
            total_w += g.weight
            total_n += 1
    global_mean = total_w / total_n
    print(f"  Weights normalized: global mean was {global_mean:.4f}")

    for cell in cells:
        for g in cell.groups:
            g.weight /= global_mean

    return global_mean


# =========================================================================
# NLL + gradient for one cell (per-HH loop — matches Julia exactly)
# =========================================================================

def cell_negll_grad(beta, lam, cell, compute_gradi=False):
    X = cell.X
    n_hh = cell.n_hh

    # V = X @ β for all rows
    V = X @ beta

    negll = 0.0
    grad = np.zeros(K + 1)
    gradi = np.zeros((n_hh, K + 1)) if compute_gradi else None

    for h, g in enumerate(cell.groups):
        ins = g.ins_rows
        w = g.weight

        # Insured nest: logsumexp of V_ins / λ
        V_ins = V[ins]
        V_scaled = np.clip(V_ins / lam, -500, 500)
        max_vs = np.max(V_scaled)
        exp_vs = np.exp(V_scaled - max_vs)
        D = np.sum(exp_vs)
        if D < 1e-300:
            D = 1e-300
        I_val = max_vs + np.log(D)

        # Within-nest shares and weighted averages
        s_jg = exp_vs / D
        X_ins = X[ins]
        x_bar = s_jg @ X_ins          # (K,)
        V_bar = np.dot(s_jg, V_ins)   # scalar

        # V_0 = β'X_0 (NOT zero)
        V_0 = V[g.unins_row]
        X_0 = X[g.unins_row]

        # log denominator: log(exp(λI) + exp(V_0))
        lI = lam * I_val
        mx_d = max(lI, V_0)
        log_denom = mx_d + np.log(np.exp(np.clip(lI - mx_d, -500, 500))
                                   + np.exp(np.clip(V_0 - mx_d, -500, 500)))

        # P(insured)
        P_ins = np.exp(lI - log_denom)
        P_ins = np.clip(P_ins, 1e-15, 1 - 1e-15)

        # Log-likelihood
        if g.chose_insured:
            V_ch = V[g.chosen_row]
            ll_h = V_ch / lam + (lam - 1.0) * I_val - log_denom
        else:
            ll_h = V_0 - log_denom
        negll -= w * ll_h

        # Gradient w.r.t. β
        if g.chose_insured:
            X_ch = X[g.chosen_row]
            g_beta = (X_ch - x_bar) / lam + (1.0 - P_ins) * (x_bar - X_0)
            g_lam = (-V_ch / lam**2 + I_val
                     - (lam - 1.0) * V_bar / lam**2
                     - P_ins * (I_val - V_bar / lam))
        else:
            g_beta = -P_ins * (x_bar - X_0)
            g_lam = -P_ins * (I_val - V_bar / lam)

        grad[:K] -= w * g_beta
        grad[K] -= w * g_lam

        if compute_gradi:
            gradi[h, :K] = -w * g_beta
            gradi[h, K] = -w * g_lam

    # Guard against NaN
    if not np.isfinite(negll):
        negll = 1e20
    grad[~np.isfinite(grad)] = 0.0

    return negll, grad, gradi


# =========================================================================
# Accumulate NLL + gradient across cells
# =========================================================================

def accumulate(theta, cells, compute_gradi=False):
    beta = theta[:K]
    lam = theta[K]

    total_negll = 0.0
    total_grad = np.zeros(K + 1)
    all_gradi = [] if compute_gradi else None

    for cell in cells:
        nll, grad, gradi = cell_negll_grad(beta, lam, cell, compute_gradi)
        total_negll += nll
        total_grad += grad
        if compute_gradi:
            all_gradi.append(gradi)

    return total_negll, total_grad, all_gradi


# =========================================================================
# Stage 1: BFGS-BHHH on β with λ fixed (matches Julia optimize_beta)
# =========================================================================

def optimize_beta(cells, beta_init=None, lam_fixed=1.0,
                  max_iter=2000, tol=1e-6, stptol=1e-10):

    beta = np.zeros(K) if beta_init is None else beta_init.copy()
    theta = np.append(beta, lam_fixed)

    # Initial evaluation with BHHH
    print("  Computing BHHH initialization...")
    negll, full_grad, all_gradi = accumulate(theta, cells, compute_gradi=True)
    grad_beta = full_grad[:K]

    # BHHH matrix (β dimensions only)
    bhhh = np.zeros((K, K))
    for gradi in all_gradi:
        gi_beta = gradi[:, :K]
        bhhh += gi_beta.T @ gi_beta
    del all_gradi

    try:
        Hm1 = np.linalg.inv(bhhh)
    except np.linalg.LinAlgError:
        print("  WARNING: BHHH singular, using scaled identity")
        Hm1 = np.eye(K) * 1e-4

    print(f"  Init: negLL = {negll:.2f}  beta_1 = {beta[0]:.6f}")

    for it in range(1, max_iter + 1):
        old_negll = negll
        old_grad = grad_beta.copy()

        d = -Hm1 @ grad_beta

        # Halving line search
        step = 2.0
        while True:
            step /= 2.0
            if step < stptol:
                break
            theta_try = np.append(beta + step * d, lam_fixed)
            nll_try, _, _ = accumulate(theta_try, cells)
            if nll_try <= old_negll:
                break

        if step < stptol:
            print(f"    beta iter {it:3d}: step too small")
            break

        # Accept
        beta = beta + step * d
        theta = np.append(beta, lam_fixed)
        negll, full_grad, _ = accumulate(theta, cells)
        grad_beta = full_grad[:K]

        # BFGS inverse Hessian update
        incr = step * d
        y = grad_beta - old_grad
        sy = np.dot(incr, y)
        if abs(sy) > 1e-20:
            yHy = y @ Hm1 @ y
            Hm1 = (Hm1
                    + (sy + yHy) / sy**2 * np.outer(incr, incr)
                    - (Hm1 @ np.outer(y, incr) + np.outer(incr, y) @ Hm1) / sy)

        if it <= 3 or it % 10 == 0:
            print(f"    beta iter {it:3d}: negLL = {negll:.2f}  step = {step:.4f}  beta_1 = {beta[0]:.6f}")

        if abs(negll - old_negll) < tol:
            print(f"    beta converged at iter {it}")
            break

    return beta, negll


# =========================================================================
# Optimize λ with β fixed (golden section search, matches Julia)
# =========================================================================

def optimize_lambda(cells, beta, lam_lo=0.01, lam_hi=1.0, tol=1e-4):

    phi = (np.sqrt(5.0) - 1.0) / 2.0  # golden ratio

    a, b = lam_lo, lam_hi
    c = b - phi * (b - a)
    d = a + phi * (b - a)

    theta_c = np.append(beta, c)
    fc, _, _ = accumulate(theta_c, cells)
    theta_d = np.append(beta, d)
    fd, _, _ = accumulate(theta_d, cells)

    while (b - a) > tol:
        if fc < fd:
            b = d; d = c; fd = fc
            c = b - phi * (b - a)
            theta_c = np.append(beta, c)
            fc, _, _ = accumulate(theta_c, cells)
        else:
            a = c; c = d; fc = fd
            d = a + phi * (b - a)
            theta_d = np.append(beta, d)
            fd, _, _ = accumulate(theta_d, cells)

    lam_opt = (a + b) / 2.0
    theta_opt = np.append(beta, lam_opt)
    negll_opt, _, _ = accumulate(theta_opt, cells)
    return lam_opt, negll_opt


# =========================================================================
# Coordinate descent: alternate β|λ and λ|β (matches Julia)
# =========================================================================

def coordinate_descent(cells, max_outer=20, outer_tol=1.0):

    print("  Coordinate descent: beta|lambda then lambda|beta")

    beta = np.zeros(K)
    lam = 1.0

    # Outer 0: optimize β at λ=1
    print(f"    Outer 0: optimizing beta at lambda = {lam:.4f}")
    beta, negll = optimize_beta(cells, beta_init=beta, lam_fixed=lam)
    print(f"    Outer 0: negLL = {negll:.2f}  lambda = {lam:.4f}  beta_1 = {beta[0]:.6f}")

    for outer in range(1, max_outer + 1):
        old_negll = negll

        # Optimize λ with β fixed
        lam, negll_lam = optimize_lambda(cells, beta)
        print(f"    Outer {outer}: lambda search -> lambda = {lam:.4f}  negLL = {negll_lam:.2f}")

        # Optimize β with λ fixed
        beta, negll = optimize_beta(cells, beta_init=beta, lam_fixed=lam)
        print(f"    Outer {outer}: beta optim  -> negLL = {negll:.2f}  beta_1 = {beta[0]:.6f}")

        if abs(negll - old_negll) < outer_tol:
            print(f"  Coordinate descent converged at outer iter {outer}")
            break

    print(f"  Final: negLL = {negll:.2f}  lambda = {lam:.4f}  beta_1 = {beta[0]:.6f}")
    return np.append(beta, lam), negll


# =========================================================================
# Main
# =========================================================================

def main():
    cell_dir = "data/output/choice_cells"
    out_dir = "results"

    print("=== Structural demand estimation (Python) ===")
    print("  V_0 = beta'X_0 (NOT 0)")
    print("  Coordinate descent: beta|lambda then lambda|beta")
    print(f"  Covariates: {K}")
    print()

    # Load cells
    cells, total_hh = load_cells(cell_dir)
    normalize_weights(cells)

    t0 = time.time()

    # Coordinate descent
    theta_opt, negll = coordinate_descent(cells)

    elapsed = time.time() - t0
    print(f"\n  Done in {elapsed / 60:.1f} min")
    print(f"  negLL = {negll:.2f}  lambda = {theta_opt[K]:.4f}")

    # Save coefficients
    terms = COVARS + ["lambda"]
    coef_path = os.path.join(out_dir, "choice_coefficients_structural.csv")
    with open(coef_path, "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["term", "estimate"])
        for t, e in zip(terms, theta_opt):
            writer.writerow([t, e])

    print("\n  Coefficients:")
    for t, e in zip(terms, theta_opt):
        print(f"    {t:25s} = {e:12.6f}")

    beta_p = theta_opt[COVARS.index("premium")]
    beta_c = theta_opt[COVARS.index("commission_broker")]
    if abs(beta_p) > 1e-10:
        print(f"\n  beta_commission / |beta_premium| = {beta_c / abs(beta_p):.4f}")

    print(f"  -> {coef_path}")
    print("\nDone.")


if __name__ == "__main__":
    main()
