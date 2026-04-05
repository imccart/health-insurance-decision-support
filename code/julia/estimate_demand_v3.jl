# estimate_demand_v3.jl
#
# Nested logit demand estimation with BFGS-BHHH optimizer.
# Incorporates ALL fixes from the R diagnostic work:
#   1. V_0 = β'X_0 (NOT normalized to 0)
#   2. IPW weights normalized to mean 1 GLOBALLY across all cells
#   3. BFGS with BHHH initialization (replicates mlogit:::mlogit.optim)
#   4. Starting values: MNL β + λ=1
#
# Called from R via system2(). Reads cell CSVs, writes coefficient CSVs.
# See code/analysis/structural/optimizer.md for full documentation.

using CSV, DataFrames, LinearAlgebra, Printf, Optim, Dates, Mmap

# Single-threaded BLAS for deterministic results
BLAS.set_num_threads(1)

# =========================================================================
# Data structures
# =========================================================================

struct HHGroup
    ins_start::Int      # first insured row in X
    ins_end::Int        # last insured row in X
    unins_row::Int      # uninsured row in X (V_0 = β'X[unins_row,:])
    chosen_row::Int     # chosen alternative row in X
    chose_insured::Bool
    weight::Float64     # IPW (raw — normalized globally later)
end

struct CellData
    X::Matrix{Float64}  # ALL rows including uninsured (n_rows × K)
    groups::Vector{HHGroup}
    n_hh::Int
end

# =========================================================================
# Covariates (must match R's COVARIATES_BASE / COVARIATES_ASST)
# =========================================================================

# =========================================================================
# Load covariate spec from file (written by R's _analysis.R)
# =========================================================================

function load_spec(path::String)
    df = CSV.read(path, DataFrame)
    String.(df.term)
end

# =========================================================================
# Build cell data from one CSV
# =========================================================================

function build_cell_data(df::DataFrame, covars::Vector{String})
    n = nrow(df)
    K = length(covars)

    # Group rows by household using a dictionary (no sort needed)
    hh_num = df.household_number
    hh_rows = Dict{Int, Vector{Int}}()
    for i in 1:n
        hh = hh_num[i]
        if haskey(hh_rows, hh)
            push!(hh_rows[hh], i)
        else
            hh_rows[hh] = [i]
        end
    end

    # Build X matrix (ALL rows including uninsured)
    X = Matrix{Float64}(undef, n, K)
    for (k, var) in enumerate(covars)
        if var in names(df)
            col = df[!, var]
            for i in 1:n
                X[i, k] = Float64(ismissing(col[i]) ? 0.0 : col[i])
            end
        else
            X[:, k] .= 0.0
        end
    end

    choice = df.choice
    plan_name = df.plan_name

    groups = HHGroup[]
    for (hh, rows) in hh_rows
        ins_start = -1; ins_end = -1; unins_row = -1; chosen_row = -1
        for r in rows
            if plan_name[r] == "Uninsured"
                unins_row = r
            else
                if ins_start == -1
                    ins_start = r; ins_end = r
                else
                    ins_start = min(ins_start, r)
                    ins_end = max(ins_end, r)
                end
            end
            choice[r] == 1 && (chosen_row = r)
        end

        # Must have insured alternatives, uninsured row, and a chosen alternative
        if ins_start != -1 && unins_row != -1 && chosen_row != -1
            push!(groups, HHGroup(ins_start, ins_end, unins_row, chosen_row,
                                   chosen_row != unins_row, Float64(df.ipweight[rows[1]])))
        end
    end

    CellData(X, groups, length(groups))
end

# =========================================================================
# Load cells from CSV directory
# =========================================================================

function load_cells(cell_dir::String, covars::Vector{String};
                     filter_assisted::Int=-1)  # -1=all, 0=unassisted, 1=assisted
    csv_files = sort(filter(f -> startswith(f, "cell_") && endswith(f, "_data.csv"),
                            readdir(cell_dir)))
    cells = CellData[]
    total_hh = 0

    # Only parse columns we actually need
    needed_cols = vcat(["household_number", "plan_name", "choice", "ipweight"], covars)
    if filter_assisted >= 0
        push!(needed_cols, "assisted")
    end
    unique!(needed_cols)

    for (idx, f) in enumerate(csv_files)
        df = CSV.read(joinpath(cell_dir, f), DataFrame; select=needed_cols)
        if filter_assisted >= 0
            filter!(row -> row.assisted == filter_assisted, df)
            nrow(df) == 0 && (df = nothing; continue)
        end
        cell = build_cell_data(df, covars)
        df = nothing  # release DataFrame memory for GC
        cell.n_hh > 0 && push!(cells, cell)
        total_hh += cell.n_hh
        idx % 20 == 0 && println("    Loaded $idx / $(length(csv_files))")
    end

    println("  Loaded $(length(cells)) cells, $total_hh HH")
    cells, total_hh
end

# =========================================================================
# Normalize weights globally to mean 1 (matching mlogit)
# =========================================================================

function normalize_weights!(cells::Vector{CellData})
    total_w = 0.0
    total_n = 0
    for cell in cells
        for g in cell.groups
            total_w += g.weight
            total_n += 1
        end
    end
    global_mean = total_w / total_n

    for cell in cells
        for (i, g) in enumerate(cell.groups)
            cell.groups[i] = HHGroup(g.ins_start, g.ins_end, g.unins_row,
                                      g.chosen_row, g.chose_insured,
                                      g.weight / global_mean)
        end
    end
    println("  Weights normalized: global mean was $(round(global_mean, digits=4))")
end

# =========================================================================
# Offload X matrices to memory-mapped files
# =========================================================================
# Moves the ~12GB of X matrices from Julia's GC heap to OS-managed
# anonymous memory maps. Uses pagefile-backed anonymous mmap (no named
# files) to avoid CrowdStrike's file filter driver intercepting I/O.

function offload_to_mmap(cells::Vector{CellData})
    new_cells = CellData[]

    for cell in cells
        X_anon = Mmap.mmap(Matrix{Float64}, size(cell.X))
        copyto!(X_anon, cell.X)
        push!(new_cells, CellData(X_anon, cell.groups, cell.n_hh))
    end

    println("  Offloaded $(length(new_cells)) cells to anonymous mmap")
    new_cells
end

# =========================================================================
# NLL + gradient for one cell (V_0 = β'X_0, NOT 0)
#
# Returns: (negll, grad, gradi)
#   negll: scalar
#   grad: K+1 vector
#   gradi: n_hh × (K+1) matrix (for BHHH init only; pass nothing to skip)
# =========================================================================

function cell_negll_grad!(β::AbstractVector{Float64}, λ::Float64,
                           cell::CellData;
                           compute_grad::Bool=true,
                           compute_gradi::Bool=false)
    X = cell.X
    K = length(β)
    n_hh = cell.n_hh

    # V = X * β for ALL rows
    V = X * β  # one allocation per cell

    negll = 0.0
    grad = compute_grad ? zeros(K + 1) : nothing
    gradi = (compute_grad && compute_gradi) ? zeros(n_hh, K + 1) : nothing

    for (h, g) in enumerate(cell.groups)
        w = g.weight

        # --- Insured nest: logsumexp of V_ins/λ ---
        max_vs = -Inf
        for r in g.ins_start:g.ins_end
            v_scaled = V[r] / λ
            v_scaled > max_vs && (max_vs = v_scaled)
        end

        D = 0.0
        for r in g.ins_start:g.ins_end
            D += exp(V[r] / λ - max_vs)
        end
        I = max_vs + log(D)  # inclusive value

        # x_bar, V_bar only needed for gradient
        if compute_grad
            x_bar = zeros(K)
            V_bar = 0.0
            for r in g.ins_start:g.ins_end
                s_j = exp(V[r] / λ - max_vs) / D
                V_bar += s_j * V[r]
                for k in 1:K
                    x_bar[k] += s_j * X[r, k]
                end
            end
        end

        # V_0 = β'X_0 (NOT zero!)
        V_0 = V[g.unins_row]

        # log denominator: log(exp(λI) + exp(V_0))
        lI = λ * I
        mx_d = max(lI, V_0)
        log_denom = mx_d + log(exp(lI - mx_d) + exp(V_0 - mx_d))

        # P(insured) — needed for both negLL (via log_denom) and gradient
        P_ins = compute_grad ? exp(lI - log_denom) : 0.0

        # --- Log-likelihood ---
        if g.chose_insured
            V_ch = V[g.chosen_row]
            ll_h = V_ch / λ + (λ - 1.0) * I - log_denom
        else
            ll_h = V_0 - log_denom
        end
        negll -= w * ll_h

        # --- Gradient (skip entirely when compute_grad=false) ---
        if compute_grad
            X_0 = @view X[g.unins_row, :]
            if g.chose_insured
                X_ch = @view X[g.chosen_row, :]
                for k in 1:K
                    g_k = (X_ch[k] - x_bar[k]) / λ + (1.0 - P_ins) * (x_bar[k] - X_0[k])
                    grad[k] -= w * g_k
                    gradi !== nothing && (gradi[h, k] = -w * g_k)
                end
                g_lam = -V_ch / λ^2 + I - (λ - 1.0) * V_bar / λ^2 - P_ins * (I - V_bar / λ)
                grad[K+1] -= w * g_lam
                gradi !== nothing && (gradi[h, K+1] = -w * g_lam)
            else
                for k in 1:K
                    g_k = -P_ins * (x_bar[k] - X_0[k])
                    grad[k] -= w * g_k
                    gradi !== nothing && (gradi[h, k] = -w * g_k)
                end
                g_lam = -P_ins * (I - V_bar / λ)
                grad[K+1] -= w * g_lam
                gradi !== nothing && (gradi[h, K+1] = -w * g_lam)
            end
        end
    end

    negll, grad, gradi
end

# =========================================================================
# Accumulate NLL + gradient across cells
# =========================================================================

function accumulate(θ::Vector{Float64}, cells::Vector{CellData};
                     compute_grad::Bool=true,
                     compute_gradi::Bool=false)
    K = length(θ) - 1
    β = @view θ[1:K]
    λ = θ[K+1]

    total_negll = 0.0
    total_grad = compute_grad ? zeros(K + 1) : nothing
    all_gradi = (compute_grad && compute_gradi) ? Vector{Matrix{Float64}}() : nothing

    for cell in cells
        negll, grad, gradi = cell_negll_grad!(β, λ, cell;
                                               compute_grad=compute_grad,
                                               compute_gradi=compute_gradi)
        total_negll += negll
        if compute_grad
            total_grad .+= grad
            compute_gradi && push!(all_gradi, gradi)
        end
    end

    total_negll, total_grad, all_gradi
end

# =========================================================================
# BFGS with BHHH initialization (replicates mlogit:::mlogit.optim)
# =========================================================================

function bfgs_bhhh(start::Vector{Float64}, cells::Vector{CellData};
                    max_iter::Int=500, tol::Float64=1e-6,
                    ftol::Float64=1e-8, stptol::Float64=1e-10,
                    print_every::Int=5)
    K = length(start) - 1
    θ = copy(start)

    # Initial evaluation with per-HH gradi for BHHH
    negll, g, all_gradi = accumulate(θ, cells; compute_gradi=true)

    # BHHH: Hm1 = inv(Σ gradi'gradi)
    bhhh = zeros(K + 1, K + 1)
    for gradi in all_gradi
        bhhh .+= gradi' * gradi
    end
    all_gradi = nothing  # free memory
    GC.gc()

    Hm1 = try
        inv(bhhh)
    catch
        @warn "BHHH singular, using identity"
        Matrix{Float64}(I, K + 1, K + 1)
    end

    @printf("  Init: negLL = %.2f  λ = %.4f  β₁ = %.6f\n", negll, θ[K+1], θ[1])

    for iter in 1:max_iter
        old_negll = negll
        old_g = copy(g)

        # Search direction (descent for minimization)
        d = -Hm1 * g

        # Halving line search
        step = 2.0
        negll_try = Inf
        while true
            step /= 2.0
            step < stptol && break
            θ_try = θ .+ step .* d
            (θ_try[K+1] <= 0.001 || θ_try[K+1] >= 5.0) && continue
            negll_try, _, _ = accumulate(θ_try, cells; compute_grad=false)
            negll_try <= old_negll && break
        end

        if step < stptol
            @printf("  Iter %3d: step too small\n", iter)
            break
        end

        # Accept step — need gradient for BFGS update
        θ .+= step .* d
        negll, g, _ = accumulate(θ, cells; compute_grad=true)

        # BFGS Hessian update
        incr = step .* d
        y = g .- old_g
        sy = dot(incr, y)
        if abs(sy) > 1e-20
            Hm1 .= Hm1 .+ (incr * incr') .* (sy + (y' * Hm1 * y)[1]) ./ sy^2 .-
                    (Hm1 * (y * incr') .+ (incr * y') * Hm1) ./ sy
        end

        chi2 = -dot(d, old_g)

        if iter % print_every == 0 || iter <= 3
            @printf("  Iter %3d: negLL = %.2f  step = %.4f  λ = %.4f  β₁ = %.6f  χ² = %.2f\n",
                    iter, negll, step, θ[K+1], θ[1], chi2)
        end

        abs(negll - old_negll) < ftol && (println("  Converged (ftol)"); break)
        abs(chi2) < tol && (println("  Converged (χ²)"); break)
    end

    θ, negll
end

# =========================================================================
# Optimize β with λ fixed (BFGS-BHHH on β only)
# =========================================================================

function optimize_beta(cells::Vector{CellData}, K::Int, β_init::Vector{Float64},
                        λ_fixed::Float64; max_iter::Int=2000,
                        tol::Float64=1e-6, stptol::Float64=1e-10,
                        verbose::Bool=true)
    β = copy(β_init)

    # Initial evaluation with gradi for BHHH
    θ = vcat(β, λ_fixed)
    negll, full_grad, all_gradi = accumulate(θ, cells; compute_gradi=true)
    grad_β = full_grad[1:K]

    # BHHH on β dimensions only
    bhhh = zeros(K, K)
    for gradi in all_gradi
        gi_β = gradi[:, 1:K]
        bhhh .+= gi_β' * gi_β
    end
    all_gradi = nothing
    GC.gc()

    Hm1 = try
        inv(bhhh)
    catch
        @warn "BHHH singular, using scaled identity"
        Matrix{Float64}(I, K, K) * 1e-4
    end

    for iter in 1:max_iter
        old_negll = negll
        old_grad = copy(grad_β)

        d = -Hm1 * grad_β

        step = 2.0
        while true
            step /= 2.0
            step < stptol && break
            θ_try = vcat(β .+ step .* d, λ_fixed)
            negll_try, _, _ = accumulate(θ_try, cells; compute_grad=false)
            negll_try <= old_negll && break
        end

        if step < stptol
            verbose && @printf("    β iter %3d: step too small\n", iter)
            break
        end

        # Accept step — need gradient for BFGS update
        β .+= step .* d
        θ = vcat(β, λ_fixed)
        negll, full_grad, _ = accumulate(θ, cells; compute_grad=true)
        grad_β = full_grad[1:K]

        incr = step .* d
        y = grad_β .- old_grad
        sy = dot(incr, y)
        if abs(sy) > 1e-20
            Hm1 .= Hm1 .+ (incr * incr') .* (sy + (y' * Hm1 * y)[1]) ./ sy^2 .-
                    (Hm1 * (y * incr') .+ (incr * y') * Hm1) ./ sy
        end

        if verbose && (iter % 10 == 0 || iter <= 3)
            @printf("    β iter %3d: negLL = %.2f  step = %.4f  β₁ = %.6f\n",
                    iter, negll, step, β[1])
        end

        abs(negll - old_negll) < tol && (verbose && @printf("    β converged at iter %d\n", iter); break)
    end

    β, negll
end

# =========================================================================
# Optimize λ with β fixed (golden section search)
# =========================================================================

function optimize_lambda(cells::Vector{CellData}, β::Vector{Float64};
                          λ_lo::Float64=0.01, λ_hi::Float64=1.0,
                          tol::Float64=1e-4)
    φ = (sqrt(5.0) - 1.0) / 2.0  # golden ratio

    a, b = λ_lo, λ_hi
    c = b - φ * (b - a)
    d = a + φ * (b - a)

    fc, _, _ = accumulate(vcat(β, c), cells; compute_grad=false)
    fd, _, _ = accumulate(vcat(β, d), cells; compute_grad=false)

    while (b - a) > tol
        if fc < fd
            b = d; d = c; fd = fc
            c = b - φ * (b - a)
            fc, _, _ = accumulate(vcat(β, c), cells; compute_grad=false)
        else
            a = c; c = d; fc = fd
            d = a + φ * (b - a)
            fd, _, _ = accumulate(vcat(β, d), cells; compute_grad=false)
        end
    end

    λ_opt = (a + b) / 2.0
    negll_opt, _, _ = accumulate(vcat(β, λ_opt), cells; compute_grad=false)
    λ_opt, negll_opt
end

# =========================================================================
# Coordinate descent: alternate β|λ and λ|β
# =========================================================================

function coordinate_descent(cells::Vector{CellData}, K::Int;
                             max_outer::Int=20, outer_tol::Float64=1.0,
                             verbose::Bool=true)
    println("  Coordinate descent: β|λ then λ|β")

    # Start: MNL (β from zeros, λ=1)
    β = zeros(K)
    λ = 1.0

    verbose && println("    Outer 0: optimizing β at λ = 1.0000")
    β, negll = optimize_beta(cells, K, β, λ; verbose=verbose)
    verbose && @printf("    Outer 0: negLL = %.2f  λ = %.4f  β₁ = %.6f\n", negll, λ, β[1])

    for outer in 1:max_outer
        old_negll = negll

        # Optimize λ with β fixed
        λ, negll_λ = optimize_lambda(cells, β)
        verbose && @printf("    Outer %d: λ search → λ = %.4f  negLL = %.2f\n", outer, λ, negll_λ)

        # Optimize β with λ fixed
        β, negll = optimize_beta(cells, K, β, λ; verbose=false)
        verbose && @printf("    Outer %d: β optim  → negLL = %.2f  β₁ = %.6f\n", outer, negll, β[1])

        if abs(negll - old_negll) < outer_tol
            @printf("  Coordinate descent converged at outer iter %d\n", outer)
            break
        end
    end

    @printf("  Final: negLL = %.2f  λ = %.4f  β₁ = %.6f\n", negll, λ, β[1])
    vcat(β, λ), negll
end

# =========================================================================
# Main
# =========================================================================

function main()
    temp_dir = get(ENV, "TEMP_DIR", "data/output")
    cell_dir = get(ENV, "DEMAND_CELL_DIR", joinpath(temp_dir, "choice_cells"))
    out_dir = "results"
    spec_path = get(ENV, "DEMAND_SPEC_PATH", joinpath(temp_dir, "demand_spec.csv"))
    out_path = get(ENV, "DEMAND_OUTPUT_PATH", joinpath(out_dir, "choice_coefficients_structural.csv"))
    filter_assisted = parse(Int, get(ENV, "DEMAND_FILTER_ASSISTED", "-1"))

    println("=== Demand estimation (Julia v3) ===")
    println("  V_0 = β'X_0 (NOT 0)")
    println("  Weights normalized to mean 1 globally")
    println("  Optim.jl LBFGS with cell-by-cell accumulator")
    println("  TEMP_DIR = $temp_dir")
    println("  CELL_DIR = $cell_dir")
    println("  SPEC = $spec_path")
    println("  OUTPUT = $out_path")
    println("  FILTER_ASSISTED = $filter_assisted")

    # --- Pooled model ---
    println("\n--- Loading data ---")
    covars = load_spec(spec_path)
    K = length(covars)
    println("  Covariates: $K terms")

    cells, total_hh = load_cells(cell_dir, covars; filter_assisted=filter_assisted)
    normalize_weights!(cells)
    GC.gc(); GC.gc()  # reclaim DataFrame/CSV.jl memory from loading

    # Move X matrices out of Julia heap into memory-mapped files.
    # Reduces Julia memory from ~12GB to <1GB; OS pages data as needed.
    cells = offload_to_mmap(cells)
    GC.gc(); GC.gc()  # reclaim old heap X matrices

    # MNL starting values: load from cache if available, otherwise estimate
    mnl_cache = joinpath(temp_dir, "mnl_starting_values.csv")
    if isfile(mnl_cache)
        println("\n  Loading cached MNL starting values...")
        mnl_df = CSV.read(mnl_cache, DataFrame)
        θ_start = mnl_df.estimate
        @printf("  Loaded %d parameters, β₁ = %.6f\n", length(θ_start), θ_start[1])
    else
        println("\n  MNL starting values (λ=1 fixed)...")
        β_start, _ = optimize_beta(cells, K, zeros(K), 1.0; verbose=true)
        θ_start = vcat(β_start, 1.0)
        # Cache for future runs
        mnl_df = DataFrame(term = vcat(covars, "lambda"), estimate = θ_start)
        CSV.write(mnl_cache, mnl_df)
        println("  MNL starting values cached to $mnl_cache")
    end

    # Optim.jl LBFGS with diagnostic logging
    # Shared-state cache: obj() computes negLL+grad together, caches result.
    # When grad!() is called at the same θ, it returns the cached gradient
    # instead of recomputing. obj() skips gradient when θ differs from cache
    # (pure line search probe).
    println("\n  Optim.jl LBFGS...")
    logfile = joinpath(temp_dir, "demand_optim_log.csv")
    open(logfile, "w") do f
        println(f, "eval,type,negll,grad_norm,lambda,beta_premium,timestamp")
    end
    eval_count = Ref(0)
    cache_θ = Float64[]          # last θ where gradient was computed
    cache_nll = Ref(0.0)
    cache_grad = zeros(K + 1)

    function obj(θ)
        θ_c = copy(θ); θ_c[K+1] = clamp(θ_c[K+1], 0.001, 5.0)
        # negLL-only: skip gradient computation (~40% faster)
        nll, _, _ = accumulate(θ_c, cells; compute_grad=false)
        eval_count[] += 1
        if !isfinite(nll)
            @warn "Non-finite NLL at eval $(eval_count[]): $nll, λ=$(θ_c[K+1])"
        end
        open(logfile, "a") do f
            @printf(f, "%d,obj,%.6f,NA,%.6f,%.8f,%s\n",
                    eval_count[], nll, θ_c[K+1], θ_c[1],
                    Dates.format(Dates.now(), "HH:MM:SS"))
        end
        return nll
    end
    function grad!(G, θ)
        θ_c = copy(θ); θ_c[K+1] = clamp(θ_c[K+1], 0.001, 5.0)
        # Check cache: if obj() was just called at this θ, reuse
        if θ_c == cache_θ
            G .= cache_grad
        else
            nll, g, _ = accumulate(θ_c, cells; compute_grad=true)
            G .= g
            cache_θ = copy(θ_c)
            cache_nll[] = nll
            cache_grad .= g
        end
        gnorm = sqrt(sum(G.^2))
        if !isfinite(gnorm) || any(!isfinite, G)
            @warn "Non-finite gradient at eval $(eval_count[]): norm=$gnorm, λ=$(θ_c[K+1])"
        end
        open(logfile, "a") do f
            @printf(f, "%d,grad,NA,%.6e,%.6f,%.8f,%s\n",
                    eval_count[], gnorm, θ_c[K+1], θ_c[1],
                    Dates.format(Dates.now(), "HH:MM:SS"))
        end
    end

    result = optimize(obj, grad!, θ_start, LBFGS(),
                       Optim.Options(iterations=2000, g_tol=1e-8, f_reltol=1e-10,
                                     show_trace=true, show_every=10))
    θ_opt = Optim.minimizer(result)
    negll = Optim.minimum(result)
    println("  Log written to $logfile")

    # Save
    coefs = DataFrame(term = vcat(covars, "lambda"), estimate = θ_opt)
    CSV.write(out_path, coefs)

    @printf("\n  Done: negLL = %.2f  λ = %.4f  converged = %s\n",
            negll, θ_opt[K+1], Optim.converged(result))
    println("\n  Coefficients:")
    for (k, name) in enumerate(covars)
        @printf("    %-25s = %12.6f\n", name, θ_opt[k])
    end
    @printf("    %-25s = %12.6f\n", "lambda", θ_opt[K+1])

    # Headline: commission-premium ratio and assisted effects (structural only)
    comm_idx = findfirst(==("commission_broker"), covars)
    prem_idx = findfirst(==("premium"), covars)
    if comm_idx !== nothing && prem_idx !== nothing
        β_p = θ_opt[prem_idx]
        β_c = θ_opt[comm_idx]
        if abs(β_p) > 1e-10
            @printf("\n  β_commission / |β_premium| = %.4f\n", β_c / abs(β_p))
            @printf("  Interpretation: \$1 commission ≈ \$%.2f premium equivalent for broker HH\n",
                    β_c / abs(β_p))
        end
    end
    for m in ["assisted_silver", "assisted_bronze", "assisted_gold", "assisted_plat"]
        idx = findfirst(==(m), covars)
        if idx !== nothing
            @printf("  %s = %.6f\n", m, θ_opt[idx])
        end
    end

    println("  -> $out_path")
    println("\nDone.")
end

# Single-cell test mode: julia estimate_demand_v3.jl test
if length(ARGS) > 0 && ARGS[1] == "test"
    println("=== Single-cell test (region 15, year 2017, all HH) ===")
    temp_dir = get(ENV, "TEMP_DIR", "data/output")
    covars = load_spec(joinpath(temp_dir, "demand_spec.csv"))
    K = length(covars)

    f = joinpath(temp_dir, "choice_cells", "cell_15_2017_data.csv")
    df = CSV.read(f, DataFrame)
    println("  Rows: ", nrow(df), "  HH: ", length(unique(df.household_number)))

    cell = build_cell_data(df, covars)
    cells = [cell]
    normalize_weights!(cells)

    # MNL starting values + Optim.jl LBFGS
    β_start, _ = optimize_beta(cells, K, zeros(K), 1.0; verbose=true)
    θ_start = vcat(β_start, 1.0)

    function obj_test(θ)
        θ_c = copy(θ); θ_c[K+1] = clamp(θ_c[K+1], 0.001, 5.0)
        nll, _, _ = accumulate(θ_c, cells; compute_grad=false)
        return nll
    end
    function grad_test!(G, θ)
        θ_c = copy(θ); θ_c[K+1] = clamp(θ_c[K+1], 0.001, 5.0)
        _, g, _ = accumulate(θ_c, cells; compute_grad=true)
        G .= g
    end

    result = optimize(obj_test, grad_test!, θ_start, LBFGS(),
                       Optim.Options(iterations=2000, g_tol=1e-6,
                                     show_trace=true, show_every=5))
    θ_opt = Optim.minimizer(result)
    negll = Optim.minimum(result)

    println("\n=== Results ===")
    @printf("  negLL  = %.4f  converged = %s\n", negll, Optim.converged(result))
    @printf("  λ      = %.6f\n", θ_opt[K+1])
    for (k, name) in enumerate(covars)
        @printf("  %-25s = %12.6f\n", name, θ_opt[k])
    end
else
    main()
end
