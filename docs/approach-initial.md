# Pre-refactor approach (snapshot at commit `48dd40e`, 2026-04-21)

This document captures the state of the data-build and demand pipeline
**immediately before** the plan-id refactor of 2026-04-22. It is meant to be
consulted whenever someone asks "what were we doing before?" or needs to
understand a pre-refactor result, intermediate, or coefficient.

Conventions:
- File citations use `48dd40e:<path>:<line>` notation.
- Every code block is verbatim from commit `48dd40e` unless explicitly
  marked as a paraphrase.
- "Pre-refactor" = state at `48dd40e`. "Refactor" = post-`26d3e11`.

---

## 1. Pipeline overview

### Data build (`code/data-build/`, 7 numbered scripts)

Orchestrated by `48dd40e:code/data-build/_data-build.R`. Reference files
(plan_data, product_definitions, zip3_choices, rating_areas,
age_rating_factors, poverty_guidelines, contribution_percentages) are loaded
once at the top and visible to all sourced scripts.

| Step | File | Inputs | Outputs |
|------|------|--------|---------|
| 1 | `1_clean-enrollment.R` | `pra_07192019.csv` (raw CC); ref tables | `enrollment_individual.csv` |
| 2 | `2_aggregate-to-hh.R` | step 1; `plan_data`, `age_rating_factors`, `poverty_guidelines_long` | `enrollment_hh.csv`, `enrollment_individual.csv` (overwritten with `household_year` key, `rating_factor`, `premiumSLC`) |
| 3 | `3_process-sipp.R` | SIPP 2008 TM2/TM6 + core W2/W6 | `sipp_immigration_logit.rds`, `sipp_emp_offer_logit.rds` |
| 4 | `4_build-acs.R` | ACS microdata (`usa_00011.dat.gz` 2014-17, `ipums-acs-2018-2019.dat.gz`); `PUMAs.csv`; counties; SIPP logits | `acs_individuals.csv`, `acs_households.csv`, `income_model_coefs.csv`, `income_distribution.csv` |
| 5 | `5_merge-and-finalize.R` | step 2 CC HH/ind, step 4 ACS HH/ind, income model coefs | `demand_households.csv`, `demand_individuals.csv` |
| 6 | `6_broker-density.R` | `RatingRegionAgentEnrollment_..._20260316.xlsx` | `broker_density.csv` |
| 7 | `7_rate-filings.R` | `2014-2020.RData` (CMS rate filing PUFs) | `rate_filing_rsdata.csv` |

### Analysis (`code/analysis/`)

Orchestrated by `48dd40e:code/analysis/_analysis.R`:

```r
TEMP_DIR    <- "D:/temp-research-data/health-insurance-decision-support"
SAMPLE_FRAC <- 0.02
MASTER_SEED <- 20260224
Sys.setenv(TEMP_DIR=TEMP_DIR, SAMPLE_FRAC=SAMPLE_FRAC, MASTER_SEED=MASTER_SEED)

source("code/0-setup.R")
source("code/data-build/_helpers.R")
source("code/analysis/helpers/constants.R")
source("code/analysis/helpers/covariates.R")
source("code/analysis/helpers/choice.R")
source("code/analysis/helpers/supply.R")
source("code/analysis/helpers/ra.R")
source("code/analysis/helpers/estimate_demand.R")
source("code/analysis/helpers/cf_worker.R")
source("code/analysis/S2-demand-specs.R")     # NB: sourced (defines no globals;
                                              #  contains the entire S2 pipeline)

source("code/analysis/1_decision-analysis.R") # → hh_full, hh_ins
source("code/analysis/2_ipw.R")               # → ipweights.csv
source("code/analysis/3_summary-stats.R")
source("code/analysis/reduced-form/_reduced-form.R")
source("code/analysis/structural/_structural.R")
source("code/analysis/4_paper-results.R")
```

`1_decision-analysis.R` reads `data/output/demand_households.csv` from step
5, joins `plan_data` to fill in plan attributes via `plan_unique_id`, derives
analysis variables, and writes `data/output/hh_full.csv` and
`data/output/hh_ins.csv`. Both reduced-form and structural pipelines reread
those CSVs.

### Critical gap discovered while documenting this

Several columns the analysis depends on (`penalty`, `cheapest_premium`,
`plan_number_nocsr`, `previous_plan_number`, `cutoff` upstream of being
re-attached) are **NOT** computed by any active data-build script in commit
`48dd40e`. They exist only in `code/data-build/_archive/` (legacy).
`hh_full.csv` carried them forward only because a stale
`demand_households.csv` from an older pipeline run was sitting in
`data/output/`.

Specifically:
- `penalty`: only defined in `48dd40e:code/data-build/_archive/10_finalize-demand.R:214,224`
  (per-year `perc_penalty` constants applied to income). Active step 5 never sets it.
- `cheapest_premium`: only defined in `48dd40e:code/data-build/_archive/10_finalize-demand.R:59`
  and `48dd40e:code/data-build/_archive/7_build-acs-market.R:660-671`.
- `plan_number_nocsr`, `previous_plan_number`: only created in
  `48dd40e:code/data-build/_archive/8_merge-exchange-acs.R` and
  `_archive/9_extend-panel.R`. The active step 2/5 do not produce them.

Step 2's `hh` aggregation (`48dd40e:code/data-build/2_aggregate-to-hh.R:134-185`)
emits `plan_id_HIOS`, `plan_unique_id`, `plan_name`, `metal`,
`metal_level_enhanced` but never `plan_number_nocsr`. Step 5
(`48dd40e:code/data-build/5_merge-and-finalize.R:95-103`) only adds the NA
plan attributes for ACS HHs; it does not add `plan_number_nocsr` to CC HHs.

**Implication**: any clean re-run of the pre-refactor pipeline would have
failed at the analysis stage. The "first successful S2 run" the user
referenced almost certainly relied on an older `demand_households.csv` that
already contained `plan_number_nocsr`/`penalty`/`cheapest_premium`. Treat
pre-refactor coefficients accordingly.

---

## 2. Outside option: how a HH was classified as exercising it

This is the most important question and the answer at `48dd40e` was tangled.

### What the data carry (per HH)

After step 5, an ACS row has `insured = 0`, `plan_unique_id = NA`,
`plan_name = NA`, `metal = NA`, `insurer = NA`. A CC row has `insured = 1`
and a non-NA `plan_*`. (`48dd40e:code/data-build/5_merge-and-finalize.R:88-102`.)

`1_decision-analysis.R` does **not** create an explicit "uninsured choice"
indicator. It just splits into `hh_full` (everyone) and `hh_ins` (the CC
subset, filtered by `insured == 1L`).

### What `build_choice_data` does

`48dd40e:code/analysis/helpers/choice.R:69-107` cross-joins each sampled HH
against the cell's choice set, then appends one synthetic plan row per HH:

```r
uninsured_row <- data.table(
  plan_name = "Uninsured", issuer = "Outside_Option",
  network_type = NA_character_, metal_level = NA_character_,
  metal = NA_character_, premium = NA_real_,
  msp = NA_real_, hsa = NA_real_, cf_resid = 0
)
```

The HH's "current plan" is carried in two columns selected from `hhs_dt`:

```r
hh_slim <- hhs_dt[, .(
  household_id, FPL, subsidized_members, rating_factor,
  hh_plan_number = plan_number_nocsr, hh_plan_name = plan_name,
  ...
)]
```

(`48dd40e:code/analysis/helpers/choice.R:75-83`.)

### How `plan_choice == 1` is set

Two places. First pass (after the cross-join, `48dd40e:choice.R:115-118`):

```r
dt[, plan_choice := fifelse(
  hh_plan_name == plan_name & !is.na(hh_plan_name) & !is.na(hh_plan_number),
  1L, 0L
)]
dt[, insured := max(plan_choice), by = household_id]
```

So an HH is classified as insured **iff** its `plan_name` matches one of the
plans in the cell's choice set AND `plan_number_nocsr` is non-NA. ACS HHs
(plan_name NA, plan_number_nocsr NA) get `insured = 0` for every row.

Second pass after the small-insurer collapse (`48dd40e:choice.R:227-232`):

```r
dt[, plan_choice := fcase(
  plan_choice == 1L & insured == 1L, 1L,
  plan_choice == 0L & insured == 0L
    & plan_name == "Uninsured" & is.na(hh_plan_number), 1L,
  default = 0L
)]
```

This is the line where uninsured HHs are mapped onto the synthetic
"Uninsured" row. The condition is:

- `plan_choice == 0L` (no plan in the choice set matched)
- `insured == 0L` (HH had no match anywhere — driven by either ACS
  `plan_name = NA` or by the HH's plan_name being absent from this cell's
  collapsed choice set)
- `plan_name == "Uninsured"` (we're looking at the synthetic row)
- `is.na(hh_plan_number)` (the HH had no recorded plan_number_nocsr)

### Where the "actual unenrolled CC observations" come in

The user's recollection: pre-refactor used "actual unenrolled CC
observations" as the outside option, not ACS. Let me describe what the code
literally does because the statement is partially right.

Step 5 writes both CC and ACS HHs into `demand_households.csv` with
`source ∈ {"CC","ACS"}` and `insured ∈ {1,0}`
(`48dd40e:code/data-build/5_merge-and-finalize.R:88-102`).

`1_decision-analysis.R` does NOT filter on source. Both CC and ACS HHs flow
into `hh_full`. **However** — and this is the subtle part — the structural
and S2 paths work off `hh_choice.csv`, which is just `hh_full` filtered to
exclude catastrophic plans (`48dd40e:code/analysis/structural/_structural.R:154-167`
and `48dd40e:code/analysis/S2-demand-specs.R:144-160`). So all ACS HHs are
included in the structural cell construction.

But they will ALSO have `plan_number_nocsr = NA` (because step 5 never set
it for ACS, see Section 1 caveat). Inside `build_choice_data`, that means
**any HH with `is.na(plan_number_nocsr)`** — whether truly ACS or a CC HH
that lost its `plan_number_nocsr` somewhere along the way — gets mapped to
the synthetic "Uninsured" plan as its observed choice.

S2-demand-specs amplifies this with the `outside_option` switch
(`48dd40e:code/analysis/S2-demand-specs.R:226-248`):

- `outside_option = "exchange"` — keep all uninsured rows in `hh_choice.csv`,
  use raw weights (this is what the user means by "actual unenrolled CC
  observations" — the SIPP-filtered ACS pool that step 4 built was treated
  as if it were "exchange-eligible non-enrollees"). Specs A1, A2, B1, B2,
  D1, D2 use this.
- `outside_option = "acs_reweight"` — same set of HHs, but uninsured row
  weights are scaled by `acs_uninsured / n_uninsured` per cell to match the
  ACS market-size target (`48dd40e:S2-demand-specs.R:295-323` builds
  `reweight_tab`; rescaling happens at `48dd40e:S2-demand-specs.R:373-385`).
  Specs C1, C2 use this. The reweighting reads
  `data/output/demand_acs_households.csv` separately to compute the ACS
  target — but the actual choice-data observations are still the ones in
  `hh_choice.csv`.

So the correct one-line summary is: pre-refactor, the outside-option
"observations" were CC HHs (and ACS HHs, indistinguishably) whose
`plan_number_nocsr` was NA after step 5. There was no clean separation
between "true ACS uninsured" and "CC HHs who lost their plan_number".

---

## 3. Variable naming (pre-refactor plan-identifier zoo)

| Variable | Type | Source | What it captures |
|---|---|---|---|
| `plan_name` | chr | step 1 (raw `plan_name` from CC), preserved through step 2 (`first(plan_name)`) | Long human-readable string ("Anthem Silver 70 PPO" etc.). Carried into `hh_full`. Inside `build_choice_data` it is overwritten with `Plan_Name2`-style codes via the join to `plan_data`. |
| `Plan_Name2` | chr | `plan_data` (CMS PUF) | Short code ("ANT_SIL", "BS_G3", "KA_BR", "Small_BR", etc.). Used as `plan_name` inside structural/S2 cell-build via `48dd40e:code/analysis/structural/_structural.R:131` and `48dd40e:S2-demand-specs.R:97-99`. |
| `plan_number_nocsr` | int | legacy (`_archive/8_merge-exchange-acs.R`) | Sequential plan integer ID, ignoring CSR variants. Used by `build_choice_data` to detect "this HH chose a plan" (`!is.na(plan_number_nocsr)`). Not produced by active step 5 → effectively comes from a stale CSV. |
| `previous_plan_number` | int | legacy (`_archive/8/9`) | HH's prior-year plan_number_nocsr (panel lag). Not actively used in cell construction at `48dd40e` but selected into `hh_choice.csv` (`48dd40e:S2-demand-specs.R:156`, `_structural.R:163`). |
| `plan_unique_id` | chr | step 2 `hh` aggregation, `48dd40e:2_aggregate-to-hh.R:148`: `paste0(first(HIOS), first(year))` | Concat of HIOS and year, matches `plan_data$HIOSYR`. Used in `1_decision-analysis.R:14-26` to join plan attributes (`insurer`, `network_type`, `metal_level`) onto each CC HH. |
| `plan_id_HIOS` | chr | step 2, `48dd40e:2_aggregate-to-hh.R:147`: `first(HIOS)` | The 10/14-character HIOS plan ID. Carried through but not actively used in the choice-build math. |
| `HIOS` | chr | step 1, `48dd40e:1_clean-enrollment.R` (constructed from `hios_id_16`/`hios_id_14`, with year-and-issuer-specific truncation) | Standardized 10-char (2014-15 non-SHARP) or 14-char (2016+) HIOS. |
| `hios_id_16`, `hios_id_14` | chr | raw / step 1 | The 16- and 14-char raw HIOS IDs. Step 1 keeps both. |
| `metal` | chr | step 1, `48dd40e:1_clean-enrollment.R:46-52` | Base tier ("Bronze"/"Silver"/"Gold"/"Platinum"/"Minimum Coverage"). All Silver-Enhanced flavours collapse to "Silver" here. |
| `metal_level_enhanced` | chr | step 1, `48dd40e:1_clean-enrollment.R:33-44` | CSR-aware tier ("Silver - Enhanced 73/87/94", "Silver", "Bronze", etc.). Filled from `plan_name` if blank. |
| `metal_level` | chr | `plan_data` column | Plan-side CSR-aware tier. Used inside `build_choice_data` for the CSR filter (`48dd40e:choice.R:91-104`). |

The pre-refactor pipeline carried all eleven of these in parallel. The
refactor (`26d3e11`) collapsed them to two — `plan_id` and `metal`.

---

## 4. Premium types — how `premium_oop` was constructed

Defined in `48dd40e:code/analysis/helpers/choice.R:128-160`. Common
preliminaries:

```r
dt[, adj_subsidy := fifelse(is.na(subsidy), 0, subsidy)]
dt[, premium_hh  := (premium / RATING_FACTOR_AGE40) * rating_factor]
```

`premium` here is the cell-mean `Premium` from `plan_data`, which is priced
at age 40 (so divide by `RATING_FACTOR_AGE40 = 1.278` to get age-21 base,
then multiply by the HH's summed `rating_factor` to get the HH's
gross monthly premium). `subsidy` is the HH-level monthly APTC.

Then by type:

```r
if (premium_type == "posted") {
  dt[, premium_oop := fcase(
    issuer == "Outside_Option",  0.0,
    default = premium_hh
  )]
} else if (premium_type == "oop") {
  dt[, premium_oop := fcase(
    issuer == "Outside_Option",  0.0,
    default = pmax(premium_hh - adj_subsidy, 0)
  )]
} else if (premium_type == "net") {
  dt[, premium_oop := fcase(
    issuer == "Outside_Option",  0.0,
    default = pmax(premium_hh - adj_subsidy, 0) - penalty / 12
  )]
} else if (premium_type == "evan") {
  dt[, premium_oop := fcase(
    issuer == "Outside_Option",  penalty / 12,
    default = pmax(premium_hh - adj_subsidy, 0)
  )]
}
```

Note `penalty` here is the HH's annual mandate penalty in dollars, divided
by 12 to get monthly. Final per-HH premium is divided by `hh_size` to get
per-person monthly:

```r
dt[, net_premium := premium_oop / hh_size]   # 48dd40e:choice.R:248
```

(name is misleading — `net_premium` is the model-input premium regardless of
which `premium_type` was selected.)

`penalty_own` is a separate exclusion-restriction term, computed at
`48dd40e:choice.R:311-312`:

```r
dt[, penalty_own := fifelse(plan_name == "Uninsured",
                            penalty / 12 / hh_size, 0)]
```

So `penalty_own` is per-person monthly penalty on the uninsured row, zero on
insured rows. It enters with its own coefficient.

The Group A specs use `premium_type = "net"`, Group B uses `"oop"` (with
`penalty_own` in the spec to identify outside-option utility separately),
Group C uses `"net"` again with ACS reweighting.

Per the comment block at `48dd40e:S2-demand-specs.R:248-251` and the user's
note in `MEMORY.md`, `"net"` and `"evan"` are algebraically identical in
nested logit (penalty is constant within HH; the shift cancels through the
common HH normalization), so Group D and E (planned) were dropped.

---

## 5. Subsidy — CC vs ACS

### CC HHs

Step 2, `48dd40e:code/data-build/2_aggregate-to-hh.R:188`:

```r
hh[, subsidy := pmax(0, gross_premium_amt_int - net_premium_amt_int)]
hh[, SLC_contribution := pmax(0, premiumSLC - subsidy)]
```

So CC subsidy is the observed APTC (gross minus net premium amount,
floored at zero). This is monthly dollars.

### ACS HHs

Step 4 computes the ACA subsidy formula manually
(`48dd40e:code/data-build/4_build-acs.R:284-330`):

1. Determine `subsidy_eligible = (FPL >= 1.38 & FPL <= 4.0)`.
2. Look up `perc_LB`, `perc_UB` (year-specific contribution percentages at
   the FPL bracket bounds), `fpl_LB`, `fpl_UB` (bracket bounds in ratio
   form).
3. Apply `aca_contribution()` (defined in `_helpers.R:33-46`) to get the
   monthly required contribution toward the SLC benchmark.
4. `subsidy = pmax(0, premiumSLC - SLC_contribution)`.

For the 300-400% bracket the formula is linear in income (flat percent); for
all others it's quadratic in FPL (linearly-interpolated percent times FPL).
See `_helpers.R:33-46` and the `bracket_300_400` flag.

### How subsidy enters demand

Inside `build_choice_data`, `adj_subsidy = fifelse(is.na(subsidy), 0, subsidy)`
(`48dd40e:choice.R:139`). It's then subtracted from `premium_hh` for any
non-uninsured plan when `premium_type ∈ {oop, net, evan}` (see Section 4).

**Pre-refactor caveat**: ACS HH `subsidy` produced in step 4 only writes to
`acs_households.csv`. Step 5's `bind_rows` (`48dd40e:5_merge-and-finalize.R:88-102`)
relies on the column being present in both CC and ACS frames. Step 4's
`subsidy` column does flow through; ACS HHs that fail subsidy_eligible get
0; this is fine.

---

## 6. Penalty — when, where, exempt classification

**Pre-refactor active code never computes `penalty`.** It is referenced by
the analysis layer (`48dd40e:choice.R:147,154,190,312`,
`48dd40e:supply.R:163,303`, `_structural.R:163`, `S2-demand-specs.R:157`)
but only defined in `_archive/10_finalize-demand.R:214-224`:

```r
perc_penalty <- c(`2014` = 0.01, `2015` = 0.02, `2016` = 0.025,
                   `2017` = 0.025, `2018` = 0.025, `2019` = 0)
hh <- hh %>% mutate(
  perc_yr = perc_penalty[as.character(year)],
  ...
  penalty = ifelse(flagged, NA_real_,
                   pmax(<flat fee>, perc_yr * (income - filing_threshold)))
)
```

Likewise the legacy script did set `flagged` (i.e., exempt) based on
several conditions (income below filing threshold; lowest available
non-catastrophic premium exceeding the year-specific affordability cutoff
applied to FPL × poverty_threshold; etc.) — see
`_archive/10_finalize-demand.R:160-180` and the `cutoff` lookup
`AFFORD_THRESHOLDS` in `48dd40e:code/analysis/helpers/constants.R:9-13`.

Effectively, penalty values reaching the analysis layer at `48dd40e` come
from a stale `demand_households.csv` produced by an earlier pipeline run.
This is a known gap that needs to be re-implemented in step 5 or a new
step 5b on any clean rebuild.

---

## 7. Weighting — `ipweight` parameter and the `hh_size` override

`build_choice_data` (`48dd40e:code/analysis/helpers/choice.R:14-15`):

```r
build_choice_data <- function(plans, hhs, sample_frac, weight_var = "ipweight",
                              spec = NULL, premium_type = "net") { ... }
```

Default is `weight_var = "ipweight"`. The IPW weight comes from
`ipweights.csv` (joined onto `hh_full` in `_structural.R:53-55` and
`S2-demand-specs.R:60-62`) and is the propensity-score-based ATT weight
estimated in `2_ipw.R:31` as
`if_else(assisted == 1, 1, pred_assist / (1 - pred_assist))`.

Override at `48dd40e:choice.R:265-268`:

```r
# Override weight for structural estimation (household_size instead of IPW)
if (weight_var == "hh_size") {
  dt[, ipweight := as.numeric(hh_size)]
}
```

This means `weight_var = "hh_size"` **overwrites** the `ipweight` column
itself with `hh_size`. After this, every downstream consumer that reads
`ipweight` from the cell CSV (the estimator in `estimate_demand.R`, the
share aggregators in `supply.R`) is using `hh_size` even though the column
is still named `ipweight`.

Both callers in the structural path force `hh_size`:
- `48dd40e:code/analysis/structural/1_demand.R:46`:
  `cd <- build_choice_data(plans, hhs, SAMPLE_FRAC, weight_var = "hh_size", ...)`
- `48dd40e:code/analysis/S2-demand-specs.R:354-355`:
  `cd <- build_choice_data(plans, hhs, SAMPLE_FRAC, weight_var = "hh_size", ...)`

The reduced-form path is the only place that uses the IPW default
(`48dd40e:code/analysis/reduced-form/_reduced-form.R` calls
`build_choice_data` without `weight_var`).

`build_supply_choice_data` defaults to `weight_var = "hh_size"` already
(`48dd40e:supply.R:11`).

So in practice: structural + S2 weight by `hh_size`. Reduced-form weights by
the IPW. The `weight_var = "ipweight"` default is a foot-gun left over from
when the structural path also used IPW.

---

## 8. Sampling — `sample_frac` semantics

Channel-stratified, **per cell**, before the cross-join
(`48dd40e:code/analysis/helpers/choice.R:21-32`):

```r
untreated_ids <- hhs_dt[channel == "Unassisted", unique(household_id)]
treated_ids   <- hhs_dt[channel != "Unassisted", unique(household_id)]

n_untreated <- max(1L, as.integer(length(untreated_ids) * sample_frac))
n_treated   <- max(1L, as.integer(length(treated_ids) * sample_frac))

keep_ids <- c(
  sample(untreated_ids, n_untreated, replace = FALSE),
  sample(treated_ids,   n_treated,   replace = FALSE)
)
hhs_dt <- hhs_dt[household_id %in% keep_ids]

if (length(unique(hhs_dt$household_id)) < 50) return(NULL)
```

So a `SAMPLE_FRAC = 0.02` cell with 1000 untreated + 200 treated keeps 20
untreated + 4 treated. Each cell is independently subsampled. Cells with
fewer than 50 surviving HH return NULL.

`channel` is derived in `1_decision-analysis.R:38-43`:

```r
channel = if_else(assisted == 1, "Assisted", "Unassisted"),
channel_detail = case_when(
  navigator == 1            ~ "Navigator",
  broker == 1 | agent == 1  ~ "Agent",
  TRUE                      ~ "Unassisted"
)
```

Cell seeds: `set.seed(MASTER_SEED)`, then `cell_seeds <- sample.int(1e7, nrow(cells))`.
S2 uses the same scheme (`48dd40e:S2-demand-specs.R:343`). Each cell sets
`set.seed(cell_seeds[j])` before the channel sampling, so the same HH set
appears in the same cell across re-runs (provided `MASTER_SEED` and the cell
order are stable).

---

## 9. CSR-enhanced silver handling

Cross-joined silver-enhanced rows are filtered to only the FPL bracket they
serve (`48dd40e:code/analysis/helpers/choice.R:88-105`):

```r
dt[, `:=`(
  csr_73 = fifelse(FPL > 2 & FPL <= 2.5 & subsidized_members > 0, 1L, 0L),
  csr_87 = fifelse(FPL > 1.5 & FPL <= 2 & subsidized_members > 0, 1L, 0L),
  csr_94 = fifelse(FPL <= 1.5 & subsidized_members > 0, 1L, 0L)
)]
dt <- dt[
  (metal_level == "Silver - Enhanced 73" & csr_73 == 1L) |
  (metal_level == "Silver - Enhanced 87" & csr_87 == 1L) |
  (metal_level == "Silver - Enhanced 94" & csr_94 == 1L) |
  metal != "Silver" |
  (metal_level == "Silver" & csr_73 == 0L & csr_87 == 0L & csr_94 == 0L) |
  is.na(metal)
]
```

Translation:
- Subsidized HH at FPL ≤ 1.5 see Silver-Enhanced 94 (and not base Silver
  or other Silver-Enhanced tiers).
- Subsidized HH at 1.5 < FPL ≤ 2 see Silver-Enhanced 87.
- Subsidized HH at 2 < FPL ≤ 2.5 see Silver-Enhanced 73.
- Unsubsidized HH (and subsidized HH at FPL > 2.5) see only base Silver.
- All HH see all non-Silver tiers.
- The Uninsured synthetic row (metal NA) passes through.

AV is set per metal_level (`48dd40e:choice.R:161-172`):

```r
dt[, av := fcase(
  metal_level == "Minimum Coverage",     0.55,
  metal_level == "Bronze",               0.60,
  metal_level == "Silver",               0.70,
  metal_level == "Gold",                 0.80,
  metal_level == "Platinum",             0.90,
  metal_level == "Silver - Enhanced 73", 0.73,
  metal_level == "Silver - Enhanced 87", 0.87,
  metal_level == "Silver - Enhanced 94", 0.94,
  issuer == "Outside_Option",            0,
  default = NA_real_
)]
```

Then near the very end (`48dd40e:choice.R:303`) the plan name is collapsed:

```r
dt[, plan_name := gsub("SIL(94|73|87)", "SIL", plan_name)]
```

So `ANT_SIL94`, `ANT_SIL87`, `ANT_SIL73` all become `ANT_SIL` for choice
modeling. AV is preserved at the CSR-specific value at the row level (the
HH still sees the actual AV for their bracket), but the *name* used in the
choice probability normalization is the base-Silver name.

This is the key place pre-refactor used both `metal_level_enhanced`
(to compute `av`) and `metal` (to gate the filter). The refactor merges
this into the single `metal` column.

---

## 10. Small insurer collapse

`48dd40e:code/analysis/helpers/choice.R:175-219` (and the parallel
`48dd40e:supply.R:131-180`):

```r
big_four <- c("Anthem", "Blue_Shield", "Kaiser", "Health_Net")

large     <- dt[issuer %in% c(big_four, "Outside_Option")]
small_raw <- dt[!issuer %in% c(big_four, "Outside_Option")]

if (nrow(small_raw) > 0) {
  small <- small_raw[, .(
    premium_oop  = min(premium_oop, na.rm = TRUE),    # cheapest in tier
    plan_choice  = max(plan_choice, na.rm = TRUE),    # any chosen → 1
    FPL          = first(FPL),
    hh_plan_name = first(hh_plan_name),
    hh_plan_number = first(hh_plan_number),
    oldest_member  = first(oldest_member),
    insured        = first(insured),
    penalty        = first(penalty),
    hsa            = mean(hsa, na.rm = TRUE),
    av             = mean(av, na.rm = TRUE),
    cf_resid       = mean(cf_resid, na.rm = TRUE)
  ), by = .(household_id, metal)]
  small[, `:=`(
    issuer = "Small_Insurer",
    plan_name = fcase(
      metal == "Platinum",         "Small_P",
      metal == "Gold",             "Small_G",
      metal == "Silver",           "Small_SIL",
      metal == "Bronze",           "Small_BR",
      metal == "Minimum Coverage", "Small_CAT",
      default = NA_character_
    )
  )]
  dt <- rbind(large, small, fill = TRUE)
}
```

**When**: inside every `build_choice_data` call, after CSR filter +
catastrophic filter + plan_choice indicator + premium math, but before
demographics join.

**By what key**: `(household_id, metal)`. So each HH gets at most one
small-insurer row per base metal tier. The "small" row's premium is the
**minimum** premium across all small-insurer plans in that tier (i.e., the
cheapest small option).

**Naming**: the collapsed plan names are `Small_P`, `Small_G`, `Small_SIL`,
`Small_BR`, `Small_CAT`. (`Small_SIL` mixes whatever Silver-Enhanced flavor
the HH was eligible for — AV is the mean across whatever rows survived the
CSR filter.)

The AV for collapsed `Small_SIL` is the mean over the survivors; if all
small Silver in a cell are CSR-94 it averages to ~0.94, etc. The
post-refactor MEMORY note pins this at 0.70 (base Silver) which differs from
this commit's `mean(av)` behavior — worth flagging.

---

## 11. S2 demand-specs

### Cell selection

`48dd40e:S2-demand-specs.R:226-233`:

```r
TARGET_REGIONS <- c(1L, 4L, 8L, 13L, 16L)
TARGET_YEARS   <- c(2014L, 2016L, 2018L)
TARGET_CELLS   <- expand.grid(region = TARGET_REGIONS, year = TARGET_YEARS)
```

15 cells (5 regions × 3 years).

### Pre-cell data prep

If `plan_choice.csv`, `plan_demographics.csv`, and `hh_choice.csv` already
exist in `TEMP_DIR`, prep is skipped (`48dd40e:S2-demand-specs.R:43-50`).
Otherwise:

1. Join IPW weights onto `hh_full`.
2. Fit first stage `assisted ~ n_agents + FPL + perc_0to17 + ...`, store
   `v_hat` (broker-density IV residual).
3. Fit logit `(channel_detail == "Navigator") ~ FPL + ...` on assisted HHs,
   store `p_nav`.
4. Drop catastrophic plans (`grepl("_CAT$", plan_name) | is.na(plan_name)`).
5. Build `plan_demographics` — weighted-mean demographics per (plan_name,
   year), collapsing CSR-enhanced silver to base.
6. Build `plan_choice` from `plan_data`:
   - Take `Plan_Name2` as `plan_name`.
   - Compute Hausman IV (`(sum(premium) - premium) / (n - 1)` within
     issuer × metal × year).
   - First-stage residual `cf_resid` from `lm(premium ~ hausman_iv + metal +
     network_type + factor(year))`.
   - Join commission lookup on `(insurer_prefix, year)` and compute
     `comm_pmpm`.
7. Write `hh_choice.csv` with the full set of HH columns expected
   downstream (including the legacy `plan_number_nocsr`,
   `previous_plan_number` — see Section 1 caveat).

### Spec building blocks

`48dd40e:S2-demand-specs.R:236-264`:

```r
PLAN_ATTRS <- c("silver", "bronze", "hmo", "hsa")
INSURER_FE <- c("Anthem", "Blue_Shield", "Kaiser", "Health_Net")
ASST_TERMS <- c("assisted_silver", "assisted_bronze",
                "commission_broker", "v_hat_commission")
DEMO_PREM  <- c("hh_size_prem", "perc_0to17_prem", "perc_18to34_prem",
                "perc_35to54_prem", "perc_male_prem", "perc_black_prem",
                "perc_hispanic_prem", "perc_asian_prem", "perc_other_prem",
                "FPL_250to400_prem", "FPL_400plus_prem")
REGION_FE  <- paste0("ra_", TARGET_REGIONS[-1])     # ra_4, ra_8, ra_13, ra_16
DEMO_INSURED <- c("FPL_250to400_insured", "FPL_400plus_insured",
                  "perc_male_insured", "perc_0to17_insured",
                  "perc_18to34_insured", "perc_35to54_insured",
                  "perc_black_insured", "perc_hispanic_insured",
                  "perc_asian_insured", "perc_other_insured")
```

### Spec list (8 specs)

`48dd40e:S2-demand-specs.R:264-307`:

| Spec | premium_type | outside_option | base | asst |
|---|---|---|---|---|
| `A1_net_exch` | net | exchange | premium + PLAN_ATTRS + INSURER_FE | ASST_TERMS |
| `A2_net_exch_dprem` | net | exchange | A1 base + DEMO_PREM | ASST_TERMS |
| `B1_oop_exch` | oop | exchange | premium + penalty_own + PLAN_ATTRS + INSURER_FE | ASST_TERMS |
| `B2_oop_exch_dprem` | oop | exchange | B1 base + DEMO_PREM | ASST_TERMS |
| `C1_net_acsrw` | net | acs_reweight | A1 base | ASST_TERMS |
| `C2_net_acsrw_dprem` | net | acs_reweight | A2 base | ASST_TERMS |
| `D1_net_exch_raFE` | net | exchange | A1 base + REGION_FE | ASST_TERMS |
| `D2_net_exch_raFE_dprem` | net | exchange | A2 base + REGION_FE | ASST_TERMS |

(Groups labeled D and E for `evan` premium type were planned but dropped
because of the `net`/`evan` algebraic equivalence — see Section 4.)

### Per-spec flow

For each spec:
- Build `combo_dir = SENS_DIR/cells_<premium_type>_<outside_option>` if not
  already built. One cell CSV per (region, year) in `TARGET_CELLS`.
- For `acs_reweight`, multiply `ipweight` of uninsured HH rows by the cell's
  `rw_factor = acs_uninsured / n_uninsured` after `build_choice_data`
  returns (`48dd40e:S2-demand-specs.R:373-385`). Note this happens AFTER
  the `weight_var = "hh_size"` override in `build_choice_data` has
  overwritten `ipweight` with `hh_size` — so the reweight scales `hh_size`,
  not the IPW.
- Add insured-only rating-area dummies for this spec
  (`48dd40e:S2-demand-specs.R:367-371`).
- Estimate via `bfgs_bhhh()` from `theta0 = c(rep(0, K), 1.0)` (no warm
  start unless `sp$warm_start` set; none of the 8 specs sets it).
- Save `coefs_<spec>.csv` and a one-line `coefs_<spec>_summary.txt`.
- Skip if both files exist (cache).

### Sampling

`SAMPLE_FRAC` (env var, set in `_analysis.R`) and `MASTER_SEED` are read
fresh. `set.seed(MASTER_SEED); all_seeds <- sample.int(1e7, n_cells)`
(`48dd40e:S2-demand-specs.R:343-344`).

---

## 12. Estimation details

### Optimizer

`48dd40e:code/analysis/helpers/estimate_demand.R:264-356`. Pure-R BFGS-BHHH
loop. Initial Hessian inverse from BHHH (per-HH gradient outer product
summed across cells); BFGS rank-2 update each iteration; halving line
search; lambda kept in `[0.001, 5.0]`. Convergence: `|Δnegll| < 1e-8` or
`|χ²| < 1e-6`.

`optim()` (L-BFGS-B / BFGS / nlminb) all fail on this problem (~28 params,
~161K HH at full sample). See `48dd40e:code/analysis/structural/optimizer.md`.

### Starting values

`theta0 = c(rep(0, K), 1.0)` — zeros for all betas, lambda = 1
(unrestricted multinomial-logit). S2 supports an optional
`sp$warm_start = "<other_spec_name>"` to pull starting values from a saved
coefs CSV (`48dd40e:S2-demand-specs.R:413-426`), but no spec at this commit
sets it.

### Per-cell likelihood structure

`48dd40e:code/analysis/helpers/estimate_demand.R:140-227`. Vectorized:
`X_ins` (insured-row design matrix), `X_0` (per-HH uninsured-row design
matrix), `X_ch` (per-HH chosen-row design matrix). Rowsum-by-HH for nest
inclusive value. Numerically stable log-sum-exp.

V_0 = β'X_0 (NOT zero); weights normalized globally to mean 1
(`normalize_weights` at `48dd40e:estimate_demand.R:114-129`). HH-level
log-likelihood:

```
chose insured: ll = V_chosen / λ + (λ-1) * I_HH - log(exp(λI) + exp(V_0))
chose unins:   ll = V_0 - log(exp(λI) + exp(V_0))
```

where `I_HH = log Σ_j exp(V_ij / λ)` over insured plans `j`.

### Last-known coefficient results referenced by the user

Per the project CLAUDE.md and MEMORY.md notes capturing earlier S2 runs at
some pre-refactor point (not necessarily `48dd40e` exactly — pre-refactor
results predate this snapshot), at simple net-premium specs the team saw
roughly `λ ≈ 0.21` and `β_premium ≈ -0.0007 per $1` (i.e., ~`-0.07 per
$100`), well off Evan's `-0.429 per $100`. The first successful refactor
S2 run jumped β_premium to `-0.71/-0.85/-0.79 per $100` across A1/A2/L2
(closer to Evan's `-0.43`) but with λ blowing up to 1.45–2.10 on a 7,270-HH
sample. These numbers are not reproducible from the `48dd40e` code alone
because the data-build at that commit cannot produce a clean run from
scratch (Section 1 caveat); reading the pre-refactor numbers means reading
whatever state of `data/output/demand_households.csv` was sitting on disk
at the time.

---

## Appendix: things that do NOT change post-refactor

For the avoidance of confusion, the refactor at commit `26d3e11` did NOT
change:

- The optimizer (`bfgs_bhhh` in `estimate_demand.R`).
- The CSR filter logic (only the column names it operates on).
- The premium-type math (`net`/`oop`/`posted`/`evan` formulas).
- The small-insurer collapse rule (still by `(household_id, metal)`).
- The S2 spec list (A1/A2/B1/B2/C1/C2/D1/D2 unchanged).
- The HH-channel sampling rule.
- The IPW vs hh_size weight semantics (still defaults to ipweight in
  `choice.R`, still overridden to hh_size by structural and S2).
- The first-stage broker-density IV.
- The control-function residual injection points.

What the refactor DID change:

- Drops `hios_id_16`, `hios_id_14`, `HIOS`, `plan_id_HIOS`, `plan_unique_id`
  from the schema. Drops the separate `metal_level_enhanced` column;
  `metal` becomes CSR-aware.
- Renames `plan_network_type` → `network_type` everywhere.
- Step 2 introduces a `(HIOS, year, metal) → plan_id` crosswalk via
  `Plan_Name2`, eliminating the `plan_unique_id` round-trip in
  `1_decision-analysis.R`.
- "Insured" detection switches from `!is.na(plan_number_nocsr)` to
  `!is.na(plan_id)` / `insured == 1L`.
- Step 5 sets `agent/broker/navigator = 0L` for ACS HHs (was NA, which made
  `channel = NA` and dropped them silently from sampling).
- Step 4 drops PUMA `6_300` (only multi-RA PUMA, 894 HHs).
- Several data.table grouped aggregations are inlined (no function wrapper)
  to avoid locked-binding errors.
