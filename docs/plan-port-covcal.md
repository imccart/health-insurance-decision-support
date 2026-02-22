# Plan: Port `process.COVCAL.data.R`

## Context

The original `process.COVCAL.data.R` is a 2900-line base R script that processes ~9.1M individual enrollment records from Covered California into clean enrollment and household datasets. It's the most complex script in the pipeline and the first data-build step. The rewrite converts it to tidyverse, uses joins instead of nested loops, and splits into 5 scripts orchestrated by a master runner.

## File Structure

```
code/
  0-setup.R                                (exists, ~25 lines)
  data-build/
    _data-build.R                          (master runner: sources 0-setup.R, then runs 1_ through 5_)
    _helpers-enrollment.R                  (~50 lines)  only truly duplicated logic
    1_load-clean-enrollment.R              (~180 lines)
    2_validate-geography-income.R          (~120 lines)
    3_construct-households.R               (~100 lines)
    4_reconcile-income.R                   (~300 lines)
    5_assemble-enrollment.R                (~200 lines)
```

`_data-build.R` is the single entry point. Open it, run it, and the whole pipeline executes:
```r
source("code/0-setup.R")
source("code/data-build/_helpers-enrollment.R")
source("code/data-build/1_load-clean-enrollment.R")
source("code/data-build/2_validate-geography-income.R")
source("code/data-build/3_construct-households.R")
source("code/data-build/4_reconcile-income.R")
source("code/data-build/5_assemble-enrollment.R")
```

Individual scripts can still be run standalone for debugging (each sources `0-setup.R` itself).

## Outputs

- `data/output/enrollment_clean.csv` — individual-level, ~20 columns (household_id, demographics, plan choice, FPL, rating_factor, service_channel, flags)
- `data/output/households_clean.csv` — household-level, ~15 columns (members, premiums, FPL, subsidy, SLC_contribution, geography)

## Helper Functions (`_helpers-enrollment.R`, ~50 lines)

Keep this minimal. Only extract functions that are **called from multiple scripts** or that appear **identically twice** within a script:

| Function | Why it's here |
|----------|---------------|
| `invert_aca_subsidy()` | Quadratic FPL inversion formula. Used identically for both singles (script 4) and families (script 4). ~15 lines. |
| `calculate_aca_contribution()` | Forward direction of the subsidy formula. Used for verification in scripts 4 and 5. ~15 lines. |
| `compute_slc_premium()` | Second-lowest silver premium lookup. Used for both singles and families in script 4. ~15 lines. |

Everything else stays inline: insurer name standardization is a simple named vector, rating factor assignment is a one-liner `if_else`, OEP cutoffs are a small tibble. No need to wrap these in functions.

## Script-by-Script Plan

### `1_load-clean-enrollment.R` (~180 lines)

**Original lines 1-471.** Key changes:

- `fread()` for the 9.1M row CSV (10x faster than `read.csv`)
- All variable repairs (gender, metal, insurer, OEP, race, age) become a single `mutate()` chain with `case_when()` / `case_match()`
- Insurer name standardization: a named vector + `recode()`, inline (~5 lines)
- OEP week cutoffs: small inline tibble joined on `year` instead of 6 separate if-blocks
- Rating factor: inline `if_else(year >= 2018, Rating_Factor2018, Rating_Factor)` after joining `age_rating_factors`
- Plan ID creation: join to `plan_data` on (HIOS, metal, year, region) with 2014-2015 HIOS truncation
- Region-fixing for ambiguous plan IDs (lines 201-449, ~250 lines): collapse into a lookup tibble + probabilistic sampling for ambiguous cases
- Preserve `set.seed(6)` for reproducibility
- Save `data/output/enrollment_step1.csv`

### `2_validate-geography-income.R` (~120 lines)

**Original lines 472-762.** Key changes:

- Geographic validation: pivot `zip3_choices` wide→long (product columns → rows), join to enrollment data, flag non-matches. Replaces ~140 lines of loop + if-block with ~25 lines of joins.
- Income validation: FPL bracket assignment via `case_when(between(...))`, ACS plan consistency via `case_when`, >400% FPL handling — all inline.
- Save `data/output/enrollment_step2.csv`

### `3_construct-households.R` (~100 lines)

**Original lines 763-1022.** Key changes:

- `group_by(case_year) %>% summarize(...)` replaces `by()` calls for anomaly detection
- Three-round household splitting: each round is `group_by(household_year, ...) %>% mutate(household_id = cur_group_id())` (~10 lines per round vs ~80 in original)
- Use `data.table` grouping if tidyverse is too slow on 9.1M rows
- Save `data/output/enrollment_step3.csv`

### `4_reconcile-income.R` (~300 lines)

**Original lines 1023-2345. Biggest compression (1300→300 lines).**

The original has nearly-identical 560-line blocks for singles and families. The core algorithm:
1. Compute SLC premium per household (via `compute_slc_premium()` from helpers)
2. Compute contribution bounds for candidate household sizes
3. Use `findInterval()` to find implied income bracket
4. Compare to reported bracket; iterate household sizes
5. Invert subsidy formula (via `invert_aca_subsidy()` from helpers)

**Key simplifications:**
- Pivot `contribution_percentages` and `poverty_guidelines` to long format at the top of the script (~10 lines). This eliminates ~400 lines of per-year column references.
- Singles and families use the same inline reconciliation logic (differ only in starting household size and rating factor summation). No need for a shared function — just two clearly labeled sections that follow the same pattern.
- Unreconciled records: Group 1/2/4 classification via inline `case_when`
- Save `data/output/enrollment_step4.csv` + `data/output/households_step4.csv`

### `5_assemble-enrollment.R` (~200 lines)

**Original lines 2346-2886.** Key changes:

- `left_join()` and `bind_rows()` to merge singles + families back to main enrollment
- Final subsidy verification inline
- ACS plan metal level reassignment: inline `case_when`
- Column selection via `select()` to match downstream interface
- Save final: `data/output/enrollment_clean.csv`, `data/output/households_clean.csv`

## Key Design Decisions

1. **Magic number 1.278** = age-40 rating factor (confirmed from `age_rating_factors.csv`). Named constant `RATING_FACTOR_AGE40 <- 1.278` with a comment.
2. **Intermediate CSVs** between scripts (not RData). Each script reads prior output and writes its own.
3. **`set.seed(6)` preserved** at the same logical points for reproducibility.
4. **Output columns match downstream expectations** (prepare.demand.data.R expects: household_id, household_year, year, individual_id, age, gender, race, plan_id, plan_name, premium21, FPL, rating_factor, OEP, end_week, service_channel, flagged, zip3, region, metal_level_enhanced).
5. **Long-format reference tables** — `contribution_percentages` and `poverty_guidelines` pivoted at the top of script 4. Eliminates per-year column indexing entirely.

## Implementation Order

1. `_helpers-enrollment.R` — 3 small functions, testable in isolation
2. `1_load-clean-enrollment.R` — get loading and cleaning working
3. `2_validate-geography-income.R` — depends on available-plans join
4. `3_construct-households.R` — self-contained household logic
5. `4_reconcile-income.R` — most complex, uses helpers
6. `5_assemble-enrollment.R` — final assembly and export
7. `_data-build.R` — wire it all together

## Verification

After each script, the user runs it in VS Code and we check:
- Row counts match expectations (~9.1M individual records, ~3M household-years)
- Key summary stats (flagged %, subsidy distribution, household size distribution)
- No NA values in required output columns
- Final outputs satisfy the column interface needed by `prepare.demand.data.R`
