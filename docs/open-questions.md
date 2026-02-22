# Open Questions

Consolidated from archaeology docs (`email-findings.md`, `pipeline-map.md`, `overleaf-inventory.md`).

---

## ~~Pending: Co-author's OneDrive archive~~ MINED

Evan Saltzman's OneDrive archive (`Navigators.zip`, 16GB) contained 367 files including code, data, paper, and commission source documents. Key findings:
- `commission_input.csv` found and copied to `research-data/Covered California/`
- `ca_PUMAs.csv` found with better multi-county and LA region data; replaced our reconstruction
- `2014-2020.RData` matches what we already have (identical file size)
- `Commissions/` folder has PDF source documents (Anthem 2017, Blue Shield 2018-2020, Kaiser, Covered CA analysis, Peter Lee letters)
- `demand_estimates.csv` provides validation targets for demand estimation
- `convergence_analysis_updated.csv` has insurer-year financials (premiums, claims, RA, RI)
- `readme.txt` confirms "Sam" did rate filing and SIPP data prep
- Code files are essentially the same as `_old-repo/` (`.nav.` suffix = navigator paper versions)

---

## Critical (blocks pipeline rebuild)

### 1. ~~Commission data (`commission_input.csv`)~~ RESOLVED
- Found in Evan's OneDrive archive; copied to `research-data/Covered California/commission_input.csv`.
- 29 rows: 12 insurers x new/renewal x network type, years 2014-2020.
- Mixed units: some PMPM dollars (Anthem $18, Kaiser $8.33), some percentage-of-premium (Blue Shield 4%, Sharp 5%).
- Source PDFs in `Navigators.zip > Data/Commissions/` (Anthem 2017, Blue Shield 2018-2020, Kaiser, Peter Lee letters).

### 2. ~~Rate filing merge (`2014-2020.RData`)~~ RESOLVED
- Contains 7 tibbles (`PUF_2014` through `PUF_2020`), each from CMS Rate Review PUFs.
- 82-83 columns per year, all states (not just CA). 43K-87K rows per year.
- "Sam" simply downloaded annual CMS PUFs and saved them together. Fully reconstructible from public CMS data.
- Key columns: `ISSUER_ID`, `PLAN_ID`, `STATE`, `COMPANY`, `METAL`, `PLAN_TYPE`, `CUR_RATE_PMPM`, `PRJ_MM`, claims/premium/RA fields.
- The pipeline code (`process.rate.data.R`) filters to California and extracts what it needs.

### 3. ~~PUMAs.csv~~ RESOLVED
- Replaced by Evan's `ca_PUMAs.csv` from OneDrive archive (has multi-county listings and correct LA region 15/16 split).
- 265 California PUMAs with columns: STATEFP, PUMA5CE, RATING_AREA, COUNTY, PUMA NAME.
- Our initial reconstruction via `scratch/build_pumas_csv.py` had 48 mismatches (all LA region defaults and multi-county simplifications).

---

## Important (affects analysis quality)

### 4. ACS data duplication (2018-2019)
- `prepare.demand.data.R` duplicates 2017 ACS data for 2018-2019 instead of using actual survey years.
- The 2018 and 2019 ACS IPUMS extracts may be available; if so, they should be used instead.
- **Action needed:** Check if actual 2018-2019 ACS microdata can be obtained from IPUMS.

### 5. Plan name matching (5,994 dropped households)
- The BS_SIL87 vs. BS_SIL2 mismatch in region 1, 2014 caused 5,994 households to be dropped during the `Plan_Name2` merge.
- Evan acknowledged this in May 2020 emails but no fix was implemented.
- **Action needed:** Investigate whether this is a genuine mismatch (wrong plan code in enrollment data) or a fixable mapping issue. 5,994 is ~0.07% of 8.3M household-years, so materiality is low, but it should be documented.

### 6. Bonus/incentive programs
- Kevin Knauss noted that insurers (Blue Shield, Health Net, Oscar, Molina) offer volume bonuses beyond base commissions (typically 25-100 new enrollment minimums).
- These are not captured in `commission_input.csv`.
- **Action needed:** Assess whether these bonuses are material. If they primarily reward volume (not specific plan steering), they may not bias the steering estimates. Worth a footnote at minimum.

### 7. Off-exchange enrollment
- Data covers on-exchange only. Kevin noted off-exchange commission structures differ (e.g., Western Health Advantage paid more for off-exchange).
- Sutter Health Plus is off-exchange only (Bay Area and Sacramento).
- **Action needed:** Document as a sample restriction. The research question is about marketplace steering, so on-exchange-only is likely the correct sample, but should be explicit.

---

## Low Priority (nice to have)

### 8. Supply-side structural model (Section 6.3)
- Model and estimation methodology are fully written in the paper, but welfare results are completely empty.
- This is not a "question" per se, but the biggest remaining analytical gap.
- **Action needed:** After demand estimation is running, implement the GMM estimation in R (replacing the planned Julia implementation).

### 9. Agent/navigator directory data
- Funding proposal mentions needing agent/navigator directory data via web-scraping or PRA request from Covered California.
- **Status:** No evidence this was ever obtained.
- **Action needed:** Determine if this was for the current paper or a future extension. If not needed for core results, deprioritize.

### 10. `paths.R` and `common_functions.R`
- Both referenced in old code but not in the repo.
- `paths.R` set absolute paths (Evan's Linux paths); the rebuild won't use it.
- `common_functions.R` is sourced but its contents are unknown.
- **Action needed:** The rebuild replaces `paths.R` with relative paths. For `common_functions.R`, grep the old code for any function calls that don't resolve to known packages; if none found, ignore.

### 11. Two table files with nested `\begin{table}` bugs
- `dominated_choice_regression.tex` and `switching_regression.tex` in `_overleaf/tables/` have wrapped `\begin{table}[H]` environments that conflict with the paper's own table wrappers.
- **Action needed:** When regenerating these tables in the new pipeline, output bare `\begin{tabular}` only (consistent with `summary_stats.tex`).
