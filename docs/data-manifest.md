# Data Manifest

Maps every input file to its source, location, and consuming script. Files are accessed via symlinks in `data/input/` pointing to the central `research-data/` store.

---

## Covered California (FOIA + public)

Symlink: `data/input/Covered California/` -> `research-data/Covered California/`

| File | Source | Used By | Description |
|------|--------|---------|-------------|
| `pra_07192019.csv` | FOIA from Covered California | `process.COVCAL.data.R` | Primary enrollment data, 2014-2019. ~8.3M household-year obs. Includes pathway variable (navigator/agent/broker/unassisted). |
| `plan_data.csv` | Covered California website, manually compiled | `process.COVCAL.data.R`, `prepare.demand.data.R` | Plan characteristics by year and rating area (metal tier, premium, deductible, insurer, network type). |
| `product_definitions.csv` | Manual | `process.COVCAL.data.R` | Column definitions for `zip3_choices.csv`. |
| `zip3_choices.csv` | Covered California website, manually compiled (2017-2019 downloaded, 2014-2016 FOIA) | `process.COVCAL.data.R` | Choice set by 3-digit ZIP and rating area. |
| `age_rating_factors.csv` | CCIIO/CMS (default federal age curve) | `process.COVCAL.data.R` | Age rating factors used to compute age-adjusted premiums. |
| `poverty_guidelines.csv` | HHS poverty guidelines | `process.COVCAL.data.R` | Federal poverty level thresholds by household size and year. |
| `contribution_percentages.csv` | ACA statute / IRS guidance | `process.COVCAL.data.R` | Required premium contribution as % of income by FPL bracket. |
| `rating_areas.csv` | Covered California | `process.COVCAL.data.R` | Mapping of California counties to rating areas. |
| `ra_reins.csv` | CMS PDFs, manually extracted | `process.rate.data.R` | Risk adjustment and reinsurance payments by HIOS issuer and year. |
| `2014-2020.RData` | CMS rate filing PUFs, merged by "Sam" (method unknown) | `process.rate.data.R` | Compiled rate filing data 2014-2020. **Origin unclear; may need reconstruction.** |
| `commission_input.csv` | Kevin Knauss (InsureMeKevin.com), recovered from Evan's OneDrive | `prepare.supply.data.R`, `run.make.data.objects.small.R` | Broker commission schedules by insurer/network/type, 2014-2020. 29 rows, mixed PMPM and percentage units. |

### County files (`Counties/` subdirectory)

| File | Source | Used By | Description |
|------|--------|---------|-------------|
| `counties_2014.csv` | Covered California | `prepare.demand.data.R` | Insurer entry/exit and county-rating area mapping, 2014. |
| `counties_2015.csv` | " | " | Same for 2015. |
| `counties_2016.csv` | " | " | Same for 2016. |
| `counties_2017.csv` | " | " | Same for 2017. |
| `counties_2018.csv` | " | " | Same for 2018. |
| `counties_2019.csv` | " | " | Same for 2019. |

---

## SIPP Data (Census Bureau)

Symlink: `data/input/SIPP Data/` -> `research-data/SIPP Data/`

### 2008 SIPP Panel (for unauthorized immigrant and employer offer imputation)

| File | Source | Used By | Description |
|------|--------|---------|-------------|
| `p08putm2.dat` | Census Bureau 2008 SIPP | `process.SIPP.R` | Topical Module 2: immigration and citizenship. Also available as `.zip`. |
| `p08putm6.dat` | Census Bureau 2008 SIPP | `process.SIPP.R` | Topical Module 6: employer-provided health insurance. Also available as `.zip`. |
| `l08puw2.zip` | Census Bureau 2008 SIPP | `process.SIPP.R` | Core wave 2 data. |
| `l08puw6.zip` | Census Bureau 2008 SIPP | `process.SIPP.R` | Core wave 6 data. |
| `inputSIPP_core.sas` | Census Bureau | `process.SIPP.R` | SAS layout for core wave fixed-width parsing. |
| `inputSIPP_tm2.sas` | Census Bureau | `process.SIPP.R` | SAS layout for Topical Module 2 fixed-width parsing. |
| `inputSIPP_tm6.sas` | Census Bureau | `process.SIPP.R` | SAS layout for Topical Module 6 fixed-width parsing. |

### 2014 SIPP Panel (for outside-option prediction)

| File | Source | Used By | Description |
|------|--------|---------|-------------|
| `pu2014w1.csv` | Census Bureau 2014 SIPP | `impute.SIPP.R` | Wave 1. |
| `pu2014w2.csv` | Census Bureau 2014 SIPP | `impute.SIPP.R` | Wave 2. |
| `pu2014w3.csv` | Census Bureau 2014 SIPP | `impute.SIPP.R` | Wave 3. |
| `status2013.csv` | Census Bureau 2014 SIPP | `impute.SIPP.R` | Annual insurance status, 2013. |
| `status2014.csv` | Census Bureau 2014 SIPP | `impute.SIPP.R` | Annual insurance status, 2014. |
| `status2015.csv` | Census Bureau 2014 SIPP | `impute.SIPP.R` | Annual insurance status, 2015. |

### SAS Layouts for ACS/IPUMS parsing

| File | Source | Used By | Description |
|------|--------|---------|-------------|
| `input2014.sas` | IPUMS | `process.SIPP.R` | SAS layout for IPUMS ACS extract (SIPP imputation step). |
| `input20142.sas` | IPUMS | `process.SIPP.R` | Alternate SAS layout (different variable set). |
| `input20143.sas` | IPUMS | `prepare.demand.data.R` | SAS layout for ACS extract used in demand data assembly. |

---

## ACS Data (IPUMS / Census Bureau)

Symlink: `data/input/ACS Data/` -> `research-data/ACS Data/`

The ACS Data directory is large (many years, all states). Only a small subset is used by this project.

| File | Source | Used By | Description |
|------|--------|---------|-------------|
| `usa_00011.dat.gz` | IPUMS USA | `prepare.demand.data.R` | ACS microdata extract (California), fixed-width gzipped. Parsed via `input20143.sas` layout. |
| `ipumsCA.gz` | IPUMS USA | `process.SIPP.R` | California ACS extract for SIPP imputation. Parsed via `input20142.sas` layout. |
| `PUMAs.csv` | Census Bureau 2010 PUMA equivalency + rating_areas.csv | `prepare.demand.data.R` | California PUMA-to-county-to-rating-area crosswalk (265 PUMAs). Built via `scratch/build_pumas_csv.py`. |
| `sahie_2014.csv` | Census SAHIE program | `prepare.demand.data.R` | Small Area Health Insurance Estimates, 2014. County-level uninsured rates for California. |

---

## Medical Loss Ratio Data (CMS)

Symlink: `data/input/Medical Loss Ratio Data/` -> `research-data/Insurance Market Data/Medical Loss Ratio Data/`

Years 2014-2018, extracted from CMS MLR PUFs. Each year has the same file structure. Years 2011-2015 also present as `.zip` archives.

### Per-year files used by `process.MLR.data.R`

| File | Description |
|------|-------------|
| `{year}/MR_Submission_Template_Header.csv` | Issuer identification and filing metadata. |
| `{year}/Part1_2_Summary_Data_Premium_Claims.csv` | Premium revenue, claims incurred, enrollment counts. |
| `{year}/Part3_MLR_Rebate_Calculation.csv` | MLR calculation and rebate amounts. |

Additional files present but not used by the pipeline: `MR_Submission_Row_Lookup.csv`, `MR_Submission_Sheet_Lookup.csv`, `Part4_Rebate_Disbursement.csv`, `Part5_Additional_Responses1.csv`, `Part5_Additional_Responses2.csv`, `Part6_Expense_Allocation.csv`.

---

## Missing Files (referenced in code but not in data store)

| File | Referenced By | Source | Status |
|------|---------------|--------|--------|
| `commission_input.csv` | `prepare.supply.data.R`, `run.make.data.objects.small.R` | Kevin Knauss (InsureMeKevin.com) | **Recovered** from Evan's OneDrive archive. Copied to `research-data/Covered California/`. 29 rows: 12 insurers x new/renewal, 2014-2020. |
| `PUMAs.csv` | `prepare.demand.data.R` | Evan's OneDrive archive (`ca_PUMAs.csv`) | **Recovered.** 265 CA PUMAs with multi-county listings and correct LA region 15/16 split. Replaced initial reconstruction. |
| `paths.R` | All old scripts via `source()` | Manual (set absolute paths) | **Intentionally excluded.** Rebuild uses relative paths. |
| `common_functions.R` | Unknown old scripts | Manual | **Intentionally excluded.** Contents unknown; likely helper functions absorbed into main scripts. |

---

## Intermediate Outputs (generated by pipeline, not raw inputs)

These are produced during the data build and consumed downstream. Listed for pipeline planning.

| Object | Produced By | Consumed By | Format |
|--------|-------------|-------------|--------|
| `acs_immigration` | `process.SIPP.R` | `prepare.demand.data.R` | RData |
| `acs_emp_offer` | `process.SIPP.R` | `prepare.demand.data.R` | RData |
| `sipp_logit` | `impute.SIPP.R` | `prepare.demand.data.R` | RData |
| `enroll_temp` | `process.COVCAL.data.R` | `prepare.demand.data.R` | RData |
| `household_temp` | `process.COVCAL.data.R` | `prepare.demand.data.R` | RData |
| `family_enroll_temp` | `process.COVCAL.data.R` | `prepare.demand.data.R` | RData |
| `enroll_data` | `prepare.demand.data.R` | `decision-analysis.R` | RData |
| `household_data` | `prepare.demand.data.R` | `decision-analysis.R`, `run.make.data.objects.small.R` | RData |
| `plan_data` (processed) | `prepare.demand.data.R` | `decision-analysis.R`, `run.make.data.objects.small.R` | RData |
| `dynamic_choices` | `prepare.demand.data.R` | `run.make.data.objects.small.R` | RData |
| `mlr_data.csv` | `process.MLR.data.R` | `prepare.supply.data.R` | CSV |
| `convergence.csv` | `process.rate.data.R` | `prepare.supply.data.R` | CSV |
| `ra_by_metal_year.csv` | `process.rate.data.R` | `prepare.supply.data.R` | CSV |
| `moments_data.csv` | `prepare.supply.data.R` | Structural model | CSV |

**Note on format change:** The old pipeline saved intermediates as `.RData`. The rebuild will save as `.csv` per project conventions.
