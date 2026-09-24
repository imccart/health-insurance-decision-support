# Variable lineage

One entry per variable that reaches the paper, whether through a table, a
figure, a summary statistic, an estimate or a counterfactual. Each entry gives
the raw file and field the variable starts from, each transformation in order
with the script that does it, its level and units in final form, and the code
files it appears in. Scripts are named without line numbers; functions are named
where the work happens inside one. Variables built by one rule from a set of
inputs (the demographic interactions, the plan-attribute indicators) are
documented as a family with every member listed.

Data-build scripts are `code/data-build/N_name.R`, sourced in order by
`_data-build.R`, which also loads the reference tables. Analysis scripts are
`code/analysis/*.R`, sourced by `_analysis.R`; helpers are
`code/analysis/helpers/*.R`.

Raw inputs referred to below (all under `data/input/`):

| File | Contents |
|---|---|
| `Covered California/pra_07192019.csv` | Covered California enrollment records, one row per member-year |
| `Covered California/plan_data.csv` | plans by region and year with the 40-year-old premium, metal, network, HSA, HIOS id and short code |
| `Covered California/product_definitions.csv`, `zip3_choices.csv` | which products are sold in each zip3, region and year |
| `Covered California/rating_areas.csv` | county to rating region |
| `Covered California/age_rating_factors.csv` | age rating factor by age, one curve through 2017 and one from 2018 |
| `Covered California/poverty_guidelines.csv` | poverty guideline by family size and year |
| `Covered California/contribution_percentages.csv` | ACA expected-contribution percentage by FPL bracket and year |
| `Covered California/RatingRegionAgentEnrollment_from_CY2014_to_CY2019__20260316.xlsx` | agents and their enrollees by region and year (FOIA) |
| `co-est2019-alldata.csv` | Census county population estimates |
| `SIPP Data/pu2014w1-3.csv`, `status2013-2015.csv` | SIPP 2014 panel, waves 1 to 3 |
| `Covered California/commission_input.csv`, `rate-filings-srrt/*.xlsx`, MLR files | commission and cost side, documented in later sections |

---

## 1. Household identifiers, sample membership and weights

### `household_id`
- Paper: the unit behind every household count and every household-level estimate.
- Raw: `pra_07192019.csv` field `ahbx_case_id_x`.
- Chain: `2_aggregate-to-hh.R` splits a case-year whose members carry different (gross premium, plan, APTC) combinations into separate households and then drops every year of any case with such a split; `household_id` is the case id of the remaining household-years. `4_build-cc-uninsured.R` expands each household to 2014 to 2019 on this id. `build1_decision-analysis.R` lags `insured` within it to build `new_enrollee`. In the cell data it is renamed `household_number` (`choice.R build_rf`, `supply.R build_structural`).
- Level: household, persistent across years.
- Files: `2_aggregate-to-hh.R`, `4_build-cc-uninsured.R`, `5_merge-and-finalize.R`, `build1_decision-analysis.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `sum3_retention.R`.

### `household_year`
- Paper: the join key for the IPW weights; not reported.
- Chain: `2_aggregate-to-hh.R` builds it as case id, year and split number; `4_build-cc-uninsured.R` gives off-year rows split 0. `build2_ipw.R` writes `ipweights.csv` on it and `build3_data-prep.R` joins on it.
- Files: `2_aggregate-to-hh.R`, `4_build-cc-uninsured.R`, `build2_ipw.R`, `build3_data-prep.R`.

### `year`
- Paper: every table and figure by year; the enrollee-count figure.
- Raw: `pra_07192019.csv` field `enrlee_enrlmnt_yr`, renamed in `1_clean-enrollment.R`.
- Chain: off-year rows take their year from the panel expansion in `4_build-cc-uninsured.R` (2014 to 2019). Region-year pairs define the estimation cells in `build3_data-prep.R`, `rf2_choice-att.R` and `s2_demand.R`. Supply and counterfactual steps use 2014 to 2018 (`SUPPLY_YEARS`).
- Files: every data-build and analysis script.

### `region`
- Paper: the market in every cell-level estimate; rating regions 1 to 19.
- Raw: `pra_07192019.csv` field `region`.
- Chain: `1_clean-enrollment.R` uses it in the plan-availability check; `2_aggregate-to-hh.R` takes the household's value and drops household-years whose members disagree on it; `4_build-cc-uninsured.R` copies it from the reference year. `6_broker-density.R` maps counties to regions with `rating_areas.csv` for the agent-density panel.
- Files: `1_clean-enrollment.R`, `2_aggregate-to-hh.R`, `4_build-cc-uninsured.R`, `5_merge-and-finalize.R`, `6_broker-density.R`, `7_rate-filings.R`, `8_risk-scores.R`, all cell-based analysis scripts and helpers.

### `insured`
- Paper: the enrollment margin; the summary statistics and reduced-form samples condition on it.
- Chain: `5_merge-and-finalize.R` sets 1 on rows from `enrollment_hh.csv` and 0 on the off-year rows from `cc_uninsured.csv`. `choice.R build_rf` and `supply.R build_structural` rebuild it inside a cell as the maximum of the plan-choice indicator over the household's rows, which reproduces the same value.
- Level: household-year, 0/1.
- Files: `5_merge-and-finalize.R`, `build1_decision-analysis.R`, `build2_ipw.R`, `build3_data-prep.R`, `10_commissions.R`, `choice.R`, `supply.R`, `sum1_desc-stats.R`, `rf2_choice-att.R`, `rf4_first-stage.R`, `s2_demand.R`, `estimate_demand.R`.

### `new_enrollee`
- Paper: the new-enrollee comparison samples in the supplemental appendix; a control in the dominated-choice regressions.
- Chain: `build1_decision-analysis.R` orders rows by household and year over the full panel, including the off-year rows the SIPP draw marks ineligible, and sets `new_enrollee = 1` when `insured == 1` and the prior year's `insured` (0 when there is no prior row) is 0. The market-eligibility filter is applied after this lag so a gap year counts as uninsured.
- Level: household-year, 0/1.
- Files: `build1_decision-analysis.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `sum1_desc-stats.R`, `rf1_dominated.R`, `rf2_choice-att.R`.

### `market_eligible`
- Paper: defines which off-year household-years count as uninsured in the market.
- Chain: `3_process-sipp.R` fits a logit of leaving or entering the individual market on FPL bracket, household size and the age, sex and race shares from the SIPP 2014 panel (`transitioned`). `4_build-cc-uninsured.R` predicts it for every off-year row and draws `market_eligible = 1` with probability one minus the prediction (seed 20260423). `5_merge-and-finalize.R` sets 1 on enrolled rows. `build1_decision-analysis.R` keeps rows with `market_eligible == 1` after computing `new_enrollee`.
- Files: `3_process-sipp.R`, `4_build-cc-uninsured.R`, `5_merge-and-finalize.R`, `build1_decision-analysis.R`, `sum3_retention.R`.

### `weight`, `hh_weight`
- Paper: the member weight behind every enrollment share, cost term and welfare aggregate.
- Chain: `5_merge-and-finalize.R` sets `weight = household_size` on every row. `choice.R build_rf` and `supply.R build_structural` carry it into the cells as `hh_weight`. The reduced-form estimates weight by `ipweight` instead; the structural likelihood, the supply conditions and the welfare scorer weight by `hh_weight`.
- Level: household, members.
- Files: `5_merge-and-finalize.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `estimate_demand.R`, `ra.R`, `cf_cell.R`, `score_cf.R`, `welfare.R`, `s3_pricing.R`, `s4_cost-gmm.R`.

### `household_size`, `hh_size`
- Paper: summary statistics ("HH Size"); covariate balance; a covariate in the propensity, first-stage and dominated-choice models; the `hh_size_prem`, `hh_size_av` and `hh_size_insured` interactions; the per-member scaling of premiums, penalties and welfare.
- Raw: the count of member rows per household-year in `pra_07192019.csv`.
- Chain: `2_aggregate-to-hh.R` sets it to the number of members in the household-year. `4_build-cc-uninsured.R` copies it from the reference year to the off-year rows; when a member of the reference year is not yet born in an earlier off-year, that member is dropped from the age shares of a multi-member household but `household_size` keeps the reference-year count. `5_merge-and-finalize.R` uses it for the poverty guideline and the tax-unit type. `choice.R build_rf` and `supply.R build_structural` rename it `hh_size` in the cells and divide household premiums and penalties by it. `add_mix_columns` (`ra.R`) sets `family = household_size > 1`.
- Level: household, members.
- Files: `2_aggregate-to-hh.R`, `4_build-cc-uninsured.R`, `5_merge-and-finalize.R`, `6_broker-density.R`, `10_commissions.R`, `build2_ipw.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `ra.R`, `cf_cell.R`, `score_cf.R`, `sum1_desc-stats.R`, `rf1_dominated.R`, `rf4_first-stage.R`, `supp1_demand-specs.R`.

---

## 2. Household demographics

### `FPL`
- Paper: summary statistics; covariate balance; the subsidy and cost-sharing eligibility behind every premium; the dominated-choice definition; the income brackets `FPL_250to400` and `FPL_400plus`.
- Raw: `pra_07192019.csv` fields `subsidy_fpl_percent_int` (percent of poverty) and `subsidy_fpl_bracket`.
- Chain: `1_clean-enrollment.R` fills a bracket recorded as "FPL Unavailable" or "Unsubsidized Applica" from the percent where the percent is valid, and drops records still without a bracket. `2_aggregate-to-hh.R` sets `FPL` to the household's maximum percent divided by 100, sets an infinite value (no member with a percent) to NA, and drops household-years whose members report different percents or brackets. `4_build-cc-uninsured.R` keeps only households with an observed FPL in some year and copies FPL from the reference year to off-year rows. `5_merge-and-finalize.R` assigns the ACA bracket (`assign_bracket`, `_helpers.R`) and computes the subsidy from it. `build1_decision-analysis.R` defines `csr_eligible` (subsidized and FPL at most 2.5) and the dominated-choice cutoffs at 1.5 and 2.0. `build2_ipw.R`, `build3_data-prep.R`, `choice.R build_rf` and `supply.R build_structural` each build `FPL_250to400` (2.5 < FPL <= 4) and `FPL_400plus` (FPL > 4).
- Level: household; ratio to the poverty line (1.5 is 150 percent).
- Files: `1_clean-enrollment.R`, `2_aggregate-to-hh.R`, `3_process-sipp.R` (the SIPP counterpart), `4_build-cc-uninsured.R`, `5_merge-and-finalize.R`, `_helpers.R`, `build1_decision-analysis.R`, `build2_ipw.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `score_cf.R`, `welfare.R`, `sum1_desc-stats.R`, `rf1_dominated.R`, `rf4_first-stage.R`, `sum2_results.R`.

### Age shares: `perc_0to17`, `perc_18to25`, `perc_26to34`, `perc_35to44`, `perc_45to54`, `perc_55to64`, `perc_65plus`, `perc_18to34`, `perc_35to54`, `perc_0to34`, `oldest_member`
- Paper: summary statistics (0-17, 18-34, 55-64); covariate balance (0-17, 18-25, 65+); covariates in the propensity, channel first stage, dominated-choice and plan-choice models (0-17, 18-34, 35-54, with 55 and over the base); the risk-score regressors (0-34); the catastrophic-plan eligibility rule (oldest member under 30).
- Raw: `pra_07192019.csv` field `age`, one per member.
- Chain: `1_clean-enrollment.R` drops records with a missing age or an age above 120. `2_aggregate-to-hh.R` computes each share as the fraction of the household's members in the band and `oldest_member` as the maximum age. `4_build-cc-uninsured.R` ages every member of the reference year by the gap to the off-year, drops off-year household-years with any member aged 65 or over, drops not-yet-born members from multi-member households, floors a lone member at age 0, and recomputes the shares and `oldest_member`; it also builds `perc_18to34` and `perc_35to54` for the SIPP prediction. `build1_decision-analysis.R` builds `perc_18to34 = perc_18to25 + perc_26to34` and `perc_35to54 = perc_35to44 + perc_45to54` on every row. `add_mix_columns` (`ra.R`) builds `perc_0to34 = perc_0to17 + perc_18to34` for the risk-score side.
- Level: household; shares of members, 0 to 1.
- Files: `1_clean-enrollment.R`, `2_aggregate-to-hh.R`, `3_process-sipp.R`, `4_build-cc-uninsured.R`, `build1_decision-analysis.R`, `build2_ipw.R`, `build3_data-prep.R`, `choice.R`, `covariates.R`, `supply.R`, `ra.R`, `welfare.R`, `sum1_desc-stats.R`, `rf1_dominated.R`, `rf4_first-stage.R`, `sum2_results.R`.

### `perc_male`
- Paper: summary statistics; covariate balance; a covariate in every household-level model; a risk-score regressor.
- Raw: `pra_07192019.csv` field `gender`.
- Chain: `1_clean-enrollment.R` codes Male 1, Female 0, and drops records with a blank. `2_aggregate-to-hh.R` averages over members. `4_build-cc-uninsured.R` copies the reference year's value to off-year rows.
- Level: household; share of members.
- Files: `1_clean-enrollment.R`, `2_aggregate-to-hh.R`, `3_process-sipp.R`, `4_build-cc-uninsured.R`, `build2_ipw.R`, `build3_data-prep.R`, `choice.R`, `covariates.R`, `supply.R`, `ra.R`, `sum1_desc-stats.R`, `rf1_dominated.R`, `rf4_first-stage.R`, `sum2_results.R`.

### Race and ethnicity shares: `perc_white`, `perc_black`, `perc_hispanic`, `perc_asian`, `perc_other`, `perc_minority`
- Paper: summary statistics (Black, Hispanic, Asian); covariate balance; covariates in every household-level model (white the base); the risk-score regressor `share_minority`.
- Raw: `pra_07192019.csv` field `race_ethnicity`.
- Chain: `1_clean-enrollment.R` recodes Latino to Hispanic, Asian, White, "Black or Africa" to Black/African American, anything else to Other Race, and a missing value to NA. `2_aggregate-to-hh.R` computes each share as the fraction of members in the category (members with NA race count in the denominator). `4_build-cc-uninsured.R` copies the reference year's shares. `add_mix_columns` (`ra.R`) builds `perc_minority` as the sum of the Asian, Black, Hispanic and Other shares.
- Level: household; shares of members.
- Files: `1_clean-enrollment.R`, `2_aggregate-to-hh.R`, `3_process-sipp.R`, `4_build-cc-uninsured.R`, `build2_ipw.R`, `build3_data-prep.R`, `choice.R`, `covariates.R`, `supply.R`, `ra.R`, `sum1_desc-stats.R`, `rf1_dominated.R`, `rf4_first-stage.R`, `sum2_results.R`.

---

## 3. Assistance channels and the selection controls

### `agent`, `broker`, `navigator` (household file)
- Paper: the channel definitions behind every assisted versus unassisted comparison; the service-channel table in the supplemental appendix.
- Raw: `pra_07192019.csv` field `service_channel`, one code per member: CIA (certified insurance agent), PBE (plan-based enroller), SCR (service-center representative), CEC (certified enrollment counselor), CEW (certified enrollment worker), otherwise unassisted.
- Chain: `2_aggregate-to-hh.R` sets, per member, `agent` for CIA, `broker` for CIA or PBE, `navigator` for SCR, CEC or CEW, and takes the maximum over the household's members. `5_merge-and-finalize.R` sets all three to 0 on off-year rows. `supp3_channel-table.R` tabulates the raw codes and their shares of enrollment records from the raw file.
- Level: household-year, 0/1.
- Files: `2_aggregate-to-hh.R`, `5_merge-and-finalize.R`, `build1_decision-analysis.R`, `rf1_dominated.R`, `supp3_channel-table.R`, `10_commissions.R` (broker share of members by insurer-year).

### `assisted`, `any_agent`, `channel`, `channel_detail`
- Paper: the treatment in the reduced form; the two channels of the structural model; the groups of the summary statistics.
- Chain: `build1_decision-analysis.R` sets `assisted = navigator | broker | agent`, `any_agent = broker | agent`, `channel` as Assisted or Unassisted, and `channel_detail` as Navigator when `navigator == 1`, Agent when `broker` or `agent` is 1 and `navigator` is 0, otherwise Unassisted. In the cells, `choice.R build_rf` and `supply.R build_structural` set `assisted` to 1 on households whose `channel` is not Unassisted, `nonbroker = assisted` for households with `any_agent == 0` and `broker = assisted` for households with `any_agent == 1`. Since `agent` (CIA) is a subset of `broker` (CIA or PBE), `any_agent` equals the household-file `broker`, and the cell column `broker` marks the same households. The two labelings differ only for a household with one member assisted by a navigator and another by an agent, which `channel_detail` calls Navigator and the cell indicators call agent; 279 of the 5.46 million enrolled household-years.
- Level: household-year, 0/1 and labels.
- Files: `build1_decision-analysis.R`, `build2_ipw.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `covariates.R`, `estimate_demand.R`, `cf_cell.R`, `sum1_desc-stats.R`, `rf1_dominated.R`, `rf2_choice-att.R`, `rf4_first-stage.R`, `s2_demand.R`, `s3_pricing.R`, `s5_se.R`, `sum2_results.R`, `sum3_retention.R`.

### `pred_assist`, `ipweight`
- Paper: the propensity-score figure; the covariate-balance figure; the weights in the dominated-choice and plan-choice ATTs.
- Chain: `build2_ipw.R` fits, by year on insured rows, a logit of `assisted` on the age, sex and race shares, `FPL_250to400`, `FPL_400plus` and `household_size`; `pred_assist` is the fitted probability and `ipweight` is 1 for assisted households and `pred_assist / (1 - pred_assist)` for unassisted ones (ATT weights). Written to `data/output/ipweights.csv` by `household_year` and joined to the household file in `build3_data-prep.R`; carried into the reduced-form cells by `choice.R build_rf`.
- Level: household-year.
- Files: `build2_ipw.R`, `build3_data-prep.R`, `choice.R`, `sum1_desc-stats.R`, `rf1_dominated.R`, `rf2_choice-att.R`.

### `n_agents`, `agents_per_10k`
- Paper: the broker-density instrument and its first-stage strength; the channel first stage.
- Raw: the FOIA workbook `RatingRegionAgentEnrollment_from_CY2014_to_CY2019__20260316.xlsx`, one sheet per year with agent license, region and enrollees; `co-est2019-alldata.csv` for population; `rating_areas.csv` for the county-to-region map.
- Chain: `6_broker-density.R` counts distinct agent licenses per region-year (`n_agents`), sums county population to regions with Los Angeles County split between regions 15 and 16 by each region's enrolled-member share, and sets `agents_per_10k = n_agents / population * 10000`. `build3_data-prep.R` joins both to the household file by region and year; `n_agents` enters the `v_hat` first stage and `agents_per_10k` the channel multinomial. `rf4_first-stage.R` reports the instrument's partial F and incremental R-squared to `results/first_stage_strength.csv`.
- Level: region-year.
- Files: `6_broker-density.R`, `build3_data-prep.R`, `rf1_dominated.R`, `rf4_first-stage.R`.

### `v_hat`
- Paper: the control-function columns of the appendix regression tables.
- Chain: `build3_data-prep.R` fits a linear probability model of `assisted` on `n_agents`, the age, sex and race shares, the FPL brackets, `household_size` and year effects on insured rows; `v_hat` is the residual, NA on uninsured rows. `choice.R build_rf` builds `cf_anthem`, `cf_blue_shield`, `cf_kaiser`, `cf_health_net`, `cf_silver` and `cf_bronze` as `v_hat` times the plan attribute for the appendix specifications.
- Level: household-year.
- Files: `build3_data-prep.R`, `choice.R`, `covariates.R`, `supply.R`, `rf1_dominated.R`, `rf2_choice-att.R`.

### `p_none_hat`, `p_nav_hat`, `p_agent_hat`, `p_nav`
- Paper: the channel-state weights in the enrollment margin of the structural model.
- Chain: `build3_data-prep.R` fits a multinomial logit of `channel_detail` on `agents_per_10k`, the age, sex and race shares, the FPL brackets, `household_size` and year effects on enrollees, weighted by `weight`, and predicts the three probabilities for every household-year. `p_nav` is a separate logit of Navigator against Agent among assisted households. Carried in `hh_choice.csv` to the structural cells.
- Level: household-year, probabilities.
- Files: `build3_data-prep.R`, `supply.R`, `estimate_demand.R`, `cf_cell.R`.

---

## 4. Plan identity and the dominated-choice outcome

### `plan_id`
- Paper: the alternative in every choice model; the plan in every supply and counterfactual result; the metal and insurer groupings of the reduced-form figures.
- Raw: `pra_07192019.csv` field `hios_id_16` with `metal_level_enhanced` and `plan_name`; `plan_data.csv` fields `HIOS` and `Plan_Name2`.
- Chain: `1_clean-enrollment.R` truncates the 16-character id to 14 characters, and to 10 for 2014 and 2015 outside SHARP, to match `plan_data`, checks that the (zip3, region, year, HIOS, metal) combination is a plan sold in that zip3 (`zip3_choices.csv` and `product_definitions.csv`), and drops records that fail. `2_aggregate-to-hh.R` replaces the plan name with the short code `Plan_Name2` keyed on (HIOS, year, metal). `5_merge-and-finalize.R` sets NA on off-year rows. In the cells (`choice.R build_rf`, `supply.R build_structural`) the cost-sharing variants collapse into one plan (`SIL94`, `SIL87`, `SIL73` become `SIL`) and an `Uninsured` row is added; the reduced-form cells also collapse insurers outside the big four into `Small_<metal>`. `rf3_summary.R` parses the insurer prefix and the metal from the code.
- Level: plan within region-year; short code such as `ANT_SIL`.
- Files: `1_clean-enrollment.R`, `2_aggregate-to-hh.R`, `5_merge-and-finalize.R`, `7_rate-filings.R`, `8_risk-scores.R`, `10_commissions.R`, `build1_decision-analysis.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, and every estimation, supply and counterfactual script.

### `metal`
- Paper: the metal-tier shares and effects; the actuarial value; the dominated-choice definition.
- Raw: `pra_07192019.csv` field `metal_level_enhanced`, filled from `plan_name` when blank.
- Chain: `1_clean-enrollment.R` keeps the cost-sharing variant in the label ("Silver - Enhanced 73/87/94") and drops records without a metal. `2_aggregate-to-hh.R` takes the household's value. `choice.R build_rf` and `supply.R build_structural` derive `silver`, `bronze`, `gold`, `platinum` and the household's actuarial value `av` from it and strip the variant for the collapsed regional plans.
- Files: `1_clean-enrollment.R`, `2_aggregate-to-hh.R`, `5_merge-and-finalize.R`, `7_rate-filings.R`, `8_risk-scores.R`, `build1_decision-analysis.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `constants.R`, `sum1_desc-stats.R`, `rf3_summary.R`, `sum2_results.R`, `supp2_cost-sharing-table.R`.

### `insurer`
- Paper: the insurer ATT figure; the big-four brand indicators; the commission-schedule figures.
- Raw: `pra_07192019.csv` field `issuer_name`; `plan_data.csv` field `Issuer_Name`.
- Chain: `1_clean-enrollment.R` and `_data-build.R` map the raw names to short names with `standardize_insurer` (`_helpers.R`). `2_aggregate-to-hh.R` takes the household's value. `sum1_desc-stats.R` groups Anthem, Blue Shield, Kaiser and Health Net separately and the rest as Other; the cells carry the four brand indicators.
- Files: `_helpers.R`, `1_clean-enrollment.R`, `2_aggregate-to-hh.R`, `5_merge-and-finalize.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `sum1_desc-stats.R`, `rf3_summary.R`, and the supply scripts.

### `network_type`, `hmo`
- Paper: the HMO indicator in the choice models; the network level of the commission schedules.
- Raw: `pra_07192019.csv` field `plan_network_type`; `plan_data.csv` field `PLAN_NETWORK_TYPE`.
- Chain: `1_clean-enrollment.R` recodes Health Net plans coded HMO to HSP in regions 1, 3, 7 and 11 for 2016 and 2017, and Bronze and Minimum Coverage HMO plans in regions 14 to 19 for 2016 to 2019. `choice.R build_rf` and `supply.R build_structural` set `hmo = 1` when `network_type == "HMO"`. `build3_data-prep.R` treats HSP as HMO only for the commission-schedule join.
- Files: `1_clean-enrollment.R`, `2_aggregate-to-hh.R`, `5_merge-and-finalize.R`, `7_rate-filings.R`, `10_commissions.R`, `build1_decision-analysis.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `s3_pricing.R`.

### `subsidized_members`, `csr_eligible`
- Paper: the cost-sharing-reduction eligibility behind the CSR silver menu and the dominated-choice sample.
- Raw: `pra_07192019.csv` field `subsidy_eligible` ("Subsidy Eligible" or not), one per member.
- Chain: `2_aggregate-to-hh.R` counts members marked eligible (`subsidized_members`). `build1_decision-analysis.R` sets `csr_eligible = 1` when `subsidized_members > 0` and FPL is at most 2.5. `choice.R build_rf` and `supply.R build_structural` use `subsidized_members > 0` with the FPL cutoffs 1.5, 2.0 and 2.5 to keep exactly one silver variant in each household's menu.
- Files: `2_aggregate-to-hh.R`, `build1_decision-analysis.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `score_cf.R`.

### `dominated_choice`
- Paper: summary statistics ("% Dominated"); the dominated-choice ATT figure and regression tables.
- Chain: `build1_decision-analysis.R` sets it NA for uninsured rows, for households not `csr_eligible`, and for FPL above 2.0; 1 for a Gold or Platinum plan at FPL up to 1.5 and for a Gold plan at FPL between 1.5 and 2.0; 0 otherwise. Households at FPL 2.0 to 2.5 (the 73 percent variant) are excluded because Gold and Platinum are not dominated there. `rf1_dominated.R` estimates the IPW-weighted regressions and the prediction-based ATT on the non-NA rows.
- Level: household-year, 0/1/NA.
- Files: `build1_decision-analysis.R`, `sum1_desc-stats.R`, `rf1_dominated.R`.

---

## 5. Premiums, subsidies and the mandate penalty at the household

### `rating_factor`
- Paper: converts every posted premium to what the household pays; the revenue weight in the pricing condition.
- Raw: `pra_07192019.csv` field `age`; `age_rating_factors.csv`.
- Chain: `2_aggregate-to-hh.R` looks up each member's factor at the member's age capped at 64 (the 2018 curve from 2018) and sums over members. `4_build-cc-uninsured.R` recomputes it from the aged members for off-year rows. `choice.R build_rf` and `supply.R build_structural` set the household premium to the posted premium divided by `RATING_FACTOR_AGE40` (1.278, `_helpers.R`) times `rating_factor`; `supply.R` also carries `rf_i = rating_factor / 1.278` as the revenue weight and `rf_member = rating_factor / hh_size` for the transfer formula.
- Level: household, sum of member factors.
- Files: `2_aggregate-to-hh.R`, `4_build-cc-uninsured.R`, `5_merge-and-finalize.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `ra.R`, `cf_cell.R`, `score_cf.R`.

### `premiumSLC`
- Paper: the benchmark premium in the subsidy formula.
- Raw: `plan_data.csv` field `Premium` (40-year-old) with `zip3_choices.csv` and `product_definitions.csv`.
- Chain: `2_aggregate-to-hh.R` takes, for each (zip3, region, year), the second-lowest silver premium among plans sold there, divides by 1.278, multiplies by each member's rating factor and sums over members. `5_merge-and-finalize.R` applies the same zip3 benchmark to off-year rows and falls back to the region's second-lowest silver where the zip3 market is missing or the step-2 value is 0 or NA.
- Level: household, dollars per month.
- Files: `2_aggregate-to-hh.R`, `5_merge-and-finalize.R`, `build3_data-prep.R`, `supply.R`, `cf_cell.R`.

### `SLC_contribution`, `subsidy`
- Paper: the net premium every household faces; the government-cost side of the counterfactuals.
- Raw: `contribution_percentages.csv`, `poverty_guidelines.csv`; `pra_07192019.csv` fields `gross_premium_amt_int` and `net_premium_amt_int` (the observed APTC is their difference).
- Chain: `5_merge-and-finalize.R` computes the ACA expected contribution (`aca_contribution`, `_helpers.R`) from FPL, the bracket's contribution percentages and the poverty guideline, and `subsidy = max(0, premiumSLC - SLC_contribution)` for FPL up to 4, else 0. It then infers a tax-household size for each household from the earliest enrolled year with a usable credit (advance credit below the premium), by scaling the guideline so that `premiumSLC - APTC` equals the formula contribution; the guideline and the contribution are rescaled to that size on every row, rows with a usable credit keep the observed contribution, and the subsidy is recomputed. `2_aggregate-to-hh.R` had set an initial `subsidy` as gross minus net premium, replaced here. In the cells the subsidy is subtracted from the household premium (`choice.R build_rf`, `supply.R build_structural`); `cf_cell.R update_premiums` recomputes it as the benchmark moves.
- Level: household, dollars per month.
- Files: `2_aggregate-to-hh.R`, `5_merge-and-finalize.R`, `_helpers.R`, `build1_decision-analysis.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `cf_cell.R`, `score_cf.R`.

### `poverty_threshold`
- Paper: enters the subsidy, the penalty and the welfare spending schedule.
- Raw: `poverty_guidelines.csv` by family size and year.
- Chain: `2_aggregate-to-hh.R` and `4_build-cc-uninsured.R` look it up by `household_size` and year (2019 for later years); `5_merge-and-finalize.R` rescales it to the inferred tax-household size.
- Level: household, dollars per year.
- Files: `_data-build.R`, `2_aggregate-to-hh.R`, `4_build-cc-uninsured.R`, `5_merge-and-finalize.R`, `build3_data-prep.R`, `choice.R`, `supply.R`.

### `cheapest_premium`
- Paper: the affordability exemption in the mandate penalty; the catastrophic-plan eligibility rule.
- Raw: `plan_data.csv` field `Premium` for Bronze plans.
- Chain: `5_merge-and-finalize.R` takes the region-year minimum bronze premium, divides by 1.278 and multiplies by `rating_factor`.
- Level: household, dollars per month.
- Files: `5_merge-and-finalize.R`, `build3_data-prep.R`, `choice.R`, `supply.R`.

### `penalty`, `penalty_own`
- Paper: the mandate penalty in the net premium and on the uninsured alternative; the exclusion restriction of the demand model.
- Chain: `5_merge-and-finalize.R` computes the annual household penalty by year from the filing threshold (single, household head, married, from `household_size` and the adult count), the flat amount per adult and half per child capped at three adults, the percent of income above the filing threshold, and the cap per person, with an exemption when income is below the filing threshold or the cheapest bronze net of subsidy exceeds the year's affordability share of income; 2019 has no penalty. `choice.R build_rf` and `supply.R build_structural` subtract `penalty / 12` from the insured premium (premium type "net") and set `penalty_own = penalty / 12 / hh_size` on the uninsured row.
- Level: household, dollars per year (annual) and per member per month (`penalty_own`).
- Files: `5_merge-and-finalize.R`, `build3_data-prep.R`, `choice.R`, `supply.R`, `cf_cell.R`, `score_cf.R`, `build-meps-uninsured-oop.R`.

---

## 6. Reduced-form plan-choice variables (cells built by `choice.R build_rf`)

`rf2_choice-att.R` builds one cell per region-year from `hh_choice.csv` and
`plan_choice.csv` (both written by `build3_data-prep.R`) with `build_rf`, using
a 20 percent draw of households per cell (`SAMPLE_FRAC`, stratified by insured
and uninsured) and the net premium. The columns below exist only inside the
cells; `cell_vars` in `rf2_choice-att.R` lists the ones kept.

### `choice`
- Paper: the dependent variable of the plan-choice ATT.
- Chain: `build_rf` sets `plan_choice = 1` on the row whose `plan_id` matches the household's plan, or on the `Uninsured` row for a household without one, and keeps households with exactly one chosen row; renamed `choice`. `rf2_choice-att.R` drops the uninsured rows, since the ATT is conditional on enrollment.
- Files: `choice.R`, `rf2_choice-att.R`.

### `premium` (cell)
- Paper: the premium coefficient of the plan-choice model.
- Raw: `plan_data.csv` field `Premium`.
- Chain: `build3_data-prep.R` writes the plan's premium to `plan_choice.csv`; `build_rf` takes the plan's value in the cell, converts it to the household premium (`Premium / 1.278 * rating_factor`), subtracts the subsidy with a floor at zero, subtracts `penalty / 12`, sets 0 on the uninsured row, takes the minimum across a household's collapsed regional plans of the same metal, and divides by `hh_size` and by 100. `perc_*_prem`, `hh_size_prem`, `FPL_250to400_prem` and `FPL_400plus_prem` are the demographic times this premium.
- Level: household-plan, hundreds of dollars per member per month.
- Files: `build3_data-prep.R`, `choice.R`, `covariates.R`, `rf2_choice-att.R`.

### Plan attributes: `silver`, `bronze`, `gold`, `platinum`, `hmo`, `hsa`, `av`, `Anthem`, `Blue_Shield`, `Kaiser`, `Health_Net`
- Paper: the plan-attribute coefficients; the metal and insurer groupings of the ATT figures.
- Raw: `plan_data.csv` fields `metal_level`, `PLAN_NETWORK_TYPE`, `HSA`, `Issuer_Name`.
- Chain: `build_rf` sets `silver` for any Silver variant, `bronze`, `gold` and `platinum` from the metal, `hmo` from the network type, `hsa` when the plan's HSA flag is positive, the four brand indicators from the issuer, and `av` as 0.60, 0.70, 0.73, 0.87, 0.94, 0.80, 0.90 by metal and variant (0 for the uninsured row). Collapsed regional plans take the mean `hsa` and `av` and no brand indicator.
- Files: `choice.R`, `covariates.R`, `rf2_choice-att.R`, `rf3_summary.R`.

### Channel interactions: `assisted_silver`, `assisted_bronze`, `assisted_gold`, `assisted_plat`, `broker_silver`, `broker_bronze`, `assisted_premium`, `broker_premium`
- Paper: the appendix pooled plan-choice specifications (navigator and agent by metal).
- Chain: `build_rf` builds them as `nonbroker` or `broker` (the navigator and agent indicators defined in section 3) times the plan attribute or the cell premium.
- Files: `choice.R`, `covariates.R`, `rf2_choice-att.R`, `rf3_summary.R`.

### Control-function interactions: `cf_anthem`, `cf_blue_shield`, `cf_kaiser`, `cf_health_net`, `cf_silver`, `cf_bronze`
- Paper: column (3) of the appendix pooled plan-choice table.
- Chain: `build_rf` multiplies `v_hat` by the plan attribute for the terms named in the spec; NA for households without `v_hat`.
- Files: `choice.R`, `covariates.R`, `rf2_choice-att.R`, `rf3_summary.R`.

### ATT summaries: `obs_purchase`, `pred_purchase`, `att`
- Paper: the metal-tier and insurer ATT figures; the new-versus-all comparison in the appendix.
- Chain: `rf2_choice-att.R` fits the conditional logit on unassisted households (weighted by `ipweight`), predicts each assisted household's probabilities from the fitted coefficients, and sums observed choices and predicted probabilities by plan, region and year (`results/choice_point_estimates.csv`, and `_new.csv` for new enrollees). `rf3_summary.R` parses metal and insurer from `plan_id`, computes the observed and predicted shares over all enrollees and their difference, and attaches bootstrap standard errors from `results/choice_bootstrap_pred.csv` (50 within-cell resamples in `rf2`).
- Level: metal or insurer, share of assisted enrollment.
- Files: `rf2_choice-att.R`, `rf3_summary.R`.

---

## 7. Structural demand variables (cells built by `supply.R build_structural`, estimated by `s2_demand.R`)

`s1_inputs.R` reads `hh_choice.csv` and `plan_choice.csv` and draws one seed
per region-year cell from `MASTER_SEED`, so the structural cells, the pricing
step and the counterfactual draw the same 20 percent of households per cell as
the reduced form. `s2_demand.R` builds each cell with `build_structural`
(`TEMP_DIR/choice_cells/`) on the spec it writes to `demand_spec.csv`, then
calls `estimate_demand` (`estimate_demand.R`). The cells hold every household
of the draw, both channels, one row per household and plan plus an `Uninsured`
row. Compared with the reduced-form cells, the seven larger regional insurers
(Molina, LA Care, Sharp, Chinese Community, Oscar, Western, Valley) stay
separate plans and only the remaining micro-carriers collapse into
`OS_<metal>` (`Other_Small`, which carries no commission); the cost-sharing
variants collapse into one silver plan as before. The sampling, menu filters,
premium construction and plan attributes are those of section 6; this section
covers what the structural cells add.

### `inside`
- Paper: the inside-good intercept of the demand model.
- Chain: `build_structural` sets `inside = 1 - uninsured_plan`, 1 on every plan row and 0 on the `Uninsured` row.
- Files: `supply.R`, `s2_demand.R`, `estimate_demand.R`, `sum2_results.R`.

### `premium` (structural), `premium_posted`, `premium_hh`, `premium_oop`
- Paper: the premium coefficient and every premium interaction; the posted premium is the object the pricing condition solves for.
- Raw: `plan_data.csv` field `Premium`.
- Chain: as in section 6, `premium` is the household's net premium in hundreds of dollars per member per month (`(premium_posted / 1.278 * rating_factor - subsidy)`, floored at zero, less `penalty / 12`, divided by `hh_size` and 100). `build_structural` also keeps `premium_posted` (the plan's 40-year-old premium), `premium_hh` (the age-rated household premium) and `premium_oop` (net of subsidy and penalty, per household) on every row, and `cf_cell.R update_premiums` rebuilds the chain from a new `premium_posted` in the counterfactuals, recomputing the subsidy from the benchmark for `subsidized` households (those with a finite `SLC_contribution`).
- Files: `build3_data-prep.R`, `supply.R`, `cf_cell.R`, `score_cf.R`, `estimate_demand.R`, `sum2_results.R`.

### `av` and the interactions `hh_size_av`, `perc_0to17_av`, `perc_18to34_av`, `perc_35to54_av`, `perc_male_av`, `perc_black_av`, `perc_hispanic_av`, `perc_asian_av`, `perc_other_av`, `FPL_250to400_av`, `FPL_400plus_av`
- Paper: the actuarial-value coefficient and the demographic terms on generosity; the AV entering the risk-adjustment transfer.
- Chain: `build_structural` assigns `av` by metal and cost-sharing variant (0.60 bronze, 0.70 silver, 0.73, 0.87 and 0.94 for the silver variants, 0.80 gold, 0.90 platinum, 0 on the uninsured row), so a household's `av` for the silver plan is the variant it qualifies for. Each interaction is the demographic times `av`. The plan-attribute table (`plan_attrs`) carries the base-metal AV (the minimum over households) for the transfer formula.
- Level: household-plan, 0 to 1.
- Files: `supply.R`, `covariates.R`, `cf_cell.R`, `ra.R`, `score_cf.R`, `welfare.R`, `s2_demand.R`, `estimate_demand.R`, `supp1_demand-specs.R`, `supp4_pbe-robustness.R`.

### Enrollment shifters: `hh_size_insured`, `perc_0to17_insured`, `perc_18to34_insured`, `perc_35to54_insured`, `perc_male_insured`, `perc_black_insured`, `perc_hispanic_insured`, `perc_asian_insured`, `perc_other_insured`, `FPL_250to400_insured`, `FPL_400plus_insured`
- Paper: the demographic terms on the propensity to enroll at all.
- Chain: `build_structural` (and `build_rf`) multiplies each demographic by the inside indicator, so the term is the demographic on plan rows and 0 on the uninsured row. `family_insured` and `subsidy_insured` are built the same way but are not in the spec.
- Files: `supply.R`, `choice.R`, `covariates.R`, `s2_demand.R`, `estimate_demand.R`.

### Channel terms: `nonbroker`, `broker`, `assisted_av`, `broker_av`, `assisted_premium`, `broker_premium`
- Paper: the navigator and agent terms on generosity and on the premium slope; the channel-state utilities of the enrollment margin.
- Chain: `build_structural` sets `nonbroker` and `broker` from `assisted` and `any_agent` as in section 3, then `assisted_av = nonbroker * av`, `broker_av = broker * av`, `assisted_premium = nonbroker * premium`, `broker_premium = broker * premium`. In estimation (`estimate_demand.R`) these terms enter plan choice at the household's realized channel and the enrollment margin through the state utilities: the navigator state adds the `assisted_*` terms and the agent state the `broker_*` and commission terms to the base utility, and the inclusive values of the three states are averaged with `p_none_hat`, `p_nav_hat` and `p_agent_hat`. `compute_utility` (`supply.R`) rebuilds the same state add-ons for the supply and counterfactual kernels. `covariates.R extensive_exclude_terms` names the terms left out of the base utility.
- Files: `supply.R`, `choice.R`, `covariates.R`, `estimate_demand.R`, `cf_cell.R`, `score_cf.R`, `welfare.R`, `s2_demand.R`, `s5_se.R`, `sum2_results.R`.

### `rate`, `is_pct`, `comm_pmpm` (the plan commission basis)
- Paper: the commission attached to each plan; the outlay side of the commission condition; the observed schedules the counterfactuals move; the commission-schedule figures (`flat_comm.pdf`, `perc_comm.pdf`), which `sum2_results.R` draws from `commission_lookup.csv` by insurer and year, flat schedules in dollars and percentage schedules in percent of premium, with a network-split carrier drawn as one series per network.
- Raw: `commission_input.csv` (the schedule table by insurer, network and year, new and renewal rates); the SRRT workbooks in `rate-filings-srrt/` (Actual-to-Expected tabs, "Agent Commissions" or "Commission Admin Expenses"); the CMS MLR filings through `9_mlr-admin.R` (`mlr_admin.csv`: commissions per member, member months and premium per member by insurer-year); `demand_households.csv` for the on-exchange member months and agent share.
- Chain: `10_commissions.R` takes each insurer-year's commission per member of its whole individual book (SRRT where filed, MLR for 2014 to 2016), converts a share-of-premium figure to dollars at the insurer's premium per member, and divides by the book's agent share (on-exchange share times the observed agent share of members, off-exchange coverage taken as agent-sold) to get a rate per agent enrollee. That filings rate is used for the carriers whose book is at least 75 percent on the exchange; Anthem, Blue Shield, Kaiser and Oscar keep the schedule table's on-exchange rates, Blue Shield throughout and Health Net where its filed HMO and PPO rates differ carry network-level rates (new and renewal averaged), and an enrolled insurer-year in no source gets an explicit zero. The result is `commission_lookup.csv` keyed on (`insurer_prefix`, `year`, `hmo`) with `rate` and `is_pct` (a rate below 0.1 is a share of premium). `build3_data-prep.R` joins it to each plan by prefix, year and network (HSP counts as HMO) and sets `comm_pmpm = rate * (Premium / 1.278) * mu_member` for percentage schedules, where `mu_member` is the enrollee-weighted mean member rating factor of the plan-cell (pooled across the silver variants, the region-year mean where the plan has no enrollees), and `comm_pmpm = rate` for flat schedules; `plan_choice.csv` carries `rate`, `is_pct` and `comm_pmpm`. `build_structural` copies them onto every row of the plan and into `plan_attrs`.
- Level: plan within region-year, dollars per member per month.
- Files: `9_mlr-admin.R`, `10_commissions.R`, `build3_data-prep.R`, `s1_inputs.R`, `supply.R`, `choice.R`, `cf_cell.R`, `cf_year.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `score_cf.R`, `sum2_results.R`.

### `comm_hh`, `commission_broker`, `commission_broker_sq`
- Paper: the commission terms of the demand model (linear and squared) and their marginal effect; the turning point `\commTurningPoint`; the commission-premium equivalence.
- Chain: `build_structural` sets the household commission `comm_hh = rate * (premium_posted / 1.278) * rating_factor / hh_size` for a percentage schedule and `comm_hh = rate` for a flat one, then `commission_broker = comm_hh` and `commission_broker_sq = comm_hh^2 / 100` on the rows of agent-assisted households (`assisted` and `any_agent`), 0 otherwise. In estimation the two terms are the agent state's commission add-on (`CH_TERMS`, `CH_TERMS_OPT` in `estimate_demand.R`); `compute_commission_derivatives` (`supply.R`) uses the household marginal utility `b1 + 2 b2 comm_hh / 100`; `cf_cell.R apply_commissions` rebuilds both from a scenario's rates.
- Level: household-plan, dollars per member per month and its square over 100.
- Files: `supply.R`, `choice.R`, `covariates.R`, `estimate_demand.R`, `cf_cell.R`, `cf2_score.R`, `cf3_se.R`, `score_cf.R`, `welfare.R`, `s2_demand.R`, `sum2_results.R`, `supp1_demand-specs.R`, `supp4_pbe-robustness.R`.

### The demand estimates: `choice_coefficients_structural.csv`, `_se.csv`, `_vcov.csv`, `lambda`
- Paper: the demand table (`demand_estimates.tex`); `\lambdaHat`, `\nDemandParams`; every supply, counterfactual and welfare quantity.
- Chain: `estimate_demand` loads the cells with the spec's columns, normalizes `hh_weight` to mean 1 across all cells, and maximizes the two-part nested-logit likelihood by BFGS-BHHH from zeros with `lambda` started at 1, writing one estimate per term plus `lambda` to `results/choice_coefficients_structural.csv`. `s5_se.R` (`se.R`) computes sandwich standard errors and the covariance on the same cells and likelihood (`prepare_cells` is shared) and writes `_se.csv` and `_vcov.csv`; `cf3_se.R` draws from that covariance. `sum2_results.R` labels the terms and writes the table with the standard errors.
- Files: `estimate_demand.R`, `s2_demand.R`, `s5_se.R`, `se.R`, `cf3_se.R`, `sum2_results.R`, and every script that reads the coefficient file.

### Commission equivalence: `commission_equivalence.csv` and the macros `\meanCommission`, `\meanNetPremium`, `\commPremRatio`, `\commPremElast`, `\commTurningPoint`
- Paper: the sentence interpreting the commission terms.
- Chain: `sum2_results.R` reads the structural cells, keeps the chosen inside plan of each agent-assisted household, and computes, weighted by `hh_size`, the mean net premium (`100 * premium`), the mean household commission `comm_hh`, and the mean price coefficient per $100 (`premium + broker_premium` plus each demographic interaction times the household's demographic). The marginal utility of a commission dollar at the mean commission is `commission_broker + 2 * commission_broker_sq * mean_commission / 100`; the turning point is `-100 * commission_broker / (2 * commission_broker_sq)`; the dollar equivalence divides the marginal utility by the price coefficient per dollar; the elasticity ratio multiplies each by its mean level. `add_num` writes each to `results/tables/paper-numbers.tex`.
- Files: `sum2_results.R`.

### Sample counts: `\nHHfull`, `\nHHclean`, `\nHHins`, `\pctNewEnrollee`, `\pctAssisted`, `\pctBroker`, `\pctNavigator`
- Paper: the household counts and channel shares quoted in the data section.
- Chain: `sum2_results.R` reads `hh_full_prepped.csv` and counts household-years overall, new enrollees and insured, and the shares of insured household-years with `channel` not Unassisted, with `any_agent == 1` and with `navigator == 1`.
- Files: `sum2_results.R`.

---

## 8. Cost-side inputs from the filings (data-build steps 7 to 9)

Raw inputs for this section: `data/input/Covered California/2014-2020.RData`
(the CMS rate-filing public use files, one data frame per filing year),
`data/input/Covered California/rate-filings-srrt/risk_score_data.csv` (plan
risk scores from the California Supplemental Rate Review Templates by insurer,
plan type, metal, rating area and year), the CMS medical loss ratio public use
files at `D:/research-data/insurance-mlr/` (2014 and 2015 zipped, 2016 to 2018
as folders), and `data/input/cms_gcf_california.csv` (the geographic cost
factors of the transfer formula).

### Filed claims and member months: `EXP_INC_CLM`, `EXP_MM`, `EXP_INC_CLM_PMPM`, `log_cost`, `EXP_TP`, `EXP_RSK_ADJ`, `EXP_REIN`
- Paper: the dependent variable and weights of the claims equation; the filed transfers and reinsurance behind the reinsurance factor.
- Raw: the PUF experience block (`EXP_MM`, `EXP_TP`, `EXP_INC_CLM`, `EXP_RSK_ADJ`, `EXP_REIN`) of each `PUF_<year>` data frame in `2014-2020.RData`.
- Chain: `7_rate-filings.R` keeps California individual-market rows, drops catastrophic and "Not Applicable" metals, maps `COMPANY` and `ISSUER_ID` to our insurers (Health Net's two issuer ids to its HMO and PPO filers), builds `plan_id` as insurer prefix, metal abbreviation and a "3" suffix for the big four's HMO products (Kaiser and the regionals carry none), sets Health Net's HSP plan ids to HSP, blanks the experience of any carrier-filing that stamps one company figure on every plan row, keys each experience row to `filing - 2` (the calendar year it describes), sums to plan-year, drops plan-years with under 12 member months or no claims, and computes the per-member-month measures. `log_cost` is the log of claims per member month. The projection block is keyed to the filing year and joined to the same plan-year for the reinsurance factor.
- Level: plan-year (no region in the filings), dollars per member month.
- Files: `7_rate-filings.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `ra.R`.

### `reins_factor`
- Paper: the reinsurance share netted out of claims in marginal cost through 2016.
- Chain: `7_rate-filings.R` sets it to projected reinsurance over projected claims for the plan-year, capped at 1 and 0 where there is no projection. `s3_pricing.R` writes the plan-year values to `reinsurance_factors.csv` and attaches each cell's plans their factor (0 when absent); `predict_mc_structural` (`ra.R`) multiplies predicted claims by one minus it.
- Files: `7_rate-filings.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `cf1_estimate.R`, `cf3_se.R`, `ra.R`, `cf_cell.R`.

### PUF-implied `risk_score`
- Paper: not reported; a diagnostic kept in `rate_filing_rsdata.csv`.
- Chain: `7_rate-filings.R` backs a score out of the filed transfer, the plan's share of member months and its AV-weighted utilization share within the year. The estimation uses the SRRT scores below instead.
- Files: `7_rate-filings.R`.

### SRRT plan risk scores: `risk_score`, `log_risk_score`, `member_months` (`plan_risk_scores.csv`, `plan_risk_scores_year.csv`)
- Paper: the dependent variable of the risk-score equation; the instrument for the predicted score in the claims moment.
- Raw: `risk_score_data.csv`, one row per insurer, plan type, metal, rating area and year with the member months behind each score.
- Chain: `8_risk-scores.R` keeps region-level rows for 2014 to 2019 with positive scores and member months, maps insurers to our prefixes, keeps the plan type as the network key for Health Net (HMO or PPO, the only carrier filing separate scores) and "Both" for every other carrier, and averages the score by member months to (insurer, metal, region, year, network) and to (insurer, metal, year, network).
- Level: insurer by metal by region by year (and by year), a liability risk score.
- Files: `8_risk-scores.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `ra.R`.

### Administrative cost and the commission saving: `admin_pmpm`, `admin0_pmpm`, `sales_ga_pmpm`, `commission_pmpm` (MLR), `beta0`, `beta` (`mlr_admin.csv`, `mlr_admin_beta.csv`, `commission_beta_carrier.csv`)
- Paper: the administrative cost per member in marginal cost; the administrative saving per commission dollar `beta_f` in the pricing and commission conditions; `(1 - beta_f)` in the cost table.
- Raw: the MLR summary files (`MR_Submission_Template_Header.csv`, `Part1_2_Summary_Data_Premium_Claims.csv`), individual-market columns: member months, earned premium, incurred claims, agents' and brokers' fees and commissions, direct sales salaries, other general and administrative expense, claims adjustment expense.
- Chain: `9_mlr-admin.R` keeps California filers with over 20,000 member months, maps company names to our prefixes, sums each item by insurer-year, and forms per-member-month amounts: `admin_pmpm` (direct sales, other G&A and claims adjustment), `sales_ga_pmpm` (direct sales and other G&A), `commission_pmpm`, `premium_pmpm`, `claims_pmpm`; 2019 carries 2018. A within-insurer regression of `sales_ga_pmpm` on `commission_pmpm` with insurer and year effects gives `beta0` (the pooled saving per commission dollar) and its fitted insurer and year effects give `admin0_pmpm`, the administrative level before any commission saving plus the observed claims adjustment cost. The same relation on all states' filers, with filer and year effects removed and the saving bounded in (0, 1) by a logistic in standardized log member months, gives each California carrier's `beta` at its own size, with a cluster bootstrap by filer for the standard error. `s3_pricing.R` attaches `admin0_pmpm` to each plan by prefix and year (`ADMIN_LOOKUP`, 0 where absent), and `s4_cost-gmm.R` fixes `beta_f` per carrier from `commission_beta_carrier.csv` (`BETA_FY`, the pooled `beta0` for a carrier outside the table) and writes it by carrier-year to `commission_beta.csv` for the counterfactuals.
- Level: insurer-year, dollars per member month; `beta` a rate per commission dollar.
- Files: `9_mlr-admin.R`, `10_commissions.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `s5_se.R`, `cf1_estimate.R`, `cf_cell.R`, `cf_year.R`, `score_cf.R`, `sum2_results.R`.

### Transfer-formula constants: `RA_IDF_BY_AV`, `RA_ADMIN_SHARE`, `gcf`
- Paper: the HHS transfer formula.
- Raw: `cms_gcf_california.csv` (geographic cost factor by rating area and benefit year); the induced-demand factors by metal AV (1.00, 1.03, 1.08, 1.15) and the 14 percent administrative reduction of the premium total from 2018 are constants in `constants.R`.
- Files: `constants.R`, `ra.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `cf_cell.R`, `cf_year.R`.

---

## 9. Cost equations (`s3_pricing.R`, `s4_cost-gmm.R`, `ra.R`)

### Plan-level enrollee composition: `share_0to34`, `share_male`, `share_family`, `share_minority`, `arf`, `demand` (predicted); `plan_demographics.csv`, `plan_demographics_region.csv` (observed)
- Paper: the regressors of the risk-score equation; the enrollee-mix terms of the pricing and commission conditions.
- Chain: two versions exist. `build3_data-prep.R` aggregates the observed enrollment (weighted by household size) to plan-year and plan-region-year shares and writes them to the two CSV files; `s3_pricing.R` uses the region-level observed shares, aggregated to insurer-metal-region-year, to fit the starting risk-score regression on the SRRT scores. The model versions come from `compute_demographic_shares` (`ra.R`): for each plan in a cell, the mean of `perc_0to34`, `perc_male`, `family` and `perc_minority` (`add_mix_columns`) over households weighted by `hh_weight` times the predicted choice probability, plus `arf`, the predicted-member mean of `rf_member = rating_factor / hh_size`, and `demand`, the predicted members. `s3_pricing.R` saves them per cell in `foc_inputs/foc_<region>_<year>.rds`; `s4_cost-gmm.R` aggregates them (weighted by `demand`) to the SRRT rows for its risk-score regression and to plan-years for the claims rows; the counterfactual recomputes them at every evaluation (`cf_cell.R`).
- Level: plan within region-year, shares of predicted members.
- Files: `build3_data-prep.R`, `ra.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `cf_cell.R`, `score_cf.R`.

### Rating-area shares of a plan-year: `share_ra2` to `share_ra19`
- Paper: the market controls of the claims equation ("Rating-area shares" in the cost table).
- Chain: `build3_data-prep.R` computes each plan-year's share of enrolled members in each region from the household file (region 1 the base, silver variants pooled) and attaches them to every region row of the plan in `plan_choice.csv`. `s3_pricing.R` and `s4_cost-gmm.R` join them to the filing plan-years and carry them in each cell's plan characteristics.
- Files: `build3_data-prep.R`, `ra.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `cf_cell.R`.

### Risk-score equation: `ra_rs_coefs_gmm.csv` (`(Intercept)`, `im_<prefix>_<metal>`, `share_0to34`, `share_male`, `share_family`, `share_minority`)
- Paper: the risk-score block of the cost table; the predicted risk score behind every claims, transfer and marginal-cost figure.
- Chain: `s4_cost-gmm.R` regresses the SRRT `log_risk_score` on a full set of insurer-by-metal indicators (Kaiser silver the base, `RS_IM_TERMS` in `ra.R`) and the four predicted composition shares aggregated to the SRRT rows, by weighted least squares with member-month weights, once, and holds the coefficients fixed (`ALPHA_FIXED`); `s3_pricing.R` fits the same regression on the observed shares as the starting values (`estimate_ra_regressions`, `ra.R`). `predict_risk_scores` applies the coefficients to each plan's indicators and predicted shares in every cell; `s5_se.R` reports the sandwich standard errors of the fixed OLS block.
- Files: `ra.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `s5_se.R`, `se.R`, `cf_cell.R`, `score_cf.R`, `sum2_results.R`.

### Claims equation: `ra_claims_coefs_gmm.csv` (`(Intercept)`, `log_risk_score`, `HMO`, `year_2015` to `year_2018`, `Anthem`, `Blue_Shield`, `Kaiser`, `Health_Net`, `share_ra2` to `share_ra19`)
- Paper: the claims block of the cost table; the pass-through of the risk score into claims; predicted claims everywhere.
- Chain: `s3_pricing.R` fits the starting values by weighted least squares of `log_cost` on the predicted log risk score and the exogenous terms (`CLAIMS_EXOG_TERMS` in `ra.R`; AV is omitted because the score carries generosity) and writes `ra_claims_coefs.csv`. `s4_cost-gmm.R` re-estimates the coefficients (`gamma`) by two-step GMM jointly with the carrier constants of the commission condition: the claims moments are the level residuals `claims - exp(gamma' W)` on the filing plan-years, with the risk score replaced by the first-stage fitted value of the SRRT plan-year score on the predicted score and the controls, weighted by member months normalized within five insurer groups (the big four and one pooled small carrier); the pricing moments are the plan-year conditions in dollars; the commission moments are the carrier conditions net of the constants. Step 1 uses the identity weight and step 2 the inverse of the block-diagonal moment covariance. `predict_claims` (`ra.R`) applies the coefficients in every cell.
- Files: `ra.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `s5_se.R`, `se.R`, `cf_cell.R`, `score_cf.R`, `sum2_results.R`.

### Carrier constants of the commission condition: `commission_wedge.csv` (`wedge_<prefix>`)
- Paper: the `delta_f` rows of the cost table.
- Chain: `s4_cost-gmm.R` estimates one level per carrier with an identified commission condition (`WEDGE_FIRMS`) as part of the GMM parameter vector, starting at zero; the condition for each commission-setting unit and year is `MB / MC - 1 + beta_f - delta_f`. Units are carrier-years, or carrier-network-years (`.HMO`, `.PPO`) where the schedule files distinct network rates. `cf1_estimate.R` holds them fixed (`WEDGE_LOOKUP`).
- Files: `s4_cost-gmm.R`, `s5_se.R`, `cf1_estimate.R`, `cf_year.R`, `sum2_results.R`.

### Cost standard errors: `cost_coefficients_gmm_se.csv`, `cost_coefficients_gmm_vcov.csv`
- Paper: the standard errors in the cost table.
- Chain: `s5_se.R` (`cost_gmm_sandwich_se`, `se.R`) computes the GMM sandwich at the step-2 solution with the same moment function and weight, and reports the risk-score OLS standard errors alongside; `sum2_results.R` writes `cost_estimates.tex` from the `equation`, `param`, `estimate` and `se` columns.
- Files: `s5_se.R`, `se.R`, `sum2_results.R`.

---

## 10. Supply-side quantities (`s3_pricing.R`, `s4_cost-gmm.R`, `supply_results.csv`)

Every quantity below is computed per plan within a region-year cell from the
structural cell data of section 7 at the estimated demand, first in
`s3_pricing.R` at the starting cost coefficients (`supply_results.csv`) and
again in `s4_cost-gmm.R` at the GMM solution (`mc_gmm.csv`,
`foc_plan_year_gmm.csv`, `commission_foc_fit.csv`).

### `shares`, `rshares`, `members`, `N`
- Paper: the enrollment behind every supply and counterfactual aggregate.
- Chain: `compute_shares_and_elasticities` (`supply.R`) gives each plan's predicted members as a share of the cell's member weight (`shares`) and the same weighted by each household's rating pass-through `rf_i = rating_factor / 1.278` (`rshares`, the revenue term). `N` is the cell's members (the sum of `hh_weight` over households, taken from the `Uninsured` rows) and `members = shares * N`.
- Files: `supply.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `ra.R`, `cf_cell.R`, `cf_year.R`.

### Demand derivatives: `elast_mat`, `relast_mat`, `zelast`, `own_mat`, `Omega`, `Omega_r`, `Omega_broker`, `comm_D`, `comm_D_r`, `comm_Dz`, `comm_qB`
- Paper: the derivative terms of the pricing and commission conditions and of the transfer response.
- Chain: `compute_shares_and_elasticities` differentiates each plan's predicted members with respect to each plan's posted premium through the household premium, the subsidy (the benchmark's premium moves every subsidized household's price) and the enrollment margin (`elast_mat`), the same weighted by `rf_i` (`relast_mat`) and by each composition characteristic (`zelast`); `compute_broker_shares_and_elasticities` gives the agent-assisted part; `compute_commission_derivatives` differentiates agent enrollment with respect to each plan's commission through the household marginal utility `b1 + 2 b2 comm_hh / 100` (`comm_D`, `comm_D_r`, `comm_Dz`) and gives agent members per plan (`comm_qB`). `own_mat` marks plans of the same carrier (plan-id prefix); `Omega = -own_mat * t(elast_mat)`, `Omega_r` and `Omega_broker` the same on the other two matrices.
- Files: `supply.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `cf_cell.R`, `cf_year.R`.

### `posted_premium`, `realized_premium`, `base_premium`, `region_factor`
- Paper: the premium columns of the supply table; the object the counterfactual solves.
- Chain: `posted_premium` is the plan's 40-year-old premium from `plan_attrs`. `realized_premium = posted_premium * rshares / shares`, the premium collected per member. `base_premium` is the member-weighted mean of `posted_premium` over the plan's regions in the year and `region_factor = posted_premium / base_premium`; cf1 solves one `base_premium` per plan-year and holds the factors.
- Level: plan-cell (and plan-year for `base_premium`), dollars per month.
- Files: `s3_pricing.R`, `s4_cost-gmm.R`, `cf1_estimate.R`, `cf_cell.R`, `cf_year.R`, `sum2_results.R`.

### `predicted_risk_score`, `predicted_claims`, `ra_transfer`, `ra_factor_static`
- Paper: the cost components behind marginal cost.
- Chain: `compute_mc` (`ra.R`) predicts the log risk score from the cell's plan indicators and predicted composition, predicts claims from it and the claims controls, and computes the transfer `T_j = tpn (x_j / R - y_j / A)` with `x_j` the score times the induced-demand and geographic factors, `y_j` the AV times the plan's `arf` and the same factors, `R` and `A` the statewide member-weighted sums (the cell's own contribution recomputed, the rest of the state from `ra_state_totals`), and `tpn` the year's premiums collected at observed premiums (`RA_TP`) net of the administrative share. `ra_factor_static` is the plan's AV times its induced-demand factor.
- Files: `ra.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `cf_cell.R`, `cf_year.R`, `cf1_estimate.R`.

### `mc_structural`, `mc_gmm`, `mc_gmm_net`, `admin_pmpm`
- Paper: the "MC (Structural)" column of the supply table and the markups; the marginal cost of every counterfactual.
- Chain: marginal cost is predicted claims times one minus the reinsurance factor, minus the transfer, plus the insurer's administrative cost per member (`admin0_pmpm` by carrier-year). `s3_pricing.R` reports `mc_structural` at the starting coefficients without the administrative cost and `admin_pmpm` beside it; `s4_cost-gmm.R` writes `mc_gmm` (with the administrative cost, the margin object) and `mc_gmm_net` (without it) at the GMM solution, and `sum2_results.R` uses `mc_gmm` for the supply table and the headline macros.
- Files: `ra.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `cf_cell.R`, `score_cf.R`, `sum2_results.R`.

### `markup`, `lerner_index`, `\meanMarkup`, `\meanLerner`, `\nSupplyCells`
- Paper: the markup and Lerner columns of the supply table and the sentence quoting them.
- Chain: `markup = realized_premium - mc_gmm` and `lerner_index = markup / realized_premium` in `sum2_results.R` (the same in `s3_pricing.R` at the starting cost, not reported). The table reports medians by metal over plan-cells with a finite `mc_foc`; the macros are the means over the same rows and the count of region-years.
- Files: `s3_pricing.R`, `sum2_results.R`.

### `mc_foc`
- Paper: the "MC (FOC)" column of the supply table and the validation figure (`supply_mc_foc_vs_structural.png`).
- Chain: `s3_pricing.R` inverts the cell's pricing condition at the observed premiums, `Omega^-1 (Omega_r p - rhs)`, for the cost the observed prices imply, less the administrative cost; a diagnostic only. `sum2_results.R` plots it against `mc_gmm_net` and adds the administrative cost back for the table column.
- Files: `s3_pricing.R`, `sum2_results.R`.

### Pricing condition: `foc_resid`, `G`, `G_per_member`, `G_dollars` (`foc_plan_year_gmm.csv`)
- Paper: the pricing first-order condition; the fit check of the baseline; the M3 moments.
- Chain: per plan-cell, `foc_resid = rshares + ra_foc - cc - (Omega_r p - Omega mc) + Omega_broker ((1 - beta) comm)` in share units per member, with `ra_foc` the transfer's response to the premium (`compute_ra_foc`: the mix response of scores and rating factors, the statewide sums, and the premium-total feedback) and `cc` the claims moving with the enrollee mix (`compute_claims_comp`). The plan-year condition is `G = sum_c N_c g_jc foc_resid_jc`; `G_dollars` divides by the weighted own-price term. Plan-cells below `SHARE_FLOOR_FOC` (0.005) are left out of the plan-year sum.
- Files: `ra.R`, `supply.R`, `s3_pricing.R`, `s4_cost-gmm.R`, `cf_year.R`, `constants.R`.

### Commission condition: `MB`, `MC`, `mu_hat`, `comm_bar`, `phi`, `lev`, `wedge` (`commission_foc_fit.csv`)
- Paper: the commission first-order condition; the fit of the carrier conditions.
- Chain: per commission-setting unit and year, `MB` is the carrier's margin (`posted_premium` on the rating-weighted derivative less cost on the member derivative) times the change in agent enrollment per unit of the unit's commission scale, summed over the carrier's plans, plus the transfer and mix responses; `MC` is the outlay, the unit's commissions times its agent members; both accumulate over cells weighted by `N`. `mu_hat = MB / MC - 1`, `comm_bar = MC / qB` (the observed commission per agent member), `phi = mu_hat + beta_f`, `lev` the off-exchange to on-exchange member ratio from `commission_filings.csv`, `wedge` the carrier constant.
- Files: `s4_cost-gmm.R`, `10_commissions.R`, `cf_year.R`, `cf1_estimate.R`.

---

## 11. Counterfactual equilibria (`cf1_estimate.R`, `cf_cell.R`, `cf_year.R`)

`cf1_estimate.R` solves each supply year on its own cluster, one worker per
region cell. Each worker builds its cell once (`cf_cell_init`: the section 7
cell data at the same seed and 20 percent draw, the observed premiums and
commissions, the plan attributes), installs a scenario (`cf_cell_scenario`),
and evaluates the pricing and commission pieces at any base-premium vector
(`cf_cell_eval_p1`, then the statewide transfer sums on the master, then
`cf_cell_eval_p2`). The master solves the plan-year base premiums
(`solve_cf_year`) and, for the baseline and the composition scenarios, the
commission scale of each commission-setting unit (`solve_cf_commissions`).
Inputs beyond section 7: the GMM cost coefficients, the reinsurance factors,
`admin0_pmpm`, `beta` by carrier-year (`commission_beta.csv`), the carrier
constants (`commission_wedge.csv`), `base_premium` and `region_factor` from
`supply_results.csv`.

### Scenario definitions: `scenario`, `tau`, `comm_scale_cf`
- Paper: the rows of the counterfactual tables.
- Chain: `build_scenario_data` (`cf_cell.R`) rewrites each household's commission and channel for a scenario. Commission bases: `observed`; `zero`; `scale` (observed times a factor, 0.5 in `SCALE_GRID`); `uniform` (the cell mean of observed positive commissions times `UNIFORM_LOW_SC`, 0.5, with band-edge runs at 0.75 and 1.25 of that); `flatbar` (a flat fee per carrier at its observed mean commission per agent member, `flat_mandate`); `aligned` (proportional to each plan's mean non-commission utility, holding the cell's commission budget); `firmscale` (observed times the per-unit multiplier `k` of a commission solve). Proportional bases scale each household's `comm_hh`; level bases set a flat per-member amount. `tau` converts that share of agent-assisted households to navigators (highest `p_nav` first) and, unless `broker_remain`, the rest to unassisted (`zero_tau0.00`, `zero_tau0.50`, `zero_tau1.00`; `endog_tau0.50` keeps the rest as agents); `defund` converts that share of navigator households to agents (lowest `p_nav` first, `defund_0.50`). The channel-state weights `p_none_hat`, `p_nav_hat` and `p_agent_hat` move with the same conversions. `comm_scale_cf` records the scenario's multiplier on observed schedules (NA when the schedule is not a multiple).
- Files: `cf_cell.R`, `cf1_estimate.R`, `score_cf.R`, `sum2_results.R`.

### Solved premiums: `premium_obs`, `premium_cf`, `premium_change`, `base_premium_cf`, `resid_dollars`, `kink` (`counterfactual_results.csv`, `cf_pricing_residuals.csv`)
- Paper: the premium column of the counterfactual table; `\cfZeroPremChg`; the premium-change figure; the baseline fit check.
- Chain: `solve_cf_year` (`cf_year.R`) iterates each solved plan's best response in its base premium on the pricing condition in dollars (`G / omega_w` at the evaluated premiums, `cf_year_aggregate`), with a damping fraction chosen per plan and steps capped at $25; a silver plan whose condition changes sign across the premium where the benchmark switches is bisected to that kink and left out of the convergence measure; Broyden's method from the stalled point (`nleqslv`, Jacobian `cf_year_jacobian_P` by forward differences) runs only if the iteration stalls; a scenario is accepted at a maximum residual of $5 per member-month. Plans are solved only when their scenario share clears `SHARE_FLOOR_FOC`; the rest hold their observed base premium. `premium_cf = base_premium_cf * region_factor` per cell, `premium_change = premium_cf - premium_obs`. The `resid_<year>_<scenario>.csv` files record the residual and the kink flag per plan; `cf1` binds them to `results/cf_pricing_residuals.csv`. The baseline is the model's own equilibrium at observed commissions, then at the solved commissions below; each year's baseline premiums are saved to `fixed_point_<year>.csv` as the next run's warm start.
- Level: plan-cell, dollars per month (posted, 40-year-old basis).
- Files: `cf_year.R`, `cf1_estimate.R`, `cf_cell.R`, `sum2_results.R`.

### Solved commissions: `k_base`, `eta_obs`, `eta_base`, `phi_base`, `k_cf`, `phi_cf` (`cf_baseline_commissions.csv`, `cf_firm_profits.csv`)
- Paper: the baseline commission fit check; the re-optimized commissions of the composition scenarios.
- Chain: `solve_cf_commissions` (`cf_year.R`) holds each commission-setting unit's scale `k` on observed rates and iterates: re-solve premiums, evaluate each unit's condition `phi = MB / MC - (1 - beta_f) - delta_f` (`cf_year_aggregate`), update `k` by a damped step (`damp` 0.5, moves capped at 1, `k` in [0.02, 10]), until the largest `|phi|` is below 0.05 or 20 rounds. Units are the carriers with a `beta` and a constant (`firms_solve`); `eta_base = MC / qB`, the solved commission per agent member; `k_base` its ratio to observed. `cf_year_firm_rows` records each carrier's monthly profit and agent members per scenario.
- Level: commission-setting unit by year, dollars per agent member per month.
- Files: `cf_year.R`, `cf1_estimate.R`, `cf_cell.R`.

### Scenario shares and costs: `share_obs`, `share_cf`, `mc`, `claims`, `commission_pmpm`, `markup_cf`
- Paper: the coverage effects; the cost decompositions.
- Chain: `cf_cell_eval_p1` recomputes household premiums (`update_premiums`, with the subsidy from the benchmark re-identified at the candidate premiums for subsidized households), utility, shares, elasticities and the predicted composition at the scenario's commissions; `cf_cell_eval_p2` recomputes the transfer, claims and marginal cost (`compute_mc`) with the cell's contribution to the statewide sums updated. `mc` in the rows includes the administrative cost per member; `claims` is predicted claims; `commission_pmpm` is the scenario's plan commission basis; `markup_cf = premium_cf - mc`.
- Level: plan-cell, shares of the cell's members and dollars per member month.
- Files: `cf_cell.R`, `cf_year.R`, `cf1_estimate.R`, `cf2_score.R`, `score_cf.R`.

---

## 12. Welfare and fiscal quantities (`cf2_score.R`, `score_cf.R`, `welfare.R`, `cf3_se.R`)

`cf2_score.R` re-scores every cell and scenario from the solved premiums and
commissions in `counterfactual_results.csv` without re-solving
(`score_cf_cell`): it rebuilds the cell from the cached cell file, applies the
scenario's conversions and commissions (`build_scenario_data`), re-levels the
premiums (`update_premiums`), and scores. Additional inputs:
`data/input/ca_standard_cost_sharing.csv` (deductible, coinsurance and
maximum out-of-pocket by year, metal and HSA, the Covered California standard
benefit designs), `data/input/meps_spending_by_demographics.csv` and
`data/input/meps_uninsured_oop.csv` (MEPS 2018 HC-209, built by the standalone
`code/data-build/build-meps-spending.R` and `build-meps-uninsured-oop.R` from
`D:/research-data/meps/h209.dta`), and the calibration constants in
`welfare.R` and `constants.R`.

### Consumer surplus: `cs_weighted`, `cs_nocomm`
- Paper: the "CS" column of the counterfactual table; the welfare-gradient figure.
- Chain: `compute_consumer_surplus` (`score_cf.R`) computes each household's expected surplus from the two-part model as the log-sum over the outside option and the inclusive value of the plans, converted to dollars by the household's base price coefficient (`compute_alpha_i` with the channel slopes excluded), per member per year. `cs_weighted` keeps every estimated term; `cs_nocomm` drops the commission terms from the agent state (`welfare_drop = COMM_TERMS`) and is the reported measure. Cell values are member-weighted means; `sum2_results.R` averages cells and differences from the baseline.
- Level: region-year-scenario, dollars per member per year.
- Files: `score_cf.R`, `cf2_score.R`, `cf3_se.R`, `welfare.R`, `sum2_results.R`.

### Navigator-rule welfare: `cs_welfare_nav`
- Paper: the "V^nav" column of the counterfactual table.
- Chain: `vN_navigator_coefs` (`welfare.R`) folds the navigator premium and AV terms into the base coefficients and zeroes the agent and commission terms; `scenario_welfare` values the plan each household actually chooses (probability-weighted) under those coefficients and converts to dollars by the navigator price slope. Reported per member per month in the cell file; `sum2_results.R` multiplies by 12.
- Files: `welfare.R`, `score_cf.R`, `sum2_results.R`.

### Objective welfare: `cs_welfare_obj`, `obj_prem`, `obj_eoop`, `obj_risk`, `obj_insured`, `share_unins`, `unins_oop`, `unins_mort`, `unins_cat`
- Paper: the "V^obj" column, the low/central/high band table, and the welfare SE table.
- Chain: `vN_objective` (`welfare.R`) values each plan row in annual dollars as minus the household's annual premium per member (`obj_prem`), minus expected out-of-pocket spending (`obj_eoop`), minus a risk term of `RHO_RISK_AVERSION / 2` times the variance of out-of-pocket spending (`obj_risk`), where out-of-pocket follows the plan's deductible, coinsurance and maximum from the cost-sharing table applied to a lognormal spending distribution with mean from the MEPS schedule by age mix and income bracket (`household_spending`) and coefficient of variation `SPENDING_CV`. The uninsured row is valued by the MEPS realized out-of-pocket of the uninsured, the catastrophic rate times `DISTRESS_COST`, and the social cost of being uninsured (risk protection plus age-weighted baseline mortality times a mortality reduction times a value of statistical life, `UNINS_*` constants at the central scenario). `scenario_welfare` returns the probability-weighted value at actual choices and its components; `obj_insured` is the insured rows' value, `share_unins` the uninsured probability, and `unins_oop`, `unins_mort`, `unins_cat` the uninsured-weighted out-of-pocket, baseline mortality and catastrophic rate, from which `sum2_results.R` and `cf3_se.R` rebuild the objective under the low, central and high uninsured-cost cases.
- Level: region-year-scenario, dollars per member per year (per household in `cf_welfare_hh/`).
- Files: `welfare.R`, `score_cf.R`, `cf2_score.R`, `cf3_se.R`, `sum2_results.R`, `constants.R`.

### Producer surplus and government cost: `producer_surplus`, `gov_subsidy`, `gov_csr`, `gov_penalty`, `gov_uc_raw`, `rs_unins`, `gov_uc`, `gov_total`
- Paper: the fiscal table.
- Chain: in `score_cf_cell`, producer surplus is each plan's revenue at the age-rated premium households pay (`p_ch * premium_posted / 1.278 * rating_factor`) less `mc` times predicted members, less `(1 - beta_f)` times the commission times agent members, summed and expressed per member of the cell per year. Government cost is the advance credit paid (the household's `subsidy_cf` capped at its premium, probability-weighted), the cost-sharing reduction share of predicted claims for CSR silver enrollees (`CSR_GOV_SHARE`), uncompensated care per uninsured member (`UC_PER_UNINSURED`, Coughlin et al. 2014 inflated by per-capita national health expenditure) scaled by the uninsured pool's predicted risk score relative to the baseline (`rs_unins` from the risk-score equation at the pool's demographics), less mandate penalty revenue; `gov_total` sums them.
- Level: region-year-scenario, dollars per member per year.
- Files: `score_cf.R`, `cf2_score.R`, `constants.R`, `ra.R`, `sum2_results.R`.

### Headline aggregates and bootstrap: `cf_bootstrap_draws.csv`, `cf_bootstrap_se.csv`, `counterfactual_welfare_se.tex`
- Paper: the standard errors on the coverage and objective welfare effects.
- Chain: `summarize_cf_headline` (`welfare.R`) turns a welfare table into named statistics, each a mean over cells of a scenario's column minus the baseline (the tau gradient of `cs_nocomm`, the value of assistance as the difference between tau 1 and tau 0, and the five band components `dshare`, `dobjins`, `doop`, `dmort`, `dcat` per scenario). `cf3_se.R` draws demand coefficient vectors from the sandwich covariance (`N_BOOT_CF` draws, lambda kept inside (0.001, 0.999)), re-scores every cell at the cf1 premiums and commissions with each draw, and appends the statistics to `cf_bootstrap_draws.csv`; draws already in the file from the current inputs are kept on a restart. `sum2_results.R` computes the standard error of each scenario's coverage and central objective effect across the draws.
- Files: `welfare.R`, `cf3_se.R`, `sum2_results.R`.

### Counterfactual tables and macros: `counterfactual_results.tex`, `counterfactual_welfare_band.tex`, `counterfactual_fiscal.tex`, `cf_welfare_gradient.png`, `cf_premium_change.png`, `\cfZeroPremChg`
- Chain: `sum2_results.R` averages each welfare column over cells by scenario, differences from the baseline, and pairs it with the share-weighted mean premium change from `counterfactual_results.csv`; the band table rebuilds the objective at the three uninsured-cost cases; the fiscal table reports the producer-surplus and government components; the two figures (`cs_nocomm` against tau; the premium change by scenario) are written but not placed in the paper; `\cfZeroPremChg` is the share-weighted premium change under `zero_tau1.00`.
- Files: `sum2_results.R`.

---

## 13. Supplement and retention outputs

### Demand specification build-up: `demand_spec_fits.csv`, `demand_spec_sensitivity.csv`, `demand_spec_sensitivity.tex`
- Paper: the demand-specification table in the supplemental appendix.
- Chain: `supp1_demand-specs.R` fits four nested specifications on the section 7 cells with the same estimator (plan attributes and premium; plus the demographic interactions and enrollment shifters; plus the channel and commission terms, which reproduces the body estimates; plus `cf_resid`, the Hausman control function from `build3_data-prep.R`), and reports the enrollment-weighted mean own-price elasticity, `lambda`, the navigator and agent effects on the silver share in percentage points (the within-nest silver share with and without the channel terms), and the two commission coefficients.
- Files: `supp1_demand-specs.R`, `estimate_demand.R`, `build3_data-prep.R`.

### PBE robustness: `pbe_robustness.csv`, `pbe_robustness.tex`
- Chain: `supp4_pbe-robustness.R` marks household-years with a PBE service channel in the raw enrollment file, drops them from copies of the cells (`choice_cells_noPBE/`), re-estimates the body specification on both samples, and tabulates the premium, AV, channel, commission and nesting parameters.
- Files: `supp4_pbe-robustness.R`, `supp1_demand-specs.R`.

### Cost-sharing schedule: `cost_sharing_schedule.tex`
- Chain: `supp2_cost-sharing-table.R` formats the 2019 rows of `ca_standard_cost_sharing.csv`.
- Files: `supp2_cost-sharing-table.R`.

### Service-channel table: `assistance_channels.tex`
- Chain: `supp3_channel-table.R` tabulates the raw `service_channel` codes and their shares of records in `pra_07192019.csv` with the category each maps to (section 3).
- Files: `supp3_channel-table.R`.

### Simulations: `menu_simulation.tex`, `gatekeeper_selection.tex`, `commission_inertia_sim.tex`
- Chain: `supp5_menu-simulation.R` and `supp6_commission-inertia-sim.R` generate their own data from stated primitives and use no project variables; the first fits the commission-in-utility model on choices from a restricted-menu process, the second compares static and dynamic commission effects.
- Files: `supp5_menu-simulation.R`, `supp6_commission-inertia-sim.R`.

### Channel persistence: `channel_transitions.csv`, `channel_retention.csv`, `channel_status_next.csv`
- Paper: not placed in the paper or the appendix at present.
- Chain: `sum3_retention.R` reads `enrollment_hh.csv` and `cc_uninsured.csv` and tabulates each household's channel in consecutive enrolled years and its next-year enrollment status (enrolled, off but market-eligible) by channel.
- Files: `sum3_retention.R`.
