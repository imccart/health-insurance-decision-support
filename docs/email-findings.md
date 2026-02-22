# Email Findings

Extracted from 119 project emails in `_emails/txt/`. Covers correspondence from ~2019-2023.

---

## 1. Data Construction Decisions

### Plan Name Variables (Evan Saltzman, May 2020)
- Three plan name variables exist in the data objects: `Plan_Name`, `Plan_Name2`, `Plan_Name_Small`
- `Plan_Name`: Most detailed, differentiates PPO, EPO, HMO, HSP with suffixes (no suffix = PPO, 2 = EPO, 3 = HMO, 4 = HSP)
- `Plan_Name2`: Consolidates PPO/EPO/HSP together (recommended for matching household to plan objects)
- `Plan_Name_Small`: Further consolidation for supply-side analysis
- Three insurers offer multiple network types: Anthem, Blue Shield, Health Net
- PPO and EPO generally not offered in same market-year, so treated as same
- `plan_name` in household object corresponds to `Plan_Name2` in plan object
- Example issue: BS_SIL87 in household data for region 1 in 2014, but plan data only has BS_SIL2 variants (EPO suffix). The mismatch caused 5,994 households to be dropped.

### Rate Filing Data (comment in code)
- `2014-2020.RData` (merged rate filing PUFs): Comment in `process.rate.data.R` says "I need to ask Sam how he constructed the merged file"
- Origin of this merged file is unclear; may need to be reconstructed from CMS PUFs

### FOIA Data
- Primary enrollment data: `pra_07192019.csv` obtained via FOIA from Covered California
- The "pathway" variable (navigator/agent/broker/unassisted) was added in a later data request; Evan "convinced Covered California to include" it
- Data covers on-exchange enrollment only (not off-exchange)

---

## 2. Commission Data

### Source: Kevin Knauss (InsureMeKevin.com), May 2020
- Licensed Covered California agent (CA LIC 0H12644) who helped provide commission schedules
- Evan reached out to him for commission data by insurer, 2014-2020
- Kevin agreed to compile from his commission statements (at least 2019-2020, with historical data where available)
- Key insights from Kevin:
  - **Sutter Health Plus** should be added (off-exchange only, Bay Area and Sacramento)
  - **Western Health Advantage** was paying higher commission for off-exchange vs on-exchange
  - Bonus/incentive programs exist beyond base commissions: Blue Shield (HMO products), Health Net, Oscar, Molina (usually 25-100 new enrollment minimums)
  - Carriers with straight PMPM commissions have removed incentive to upsell to higher metal tiers
  - Agents guide CSR-eligible consumers toward Enhanced Silver over Gold/Platinum (confirms the dominated choice hypothesis)

### Commission Structure
- Two types: flat PMPM (per member per month) and percentage of premium
- Presentation slides show commission elasticity: 11.18 for agent users, 5.39 for all enrollees
- `commission_input.csv` (referenced in code but not in repo) appears to be the compiled commission schedule

---

## 3. Identification and Methodology

### Bounds in Discrete Choice (Dan Millimet, SMU, March 2021)
- Ian explored partial identification bounds for treatment effect of decision assistance
- Question: Can discrete choice framework yield tighter bounds than binary outcome (since upper bound on P(plan=A) < 1)?
- Millimet's response: No, worst-case bounds remain the same. P(Plan=A|D=0) still has upper limit of 1 as a counterfactual. The fact that the choice set has more alternatives doesn't bound this probability below 1 without additional assumptions.
- Conclusion: Tighter bounds would require additional assumptions beyond worst-case; this approach was apparently not pursued further in the paper.

---

## 4. External Interest

### Congressional Budget Office (Asha Saavoss, Sept 2023)
- Chief of Medicare unit at CBO asked Ian about Medicare Advantage broker fees
- Referred by "Chapin" (likely Chapin White)
- Looking for rough estimates of total MA plan spending on broker/agent fees
- Indicates broader policy relevance of the broker commission research

---

## 5. Open Questions from Emails

1. **Commission data completeness**: Did Kevin Knauss provide all years 2014-2020 for the Big Four (Anthem, Blue Shield, Kaiser, Health Net)? Status of `commission_input.csv` unclear.
2. **Sam's merged rate filing**: Who is Sam and how was `2014-2020.RData` constructed? Can it be reproduced from CMS PUFs?
3. **Off-exchange enrollment**: Kevin mentions off-exchange plans (Sutter, Western Health Advantage with different commission structures). The data only covers on-exchange. Is this a limitation worth noting?
4. **Bonus/incentive programs**: Beyond base commissions, insurers offer bonuses for volume. These are not captured in commission_input.csv. Material omission?
5. **Agent/navigator directory data**: Funding proposal mentions needing this via web-scraping or PRA request. Was it ever obtained?

---

## 6. Email Categories (for reference)

Most emails fell into these categories:
- **Evan Saltzman collaboration** (~40%): Data construction, code questions, plan name matching, commission data collection
- **Conference/presentation** (~15%): ASHEcon, SHESG, CREED, ASSA abstracts and logistics
- **Covered California FOIA** (~10%): Data requests and follow-up
- **External researchers/policymakers** (~10%): CBO inquiry, other academics
- **Methodology discussions** (~5%): Bounds, identification, partial identification
- **Administrative/other** (~20%): Scheduling, grant logistics, unrelated correspondence
