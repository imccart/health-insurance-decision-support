# Overleaf Export Inventory

Generated from `_overleaf/` export on 2026-02-15.

---

## 1. Paper and Appendix

| File | Purpose |
|------|---------|
| `finals/paper.tex` | **Main paper**: "Decision Assistance and Steering in Health Insurance" (McCarthy & Saltzman, June 2021) |
| `finals/appendix.tex` | **Supplemental appendix**: outside option construction (App A), structural model derivatives (App B) |
| `finals/BibTeX_Library.bib` | Bibliography (10,701 lines; full copy of canonical library) |

### Paper Section Structure
1. Introduction -- Motivation, overview of results, literature review
2. Background and Institutional Details -- Choice sets, premiums/cost-sharing, decision assistance
3. Data -- Covered California 2014-2019, 8.3M household-years
4. Decision Assistance and Plan Choice -- Nested logit + AIPW estimation of ATT
5. Dominated Choices -- Dominated choice definition and ATT estimation
6. Welfare and Insurer Steering -- Two-stage structural model
   - 6.1: Model (firm decision variables, risk adjustment, variable profit, FOCs)
   - 6.2: Estimation (GMM with four moment conditions)
   - **6.3: Welfare Results -- EMPTY**
7. Conclusion and Discussion -- Preliminary; solicits feedback (seminar draft)

### Completeness Assessment
- **Demand-side (Sections 1-5):** Substantially complete with full text, methods, results, and figures
- **Supply-side (Section 6):** Model and estimation methodology fully written; **results subsection is empty**
- **Conclusion:** Written as workshop draft with numbered limitations list
- **Overall:** ~60-70% complete working paper

---

## 2. Tables and Figures Referenced in Paper

**Tables:**
- Table 1: `summary_stats.tex` -- Summary statistics (bare tabular, correct format)

**Figures:**
- Figure 1: `flat_comm.jpg` + `perc_comm.jpg` -- Commission rates
- Figure 2: `enrollee_count.png` -- Enrollment counts
- Figure 3: `ps_assist_clean.png` -- Propensity scores
- Figure 4: `cov_balance.png` -- Covariate balance
- Figure 5: `choice_insurer.png` -- ATT by insurer
- Figure 6: `choice_metals.png` -- ATT by metal tier
- Figure 7: `dom_choice.png` -- ATT on dominated choices
- Figure 8: `CA_Rating_Regions2.jpg` -- Rating area map

**Unreferenced table files** (used in presentations only):
- `metal_assistance.tex` -- Metal tier distribution by assistance type
- `dominated_choice_regression.tex` -- **Bug:** nested `\begin{table}[H]` wrapper
- `switching_regression.tex` -- **Same nested table bug**

---

## 3. Presentations

Two xaringan (R Markdown) slide decks (not Beamer):
1. **Emory Lunch & Learn** (`finals/lunch-and-learn/lunch-learn-202103.Rmd`), March 2021
2. **CREED** (`finals/creed/creed-202105.Rmd`), May 2021

Both show commission elasticity: 11.18 (agent users), 5.39 (all enrollees).

---

## 4. Conference Abstracts

| File | Event |
|------|-------|
| `finals/abstract-ashecon-201910.tex` | ASHEcon, October 2019 |
| `finals/abstract-shesg-202006.tex` | SHESG, June 2020 |
| `finals/abstract-emory-lunchlearn-202103.tex` | Emory Lunch & Learn, March 2021 |
| `finals/abstract-ashecon-assa-2022.tex` | ASHEcon/ASSA, 2022 |

---

## 5. Funding Proposal

**File:** `funding/equitable-growth.tex`
**Target:** Washington Center for Equitable Growth

Frames project with racial disparities angle. Mentions needing agent/navigator directory data via web-scraping or PRA request. Uses project-specific bibliography `navbib.bib` (~140 entries).

---

## 6. Key Takeaways for Rebuild

1. Reduced-form analysis is complete with results, figures, and text
2. Structural model is fully specified in math but has **no estimation results**
3. Two table files have nested `\begin{table}` bugs
4. Julia archive files confirm Saltzman used Julia for demand estimation
5. Old pipeline uses `renv` and `pacman` (superseded by groundhog)
6. `build-data.R` also renders abstracts and paper via R Markdown
