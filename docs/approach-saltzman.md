# Saltzman Reference: Demand Model and Original Code

This document is the canonical record of Evan Saltzman's published demand model
(JHE 2019, RAND 2021) and the original code we inherited (`_old-repo/`). It is
intended as a self-contained reference. A reader should be able to consult this
document instead of re-reading the papers when checking what Saltzman did, what
the parameter values were, and how the old data-build maps to those papers.

All citations are to:

- **JHE 2019**: Saltzman, E. (2019). "Demand for health insurance: Evidence
  from the California and Washington ACA exchanges." *Journal of Health
  Economics*, 63, 197--222.
- **RAND 2021**: Saltzman, E. (2021). "Managing adverse selection:
  underinsurance versus underenrollment." *RAND Journal of Economics*, 52(2),
  359--381.

Old-repo file paths are relative to `_old-repo/`.

---

## 1. Model

### 1.1 Setup (JHE 2019, Section 4)

Saltzman writes the model at the **household** level (not individual). The
three reasons (JHE p. 200) are (1) household members make joint decisions about
coverage, (2) decisions across members are highly correlated (a 5-year-old does
not choose alone), and (3) subsidies and penalties are calculated at the
household level.

Household $i$ chooses plan $j$ from a menu $J_{i}$ that includes all available
exchange plans plus an outside option ("uninsured"). The indirect utility from
an exchange plan is

$$U_{ij} = \alpha_i p_{ij} + x_j' \beta + d_i' \varphi + \xi_j + \epsilon_{ij}, \qquad (\text{JHE Eq. 1})$$

and the utility from the outside option is

$$U_{i0} = \alpha_i \rho_i + \epsilon_{i0}, \qquad (\text{JHE Eq. 2})$$

where:

- $p_{ij}$ is the **average premium per household member** (HH total premium
  divided by HH size)
- $\rho_i$ is the **average penalty per household member** for being uninsured
- $x_j$ is a vector of observed plan characteristics (AV, HMO indicator,
  insurer dummies, deductible ratio, max-OOP ratio in WA)
- $d_i$ is a vector of demographic characteristics (income brackets, age
  brackets, gender, race, smoker, family, year)
- $\xi_j$ is unobserved plan quality
- $\alpha_i = \alpha + d_i'\gamma$ allows premium sensitivity to vary with
  demographics
- $\varphi$ holds the demographic main effects (which appear only in the
  inside-plan utilities, not in $U_{i0}$, so they are identified)
- $\gamma$ holds the premium $\times$ demographic interactions
- $\epsilon$'s are GEV (generalized extreme value)

### 1.2 Nested logit structure (JHE Section 6, RAND Section 5)

Two nests:
1. **Inside nest**: all exchange plans
2. **Outside nest**: only the uninsured option (single-element nest)

The two-nest structure is chosen (JHE p. 201) over the natural alternative of
"each metal tier as a nest plus outside option" because the *primary observed
substitution pattern* in the data is between the silver tier and the outside
option, due to the ACA's linkage of CSRs to silver plans. A standard
multinomial logit on all $J+1$ alternatives would impose IIA / proportional
substitution to the outside option, which the silver-CSR design clearly
violates.

The household choice probability is

$$q_{ij}(\mathbf{p}; \boldsymbol\theta) = \frac{e^{V_{ij}/\lambda} \left(\sum_{k} e^{V_{ik}/\lambda}\right)^{\lambda - 1}}{1 + \left(\sum_{k} e^{V_{ik}/\lambda}\right)^{\lambda}} \qquad (\text{JHE Eq. 6, RAND Eq. 14})$$

where $V_{ij} \equiv \alpha_i p_{ij} + x_j'\beta + d_i'\varphi + \xi_j$ and
$\lambda$ is the nesting parameter for the exchange (inside) nest. Lower
$\lambda$ means more substitution within the inside nest relative to between
inside and outside.

Decomposed (standard nested logit factoring):

$$P(j \mid \text{inside}) = \frac{e^{V_{ij}/\lambda}}{\sum_k e^{V_{ik}/\lambda}}, \qquad
\text{IV}_i = \log \sum_k e^{V_{ik}/\lambda}, \qquad
P(\text{inside}) = \frac{e^{\lambda \cdot \text{IV}_i}}{1 + e^{\lambda \cdot \text{IV}_i}}.$$

The "+1" in the denominator of Eq. 6 corresponds to $V_{i0} = \alpha_i \rho_i$
being absorbed (because the outside nest contains a single plan and
$U_{i0}/\lambda_0$ with $\lambda_0 = 1$ for the singleton). In the new repo's
implementation, $V_{i0}$ is computed as $\alpha_i \rho_i$ and treated as a
separate term in the denominator, which is algebraically equivalent.

Estimation is by maximum likelihood:

$$LL(\boldsymbol\theta) = \sum_{i,j} w_i c_{ij} \log q_{ij}(\mathbf{p}; \boldsymbol\theta),$$

with $w_i$ the household weight and $c_{ij}=1$ if household $i$ chose plan $j$.

### 1.3 Premium definition (JHE Eq. 3)

The household premium is

$$p_{ij} = \max\left\{ \underbrace{r_i p_j}_{\text{full premium}} - \underbrace{\max\{r_i p_b - \text{cap}_i, 0\}}_{\text{subsidy}}, \; 0\right\} / N_i, \qquad (\text{JHE Eq. 3})$$

where:

- $r_i$ = household rating factor (sum of age $\times$ smoking $\times$
  geographic factors across members)
- $p_j$ = insurer's base premium for plan $j$ (the "21-year-old non-smoker"
  base)
- $p_b$ = base premium of the **benchmark** (second-lowest-cost silver) plan
  in the household's rating area
- $\text{cap}_i$ = household's income contribution cap (a function of FPL,
  ranging from 2% of income at 100% FPL to 9.5% of income at 400% FPL in 2014)
- $N_i$ = number of household members

The outer max enforces non-negativity (the subsidy cannot exceed the full
premium). The division by $N_i$ converts to per-member, consistent with
$\alpha_i p_{ij}$ representing the dollar response per HH member.

This is **net of subsidy**, **per HH member**, **monthly**. The estimated
$\beta_{premium}$ is therefore "utils per dollar of monthly per-member
premium." Reported elasticities/semi-elasticities (Tables 4-5) convert this:
the per-$100 monthly semi-elasticity multiplies $\beta$ by 100 and the
per-$100 *annual* version multiplies by $100/12 \approx 8.33$ (see JHE
Appendix E, Eq. 10).

### 1.4 Mandate / "taste for compliance" (JHE Eqs. 4-5)

Saltzman tests three specifications:

1. **Mandate intercept only**: a $d_{mi}$ indicator with coefficient
   $\varphi_m$ added to $d_i$ in $U_{ij}$ (mandate as a discrete demographic
   shifter). Does not include penalty as a price.
2. **Separate penalty parameter**: replace $\alpha_i$ in $U_{i0}$ with
   $\alpha'_i$:
   $$U_{i0} = \alpha'_i \rho_i + \epsilon_{i0} \qquad (\text{JHE Eq. 4})$$
   so consumers respond to the penalty *amount* differently than to the
   premium.
3. **Both**: mandate intercept and separate $\alpha'_i$ together.

The "taste for compliance" is the existence-of-penalty effect: $\varphi_m > 0$
implies utility from being insured rises just because the mandate is in force,
beyond the dollar value of the penalty. Saltzman defines the compensating
variation as

$$\tau_i = \frac{U^m_i - U^1_i}{\alpha_i} = \frac{\varphi_m}{\alpha_i} \qquad (\text{JHE Eq. 5}).$$

He converts this to dollars/month: about **\$13/month in WA** and **\$64/month
in CA** (JHE p. 203).

---

## 2. Identification

### 2.1 Premium parameter $\alpha$ (JHE Section 6.1)

Premiums vary across insurers, markets, and households for endogenous and
exogenous reasons. Saltzman lists five sources of *exogenous* household-level
premium variation he exploits:

1. **400% FPL subsidy cliff**: subsidies abruptly stop above 400% FPL,
   creating a discontinuity in net premium for otherwise-similar households
   (JHE Fig. 6 in Appendix F shows a discontinuity in enrollment around 400%
   FPL).
2. **Age 21 rating curve breakpoint**: the CMS default rating curve has a 57%
   jump between ages 20 and 21 (JHE Fig. 3 shows enrollment drop at 21).
3. **Tax filing threshold mandate exemption**: households below the IRS
   filing threshold are exempt from the mandate (JHE Fig. 4 shows an
   enrollment jump at the threshold in CA).
4. **Affordability threshold mandate exemption**: households for whom the
   cheapest exchange plan exceeds ~8% of income are exempt (JHE Fig. 5).
5. **Mandate penalty phase-in across years**: penalty grew from 1% of income
   in 2014 to 2.5% by 2016, then fell to 0 in 2019. Provides time-series
   variation in price differential between insured and uninsured.

To control for non-linear premium variation by age, smoking, geography, and
income, Saltzman includes those characteristics in the utility function. RD
analysis (JHE Table 18) confirms enrollment responds at the 400% FPL and tax
filing thresholds but **not** at the age-21 break or the affordability
threshold (in CA), suggesting age-21 is contaminated by other shifts and the
affordability test is too cognitively complex.

### 2.2 Control function approach (JHE Section 6.1, Appendix B)

As a robustness check, Saltzman implements Petrin & Train (2010) control
function for premium endogeneity. He cannot use Berry, Levinsohn, Pakes (1995)
because there is significant **household-level** variation in premiums and in
penalty assessments, which precludes absorbing premium endogeneity into
product-level constants.

First stage: regress $p_{ij}$ on instruments $z_{ij}$. Instruments are all
non-premium variables (assumed exogenous) plus **geographic cost factors**
reported in CA and WA insurer rate filings, which measure each plan's cost
relative to its cost in other rating areas where the same insurer
participates. This proxies for insurer-area bargaining power with providers
and is correlated with premium but not with idiosyncratic demand for the plan
in this area.

Compute residuals $\mu_{ij}$. Assume $(\mu_{ij}, \xi_{ij})$ are jointly
normal, so $\xi_{ij} \mid \mu_{ij}$ is also normal with mean
$\upsilon \mu_{ij}$ and variance $\psi^2$. Then

$$U_{ij} = \alpha_i p_{ij} + x_j'\beta + d_i'\varphi + \upsilon \mu_{ij} + \psi \eta_{ij} + \epsilon_{ij}, \qquad (\text{JHE Eq. 7})$$

with $\eta_{ij} \sim N(0,1)$. Choice probability is integrated by simulation
over $\eta$.

Effect: the control function correction roughly *doubles* the premium
sensitivity. CA mean own-premium elasticity moves from $-9.1$ (base) to
$-10.6$ (CF); WA from $-7.2$ to $-8.1$ (JHE Tables 4 vs. 16).

### 2.3 Nesting parameter $\lambda$

$\lambda$ is identified by the *cross-nest* substitution moment, namely the
relative size of switching from inside plans to the outside option vs.
switching across inside plans, when premiums or other shifters move. Identifying
variation comes from:

- Households who *do* enroll in an exchange plan vs. those who don't (binary
  outside/inside margin), conditional on the shifters.
- The relative rates at which premium increases push consumers across plans
  inside the nest vs. out of the nest entirely.

Saltzman estimates $\hat\lambda \in (0, 1)$ in all specifications:

- JHE 2019, CA: $\hat\lambda = 0.308$ (s.e. 0.022) base, $0.328$ cheapest
  spec, $0.297$ control function (Table 12, 15).
- JHE 2019, WA: $\hat\lambda = 0.356$ base, $0.347$ control function.
- RAND 2021: $\hat\lambda$ values not reported as a single column header, but
  implied by the Table 2 elasticities.

A $\hat\lambda < 1$ confirms more within-nest substitution than the multinomial
logit benchmark would predict. (Our new repo had $\hat\lambda > 1$ in early
runs, which violates the random utility consistency of the model.)

### 2.4 Demographic interactions

Identified by within-demographic-group variation in choices and premiums.
$\gamma$ (premium $\times$ demographic) is identified by the differential
slope of demand with respect to premium across groups; $\varphi$ (demographic
intercept) by the level of insured-vs-uninsured rates after netting out price.

Note: Saltzman reports being **unable to control for health status or
ex-post realized risk** (JHE p. 205, footnote in Section 9). His taste-for-
compliance estimates may be upward biased if uninsured groups (smokers, young
adults, low-income) are systematically healthier.

---

## 3. Outside Option / Uninsured Sample

**Two different approaches across his two papers.** Important to keep straight:

- **JHE 2019**: uninsured pool drawn from **ACS** (filtered to ACA-eligible
  uninsured citizens, with SIPP-imputed ESI access and immigration status). The
  ACS HHs are merged with CC enrollees. The estimation sample then **subsamples
  CC and ACS households equally** (not proportional to population), so that CC
  and ACS contribute comparable numbers of raw HH-year observations to the
  likelihood. Population PERWT weights then re-weight ACS up. (See
  `_old-repo/data-code/run.make.data.objects.small.R:42-44`: `sample_size <-
  55000` total HHs, drawn equally from both groups.) This matters because the
  raw 5-7% uninsured rate in ACS would otherwise be swamped 70:1 by CC
  enrollees in the unsampled data.

- **RAND 2021** (Online Appendix E): explicitly drops the ACS approach because
  "the sample of uninsured in the ACS is limited and ACS geographic identifiers
  are difficult to match with those in my administrative data." Instead, the
  uninsured pool is constructed from **Covered California HHs in years they
  were NOT enrolled** — i.e., panel structure across 2014-2019. A CC HH
  observed enrolled in 2015-2016 contributes uninsured observations for 2014,
  2017, 2018, 2019 (subject to market eligibility). The SIPP **transitioned**
  logit (`impute.SIPP.R:760-763`) is used to drop HH-years where the HH likely
  lost market eligibility (gained ESI, moved out of CA, turned 65 → Medicare,
  income dropped to Medicaid). Stochastic Bernoulli draw on the predicted
  transition probability removes those HH-years before estimation. ACS is no
  longer used for the uninsured pool.

### 3.1 ACS-based, SIPP-augmented (JHE Section 5, RAND Web Appendix E)

Saltzman uses the **American Community Survey (ACS)** as the universe of
potential exchange consumers from outside the exchange. He filters ACS to:

1. **Drop anyone enrolled in or eligible for another source of coverage**:
   Medicaid, CHIP, ESI, Medicare, TRICARE, IHS.
2. **Drop undocumented immigrants** (ineligible for exchange).

The remaining ACS households become the "uninsured" sample. Merge with
exchange enrollees to form the universe.

ACS limitations addressed:
- Lacks **smoking status** $\to$ imputed via Behavioral Risk Factor
  Surveillance System (BRFSS).
- Lacks **immigration status** $\to$ imputed via SIPP using the Hall, Greenman
  & Farkas (2010) and Van Hook et al. (2015) approach.

He verifies the imputed undocumented count matches state-level DHS targets
(2.82 million in CA, 0.27 million in WA in 2012, JHE p. 200).

### 3.2 SIPP imputations (process.SIPP.R, lines 99-627)

Two SIPP-trained logits are applied to ACS:

**(a) Undocumented status** (lines 99-357):

- Sample: SIPP non-citizens (ECITIZNT == 2). ACS non-citizens (CITIZEN == 3).
- Define "unauthorized" in SIPP using Hall et al. (2010): citizens are out;
  permanent residents (TIMSTAT == 1 or EADJUST == 1) are out; recipients of
  federal welfare programs (SSI, food stamps, WIC, Pell, veteran's, AFDC/TANF)
  are out; college students (EENLEVEL 3-8) are out.
- Logit covariates: duration in US, birth place region, English proficiency,
  FPL, employed, age, sex, Hispanic, race, industry (5 cats), married,
  education (3 cats), family size, uninsured, home-owner.
- Predict in ACS, then **deterministic top-N selection**: rank ACS by
  predicted probability, mark the top $N$ as undocumented, where $N$ is
  scaled to match the DHS state-level target (2,820,000 in CA in 2012).
  Implementation uses `sample(rownames(acs), size=N, prob=predictions,
  replace=FALSE)`.
- Same procedure for **permanent residents** in the residual authorized
  non-citizen pool; the rest are temporary residents.

**(b) Affordable employer offer** (lines 396-627, this is the "ESI offer
logit"):

- Sample: SIPP households with no ESI, $\geq 1$ uninsured member, $\geq 1$
  worker (i.e., households for whom the question "do they have an unused ESI
  offer?" is meaningful).
- Define `employer_offer = 1` in SIPP if EHEALPLA == 1 (employer offers
  coverage) AND ENOTPLAN == 3 (worker chose not to enroll). `employer_offer
  = 0` if employer doesn't offer (EHEALPLA == 2) or worker is ineligible
  (ENOTPLAN $\in$ {1, 2, 4}).
- Logit covariates (line 607): FPL, Hispanic, age, sex, race, industry (10
  cats), education, family size, hours worked.
- Predict in ACS workers, then **stochastic Bernoulli imputation** at line
  615:
  ```r
  acs_workers$employer_offer <- as.numeric(offer_predictions - runif(...) > 0)
  ```
  This is the line called out in the user's instruction. Each ACS worker is
  given a *random draw* against their predicted probability. The household's
  `access_to_emp_offer` is then 1 if any member has a predicted offer
  (line 616, 620).

### 3.3 The big filter: `access_to_emp_offer` (prepare.demand.data.R line 494)

After the imputation, Saltzman *drops all ACS households where any member has
either a real ESI offer or an imputed one*:

```r
acs <- acs[acs$access_to_emp_offer == 0,]   # line 494
```

Comments at lines 488-493 acknowledge this is "a fairly strong assumption":
households with an employer offer might still have an unaffordable offer and
remain in the exchange-eligible pool. The opposite extreme (assume all offers
are unaffordable) was flagged for robustness but not implemented in the
released code.

This filter is the dominant determinant of the uninsured-sample size. In the
new repo it is the SIPP-derived "ESI offer" flag in `4_build-acs.R`.

### 3.4 Other ACS filters (prepare.demand.data.R, lines 486-947)

Sequential drops:
- Undocumented immigrants (line 486)
- Households with employer offer (line 494)
- Households with no remaining uninsured member (lines 497-503)
- Medicaid-eligible individuals (line 521; FPL < 138% with non-temporary
  status)
- CHIP-eligible children (lines 530-533; FPL up to 266% statewide, 322% in 4
  high-CHIP counties)
- Income above 400% FPL is *kept* but flagged subsidy-ineligible
- Final restriction to actual uninsured: `acs <- acs[acs$uninsured == 1,]`
  (line 946)

After all filters, the published JHE 2019 sample (Table 2) has the uninsured
constituting 1,407,430 of 2,646,946 observations in CA (53%) and 218,797 of
386,582 in WA (57%). The RAND 2021 update has uninsured constituting
roughly 1/3 of the total (RAND p. 371: "the large share who are uninsured
(roughly one-third of the total population) indicates there is also
underenrollment").

---

## 4. Premium definition in code

The code-level realization of JHE Eq. 3 has two pieces.

### 4.1 Subsidy formula (prepare.demand.data.R, lines 618-733)

Linear interpolation of the contribution cap by FPL bracket:

```r
calculate_contribution <- function(data) {
  contribution <- ((data$perc_UB - data$perc_LB) *
                   (data$FPL - data$fpl_LB)/(data$fpl_UB - data$fpl_LB)
                   + data$perc_LB) *
                  (data$poverty_threshold/12 * data$FPL)
  return(contribution)
}
```

Note that `FPL` here is in *ratio form* (1.5 = 150% FPL), matching Saltzman's
input file `contribution_percentages.csv`. The new repo's
`_helpers.R::aca_contribution()` follows the same convention (see MEMORY.md).

The subsidy is then

```r
households$subsidy <- pmax(0, premiumSLC - premiumSLC_unsubsidized
                              - SLC_contribution)   # line 732
```

with `premiumSLC` the second-lowest-cost silver premium times the household
rating factor, and `premiumSLC_unsubsidized` carved out for mixed-eligibility
households.

### 4.2 Net premium per member (choice_data_function_map.R, line 156-159)

In the choice data construction:

```r
net_premium = case_when(
   plan_name != "Uninsured" ~ final_premium,
   plan_name == "Uninsured" ~ monthly_penalty),
net_premium = net_premium / hh_size
```

So **for inside plans**, the price faced is `(hh_premium - subsidy) /
hh_size` (clamped at zero, monthly). **For the uninsured "plan"**, the price
is `monthly_penalty / hh_size`. This corresponds exactly to JHE Eqs. 1-3:
$\alpha_i p_{ij}$ with $p_{ij}$ the per-member net premium for inside, and
$\alpha_i \rho_i$ with $\rho_i$ the per-member monthly penalty for outside.

In the new repo this is the `"net"` premium type. Saltzman never separately
labels this; it is just "the premium" in the published model.

The `"evan"` premium type in the new repo (penalty subtracted from inside
premiums and zero on the outside option) is **algebraically identical** to
`"net"` in nested logit because the constant subtraction within a household
cancels out of the choice probabilities (see MEMORY.md). Saltzman's own code
implements the `"net"` form directly.

---

## 5. Subsidy treatment

**Formula-based, not observed APTC.** Saltzman computes the subsidy from the
income contribution cap and the second-lowest-cost silver premium for each
household-rating-area-year, ignoring whether the household actually claimed
APTC.

This is a deliberate choice (JHE Section 5): observed APTC is endogenous to
plan choice (waivers, advance vs. final), and contains measurement error.
Formula-based subsidy aligns with the choice-relevant price.

The benchmark (second-lowest-cost silver) is computed in
`prepare.demand.data.R` lines 661-691 via `compute_second_lowest()`, which
sorts available silver premiums in the household's county and rating area,
divides by the age-21 rating factor (1.278) to get the base, and returns the
second element.

Cost-sharing reductions (CSRs) enter through plan AV (0.73, 0.87, 0.94 for
silver-CSR vs. 0.70 base silver) but are not separately modeled as a financial
benefit. AV enters $x_j$ directly. CSRs are *eligibility-restricted* (see
choice_data_function_map.R lines 42-54): households at FPL $\leq$ 250% can
only see the CSR-enhanced silver plans; everyone else sees base Silver.

---

## 6. Penalty / Mandate

The mandate enters in two ways.

### 6.1 Penalty as price on outside option (JHE Eq. 2)

$U_{i0} = \alpha_i \rho_i + \epsilon_{i0}$ where $\rho_i$ is the per-member
monthly penalty. Penalty amount (JHE p. 199, p. 200 footnote):

- 2014: greater of \$95 per adult and 1% of income above filing threshold.
- 2015: greater of \$325 and 2%.
- 2016-2018: greater of \$695 and 2.5%.
- 2019+: $0 (Tax Cuts and Jobs Act, December 2017).

In code (`prepare.demand.data.R` and `choice_data_function_map.R` line 158)
the penalty is converted to monthly: `monthly_penalty = penalty / 12` and
then per-member by dividing by `hh_size`.

### 6.2 Mandate exemptions (prepare.demand.data.R, lines 736-938)

Saltzman exempts:
- American Indians (RACAMIND == 2; line 747)
- Households below the IRS filing threshold (lines 750-832; threshold table
  hardcoded for 2014-2019 and tax unit type).
- Households without an "affordable" exchange offer: cheapest bronze annual
  premium (net of subsidy if subsidy-eligible) exceeds 8.0% (2014), 8.05%
  (2015), 8.13% (2016), 8.16% (2017), 8.05% (2018), 8.30% (2019) of household
  income. Lines 879-919 spell out each year's threshold.

Exempt households face $\rho_i = 0$.

### 6.3 Taste for compliance (JHE Section 4 cont.)

Distinct from the dollar penalty: Saltzman tests for $\varphi_m > 0$ on the
mandate-existence indicator. He finds (Table 7):

- CA: $\varphi_m = 0.479$ (s.e. 0.062), implying $\tau_i \approx \$64$/month.
- WA: $\varphi_m = 0.095$ (s.e. 0.034), implying $\tau_i \approx \$13$/month.

When *both* the intercept and a separate penalty parameter $\alpha'$ are
included (column 3 of Table 7):

- CA: $\varphi_m = 1.218$ (s.e. 0.069), $\alpha'_{penalty} = -0.008$ per \$
  (s.e. 0.001).
- WA: $\varphi_m = 0.754$ (s.e. 0.030), $\alpha'_{penalty} = -0.004$ per \$
  (s.e. 0.000).

The headline conclusion (JHE p. 198): consumers respond strongly to the
*existence* of the mandate but only weakly to its *amount*. $\alpha'$ is
roughly an order of magnitude smaller than $\alpha$ (\$0.008 vs. \$0.429
per \$100 in CA = \$0.00429 per \$1).

---

## 7. Sample composition

### 7.1 JHE 2019 (Table 2)

| | CA exchange | CA uninsured | WA exchange | WA uninsured |
|---|---|---|---|---|
| Average annual N | 1,239,266 | 1,407,430 | 168,785 | 218,797 |

Uninsured share is roughly **53%** in CA, **57%** in WA. Pooled across
2014--2015.

Metal mix among CA exchange enrollees: 64.9% Silver, 24.0% Bronze, 5.5% Gold,
4.8% Platinum, 0.7% Catastrophic. Subsidy take-up: 90.7% premium tax credits,
68.5% cost-sharing reductions.

### 7.2 RAND 2021 (Table 1)

CA only, 2014-2019 panel:

| Year | Market size | Total enrollment | Uninsured share |
|---|---|---|---|
| 2014 | 2,197,669 | 1,362,316 | 38.0% |
| 2015 | 2,420,764 | 1,639,923 | 32.3% |
| 2016 | 2,461,389 | 1,702,160 | 30.8% |
| 2017 | 2,444,815 | 1,697,070 | 30.6% |
| 2018 | 2,429,209 | 1,710,469 | 29.6% |
| 2019 | 2,272,457 | 1,553,374 | 31.6% |
| Overall | 14,226,173 | 9,665,316 | 32.1% |

By 2019, ~30-32% are uninsured. Saltzman's quote (RAND p. 371) "roughly
one-third of the total population" matches this.

### 7.3 Cross-nest moment / $\lambda$ identification

Identification of $\lambda$ requires the inside-vs-outside choice to vary
with shifters that move the inside-nest IV. In Saltzman's data, the inside
nest is large (50-60+ plans per market-year in CA) and the outside option is
substantial (30-57% of HHs). Both inside and outside are well-populated, so
$\lambda$ is well-identified.

The new repo's small-sample failure ($\hat\lambda > 1$ at SAMPLE_FRAC=0.02)
likely reflects insufficient within-cell observations to identify the
cross-nest substitution slope.

---

## 8. Coefficient estimates

### 8.1 JHE 2019 base specification (Table 12, "Base" column)

**California:**

| Parameter | Estimate | s.e. |
|---|---|---|
| Monthly premium ($100/member/month) | $-0.429$ | 0.027 |
| Actuarial value (AV) | 4.125 | 0.240 |
| HMO indicator | $-0.275$ | 0.016 |
| Mandate intercept | 0.479 | 0.062 |
| Nesting parameter $\lambda$ | **0.308** | 0.022 |
| Premium $\times$ Male | $-0.059$ | 0.005 |
| Premium $\times$ age 0-17 | $-0.569$ | 0.027 |
| Premium $\times$ age 18-34 | $-0.616$ | 0.033 |
| Premium $\times$ age 35-54 | $-0.306$ | 0.016 |
| Premium $\times$ FPL 138-250 | $-0.035$ | 0.017 |
| Premium $\times$ FPL 250-400 | $+0.070$ | 0.016 |
| Premium $\times$ FPL 400+ | $+0.126$ | 0.016 |
| Premium $\times$ Family | $-0.015$ | 0.003 |
| Premium $\times$ Year 2015 | $-0.019$ | 0.002 |

**Washington:**

| Parameter | Estimate | s.e. |
|---|---|---|
| Monthly premium ($100) | $-0.827$ | 0.025 |
| AV | 3.591 | 0.159 |
| HMO | $+1.009$ | 0.085 |
| Deductible ratio | $-0.096$ | 0.008 |
| Mandate intercept | 0.095 | 0.034 |
| Nesting parameter $\lambda$ | **0.356** | 0.023 |
| Premium $\times$ Smoker | $-0.409$ | 0.024 |
| Premium $\times$ age 18-34 | $-0.553$ | 0.017 |
| Premium $\times$ FPL 400+ | $+0.589$ | 0.028 |

**Units check.** $\beta_{premium} = -0.429$ utils per \$100 of monthly
per-member premium **in CA**. The raw per-\$1 coefficient is $-0.00429$.
This is the figure used as the reference benchmark in our project (see
MEMORY.md: "Evan's JHE $\beta = -0.429$ per \$100/month ... real gap ~4x,
not 400x").

### 8.2 JHE 2019 mandate experiments (Table 13)

CA mandate regressions show the trade-off between intercept and amount:

| Spec | $\beta_{premium}$ | $\beta_{penalty}$ | $\varphi_m$ |
|---|---|---|---|
| Mandate intercept only | $-0.429$ (0.027) | -- | 0.479 (0.062) |
| Separate penalty only | $-0.467$ (0.031) | $-0.777$ per \$100 (0.061) | -- |
| Both (intercept + penalty) | $-0.403$ (0.026) | $-0.157$ per \$100 (0.055) | 1.218 (0.069) |
| Income interaction | $-0.378$ (0.017) | -- | 1.467 (0.086), Mandate$\times$gt400 = $-2.272$ (0.163) |

Last specification: above-400% FPL households are essentially unaffected by
the mandate ($1.467 - 2.272 \approx -0.8$, basically zero relative to the
intercept), consistent with these households being able to verify their
exemption status easily.

### 8.3 JHE 2019 elasticities (Table 4 / 16)

| Group | CA $\varepsilon$ (base) | CA $\varepsilon$ (CF) | WA $\varepsilon$ (base) | WA $\varepsilon$ (CF) |
|---|---|---|---|---|
| Overall | $-9.1$ | $-10.6$ | $-7.2$ | $-8.1$ |
| Income 0-138% FPL | $-8.8$ | $-10.6$ | $-10.7$ | $-11.7$ |
| Income 400+% FPL | $-7.8$ | $-9.1$ | $-5.3$ | $-6.2$ |
| Age 18-34 | $-13.1$ | $-14.7$ | $-10.0$ | $-11.0$ |
| Age 55+ | $-5.6$ | $-7.2$ | $-4.9$ | $-5.8$ |
| Smoker (WA only) | -- | -- | $-10.3$ | $-11.2$ |

These are *plan-level* own-premium elasticities. Coverage elasticity (Table
5) is much smaller in magnitude: $-1.2$ in CA, $-1.1$ in WA, because most
substitution is *across* plans, not in/out of the exchange. This is the
direct consequence of $\lambda < 1$ and the price-linked subsidy design.

### 8.4 RAND 2021 estimates (Table 3)

RAND 2021 estimates a 2014-2019 CA panel using GMM (jointly estimating
demand, plan risk, and average claims). Demand is now richer (includes
inertia $y_{i(t-1)}$ and per-household demographic intercepts).

**Spec (3) preferred (insurer-market FE, demographic intercepts):**

| Parameter | Estimate | s.e. |
|---|---|---|
| Monthly premium ($100) | $-0.309$ | 0.011 |
| AV | 2.107 | 0.055 |
| Silver | 0.524 | 0.017 |
| HMO | $-0.162$ | 0.014 |
| Previous choice (inertia $\beta^I$) | 1.701 | 0.104 |

Implied elasticities (Table 2, spec 2):

- Overall own-premium elasticity: $-5.3$ (smaller than JHE because of inertia
  controls)
- Coverage elasticity: $-0.6$
- 18-34 own-premium: $-8.0$
- 55+ own-premium: $-3.9$

The big difference from JHE: the **previous choice** parameter (inertia)
absorbs much of what was previously identified through cross-section; net
premium sensitivity falls from $-0.43$ (JHE) to $-0.31$ (RAND, spec 3).

Also estimated jointly (RAND innovation):

- Risk-score parameters $\boldsymbol\gamma$: e.g., AV coefficient on log risk
  score = 2.827 (0.152); share male = $-0.988$, i.e., higher male share
  reduces plan risk score.
- Average claims parameters $\boldsymbol\theta$: log risk score $\to$ log
  claims pass-through is 1.098 (0.030), close to one (additive scaling).

---

## 9. Differences between JHE 2019 and RAND 2021

| Aspect | JHE 2019 | RAND 2021 |
|---|---|---|
| **States** | CA + WA | CA only |
| **Years** | 2014-2015 | 2014-2019 (six-year panel) |
| **Estimator** | MLE (single equation) | GMM (joint demand + risk + claims + supply) |
| **Inertia** | Sensitivity check (Table 12 "Inertia" col) | Built-in: $\beta^I y_{i(t-1)}$ in $V$ |
| **Premium variation source** | Cross-sectional + age curve, FPL, mandate phase-in | Adds 2018 silver-loading shock, 2019 mandate repeal |
| **Mandate test** | Central focus (Sections 7-9) | Used as a counterfactual / model validation |
| **Risk adjustment** | Not modeled | Endogenized: plans differ in $r_j$, RA transfer in firm FOC |
| **Outside option** | Mandate penalty only | Same: penalty in $U_{i0}$ |
| **$\beta_{premium}$ scale** | $-0.429$ per \$100 (CA) | $-0.309$ per \$100 (spec 3) |
| **$\lambda$** | 0.308 (CA), 0.356 (WA) | Not single value; nest structure same |
| **CSR treatment** | AV index only | Same: $x_{ij}$ vector includes plan AV (CSR-enhanced where eligible) |
| **Enrollment in model** | All ACA exchange + ACS uninsured | Same |
| **Counterfactual scope** | Mandate repeal under price-linked vs. voucher subsidies | Risk adjustment + mandate, with endogenous premiums |

The two papers are companion pieces. JHE focuses on *demand* and the role of
the mandate; RAND embeds the same demand model in a fuller supply-side
equilibrium.

The RAND demand model adds **inertia** as a key new feature
($\beta^I y_{i(t-1)}$ where $y_{i(t-1)} = 1$ if the household chose plan $j$
last year). This single addition makes the price coefficient ~30% smaller
because past choices absorb persistent unobserved heterogeneity that
previously loaded onto premium.

---

## 10. Old-code mapping

The full pipeline orchestration is in `_old-repo/data-code/build-data.R`
(62 lines, mostly source() calls). Sequential flow:

| Step | Old script | Function | Maps to paper |
|---|---|---|---|
| 1 | `process.SIPP.R` lines 99-392 | Imputation logit for undocumented status | JHE Section 5: "I use SIPP to impute immigration status" |
| 2 | `process.SIPP.R` lines 396-627 | Imputation logit for ESI offer; **stochastic Bernoulli at line 615** | JHE Section 5: SIPP-derived offer flag |
| 3 | `process.COVCAL.data.R` (2886 lines) | Clean Covered California enrollment file `pra_07192019.csv`; fix metal, county, region, premium, NA values | JHE Section 5 data description: "I obtain data from Covered California" |
| 4 | `prepare.demand.data.R` lines 99-200 | Read ACS, attach immigration & employer-offer flags from SIPP outputs | JHE Section 5 |
| 5 | `prepare.demand.data.R` lines 480-481 | Income OLS (used to impute FPL for above-400% exchange enrollees with missing income) | Implicit in JHE Section 5: "income OLS regression" |
| 6 | `prepare.demand.data.R` lines 486-503 | Drop undocumented; **drop access_to_emp_offer == 1** (line 494); drop households with no uninsured | JHE Section 5: ACS sample restrictions |
| 7 | `prepare.demand.data.R` lines 505-555 | Medicaid eligibility (FPL < 138% non-temp), CHIP eligibility (FPL up to 266%/322% by county) | JHE p. 200 |
| 8 | `prepare.demand.data.R` lines 558-734 | Subsidy formula: linear interpolation of contribution cap, second-lowest-cost silver, HH rating factor | JHE Eq. 3 |
| 9 | `prepare.demand.data.R` lines 736-938 | Mandate exemptions: filing threshold, affordability threshold (cheapest bronze net of subsidy > 8% of income) | JHE p. 199 |
| 10 | `prepare.demand.data.R` lines 1044-end | Merge ACS + Covered California; dynamic_choices matrix | JHE Section 5 |
| 11 | `process.MLR.data.R`, `process.MLR.data.nav.R` | Pull insurer Medical Loss Ratio reports for fixed costs and variable admin costs | RAND Section 4 (data); used in supply-side $V_{ft}$, $FC_{ft}$ |
| 12 | `process.rate.data.R` (2533 lines) | Pull CMS Rate Filing PUF for plan claims, premiums, geographic cost factors | RAND Section 4; also JHE Appendix B for control function instruments |
| 13 | `analysis/decision-support/_ChoiceModel.R` | Loop over (region, year) cells; build choice data; estimate nested logit; bootstrap | JHE Section 6 / RAND Section 5 estimation |
| 14 | `analysis/decision-support/choice_data_function_map.R` | Build choice set per cell; impose CSR eligibility, catastrophic age limit; collapse small insurers; build per-member net premium | JHE Eq. 3; key vocabulary for new repo |
| 15 | `analysis/decision-support/choice_est_function_map.R` | Run `mlogit::mlogit()` with `nests=list(insured=plan_names, uninsured="Uninsured")`, weights from IPW, `un.nest.el=TRUE` | JHE Eq. 6, $\lambda$ free |

### 10.1 Choice data (choice_data_function_map.R)

Key implementation details from `choice.data.fnc2()`:

- Outside option is added explicitly as `plan_name = "Uninsured"` with
  `Issuer_Name = "Outside_Option"` (line 12-15). Same convention as new
  repo's `build_choice_data()`.
- Choice set is restricted by FPL bracket:
  - FPL $\in (200\%, 250\%]$ subsidized $\to$ Silver - Enhanced 73 only
  - FPL $\in (150\%, 200\%]$ subsidized $\to$ Silver - Enhanced 87 only
  - FPL $\leq 150\%$ subsidized $\to$ Silver - Enhanced 94 only
  - Else $\to$ base Silver only
- Catastrophic plans: only available to households whose oldest member is
  $< 30$ AND whose effective premium for the cheapest exchange plan exceeds
  the affordability threshold (lines 64-66).
- Premium computation (lines 76-82):
  ```r
  hh_premium = (Premium / 1.278) * hh_rating       # Premium is age-21 base
                                                   # 1.278 is age-40 factor;
                                                   # need to verify
  final_premium = case_when(
    metal_level != "Minimum Coverage" | is.na(metal) ~ pmax(hh_premium - subsidy, 0),
    metal_level == "Minimum Coverage" ~ hh_premium,             # no subsidy
    Issuer_Name == "Outside_Option" ~ penalty / 12)
  ```
- Small insurers (anything not Anthem, Blue_Shield, Kaiser, Health_Net) are
  collapsed by metal: `Small_P`, `Small_G`, `Small_SIL`, `Small_BR`,
  `Small_CAT`. **Same convention as new repo.**
- Per-member premium: line 159, `net_premium = net_premium / hh_size`.
- AV by metal (lines 87-96): 0.55 catastrophic, 0.60 bronze, 0.70 silver,
  0.73/0.87/0.94 enhanced silver, 0.80 gold, 0.90 platinum, 0 outside.

### 10.2 Estimation (choice_est_function_map.R)

`dchoice.reg()` is the actual estimation call. Notable choices:

- **Variable-selection rules** (lines 13-47): `HMO` and `HSA` are included
  only if more than 10% of choices have them. Insurer dummies are added in
  decreasing order of share until cumulative share exceeds 90%. This is an
  **automatic, data-driven** specification rule, applied per (region, year)
  cell.
- Always-included covariates: `premium`, `silver`, `bronze`, `hh_size_prem`
  (premium $\times$ household size).
- `mlogit()` call (line 56-58):
  ```r
  mlogit(nested.formula, data=nested.data, weights=ipweight,
         nests=list(insured=nest.names, uninsured="Uninsured"),
         un.nest.el=TRUE)
  ```
  - `weights = ipweight`: inverse-propensity weights for the
    decision-support paper's IPW reweighting (NOT in the published model;
    Saltzman doesn't use IPW in JHE 2019 or RAND 2021).
  - `un.nest.el=TRUE`: imposes a single $\lambda$ for the inside nest. The
    outside nest, being a singleton, has $\lambda_0 = 1$ implicitly.
- Initial values: from a binary logit fit with the same RHS, used only as a
  fallback if the default `mlogit` start fails (line 60-62).

Note: the `_old-repo/analysis/decision-support/` folder is the
**decision-support extension**, not the JHE or RAND core estimation. It
adds IPW reweighting and an out-of-sample prediction step for "treated"
(broker/agent/navigator-assisted) households, which is the new paper's
contribution. The Saltzman published model is the un-reweighted core inside
this code. To reproduce JHE/RAND from this code, set `ipweight = 1` and run
on the appropriate sample.

### 10.3 What this repo does *not* contain from Saltzman's published code

The `_old-repo/` does not include:

- The single-equation MLE estimation as published in JHE 2019 (the inherited
  code wraps it in the IPW-reweighted decision-support framework).
- The control function (Petrin-Train) implementation from JHE Appendix B.
- The supply-side first-order conditions and GMM joint estimator from RAND
  2021 (those would be in a separate Julia/Stata code base; only the
  generated outputs `make.julia.learning.R` are referenced).
- The simulation/counterfactual code from JHE Section 9 or RAND Section 6.

Saltzman never publicly released his JHE/RAND estimation code; the inherited
`_old-repo/` is the **co-authored decision-support project's** code, which
uses the Saltzman model as a building block.

---

## 11. Items not addressed in the papers or code

The following items the user might expect to see are explicitly *not*
addressed by Saltzman, based on a thorough read:

- **Broker-density IV.** Saltzman does not use a broker-density instrument.
  His instruments are (a) the geographic exogenous shifters listed in
  Section 6.1 above (FPL cliff, age-21 break, mandate exemptions), and (b)
  for the control function, *geographic cost factors from insurer rate
  filings*. Broker density is a contribution of the new repo / new paper.
- **Decision support / channel effects.** Saltzman's papers do not address
  brokers, navigators, or assisted vs. unassisted enrollment. The
  `decision-support` analysis folder is the new project's contribution
  built on top of his model.
- **Choice frictions / search costs.** Not in Saltzman's framework. Inertia
  in RAND 2021 is the closest analogue.
- **Health-state-dependent demand.** Saltzman explicitly notes (JHE p. 205)
  that he cannot control for health status; this is listed as a caveat.
- **Off-exchange plans.** Saltzman cannot observe off-exchange enrollment
  (RAND p. 369: "I lack demand data on individual plans sold outside the
  ACA exchanges"). He assumes substitution between on- and off-exchange is
  small because off-exchange plans are ineligible for subsidies and are in
  the same risk pool.
- **Provider networks.** Acknowledged limitation (RAND p. 369: "Ignoring
  provider networks and formularies could bias the magnitude of my
  estimates").

---

## Quick reference: numbers to remember

- **JHE 2019 base CA $\beta_{premium} = -0.429$** per \$100 monthly per-member
  premium.
- **JHE 2019 base WA $\beta_{premium} = -0.827$** per \$100 monthly per-member
  premium (WA is more price-sensitive in the JHE estimates).
- **JHE 2019 $\lambda = 0.308$ (CA), 0.356 (WA)**. Always in $(0, 1)$.
- **RAND 2021 spec-3 CA $\beta_{premium} = -0.309$** (smaller because of
  inertia control).
- **CA mean own-premium elasticity: $-9.1$ (base), $-10.6$ (CF).**
- **CA coverage elasticity: $-1.2$.**
- **CA mandate intercept: 0.479; implied taste-for-compliance \$64/month.**
- **WA mandate intercept: 0.095; implied taste-for-compliance \$13/month.**
- **Uninsured share: ~50-57% in JHE pooled CA/WA 2014-2015; ~30-32% in RAND
  CA 2019.**
- **Number of unique CA exchange records: ~2.5 million (2014-2015 JHE);
  ~14.2 million (2014-2019 RAND).**
