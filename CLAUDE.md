# Health Insurance Decision Support

## Project Overview

Academic research on decision assistance and insurer steering in the ACA health insurance marketplace (Covered California). Two main parts:

1. **Demand-side (reduced form):** Design-based analysis showing decision support tools affect plan choice. Includes dominated choice analysis and nested logit discrete choice estimation.
2. **Supply-side (structural):** Structural model of insurer premium setting and commission setting, building on demand estimates. The commission-setting model is the core contribution.

Rebuild of an earlier project (original repo: https://github.com/imccart/aca-decision-support). The old repo is poorly organized and undocumented. This rebuild starts fresh with a clean, reproducible pipeline.

## Research Questions

- How do decision support tools (navigators, agents/brokers) affect health insurance plan choice?
- Do insurers steer consumers toward higher-margin plans via broker commissions?
- What is the welfare effect of commission-based steering?

## Data Sources

- **Enrollments:** FOIA from Covered California (pra_07192019.csv)
- **Plans:** Covered California website, manually appended (plan_data.csv + product_definitions.csv)
- **Choices:** 2017-2019 downloaded, 2014-2016 FOIA, manually appended (zip3_choices.csv)
- **Age rating:** CCIIO age rating curves from CMS (age_rating_factors.csv)
- **Poverty thresholds:** poverty_guidelines.csv
- **ACA contribution percentages:** contribution_percentages.csv
- **California rating areas:** rating_areas.csv
- **ACS and SIPP:** Multiple files for outside option prediction
- **PUMAs:** Census Bureau (PUMAs.csv)
- **SAHIE:** Census Bureau (sahie_2014.csv)
- **County files:** counties_2014 through counties_2019
- **Rate filings:** CMS website, merged into 2014-2020.RData
- **MLR:** CMS website, separate folders by year (2014-2018)
- **RA/RI:** CMS PDFs, manually extracted to ra_reins.csv

## Identification Strategy

Decision support effect identified by variation in how consumers access the marketplace (direct vs. navigator-assisted vs. agent/broker-assisted). Dominated choice analysis provides reduced-form evidence. Nested logit discrete choice model for structural demand estimation.

Supply side: structural model of insurer premium and commission setting, using demand estimates as inputs.

## Code Pipeline

### Data Build (code/data-build/)
Scripts numbered in execution order. Each script sources `code/0-setup.R` for packages.

### Analysis (code/analysis/)
Scripts numbered in execution order. Demand-side analysis first, then supply-side structural model.

## Reference Materials

- `_old-repo/` — cloned old GitHub repo (gitignored)
- `_local-files/` — old local project files (gitignored)
- `_emails/` — exported project emails as .msg and .txt (gitignored)

These are for reference only during the rebuild and are not part of the clean project.

## Open Questions

See `docs/open-questions.md` once the archaeology phase is complete.
