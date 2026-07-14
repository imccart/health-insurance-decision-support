# welfare_objective.R — Objective (money-metric) normative utility V^N.
#
# A second normative benchmark to set alongside the navigator-based V^N. Values
# each plan in dollars as premium plus the certainty-equivalent burden of
# out-of-pocket (OOP) spending, with OOP built from the plan's ACTUAL cost-sharing
# (deductible -> coinsurance -> MOOP) rather than approximated by (1 - AV). This is
# feasible here because Covered California uses standardized benefit designs, so
# cost-sharing is fixed by metal/CSR tier (data/input/ca_standard_cost_sharing.csv).
#
# NOTHING below is filled with invented numbers. The CA cost-sharing values must be
# entered from Covered California's published Patient-Centered Benefit Designs, and
# the two calibration scalars must be set from the literature. The functions error
# loudly on NA so nothing can silently run on placeholders.

# --- Calibration scalars (literature values; confirm before final results) ----
# Coefficient of variation of annual individual health spending. Right-skewed;
# from AHRQ MEPS 2022 concentration tabulation (Stat. Brief #560), CV >= 2.5 as a
# strict lower bound, plausibly 3-4 with within-group dispersion. Using the lower
# bound; sensitivity to 3-4 worth reporting. Gives the lognormal spending
# distribution its spread, which is what makes risk aversion bite.
SPENDING_CV <- 2.5
# CARA absolute risk aversion, PER DOLLAR. Handel (2013, AER) mean ~2.3e-4/$ is
# the common ESI/marketplace benchmark; defensible range ~1e-4 to 1e-3. Units:
# objective_vN works in raw annual dollars (premium, OOP, MOOP all in $), so this
# per-dollar value is used directly — no /100 rescaling (unlike the demand alpha,
# which is per-$100).
RHO_RISK_AVERSION <- 2.3e-4

# --- CA standardized cost-sharing lookup -------------------------------------
# Returns a data.frame keyed by (year, metal, hsa) with av + deductible,
# coinsurance, moop. Join to cell_data on (year, metal, hsa).
load_ca_cost_sharing <- function(path = "data/input/ca_standard_cost_sharing.csv") {
  cs <- read.csv(path, stringsAsFactors = FALSE)
  if (anyNA(cs$deductible) || anyNA(cs$coinsurance) || anyNA(cs$moop))
    warning("ca_standard_cost_sharing.csv has unfilled cost-sharing cells — ",
            "fill them from Covered California's standard benefit designs.")
  cs
}

# --- OOP schedule and moments ------------------------------------------------
# Standard schedule: pay 100% up to the deductible, then coinsurance, capped at
# MOOP (and never more than total spending).
oop_schedule <- function(s, deductible, coinsurance, moop) {
  paid <- pmin(s, deductible) + coinsurance * pmax(s - deductible, 0)
  pmin(paid, moop)
}

# E[OOP] and Var[OOP] over a lognormal annual-spending distribution with mean
# `espend` and coefficient of variation `cv`. Deterministic quadrature (no RNG)
# so it is exactly reproducible.
oop_moments <- function(espend, cv, deductible, coinsurance, moop, n_grid = 400L) {
  if (anyNA(c(cv, deductible, coinsurance, moop)))
    stop("oop_moments: NA input — fill the CA cost-sharing table and set SPENDING_CV.")
  if (espend <= 0) return(c(mean = 0, var = 0))
  s2    <- log(1 + cv^2)
  sdlog <- sqrt(s2)
  mlog  <- log(espend) - s2 / 2
  p     <- (seq_len(n_grid) - 0.5) / n_grid          # midpoint quantiles
  s     <- qlnorm(p, meanlog = mlog, sdlog = sdlog)
  oop   <- oop_schedule(s, deductible, coinsurance, moop)
  m     <- mean(oop)
  c(mean = m, var = mean((oop - m)^2))
}

# --- Objective normative value of a plan (annual dollars) ---------------------
# premium_annual must be the annual premium the household pays (net premium x 12).
# Returns V^N in dollars; higher is better (lower total cost of coverage). This is
# the negative of premium + the mean-variance certainty equivalent of OOP.
objective_vN <- function(premium_annual, espend, cv, rho, deductible, coinsurance, moop) {
  m <- oop_moments(espend, cv, deductible, coinsurance, moop)
  unname(-(premium_annual + m["mean"] + (rho / 2) * m["var"]))
}

# --- Validation helper -------------------------------------------------------
# Implied actuarial value = 1 - E[OOP]/E[spend]. Should track the statutory `av`
# column; a check on the table entries and on SPENDING_CV before using results.
implied_av <- function(espend, cv, deductible, coinsurance, moop) {
  m <- oop_moments(espend, cv, deductible, coinsurance, moop)
  unname(1 - m["mean"] / espend)
}

# --- Age/income spending schedule (optional) ---------------------------------
# Replaces the flat MEAN_SPENDING with household-specific expected annual individual
# spending drawn from an EXTERNAL schedule (e.g. MEPS by age x income). This is a
# MEASURED input, not a calibration -- it swaps a guessed constant for numbers a
# large survey pins down, and it introduces no moral hazard because spending depends
# on the household's characteristics, never on the plan.
#
# load_spending_schedule returns NULL if the file is missing or any mean_spend cell
# is unfilled, so the caller falls back to the flat scalar and the pipeline still
# runs. Fill data/input/meps_spending_by_demographics.csv from MEPS to turn it on;
# nothing here is populated with invented numbers.
load_spending_schedule <- function(path = "data/input/meps_spending_by_demographics.csv") {
  if (!file.exists(path)) return(NULL)
  s <- read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("age_group", "income", "mean_spend") %in% names(s))) return(NULL)
  if (anyNA(s$mean_spend)) {
    warning("meps_spending_by_demographics.csv has unfilled mean_spend -- ",
            "using flat MEAN_SPENDING until it is filled from MEPS.")
    return(NULL)
  }
  s
}

# Per-ROW expected annual individual spending from the household's age mix and income
# bracket. Household spending follows the person (age composition + income), not the
# plan, so it is constant across a household's plan rows and carries no moral hazard.
# schedule = NULL -> flat `default` (reproduces the pre-schedule behavior exactly).
household_spending <- function(cell_data, schedule = NULL, default = MEAN_SPENDING) {
  n <- nrow(cell_data)
  if (is.null(schedule)) return(rep(default, n))
  d   <- as.data.table(cell_data)
  p0  <- if ("perc_0to17"  %in% names(d)) d$perc_0to17  else rep(0, n)
  p18 <- if ("perc_18to34" %in% names(d)) d$perc_18to34 else rep(0, n)
  p35 <- if ("perc_35to54" %in% names(d)) d$perc_35to54 else rep(0, n)
  p55 <- pmax(0, 1 - p0 - p18 - p35)
  inc <- fifelse(("FPL_400plus"  %in% names(d)) & d$FPL_400plus  == 1, "400plus",
          fifelse(("FPL_250to400" %in% names(d)) & d$FPL_250to400 == 1, "250to400", "lt250"))
  look <- function(age, income) {
    m <- schedule$mean_spend[match(paste(age, income), paste(schedule$age_group, schedule$income))]
    fifelse(is.na(m), default, m)
  }
  p0 * look("0to17", inc) + p18 * look("18to34", inc) +
    p35 * look("35to54", inc) + p55 * look("55plus", inc)
}
