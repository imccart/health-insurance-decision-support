# 4_build-cc-uninsured.R
# Build the uninsured pool from CC HHs in years they were not enrolled
# (RAND 2021 Appendix E approach).
#
# Universe: CC HHs that ever enrolled in some year (2014-2019). For each,
# expand to all 6 years; the years they actually enrolled stay in
# enrollment_hh.csv (handled by step 2). The OFF-YEARS are synthesized here:
# each off-year copies the household's nearest observed year (previous year
# preferred over next at equal distance), ages every member by the year gap,
# recomputes the age shares and the year-specific rating factor from the aged
# members, and drops households with any member aged 65+ (Medicare, not an
# individual-market choice). Income (FPL), race, gender, and language carry
# from the reference year. The SIPP transition logit then drops HH-years where
# the HH likely lost market eligibility (gained ESI, moved out of CA, became
# Medicaid-eligible).
#
# Inputs:
#   data/output/enrollment_hh.csv          (CC enrolled HHs from step 2)
#   data/output/enrollment_individual.csv  (member ages by household_year, step 2)
#   data/output/sipp_logit.rds             (transition logit from step 3)
#   age_rating_factors, poverty_guidelines_long  (loaded by _data-build.R)
# Output:
#   data/output/cc_uninsured.csv      (CC HH-years with synthesized
#                                      uninsured-choice rows)

set.seed(20260423)

enroll_hh  <- fread("data/output/enrollment_hh.csv")
sipp_logit <- readRDS("data/output/sipp_logit.rds")

# Complete cases on FPL: drop HHs that never have observed FPL
hh_with_fpl <- enroll_hh[!is.na(FPL) & is.finite(FPL), unique(household_id)]
enroll_hh   <- enroll_hh[household_id %in% hh_with_fpl]
cat(sprintf("  HHs with observed FPL in at least one year: %d\n",
            length(hh_with_fpl)))

# Build off-year panel: (HH × all years) minus actually-enrolled (HH, year).
all_years     <- 2014:2019
panel         <- CJ(household_id = hh_with_fpl, year = all_years)
enrolled_keys <- unique(enroll_hh[, .(household_id, year)])
panel         <- panel[!enrolled_keys, on = c("household_id", "year")]

# Reference year: the household's nearest observed year, previous year
# preferred over next at equal distance (-1, +1, -2, +2, ...).
cand <- merge(panel, unique(enroll_hh[, .(household_id, obs_year = year)]),
              by = "household_id", allow.cartesian = TRUE)
cand[, `:=`(dist = abs(year - obs_year), prev = as.integer(obs_year < year))]
setorder(cand, household_id, year, dist, -prev)
panel <- cand[, .(obs_year = obs_year[1]), by = .(household_id, year)]
rm(cand)

# Copy the reference-year household row: income, geography, and the
# demographics not recomputed from member ages below.
ref_cols <- c("household_year", "FPL", "household_size", "perc_male",
              "perc_white", "perc_black", "perc_hispanic", "perc_asian",
              "perc_other", "english", "spanish", "other_language",
              "zip3", "region")
panel <- merge(panel,
               enroll_hh[, c("household_id", "year", ref_cols), with = FALSE],
               by.x = c("household_id", "obs_year"),
               by.y = c("household_id", "year"))
setnames(panel, "household_year", "ref_household_year")

# Age members from the reference year by the year gap ----------------------
cat("  Aging members from the reference year...\n")
ind <- fread("data/output/enrollment_individual.csv",
             select = c("household_year", "age"))
mem <- merge(panel[, .(household_id, year, ref_household_year,
                       gap = year - obs_year)],
             ind, by.x = "ref_household_year", by.y = "household_year",
             allow.cartesian = TRUE)
mem[, age := age + gap]
rm(ind)

# Drop off-year HHs with any member aged 65+ (Medicare, not a market choice)
over65 <- unique(mem[age >= 65, .(household_id, year)])
panel  <- panel[!over65, on = c("household_id", "year")]
mem    <- mem[!over65, on = c("household_id", "year")]
cat(sprintf("  Dropped %d off-year HH-years with a member aged 65+\n",
            nrow(over65)))
rm(over65)

# Members aged below zero (reference year after the off-year, i.e. not yet
# born) leave multi-member HHs; a lone member is floored at age 0.
mem[, n_mem := .N, by = .(household_id, year)]
mem <- mem[!(age < 0 & n_mem > 1)]
mem[, age := pmax(0L, age)]

# Recompute age shares, oldest member, and the year-specific rating factor
# (2018+ separate curve, ages capped at 64) from the aged members.
mem[, rating_factor_i := fifelse(
  year >= 2018,
  age_rating_factors$Rating_Factor2018[match(pmin(64L, age), age_rating_factors$Age)],
  age_rating_factors$Rating_Factor[match(pmin(64L, age), age_rating_factors$Age)]
)]
agg <- mem[, .(
  oldest_member = max(age),
  rating_factor = sum(rating_factor_i, na.rm = TRUE),
  perc_0to17    = mean(age <= 17),
  perc_18to25   = mean(age >= 18 & age <= 25),
  perc_26to34   = mean(age >= 26 & age <= 34),
  perc_35to44   = mean(age >= 35 & age <= 44),
  perc_45to54   = mean(age >= 45 & age <= 54),
  perc_55to64   = mean(age >= 55 & age <= 64),
  perc_65plus   = mean(age >= 65)
), by = .(household_id, year)]
panel <- merge(panel, agg, by = c("household_id", "year"))
panel[, c("obs_year", "ref_household_year") := NULL]
rm(mem, agg)

# Variables needed for sipp_logit prediction
panel[, FPL_bracket := assign_bracket(FPL)]
panel[, perc_18to34 := perc_18to25 + perc_26to34]
panel[, perc_35to54 := perc_35to44 + perc_45to54]

# Predict P(transitioned) and stochastic Bernoulli draw. KEEP all rows with a
# market_eligible flag (1 = still in individual market, 0 = transitioned out).
# Deleting transitioned rows here would break the lag-based new_enrollee
# derivation in 1_decision-analysis.R, which needs every off-year visible to
# detect gaps. Downstream code filters by market_eligible AFTER computing
# new_enrollee.
panel[, p_transitioned  := predict(sipp_logit, newdata = panel, type = "response")]
panel[, market_eligible := as.integer(p_transitioned <= runif(.N))]
cat(sprintf("  Off-year HH-years: %d (%.1f%% market-eligible after SIPP draw)\n",
            nrow(panel),
            100 * mean(panel$market_eligible == 1L)))
panel[, p_transitioned := NULL]

# Year × hh_size poverty threshold (cap year at 2019 for the CMS table)
pov_dt <- as.data.table(poverty_guidelines_long)
panel[, year_cap := pmin(year, 2019L)]
panel[pov_dt, on = c("year_cap" = "year", "household_size" = "Family_Size"),
      poverty_threshold := i.poverty_threshold]
panel[, year_cap := NULL]

# Composite household_year to mirror step 2 (split = 0 marks off-year synthesis)
panel[, household_year := paste(household_id, year, "0", sep = "_")]

fwrite(panel, "data/output/cc_uninsured.csv")

rm(enroll_hh, sipp_logit, panel, hh_with_fpl, enrolled_keys, pov_dt)
gc(verbose = FALSE)
