# 4_build-cc-uninsured.R
# Build the uninsured pool from CC HHs in years they were not enrolled
# (RAND 2021 Appendix E approach).
#
# Universe: CC HHs that ever enrolled in some year (2014-2019). For each,
# expand to all 6 years; the years they actually enrolled stay in
# enrollment_hh.csv (handled by step 2). The OFF-YEARS are synthesized here:
# demographics filled by HH-mean across observed years, then the SIPP
# transition logit drops HH-years where the HH likely lost market eligibility
# (gained ESI, moved out of CA, turned 65, became Medicaid-eligible).
#
# Inputs:
#   data/output/enrollment_hh.csv     (CC enrolled HHs from step 2)
#   data/output/sipp_logit.rds        (transition logit from step 3)
#   poverty_guidelines_long           (loaded by _data-build.R)
# Output:
#   data/output/cc_uninsured.csv      (CC HH-years with synthesized
#                                      uninsured-choice rows)

set.seed(20260423)

cat("  Loading enrollment HHs and SIPP transition logit...\n")
enroll_hh  <- fread("data/output/enrollment_hh.csv")
sipp_logit <- readRDS("data/output/sipp_logit.rds")

# Complete cases on FPL: drop HHs that never have observed FPL
hh_with_fpl <- enroll_hh[!is.na(FPL) & is.finite(FPL), unique(household_id)]
enroll_hh   <- enroll_hh[household_id %in% hh_with_fpl]
cat(sprintf("  HHs with observed FPL in at least one year: %d\n",
            length(hh_with_fpl)))

# HH-level means across observed years (used to fill off-year synthetics).
demo_cols <- c("FPL", "household_size", "oldest_member", "rating_factor",
               "perc_0to17", "perc_18to25", "perc_26to34", "perc_35to44",
               "perc_45to54", "perc_55to64", "perc_65plus", "perc_male",
               "perc_white", "perc_black", "perc_hispanic", "perc_asian",
               "perc_other", "english", "spanish", "other_language")
hh_means <- enroll_hh[, lapply(.SD, mean, na.rm = TRUE),
                       by = household_id, .SDcols = demo_cols]
hh_geo   <- enroll_hh[, .(zip3 = first(zip3), region = first(region)),
                       by = household_id]

# Build off-year panel: (HH × all years) minus actually-enrolled (HH, year).
all_years     <- 2014:2019
panel         <- CJ(household_id = hh_with_fpl, year = all_years)
enrolled_keys <- unique(enroll_hh[, .(household_id, year)])
panel         <- panel[!enrolled_keys, on = c("household_id", "year")]

# Attach HH-level demographics + geography
panel <- merge(panel, hh_means, by = "household_id")
panel <- merge(panel, hh_geo,   by = "household_id")

# Round household_size to integer (mean across years can give fractions)
panel[, household_size := pmax(1L, as.integer(round(household_size)))]

# Variables needed for sipp_logit prediction
panel[, FPL_bracket := assign_bracket(FPL)]
panel[, perc_18to34 := perc_18to25 + perc_26to34]
panel[, perc_35to54 := perc_35to44 + perc_45to54]

# Predict P(transitioned) and stochastic Bernoulli draw to drop HH-years
# where the HH was likely not market-eligible.
panel[, p_transitioned := predict(sipp_logit, newdata = panel, type = "response")]
panel[, transitioned   := as.integer(p_transitioned > runif(.N))]
cat(sprintf("  Off-year HH-years before SIPP filter: %d\n", nrow(panel)))
cat(sprintf("  After SIPP filter (transitioned == 0): %d (%.1f%% retained)\n",
            sum(panel$transitioned == 0L),
            100 * mean(panel$transitioned == 0L)))
panel <- panel[transitioned == 0L]
panel[, c("p_transitioned", "transitioned") := NULL]

# Year × hh_size poverty threshold (cap year at 2019 for the CMS table)
pov_dt <- as.data.table(poverty_guidelines_long)
panel[, year_cap := pmin(year, 2019L)]
panel[pov_dt, on = c("year_cap" = "year", "household_size" = "Family_Size"),
      poverty_threshold := i.poverty_threshold]
panel[, year_cap := NULL]

# Composite household_year to mirror step 2 (split = 0 marks off-year synthesis)
panel[, household_year := paste(household_id, year, "0", sep = "_")]

fwrite(panel, "data/output/cc_uninsured.csv")
cat(sprintf("Step 4 complete: %d uninsured CC HH-years -> data/output/cc_uninsured.csv\n",
            nrow(panel)))

rm(enroll_hh, sipp_logit, hh_means, hh_geo, panel, hh_with_fpl, enrolled_keys, pov_dt)
gc(verbose = FALSE)
