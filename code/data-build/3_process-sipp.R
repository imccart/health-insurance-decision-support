# 3_process-sipp.R
# Fit the SIPP-based logit used downstream:
#   nongroup_transition_logit - P(household leaves the individual market | FPL
#   bracket, size, age, sex and race shares), fit on the SIPP 2014 panel waves
#   1-3. Step 4 applies it to the off-year rows of enrolled households.
#
# Output:
#   data/output/sipp_logit.rds

set.seed(5)

sipp_dir <- "data/input/SIPP Data"


# =============================================================================
# NONGROUP-TRANSITION LOGIT — from SIPP 2014 panel waves 1-3
# =============================================================================
# Predicts whether a household transitioned into or out of the individual
# market in a given year (RAND 2021 Appendix E). Used in step 4 to filter
# CC HH-years where the HH was likely not market-eligible. Uses SIPP 2014
# panel (calendar 2013-2015) since it tracks plan transitions across years.
# Plan codes (status20YY): 1=nongroup, 2=military, 3=Medicare, 4=ESI,
# 5=Medicaid/CHIP, 6=other, 7=uninsured.
cat("  Fitting nongroup-transition logit on SIPP 2014 panel...\n")

read_wave <- function(pu_path, status_path, yr) {
  pu <- fread(pu_path)
  if ("RMOVER" %in% names(pu) && !"TMOVER" %in% names(pu)) {
    setnames(pu, "RMOVER", "TMOVER")
  }
  # Coverage status is monthly. ng_first / ng_last are the first and last month
  # of the person's non-group spell (code 1); jan and dec are the codes in those
  # months, which carry what the household came from and went to.
  mcols <- paste0(c("jan", "feb", "mar", "apr", "may", "jun",
                    "jul", "aug", "sep", "oct", "nov", "dec"), yr)
  st <- fread(status_path)[, c("SSUID", "PNUM", "SWAVE", "TAGE", mcols), with = FALSE]
  setnames(st, mcols, paste0("m", 1:12))
  ng <- as.matrix(st[, paste0("m", 1:12), with = FALSE]) == 1L
  ng[is.na(ng)] <- FALSE
  any_ng <- rowSums(ng) > 0
  st[, `:=`(
    ng_first = fifelse(any_ng, max.col(ng, ties.method = "first"), NA_integer_),
    ng_last  = fifelse(any_ng, 13L - max.col(ng[, 12:1, drop = FALSE], ties.method = "first"),
                       NA_integer_),
    jan = m1, dec = m12
  )]
  st <- st[, .(SSUID, PNUM, SWAVE, TAGE, ng_first, ng_last, jan, dec)]
  pu <- merge(pu, st, by = c("SSUID", "PNUM", "SWAVE", "TAGE"), all.x = TRUE)
  pu[, year := yr]
  pu[, .(SSUID, PNUM, MONTHCODE, year, TAGE, ESEX, ERACE, EORIGIN,
         THTOTINC, RHPOV, TMOVER, jan, dec, ng_first, ng_last,
         EYNOESI_COV, EYNOESI_EXP, EYNOESI_HTH, EYNOESI_ELS, EYNOESI_UNH)]
}

sipp_pm <- rbindlist(list(
  read_wave(file.path(sipp_dir, "pu2014w1.csv"),
            file.path(sipp_dir, "status2013.csv"), 2013),
  read_wave(file.path(sipp_dir, "pu2014w2.csv"),
            file.path(sipp_dir, "status2014.csv"), 2014),
  read_wave(file.path(sipp_dir, "pu2014w3.csv"),
            file.path(sipp_dir, "status2015.csv"), 2015)
))

# Person-month indicators (age uses & not |)
sipp_pm[, `:=`(
  on_nongroup = as.integer(!is.na(ng_first)),
  young       = as.integer(TAGE <= 17),
  middle      = as.integer(TAGE >= 18 & TAGE <= 34),
  old         = as.integer(TAGE >= 35 & TAGE <= 54),
  male        = as.integer(ESEX == 1),
  asian       = as.integer(ERACE == 3),
  black       = as.integer(ERACE == 2),
  hispanic    = as.integer(EORIGIN == 1),
  other       = as.integer(ERACE == 4 & EORIGIN == 2),
  emp_offer_decline = as.integer(
    EYNOESI_COV == 1 | EYNOESI_EXP == 1 | EYNOESI_HTH == 1 |
    EYNOESI_ELS == 1 | EYNOESI_UNH == 1)
)]

# Household-head plan in January (lowest PNUM in HH = SIPP reference person)
hh_head_jan <- sipp_pm[MONTHCODE == 1,
                        .(jan_plan_head = jan[which.min(PNUM)]),
                        by = .(SSUID, year)]

# Aggregate to HH-year
sipp_hh <- sipp_pm[, .(
  household_size = .N / 12,
  perc_0to17     = mean(young),
  perc_18to34    = mean(middle),
  perc_35to54    = mean(old),
  perc_male      = mean(male),
  perc_asian     = mean(asian),
  perc_black     = mean(black),
  perc_hispanic  = mean(hispanic),
  perc_other     = mean(other),
  FPL            = pmax(0, mean(THTOTINC) / mean(RHPOV)),
  TMOVER         = max(TMOVER, na.rm = TRUE),
  employer_offer = as.integer(any(emp_offer_decline == 1, na.rm = TRUE)),
  ever_nongroup  = as.integer(any(on_nongroup == 1)),
  start_nongroup = if (any(on_nongroup == 1)) min(ng_first, na.rm = TRUE) else NA_integer_,
  end_nongroup   = if (any(on_nongroup == 1)) max(ng_last, na.rm = TRUE) else NA_integer_
), by = .(SSUID, year)]
sipp_hh[is.infinite(TMOVER), TMOVER := NA_real_]
sipp_hh <- merge(sipp_hh, hh_head_jan, by = c("SSUID", "year"), all.x = TRUE)

# Keep only HHs in all 3 waves AND ever on nongroup somewhere in the panel
hh_in_all  <- sipp_hh[, .N, by = SSUID][N == 3, SSUID]
hh_ever_ng <- unique(sipp_hh[ever_nongroup == 1, SSUID])
sipp_hh <- sipp_hh[SSUID %in% intersect(hh_in_all, hh_ever_ng)]
setorder(sipp_hh, SSUID, year)

# Adjacent-year plan codes via shift; fallback to "still nongroup" (code 1)
sipp_hh[, `:=`(
  prev_ng  = shift(ever_nongroup,  1L, type = "lag"),
  next_ng  = shift(ever_nongroup,  1L, type = "lead"),
  prev_jan = shift(jan_plan_head,  1L, type = "lag"),
  next_jan = shift(jan_plan_head,  1L, type = "lead")
), by = SSUID]
sipp_hh[, entered  := as.integer(ever_nongroup == 1 & (is.na(prev_ng) | prev_ng == 0))]
sipp_hh[, exited   := as.integer(ever_nongroup == 1 &
                                 ((is.na(next_ng) | next_ng == 0) | end_nongroup < 12))]
sipp_hh[, old_plan := fifelse(entered == 1 & !is.na(prev_jan), prev_jan, 1)]
sipp_hh[, new_plan := fifelse(exited  == 1 & !is.na(next_jan), next_jan, 1)]

# entered_market: from old_plan; exited_market: from new_plan.
# 0 if from/to uninsured (7), 1 if from/to ESI/Medicaid (4,5), NA otherwise.
# Bumped to 1 if employer offer declined or HH moved (TMOVER >= 4).
sipp_hh[, entered_market := NA_integer_]
sipp_hh[old_plan == 7,            entered_market := 0L]
sipp_hh[old_plan %in% c(4, 5),    entered_market := 1L]
sipp_hh[!is.na(entered_market) & entered_market == 0L &
        (employer_offer == 1 | (!is.na(TMOVER) & TMOVER >= 4)),
        entered_market := 1L]

sipp_hh[, exited_market := NA_integer_]
sipp_hh[new_plan == 7,            exited_market := 0L]
sipp_hh[new_plan %in% c(4, 5),    exited_market := 1L]
sipp_hh[!is.na(exited_market) & exited_market == 0L &
        (employer_offer == 1 | (!is.na(TMOVER) & TMOVER >= 4)),
        exited_market := 1L]

sipp_hh[, transitioned := fcase(
  entered_market == 1L | exited_market == 1L, 1L,
  entered_market == 0L & is.na(exited_market), 0L,
  is.na(entered_market) & exited_market == 0L, 0L,
  entered_market == 0L & exited_market == 0L, 0L,
  default = NA_integer_
)]
sipp_hh[, FPL_bracket := assign_bracket(FPL)]

sipp_logit <- glm(
  transitioned ~ FPL_bracket + household_size +
    perc_0to17 + perc_18to34 + perc_35to54 +
    perc_male + perc_asian + perc_black + perc_hispanic + perc_other,
  data = sipp_hh[!is.na(transitioned)],
  family = binomial
)
saveRDS(sipp_logit, "data/output/sipp_logit.rds")
rm(sipp_pm, sipp_hh, hh_head_jan); gc(verbose = FALSE)
cat("  done. n =", length(sipp_logit$y),
    "; mean(transitioned) =", round(mean(sipp_logit$y), 3), "\n")


cat("Step 3 complete: nongroup-transition logit fit.\n")
rm(sipp_logit); gc(verbose = FALSE)
