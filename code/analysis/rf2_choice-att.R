# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-24
## Description:   MNL plan-choice ATT among enrollees. Estimate on insured +
##                unassisted via mlogit; OOS-predict for insured + assisted.
##                ATT = observed - predicted plan-attribute means by cell.
##                No outside good, no nest — the estimand is conditional on
##                enrollment, so synthetic off-year (uninsured) rows are
##                dropped from estimation.

# Reduced-form specification (this MNL is the only RF step that uses it) ---
REDUCED_FORM_SPEC <- c(
  "premium",
  "silver", "bronze", "hmo", "hsa",
  "Anthem", "Blue_Shield", "Kaiser", "Health_Net",
  "hh_size_prem", "perc_0to17_prem", "perc_18to34_prem", "perc_35to54_prem",
  "perc_male_prem", "perc_black_prem", "perc_hispanic_prem", "perc_asian_prem", "perc_other_prem",
  "FPL_250to400_prem", "FPL_400plus_prem"
)
REDUCED_FORM_CF   <- c("cf_anthem", "cf_blue_shield", "cf_kaiser", "cf_health_net",
                       "cf_silver", "cf_bronze")
REDUCED_FORM_FULL <- c(REDUCED_FORM_SPEC, REDUCED_FORM_CF)
write_demand_spec(REDUCED_FORM_FULL, character(0),
                  file.path(TEMP_DIR, "demand_spec_reduced.csv"))

CELL_DIR <- file.path(TEMP_DIR, "choice_cells")

plan_choice <- fread(file.path(TEMP_DIR, "plan_choice.csv")) %>% as_tibble()
hh_all <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split <- split(hh_all, by = c("region", "year"))
cells <- unique(hh_all[, .(region, year)])[order(region, year)]
rm(hh_all); gc(verbose = FALSE)

if (!dir.exists(CELL_DIR)) dir.create(CELL_DIR, recursive = TRUE)

# Phase 1: build cell data ------------------------------------------------

cat("Phase 1: Building cell data (", nrow(cells), "cells)...\n")

set.seed(MASTER_SEED)
cell_seeds <- sample.int(1e7, nrow(cells))

if (dir.exists(CELL_DIR)) unlink(CELL_DIR, recursive = TRUE)
dir.create(CELL_DIR, recursive = TRUE)

n_built <- 0L
n_skip  <- 0L

for (i in seq_len(nrow(cells))) {
  r <- cells$region[i]
  y <- cells$year[i]

  out_file <- file.path(CELL_DIR, paste0("cell_", r, "_", y, "_data.csv"))

  set.seed(cell_seeds[i])
  hhs <- hh_split[[paste0(r, ".", y)]]
  if (is.null(hhs) || nrow(hhs) == 0) { n_skip <- n_skip + 1L; next }
  hhs <- as.data.frame(hhs)

  plans <- plan_choice %>% filter(region == r, year == y)
  if (nrow(plans) == 0) { n_skip <- n_skip + 1L; next }

  cd <- build_rf(plans, hhs, SAMPLE_FRAC, spec = REDUCED_FORM_FULL,
                 premium_type = "net")
  rm(hhs, plans)

  if (!is.null(cd)) {
    cd$region <- r
    cd$year <- y
    fwrite(cd, out_file)
    n_built <- n_built + 1L
  } else {
    n_skip <- n_skip + 1L
  }
  rm(cd)

  if (i %% 20 == 0) {
    gc(verbose = FALSE)
    cat("  Cell", i, "of", nrow(cells), "\n")
  }
}

rm(plan_choice, hh_split)
gc(verbose = FALSE)
cat("  Built:", n_built, "  Skipped:", n_skip, "\n")

# Phase 2: pool insured + unassisted, fit MNL ------------------------------

cat("\nPhase 2: Pooling cells and fitting MNL...\n")

needed <- unique(c("household_number", "plan_id", "choice", "hh_weight", "ipweight",
                   "assisted", "new_enrollee", "region", "year",
                   REDUCED_FORM_FULL))

read_cell <- function(path) {
  header <- names(fread(path, nrows = 0L))
  fread(path, select = intersect(needed, header))
}

pool_list <- vector("list", nrow(cells))
for (i in seq_len(nrow(cells))) {
  csv_path <- file.path(CELL_DIR, paste0("cell_", cells$region[i], "_",
                                         cells$year[i], "_data.csv"))
  if (!file.exists(csv_path)) next
  pool_list[[i]] <- read_cell(csv_path)
}
pooled <- rbindlist(pool_list, fill = TRUE)
rm(pool_list); gc(verbose = FALSE)

# Drop uninsured rows (no outside good in this MNL) and make a unique chid
pooled <- pooled[plan_id != "Uninsured"]
pooled[, chid := paste(region, year, household_number, sep = "_")]

# Estimation sample: unassisted only, restricted to chids with a chosen row
unassist <- pooled[assisted == 0L]
keep_chids <- unassist[choice == 1L, unique(chid)]
unassist <- unassist[chid %in% keep_chids]
cat(sprintf("  Estimation sample: %d rows, %d HH-years\n",
            nrow(unassist), length(keep_chids)))

# Baseline = observational, NO control function. The cf_ terms must not sit in
# the estimation: including them distorts every other coefficient through
# partialling and drives the assisted counterfactual to ~33% silver (a spurious
# +36pp ATT). Dropped here; the selection-corrected version is a separate model.
fmla <- as.formula(paste("choice ~",
                         paste(REDUCED_FORM_SPEC, collapse = " + "),
                         "| 0 | 0"))

fit <- mlogit(fmla, data = unassist,
              chid.var = "chid", alt.var = "plan_id",
              weights = ipweight)

cat("\n  Coefficients:\n")
print(summary(fit)$CoefTable)

coefs <- tibble(term = names(coef(fit)), estimate = coef(fit))
fwrite(coefs, "results/choice_coefficients.csv")

# Phase 3: OOS predictions for assisted, ATT by cell × plan ---------------

cat("\nPhase 3: OOS predictions on assisted enrollees...\n")

assist <- pooled[assisted == 1L]
keep_a <- assist[choice == 1L, unique(chid)]
assist <- assist[chid %in% keep_a]

# V = X β per row, P(j | HH) = exp(V_j) / sum_k exp(V_k)
beta <- coefs$estimate
names(beta) <- coefs$term

# Counterfactual = as if unassisted. The baseline spec carries no control
# function (see fmla above), so this is a plain out-of-sample prediction. The
# cf_ skip is defensive in case a cf term is ever added back to the spec.
V <- numeric(nrow(assist))
for (v in coefs$term) {
  if (startsWith(v, "cf_")) next
  if (v %in% names(assist)) {
    col <- assist[[v]]
    col[is.na(col)] <- 0
    V <- V + beta[[v]] * col
  }
}
assist[, V := V]
assist[, exp_V := exp(V - max(V)), by = chid]
assist[, pred  := exp_V / sum(exp_V), by = chid]

plan_summary <- assist[, .(
  tot_nonmiss   = .N,
  obs_purchase  = sum(choice, na.rm = TRUE),
  pred_purchase = sum(pred,   na.rm = TRUE)
), by = .(plan_id, region, year)]

fwrite(plan_summary, "results/choice_point_estimates.csv")
cat("  Predictions:", nrow(plan_summary),
    "rows -> results/choice_point_estimates.csv\n")

rm(assist); gc(verbose = FALSE)


# Phase 3b: Appendix -- baseline ATT on NEW enrollees (sample-sensitivity) -----
# The body plan-choice ATT is on all enrollees (to match the structural model).
# This repeats the baseline prediction-based ATT on new enrollees only, so the
# appendix can show the body result is not driven by the sample. Baseline =
# unassisted-only fit, no control function, IPW; unassisted-only, so no
# SAMPLE_FRAC. Guarded so it stays inert until the cells carry new_enrollee
# (build3 + a one-time cell rebuild).

if ("new_enrollee" %in% names(pooled)) {
  cat("\nPhase 3b: New-enrollee baseline ATT (appendix sample check)...\n")

  un_new <- pooled[assisted == 0L & new_enrollee == 1L]
  un_new <- un_new[chid %in% un_new[choice == 1L, unique(chid)]]
  fit_new <- mlogit(fmla, data = un_new, chid.var = "chid",
                    alt.var = "plan_id", weights = ipweight)

  as_new <- pooled[assisted == 1L & new_enrollee == 1L]
  as_new <- as_new[chid %in% as_new[choice == 1L, unique(chid)]]
  beta_new <- coef(fit_new)
  Vn <- numeric(nrow(as_new))
  for (v in names(beta_new)) {
    if (startsWith(v, "cf_")) next
    if (v %in% names(as_new)) { col <- as_new[[v]]; col[is.na(col)] <- 0; Vn <- Vn + beta_new[[v]] * col }
  }
  as_new[, V := Vn]
  as_new[, exp_V := exp(V - max(V)), by = chid]
  as_new[, pred  := exp_V / sum(exp_V), by = chid]
  new_summary <- as_new[, .(tot_nonmiss   = .N,
                            obs_purchase  = sum(choice, na.rm = TRUE),
                            pred_purchase = sum(pred,   na.rm = TRUE)),
                        by = .(plan_id, region, year)]
  fwrite(new_summary, "results/choice_point_estimates_new.csv")
  cat("  New-enrollee baseline ->", nrow(new_summary),
      "rows -> results/choice_point_estimates_new.csv\n")
  rm(un_new, as_new, fit_new); gc(verbose = FALSE)
}


# Phase 4: bootstrap (within-cell chid resampling, stratified by region × year)

n_boot <- if (exists("N_BOOT")) N_BOOT else 50L

if (n_boot > 0L) {
  cat("\nPhase 4: Bootstrap (", n_boot, "reps)...\n")
  set.seed(MASTER_SEED + 1L)

  chid_cells <- unique(pooled[, .(chid, region, year)])
  boot_results <- vector("list", n_boot)

  for (b in seq_len(n_boot)) {
    # Within each (region, year), resample chids with replacement; preserve cell sizes
    sampled <- chid_cells[, .(chid = sample(chid, .N, replace = TRUE)),
                          by = .(region, year)]
    sampled[, b_chid := paste0("b", b, "_", .I)]   # unique id per occurrence

    pool_b <- merge(sampled[, .(chid, b_chid)], pooled, by = "chid",
                    allow.cartesian = TRUE)
    pool_b[, chid := b_chid][, b_chid := NULL]

    un_b <- pool_b[assisted == 0L]
    keep <- un_b[choice == 1L, unique(chid)]
    un_b <- un_b[chid %in% keep]

    fit_b <- tryCatch(
      mlogit(fmla, data = un_b, chid.var = "chid", alt.var = "plan_id",
             weights = ipweight),
      error = function(e) {
        cat("  rep", b, "fit failed:", conditionMessage(e), "\n"); NULL
      }
    )
    if (is.null(fit_b)) { rm(pool_b, un_b); gc(verbose = FALSE); next }

    as_b <- pool_b[assisted == 1L]
    keep_a <- as_b[choice == 1L, unique(chid)]
    as_b <- as_b[chid %in% keep_a]
    rm(pool_b, un_b)

    beta_b <- coef(fit_b)
    V_b <- numeric(nrow(as_b))
    for (v in names(beta_b)) {
      if (startsWith(v, "cf_")) next          # v_hat -> 0 in the counterfactual
      if (v %in% names(as_b)) {
        col <- as_b[[v]]; col[is.na(col)] <- 0
        V_b <- V_b + beta_b[[v]] * col
      }
    }
    as_b[, V := V_b]
    as_b[, exp_V := exp(V - max(V)), by = chid]
    as_b[, pred  := exp_V / sum(exp_V), by = chid]

    boot_results[[b]] <- as_b[, .(
      tot_nonmiss   = .N,
      obs_purchase  = sum(choice, na.rm = TRUE),
      pred_purchase = sum(pred,   na.rm = TRUE),
      boot          = b
    ), by = .(plan_id, region, year)]

    rm(as_b, fit_b); gc(verbose = FALSE)
    if (b %% 5L == 0L) cat("  rep", b, "/", n_boot, "\n")
  }

  boot_long <- rbindlist(boot_results, fill = TRUE)
  fwrite(boot_long, "results/choice_bootstrap_pred.csv")
  cat("  Bootstrap output:", nrow(boot_long),
      "rows -> results/choice_bootstrap_pred.csv\n")
  rm(boot_results, boot_long, chid_cells)
}

# Phase 5: Appendix -- pooled assisted-as-covariate MNL, progressive specs ------
# The selection story as coefficients, the appendix companion to the body's
# prediction-based ATT. Pooled over both groups, IPW-weighted, with "assisted x
# metal" as the assisted effect on metal choice. Reported as a progressive
# sequence mirroring the dominated table: plan attributes only, then the full
# control set, then the full set plus the broker-density control function
# (cf_ = v_hat x plan attribute).
#
# There is no region-FE column and there cannot be one. A household-level region
# dummy is differenced out of a conditional logit, and the term that would
# actually control region here is region interacted with metal and insurer --
# more than a hundred additional parameters, with the insurer block structurally
# empty wherever an insurer does not operate. So region is uncontrolled in this
# model rather than absorbed, and the text says so.
#
# Both groups enter, so households are subsampled to SAMPLE_FRAC. The
# unassisted-only baseline in Phase 2 stays at full.

cat("\nPhase 5: Appendix pooled assisted-as-covariate MNL (progressive specs)...\n")

set.seed(MASTER_SEED)
ap_chids <- pooled[choice == 1L, unique(chid)]
ap_chids <- sample(ap_chids, floor(SAMPLE_FRAC * length(ap_chids)))
pool_ap  <- pooled[chid %in% ap_chids]
pool_ap  <- pool_ap[chid %in% pool_ap[choice == 1L, unique(chid)]]
cat(sprintf("  SAMPLE_FRAC=%.2f: %s rows, %s households\n", SAMPLE_FRAC,
            format(nrow(pool_ap), big.mark = ","),
            format(uniqueN(pool_ap$chid), big.mark = ",")))

PLAN_ATTRS   <- c("premium", "silver", "bronze", "hmo", "hsa",
                  "Anthem", "Blue_Shield", "Kaiser", "Health_Net")
ASSIST_METAL <- c("assisted_silver", "assisted_bronze", "assisted_gold", "assisted_plat")
CF_TERMS     <- c("cf_silver", "cf_bronze", "cf_anthem", "cf_blue_shield",
                  "cf_kaiser", "cf_health_net")

ap_specs <- list(
  attrs   = c(PLAN_ATTRS, ASSIST_METAL),
  full    = c(REDUCED_FORM_SPEC, ASSIST_METAL),
  full_cf = c(REDUCED_FORM_SPEC, ASSIST_METAL, CF_TERMS)
)

ap_tab <- rbindlist(lapply(names(ap_specs), function(nm) {
  fmla <- as.formula(paste("choice ~", paste(ap_specs[[nm]], collapse = " + "), "| 0 | 0"))
  fit  <- mlogit(fmla, data = pool_ap, chid.var = "chid",
                 alt.var = "plan_id", weights = ipweight)
  cat("  spec", nm, "- logLik", round(as.numeric(logLik(fit)), 1), "\n")
  data.table(term = names(coef(fit)), estimate = coef(fit), spec = nm)
}))

fwrite(ap_tab, "results/choice_appendix_pooled.csv")
cat("  Assisted x metal (and CF) coefficients:\n")
print(dcast(ap_tab[grepl("^assisted_|^cf_", term)], term ~ spec, value.var = "estimate"))

rm(pool_ap, ap_chids); gc(verbose = FALSE)


rm(pooled); gc(verbose = FALSE)
cat("Choice model ATT estimation complete.\n")
