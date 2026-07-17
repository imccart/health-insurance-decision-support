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
                   "assisted", "region", "year", REDUCED_FORM_FULL))

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

# Phase 5: MTE (separate approach) -- plan-choice LATE over the support --------
# Two plan-choice MNLs, one on the unassisted and one on the assisted, each with
# the propensity P interacted with the plan attributes (metal, insurer) and
# household-size weighted (the broker-density instrument carries the selection
# correction here, not IPW). Plan-level MTE_j(p) = MTR1_j(p) - MTR0_j(p) with
#   MTR1_j(p) =  d/dp[ p * E(share_j | P=p, assisted) ]
#   MTR0_j(p) = -d/dp[ (1-p) * E(share_j | P=p, unassisted) ],
# summed to metal and insurer over the common support of P -- a LATE for the
# marginal households, no extrapolation to a full ATT. P = assisted - v_hat (the
# build3 first stage); sample size is governed by SAMPLE_FRAC (the cells) since
# both groups now enter the fit.

cat("\nPhase 5: Plan-choice MTE (LATE over the propensity support)...\n")

pooled[, P := assisted - v_hat]
pooled[, `:=`(P_silver = P * silver, P_bronze = P * bronze,
              P_anthem = P * Anthem, P_bs = P * Blue_Shield,
              P_kaiser = P * Kaiser, P_hn = P * Health_Net)]
pooled[, metal := fifelse(grepl("_SIL", plan_id), "Silver",
                  fifelse(grepl("_BR", plan_id), "Bronze",
                  fifelse(grepl("_G", plan_id), "Gold",
                  fifelse(grepl("_P", plan_id), "Platinum", "Cat"))))]
pooled[, insurer := sub("_.*", "", plan_id)]

P_TERMS <- c("P_silver", "P_bronze", "P_anthem", "P_bs", "P_kaiser", "P_hn")
P_DUM   <- c(P_silver = "silver", P_bronze = "bronze", P_anthem = "Anthem",
             P_bs = "Blue_Shield", P_kaiser = "Kaiser", P_hn = "Health_Net")
MTE_SPEC <- c(REDUCED_FORM_SPEC, P_TERMS)
fmla_mte <- as.formula(paste("choice ~", paste(MTE_SPEC, collapse = " + "), "| 0 | 0"))

# Both groups now enter, so subsample households to SAMPLE_FRAC (the top-level
# knob). The unassisted-only baseline in Phase 2 stays at full; every both-group
# mlogit here and in Phase 6 is drawn on the SAME SAMPLE_FRAC households.
set.seed(MASTER_SEED)
mte_chids  <- pooled[choice == 1L, unique(chid)]
mte_chids  <- sample(mte_chids, floor(SAMPLE_FRAC * length(mte_chids)))
pooled_mte <- pooled[chid %in% mte_chids]

un_mte <- pooled_mte[assisted == 0L]; un_mte <- un_mte[chid %in% un_mte[choice == 1L, unique(chid)]]
as_mte <- pooled_mte[assisted == 1L]; as_mte <- as_mte[chid %in% as_mte[choice == 1L, unique(chid)]]
cat(sprintf("  SAMPLE_FRAC=%.2f: unassisted %s rows, assisted %s rows\n", SAMPLE_FRAC,
            format(nrow(un_mte), big.mark = ","), format(nrow(as_mte), big.mark = ",")))

fit_un_mte <- mlogit(fmla_mte, data = un_mte, chid.var = "chid", alt.var = "plan_id", weights = hh_weight)
fit_as_mte <- mlogit(fmla_mte, data = as_mte, chid.var = "chid", alt.var = "plan_id", weights = hh_weight)
beta_un <- coef(fit_un_mte); beta_as <- coef(fit_as_mte)

# Split each row's utility into the P-independent part (baseV) and the P-dummy
# loading (pdum), so a prediction at P=p is just baseV + p*pdum.
prep_mte <- function(dt, beta) {
  baseV <- numeric(nrow(dt))
  for (v in REDUCED_FORM_SPEC) if (v %in% names(beta) && v %in% names(dt)) {
    c0 <- dt[[v]]; c0[is.na(c0)] <- 0; baseV <- baseV + beta[[v]] * c0
  }
  pdum <- numeric(nrow(dt))
  for (pt in P_TERMS) if (pt %in% names(beta)) {
    c0 <- dt[[P_DUM[[pt]]]]; c0[is.na(c0)] <- 0; pdum <- pdum + beta[[pt]] * c0
  }
  dt[, baseV := baseV]; dt[, pdum := pdum]; dt
}
un_mte <- prep_mte(un_mte, beta_un)
as_mte <- prep_mte(as_mte, beta_as)
W_un <- sum(unique(un_mte[, .(chid, hh_weight)])$hh_weight)
W_as <- sum(unique(as_mte[, .(chid, hh_weight)])$hh_weight)

# E(share by group | P=p) marginalized over households, one fitted group.
share_at <- function(dt, W, p, grp) {
  dt[, V := baseV + p * pdum]
  dt[, pr := exp(V - max(V)), by = chid][, pr := pr / sum(pr), by = chid]
  a <- dt[, .(s = sum(pr * hh_weight) / W), by = grp]
  setNames(a$s, a[[grp]])
}

# Common support of P (1st-99th pct overlap of the two groups' HH-level P).
p_lo <- max(quantile(un_mte[choice == 1L, P], 0.01), quantile(as_mte[choice == 1L, P], 0.01))
p_hi <- min(quantile(un_mte[choice == 1L, P], 0.99), quantile(as_mte[choice == 1L, P], 0.99))
eps <- 0.01
mte_grid <- seq(p_lo, p_hi, length.out = 9)

# MTE(p) = MTR1 - MTR0 for each level of a grouping (metal / insurer), over the grid.
mte_by <- function(grp) {
  levs <- union(names(share_at(un_mte, W_un, mean(mte_grid), grp)),
                names(share_at(as_mte, W_as, mean(mte_grid), grp)))
  g <- function(v, L) { x <- v[L]; if (is.na(x)) 0 else x }
  out <- rbindlist(lapply(mte_grid, function(p) {
    eu_hi <- share_at(un_mte, W_un, p + eps, grp); eu_lo <- share_at(un_mte, W_un, p - eps, grp)
    ea_hi <- share_at(as_mte, W_as, p + eps, grp); ea_lo <- share_at(as_mte, W_as, p - eps, grp)
    data.table(propensity = p, level = levs, mte = vapply(levs, function(L) {
      mtr1 <-  ((p + eps) * g(ea_hi, L) - (p - eps) * g(ea_lo, L)) / (2 * eps)
      mtr0 <- -(((1 - p - eps) * g(eu_hi, L) - (1 - p + eps) * g(eu_lo, L)) / (2 * eps))
      mtr1 - mtr0
    }, numeric(1)))
  }))
  out[, group_type := grp][]
}

mte_metal   <- mte_by("metal")
mte_insurer <- mte_by("insurer")
choice_mte  <- rbind(mte_metal, mte_insurer)
fwrite(choice_mte, "results/choice_mte.csv")

late_metal   <- mte_metal[,   .(LATE = mean(mte)), by = level][order(-LATE)]
late_insurer <- mte_insurer[, .(LATE = mean(mte)), by = level][order(-LATE)]
cat(sprintf("  Plan-choice MTE over support [%.2f, %.2f] (LATE = mean over support):\n", p_lo, p_hi))
cat("  --- by metal ---\n");   print(late_metal)
cat("  --- by insurer ---\n"); print(late_insurer)

rm(un_mte, as_mte, fit_un_mte, fit_as_mte); gc(verbose = FALSE)


# Phase 6: Appendix -- pooled assisted-as-covariate MNL, without and with CF ----
# The selection story as coefficients (the appendix companion to the body's
# prediction-based ATT and MTE). Pooled over both groups on the SAME SAMPLE_FRAC
# households as the MTE, IPW-weighted. "Assisted x metal" is the assisted effect
# on metal choice; the +CF version adds the broker-density control function
# (cf_ = v_hat x plan attribute). Region FE is not applicable to this conditional
# logit -- a household-level fixed effect is mechanically absorbed.

cat("\nPhase 6: Appendix pooled assisted-as-covariate MNL (no CF / +CF)...\n")

ASSIST_METAL <- c("assisted_silver", "assisted_bronze", "assisted_gold", "assisted_plat")
CF_TERMS     <- c("cf_silver", "cf_bronze", "cf_anthem", "cf_blue_shield",
                  "cf_kaiser", "cf_health_net")

pool_ap <- pooled_mte[chid %in% pooled_mte[choice == 1L, unique(chid)]]
fmla_ap_nocf <- as.formula(paste("choice ~",
  paste(c(REDUCED_FORM_SPEC, ASSIST_METAL), collapse = " + "), "| 0 | 0"))
fmla_ap_cf   <- as.formula(paste("choice ~",
  paste(c(REDUCED_FORM_SPEC, ASSIST_METAL, CF_TERMS), collapse = " + "), "| 0 | 0"))

fit_ap_nocf <- mlogit(fmla_ap_nocf, data = pool_ap, chid.var = "chid",
                      alt.var = "plan_id", weights = ipweight)
fit_ap_cf   <- mlogit(fmla_ap_cf,   data = pool_ap, chid.var = "chid",
                      alt.var = "plan_id", weights = ipweight)

ap_tab <- rbind(
  data.table(term = names(coef(fit_ap_nocf)), estimate = coef(fit_ap_nocf), spec = "no_CF"),
  data.table(term = names(coef(fit_ap_cf)),   estimate = coef(fit_ap_cf),   spec = "with_CF")
)
fwrite(ap_tab, "results/choice_appendix_pooled.csv")
cat("  Assisted x metal (and CF) coefficients:\n")
print(ap_tab[grepl("^assisted_|^cf_", term)])

rm(pool_ap, pooled_mte, fit_ap_nocf, fit_ap_cf); gc(verbose = FALSE)


rm(pooled); gc(verbose = FALSE)
cat("Choice model ATT estimation complete.\n")
