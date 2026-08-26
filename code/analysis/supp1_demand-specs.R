# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Demand specification sensitivity for the supplemental
##                appendix. Four nested-logit specifications that build from a
##                standard plan-attribute model to the body model and its
##                control-function robustness check, all on the same 20% cell
##                data s2_demand.R built (TEMP_DIR/choice_cells), so the body
##                column reproduces the main-text estimates exactly.
##
##                Columns:
##                  (1) plan attributes + premium
##                  (2) + demographic heterogeneity (price and metal preference)
##                  (3) + assistance and commission steering  [= body model]
##                  (4) + broker-density control function
##
##                The reported table leads with the enrollment-weighted mean
##                own-price elasticity, which folds the price coefficient and
##                the nesting parameter together and is stable across the
##                build-up even though the coefficient and lambda trade off. The
##                steering coefficients are reported as marginal effects on the
##                silver metal share (percentage points), the structural
##                analogue of the reduced-form silver ATT.
##
##                Standalone. Run AFTER s2_demand.R has built choice_cells.

# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, data.table)
setDTthreads(1)

source("code/analysis/helpers/estimate_demand.R")
source("code/analysis/helpers/covariates.R")   # get_prem_interactions, menu

# Hyperparameters ---------------------------------------------------------
TEMP_DIR <- "D:/temp-research-data/health-insurance-decision-support"
CELL_DIR <- file.path(TEMP_DIR, "choice_cells")   # built by s2_demand (full spec, 20%)
stopifnot("choice_cells not found — run s2_demand.R first" = dir.exists(CELL_DIR))

# Specification sequence --------------------------------------------------
price     <- "premium"
plan_attr <- c("av", "hmo",
               "Anthem", "Blue_Shield", "Kaiser", "Health_Net")
demo_het  <- c("hh_size_prem", "perc_0to17_prem", "perc_18to34_prem",
               "perc_35to54_prem", "perc_male_prem", "perc_black_prem",
               "perc_hispanic_prem", "perc_asian_prem", "perc_other_prem",
               "FPL_250to400_prem", "FPL_400plus_prem",
               "hh_size_av", "perc_0to17_av", "perc_18to34_av",
               "perc_35to54_av", "perc_male_av", "perc_black_av",
               "perc_hispanic_av", "perc_asian_av", "perc_other_av",
               "FPL_250to400_av", "FPL_400plus_av")
steering  <- c("assisted_av", "broker_av", "assisted_premium", "broker_premium",
               "commission_broker")
cfun      <- "cf_resid"

spec1 <- c(price, plan_attr)
spec2 <- c(spec1, demo_het)
spec3 <- c(spec2, steering)          # = body model
spec4 <- c(spec3, cfun)

# Fitter ------------------------------------------------------------------
fit_nested <- function(covars) {
  cells <- normalize_weights(load_all_cells(CELL_DIR, covars, filter_assisted = -1L)$cells)
  excl_idx <- match(extensive_exclude_terms(covars), covars)   # two-part nested logit, as in s2_demand
  for (ci in seq_along(cells)) cells[[ci]]$excl_idx <- excl_idx
  theta <- bfgs_bhhh(c(rep(0, length(covars)), 1.0), cells)
  setNames(theta, c(covars, "lambda"))
}

# Estimate ----------------------------------------------------------------
cat("\n=== (1) plan attributes + premium ===\n");   col1 <- fit_nested(spec1)
cat("\n=== (2) + demographic heterogeneity ===\n"); col2 <- fit_nested(spec2)
cat("\n=== (3) + steering [body model] ===\n");      col3 <- fit_nested(spec3)
cat("\n=== (4) + control function ===\n");           col4 <- fit_nested(spec4)

fits <- list(col1, col2, col3, col4)

# Persist the full coefficient vectors so downstream summaries need not re-fit.
all_terms <- unique(c(spec4, "lambda"))
fit_wide  <- data.frame(term = all_terms,
                        sapply(fits, function(f) f[match(all_terms, names(f))]))
names(fit_wide)[-1] <- paste0("col", seq_along(fits))
write.csv(fit_wide, "results/demand_spec_fits.csv", row.names = FALSE)

# Sanity check: col (3) reproduces the body estimates.
main <- read.csv("results/choice_coefficients_structural.csv", stringsAsFactors = FALSE)
chk  <- merge(data.frame(term = names(col3), spec3 = as.numeric(col3)), main, by = "term")
cat(sprintf("\n  Col (3) vs body demand estimates: max abs diff = %.6f\n",
            max(abs(chk$spec3 - chk$estimate))))

# Cell data for the elasticity and marginal-effect computations -----------
prem_map <- get_prem_interactions(spec4)
raw_demo <- unique(unlist(prem_map))
need <- unique(c("region", "year", "household_number", "plan_id", "choice",
                 "hh_weight", "hh_size", "premium", "silver", "bronze", "av", "comm_pmpm",
                 spec4, raw_demo))
dat <- rbindlist(lapply(list.files(CELL_DIR, full.names = TRUE), function(f) {
  d <- fread(f); d[, intersect(need, names(d)), with = FALSE]
})) %>% as_tibble()
feat <- intersect(unique(c(spec4, raw_demo, "premium", "silver", "bronze", "av", "comm_pmpm")),
                  names(dat))
dat <- dat %>% mutate(across(all_of(feat), ~ replace_na(as.numeric(.), 0)))

# Within-nest silver share per household given a utility vector.
silver_share <- function(V, lambda) {
  dat %>%
    mutate(V = V) %>%
    filter(plan_id != "Uninsured") %>%
    group_by(region, year, household_number) %>%
    mutate(m = max(V), eV = exp((V - m) / lambda), s_j = eV / sum(eV)) %>%
    summarise(psilv = sum(s_j * silver), w = first(hh_weight), .groups = "drop")
}

# Mean price coefficient and mean own-price elasticity.
price_and_elast <- function(fit) {
  covars <- setdiff(names(fit), "lambda")
  lambda <- fit[["lambda"]]
  coefs_fit <- data.frame(term = names(fit), estimate = as.numeric(fit), stringsAsFactors = FALSE)
  u <- compute_utility(dat, coefs_fit)
  # Price slope in utils per $100 of net premium: full (with channel slopes) and
  # base (without), per row
  slope <- function(base) {
    a <- fit[["premium"]]
    pm <- prem_map
    if (base) pm <- pm[setdiff(names(pm), extensive_exclude_terms(names(pm)))]
    for (nm in names(pm)) {
      if (!nm %in% names(fit)) next
      rc <- pm[[nm]]
      if (rc %in% names(dat)) a <- a + fit[[nm]] * dat[[rc]]
    }
    a
  }
  d <- as.data.table(dat)
  d[, `:=`(alpha_f = slope(FALSE), alpha_b = slope(TRUE), cellkey = paste(region, year))]
  d[, household_number := paste(cellkey, household_number)]   # unique across cells
  ins <- nest_inside_rows(d, u$V, u$V_base, lambda)
  # Two-part own-price semi-elasticity: d log q_j / d premium_j =
  #   alpha_f (1 - s_jg)/lambda + alpha_b (1 - s_g) s_jg_b, times the premium
  ins[, elast := premium * (alpha_f * (1 - s_jg) / lambda_i + alpha_b * (1 - s_g) * s_jg_b)]
  hh <- d[, .SD[1], by = household_number]
  c(price = weighted.mean(hh$alpha_f, hh$hh_weight, na.rm = TRUE),
    elast = weighted.mean(ins$elast, ins$q_j * ins$hh_weight, na.rm = TRUE))
}

# Average marginal effect of assistance on the silver metal share (pp). Assign
# every household to unassisted / navigator / broker, recompute the within-nest
# silver share, and difference vs unassisted. NA for specs without steering.
assist_cols <- c("assisted_av", "broker_av", "assisted_premium", "broker_premium",
                 "commission_broker")
assist_mfx <- function(fit) {
  covars <- setdiff(names(fit), "lambda")
  if (!any(assist_cols %in% covars)) return(c(nav = NA_real_, brk = NA_real_))
  lambda <- fit[["lambda"]]
  nonass <- intersect(setdiff(covars, assist_cols), names(dat))
  Vbase  <- as.numeric(as.matrix(dat[, nonass]) %*% fit[nonass])
  b <- function(nm) if (nm %in% names(fit)) fit[[nm]] else 0
  V_nav <- Vbase + b("assisted_av") * dat$av + b("assisted_premium") * dat$premium
  V_brk <- Vbase + b("broker_av") * dat$av +
           b("broker_premium") * dat$premium + b("commission_broker") * dat$comm_pmpm
  s_un  <- silver_share(Vbase, lambda)
  s_nav <- silver_share(V_nav, lambda)
  s_brk <- silver_share(V_brk, lambda)
  c(nav = weighted.mean(s_nav$psilv - s_un$psilv, s_un$w) * 100,
    brk = weighted.mean(s_brk$psilv - s_un$psilv, s_un$w) * 100)
}

pe  <- sapply(fits, price_and_elast)   # rows: price, elast
mfx <- sapply(fits, assist_mfx)        # rows: nav, brk
avg_price <- pe["price", ]; avg_elast <- pe["elast", ]
nav_mfx   <- mfx["nav", ];  brk_mfx   <- mfx["brk", ]

cat("\n  mean own-price elast.:", paste(round(avg_elast, 3), collapse = "  "), "\n")
cat("  navigator silver (pp):", paste(round(nav_mfx, 2), collapse = "  "), "\n")
cat("  broker silver (pp):   ", paste(round(brk_mfx, 2), collapse = "  "), "\n")

# CSV (all summaries, for traceability) -----------------------------------
getp <- function(f, t) if (t %in% names(f)) unname(f[t]) else NA_real_
csv <- data.frame(
  parameter = c("mean_own_price_elasticity", "mean_price_coefficient", "lambda",
                "navigator_silver_pp", "broker_silver_pp",
                "assisted_av", "broker_av", "commission_broker"),
  t(sapply(seq_along(fits), function(i) c(
    avg_elast[i], avg_price[i], fits[[i]][["lambda"]], nav_mfx[i], brk_mfx[i],
    getp(fits[[i]], "assisted_av"), getp(fits[[i]], "broker_av"),
    getp(fits[[i]], "commission_broker")))) %>% t()
)
names(csv)[-1] <- paste0("col", seq_along(fits))
write.csv(csv, "results/demand_spec_sensitivity.csv", row.names = FALSE)
cat("  -> results/demand_spec_sensitivity.csv\n")

# Bare tabular for \input in the appendix ---------------------------------
lambda_v <- sapply(fits, function(f) f[["lambda"]])
comm_v   <- sapply(fits, getp, t = "commission_broker")
rows <- list(
  list(lab = "Mean own-price elasticity",           v = avg_elast, f = "%.2f"),
  list(lab = "$\\lambda$ (nesting parameter)",        v = lambda_v,  f = "%.2f"),
  list(lab = "Navigator effect on silver (pp)",       v = nav_mfx,   f = "%.1f"),
  list(lab = "Broker effect on silver (pp)",          v = brk_mfx,   f = "%.1f"),
  list(lab = "Commission $\\times$ broker",           v = comm_v,    f = "%.3f")
)
lines <- c("\\begin{tabular}{lcccc}", "\\hline\\hline",
           " & (1) & (2) & (3) & (4) \\\\", "\\hline")
for (r in rows) {
  cells <- ifelse(is.na(r$v), "", sprintf(r$f, r$v))
  lines <- c(lines, paste0(r$lab, " & ", paste(cells, collapse = " & "), " \\\\"))
}
lines <- c(lines, "\\hline\\hline", "\\end{tabular}")
writeLines(lines, "results/tables/demand_spec_sensitivity.tex")
cat("  -> results/tables/demand_spec_sensitivity.tex\n")
cat(paste(lines, collapse = "\n"), "\n")
