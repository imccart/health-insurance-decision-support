# cf_headline.R — headline counterfactual statistics from one CF welfare table.
#
# summarize_cf_headline(cf) takes a welfare table with one row per (region, year,
# scenario) carrying the cf2 welfare columns and returns the named vector of
# headline statistics the paper reports: each is a mean over cells of a scenario's
# welfare column minus the model-baseline value. Shared by cf3_se (bootstrap
# draws) and cf4_se-comm (delta-method gradients), so both difference exactly the
# statistics sum2 reports.

summarize_cf_headline <- function(cf) {
  cf  <- as.data.frame(cf)
  obs <- unique(cf[cf$scenario == "baseline",
                   c("region", "year", "cs_weighted", "cs_nocomm",
                     "cs_welfare_nav", "cs_welfare_obj",
                     "obj_prem", "obj_eoop", "obj_risk",
                     "obj_insured", "share_unins", "unins_oop", "unins_mort", "unins_cat")])
  mdelta <- function(scen, col) {                  # mean over cells of (col[scen] - col[baseline])
    s <- unique(cf[cf$scenario == scen, c("region", "year", col)])
    s <- s[!duplicated(s[c("region", "year")]), ]
    o <- obs[, c("region", "year", col)]; names(o)[3] <- "obsval"
    m <- merge(s, o, by = c("region", "year"))
    if (nrow(m) == 0) return(NA_real_)
    mean(m[[col]] - m$obsval, na.rm = TRUE)
  }
  taus <- c(0, 0.25, 0.5, 0.75, 1.0)
  grad <- vapply(taus, function(t) mdelta(sprintf("zero_tau%.2f", t), "cs_weighted"), numeric(1))
  names(grad) <- paste0("grad_cs_tau", sprintf("%.2f", taus))
  # Endogenous-commission scenarios (endog_tau0 = baseline, not carried).
  taus_e <- c(0.5, 1.0)
  grad_e <- vapply(taus_e, function(t) mdelta(sprintf("endog_tau%.2f", t), "cs_weighted"), numeric(1))
  names(grad_e) <- paste0("grad_cs_endog_tau", sprintf("%.2f", taus_e))
  # Cost-band components per scenario (coverage share, insured composition, and the
  # uninsured-weighted OOP / baseline mortality / catastrophic pieces). sum2 rebuilds
  # the objective band from these.
  comp_scen <- c("zero_tau0.00", "zero_tau1.00", "uniform", "aligned",
                 "endog_tau1.00", "flat_mandate", "defund_1.00")
  comp <- unlist(lapply(comp_scen, function(s)
    setNames(c(mdelta(s, "share_unins"), mdelta(s, "obj_insured"), mdelta(s, "unins_oop"),
               mdelta(s, "unins_mort"),  mdelta(s, "unins_cat")),
             paste0(c("dshare_", "dobjins_", "doop_", "dmort_", "dcat_"), s))))
  # obj decomposed into premium / expected-OOP / risk.
  c(va_cs            = unname(grad["grad_cs_tau1.00"] - grad["grad_cs_tau0.00"]),
    grad,
    va_nav           = mdelta("zero_tau1.00", "cs_welfare_nav") - mdelta("zero_tau0.00", "cs_welfare_nav"),
    va_obj           = mdelta("zero_tau1.00", "cs_welfare_obj") - mdelta("zero_tau0.00", "cs_welfare_obj"),
    va_obj_prem      = mdelta("zero_tau1.00", "obj_prem") - mdelta("zero_tau0.00", "obj_prem"),
    va_obj_eoop      = mdelta("zero_tau1.00", "obj_eoop") - mdelta("zero_tau0.00", "obj_eoop"),
    va_obj_risk      = mdelta("zero_tau1.00", "obj_risk") - mdelta("zero_tau0.00", "obj_risk"),
    grad_e,
    va_cs_endog      = unname(grad_e["grad_cs_endog_tau1.00"]),
    va_nav_endog     = mdelta("endog_tau1.00", "cs_welfare_nav"),
    va_obj_endog     = mdelta("endog_tau1.00", "cs_welfare_obj"),
    flatmand_dcs     = mdelta("flat_mandate", "cs_weighted"),
    flatmand_obj     = mdelta("flat_mandate", "cs_welfare_obj"),
    defund_dcs       = mdelta("defund_1.00", "cs_weighted"),
    defund_obj       = mdelta("defund_1.00", "cs_welfare_obj"),
    aligned_dcs      = mdelta("aligned", "cs_weighted"),
    aligned_dcs_nc   = mdelta("aligned", "cs_nocomm"),
    aligned_nav      = mdelta("aligned", "cs_welfare_nav"),
    aligned_obj      = mdelta("aligned", "cs_welfare_obj"),
    aligned_obj_prem = mdelta("aligned", "obj_prem"),
    aligned_obj_eoop = mdelta("aligned", "obj_eoop"),
    aligned_obj_risk = mdelta("aligned", "obj_risk"),
    comp)
}
