# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Menu-restriction robustness for the demand model. Plan-based
##                enrollers (service_channel == "PBE") are affiliated with a
##                single insurer, so their influence on insurer choice is a menu
##                restriction rather than a commission response. We merge the raw
##                service_channel onto the existing 20% choice cells by household
##                and year, drop the PBE households, and re-estimate the body
##                demand specification on that same sample. No rebuild: uses the
##                cells s2_demand.R already produced.
##
##                Sourced by _analysis.R after s2_demand.R and supp1_demand-specs.R.

CELL_DIR <- "D:/temp-research-data/health-insurance-decision-support/choice_cells"
FILT_DIR <- "D:/temp-research-data/health-insurance-decision-support/choice_cells_noPBE"

# Body specification (col3 in the demand-spec sensitivity fits) ------------
fits <- read.csv("results/demand_spec_fits.csv", stringsAsFactors = FALSE)
body_covars <- setdiff(fits$term[!is.na(fits$col3)], "lambda")

# PBE household-years from the raw enrollment -----------------------------
pra <- fread("data/input/Covered California/pra_07192019.csv",
             select = c("ahbx_case_id_x", "enrlee_enrlmnt_yr", "service_channel"))
setnames(pra, c("household_number", "year", "service_channel"))
pbe_keys <- unique(pra[service_channel == "PBE", .(household_number, year = as.integer(year))])
pbe_keys[, is_pbe := 1L]

# Filter the cells: drop PBE households, write to a parallel directory -----
dir.create(FILT_DIR, showWarnings = FALSE)
invisible(file.remove(list.files(FILT_DIR, full.names = TRUE)))
n_total <- 0L; n_pbe <- 0L
for (f in list.files(CELL_DIR, full.names = TRUE)) {
  d <- fread(f)
  d[, year := as.integer(year)]
  d <- merge(d, pbe_keys, by = c("household_number", "year"), all.x = TRUE)
  d[is.na(is_pbe), is_pbe := 0L]
  hh <- unique(d[, .(household_number, year, is_pbe)])
  n_total <- n_total + nrow(hh); n_pbe <- n_pbe + sum(hh$is_pbe)
  d[is_pbe == 0L][, is_pbe := NULL] |> fwrite(file.path(FILT_DIR, basename(f)))
}
write.csv(data.frame(n_pbe = n_pbe, n_total = n_total, pct_pbe = n_pbe / n_total * 100),
          "results/pbe_share.csv", row.names = FALSE)

# Full-sample estimates from s2; the body spec re-estimated without PBE ------
fit_nested <- function(dir, covars) {
  cells <- normalize_weights(load_all_cells(dir, covars, filter_assisted = -1L)$cells)
  cells <- prepare_cells(cells, covars, extensive_exclude_terms(covars))   # channel-state enrollment margin, as in s2_demand
  setNames(bfgs_bhhh(c(rep(0, length(covars)), 1.0), cells), c(covars, "lambda"))
}
body  <- read.csv("results/choice_coefficients_structural.csv", stringsAsFactors = FALSE)
full  <- setNames(body$estimate, body$term)
noPBE <- fit_nested(FILT_DIR, body_covars)

# Compare the price, steering, and nesting parameters ----------------------
key <- c("premium", "av", "assisted_av", "broker_av", "assisted_premium", "broker_premium",
         "commission_broker", "commission_broker_sq", "lambda")
comp <- data.frame(term = key,
                   full = unname(full[key]),
                   no_PBE = unname(noPBE[key]),
                   diff = unname(noPBE[key] - full[key]))
comp[, 2:4] <- lapply(comp[, 2:4], round, 4)
write.csv(comp, "results/pbe_robustness.csv", row.names = FALSE)

# Bare tabular for the appendix -------------------------------------------
lab <- c(premium = "Premium",
         av = "Actuarial value (AV)",
         assisted_av = "Navigator $\\times$ AV",
         broker_av = "Agent $\\times$ AV",
         assisted_premium = "Navigator $\\times$ premium",
         broker_premium = "Agent $\\times$ premium",
         commission_broker = "Commission $\\times$ agent",
         commission_broker_sq = "Commission$^2$/100 $\\times$ agent",
         lambda = "$\\lambda$ (nesting parameter)")
rownames(comp) <- comp$term
tl <- c("\\begin{tabular}{lcc}", "\\hline\\hline",
        " & Full sample & Excl.\\ PBE \\\\", "\\hline")
for (t in names(lab))
  tl <- c(tl, sprintf("%s & %.3f & %.3f \\\\", lab[[t]], comp[t, "full"], comp[t, "no_PBE"]))
tl <- c(tl, "\\hline\\hline", "\\end{tabular}")
writeLines(tl, "results/tables/pbe_robustness.tex")

