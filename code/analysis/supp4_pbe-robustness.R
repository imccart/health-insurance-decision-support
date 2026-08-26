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
##                Standalone. Run AFTER s2_demand.R and supp1_demand-specs.R.

# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, data.table)
setDTthreads(1)

source("code/analysis/helpers/estimate_demand.R")

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
cat("PBE household-years in raw enrollment:", format(nrow(pbe_keys), big.mark = ","), "\n")

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
cat(sprintf("PBE households in 20%% cells: %s of %s (%.3f%%)\n",
            format(n_pbe, big.mark = ","), format(n_total, big.mark = ","),
            n_pbe / n_total * 100))

# Estimate the body spec on the full and the PBE-excluded samples ----------
fit_nested <- function(dir, covars) {
  cells <- normalize_weights(load_all_cells(dir, covars, filter_assisted = -1L)$cells)
  excl_idx <- match(extensive_exclude_terms(covars), covars)   # two-part nested logit, as in s2_demand
  for (ci in seq_along(cells)) cells[[ci]]$excl_idx <- excl_idx
  setNames(bfgs_bhhh(c(rep(0, length(covars)), 1.0), cells), c(covars, "lambda"))
}
cat("\n=== full sample (body spec) ===\n");     full  <- fit_nested(CELL_DIR, body_covars)
cat("\n=== PBE-excluded (body spec) ===\n");     noPBE <- fit_nested(FILT_DIR, body_covars)

# Compare the price, steering, and nesting parameters ----------------------
key <- c("premium", "av", "assisted_av", "broker_av", "assisted_premium", "broker_premium",
         "commission_broker", "lambda")
comp <- data.frame(term = key,
                   full = unname(full[key]),
                   no_PBE = unname(noPBE[key]),
                   diff = unname(noPBE[key] - full[key]))
comp[, 2:4] <- lapply(comp[, 2:4], round, 4)
write.csv(comp, "results/pbe_robustness.csv", row.names = FALSE)
cat("\n  -> results/pbe_robustness.csv\n")

# Bare tabular for the appendix -------------------------------------------
lab <- c(premium = "Premium",
         av = "Actuarial value (AV)",
         assisted_av = "Navigator $\\times$ AV",
         broker_av = "Broker $\\times$ AV",
         assisted_premium = "Navigator $\\times$ premium",
         broker_premium = "Broker $\\times$ premium",
         commission_broker = "Commission $\\times$ broker",
         lambda = "$\\lambda$ (nesting parameter)")
rownames(comp) <- comp$term
tl <- c("\\begin{tabular}{lcc}", "\\hline\\hline",
        " & Full sample & Excl.\\ PBE \\\\", "\\hline")
for (t in names(lab))
  tl <- c(tl, sprintf("%s & %.3f & %.3f \\\\", lab[[t]], comp[t, "full"], comp[t, "no_PBE"]))
tl <- c(tl, "\\hline\\hline", "\\end{tabular}")
writeLines(tl, "results/tables/pbe_robustness.tex")
cat("  -> results/tables/pbe_robustness.tex\n")

print(comp, row.names = FALSE)
