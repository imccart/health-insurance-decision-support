# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Prior-year plan check for the demand model. Adds an indicator
##                for the household's prior-year exchange plan to copies of the
##                20% choice cells and re-estimates the body specification with
##                the indicator in the enrollment inclusive value. Reports the
##                key coefficients next to the body estimates, which s2_demand.R
##                produced on the same cells, so only the lagged specification is
##                fitted here.
##
##                Sourced by _analysis.R after s2_demand.R.

CELL_DIR <- file.path(TEMP_DIR, "choice_cells")
LAG_DIR  <- file.path(TEMP_DIR, "choice_cells_lag")

# Prior-year plan per household from the household panel ------------------
hh <- fread(file.path(TEMP_DIR, "hh_choice.csv"),
            select = c("household_id", "year", "plan_id"))
hh <- hh[!is.na(plan_id) & plan_id != ""]
# Prior-year plan in the cell codes: CSR silver variants collapsed as in
# choice.R, and the micro-carriers folded into the OS_ bucket by metal as in
# supply.R
prev <- hh[, .(household_number = as.character(household_id),
               year = as.integer(year) + 1L,
               prev_plan = gsub("SIL(94|73|87)", "SIL", plan_id))]
prev[sub("_.*", "", prev_plan) %in% c("UHC", "CONT"),
     prev_plan := paste0("OS_", sub("^[^_]*_(BR|SIL|G|P|CAT).*$", "\\1", prev_plan))]
setkey(prev, household_number, year)
rm(hh)

# Cells with the prior-year plan indicator ---------------------------------
dir.create(LAG_DIR, showWarnings = FALSE)
invisible(file.remove(list.files(LAG_DIR, full.names = TRUE)))
for (f in list.files(CELL_DIR, pattern = "_data\\.csv$", full.names = TRUE)) {
  d <- fread(f)
  yr <- as.integer(d$year[1])
  prev_of_row <- prev[.(as.character(d$household_number), rep(yr, nrow(d))), prev_plan]
  d[, previous_choice := as.integer(!is.na(prev_of_row) & plan_id == prev_of_row & inside == 1L)]
  fwrite(d, file.path(LAG_DIR, basename(f)))
}
rm(prev)

# Body specification plus the indicator in the enrollment inclusive value --
spec <- read_demand_spec(file.path(TEMP_DIR, "demand_spec.csv"))
write_demand_spec(c(spec$base, "previous_choice"), spec$assisted,
                  file.path(TEMP_DIR, "demand_spec_lag.csv"))
lag_fit <- estimate_demand(
  cell_dir        = LAG_DIR,
  spec_path       = file.path(TEMP_DIR, "demand_spec_lag.csv"),
  out_path        = file.path(TEMP_DIR, "choice_coefficients_lag.csv"),
  filter_assisted = -1L,
  ext_exclude     = spec$assisted
)

# Key coefficients next to the body estimates ------------------------------
body <- read.csv("results/choice_coefficients_structural.csv", stringsAsFactors = FALSE)
key <- c("premium", "av", "assisted_av", "broker_av", "assisted_premium", "broker_premium",
         "commission_broker", "commission_broker_sq", "previous_choice", "lambda")
comp <- data.frame(term     = key,
                   body     = body$estimate[match(key, body$term)],
                   with_lag = lag_fit$estimate[match(key, lag_fit$term)])
write.csv(comp, "results/inertia_lag.csv", row.names = FALSE)

# Switching cost: the prior-year-plan coefficient over the member-weighted mean
# premium slope (base slope with its demographic interactions; premium is in
# $100 per member per month), in dollars per member per year
b  <- setNames(lag_fit$estimate, lag_fit$term)
pm <- get_prem_interactions(names(b))
pm <- pm[setdiff(names(pm), extensive_exclude_terms(names(pm)))]
hh <- rbindlist(lapply(list.files(LAG_DIR, pattern = "_data\\.csv$", full.names = TRUE), function(f)
  fread(f, select = unique(c("choice", "hh_size", unlist(pm))))[choice == 1L]))
alpha <- rep(b[["premium"]], nrow(hh))
for (nm in names(pm)) alpha <- alpha + b[[nm]] * hh[[pm[[nm]]]]
alpha_bar <- weighted.mean(alpha, hh$hh_size)
write.csv(data.frame(previous_choice = b[["previous_choice"]], mean_premium_slope = alpha_bar,
                     switching_cost_annual = b[["previous_choice"]] / abs(alpha_bar) * 100 * 12),
          "results/inertia_switching_cost.csv", row.names = FALSE)

# Bare tabular for the appendix -------------------------------------------
lab <- c(premium              = "Premium",
         av                   = "Actuarial value (AV)",
         assisted_av          = "Navigator $\\times$ AV",
         broker_av            = "Agent $\\times$ AV",
         assisted_premium     = "Navigator $\\times$ premium",
         broker_premium       = "Agent $\\times$ premium",
         commission_broker    = "Commission $\\times$ agent",
         commission_broker_sq = "Commission$^2$/100 $\\times$ agent",
         previous_choice      = "Prior-year plan",
         lambda               = "$\\lambda$ (nesting parameter)")
fmt <- function(x) ifelse(is.na(x), "", sprintf("%.4f", x))
tl <- c("\\begin{tabular}{lcc}", "\\hline\\hline",
        " & Body & With prior-year plan \\\\", "\\hline")
for (t in key)
  tl <- c(tl, sprintf("%s & %s & %s \\\\", lab[[t]],
                      fmt(comp$body[comp$term == t]), fmt(comp$with_lag[comp$term == t])))
tl <- c(tl, "\\hline\\hline", "\\end{tabular}")
writeLines(tl, "results/tables/inertia_lag.tex")
