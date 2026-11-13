# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-11-10
## Description:   Marginal effects of commission, premium, and actuarial value
##                on plan choice among enrolled households, from the estimated
##                demand model at each household's realized channel.
##                Plan level: one plan's characteristic changes, all others
##                held fixed, effect on that plan's choice probability.
##                Insurer level: the characteristic changes on every plan of one
##                insurer, effect on the probability of choosing that insurer,
##                (1 - S_F) * sum_{k in F} s_k b_k / lambda.
##                Changes: commission +$1 PMPM, premium +$10 per member per
##                month, actuarial value +10 points.
##                Outputs: results/demand_marginal_effects.csv,
##                results/tables/demand_marginal_effects.tex

# Dependencies: _analysis.R preamble (TEMP_DIR, helpers) loaded.


# Coefficients ------------------------------------------------------------

coefs  <- fread("results/choice_coefficients_structural.csv")
lambda <- coefs[term == "lambda", estimate]
coefs  <- coefs[term != "lambda"]
b      <- setNames(coefs$estimate, coefs$term)

demos <- c("hh_size", "perc_0to17", "perc_18to34", "perc_35to54", "perc_male",
           "perc_black", "perc_hispanic", "perc_asian", "perc_other",
           "FPL_250to400", "FPL_400plus")


# Per-cell effects --------------------------------------------------------

cell_files <- sort(list.files(file.path(TEMP_DIR, "choice_cells"),
                              pattern = "^cell_.*_data\\.csv$", full.names = TRUE))

plan_rows    <- vector("list", length(cell_files))
insurer_rows <- vector("list", length(cell_files))

for (i in seq_along(cell_files)) {
  d <- fread(cell_files[i],
             select = unique(c("household_number", "plan_id", "choice", "hh_weight",
                               "comm_hh", names(b))))
  for (cn in names(b)) d[is.na(get(cn)), (cn) := 0]

  # Enrolled households only, insured rows only
  enrolled <- d[choice == 1L & plan_id != "Uninsured", unique(household_number)]
  d <- d[household_number %in% enrolled & plan_id != "Uninsured"]

  X <- as.matrix(d[, names(b), with = FALSE])
  d[, V := as.numeric(X %*% b) / lambda]
  d[, s := exp(V - max(V)), by = household_number]
  d[, s := s / sum(s), by = household_number]

  d[, channel := fcase(any(broker_av != 0), "Agent",
                       any(assisted_av != 0), "Navigator",
                       default = "Unassisted"), by = household_number]
  d[, agent := as.numeric(channel == "Agent")]
  d[, nav   := as.numeric(channel == "Navigator")]

  # Household slopes at the realized channel (premium per $100, AV per unit)
  d[, b_prem := b["premium"] +
        as.numeric(as.matrix(d[, paste0(demos, "_insured"), with = FALSE]) %*% b[paste0(demos, "_prem")]) +
        b["assisted_premium"] * nav + b["broker_premium"] * agent]
  d[, b_av := b["av"] +
        as.numeric(as.matrix(d[, paste0(demos, "_insured"), with = FALSE]) %*% b[paste0(demos, "_av")]) +
        b["assisted_av"] * nav + b["broker_av"] * agent]
  d[, b_comm := (b["commission_broker"] + 2 * b["commission_broker_sq"] * comm_hh / 100) * agent]

  # Effects of the stated changes on the plan's choice probability (points)
  d[, issuer := sub("_.*$", "", plan_id)]
  p <- d[, .(channel, wt = hh_weight, s,
             comm_pp = 100 * s * (1 - s) * b_comm / lambda,
             prem_pp = 100 * s * (1 - s) * b_prem * 0.1 / lambda,
             av_pp   = 100 * s * (1 - s) * b_av   * 0.1 / lambda,
             comm_pct = 100 * (1 - s) * b_comm / lambda,
             prem_pct = 100 * (1 - s) * b_prem * 0.1 / lambda,
             av_pct   = 100 * (1 - s) * b_av   * 0.1 / lambda)]

  f <- d[, .(channel = channel[1], wt = hh_weight[1], S = sum(s),
             A_comm = sum(s * b_comm), A_prem = sum(s * b_prem)),
         by = .(household_number, issuer)][
    , .(channel, wt, S,
        comm_pp = 100 * (1 - S) * A_comm / lambda,
        prem_pp = 100 * (1 - S) * A_prem * 0.1 / lambda,
        av_pp   = NA_real_,
        comm_pct = 100 * (1 - S) * A_comm / (lambda * S),
        prem_pct = 100 * (1 - S) * A_prem * 0.1 / (lambda * S),
        av_pct   = NA_real_)]
  setnames(f, "S", "s")

  # Weighted sums by channel, combined across cells below
  plan_rows[[i]]    <- p[, c(.(sw = sum(wt)), lapply(.SD, function(x) sum(wt * x))),
                         by = channel, .SDcols = -c("channel", "wt")]
  insurer_rows[[i]] <- f[, c(.(sw = sum(wt)), lapply(.SD, function(x) sum(wt * x))),
                         by = channel, .SDcols = -c("channel", "wt")]
  rm(d, X, p, f)
}


# Averages by channel, member-weighted -----------------------------------

me_vars <- c("s", "comm_pp", "prem_pp", "av_pp", "comm_pct", "prem_pct", "av_pct")

plan_me <- rbindlist(plan_rows)[, lapply(.SD, sum), by = channel][
  , c(.(level = "plan", channel = channel), lapply(.SD, function(x) x / sw)),
  .SDcols = me_vars]
insurer_me <- rbindlist(insurer_rows)[, lapply(.SD, sum), by = channel][
  , c(.(level = "insurer", channel = channel), lapply(.SD, function(x) x / sw)),
  .SDcols = me_vars]

fwrite(rbind(plan_me, insurer_me), "results/demand_marginal_effects.csv")


# Table: percent change in the choice probability, by channel -------------

me <- fread("results/demand_marginal_effects.csv")
chan <- c("Unassisted", "Navigator", "Agent")
me_row <- function(lv, v, d = 1, scale = 1) {
  x <- me[level == lv][match(chan, channel), get(v)] * scale
  if (v == "comm_pct") x[chan != "Agent"] <- NA   # commissions reach agent clients only
  paste(ifelse(is.na(x), "", formatC(x, format = "f", digits = d)), collapse = " & ")
}
tl <- c("\\begin{tabular}{lrrr}", "\\hline\\hline",
        " & Unassisted & Navigator & Agent \\\\", "\\hline",
        "\\multicolumn{4}{l}{\\emph{Plan level}} \\\\",
        paste0("\\quad Mean choice probability (\\%) & ", me_row("plan", "s", 1, 100), " \\\\"),
        paste0("\\quad Premium, $+$\\$10 & ", me_row("plan", "prem_pct"), " \\\\"),
        paste0("\\quad Actuarial value, $+$10 points & ", me_row("plan", "av_pct"), " \\\\"),
        paste0("\\quad Commission, $+$\\$1 & ", me_row("plan", "comm_pct"), " \\\\"),
        "\\multicolumn{4}{l}{\\emph{Insurer level}} \\\\",
        paste0("\\quad Mean choice probability (\\%) & ", me_row("insurer", "s", 1, 100), " \\\\"),
        paste0("\\quad Premium, $+$\\$10 & ", me_row("insurer", "prem_pct"), " \\\\"),
        paste0("\\quad Commission, $+$\\$1 & ", me_row("insurer", "comm_pct"), " \\\\"),
        "\\hline\\hline", "\\end{tabular}")
writeLines(tl, "results/tables/demand_marginal_effects.tex")
