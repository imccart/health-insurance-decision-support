# sum3_ext-margin.R — Robustness of the coverage (extensive-margin) result.
# 1) Decomposes the demand-side objective welfare change from removing assistance
#    (premiums fixed) into a coverage piece and a plan-choice piece, order-independent.
# 2) Traces the coverage effect across the nesting parameter and the outside-option
#    level, and by which assistance term drives it.
# Assumes the _analysis.R preamble is loaded. Reads the cached structural cells.

cat("\n=== sum3: extensive-margin robustness ===\n")

coefs   <- read_csv("results/choice_coefficients_structural.csv", show_col_types = FALSE)
lambda0 <- setNames(coefs$estimate, coefs$term)[["lambda"]]
CS_TABLE <- read.csv("data/input/ca_standard_cost_sharing.csv", stringsAsFactors = FALSE)
SPENDING_SCHEDULE <- load_spending_schedule()
UNINS_SCHED <- load_uninsured_oop()
CELL_DIR <- file.path(TEMP_DIR, "choice_cells")
cells <- list.files(CELL_DIR, pattern = "^cell_.*_data\\.csv$", full.names = TRUE)

PRICE <- c("assisted_premium", "broker_premium")
METAL <- c("assisted_silver", "assisted_bronze", "assisted_gold", "assisted_plat",
           "broker_silver", "broker_bronze")
COMM  <- c("commission_broker")
ALL_ASSIST <- c(PRICE, METAL, COMM)

zero_cols <- function(dt, cols) { dt <- copy(dt); for (c in intersect(cols, names(dt))) dt[[c]] <- 0; dt }

# per-household P(uninsured), with optional nesting override and outside-option shift
p_unins_hh <- function(dt, lambda, v0_shift = 0) {
  V <- compute_utility(dt, coefs)$V
  dt <- as.data.table(dt); dt[, Vx := V]
  dt[plan_id == "Uninsured", Vx := Vx + v0_shift]
  ins <- dt[plan_id != "Uninsured"]
  ins[, vs := Vx / lambda]; ins[, mx := max(vs), by = household_number]
  hh <- ins[, .(logD = first(mx) + log(sum(exp(vs - first(mx)))), w = first(hh_weight)), by = household_number]
  v0 <- dt[plan_id == "Uninsured", .(household_number, V0 = Vx)]
  hh <- merge(hh, v0, by = "household_number", all.x = TRUE); hh[is.na(V0), V0 := 0]
  hh[, m := pmax(V0, lambda * logD)]
  hh[, pu := exp(V0 - m) / (exp(V0 - m) + exp(lambda * logD - m))]
  hh[, .(w, pu)]
}
# weighted (numerator, denominator) for the uninsured share
wnd <- function(dt, cols, lambda = lambda0, v0 = 0) {
  h <- p_unins_hh(if (length(cols)) zero_cols(dt, cols) else dt, lambda, v0)
  c(sum(h$pu * h$w), sum(h$w))
}

dec  <- c(cov = 0, pc = 0, tot = 0, den = 0)               # welfare decomposition
LAM  <- c(0.5, lambda0, 0.85, 1.0); SHIFT <- c(-1, -0.5, 0, 0.5, 1)
chan_nm <- c("full", "price", "metal", "comm")
chan_cols <- list(full = ALL_ASSIST, price = PRICE, metal = METAL, comm = COMM)
base_nd <- c(0, 0)
chan_nd <- setNames(rep(list(c(0, 0)), length(chan_nm)), chan_nm)
lam_nd  <- setNames(rep(list(c(0, 0, 0)), length(LAM)),  as.character(LAM))    # base_num, full_num, den
sh_nd   <- setNames(rep(list(c(0, 0, 0)), length(SHIFT)), as.character(SHIFT))

for (f in cells) {
  y  <- as.integer(sub(".*_(\\d{4})_data\\.csv$", "\\1", f))
  dt <- as.data.table(fread(f))

  # --- welfare decomposition (objective, premiums fixed) ---
  es <- household_spending(dt, SPENDING_SCHEDULE)
  v  <- vN_objective(dt, y, CS_TABLE, SPENDING_CV, RHO_RISK_AVERSION, es, unins_sched = UNINS_SCHED)$v_total
  p_obs <- choice_probs(dt, coefs, lambda0)
  p_cf  <- choice_probs(zero_cols(dt, ALL_ASSIST), coefs, lambda0)
  D <- data.table(hh = dt$household_number, w = dt$hh_weight, v = v, pobs = p_obs, pcf = p_cf,
                  out = dt$plan_id == "Uninsured")
  ins <- D[out == FALSE]; vu <- D[out == TRUE, .(hh, vu = v, w)]
  agg <- ins[, .(Po = sum(pobs), Pc = sum(pcf), SPo = sum(pobs * v), SPc = sum(pcf * v)), by = hh]
  agg <- merge(agg, vu, by = "hh")
  agg[, Wo := fifelse(Po > 1e-12, SPo / Po, vu)]; agg[, Wc := fifelse(Pc > 1e-12, SPc / Pc, vu)]
  agg[, dcov := (Pc - Po) * (0.5 * (Wo + Wc) - vu)]; agg[, dpc := 0.5 * (Pc + Po) * (Wc - Wo)]
  agg[, dtot := (Pc * Wc + (1 - Pc) * vu) - (Po * Wo + (1 - Po) * vu)]
  dec["cov"] <- dec["cov"] + agg[, sum(dcov * w)]; dec["pc"]  <- dec["pc"]  + agg[, sum(dpc * w)]
  dec["tot"] <- dec["tot"] + agg[, sum(dtot * w)]; dec["den"] <- dec["den"] + agg[, sum(w)]

  # --- coverage-effect sensitivity (uninsured share) ---
  base_nd <- base_nd + wnd(dt, character(0))
  for (nm in chan_nm) chan_nd[[nm]] <- chan_nd[[nm]] + wnd(dt, chan_cols[[nm]])
  for (lam in LAM)  { k <- as.character(lam); b <- wnd(dt, character(0), lambda = lam); fu <- wnd(dt, ALL_ASSIST, lambda = lam)
    lam_nd[[k]] <- lam_nd[[k]] + c(b[1], fu[1], b[2]) }
  for (s0 in SHIFT) { k <- as.character(s0); b <- wnd(dt, character(0), v0 = s0); fu <- wnd(dt, ALL_ASSIST, v0 = s0)
    sh_nd[[k]] <- sh_nd[[k]] + c(b[1], fu[1], b[2]) }
}

# --- assemble + write ---
tot <- dec["tot"] / dec["den"]; cov <- dec["cov"] / dec["den"]; pc <- dec["pc"] / dec["den"]
decomp <- data.frame(
  component = c("total", "coverage", "plan_choice"),
  dollars   = round(c(tot, cov, pc), 1),
  share_pct = round(100 * c(tot, cov, pc) / tot, 0))
write_csv(decomp, "results/coverage_decomposition.csv")

base_share <- base_nd[1] / base_nd[2]
sens <- rbind(
  data.frame(dimension = "channel", key = chan_nm,
             effect_pp = round(sapply(chan_nm, function(n) 100 * (chan_nd[[n]][1] / chan_nd[[n]][2] - base_share)), 1),
             base_uninsured = round(base_share, 4)),
  data.frame(dimension = "lambda", key = names(lam_nd),
             effect_pp = round(sapply(lam_nd, function(z) 100 * (z[2] - z[1]) / z[3]), 1),
             base_uninsured = round(sapply(lam_nd, function(z) z[1] / z[3]), 4)),
  data.frame(dimension = "outside_shift", key = names(sh_nd),
             effect_pp = round(sapply(sh_nd, function(z) 100 * (z[2] - z[1]) / z[3]), 1),
             base_uninsured = round(sapply(sh_nd, function(z) z[1] / z[3]), 4)))
write_csv(sens, "results/coverage_sensitivity.csv")

cat(sprintf("  decomposition ($/member/yr): total %+.0f | coverage %+.0f (%.0f%%) | plan choice %+.0f (%.0f%%)\n",
    tot, cov, 100 * cov / tot, pc, 100 * pc / tot))

# --- robustness table (two panels) ---
fmt0 <- function(x) formatC(x, format = "f", digits = 0)
lam_eff <- sapply(as.character(LAM), function(k) sapply(lam_nd, function(z) 100 * (z[2] - z[1]) / z[3])[k])
sh_eff  <- sapply(as.character(SHIFT), function(k) sapply(sh_nd, function(z) 100 * (z[2] - z[1]) / z[3])[k])
rob <- c(
  "\\begin{tabular}{lr}",
  "\\hline\\hline",
  "\\multicolumn{2}{l}{\\textit{Panel A. Welfare change from removing assistance (\\$/member/year)}} \\\\",
  "\\hline",
  sprintf("Total & %s \\\\", fmt0(tot)),
  sprintf("\\quad Coverage margin & %s \\\\", fmt0(cov)),
  sprintf("\\quad Plan-choice margin & %s \\\\", fmt0(pc)),
  "\\hline",
  "\\multicolumn{2}{l}{\\textit{Panel B. Coverage effect (change in uninsured share, pp)}} \\\\",
  "\\hline",
  sprintf("Nesting parameter $\\lambda = %.2f$ & %s \\\\", LAM[1], fmt0(sapply(lam_nd, function(z) 100*(z[2]-z[1])/z[3])[[as.character(LAM[1])]])),
  sprintf("$\\qquad \\lambda = %.2f$ (estimated) & %s \\\\", lambda0, fmt0(sapply(lam_nd, function(z) 100*(z[2]-z[1])/z[3])[[as.character(lambda0)]])),
  sprintf("$\\qquad \\lambda = %.2f$ & %s \\\\", LAM[3], fmt0(sapply(lam_nd, function(z) 100*(z[2]-z[1])/z[3])[[as.character(LAM[3])]])),
  sprintf("$\\qquad \\lambda = %.2f$ & %s \\\\", LAM[4], fmt0(sapply(lam_nd, function(z) 100*(z[2]-z[1])/z[3])[[as.character(LAM[4])]])),
  sprintf("Outside-option value $-1$ util & %s \\\\", fmt0(sapply(sh_nd, function(z) 100*(z[2]-z[1])/z[3])[["-1"]])),
  sprintf("$\\qquad$ baseline & %s \\\\", fmt0(sapply(sh_nd, function(z) 100*(z[2]-z[1])/z[3])[["0"]])),
  sprintf("$\\qquad +1$ util & %s \\\\", fmt0(sapply(sh_nd, function(z) 100*(z[2]-z[1])/z[3])[["1"]])),
  "\\hline\\hline",
  "\\end{tabular}")
writeLines(rob, "results/tables/coverage_robustness.tex")
cat("  Wrote results/tables/coverage_robustness.tex\n")

# --- macros for inline numbers ---
mac <- c(
  sprintf("\\newcommand{\\covWelfTotal}{%s}", fmt0(abs(tot))),
  sprintf("\\newcommand{\\covCoverageDollar}{%s}", fmt0(abs(cov))),
  sprintf("\\newcommand{\\covPlanChoiceDollar}{%s}", fmt0(abs(pc))),
  sprintf("\\newcommand{\\covCoverageShare}{%s}", fmt0(100 * cov / tot)))
writeLines(mac, "results/tables/coverage_macros.tex")
cat("  Wrote results/tables/coverage_macros.tex\n")
cat("sum3 complete.\n")
