# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Alternative nesting structures for demand. Estimates three
##                three-level nests (enroll, channel, plan) in
##                helpers/estimate_demand_alt_nest.R, in each of which the
##                channel choice depends on the channel's menu value as well
##                as the first-stage covariates, and compares them with the
##                two-part model on the same households at observed premiums:
##                  menu   enrollment equation of the two-part model; the
##                         channel covariates set channel use only
##                  std    standard three-level nest
##                  std_e  standard three-level nest with year effects and
##                         agent density in the enrollment equation
##                Scenarios: the agent channel removed (against the two-part
##                model's tau gradient), commissions halved, and commissions
##                set to zero. A quarter of the households in each 20% choice
##                cell (a 5% sample).
##
##                Sourced by _analysis.R after s2_demand.R.

CELL_DIR <- file.path(TEMP_DIR, "choice_cells")

spec_df <- read.csv(file.path(TEMP_DIR, "demand_spec.csv"), stringsAsFactors = FALSE)
asst    <- spec_df$term[spec_df$group == "assisted"]
body    <- read.csv("results/choice_coefficients_structural.csv", stringsAsFactors = FALSE)
bd <- read.csv("data/output/broker_density.csv", stringsAsFactors = FALSE)
a10k <- as.list(setNames(as.numeric(scale(bd$agents_per_10k)), paste(bd$region, bd$year, sep = "_")))

models <- list(
  menu  = list(covars = spec_df$term, normalize = TRUE),
  std   = list(covars = spec_df$term, normalize = FALSE),
  std_e = list(covars = c(spec_df$term, "inside_a10k", paste0("inside_", AN_YEAR_TERMS)), normalize = FALSE))

theta_of <- function(df, covars) {
  nm <- c(covars, "lambda", "lambda_C", paste0("chN_", AN_Z_TERMS), paste0("chA_", AN_Z_TERMS))
  m <- match(nm, df$term)
  ifelse(is.na(m), 0, df$estimate[m])
}
logistic <- function(z) 1 / (1 + exp(-z))
lse <- function(a) { mx <- apply(a, 1, max); mx + log(rowSums(exp(a - mx))) }

# Nest shares of uninsured, unassisted, navigator, agent
nest_shares <- function(th, cells, K, normalize, comm_scale, agents = TRUE) {
  tp <- an_split_theta(th, K)
  acc <- numeric(4); den <- 0
  for (cl in cells) {
    cl$comm_ins <- cl$comm_ins * comm_scale
    pc <- an_cell_ll_pieces(tp, cl, normalize); lc <- tp$lamC
    U <- cbind(pc$U_0, pc$U_N, pc$U_A)
    W <- cbind(0, pc$WN, pc$WA)
    if (!agents) { U <- U[, 1:2, drop = FALSE]; W <- W[, 1:2, drop = FALSE] }
    IE <- lc * lse(U / lc) - if (normalize) lc * lse(W / lc) else 0
    s <- logistic(IE - pc$V_0)
    g <- exp(U / lc - lse(U / lc))
    if (!agents) g <- cbind(g, 0)
    acc <- acc + colSums(cl$wt * cbind(1 - s, s * g))
    den <- den + sum(cl$wt)
  }
  acc / den
}

# Two-part model: uninsured share with the agent weight moved tau to
# navigators and 1 - tau to unassisted (as cf_cell.R does), and with
# commissions scaled
taus <- seq(0, 1, by = 0.05)
two_part <- function(cells, K, comm_scale) {
  th <- theta_of(body, models$menu$covars)
  th[K + 2] <- 1
  tp <- an_split_theta(th, K)
  num <- numeric(length(taus) + 1); den <- 0
  for (cl in cells) {
    cl$comm_ins <- cl$comm_ins * comm_scale
    pc <- an_cell_ll_pieces(tp, cl, FALSE); lam <- tp$lambda
    num[1] <- num[1] + sum(cl$wt * (1 - logistic(lam * (cl$p0 * pc$I_0 + cl$pN * pc$I_N + cl$pA * pc$I_A) - pc$V_0)))
    for (k in seq_along(taus)) {
      ibar <- (cl$p0 + (1 - taus[k]) * cl$pA) * pc$I_0 + (cl$pN + taus[k] * cl$pA) * pc$I_N
      num[k + 1] <- num[k + 1] + sum(cl$wt * (1 - logistic(lam * ibar - pc$V_0)))
    }
    den <- den + sum(cl$wt)
  }
  num / den
}

coefs <- list(); scen <- list(); summ <- list()
for (nm in names(models)) {
  md <- models[[nm]]
  K <- length(md$covars)
  if (nm != "std") cells <- an_load_cells(CELL_DIR, md$covars, asst, a10k, sub_frac = 0.25, seed0 = MASTER_SEED)

  if (nm == "menu") {
    tp1 <- two_part(cells, K, 1)
    tau_curve <- data.frame(tau = taus, d_unins = tp1[-1] - tp1[1])
    two_part_comm <- c(comm_half = two_part(cells, K, 0.5)[1] - tp1[1],
                       comm_zero = two_part(cells, K, 0)[1] - tp1[1])
  }

  fit <- estimate_demand_alt_nest(cells, md$covars, body, md$normalize)
  coefs[[nm]] <- data.frame(model = nm, fit)
  th <- theta_of(fit, md$covars)
  base <- nest_shares(th, cells, K, md$normalize, 1)
  d <- rbind(no_agents = nest_shares(th, cells, K, md$normalize, 1, agents = FALSE) - base,
             comm_half = nest_shares(th, cells, K, md$normalize, 0.5) - base,
             comm_zero = nest_shares(th, cells, K, md$normalize, 0) - base)
  colnames(d) <- c("unins", "unassisted", "navigator", "agent")
  scen[[nm]] <- data.frame(model = nm, scenario = rownames(d), d, row.names = NULL)
  du <- d["no_agents", "unins"]
  summ[[nm]] <- data.frame(model = nm, negll = an_accumulate(th, cells, K, md$normalize, FALSE)$negll,
                           base_unins = base[1], base_agent = base[4],
                           tau_flow = d["no_agents", "navigator"] / (d["no_agents", "navigator"] + d["no_agents", "unassisted"]),
                           tau_coverage = if (du > max(tau_curve$d_unins) || du < min(tau_curve$d_unins)) NA
                                          else approx(tau_curve$d_unins, tau_curve$tau, xout = du)$y)
  if (nm != "menu") rm(cells)
}
coefs <- do.call(rbind, coefs); scen <- do.call(rbind, scen); summ <- do.call(rbind, summ)
write.csv(coefs, "results/alt_nest_coefficients.csv", row.names = FALSE)
write.csv(scen, "results/alt_nest_scenarios.csv", row.names = FALSE)
write.csv(summ, "results/alt_nest_summary.csv", row.names = FALSE)
write.csv(tau_curve, "results/alt_nest_tau_curve.csv", row.names = FALSE)
write.csv(data.frame(scenario = names(two_part_comm), unins = two_part_comm),
          "results/alt_nest_two_part_commissions.csv", row.names = FALSE)

# Bare tabulars for the appendix ------------------------------------------
lab <- c(premium              = "Premium",
         av                   = "Actuarial value (AV)",
         assisted_av          = "Navigator $\\times$ AV",
         broker_av            = "Agent $\\times$ AV",
         assisted_premium     = "Navigator $\\times$ premium",
         broker_premium       = "Agent $\\times$ premium",
         commission_broker    = "Commission $\\times$ agent",
         commission_broker_sq = "Commission$^2$/100 $\\times$ agent",
         lambda               = "$\\lambda$ (plans within channel)",
         lambda_C             = "$\\lambda_C$ (channels within enrollment)")
fmt <- function(x) ifelse(is.na(x), "", sprintf("%.4f", x))
cf <- function(nm, t) coefs$estimate[coefs$model == nm & coefs$term == t]
tl <- c("\\begin{tabular}{lcccc}", "\\hline\\hline",
        " & Two-part & Nest (A) & Nest (B) & Nest (C) \\\\", "\\hline")
for (t in names(lab))
  tl <- c(tl, sprintf("%s & %s & %s & %s & %s \\\\", lab[[t]], fmt(body$estimate[match(t, body$term)]),
                      fmt(cf("menu", t)), fmt(cf("std", t)), fmt(cf("std_e", t))))
tl <- c(tl, sprintf("Log-likelihood & & %s & %s & %s \\\\",
                    format(round(-summ$negll[summ$model == "menu"]), big.mark = ","),
                    format(round(-summ$negll[summ$model == "std"]), big.mark = ","),
                    format(round(-summ$negll[summ$model == "std_e"]), big.mark = ",")),
        "\\hline\\hline", "\\end{tabular}")
writeLines(tl, "results/tables/alt_nest_coefficients.tex")

pp <- function(x) ifelse(is.na(x), "", sprintf("%.1f", 100 * x))
rows <- c(no_agents = "No agent channel", comm_half = "Commissions halved", comm_zero = "Commissions zero")
heads <- c(menu = "Nest (A)", std = "Nest (B)", std_e = "Nest (C)")
tl <- c("\\begin{tabular}{lcccc}", "\\hline\\hline",
        " & Uninsured & Unassisted & Navigator & Agent \\\\", "\\hline",
        "\\multicolumn{5}{l}{Two-part model} \\\\")
for (t in c(0, 0.5, 1))
  tl <- c(tl, sprintf("\\quad No agent channel, $\\tau = %s$ & %s & & & \\\\", format(t),
                      pp(tau_curve$d_unins[abs(tau_curve$tau - t) < 1e-9])))
tl <- c(tl, sprintf("\\quad Commissions halved & %s & & & \\\\", pp(two_part_comm[["comm_half"]])),
        sprintf("\\quad Commissions zero & %s & & & \\\\", pp(two_part_comm[["comm_zero"]])))
for (nm in names(heads)) {
  tl <- c(tl, sprintf("\\multicolumn{5}{l}{%s} \\\\", heads[[nm]]))
  for (r in names(rows)) {
    o <- scen[scen$model == nm & scen$scenario == r, ]
    tl <- c(tl, sprintf("\\quad %s & %s & %s & %s & %s \\\\", rows[[r]], pp(o$unins), pp(o$unassisted),
                        pp(o$navigator), pp(o$agent)))
  }
}
tl <- c(tl, "\\hline\\hline", "\\end{tabular}")
writeLines(tl, "results/tables/alt_nest_scenarios.tex")

rm(coefs, scen, summ, body, spec_df, bd, a10k, tau_curve, two_part_comm, models)
