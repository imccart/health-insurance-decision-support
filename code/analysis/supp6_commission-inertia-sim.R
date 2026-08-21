# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Commission-inertia equivalence simulation for the supplemental
##                appendix. A static plan-choice model with commissions but no
##                inertia recovers the same CUMULATIVE effect of commissions on
##                choice as a dynamic model with both commissions and inertia,
##                because the static commission coefficient absorbs the
##                persistence that inertia would otherwise carry.
##
##                Two-period DGP. Commissions steer the period-1 choice; that
##                choice persists into period 2 through an inertia term, so a
##                commission has a direct period-2 push plus a compounding effect
##                through the state it created in period 1. We estimate a dynamic
##                model (commission + inertia) and a static model (commission
##                only) on the period-2 data, and compare three marginal effects
##                of a plan's own commission on its period-2 share: the DGP's
##                cumulative effect (both periods respond), the DGP's direct-only
##                effect (period-1 state held fixed), and the static model's
##                implied effect. The static effect should match the cumulative,
##                not the direct.
##
##                Self-contained. Not in the driver.

set.seed(20260224)

N     <- 300000L
J     <- 5L
p     <- c(2.0, 3.5, 3.0, 4.5, 4.0)   # plan prices (relative)
comm  <- c(0.5, 2.0, 1.0, 3.0, 1.5)   # plan commissions (independent of price)
alpha <- -0.60                        # price coefficient
betaC <-  0.35                        # commission steering (direct)
gamma <-  2.20                        # inertia (switching cost), large

softmax_row <- function(v) { e <- exp(v - max(v)); e / sum(e) }
rgumbel <- function(n) -log(-log(runif(n)))

# Period 1: commissions steer choice --------------------------------------
Vsys <- alpha * p + betaC * comm
V1 <- matrix(Vsys, N, J, byrow = TRUE) + matrix(rgumbel(N * J), N, J)
choice1 <- max.col(V1, ties.method = "first")

# Period 2: same systematic utility + inertia toward the period-1 plan -----
inertia <- matrix(0, N, J); inertia[cbind(seq_len(N), choice1)] <- 1
V2 <- matrix(Vsys, N, J, byrow = TRUE) + gamma * inertia + matrix(rgumbel(N * J), N, J)
choice2 <- max.col(V2, ties.method = "first")

# Sufficient statistic: choice1 x choice2 counts ---------------------------
tab <- table(factor(choice1, 1:J), factor(choice2, 1:J))
tab <- matrix(as.numeric(tab), J, J)
n2  <- colSums(tab)

# Estimate on the period-2 data -------------------------------------------
nll_dyn <- function(th) {           # commission + inertia
  a <- th[1]; b <- th[2]; g <- th[3]; ll <- 0
  for (t in seq_len(J)) {
    V <- a * p + b * comm + g * (seq_len(J) == t)
    ll <- ll + sum(tab[t, ] * (V - (max(V) + log(sum(exp(V - max(V)))))))
  }
  -ll
}
nll_stat <- function(th) {          # commission only, no inertia
  a <- th[1]; b <- th[2]
  V <- a * p + b * comm
  -sum(n2 * (V - (max(V) + log(sum(exp(V - max(V)))))))
}
dyn  <- optim(c(0, 0, 0), nll_dyn,  method = "BFGS", control = list(reltol = 1e-12))$par
stat <- optim(c(0, 0),    nll_stat, method = "BFGS", control = list(reltol = 1e-12))$par

cat(sprintf("True:    alpha %.3f  betaC %.3f  gamma %.3f\n", alpha, betaC, gamma))
cat(sprintf("Dynamic: alpha %.3f  betaC %.3f  gamma %.3f\n", dyn[1], dyn[2], dyn[3]))
cat(sprintf("Static:  alpha %.3f  betaC %.3f  (no inertia term)\n", stat[1], stat[2]))
cat(sprintf("  static commission coef / dynamic direct coef = %.2f x\n", stat[2] / dyn[2]))

# Period-2 shares as a function of commissions (analytical DGP mixture) -----
dgp_s2 <- function(cm, a = alpha, b = betaC, g = gamma) {
  s1 <- softmax_row(a * p + b * cm)
  s2 <- numeric(J)
  for (t in seq_len(J)) s2 <- s2 + s1[t] * softmax_row(a * p + b * cm + g * (seq_len(J) == t))
  s2
}
dgp_s2_directonly <- function(cm, s1_fixed, a = alpha, b = betaC, g = gamma) {
  s2 <- numeric(J)
  for (t in seq_len(J)) s2 <- s2 + s1_fixed[t] * softmax_row(a * p + b * cm + g * (seq_len(J) == t))
  s2
}
stat_s2 <- function(cm) softmax_row(stat[1] * p + stat[2] * cm)

# Own-commission marginal effect on period-2 share, per plan ---------------
eps <- 1e-4
s1_base <- softmax_row(alpha * p + betaC * comm)
me_cum <- me_dir <- me_stat <- numeric(J)
for (k in seq_len(J)) {
  cmk <- comm; cmk[k] <- cmk[k] + eps
  me_cum[k]  <- (dgp_s2(cmk)[k] - dgp_s2(comm)[k]) / eps
  me_dir[k]  <- (dgp_s2_directonly(cmk, s1_base)[k] - dgp_s2_directonly(comm, s1_base)[k]) / eps
  me_stat[k] <- (stat_s2(cmk)[k] - stat_s2(comm)[k]) / eps
}

cat("\nOwn-commission marginal effect on period-2 share (per plan):\n")
cat(sprintf("  %-22s %s\n", "plan", paste(sprintf("%6d", 1:J), collapse = "")))
cat(sprintf("  %-22s %s\n", "DGP cumulative",      paste(sprintf("%6.3f", me_cum),  collapse = "")))
cat(sprintf("  %-22s %s\n", "DGP direct only",     paste(sprintf("%6.3f", me_dir),  collapse = "")))
cat(sprintf("  %-22s %s\n", "Static model implied", paste(sprintf("%6.3f", me_stat), collapse = "")))
cat(sprintf("\n  corr(static, cumulative) = %.4f ; corr(static, direct) = %.4f\n",
            cor(me_stat, me_cum), cor(me_stat, me_dir)))
cat(sprintf("  mean |static - cumulative| = %.4f ; mean |static - direct| = %.4f\n",
            mean(abs(me_stat - me_cum)), mean(abs(me_stat - me_dir))))

# Appendix table ----------------------------------------------------------
lines <- c(
  "\\begin{tabular}{lccccc}", "\\hline\\hline",
  paste0("Own-commission marginal effect & ",
         paste(sprintf("Plan %d", seq_len(J)), collapse = " & "), " \\\\"),
  "\\hline",
  paste0("Dynamic model, cumulative & ",
         paste(sprintf("%.3f", me_cum),  collapse = " & "), " \\\\"),
  paste0("Dynamic model, direct only & ",
         paste(sprintf("%.3f", me_dir),  collapse = " & "), " \\\\"),
  paste0("Static commission model & ",
         paste(sprintf("%.3f", me_stat), collapse = " & "), " \\\\"),
  "\\hline\\hline", "\\end{tabular}")
writeLines(lines, "results/tables/commission_inertia_sim.tex")
cat("  -> results/tables/commission_inertia_sim.tex\n")
