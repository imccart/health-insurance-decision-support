# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Menu-restriction simulation for the supplemental appendix.
##                Generates plan choices from a GATEKEEPER data-generating
##                process, in which a broker presents a restricted menu tilted
##                toward high-commission insurers and the household chooses its
##                best plan from that menu, with no commission term in utility.
##                We fit our COMMISSION-IN-UTILITY model on the full menu and
##                show it reproduces the gatekeeper model's plan-choice shares.
##                The two are observationally equivalent for plan-choice
##                predictions; our specification loads the menu tilt onto the
##                commission coefficient without distorting predicted choices.
##
##                Two parts. The first is the baseline, in which menu
##                availability depends only on commission. The second checks that
##                the equivalence survives patient and agent selection, letting
##                the menu be selected on an unobserved household type that also
##                shifts utility, swept from no selection to strong, both aligned
##                with commission (realistic) and orthogonal to it (adversarial).
##
##                Self-contained (no data). Standalone, not in the driver.
##                Writes menu_simulation.tex and gatekeeper_selection.tex.

pacman::p_load(tidyverse, data.table)

# Market primitives --------------------------------------------------------
K          <- 6
commission <- c(0.5, 1.0, 1.5, 2.5, 3.5, 4.5)
alpha_true <- -0.5
N          <- 50000L

gumbel  <- function(n) -log(-log(runif(n)))
softmax <- function(v) { e <- exp(v - max(v)); e / sum(e) }

# =========================================================================
# Part 1. Baseline gatekeeper: menu availability depends only on commission
# =========================================================================
set.seed(20260224)
price  <- runif(K, 2, 6)
appeal <- rnorm(K, 0, 1)
base_u <- alpha_true * price + appeal
avail_prob <- plogis(0.4 + 0.9 * (commission - mean(commission)))

sim_choices <- function(n, gatekeeper) {
  U <- matrix(base_u, n, K, byrow = TRUE) + matrix(gumbel(n * K), n, K)
  if (gatekeeper) {
    A <- matrix(runif(n * K), n, K) < matrix(avail_prob, n, K, byrow = TRUE)
    A[rowSums(A) == 0, ] <- TRUE
    U[!A] <- -Inf
  }
  max.col(U, ties.method = "first")
}

ch_b <- sim_choices(N, gatekeeper = TRUE)
ch_u <- sim_choices(N, gatekeeper = FALSE)
s_b  <- tabulate(ch_b, K) / N
s_u  <- tabulate(ch_u, K) / N

negll <- function(par) {
  a <- par[1]; d <- c(0, par[2:K]); bC <- par[K + 1]
  PU <- softmax(a * price + d)
  PB <- softmax(a * price + d + bC * commission)
  -(N * sum(s_u * log(PU)) + N * sum(s_b * log(PB)))
}
opt <- optim(c(-0.5, rep(0, K - 1), 0), negll, method = "BFGS",
             control = list(maxit = 1000, reltol = 1e-12))
a  <- opt$par[1]; d <- c(0, opt$par[2:K]); bC <- opt$par[K + 1]
PB_fit <- softmax(a * price + d + bC * commission)

max_diff <- max(abs(s_b - PB_fit)); corr <- cor(s_b, PB_fit)
cat(sprintf("Baseline: commission coef = %.3f, max abs diff = %.4f, correlation = %.4f\n",
            bC, max_diff, corr))
write.csv(data.frame(commission_coef = bC, max_abs_diff = max_diff, correlation = corr),
          "results/menu_simulation_stats.csv", row.names = FALSE)

ord <- order(commission)
lines <- c("\\begin{tabular}{rrrrr}", "\\hline\\hline",
           "Insurer & Commission & Unassisted & \\shortstack{Agent\\\\(gatekeeper)} & \\shortstack{Agent\\\\(fitted)} \\\\",
           "\\hline")
for (k in ord) {
  lines <- c(lines, sprintf("%d & %.1f & %.3f & %.3f & %.3f \\\\",
                            k, commission[k], s_u[k], s_b[k], PB_fit[k]))
}
lines <- c(lines, "\\hline\\hline", "\\end{tabular}")
writeLines(lines, "results/tables/menu_simulation.tex")
cat("  -> results/tables/menu_simulation.tex\n")

# =========================================================================
# Part 2. The equivalence under patient and agent selection
# =========================================================================
# The gatekept menu is correlated with an unobserved household type (the agent a
# household is matched to, or patient sorting) that also shifts utility, so the
# selection is a genuine confounder. We sweep the selection strength (0 is the
# baseline above) for a target aligned with commission and a target orthogonal
# to it, and check that the commission-in-utility model still reproduces the
# gatekeeper broker shares.
set.seed(20260224)
price  <- runif(K, 2, 6)
appeal <- rnorm(K, 0, 1)
base_u <- alpha_true * price + appeal
cm_c   <- (commission - mean(commission)) / sd(commission)
ortho  <- residuals(lm(rnorm(K) ~ commission)); ortho <- ortho / sd(ortho)

sim_shares <- function(s, lam, tvec, gatekeeper) {
  z <- rnorm(N)
  U <- matrix(base_u, N, K, byrow = TRUE) + outer(z, lam * tvec) + matrix(gumbel(N * K), N, K)
  if (gatekeeper) {
    lp <- matrix(0.4 + 0.9 * (commission - mean(commission)), N, K, byrow = TRUE) + outer(z, s * tvec)
    A  <- matrix(runif(N * K), N, K) < plogis(lp)
    A[rowSums(A) == 0, ] <- TRUE
    U[!A] <- -Inf
  }
  tabulate(max.col(U, ties.method = "first"), K) / N
}
fit_comm <- function(su, sb) {
  nll <- function(par) {
    a <- par[1]; d <- c(0, par[2:K]); bC <- par[K + 1]
    -(sum(su * log(softmax(a * price + d))) +
      sum(sb * log(softmax(a * price + d + bC * commission))))
  }
  o <- optim(c(-0.5, rep(0, K - 1), 0), nll, method = "BFGS", control = list(reltol = 1e-12))
  a <- o$par[1]; d <- c(0, o$par[2:K]); bC <- o$par[K + 1]
  list(bC = bC, PB = softmax(a * price + d + bC * commission))
}
run_sel <- function(s, tvec) {
  lam <- 0.5 * s
  su  <- sim_shares(0, lam, tvec, gatekeeper = FALSE)
  sb  <- sim_shares(s, lam, tvec, gatekeeper = TRUE)
  f   <- fit_comm(su, sb)
  c(bC = f$bC, max_diff = max(abs(sb - f$PB)), corr = cor(sb, f$PB))
}

grid <- c(0, 0.5, 1.0, 2.0)
rows <- list()
for (nm in c("aligned", "orthogonal")) {
  tvec <- if (nm == "aligned") cm_c else ortho
  for (s in grid) {
    r <- run_sel(s, tvec)
    rows[[paste(nm, s)]] <- data.frame(regime = nm, strength = s, commission_coef = r["bC"],
                                       max_abs_diff = r["max_diff"], correlation = r["corr"])
  }
}
sel <- bind_rows(rows); rownames(sel) <- NULL
cat("Selection sweep (strength 0 is the baseline):\n"); print(sel, digits = 4)
write.csv(sel, "results/gatekeeper_selection_stats.csv", row.names = FALSE)

lines <- c("\\begin{tabular}{lrrrr}", "\\hline\\hline",
           "Selection & Strength & Commission coef. & Max abs.\\ diff.\\ & Correlation \\\\", "\\hline")
for (nm in c("aligned", "orthogonal")) {
  lab <- if (nm == "aligned") "Aligned with commission" else "Orthogonal to commission"
  for (s in grid) {
    r <- sel[sel$regime == nm & sel$strength == s, ]
    lines <- c(lines, sprintf("%s & %.1f & %.3f & %.4f & %.4f \\\\",
                              if (s == grid[1]) lab else "", s, r$commission_coef, r$max_abs_diff, r$correlation))
  }
  if (nm == "aligned") lines <- c(lines, "\\hline")
}
lines <- c(lines, "\\hline\\hline", "\\end{tabular}")
writeLines(lines, "results/tables/gatekeeper_selection.tex")
cat("  -> results/tables/gatekeeper_selection.tex\n")
