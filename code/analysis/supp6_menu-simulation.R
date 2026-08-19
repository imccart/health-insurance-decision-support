# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Menu-restriction simulation for the supplemental appendix.
##                Generates plan choices from a GATEKEEPER data-generating
##                process, in which a broker presents a restricted menu tilted
##                toward high-commission insurers and the household chooses its
##                best plan from that menu, with no commission term in utility.
##                We then fit our COMMISSION-IN-UTILITY model on the full menu
##                and show it reproduces the gatekeeper model's plan-choice
##                shares. The two are observationally equivalent for plan-choice
##                predictions; our specification loads the menu tilt onto the
##                commission coefficient without distorting predicted choices.
##
##                Self-contained (no data). Standalone, not in the driver.

pacman::p_load(tidyverse)
set.seed(20260224)

# Market primitives --------------------------------------------------------
K          <- 6                                  # insurers (one plan each)
commission <- c(0.5, 1.0, 1.5, 2.5, 3.5, 4.5)    # varies across insurers
price      <- runif(K, 2, 6)                     # net premium ($100), independent of commission
appeal     <- rnorm(K, 0, 1)                     # base insurer appeal
alpha_true <- -0.5                               # price coefficient in the DGP (no commission term)
N          <- 50000L                             # households per assistance type

base_u <- alpha_true * price + appeal

# Gatekeeper availability: higher commission -> more likely on the broker's menu
avail_prob <- plogis(0.4 + 0.9 * (commission - mean(commission)))

gumbel <- function(n) -log(-log(runif(n)))
softmax <- function(v) { e <- exp(v - max(v)); e / sum(e) }

sim_choices <- function(n, gatekeeper) {
  U <- matrix(base_u, n, K, byrow = TRUE) + matrix(gumbel(n * K), n, K)
  if (gatekeeper) {
    A <- matrix(runif(n * K), n, K) < matrix(avail_prob, n, K, byrow = TRUE)
    A[rowSums(A) == 0, ] <- TRUE                 # if the menu is empty, full menu (rare)
    U[!A] <- -Inf
  }
  max.col(U, ties.method = "first")
}

# DGP: brokers face the restricted menu, unassisted face the full menu -----
ch_b <- sim_choices(N, gatekeeper = TRUE)
ch_u <- sim_choices(N, gatekeeper = FALSE)
s_b  <- tabulate(ch_b, K) / N                    # gatekeeper broker shares
s_u  <- tabulate(ch_u, K) / N                    # unassisted shares

# Fit the commission-in-utility model on the FULL menu for everyone --------
# P^U = softmax(a*price + delta);  P^B = softmax(a*price + delta + bC*commission)
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

# Fit quality --------------------------------------------------------------
max_diff <- max(abs(s_b - PB_fit))
corr     <- cor(s_b, PB_fit)
cat(sprintf("estimated commission coefficient = %.3f\n", bC))
cat(sprintf("broker shares, DGP vs fitted: max abs diff = %.4f, correlation = %.4f\n",
            max_diff, corr))
write.csv(data.frame(commission_coef = bC, max_abs_diff = max_diff, correlation = corr),
          "results/menu_simulation_stats.csv", row.names = FALSE)

# Comparison table for the appendix ---------------------------------------
ord <- order(commission)
lines <- c("\\begin{tabular}{rrrrr}", "\\hline\\hline",
           "Insurer & Commission & Unassisted & \\shortstack{Broker\\\\(gatekeeper)} & \\shortstack{Broker\\\\(fitted)} \\\\",
           "\\hline")
for (k in ord) {
  lines <- c(lines, sprintf("%d & %.1f & %.3f & %.3f & %.3f \\\\",
                            k, commission[k], s_u[k], s_b[k], PB_fit[k]))
}
lines <- c(lines, "\\hline\\hline", "\\end{tabular}")
writeLines(lines, "results/tables/menu_simulation.tex")
cat("  -> results/tables/menu_simulation.tex\n")
cat(paste(lines, collapse = "\n"), "\n")
