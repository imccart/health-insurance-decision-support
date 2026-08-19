# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Standalone appendix table build. Formats the Covered
##                California standardized cost-sharing schedule
##                (data/input/ca_standard_cost_sharing.csv) into a bare
##                tabular for the supplemental appendix. Shows 2019 as a
##                representative year; the full 2014-2019 schedule, which
##                varies by year, is what the welfare objective uses.
##                Not part of the numbered pipeline.

pacman::p_load(data.table)

cs <- fread("data/input/ca_standard_cost_sharing.csv")

cs[, tier := ifelse(metal == "Bronze" & hsa == 1L, "Bronze (HSA)", metal)]
ord    <- c("Bronze", "Bronze (HSA)", "Silver", "Silver - Enhanced 73",
            "Silver - Enhanced 87", "Silver - Enhanced 94", "Gold", "Platinum")
labels <- c("Bronze", "Bronze (HSA)", "Silver", "Silver (CSR 73)",
            "Silver (CSR 87)", "Silver (CSR 94)", "Gold", "Platinum")

cs19 <- cs[year == 2019][match(ord, tier)]

dol <- function(x) paste0("\\$", format(x, big.mark = ",", trim = TRUE))
lines <- c(
  "\\begin{tabular}{lrrrr}",
  "\\hline\\hline",
  "Tier & AV & Deductible & Coins. & MOOP \\\\",
  "\\hline"
)
for (i in seq_len(nrow(cs19))) {
  r <- cs19[i]
  lines <- c(lines, sprintf("%s & %.2f & %s & %d\\%% & %s \\\\",
                            labels[i], r$av, dol(r$deductible),
                            as.integer(round(r$coinsurance * 100)), dol(r$moop)))
}
lines <- c(lines, "\\hline\\hline", "\\end{tabular}")

writeLines(lines, "results/tables/cost_sharing_schedule.tex")
cat("  -> results/tables/cost_sharing_schedule.tex\n")
cat(paste(lines, collapse = "\n"), "\n")
