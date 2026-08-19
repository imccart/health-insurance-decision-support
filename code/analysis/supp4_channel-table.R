# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Standalone appendix table build. Tabulates the Covered
##                California service-channel codes in the raw enrollment file,
##                with each code's description, the higher-level category we
##                group it into (agent / navigator / unassisted), and its share
##                of enrollment records. Not part of the numbered pipeline.

pacman::p_load(data.table)

enroll <- fread("data/input/Covered California/pra_07192019.csv")
sc <- enroll[, .N, by = service_channel]
sc[, share := N / sum(N) * 100]

meta <- data.table(
  service_channel = c("CIA", "PBE", "SCR", "CEC", "CEW", "Unassisted"),
  descr = c("Certified insurance agent", "Plan-based enroller",
            "Service-center representative", "Certified enrollment counselor",
            "Certified enrollment worker", "Unassisted"),
  category = c("Agent", "Agent", "Navigator", "Navigator", "Navigator", "Unassisted"))

tab <- merge(meta, sc, by = "service_channel", all.x = TRUE)
tab <- tab[match(meta$service_channel, service_channel)]

lines <- c("\\begin{tabular}{lllr}", "\\hline\\hline",
           "Code & Description & Grouped as & Share \\\\", "\\hline")
for (i in seq_len(nrow(tab))) {
  r <- tab[i]
  lines <- c(lines, sprintf("%s & %s & %s & %.1f\\%% \\\\",
                            r$service_channel, r$descr, r$category, r$share))
}
lines <- c(lines, "\\hline\\hline", "\\end{tabular}")
writeLines(lines, "results/tables/assistance_channels.tex")
cat("  -> results/tables/assistance_channels.tex\n")
cat(paste(lines, collapse = "\n"), "\n")
