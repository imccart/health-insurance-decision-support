# sum3_retention.R — Channel persistence and next-year retention by channel.
#
# The assistance terms enter plan choice conditional on enrolling and are
# excluded from the enrollment decision (two-part nested logit, s2_demand).
# This step reports the panel facts behind that restriction: how stable a
# household's channel is across enrolled years, and whether next-year
# retention on the model's margin (enrolled vs off but market-eligible) differs
# by channel. Reads the data-build panel from disk; assumes the _analysis.R
# preamble is loaded.
#
# Outputs:
#   results/channel_transitions.csv
#   results/channel_retention.csv
#   results/tables/channel_transitions.tex
#   results/tables/channel_retention.tex

cat("\n=== sum3: channel persistence and retention ===\n")

enr <- fread("data/output/enrollment_hh.csv",
             select = c("household_id", "year", "agent", "broker", "navigator"))
un  <- fread("data/output/cc_uninsured.csv",
             select = c("household_id", "year", "market_eligible"))
cat("  enrolled HH-years:", nrow(enr), " off-year rows:", nrow(un), "\n")
stopifnot(uniqueN(enr, by = c("household_id", "year")) == nrow(enr),
          uniqueN(un,  by = c("household_id", "year")) == nrow(un))

enr[, channel := fcase(broker == 1L | agent == 1L, "Agent",
                       navigator == 1L,            "Navigator",
                       default = "Unassisted")]
chan_levels <- c("Agent", "Navigator", "Unassisted")

# 1. Channel transitions across consecutive enrolled years ------------------
setorder(enr, household_id, year)
enr[, n_years := .N, by = household_id]
enr[, `:=`(next_year = shift(year, -1L), next_channel = shift(channel, -1L)), by = household_id]
trans <- enr[!is.na(next_year) & next_year == year + 1L, .N, by = .(channel, next_channel)]
trans[, share := N / sum(N), by = channel]
trans_wide <- dcast(trans, channel ~ next_channel, value.var = "share")
trans_n    <- trans[, .(n_pairs = sum(N)), by = channel]
trans_wide <- merge(trans_wide, trans_n, by = "channel")
trans_wide <- trans_wide[match(chan_levels, channel)]
write_csv(trans_wide, "results/channel_transitions.csv")

hh <- unique(enr[, .(household_id, n_years)])
hh_ch <- enr[, .(n_channels = uniqueN(channel)), by = household_id]
single_share <- merge(hh, hh_ch, by = "household_id")[n_years >= 2, mean(n_channels == 1)]
cat(sprintf("  HH with 2+ enrolled years and one channel throughout: %.3f\n", single_share))

# 2. Next-year retention by channel ------------------------------------------
un[, year_prev := year - 1L]
enr <- merge(enr, un[, .(household_id, year = year_prev, elig_next = market_eligible)],
             by = c("household_id", "year"), all.x = TRUE)
enr[, enrolled_next := !is.na(next_year) & next_year == year + 1L]
enr[, status_next := fcase(enrolled_next, "enrolled",
                           !enrolled_next & !is.na(elig_next) & elig_next == 1L, "off_eligible",
                           !enrolled_next & !is.na(elig_next) & elig_next == 0L, "off_ineligible",
                           default = "off_not_in_panel")]
enr[, first_year := min(year), by = household_id]
maxy <- max(enr$year)

ret_fun <- function(d) d[, .(n = .N,
                             retained = mean(status_next == "enrolled")),
                         by = channel][match(chan_levels, channel)]
base_rows <- enr[year < maxy & status_next %in% c("enrolled", "off_eligible")]
ret <- rbind(
  ret_fun(base_rows)[, sample := "All enrollees"],
  ret_fun(base_rows[year == first_year])[, sample := "New enrollees"],
  ret_fun(base_rows[year >  first_year])[, sample := "Continuing enrollees"]
)
# Raw next-year status shares (including exits from the individual market)
raw <- enr[year < maxy, .N, by = .(channel, status_next)]
raw[, share := N / sum(N), by = channel]
raw_wide <- dcast(raw, channel ~ status_next, value.var = "share")[match(chan_levels, channel)]
write_csv(ret, "results/channel_retention.csv")
write_csv(raw_wide, "results/channel_status_next.csv")
cat("  Retention on the model's margin (enrolled t+1 | enrolled or off-eligible):\n")
print(ret)

# 3. Tables -------------------------------------------------------------------
fmt_pct <- function(x) sprintf("%.1f", 100 * x)
tab_trans <- trans_wide %>%
  mutate(across(all_of(chan_levels), fmt_pct),
         n_pairs = formatC(n_pairs, format = "d", big.mark = ",")) %>%
  select(channel, all_of(chan_levels), n_pairs)
kbl_trans <- kable(tab_trans, format = "latex", booktabs = TRUE, linesep = "",
                   col.names = c("Channel in $t$", "Agent", "Navigator", "Unassisted", "HH-year pairs"),
                   align = c("l", "r", "r", "r", "r"), escape = FALSE) %>%
  add_header_above(c(" " = 1, "Channel in $t+1$ (percent)" = 3, " " = 1), escape = FALSE)
writeLines(as.character(kbl_trans), "results/tables/channel_transitions.tex")

tab_ret <- ret %>%
  mutate(retained = fmt_pct(retained), n = formatC(n, format = "d", big.mark = ",")) %>%
  select(sample, channel, retained, n) %>%
  pivot_wider(names_from = channel, values_from = c(retained, n)) %>%
  select(sample, retained_Agent, retained_Navigator, retained_Unassisted,
         n_Agent, n_Navigator, n_Unassisted)
kbl_ret <- kable(tab_ret, format = "latex", booktabs = TRUE, linesep = "",
                 col.names = c("Sample", "Agent", "Navigator", "Unassisted",
                               "Agent", "Navigator", "Unassisted"),
                 align = c("l", rep("r", 6)), escape = FALSE) %>%
  add_header_above(c(" " = 1, "Retained next year (percent)" = 3, "Household-years" = 3))
writeLines(as.character(kbl_ret), "results/tables/channel_retention.tex")
cat("  Wrote results/tables/channel_transitions.tex and channel_retention.tex\n")
rm(enr, un, hh, hh_ch, trans, raw, base_rows); gc(verbose = FALSE)
