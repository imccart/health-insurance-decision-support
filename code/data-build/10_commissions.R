# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Agent commission per broker enrollee by insurer and year, from
##                what the insurers report paying agents: the Covered California
##                Supplemental Rate Review Templates (actual commission per
##                member-month by plan year, 2017-2020 filings) and the CMS MLR
##                filings (agents' and brokers' fees and commissions per member,
##                2014-2018; step 9). The two agree where they overlap; the SRRT
##                is used where available and the MLR for 2014-2016. The filings'
##                figure is per member of the insurer's whole individual book, on
##                and off the exchange, at the same rate on both (the Covered
##                California contract and the filings' own statements); the rate
##                per broker enrollee divides it by the book's broker share, the
##                on-exchange share (enrollment data) with off-exchange coverage
##                taken as agent-sold. Used for the carriers whose book is on
##                the exchange; the carriers with a large off-exchange book keep
##                the schedule table's on-exchange rates (kept in full as
##                commission_lookup_schedules.csv). One commission series then
##                enters the demand model, the insurer conditions, and the
##                counterfactuals.
## Input:         data/input/Covered California/rate-filings-srrt/*_SRRT_*.xlsx
##                data/output/mlr_admin.csv (step 9), data/output/demand_households.csv
## Output:        data/output/commission_lookup.csv  (insurer_prefix, year, rate, is_pct)
##                data/output/commission_filings.csv (the components)

cat("Building commissions from the filings...\n")
SRRT_DIR <- "data/input/Covered California/rate-filings-srrt"
SRRT_PREFIX <- c(Anthem = "ANT", Blue_Shield = "BS", Chinese_Community = "CC", Health_Net_HMO = "HN",
                 Health_Net_PPO = "HN", Kaiser = "KA", LA_Care = "LA", Molina = "MOL", Oscar = "OSC",
                 Sharp = "SH", Valley = "VAL", Western = "WEST")

# SRRT: the Actual-to-Expected tabs carry "Agent Commissions" (2019+ filings) or
# "Commission Admin Expenses" (2018 filings) with projected and actual columns;
# some filers report a share of premium rather than dollars.
srrt_rows <- list()
for (f in list.files(SRRT_DIR, pattern = "^20(18|19|20)_SRRT_.*[.]xlsx$", full.names = TRUE)) {
  filer <- sub("^20\\d\\d_SRRT_(.*)[.]xlsx$", "\\1", basename(f))
  if (!filer %in% names(SRRT_PREFIX)) next
  for (sh in grep("Actual-to-Expected", excel_sheets(f), value = TRUE)) {
    py <- as.integer(str_extract(sh, "\\d{4}"))
    tab <- suppressMessages(read_excel(f, sheet = sh, col_names = FALSE, .name_repair = "minimal"))
    for (i in seq_len(nrow(tab))) {
      cells <- unlist(tab[i, ]); cells <- cells[!is.na(cells)]
      if (length(cells) < 3) next
      lab <- str_trim(as.character(cells[1]))
      if (!(lab == "Agent Commissions" || lab == "Commission Admin Expenses")) next
      # projected | actual (| ratio); a projected "n/a" leaves the actual alone
      vals <- cells[-1][1:min(2, length(cells) - 1)]
      nums <- suppressWarnings(as.numeric(vals))
      if (length(nums) < 2 || is.na(nums[2])) next
      srrt_rows[[length(srrt_rows) + 1]] <- tibble(insurer_prefix = SRRT_PREFIX[[filer]], filer = filer,
                                                    year = py, filing_year = as.integer(substr(basename(f), 1, 4)),
                                                    projected = nums[1], actual = nums[2])
    }
  }
}
srrt <- bind_rows(srrt_rows)
cat("  SRRT commission rows:", nrow(srrt), "\n")

mlr <- read_csv("data/output/mlr_admin.csv", show_col_types = FALSE) %>%
  filter(!is.na(mm)) %>%
  select(insurer_prefix, year, mm_book = mm, mlr_comm_pmpm = commission_pmpm, premium_pmpm)

# Shares of premium (values below 0.1) in dollars at the insurer's premium per
# member; the latest filing's actual for each plan year; Health Net's two filers averaged
srrt <- srrt %>%
  left_join(mlr %>% select(insurer_prefix, year, premium_pmpm), by = c("insurer_prefix", "year")) %>%
  group_by(insurer_prefix) %>%
  mutate(premium_pmpm = ifelse(is.na(premium_pmpm), max(premium_pmpm, na.rm = TRUE), premium_pmpm)) %>%
  ungroup() %>%
  mutate(actual_pmpm = ifelse(actual < 0.1, actual * premium_pmpm, actual)) %>%
  arrange(insurer_prefix, filer, year, desc(filing_year)) %>%
  distinct(insurer_prefix, filer, year, .keep_all = TRUE) %>%
  group_by(insurer_prefix, year) %>%
  summarize(srrt_comm_pmpm = mean(actual_pmpm, na.rm = TRUE), .groups = "drop")

# On-exchange member months and broker share by insurer-year (enrolled households)
hh <- fread("data/output/demand_households.csv",
            select = c("plan_id", "year", "household_size", "insured", "broker"))
hh <- hh[insured == 1 & !is.na(plan_id) & plan_id != "" & plan_id != "Uninsured"]
hh[, insurer_prefix := sub("_.*", "", plan_id)]
on_ex <- hh[, .(mm_on = 12 * sum(household_size),
                bs_on = sum(household_size * fifelse(is.na(broker), 0, as.numeric(broker))) / sum(household_size)),
            by = .(insurer_prefix, year)]
rm(hh); gc(verbose = FALSE)

comm <- full_join(srrt, mlr, by = c("insurer_prefix", "year")) %>%
  inner_join(as_tibble(on_ex), by = c("insurer_prefix", "year")) %>%
  mutate(comm_pmpm = ifelse(is.na(srrt_comm_pmpm), mlr_comm_pmpm, srrt_comm_pmpm),
         source    = ifelse(is.na(srrt_comm_pmpm), "MLR", "SRRT"),
         on_share  = ifelse(is.na(mm_book), 1, pmin(mm_on / mm_book, 1)),
         bs_book   = on_share * bs_on + (1 - on_share),
         rate      = comm_pmpm / bs_book) %>%
  filter(is.finite(rate), rate >= 0) %>%
  select(insurer_prefix, year, rate, comm_pmpm, source, srrt_comm_pmpm, mlr_comm_pmpm, on_share, bs_on, bs_book)
cat("  insurer-years with a commission from the filings:", nrow(comm), "\n")
cat("  rate per broker enrollee ($ per member-month) by insurer and year:\n")
print(comm %>% select(insurer_prefix, year, rate) %>% mutate(rate = round(rate, 1)) %>%
        pivot_wider(names_from = year, values_from = rate), n = Inf)

# Hybrid: the filings' figure is per member of the whole individual book, so the
# implied rate is an on-exchange rate without further assumption only for the
# carriers whose book is on the exchange (on-exchange share of members at least
# 0.75: the regional plans, where the schedule table had no year variation).
# Carriers with a large off-exchange book (Anthem, Blue Shield, Health Net,
# Kaiser, Oscar) keep their schedule rates, which are on-exchange by
# construction and vary by year for the three that changed them. UHC, the pooled
# Small group, and Valley 2014 keep their schedule rows.
if (!file.exists("data/output/commission_lookup_schedules.csv"))
  write_csv(read_csv("data/output/commission_lookup.csv", show_col_types = FALSE),
            "data/output/commission_lookup_schedules.csv")
old <- read_csv("data/output/commission_lookup_schedules.csv", show_col_types = FALSE)
on_share_ins <- comm %>% group_by(insurer_prefix) %>% summarize(on_share = mean(on_share), .groups = "drop")
filings_ins <- on_share_ins$insurer_prefix[on_share_ins$on_share >= 0.75]
cat("  on-exchange share of the book by insurer:",
    paste(on_share_ins$insurer_prefix, round(on_share_ins$on_share, 2), collapse = ", "), "\n")
cat("  commission from the filings:", paste(filings_ins, collapse = ", "),
    "; from the schedule table:", paste(setdiff(unique(old$insurer_prefix), filings_ins), collapse = ", "), "\n")
comm_used <- comm %>% filter(insurer_prefix %in% filings_ins)
kept <- old %>% anti_join(comm_used, by = c("insurer_prefix", "year")) %>% select(insurer_prefix, year, rate, is_pct)
lookup <- bind_rows(comm_used %>% transmute(insurer_prefix, year, rate, is_pct = FALSE), kept) %>%
  arrange(insurer_prefix, year)
missing <- on_ex %>% filter(mm_on > 0) %>% anti_join(lookup, by = c("insurer_prefix", "year"))
if (nrow(missing) > 0) cat("  WARNING: enrolled insurer-years with no commission row:",
                           paste(missing$insurer_prefix, missing$year, collapse = ", "), "\n")
write_csv(comm, "data/output/commission_filings.csv")
write_csv(lookup, "data/output/commission_lookup.csv")
cat("  -> data/output/commission_lookup.csv, commission_filings.csv\n")
