# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Description:   Plan risk scores from the California Supplemental Rate Review
##                Templates (SRRT), by insurer x metal x rating region x year.
##                Reads the processed file that accompanies the filings
##                (risk_score_data.csv; one row per insurer, plan type, metal,
##                rating area, and year, with the member months behind each
##                score) and writes them keyed to our plan_id prefixes and base
##                metals. Health Net's HMO and PPO/HSP rows are combined within
##                insurer-metal-region-year (member-month weighted), since our
##                plan ids do not carry the network type.
## Input:         data/input/Covered California/rate-filings-srrt/risk_score_data.csv
## Output:        data/output/plan_risk_scores.csv        (insurer_prefix, metal, region, year)
##                data/output/plan_risk_scores_year.csv   (insurer_prefix, metal, year)

cat("Loading SRRT risk scores...\n")
srrt <- read_csv("data/input/Covered California/rate-filings-srrt/risk_score_data.csv",
                 show_col_types = FALSE, name_repair = "minimal")
names(srrt)[1] <- "row_key"
cat("  rows read:", nrow(srrt), "\n")

PREFIX_MAP <- c(Anthem = "ANT", Blue_Shield = "BS", Chinese_Community = "CC",
                Health_Net = "HN", Kaiser = "KA", LA_Care = "LA", Molina = "MOL",
                Oscar = "OSC", Sharp = "SH", Valley = "VAL", Western = "WEST")

rs <- srrt %>%
  filter(rating_area != "Total", metal != "Total", metal != "Minimum Coverage",
         year >= 2014, year <= 2019,
         !is.na(risk_score), risk_score > 0, !is.na(member_months), member_months > 0) %>%
  mutate(region = as.integer(rating_area),
         insurer_prefix = unname(PREFIX_MAP[insurer])) %>%
  filter(!is.na(insurer_prefix), !is.na(region))
cat("  usable rows (2014-2019, region-level, positive score and member months):", nrow(rs), "\n")

plan_risk_scores <- rs %>%
  group_by(insurer_prefix, metal, region, year) %>%
  summarize(risk_score    = weighted.mean(risk_score, member_months),
            member_months = sum(member_months),
            n_types       = n(),
            .groups = "drop") %>%
  mutate(log_risk_score = log(risk_score))
cat("  insurer-metal-region-year rows:", nrow(plan_risk_scores), "\n")
print(plan_risk_scores %>% count(year, insurer_prefix) %>%
        tidyr::pivot_wider(names_from = year, values_from = n, values_fill = 0))

plan_risk_scores_year <- rs %>%
  group_by(insurer_prefix, metal, year) %>%
  summarize(risk_score    = weighted.mean(risk_score, member_months),
            member_months = sum(member_months),
            .groups = "drop") %>%
  mutate(log_risk_score = log(risk_score))
cat("  insurer-metal-year rows:", nrow(plan_risk_scores_year), "\n")

cat("  risk score by metal (member-month weighted):\n")
print(plan_risk_scores %>% group_by(metal) %>%
        summarize(mean_rs = weighted.mean(risk_score, member_months), n = n(), .groups = "drop"))

write_csv(plan_risk_scores, "data/output/plan_risk_scores.csv")
write_csv(plan_risk_scores_year, "data/output/plan_risk_scores_year.csv")
cat("  -> data/output/plan_risk_scores.csv, plan_risk_scores_year.csv\n")
