# Meta --------------------------------------------------------------------
#
## Author:        Ian McCarthy
## Description:   Channel use and the plan menu. Share of enrolled households
##                using an agent and summary measures of the menu (carriers,
##                plans, spread in silver premiums, mean commission) by
##                region-year and by region, and the correlations across
##                regions between agent use and each menu measure.
##
##                Sourced by _analysis.R after build3_data-prep.R.

hh <- fread("data/output/hh_full.csv",
            select = c("region", "year", "insured", "channel_detail"))
hh <- hh[insured == 1L]

cells <- hh %>%
  as_tibble() %>%
  group_by(region, year) %>%
  summarize(n_hh = n(),
            agent = mean(channel_detail == "Agent"),
            .groups = "drop")
rm(hh)

# Menu measures over standard plans (CSR silver variants excluded). The silver
# spread is the highest minus the lowest silver premium, in dollars per month;
# commissions are per member per month, averaged over the plans offered
plans <- fread(file.path(TEMP_DIR, "plan_choice.csv"),
               select = c("region", "year", "metal", "issuer", "premium", "comm_pmpm")) %>%
  as_tibble() %>%
  filter(!str_detect(metal, "Enhanced"))

menu <- plans %>%
  group_by(region, year) %>%
  summarize(n_plans = n(),
            n_carriers = n_distinct(issuer),
            silver_spread = max(premium[metal == "Silver"]) - min(premium[metal == "Silver"]),
            comm_plan = mean(comm_pmpm),
            .groups = "drop")
rm(plans)

channel_cells <- cells %>%
  inner_join(menu, by = c("region", "year"), relationship = "one-to-one")
rm(cells, menu)
write_csv(channel_cells, "results/channel_menu_cells.csv")

channel_region <- channel_cells %>%
  group_by(region) %>%
  summarize(agent = weighted.mean(agent, n_hh),
            n_carriers = mean(n_carriers),
            n_plans = mean(n_plans),
            silver_spread = mean(silver_spread),
            comm_plan = mean(comm_plan),
            n_hh = sum(n_hh),
            .groups = "drop")
write_csv(channel_region, "results/channel_menu_region.csv")

channel_cor <- channel_region %>%
  summarize(n_carriers = cor(agent, n_carriers),
            n_plans = cor(agent, n_plans),
            silver_spread = cor(agent, silver_spread),
            comm_plan = cor(agent, comm_plan)) %>%
  pivot_longer(everything(), names_to = "measure", values_to = "cor_agent")
write_csv(channel_cor, "results/channel_menu_cor.csv")
rm(channel_cells, channel_region, channel_cor)
