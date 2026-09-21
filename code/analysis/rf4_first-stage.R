# Meta --------------------------------------------------------------------

## Description:   Broker-density instrument first-stage diagnostic. Reports the
##                instrument's OWN strength (partial F / t on n_agents,
##                incremental R^2) rather than the whole-model F build3 prints,
##                both unclustered and clustered on region (where n_agents
##                actually varies). This is the number behind the appendix's
##                "sensible but rests on too few markets" framing. Full insured
##                population (reads hh_full_prepped.csv from disk, like rf1);
##                writes results/first_stage_strength.csv. Assumes the preamble
##                is loaded (data.table + fixest).


cols <- c("assisted", "insured", "n_agents", "year",
          "perc_0to17", "perc_18to34", "perc_35to54", "perc_male",
          "perc_black", "perc_hispanic", "perc_asian", "perc_other",
          "FPL_250to400", "FPL_400plus", "household_size", "region")

d <- fread(file.path(TEMP_DIR, "hh_full_prepped.csv"), select = cols)
d <- d[insured == 1L]

# Unclustered OLS first stage, with and without the instrument
full <- lm(assisted ~ n_agents + perc_0to17 + perc_18to34 + perc_35to54 +
             perc_male + perc_black + perc_hispanic + perc_asian + perc_other +
             FPL_250to400 + FPL_400plus + household_size + factor(year),
           data = d)
drop <- lm(assisted ~ perc_0to17 + perc_18to34 + perc_35to54 +
             perc_male + perc_black + perc_hispanic + perc_asian + perc_other +
             FPL_250to400 + FPL_400plus + household_size + factor(year),
           data = d)
s <- summary(full)
whole_F     <- unname(s$fstatistic[1])
b_unc       <- coef(s)["n_agents", "Estimate"]
se_unc      <- coef(s)["n_agents", "Std. Error"]
t_unc       <- coef(s)["n_agents", "t value"]
r2_with     <- s$r.squared
r2_without  <- summary(drop)$r.squared


# Clustered on region, since n_agents varies only at region-year
fs_cl <- feols(assisted ~ n_agents + perc_0to17 + perc_18to34 + perc_35to54 +
                 perc_male + perc_black + perc_hispanic + perc_asian + perc_other +
                 FPL_250to400 + FPL_400plus + household_size | year,
               data = d, cluster = ~region)
b_cl  <- coeftable(fs_cl)["n_agents", "Estimate"]
se_cl <- coeftable(fs_cl)["n_agents", "Std. Error"]
t_cl  <- coeftable(fs_cl)["n_agents", "t value"]

# Region-year variation in the instrument itself
cells <- unique(d[, .(region, year, n_agents)])

# Traceable output for the appendix
out <- data.frame(
  statistic = c("whole_model_F", "coef", "se_unclustered", "t_unclustered",
                "partialF_unclustered", "se_region_clustered", "t_region_clustered",
                "partialF_region_clustered", "incremental_R2", "region_year_cells",
                "n_agents_sd_across_cells"),
  value = c(whole_F, b_unc, se_unc, t_unc, t_unc^2, se_cl, t_cl, t_cl^2,
            r2_with - r2_without, nrow(cells), sd(cells$n_agents, na.rm = TRUE))
)
write.csv(out, "results/first_stage_strength.csv", row.names = FALSE)
