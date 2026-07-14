# Meta --------------------------------------------------------------------

## Description:   Head of the structural block: builds the cell index and
##                per-cell seeds. Sourced by the driver before s2_demand, and
##                standalone by cf3_se. Reads the build3_data-prep outputs from
##                disk. Generating cell_seeds here (once, from MASTER_SEED over
##                the same cells) guarantees demand, pricing, and the CF draw
##                identical SAMPLE_FRAC subsamples per cell.

cat("Loading shared structural data...\n")
hh_all   <- fread(file.path(TEMP_DIR, "hh_choice.csv"))
hh_split <- split(hh_all, by = c("region", "year"))
cells    <- unique(hh_all[, .(region, year)])[order(region, year)]
rm(hh_all); gc(verbose = FALSE)

plan_choice       <- fread(file.path(TEMP_DIR, "plan_choice.csv")) %>% as_tibble()
commission_lookup <- fread("data/output/commission_lookup.csv")    %>% as_tibble()

set.seed(MASTER_SEED)
cell_seeds <- sample.int(1e7, nrow(cells))

cat("  Cells:", nrow(cells), "\n\n")
