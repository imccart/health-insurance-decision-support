# Meta --------------------------------------------------------------------

## Description:   Shared structural inputs, sourced by both _demand.R and
##                _supply.R. Reads the 0_data-prep outputs from disk and builds
##                the cell index and per-cell seeds. Generating cell_seeds here
##                (once, from MASTER_SEED over the same cells) guarantees demand
##                and supply draw identical SAMPLE_FRAC subsamples per cell.

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
