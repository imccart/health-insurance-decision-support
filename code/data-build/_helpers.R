# Shared helpers for the data-build pipeline.
# Kept minimal: only things that are actually reused across scripts.

# Age-40 rating factor: CA plans are priced at age 21, then multiplied by
# age-specific factor. Factor at age 40 is 1.278. Used to convert plan_data
# Premium (age-40) to age-21 base (premium21).
RATING_FACTOR_AGE40 <- 1.278

# Insurer name standardization ---------------------------------------------
# Canonical mapping from raw CC/plan_data names to short codes.
INSURER_MAP <- c(
  "Anthem Blue Cross"        = "Anthem",
  "Blue Shield"              = "Blue_Shield",
  "Chinese Community"        = "Chinese_Community",
  "Contra Costa Heal"        = "Contra_Costa",
  "Health Net"               = "Health_Net",
  "LA Care"                  = "LA_Care",
  "Molina Health Car"        = "Molina",
  "Oscar Health Plan"        = "Oscar",
  "SHARP Health Plan"        = "SHARP",
  "UnitedHealthcare"         = "United",
  "Valley Health"            = "Valley",
  "Western Health"           = "Western",
  "Contra Costa Health Plan" = "Contra_Costa",
  "Sharp"                    = "SHARP"
)

standardize_insurer <- function(x) {
  ifelse(x %in% names(INSURER_MAP), INSURER_MAP[x], x)
}

# ACA contribution (monthly $) required of a HH toward second-lowest silver
# Inputs: FPL in ratio form (1.5 = 150% of poverty), NOT percentage
#         perc_LB/UB: contribution pct at bracket bounds
#         fpl_LB/UB: bracket bounds (ratio form)
#         poverty_threshold: annual $ for the HH size
aca_contribution <- function(fpl, perc_LB, perc_UB, fpl_LB, fpl_UB,
                              poverty_threshold, bracket_300_400 = FALSE) {
  contribution <- rep(NA_real_, length(fpl))
  # 300-400% bracket: flat percentage, linear in income
  lin <- bracket_300_400
  contribution[lin] <- perc_LB[lin] * (poverty_threshold[lin] / 12 * fpl[lin])
  # All other brackets: contribution pct interpolates linearly across FPL,
  # yielding quadratic contribution in FPL
  quad <- !bracket_300_400
  contribution[quad] <- ((perc_UB[quad] - perc_LB[quad]) *
    (fpl[quad] - fpl_LB[quad]) / (fpl_UB[quad] - fpl_LB[quad]) + perc_LB[quad]) *
    (poverty_threshold[quad] / 12 * fpl[quad])
  contribution
}

# Reference tables ---------------------------------------------------------
# Bracket name → FPL lower/upper bounds (ratio form).
# Matches the CC enrollment data's subsidy_linear_piece variable.
FPL_BRACKETS <- tibble::tribble(
  ~bracket,                 ~fpl_LB, ~fpl_UB,
  "138% FPL or less",        0,       1.38,
  "138% FPL to 150% FPL",    1.38,    1.5,
  "150% FPL to 200% FPL",    1.5,     2.0,
  "200% FPL to 250% FPL",    2.0,     2.5,
  "250% FPL to 300% FPL",    2.5,     3.0,
  "300% FPL to 400% FPL",    3.0,     4.0,
  "400% FPL or greater",     4.0,     Inf
)

# Derive bracket name from exact FPL (ratio form).
assign_bracket <- function(fpl) {
  dplyr::case_when(
    is.na(fpl)      ~ NA_character_,
    fpl <= 1.38     ~ "138% FPL or less",
    fpl <= 1.5      ~ "138% FPL to 150% FPL",
    fpl <= 2.0      ~ "150% FPL to 200% FPL",
    fpl <= 2.5      ~ "200% FPL to 250% FPL",
    fpl <= 3.0      ~ "250% FPL to 300% FPL",
    fpl <= 4.0      ~ "300% FPL to 400% FPL",
    TRUE            ~ "400% FPL or greater"
  )
}

# Parse SAS dictionary + read fixed-width data (for SIPP / ACS / IPUMS).
read_fwf_sas <- function(sas_path, data_path, is_gzip = FALSE, is_zip = FALSE) {
  input <- parse.SAScii(sas_path)
  col_positions <- fwf_widths(input$width, col_names = input$varname)
  if (is_zip) {
    tmp <- tempdir(); extracted <- unzip(data_path, exdir = tmp)
    df <- read_fwf(extracted[1], col_positions, col_types = cols(.default = "d"))
    file.remove(extracted); df
  } else {
    read_fwf(data_path, col_positions, col_types = cols(.default = "d"))
  }
}
