# cell_matrices.R — in-memory adaptation of load_one_cell() (kernels_demand.R)
#
# The joint estimator builds each cell once via build_structural() and keeps the
# cell_data tibble in memory; the demand score (cell_negll_gradi) needs the dense
# X-matrix view. This is load_one_cell()'s exact body with the CSV read replaced
# by `as.data.table(cell_data)`. No other logic changed — same valid-HH filter,
# same X_ins / X_0 / X_ch / hh_id construction.

cell_data_to_matrices <- function(cell_data, covars, filter_assisted = -1L) {
  df <- as.data.table(cell_data)

  if (filter_assisted >= 0) {
    df <- df[assisted == filter_assisted]
    if (nrow(df) == 0) return(NULL)
  }

  setorder(df, household_number, plan_id)

  K <- length(covars)
  n_rows <- nrow(df)

  # Build full X matrix; missing covariates filled with 0 (matches load_one_cell)
  X_full <- matrix(0, nrow = n_rows, ncol = K)
  for (k in seq_along(covars)) {
    col_name <- covars[k]
    if (col_name %in% names(df)) {
      vals <- df[[col_name]]
      vals[is.na(vals)] <- 0
      X_full[, k] <- as.numeric(vals)
    }
  }

  plan_nm <- as.character(df$plan_id)
  hh_num  <- df$household_number
  is_unins <- plan_nm == "Uninsured"
  is_ins   <- !is_unins

  # Identify valid HH: must have insured rows, an uninsured row, and a chosen row
  df[, row_idx := .I]
  df[, is_unins := (plan_id == "Uninsured")]
  hh_summary <- df[, .(
    has_ins       = any(!is_unins),
    has_unins     = any(is_unins),
    has_choice    = any(choice == 1L),
    unins_idx     = row_idx[is_unins][1],
    chosen_idx    = row_idx[choice == 1L][1],
    chose_insured = (plan_id[choice == 1L][1] != "Uninsured"),
    weight        = hh_weight[1]
  ), by = household_number]

  valid_hh <- hh_summary[has_ins == TRUE & has_unins == TRUE & has_choice == TRUE]
  n_hh <- nrow(valid_hh)
  if (n_hh == 0) return(NULL)

  valid_hh[, hh_idx := .I]
  valid_hh_set <- valid_hh$household_number

  ins_mask <- is_ins & (hh_num %in% valid_hh_set)
  X_ins <- X_full[ins_mask, , drop = FALSE]

  ins_hh_nums <- hh_num[ins_mask]
  hh_lookup <- valid_hh$hh_idx
  names(hh_lookup) <- as.character(valid_hh$household_number)
  hh_id <- as.integer(hh_lookup[as.character(ins_hh_nums)])

  X_0  <- X_full[valid_hh$unins_idx, , drop = FALSE]
  X_ch <- X_full[valid_hh$chosen_idx, , drop = FALSE]

  list(
    X_ins     = X_ins,
    X_0       = X_0,
    X_ch      = X_ch,
    hh_id     = hh_id,
    n_hh      = n_hh,
    chose_ins = valid_hh$chose_insured,
    wt        = valid_hh$weight
  )
}
