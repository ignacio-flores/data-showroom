if (!exists("color_var")) color_var <- NULL

# Define new columns if necessary
if (!is.null(new.cols)) {
  new_col_groups <- lapply(new.cols, function(x) x)
  new_col_names <- names(new.cols)
  all_varlists <- list(
    new_col_groups,
    axis_vars$x_axis$var, axis_vars$y_axis$var,
    names(fixed_selectors),
    names(dt.cols),
    names(tooltip_vars),
    "source"
  )
  all_vars <- setdiff(unique(unlist(all_varlists)), new_col_names)
} else {
  if (!is.null(tooltip_vars)) {
    all_varlists <- list(
      axis_vars$x_axis$var, axis_vars$y_axis$var,
      names(fixed_selectors),
      names(dt.cols),
      names(tooltip_vars),
      color_var, "source"
    )
    all_vars <- unique(unlist(all_varlists))
  } else {
    all_varlists <- list(
      axis_vars$x_axis$var, axis_vars$y_axis$var,
      names(fixed_selectors),
      names(dt.cols),
      color_var,
      "source")
    all_vars <- unique(unlist(all_varlists))
  }
}