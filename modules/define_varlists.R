if (!exists("color_var")) color_var <- NULL

parse_axis_values <- function(values) {
  if (is.null(values)) return(NULL)
  if (is.character(values) && length(values) == 1 && grepl("^c\\(", values)) {
    parsed <- tryCatch(eval(parse(text = values)), error = function(e) values)
    return(parsed)
  }
  values
}

extract_axis_columns <- function(axis_info) {
  if (is.null(axis_info)) return(NULL)
  unique(c(axis_info$var, parse_axis_values(axis_info$choices)))
}

axis_cols <- unique(c(
  extract_axis_columns(axis_vars$x_axis),
  extract_axis_columns(axis_vars$y_axis),
  extract_axis_columns(axis_vars$y2_axis)
))

# Define new columns if necessary
if (!is.null(new.cols)) {
  new_col_groups <- lapply(new.cols, function(x) x)
  new_col_names <- names(new.cols)
  all_varlists <- list(
    new_col_groups,
    axis_cols,
    names(fixed_selectors),
    names(dt.cols),
    names(tooltip_vars),
    "source"
  )
  all_vars <- setdiff(unique(unlist(all_varlists)), new_col_names)
} else {
  if (!is.null(tooltip_vars)) {
    all_varlists <- list(
      axis_cols,
      names(fixed_selectors),
      names(dt.cols),
      names(tooltip_vars),
      color_var, "source"
    )
    all_vars <- unique(unlist(all_varlists))
  } else {
    all_varlists <- list(
      axis_cols,
      names(fixed_selectors),
      names(dt.cols),
      color_var,
      "source")
    all_vars <- unique(unlist(all_varlists))
  }
}
