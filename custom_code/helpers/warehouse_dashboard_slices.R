warehouse_dashboard_slice_spec <- function() {
  data.frame(
    output = c(
      "inhe_warehouse_meta_v2.csv",
      "ineq_warehouse_meta_v2.csv",
      "taxw_warehouse_meta_v2.csv",
      "topo_warehouse_meta_v2.csv"
    ),
    dashboard = c("i", "t", "x", "p"),
    dashboard_label = c(
      "Inheritance Trends",
      "Wealth Inequality",
      "Taxes on Wealth",
      "Wealth Topography"
    ),
    stringsAsFactors = FALSE
  )
}

validate_warehouse_dashboard_slices <- function(data, spec = warehouse_dashboard_slice_spec()) {
  required_columns <- c("d1_dashboard", "d1_dashboard_lab")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns)) {
    stop(
      "Canonical warehouse is missing dashboard columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  expected_labels <- stats::setNames(spec$dashboard_label, spec$dashboard)
  observed_labels <- unname(expected_labels[as.character(data$d1_dashboard)])
  invalid_rows <- is.na(observed_labels) |
    as.character(data$d1_dashboard_lab) != observed_labels

  if (any(invalid_rows)) {
    invalid_values <- unique(data[invalid_rows, ..required_columns])
    stop(
      "Canonical warehouse contains unknown or mismatched dashboard values: ",
      paste(utils::capture.output(print(invalid_values)), collapse = " ")
    )
  }

  missing_dashboards <- setdiff(spec$dashboard, unique(as.character(data$d1_dashboard)))
  if (length(missing_dashboards)) {
    stop(
      "Canonical warehouse is missing dashboard slice(s): ",
      paste(missing_dashboards, collapse = ", ")
    )
  }
}

write_warehouse_dashboard_slices <- function(input_file,
                                              output_dir = "data",
                                              spec = warehouse_dashboard_slice_spec()) {
  if (!file.exists(input_file)) {
    stop("Missing canonical warehouse file: ", input_file)
  }

  data <- data.table::fread(input_file, showProgress = FALSE)
  validate_warehouse_dashboard_slices(data, spec)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  for (idx in seq_len(nrow(spec))) {
    slice <- data[as.character(data$d1_dashboard) == spec$dashboard[[idx]]]
    output_file <- file.path(output_dir, spec$output[[idx]])
    data.table::fwrite(slice, output_file)
    message("Saved ", nrow(slice), " rows to ", output_file)
  }

  invisible(file.path(output_dir, spec$output))
}
