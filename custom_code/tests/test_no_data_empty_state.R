#!/usr/bin/env Rscript

source("modules/PlotServer.R")

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

expect_false <- function(value, message) {
  if (isTRUE(value)) fail(message)
}

expect_equal <- function(actual, expected, message) {
  if (!identical(actual, expected)) {
    fail(paste0(
      message,
      "\nExpected: ", paste(expected, collapse = ", "),
      "\nActual: ", paste(actual, collapse = ", ")
    ))
  }
}

expect_false(
  has_plot_rows(NULL),
  "NULL data should not be treated as plottable."
)

expect_false(
  has_plot_rows(data.frame(x = numeric(), y = numeric())),
  "Zero-row data should not be treated as plottable."
)

expect_true(
  has_plot_rows(data.frame(x = 1, y = 2)),
  "Data with at least one row should be treated as plottable."
)

all_na <- data.frame(
  x = c(1, 2),
  y = c(NA_real_, NA_real_),
  group = c("A", "B")
)
expect_false(
  has_drawable_values(all_na, required_vars = c("x", "group"), value_vars = "y"),
  "Rows with only missing plotted values should not be drawable."
)

zero_only <- data.frame(
  x = c("A", "B"),
  y = c(0, 0),
  group = c("A", "B")
)
expect_true(
  has_drawable_values(zero_only, required_vars = c("x", "group"), value_vars = "y"),
  "Zero values should count as drawable unless non-zero values are required."
)
expect_false(
  has_drawable_values(
    zero_only,
    required_vars = c("x", "group"),
    value_vars = "y",
    require_nonzero = TRUE
  ),
  "Zero-only rows should not be drawable for bar-style non-zero requirements."
)

nonzero <- data.frame(
  x = c("A", "B"),
  y = c(0, 3),
  group = c("A", "B")
)
expect_true(
  has_drawable_values(
    nonzero,
    required_vars = c("x", "group"),
    value_vars = "y",
    require_nonzero = TRUE
  ),
  "At least one non-zero value should be drawable when non-zero values are required."
)

empty_plot <- plotly::plotly_build(no_data_plotly("Nothing to show", height = 321))
expect_equal(
  empty_plot$x$layout$annotations[[1]]$text,
  "Nothing to show",
  "The empty-state plot should carry the configured annotation text."
)
expect_equal(
  empty_plot$x$layout$height,
  321,
  "The empty-state plot should preserve the requested height."
)
expect_false(
  empty_plot$x$config$displayModeBar,
  "The empty-state plot should hide the Plotly modebar."
)

cat("No-data empty-state checks passed.\n")
