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

expect_integer_ticks <- function(values, message) {
  expect_true(length(values) > 0, paste(message, "should not be empty"))
  expect_true(
    all(is.finite(values) & values == round(values)),
    paste(message, "should contain only whole-number tick values")
  )
}

expect_decimal_free_text <- function(values, message) {
  expect_true(length(values) > 0, paste(message, "should not be empty"))
  expect_false(
    any(grepl("\\.", values)),
    paste(message, "should not contain decimal labels")
  )
}

expect_true(
  is_year_axis_var("year"),
  "Lowercase year should be treated as a year axis."
)
expect_true(
  is_year_axis_var("Year"),
  "Capitalized Year should be treated as a year axis."
)
expect_false(
  is_year_axis_var("fiscal_year"),
  "Similar non-exact names should not be treated as year axes."
)
expect_false(
  is_year_axis_var("year_label"),
  "Year-prefixed names should not be treated as year axes."
)

short_breaks <- year_axis_breaks(c(2019, 2021), n = 8)
expect_equal(
  short_breaks,
  2019:2021,
  "Short year ranges should use each integer year."
)
expect_integer_ticks(short_breaks, "Short year range breaks")
expect_decimal_free_text(year_axis_tick_text(short_breaks), "Short year range labels")

factor_breaks <- year_axis_breaks(factor(c("2019", "2021")), n = 8)
expect_equal(
  factor_breaks,
  2019:2021,
  "Factor year values should be interpreted through their labels, not factor codes."
)

wide_breaks <- year_axis_breaks(c(1990, 2023), n = 8)
expect_integer_ticks(wide_breaks, "Wide year range breaks")
expect_decimal_free_text(year_axis_tick_text(wide_breaks), "Wide year range labels")
expect_true(
  min(wide_breaks) == 1990 && max(wide_breaks) == 2023,
  "Wide year breaks should retain the integer endpoints."
)

layout_axis <- plotly_year_xaxis_layout(
  list(title = "Year"),
  values = c(1990, 2023),
  n = 8,
  enabled = TRUE
)
expect_equal(
  layout_axis$tickmode,
  "array",
  "Year Plotly x-axis should use explicit array ticks."
)
expect_integer_ticks(layout_axis$tickvals, "Year Plotly x-axis tick values")
expect_decimal_free_text(layout_axis$ticktext, "Year Plotly x-axis tick labels")

non_year_axis <- plotly_year_xaxis_layout(
  list(title = "Value"),
  values = c(1.2, 2.8),
  n = 4,
  enabled = FALSE
)
expect_true(
  is.null(non_year_axis$tickmode),
  "Non-year axes should not be forced to explicit year tick arrays."
)

pp <- plotly::layout(
  plotly::plot_ly(
    data = data.frame(year = c(1990, 2023), value = c(1, 2)),
    x = ~year,
    y = ~value
  ),
  xaxis = list(title = "Year")
)
pp <- apply_plotly_year_xaxis_ticks(
  pp,
  values = c(1990, 2023),
  n = 8,
  enabled = TRUE
)
expect_equal(
  pp$x$layout$xaxis$tickmode,
  "array",
  "Plotly objects should receive explicit year x-axis ticks."
)
expect_integer_ticks(pp$x$layout$xaxis$tickvals, "Plotly object year tick values")
expect_decimal_free_text(pp$x$layout$xaxis$ticktext, "Plotly object year tick labels")

expect_equal(
  format_year_axis_value(2020),
  "2020",
  "Tooltip year values should be formatted as whole years."
)
expect_equal(
  format_year_axis_value("not a year"),
  "not a year",
  "Non-numeric tooltip values should be preserved."
)

cat("OK: year x-axis ticks and labels remain integer-only.\n")
