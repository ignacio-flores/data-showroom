#!/usr/bin/env Rscript

source("modules/PlotServer.R")

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
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

expect_no_scientific <- function(values, message) {
  expect_true(
    !any(grepl("e[+-]?[0-9]+", values, ignore.case = TRUE)),
    message
  )
}

expect_equal(
  format_axis_number(1250, spec = list(format = "comma", digits = 0L)),
  "1,250",
  "Comma formatting should preserve readable thousands."
)

expect_equal(
  format_axis_number(
    c(42000, 3500000, 8e9, 12e12),
    spec = list(format = "compact")
  ),
  c("42k", "3.5M", "8B", "12T"),
  "Compact formatting should use k/M/B/T suffixes."
)

expect_equal(
  format_axis_number(-1500000, spec = list(format = "compact")),
  "-1.5M",
  "Compact formatting should preserve negative signs."
)

expect_equal(
  format_axis_number(c(0, 0.25, 12.5), spec = list(format = "compact")),
  c("0", "0.25", "12.5"),
  "Compact formatting should leave small values as readable decimals."
)

rate_spec <- axis_number_format_spec(
  var_name = "top_marginal_rate",
  label = "Top marginal rate"
)
expect_equal(
  format_axis_number(55, spec = rate_spec),
  "55%",
  "Rate variables should infer percent-style axis labels."
)

percent_label_spec <- axis_number_format_spec(
  var_name = "share_tax",
  label = "% of total tax revenue"
)
expect_equal(
  format_axis_number(12.5, spec = percent_label_spec),
  "12.5%",
  "Percent labels should infer percent-style axis labels."
)

override_spec <- axis_number_format_spec(
  var_name = "total_revenue_from_tax",
  label = "Total revenue",
  axis_info = list(
    number_format = "comma",
    number_formats = list(total_revenue_from_tax = "compact"),
    number_format_digits = list(total_revenue_from_tax = 2)
  )
)
expect_equal(
  format_axis_number(1250000, spec = override_spec),
  "1.25M",
  "Per-variable axis format overrides should win over axis defaults."
)

expect_equal(
  format_axis_number(2500000, spec = list(format = "compact_long")),
  "2.5 million",
  "Long compact formatting should use readable word suffixes."
)

huge_label <- format_axis_number(7.665504e16, spec = list(format = "compact"))
expect_no_scientific(
  huge_label,
  "Very large values should not be displayed in scientific notation."
)
expect_true(
  grepl("T$", huge_label),
  "Very large compact values should use the trillion suffix when needed."
)

plain_label <- format_axis_number(1234567, spec = list(format = "plain", digits = 0L))
expect_equal(
  plain_label,
  "1234567",
  "Plain formatting should avoid commas while still avoiding scientific notation."
)

axis_layout <- plotly_number_axis_layout(
  list(title = "Revenue"),
  values = c(0, 1e6, 2e6),
  var_name = "revenue",
  label = "Revenue"
)
expect_equal(
  axis_layout$tickmode,
  "array",
  "Numeric Plotly axes should use explicit tick arrays."
)
expect_true(
  length(axis_layout$tickvals) == length(axis_layout$ticktext),
  "Numeric Plotly axes should keep tick values and text aligned."
)
expect_no_scientific(
  axis_layout$ticktext,
  "Numeric Plotly axis tick labels should avoid scientific notation."
)
expect_true(
  any(grepl("M$", axis_layout$ticktext)),
  "Numeric Plotly axis tick labels should use compact suffixes."
)

dual_axis_plot <- plotly::plot_ly(
  data = data.frame(year = c(2020, 2021), left = c(1e6, 2e6), right = c(10, 55)),
  x = ~year,
  y = ~left,
  type = "scatter",
  mode = "lines"
) %>%
  plotly::add_trace(y = ~right, yaxis = "y2", mode = "lines") %>%
  plotly::layout(
    yaxis = plotly_number_axis_layout(
      list(title = "Revenue"),
      values = c(1e6, 2e6),
      var_name = "revenue",
      label = "Revenue"
    ),
    yaxis2 = plotly_number_axis_layout(
      list(title = "Tax rate", overlaying = "y", side = "right"),
      values = c(10, 55),
      var_name = "top_marginal_rate",
      label = "Tax rate"
    )
  )
dual_axis_plot <- plotly::plotly_build(dual_axis_plot)
expect_no_scientific(
  dual_axis_plot$x$layout$yaxis$ticktext,
  "Dual-axis primary y labels should avoid scientific notation."
)
expect_true(
  any(grepl("M$", dual_axis_plot$x$layout$yaxis$ticktext)),
  "Dual-axis primary y labels should use compact suffixes."
)
expect_true(
  all(grepl("%$", dual_axis_plot$x$layout$yaxis2$ticktext)),
  "Dual-axis secondary rate labels should use percent suffixes."
)

hbar_plot <- plotly::plot_ly(
  data = data.frame(country = c("A", "B"), value = c(1000, 2500000)),
  x = ~value,
  y = ~country,
  type = "bar",
  orientation = "h"
) %>%
  plotly::layout(
    xaxis = plotly_number_axis_layout(
      list(title = "Value", rangemode = "tozero"),
      values = c(1000, 2500000),
      var_name = "value",
      label = "Value"
    )
  )
hbar_plot <- plotly::plotly_build(hbar_plot)
expect_no_scientific(
  hbar_plot$x$layout$xaxis$ticktext,
  "Horizontal bar numeric x-axis labels should avoid scientific notation."
)
expect_true(
  any(grepl("M$", hbar_plot$x$layout$xaxis$ticktext)),
  "Horizontal bar numeric x-axis labels should use compact suffixes."
)

colorbar <- plotly_colorbar_style(
  "Revenue",
  values = c(0, 1e6, 2e6),
  var_name = "revenue",
  label = "Revenue"
)
expect_equal(
  colorbar$tickmode,
  "array",
  "Map colorbars should use explicit formatted tick arrays."
)
expect_no_scientific(
  colorbar$ticktext,
  "Map colorbar tick labels should avoid scientific notation."
)

cat("Axis number-format checks passed.\n")
