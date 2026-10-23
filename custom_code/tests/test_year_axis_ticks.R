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
  min(wide_breaks) == 1990 && max(wide_breaks) == 2020,
  "Wide year breaks should follow the regular spacing without forcing the final year."
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
expect_false(
  2023 %in% layout_axis$tickvals,
  "The Plotly year axis should not force a final-year label between regular ticks."
)

min_layout_axis <- plotly_year_xaxis_layout(
  list(title = "Year"),
  values = c(1800, 1850, 1900, 2023),
  n = 8,
  enabled = TRUE,
  axis_info = list(min = 1900)
)
expect_equal(
  min_layout_axis$range[[1]],
  1900,
  "Year Plotly x-axis with axis_info$min should start at the configured floor."
)
expect_equal(
  min_layout_axis$range[[2]],
  2023,
  "Year Plotly x-axis with axis_info$min should retain the data max."
)
expect_true(
  all(min_layout_axis$tickvals >= 1900),
  "Year Plotly x-axis ticks should not include values below axis_info$min."
)

numeric_min_axis <- plotly_number_axis_layout(
  list(title = "Year-like value"),
  values = c(1800, 1900, 2023),
  var_name = "year_value",
  label = "Year-like value",
  axis_info = list(min = 1900),
  n = 8,
  apply_axis_min = TRUE
)
expect_equal(
  numeric_min_axis$range[[1]],
  1900,
  "Numeric Plotly x-axis with axis_info$min should start at the configured floor."
)
expect_true(
  all(numeric_min_axis$tickvals >= 1900),
  "Numeric Plotly x-axis ticks should not include values below axis_info$min."
)

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

pp_min <- apply_plotly_year_xaxis_ticks(
  plotly::layout(
    plotly::plot_ly(
      data = data.frame(year = c(1800, 1900, 2023), value = c(1, 2, 3)),
      x = ~year,
      y = ~value
    ),
    xaxis = list(title = "Year")
  ),
  values = c(1800, 1900, 2023),
  n = 8,
  enabled = TRUE,
  axis_info = list(min = 1900)
)
expect_equal(
  pp_min$x$layout$xaxis$range[[1]],
  1900,
  "Plotly objects with axis_info$min should receive a year x-axis range floor."
)
expect_true(
  all(pp_min$x$layout$xaxis$tickvals >= 1900),
  "Plotly object year tick values should not include values below axis_info$min."
)

full_axis_info <- yaml::read_yaml("yaml/config_ineq_single.yaml")$axis_vars$x_axis
full_year_plot <- plotly::layout(
  plotly::plot_ly(
    data = data.frame(year = 1900:2023, value = seq_along(1900:2023)),
    x = ~year,
    y = ~value,
    type = "scatter",
    mode = "lines"
  ),
  xaxis = list(title = "Year")
)
full_year_plot <- apply_plotly_year_xaxis_ticks(
  full_year_plot,
  values = 1900:2023,
  n = full_axis_info$breaks,
  axis_info = full_axis_info
)
full_year_plot <- build_plotly_for_display(full_year_plot)
expect_true(
  full_year_plot$x$layout$xaxis$tickangle == 0 &&
    length(full_year_plot$x$layout$xaxis$tickvals) <= 5,
  "Full-size year charts should also use horizontal labels with at most five ticks."
)

preview_paths <- c(
  "yaml/config_ineq_prev.yaml",
  "yaml/config_inhe_prev.yaml",
  "yaml/config_topo_prev.yaml",
  "yaml/config_eigt-prev.yaml"
)
for (path in preview_paths) {
  preview_axis_info <- yaml::read_yaml(path)$axis_vars$x_axis
  expect_true(
    identical(preview_axis_info$var, "year") &&
      preview_axis_info$breaks == 5 &&
      preview_axis_info$max_ticks == 5 &&
      preview_axis_info$tick_angle == 0,
    paste(path, "should use the shared horizontal preview year-axis settings.")
  )

  preview_axis <- plotly_year_xaxis_layout(
    list(tickangle = -40),
    values = 1990:2023,
    n = preview_axis_info$breaks,
    axis_info = preview_axis_info
  )
  expect_true(
    preview_axis$tickangle == 0 &&
      length(preview_axis$tickvals) <= 5,
    paste(path, "should override rotation and limit the visible year ticks.")
  )
}

preview_data <- data.frame(year = 1990:2023, value = seq_along(1990:2023))
preview_axis_info <- yaml::read_yaml(preview_paths[[1]])$axis_vars$x_axis
preview_ggplot <- ggplot2::ggplot(preview_data, ggplot2::aes(year, value)) +
  ggplot2::geom_line() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
preview_plotly <- apply_plotly_year_xaxis_ticks(
  plotly::ggplotly(preview_ggplot),
  values = preview_data$year,
  n = preview_axis_info$breaks,
  axis_info = preview_axis_info
)
preview_plotly <- plotly::plotly_build(preview_plotly)
expect_true(
  preview_plotly$x$layout$xaxis$tickangle == 0 &&
    length(preview_plotly$x$layout$xaxis$tickvals) <= 5,
  "Preview settings should override ggplotly's diagonal year labels."
)

faceted_plotly <- plotly::subplot(
  plotly::plot_ly(preview_data, x = ~year, y = ~value,
                  type = "scatter", mode = "lines"),
  plotly::plot_ly(preview_data, x = ~year, y = ~value,
                  type = "scatter", mode = "lines"),
  nrows = 1
)
faceted_plotly <- apply_plotly_year_xaxis_ticks(
  faceted_plotly,
  values = preview_data$year,
  n = preview_axis_info$breaks,
  axis_info = preview_axis_info
)
faceted_xaxis_names <- plotly_axis_layout_names(faceted_plotly$x$layout, "x")
expect_true(
  length(faceted_xaxis_names) == 2 &&
  all(vapply(
    faceted_plotly$x$layout[faceted_xaxis_names],
    function(axis) axis$tickangle == 0 && length(axis$tickvals) <= 5,
    logical(1)
  )),
  "Preview settings should reach every faceted x-axis."
)

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
