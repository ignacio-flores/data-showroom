#!/usr/bin/env Rscript

source("modules/PlotServer.R")
us2_config <- yaml::read_yaml("yaml/config_eigt_us2.yaml")
us3_config <- yaml::read_yaml("yaml/config_eigt_us3.yaml")

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

expect_numeric_equal <- function(actual, expected, message, tolerance = 1e-9) {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    fail(paste0(
      message,
      "\nExpected: ", paste(expected, collapse = ", "),
      "\nActual: ", paste(actual, collapse = ", ")
    ))
  }
}

df <- data.frame(
  lower_bound = c(0, 1, 0, 1),
  marginal_rate = c(10, 20, 10, 20),
  series = c("A", "A", "B", "B"),
  tooltip_text = c(
    "Marginal tax rate: 10.00",
    "Marginal tax rate: 20.00",
    "Marginal tax rate: 10.00",
    "Marginal tax rate: 20.00"
  ),
  stringsAsFactors = FALSE
)

omitted <- apply_overlap_offset_to_data(
  df,
  y_var = "marginal_rate",
  groupvars = "series",
  color_var = "series",
  overlap_offset = NULL,
  gopts = "step"
)
expect_true(!omitted$enabled, "Omitted overlap_offset config should be disabled.")
expect_equal(omitted$y_var, "marginal_rate", "Disabled offsets should keep the original y variable.")
expect_numeric_equal(
  omitted$data[[omitted$y_var]],
  df$marginal_rate,
  "Omitted overlap_offset config should leave plotted values unchanged."
)

disabled <- apply_overlap_offset_to_data(
  df,
  y_var = "marginal_rate",
  groupvars = "series",
  color_var = "series",
  overlap_offset = list(enabled = FALSE, amount = 0.1),
  gopts = "step"
)
expect_true(!disabled$enabled, "Explicitly disabled overlap_offset config should be disabled.")
expect_numeric_equal(
  disabled$data[[disabled$y_var]],
  df$marginal_rate,
  "Explicitly disabled overlap_offset config should leave plotted values unchanged."
)

enabled <- apply_overlap_offset_to_data(
  df,
  y_var = "marginal_rate",
  groupvars = "series",
  color_var = "series",
  overlap_offset = list(enabled = TRUE, amount = 0.1),
  gopts = "step"
)
expect_true(enabled$enabled, "Enabled overlap_offset config should create a visual y variable.")
expect_true(
  !identical(enabled$y_var, "marginal_rate"),
  "Enabled overlap_offset config should not overwrite the source y variable."
)
expect_numeric_equal(
  enabled$data$marginal_rate,
  df$marginal_rate,
  "Enabled overlap_offset config should leave source values unchanged."
)
expect_equal(
  enabled$data$tooltip_text,
  df$tooltip_text,
  "Enabled overlap_offset config should leave tooltip text unchanged."
)
expect_numeric_equal(
  enabled$data[[enabled$y_var]],
  c(9.5, 19.5, 10.5, 20.5),
  "Enabled overlap_offset config should fan out groups symmetrically around true values."
)

expect_equal(
  us2_config$overlap_offset,
  us3_config$overlap_offset,
  "US trend lines should use the same offset settings as full tax schedules."
)
trend_offset <- apply_overlap_offset_to_data(
  df,
  y_var = "marginal_rate",
  groupvars = "series",
  color_var = "series",
  overlap_offset = us2_config$overlap_offset,
  gopts = us2_config$gopts
)
expect_true(trend_offset$enabled, "US trend lines should separate overlapping states.")
expect_true(
  trend_offset$data[[trend_offset$y_var]][[1]] !=
    trend_offset$data[[trend_offset$y_var]][[3]],
  "Equal values in distinct trend series should plot at slightly different heights."
)
expect_equal(
  trend_offset$data$tooltip_text,
  df$tooltip_text,
  "US trend line offsets should preserve true hover values."
)
expect_numeric_equal(
  trend_offset$data$marginal_rate,
  df$marginal_rate,
  "US trend line offsets should preserve source values for tables and downloads."
)

zero_schedule <- data.frame(
  adjlbo = rep(c(0, 1), 2),
  adjmrt = rep(0, 4),
  state_name = rep(c("A", "B"), each = 2),
  stringsAsFactors = FALSE
)
render_schedule <- function(schedule) {
  rendered <- NULL
  shiny::testServer(
    plotModuleServer,
    args = list(
      filtered_data_func = shiny::reactive(schedule),
      x_var = "adjlbo", x_var_lab = "Bracket lower bound",
      y_var = "adjmrt", y_var_lab = "Marginal tax rate",
      color_var = "state_name", color_var_lab = "State",
      facet_var = NULL, facet_var_lab = NULL,
      tooltip_vars = list(state_name = "State:", adjmrt = "Rate:"),
      hide.legend = FALSE, gopts = "step", xnum_breaks = 10,
      extra_layer = NULL, color_style = "dichromat::DarkRedtoBlue_12",
      plot_height = 400, groupvars = "state_name",
      overlap_offset = us3_config$overlap_offset,
      y_axis_info = us3_config$axis_vars$y_axis
    ),
    {
      rendered <<- jsonlite::fromJSON(output$valuePlot, simplifyVector = FALSE)
    }
  )
  rendered
}

zero_plot <- render_schedule(zero_schedule)
expect_numeric_equal(
  unlist(zero_plot$x$layout$yaxis$range),
  c(-0.5, 10),
  "Zero-rate schedules should show a regular percent axis with room below zero."
)
expect_numeric_equal(
  unlist(zero_plot$x$layout$yaxis$tickvals),
  c(0, 5, 10),
  "Zero-rate schedules should not show tiny offset values as axis ticks."
)
expect_true(
  any(vapply(zero_plot$x$data, function(trace) {
    any(abs(as.numeric(unlist(trace$x)) - 1.1) < 1e-9)
  }, logical(1))),
  "The zero-rate lines should still extend horizontally to the right."
)
zero_plotted_y <- as.numeric(unlist(lapply(zero_plot$x$data, `[[`, "y")))
zero_plotted_y <- zero_plotted_y[is.finite(zero_plotted_y)]
expect_true(
  length(unique(zero_plotted_y)) > 1 && max(abs(zero_plotted_y)) < 0.1,
  "Zero-rate lines should retain their slight visual spacing near zero."
)

mixed_plot <- render_schedule(transform(
  zero_schedule, adjmrt = c(0, 40, 0, 40)
))
expect_true(
  max(as.numeric(unlist(mixed_plot$x$layout$yaxis$range))) >= 40,
  "Schedules with positive rates should keep their existing automatic axis."
)
expect_true(
  any(vapply(mixed_plot$x$data, function(trace) {
    any(abs(as.numeric(unlist(trace$x)) - 1.1) < 1e-9)
  }, logical(1))),
  "The narrower axis fix should preserve the horizontal extension in other schedules."
)

unsupported <- apply_overlap_offset_to_data(
  df,
  y_var = "marginal_rate",
  groupvars = "series",
  color_var = "series",
  overlap_offset = list(enabled = TRUE, amount = 0.1),
  gopts = "point"
)
expect_true(!unsupported$enabled, "Overlap offsets should be ignored for unsupported plot types.")
expect_numeric_equal(
  unsupported$data[[unsupported$y_var]],
  df$marginal_rate,
  "Unsupported plot types should leave plotted values unchanged."
)

cat("Overlap offset checks passed.\n")
