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
