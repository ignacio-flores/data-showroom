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

removed_buttons <- plotly_modebar_buttons_to_remove()

for (button in plotly_modebar_reset_buttons) {
  expect_false(
    button %in% removed_buttons,
    paste0(button, " should remain available for graph reset.")
  )
}

for (button in c("toImage", "sendDataToCloud", "lasso2d", "select2d", "toggleHover")) {
  expect_true(
    button %in% removed_buttons,
    paste0(button, " should remain hidden from the Plotly modebar.")
  )
}

map_removed_buttons <- plotly_modebar_buttons_to_remove(extra_remove = "pan")
expect_true(
  "pan" %in% map_removed_buttons,
  "Map-specific modebar cleanup should still be able to hide pan."
)
expect_false(
  "resetGeo" %in% map_removed_buttons,
  "Map-specific modebar cleanup should keep the geo reset control."
)

configured_plot <- plotly::plot_ly(
  data = data.frame(x = c(1, 2), y = c(1, 2)),
  x = ~x,
  y = ~y,
  type = "scatter",
  mode = "lines"
) %>%
  plotly_modebar_config()

expect_equal(
  configured_plot$x$config$displaylogo,
  FALSE,
  "The shared modebar helper should hide the Plotly logo."
)

configured_removed_buttons <- configured_plot$x$config$modeBarButtonsToRemove[[1]]
expect_false(
  "resetScale2d" %in% configured_removed_buttons,
  "Configured Cartesian plots should keep resetScale2d."
)
expect_false(
  "autoScale2d" %in% configured_removed_buttons,
  "Configured Cartesian plots should keep autoScale2d."
)

cat("Plotly modebar reset checks passed.\n")
