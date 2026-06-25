#!/usr/bin/env Rscript

source("modules/PlotServer.R")

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

expect_equal <- function(value, expected, message) {
  if (!identical(value, expected)) {
    fail(paste0(message, " Expected ", expected, ", got ", value, "."))
  }
}

expect_equal(
  plotly_contrast_text_color("#111111"),
  "#FFFFFF",
  "Dark tooltip backgrounds should use white text."
)
expect_equal(
  plotly_contrast_text_color("#F7E35F"),
  "#000000",
  "Light tooltip backgrounds should use black text."
)
expect_equal(
  plotly_contrast_text_color("navy"),
  "#FFFFFF",
  "Named dark colors should use white text."
)
expect_equal(
  plotly_contrast_text_color("rgb(250, 250, 250)"),
  "#000000",
  "RGB light colors should use black text."
)

dark_hover <- plotly_hoverlabel_style(bgcolor = "#111111")
light_hover <- plotly_hoverlabel_style(bgcolor = "#F7E35F")
fallback_hover <- plotly_hoverlabel_style(bgcolor = "not-a-color")

expect_equal(dark_hover$font$color, "#FFFFFF", "Dark hover labels should be readable.")
expect_equal(light_hover$font$color, "#000000", "Light hover labels should be readable.")
expect_equal(fallback_hover$bgcolor, "not-a-color", "Unknown explicit backgrounds should be preserved.")
expect_equal(fallback_hover$font$color, plotly_neutral_hover_fg, "Unknown colors should use the neutral font fallback.")

expect_equal(
  plotly_trace_hover_color(list(
    type = "scatter",
    mode = "lines",
    line = list(color = "#111111"),
    marker = list(color = "#F7E35F")
  )),
  "#111111",
  "Line traces should use their line color for hover labels."
)
expect_equal(
  plotly_trace_hover_color(list(
    type = "scatter",
    mode = "lines",
    fill = "tozeroy",
    fillcolor = "rgba(17, 17, 17, 0.4)",
    line = list(color = "#F7E35F")
  )),
  "#111111",
  "Filled area traces should prefer their fill color as an opaque hover label."
)
expect_equal(
  plotly_trace_hover_color(list(
    type = "scatter",
    mode = "lines",
    hoverlabel = list(bgcolor = "black"),
    line = list(color = "#F7E35F")
  )),
  "#000000",
  "Explicit trace hover backgrounds should take precedence."
)

trace_plot <- plotly::plot_ly() %>%
  plotly::add_trace(
    x = 1,
    y = 1,
    type = "scatter",
    mode = "markers",
    marker = list(color = "#111111"),
    text = "dark",
    hoverinfo = "text"
  ) %>%
  plotly::add_trace(
    x = 2,
    y = 2,
    type = "scatter",
    mode = "markers",
    marker = list(color = "#F7E35F"),
    text = "light",
    hoverinfo = "text"
  )

trace_plot <- apply_plotly_contrast_hoverlabels(plotly::plotly_build(trace_plot))
expect_equal(trace_plot$x$data[[1]]$hoverlabel$bgcolor, "#111111", "Dark trace should keep its series color.")
expect_equal(trace_plot$x$data[[1]]$hoverlabel$font$color, "#FFFFFF", "Dark trace should use white tooltip text.")
expect_equal(trace_plot$x$data[[2]]$hoverlabel$bgcolor, "#F7E35F", "Light trace should keep its series color.")
expect_equal(trace_plot$x$data[[2]]$hoverlabel$font$color, "#000000", "Light trace should use black tooltip text.")

map_plot <- plotly::plot_ly(
  type = "choropleth",
  locations = c("FRA"),
  z = c(1),
  text = c("France"),
  hoverinfo = "text"
)
map_plot <- apply_plotly_contrast_hoverlabels(plotly::plotly_build(map_plot))
expect_equal(map_plot$x$data[[1]]$hoverlabel$bgcolor, plotly_neutral_hover_bg, "Map traces should use neutral tooltip backgrounds.")
expect_equal(map_plot$x$data[[1]]$hoverlabel$font$color, plotly_neutral_hover_fg, "Map traces should use neutral tooltip text.")
expect_equal(map_plot$x$data[[1]]$hoverlabel$bordercolor, plotly_neutral_hover_border, "Map traces should use the neutral tooltip border.")

cat("Contrast-safe hoverlabel checks passed.\n")
