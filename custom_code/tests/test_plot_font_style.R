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

expect_font <- function(font,
                        size,
                        color = plot_text_style$color,
                        message = "Font should match the shared plot style.") {
  expect_equal(font$family, plot_text_style$family, paste(message, "family"))
  expect_equal(font$size, size, paste(message, "size"))
  expect_equal(font$color, color, paste(message, "color"))
}

expect_font(
  plotly_font(),
  plot_text_style$legend_size,
  plot_text_style$color,
  "plotly_font() defaults should use the shared plot text style."
)

axis <- plotly_axis_style(list())
expect_font(
  axis$titlefont,
  plot_text_style$axis_title_size,
  plot_text_style$color,
  "Axis titles should use the shared axis-title font."
)
expect_font(
  axis$tickfont,
  plot_text_style$axis_tick_size,
  plot_text_style$color,
  "Axis tick labels should use the shared axis-tick font."
)

styled_axis <- plotly_typography_axis(list(
  titlefont = list(color = "#2C6DB2"),
  tickfont = list(color = "#E6A21A")
))
expect_font(
  styled_axis$titlefont,
  plot_text_style$axis_title_size,
  "#2C6DB2",
  "Typography normalization should preserve intentional axis title colors."
)
expect_font(
  styled_axis$tickfont,
  plot_text_style$axis_tick_size,
  "#E6A21A",
  "Typography normalization should preserve intentional axis tick colors."
)

pp <- plotly::plot_ly(
  data = data.frame(x = 1:2, y = c(10, 20), label = c("A", "B")),
  x = ~x,
  y = ~y,
  text = ~label,
  type = "scatter",
  mode = "markers+text",
  showlegend = TRUE
) %>%
  plotly::layout(
    xaxis = list(title = "X"),
    yaxis = list(
      title = "Y",
      titlefont = list(color = "#2C6DB2"),
      tickfont = list(color = "#E6A21A")
    ),
    legend = list(title = list(text = "Legend")),
    annotations = list(list(
      text = "Note",
      x = 1,
      y = 20,
      showarrow = FALSE,
      font = list(size = 14)
    )),
    sliders = list(list(currentvalue = list(prefix = "Year: "))),
    updatemenus = list(list(type = "buttons"))
  )

pp <- plotly::plotly_build(pp)
pp$x$data[[1]]$textfont <- list(size = 12, color = "#555555")
pp$x$data[[1]]$hoverlabel <- list(
  bgcolor = "#111111",
  font = list(color = "#FFFFFF")
)
pp$x$data[[1]]$marker$colorbar <- list(title = list(text = "Scale"))

pp <- apply_plotly_typography(pp, show_grid = FALSE)

expect_font(
  pp$x$layout$font,
  plot_text_style$legend_size,
  plot_text_style$color,
  "Layout font should be standardized."
)
expect_font(
  pp$x$layout$xaxis$titlefont,
  plot_text_style$axis_title_size,
  plot_text_style$color,
  "X-axis title font should be standardized."
)
expect_font(
  pp$x$layout$xaxis$tickfont,
  plot_text_style$axis_tick_size,
  plot_text_style$color,
  "X-axis tick font should be standardized."
)
expect_equal(
  pp$x$layout$xaxis$showgrid,
  FALSE,
  "Typography pass should preserve the requested grid visibility."
)
expect_font(
  pp$x$layout$yaxis$titlefont,
  plot_text_style$axis_title_size,
  "#2C6DB2",
  "Y-axis title color should remain intentional."
)
expect_font(
  pp$x$layout$yaxis$tickfont,
  plot_text_style$axis_tick_size,
  "#E6A21A",
  "Y-axis tick color should remain intentional."
)
expect_font(
  pp$x$layout$legend$font,
  plot_text_style$legend_size,
  plot_text_style$color,
  "Legend text should be standardized."
)
expect_font(
  pp$x$layout$legend$title$font,
  plot_text_style$legend_size,
  plot_text_style$color,
  "Legend title should be standardized."
)
expect_font(
  pp$x$layout$annotations[[1]]$font,
  14,
  plot_text_style$color,
  "Annotation font family/color should be standardized without changing explicit size."
)
expect_font(
  pp$x$data[[1]]$textfont,
  12,
  "#555555",
  "Trace text should inherit shared family without changing configured size/color."
)
expect_font(
  pp$x$data[[1]]$hoverlabel$font,
  plot_text_style$hover_size,
  "#FFFFFF",
  "Trace hover label font should preserve contrast color."
)
expect_font(
  pp$x$data[[1]]$marker$colorbar$title$font,
  plot_text_style$legend_size,
  plot_text_style$color,
  "Colorbar title should be standardized."
)
expect_font(
  pp$x$data[[1]]$marker$colorbar$tickfont,
  plot_text_style$legend_size,
  plot_text_style$color,
  "Colorbar tick font should be standardized."
)
expect_font(
  pp$x$layout$sliders[[1]]$currentvalue$font,
  plot_text_style$data_label_size,
  plot_text_style$color,
  "Animation slider current value should be standardized."
)
expect_font(
  pp$x$layout$updatemenus[[1]]$font,
  plot_text_style$data_label_size,
  plot_text_style$color,
  "Animation controls should be standardized."
)

line_plot <- plotly::plot_ly(
  data = data.frame(x = 1:3, y = c(1, 3, 2), label = c("A", "B", "C")),
  x = ~x,
  y = ~y,
  text = ~label,
  hoverinfo = "text",
  type = "scatter",
  mode = "lines",
  fill = "tozeroy"
)
line_plot <- apply_plotly_typography(plotly::plotly_build(line_plot))
expect_true(
  is.null(line_plot$x$data[[1]]$textfont),
  "Line/area scatter traces should not receive textfont unless they render text."
)
rebuilt_line_plot <- plotly::plotly_build(line_plot)
expect_equal(
  rebuilt_line_plot$x$data[[1]]$mode,
  "lines",
  "Rebuilding a styled line/area trace should not add text to the trace mode."
)

contrast_plot <- plotly::plot_ly() %>%
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

contrast_plot <- build_plotly_for_display(contrast_plot)
expect_font(
  contrast_plot$x$data[[1]]$hoverlabel$font,
  plot_text_style$hover_size,
  "#FFFFFF",
  "Dark hover labels should keep contrast-safe white text."
)
expect_font(
  contrast_plot$x$data[[2]]$hoverlabel$font,
  plot_text_style$hover_size,
  "#000000",
  "Light hover labels should keep contrast-safe black text."
)

cat("Plot typography checks passed.\n")
