#!/usr/bin/env Rscript

source("modules/PlotServer.R")

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

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

data_path <- "data/topo_base.qs"
if (!file.exists(data_path)) {
  fail(paste("Missing required test data:", data_path))
}

df <- qs::qread(data_path)
df <- subset(
  df,
  GEO_long == "United States" &
    d2_sector_lab == "Households"
)
df$tooltip_text <- ""
df <- df[do.call(order, df[c("legend", "d4_concept_lab", "year")]), , drop = FALSE]

extra_layer_values <- "Net Wealth"
extra_df <- subset(df, d4_concept_lab %in% extra_layer_values)
main_df <- subset(df, !(d4_concept_lab %in% extra_layer_values))

facet_levels <- unique(main_df$legend)
color_levels <- unique(main_df$d4_concept_lab)
first_facet_colors <- unique(main_df$d4_concept_lab[main_df$legend == facet_levels[[1]]])

expect_true(
  length(color_levels) > length(first_facet_colors),
  "Regression setup should have more color categories globally than in the first facet."
)

pal <- as.vector(
  paletteer::paletteer_d(
    palette = "nationalparkcolors::Badlands",
    n = length(color_levels),
    direction = 1,
    type = "continuous"
  )
)

plots <- vector("list", length(facet_levels))
legend_seen <- character(0)

for (facet_idx in seq_along(facet_levels)) {
  facet_level <- facet_levels[[facet_idx]]
  df_facet <- main_df[main_df$legend == facet_level, , drop = FALSE]

  plt <- plotly::plot_ly(
    df_facet,
    x = ~year,
    y = ~value,
    color = ~d4_concept_lab,
    colors = pal,
    text = ~tooltip_text,
    hoverinfo = "text",
    type = "scatter",
    mode = "lines",
    fill = "tozeroy",
    line = list(shape = "linear"),
    stackgroup = "one",
    height = 600,
    opacity = 1,
    legendgroup = ~d4_concept_lab,
    showlegend = facet_level == facet_levels[[1]]
  ) %>%
    plotly::layout(
      xaxis = plotly_axis_style(list(title = "Year")),
      yaxis = plotly_axis_style(list(title = "Value")) %>%
        plotly_number_axis_layout(
          values = df_facet$value,
          var_name = "value",
          label = "Value"
        )
    )

  df_extra_facet <- extra_df[extra_df$legend == facet_level, , drop = FALSE]
  if (nrow(df_extra_facet) > 0) {
    plt <- plt %>%
      plotly::add_trace(
        data = df_extra_facet,
        x = ~year,
        y = ~value,
        type = "scatter",
        mode = "line",
        line = list(color = "black", width = 2),
        fill = NULL,
        name = extra_layer_values,
        text = ~tooltip_text,
        hoverinfo = "text",
        color = I("black"),
        showlegend = facet_level == facet_levels[[1]],
        legendgroup = "extra_layer",
        inherit = FALSE
      )
  }

  legend_result <- set_plotly_legend_once(
    plotly::plotly_build(plt),
    legend_seen,
    show_legend = TRUE
  )
  plots[[facet_idx]] <- apply_plotly_typography(
    apply_plotly_contrast_hoverlabels(legend_result$plot)
  )
  legend_seen <- legend_result$legend_seen
}

n_facets <- length(facet_levels)
nrows <- ceiling(sqrt(n_facets))

stack_sum <- function(data) {
  if (nrow(data) == 0) return(numeric(0))
  stats::aggregate(value ~ legend + year, data = data, FUN = sum)$value
}
combined_y <- c(
  stack_sum(main_df[main_df$value > 0, , drop = FALSE]),
  stack_sum(main_df[main_df$value < 0, , drop = FALSE]),
  extra_df$value
)
y_range <- range(combined_y, na.rm = TRUE)
shared_y_axis_layout <- plotly_axis_style(list(
  range = y_range,
  zeroline = FALSE,
  showticklabels = TRUE
)) %>%
  plotly_number_axis_layout(
    values = combined_y,
    var_name = "value",
    label = "Value",
    n = 5L
  )
shared_y_axis_title_annotation <- list(list(
  x = -0.065,
  y = 0.5,
  text = "Value",
  showarrow = FALSE,
  xanchor = "center",
  yanchor = "middle",
  xref = "paper",
  yref = "paper",
  textangle = -90,
  font = plotly_font(plot_text_style$axis_title_size)
))

built <- build_plotly_for_display(
  plotly::subplot(
    plots,
    nrows = nrows,
    shareX = TRUE,
    shareY = TRUE,
    titleY = FALSE
  ) %>%
    plotly::layout(
      annotations = shared_y_axis_title_annotation,
      yaxis = shared_y_axis_layout
    ) %>%
    apply_plotly_shared_yaxis_layout(
      shared_y_axis_layout,
      remove_title = TRUE
    )
)
traces <- built$x$data
legend_names <- vapply(
  traces[vapply(traces, function(trace) isTRUE(trace$showlegend), logical(1))],
  function(trace) trace$name %||% plotly_trace_legend_key(trace),
  character(1)
)

expected_legend_names <- c(as.character(color_levels), extra_layer_values)
expect_true(
  setequal(legend_names, expected_legend_names),
  "Faceted legend should include every color category plus the extra layer exactly once."
)
expect_true(
  !anyDuplicated(legend_names),
  "Faceted legend should not duplicate entries."
)

main_traces <- traces[vapply(traces, function(trace) {
  !identical(plotly_trace_legend_key(trace), "extra_layer")
}, logical(1))]
main_modes <- vapply(main_traces, function(trace) trace$mode %||% "", character(1))
main_fills <- vapply(main_traces, function(trace) trace$fill %||% "", character(1))

expect_true(
  all(main_modes == "lines"),
  "Main faceted area traces should remain line-only traces, without marker points."
)
expect_true(
  all(main_fills == "tozeroy"),
  "Main faceted area traces should retain the original area fill behavior."
)

layout <- built$x$layout
y_axis_names <- plotly_axis_layout_names(layout, "y")
expect_true(
  length(y_axis_names) > 1,
  "Regression setup should create multiple subplot y axes."
)
y_axes <- layout[y_axis_names]

axis_numeric_field <- function(axis, field) {
  unname(as.numeric(unlist(axis[[field]])))
}
axis_character_field <- function(axis, field) {
  as.character(unlist(axis[[field]]))
}
axis_title_text <- function(axis) {
  title <- axis$title
  if (is.null(title)) return("")
  if (is.list(title)) return(title$text %||% "")
  as.character(title[[1]])
}

expected_range <- axis_numeric_field(shared_y_axis_layout, "range")
expected_tickvals <- axis_numeric_field(shared_y_axis_layout, "tickvals")
expected_ticktext <- axis_character_field(shared_y_axis_layout, "ticktext")

for (axis_name in y_axis_names) {
  axis <- layout[[axis_name]]
  expect_equal(
    axis_numeric_field(axis, "range"),
    expected_range,
    paste("Faceted", axis_name, "should use the shared global y range.")
  )
  expect_equal(
    axis_numeric_field(axis, "tickvals"),
    expected_tickvals,
    paste("Faceted", axis_name, "should use the shared global y tick values.")
  )
  expect_equal(
    axis_character_field(axis, "ticktext"),
    expected_ticktext,
    paste("Faceted", axis_name, "should use the shared global y tick labels.")
  )
}

y_axis_titles <- vapply(y_axes, axis_title_text, character(1))
expect_true(
  all(!nzchar(y_axis_titles)),
  "Multi-facet subplot row y axes should not repeat the y-axis title."
)

value_title_annotations <- Filter(function(annotation) {
  identical(annotation$text, "Value") &&
    identical(as.numeric(annotation$textangle), -90)
}, layout$annotations)
expect_true(
  length(value_title_annotations) == 1,
  "Multi-facet plots should have exactly one shared rotated y-axis title annotation."
)

cat("Faceted legend and y-axis regression checks passed.\n")
