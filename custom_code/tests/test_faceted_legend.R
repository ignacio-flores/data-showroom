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
  plots[[facet_idx]] <- legend_result$plot
  legend_seen <- legend_result$legend_seen
}

built <- plotly::plotly_build(
  plotly::subplot(plots, nrows = ceiling(sqrt(length(plots))), shareX = TRUE, shareY = TRUE)
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

cat("Faceted legend regression checks passed.\n")
