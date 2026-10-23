#!/usr/bin/env Rscript

library(shiny)
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

expect_equal(
  format_axis_number(c(0, 0.00042), spec = list(format = "percent")),
  c("0%", "0.00042%"),
  "Small GDP-share axis ticks should remain distinguishable from zero."
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

axis_case <- function(data, label = "Value", var_name = "value") {
  presentation <- axis_display_presentation(data, var_name, label, list())
  list(
    label = presentation$label,
    axis_info = presentation$axis_info,
    spec = axis_number_format_spec(
      var_name, presentation$label, presentation$axis_info
    )
  )
}

inheritance_ratio <- axis_case(data.frame(
  value = 0.1,
  d3_vartype_lab = "Ratio",
  d4_concept_lab = "Inheritances & Gifts"
))
expect_equal(inheritance_ratio$label, "Inheritances & Gifts ratio",
             "Inheritance charts should name the selected ratio.")
expect_equal(format_axis_number(0.1, spec = inheritance_ratio$spec), "10%",
             "Inheritance fractions should display as percentages.")

inheritance_aggregate <- axis_case(data.frame(
  value = 1e9,
  d3_vartype_lab = "Aggregate",
  d4_concept_lab = "Inheritances & Gifts"
))
expect_equal(inheritance_aggregate$label, "Inheritances & Gifts aggregate",
             "Inheritance aggregate charts should name the selected measure.")
expect_equal(format_axis_number(1e9, spec = inheritance_aggregate$spec), "1B",
             "Inheritance aggregates should retain monetary formatting.")

dual_inheritance <- axis_case(data.frame(average_effective_tax_rate = 0.07),
                              "Average effective tax rate",
                              "average_effective_tax_rate")
expect_equal(format_axis_number(0.07, spec = dual_inheritance$spec), "7%",
             "Dual-axis inheritance tax rates should display fraction data as percentages.")

tax_share <- axis_case(data.frame(
  value = 0.0053,
  d4_concept_lab = "Total Revenue from Tax as % of Gross Domestic Product",
  tax_category = "Estate, inheritance and gift taxes (EIG)"
))
expect_equal(tax_share$label,
             "Total Revenue from Tax as % of GDP",
             "EIGT axes should name the selected tax feature.")
expect_equal(format_axis_number(0.0053, spec = tax_share$spec), "0.0053%",
             "EIGT percentage-point data should not be multiplied by 100.")
tax_map_colorbar <- plotly_colorbar_style(
  tax_share$label, values = c(0.0053, 0.01), var_name = "value",
  label = tax_share$label, axis_info = tax_share$axis_info
)
expect_equal(tax_map_colorbar$title$text,
             "Total Revenue from Tax<br>as % of GDP",
             "Long map colorbar titles should break at a natural phrase boundary.")
expect_true(all(grepl("%$", tax_map_colorbar$ticktext)),
            "Map colorbars should display tax revenue shares as percentages.")
expect_equal(plotly_colorbar_title("Top Marginal Rate"), "Top Marginal Rate",
             "Short map colorbar titles should stay on one line.")
expect_equal(
  plotly_colorbar_title("Total Revenue from Tax as % of Total Tax Revenue"),
  "Total Revenue from Tax<br>as % of Total Tax Revenue",
  "Other long revenue titles should also break between complete phrases."
)
expect_true(
  grepl("<br>", plotly_colorbar_title("An exceptionally long map legend title"),
        fixed = TRUE),
  "Long map titles without revenue wording should also wrap."
)
expect_equal(
  format_eigt_tooltip_value(
    "Total Revenue from Tax as % of Gross Domestic Product",
    "d4_concept_lab"
  ),
  "Total Revenue from Tax as % of GDP",
  "Tax feature tooltips should use the abbreviated display label."
)
tax_bar_axis <- animated_bar_numeric_axis_layout(
  values = c(0.0053, 0.01), axis = list(title = tax_share$label),
  var_name = "value", label = tax_share$label,
  axis_info = tax_share$axis_info
)
expect_true(all(grepl("%$", tax_bar_axis$ticktext)),
            "Animated bar numeric axes should display percentage units.")
tax_rate <- axis_case(data.frame(
  value = 40, d4_concept_lab = "Top Marginal Rate",
  tax_category = "Estate, inheritance and gift taxes (EIG)"
))
expect_equal(format_axis_number(40, spec = tax_rate$spec), "40%",
             "EIGT top rates already in percentage points should keep their scale.")

wealth_share <- axis_case(data.frame(
  value = 19.2,
  d3_vartype_lab = "Share of Total Net Wealth",
  d4_concept_lab = "Net Wealth"
))
expect_equal(wealth_share$label, "Share of Total Net Wealth",
             "Inequality axes should name the selected variable.")
expect_equal(format_axis_number(19.2, spec = wealth_share$spec), "19.2%",
             "Inequality shares already in percentage points should keep their scale.")
gini <- axis_case(data.frame(value = 73.2,
                             d3_vartype_lab = "Gini Coefficient"))
expect_equal(format_axis_number(73.2, spec = gini$spec), "73.2",
             "Gini coefficients should not acquire a percent suffix.")

topo_ratio <- axis_case(data.frame(
  value = 0.1,
  d4_concept_lab = "Debt",
  pop_lab = "Ratio to National Income"
))
expect_equal(topo_ratio$label, "Debt (% of national income)",
             "Topo axes should name the selected concept and unit.")
expect_equal(format_axis_number(0.1, spec = topo_ratio$spec), "10%",
             "Topo ratios to national income should display as percentages.")

topo_amount <- axis_case(data.frame(
  value = 1200,
  d4_concept_lab = "Debt",
  pop_lab = "Per capita"
))
expect_equal(topo_amount$label, "Debt (Per capita)",
             "Topo monetary axes should include the selected unit.")
expect_equal(format_axis_number(1200, spec = topo_amount$spec), "1.2k",
             "Topo monetary amounts should not be converted to percentages.")

axis_fixtures <- list(
  eigt = data.frame(value = 40, d4_concept_lab = "Top Marginal Rate",
                    tax_category = "Estate, inheritance and gift taxes (EIG)"),
  ineq = data.frame(value = 19.2,
                    d3_vartype_lab = "Share of Total Net Wealth",
                    d4_concept_lab = "Net Wealth"),
  inhe = data.frame(value = 0.1, d3_vartype_lab = "Ratio",
                    d4_concept_lab = "Inheritances & Gifts"),
  topo = data.frame(value = 1000, d4_concept_lab = "Debt",
                    pop_lab = "Per capita")
)
config_paths <- list.files("yaml", pattern = "^config_.*[.]yaml$",
                           full.names = TRUE)
for (config_path in config_paths) {
  config <- yaml::read_yaml(config_path)
  y_axis <- config$axis_vars$y_axis
  if (is.null(y_axis) || !axis_generic_label(y_axis$label)) next

  family <- sub("^config_([a-z]+).*", "\\1", basename(config_path))
  if (identical(y_axis$var, "value")) {
    presentation <- axis_display_presentation(
      axis_fixtures[[family]], y_axis$var, y_axis$label, y_axis
    )
    expect_true(
      !axis_generic_label(presentation$label),
      paste("Generic y-axis label remains in", basename(config_path))
    )
  } else {
    expect_true(
      !is.null(y_axis$alt.names),
      paste("Generic selectable y-axis lacks measure labels in",
            basename(config_path))
    )
  }
}

inheritance_lines <- data.frame(
  year = c(2020, 2021),
  value = c(0.1, 0.2),
  series = c("A", "A"),
  d3_vartype_lab = c("Ratio", "Ratio"),
  d4_concept_lab = c("Inheritances & Gifts", "Inheritances & Gifts")
)
shiny::testServer(
  plotModuleServer,
  args = list(
    filtered_data_func = shiny::reactive(inheritance_lines),
    x_var = "year", x_var_lab = "Year",
    y_var = "value", y_var_lab = "Value",
    color_var = "series", color_var_lab = "Series",
    facet_var = NULL, facet_var_lab = NULL,
    tooltip_vars = list(series = "Series:", value = "Value:"),
    hide.legend = FALSE, gopts = "line", xnum_breaks = 5,
    extra_layer = NULL, color_style = "dichromat::DarkRedtoBlue_12",
    plot_height = 400, groupvars = "series"
  ),
  {
    payload <- jsonlite::fromJSON(output$valuePlot, simplifyVector = FALSE)
    expect_equal(
      payload$x$layout$yaxis$title$text,
      "Inheritances & Gifts ratio",
      "The rendered y-axis should name the selected inheritance variable."
    )
    tick_text <- as.character(unlist(payload$x$layout$yaxis$ticktext))
    expect_true(
      "10%" %in% tick_text && "20%" %in% tick_text,
      "The rendered y-axis should convert inheritance fractions to percentages."
    )
    hover_text <- as.character(unlist(lapply(payload$x$data, `[[`, "text")))
    expect_true(
      any(grepl("10%", hover_text, fixed = TRUE)),
      "The tooltip should use the same percentage unit as the y-axis."
    )
  }
)

inheritance_aggregate_lines <- inheritance_lines
inheritance_aggregate_lines$value <- c(1e11, 1.1e11)
inheritance_aggregate_lines$d3_vartype_lab <- "Aggregate"
inheritance_aggregate_lines$xrate_lab <- "Local currency, nominal prices"
shiny::testServer(
  plotModuleServer,
  args = list(
    filtered_data_func = shiny::reactive(inheritance_aggregate_lines),
    x_var = "year", x_var_lab = "Year",
    y_var = "value", y_var_lab = "Value",
    color_var = "series", color_var_lab = "Series",
    facet_var = NULL, facet_var_lab = NULL,
    tooltip_vars = NULL,
    hide.legend = FALSE, gopts = "line", xnum_breaks = 5,
    extra_layer = NULL, color_style = "dichromat::DarkRedtoBlue_12",
    plot_height = 400, groupvars = "series"
  ),
  {
    payload <- jsonlite::fromJSON(output$valuePlot, simplifyVector = FALSE)
    expect_equal(
      payload$x$layout$yaxis$title$text,
      "Inheritances & Gifts<br>LCU, nominal",
      "The rendered aggregate y-axis should wrap the plain ASCII currency title."
    )
  }
)

inheritance_dual_lines <- data.frame(
  year = c(2020, 2021),
  GEO_long = c("A", "A"),
  inheritances_gifts_ratio = c(0.1, 0.2),
  average_effective_tax_rate = c(0.01, 0.02)
)
shiny::testServer(
  plotModuleServer,
  args = list(
    filtered_data_func = shiny::reactive(inheritance_dual_lines),
    x_var = "year", x_var_lab = "Year",
    y_var = "inheritances_gifts_ratio",
    y_var_lab = "Inheritances & gifts ratio",
    y2_var = "average_effective_tax_rate",
    y2_var_lab = "Average effective tax rate",
    color_var = "GEO_long", color_var_lab = "Country",
    facet_var = NULL, facet_var_lab = NULL,
    tooltip_vars = list(GEO_long = "Country:"),
    hide.legend = FALSE, gopts = "dual_axis_line", xnum_breaks = 5,
    extra_layer = NULL, color_style = "dichromat::DarkRedtoBlue_12",
    plot_height = 400, groupvars = "GEO_long"
  ),
  {
    payload <- jsonlite::fromJSON(output$valuePlot, simplifyVector = FALSE)
    left_ticks <- as.character(unlist(payload$x$layout$yaxis$ticktext))
    right_ticks <- as.character(unlist(payload$x$layout$yaxis2$ticktext))
    expect_true("10%" %in% left_ticks,
                "Dual-axis inheritance ratios should display 0.1 as 10%.")
    expect_true("1%" %in% right_ticks,
                "Dual-axis effective tax rates should display 0.01 as 1%.")
  }
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

animated_axis <- animated_bar_numeric_axis_layout(
  values = c(5, 100),
  axis = list(title = "Value"),
  var_name = "value",
  label = "Value"
)
expect_equal(
  animated_axis$range,
  c(0, 105),
  "Animated positive bar axes should explicitly cover a later frame maximum."
)
expect_equal(
  animated_axis$autorange,
  FALSE,
  "Animated bar axes should use an explicit range instead of retained autorange state."
)

mixed_animation_range <- animated_bar_numeric_range(c(-20, 80))
expect_equal(
  mixed_animation_range,
  c(-25, 85),
  "Animated bar ranges should pad both sides when values cross zero."
)

animation_data <- data.frame(
  country = rep(c("A", "B"), 2),
  year = rep(c(2020, 2021), each = 2),
  value = c(10, 5, 100, 50)
)
shiny::testServer(
  plotModuleServer,
  args = list(
    filtered_data_func = shiny::reactive(animation_data),
    x_var = "country",
    x_var_lab = "",
    y_var = "value",
    y_var_lab = "",
    color_var = NULL,
    color_var_lab = NULL,
    facet_var = NULL,
    facet_var_lab = NULL,
    tooltip_vars = list(
      country = "Country:",
      year = "Year:",
      value = "Value:"
    ),
    hide.legend = TRUE,
    gopts = c("bar", "hbar", "animate"),
    xnum_breaks = NULL,
    extra_layer = NULL,
    color_style = NULL,
    plot_height = 700,
    groupvars = NULL
  ),
  {
    payload <- jsonlite::fromJSON(output$valuePlot, simplifyVector = FALSE)
    initial_range <- as.numeric(unlist(payload$x$layout$xaxis$range))
    frame_numeric_axes <- lapply(
      payload$x$frames,
      function(frame) frame$layout$xaxis
    )
    expect_equal(
      initial_range,
      c(0, 105),
      "The animation should start with the range required by its highest value."
    )
    expect_true(
      all(vapply(frame_numeric_axes, is.null, logical(1))),
      paste0(
        "Animation frames should not duplicate the numeric x-axis; ",
        "they inherit the fixed range from the main layout."
      )
    )
  }
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
