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

expect_identical <- function(actual, expected, message) {
  if (!identical(actual, expected)) {
    fail(paste0(
      message,
      "\nExpected: ", paste(expected, collapse = ", "),
      "\nActual: ", paste(actual, collapse = ", ")
    ))
  }
}

expect_false(dual_axis_compact_hover_enabled(NULL),
  "Missing dual-axis options should preserve legacy hover behavior."
)
expect_false(
  dual_axis_compact_hover_enabled(list(hover = list(mode = "legacy"))),
  "Non-compact hover modes should preserve legacy hover behavior."
)
expect_true(
  dual_axis_compact_hover_enabled(list(hover = list(mode = " Compact "))),
  "Compact hover mode should be normalized and enabled."
)

context_vars <- list(
  GEO_long = list(label = "Country:"),
  tax_type_view = list(
    label = "Type of tax:",
    show_for = c("toprat", "exempt")
  ),
  revenue_tax_category = list(
    label = "Revenue category:",
    show_for = c("revenu", "prorev", "revgdp")
  ),
  xrate_lab = list(
    label = "Currency & price adjustment:",
    show_for = c("revenu", "exempt")
  ),
  unselected_metric = list(
    label = "Unselected:",
    show_for = "revgdp"
  )
)

specs <- dual_axis_hover_context_specs(
  context_vars,
  selected_vars = c("prorev", "toprat"),
  excluded_vars = c("year", "prorev", "toprat"),
  available_vars = c(
    "GEO_long", "tax_type_view", "revenue_tax_category",
    "xrate_lab", "unselected_metric"
  )
)
expect_identical(
  vapply(specs, `[[`, character(1), "var"),
  c("GEO_long", "tax_type_view", "revenue_tax_category"),
  "Context filtering should retain shared and selected-metric context only."
)

df <- data.frame(
  year = c(2020, 2021, 2022),
  prorev = c(5, NA_real_, 6.25),
  toprat = c(30, 31, NA_real_),
  GEO_long = rep("United States", 3),
  tax_type_view = c(
    "Inheritance or estate tax",
    NA_character_,
    "Inheritance tax"
  ),
  revenue_tax_category = rep(
    "Estate, inheritance and gift taxes (EIG)",
    3
  ),
  xrate_lab = rep("Euro adjusting for inflation", 3),
  unselected_metric = rep("must not appear", 3),
  stringsAsFactors = FALSE
)

templates <- dual_axis_compact_hover_templates(
  df,
  x_var = "year",
  y_var = "prorev",
  y_var_lab = "Tax Revenue % Total Tax Revenue",
  y2_var = "toprat",
  y2_var_lab = "Top Marginal Rate",
  context_vars = context_vars
)

expect_true(
  templates$include_right,
  "Distinct selected metrics should retain both dual-axis traces."
)
expect_true(
  grepl("<b>Tax Revenue % Total Tax Revenue</b>: 5%", templates$left[[1]],
    fixed = TRUE
  ),
  "Left metrics should use shared axis-number formatting."
)
expect_true(
  grepl("<b>Top Marginal Rate</b>: 30%", templates$right[[1]], fixed = TRUE),
  "Right metrics should use shared axis-number formatting."
)
expect_false(
  any(grepl("Year|2020|2021|2022", c(templates$left, templates$right))),
  "Year should appear only in Plotly's unified x-axis header."
)
expect_false(
  any(grepl(
    "Country|Revenue category|Type of tax",
    c(templates$left, templates$right)
  )),
  "Colored metric traces should contain no shared context."
)
expect_false(
  any(grepl("<br>", c(templates$left, templates$right), fixed = TRUE)),
  "Each colored trace should render a one-line metric-only template."
)
expect_true(
  all(c(
    grepl("<b>Country</b>: United States", templates$context[[1]], fixed = TRUE),
    grepl(
      "<b>Type of tax</b>: Inheritance or estate tax",
      templates$context[[1]],
      fixed = TRUE
    ),
    grepl(
      "<b>Revenue category</b>: Estate, inheritance and gift taxes (EIG)",
      templates$context[[1]],
      fixed = TRUE
    )
  )),
  "Applicable shared context should appear once in its own template."
)
expect_false(
  any(grepl(
    "Currency & price adjustment|Unselected|must not appear",
    templates$context
  )),
  "Context for unselected metrics should be omitted."
)
expect_false(
  grepl("Type of tax|NA", templates$context[[2]]) ||
    grepl("NA", templates$left[[2]]) ||
    grepl("NA", templates$right[[2]]),
  "Missing context and selected metric values should be omitted."
)
expect_true(
  all(c(
    grepl("<b>Tax Revenue % Total Tax Revenue</b>: 6.2%", templates$left[[3]],
      fixed = TRUE
    ),
    grepl("<b>Country</b>: United States", templates$context[[3]], fixed = TRUE),
    !grepl("Top Marginal Rate|NA", templates$right[[3]])
  )),
  "Context should remain independent when only the left metric has a value."
)
expect_true(
  templates$include_context &&
    identical(templates$context_carrier_yaxis, "y") &&
    all(templates$context_carrier_y %in% df$prorev[is.finite(df$prorev)]),
  "The context carrier should reuse an observed left-axis value."
)

same_metric <- dual_axis_compact_hover_templates(
  df,
  x_var = "year",
  y_var = "prorev",
  y_var_lab = "Tax Revenue % Total Tax Revenue",
  y2_var = "prorev",
  y2_var_lab = "Tax Revenue % Total Tax Revenue",
  context_vars = context_vars
)
expect_false(
  same_metric$include_right,
  "Selecting the same metric on both axes should suppress the duplicate trace."
)
expect_true(
  grepl("<b>Revenue category</b>:", same_metric$context[[1]], fixed = TRUE) &&
    !grepl("Revenue category", same_metric$left[[1]], fixed = TRUE) &&
    !grepl("Tax Revenue", same_metric$right[[1]], fixed = TRUE),
  "Same-metric suppression should retain one metric trace and one context carrier."
)

right_only_carrier <- dual_axis_compact_hover_carrier(
  rep(NA_real_, 3),
  c(30, 31, 32),
  active_rows = c(TRUE, TRUE, FALSE)
)
expect_true(
  identical(right_only_carrier$yaxis, "y2") &&
    all(right_only_carrier$y[1:2] %in% c(30, 31, 32)) &&
    is.na(right_only_carrier$y[[3]]),
  "The carrier should use y2 only when the left series has no finite value."
)

empty_metric_row <- df[1, , drop = FALSE]
empty_metric_row$prorev <- NA_real_
empty_metric_row$toprat <- NA_real_
empty_templates <- dual_axis_compact_hover_templates(
  empty_metric_row,
  x_var = "year",
  y_var = "prorev",
  y_var_lab = "Tax Revenue % Total Tax Revenue",
  y2_var = "toprat",
  y2_var_lab = "Top Marginal Rate",
  context_vars = context_vars
)
expect_false(
  empty_templates$include_context,
  "Context should have no carrier when neither selected metric exists."
)

expect_identical(
  dual_axis_hover_format_values(
    1500000,
    var_name = "revenu",
    label = "Tax Revenue"
  ),
  "1.5M",
  "Compact hover values should delegate to format_axis_number."
)

all_metric_data <- data.frame(
  year = 2023L,
  revenu = 1250.5,
  prorev = 5.25,
  revgdp = 0.65,
  toprat = 30,
  exempt = 2.5,
  GEO_long = "United States",
  tax_type_view = "Inheritance or estate tax",
  revenue_tax_category = "Estate, inheritance and gift taxes (EIG)",
  xrate_lab = "Euro adjusting for inflation",
  stringsAsFactors = FALSE
)
metric_labels <- c(
  revenu = "Tax Revenue (millions, selected currency)",
  prorev = "Tax Revenue % Total Tax Revenue",
  revgdp = "Tax Revenue % GDP",
  toprat = "Top Marginal Rate",
  exempt = "Exemption Threshold (millions, selected currency)"
)
left_metrics <- c("revenu", "prorev", "revgdp")
right_metrics <- c("toprat", "exempt", "prorev", "revgdp")
policy_metrics <- c("toprat", "exempt")
revenue_metrics <- c("revenu", "prorev", "revgdp")
monetary_metrics <- c("revenu", "exempt")
context_expectations <- list(
  Country = function(selected) TRUE,
  `Type of tax` = function(selected) any(selected %in% policy_metrics),
  `Revenue category` = function(selected) any(selected %in% revenue_metrics),
  `Currency & price adjustment` = function(selected) {
    any(selected %in% monetary_metrics)
  }
)

for (left_metric in left_metrics) {
  for (right_metric in setdiff(right_metrics, left_metric)) {
    selected_metrics <- c(left_metric, right_metric)
    pair_templates <- dual_axis_compact_hover_templates(
      all_metric_data,
      x_var = "year",
      y_var = left_metric,
      y_var_lab = metric_labels[[left_metric]],
      y2_var = right_metric,
      y2_var_lab = metric_labels[[right_metric]],
      context_vars = context_vars
    )
    pair_metric_hover <- c(pair_templates$left, pair_templates$right)
    pair_context_hover <- pair_templates$context

    for (metric in selected_metrics) {
      expect_identical(
        sum(grepl(
          paste0("<b>", metric_labels[[metric]], "</b>:"),
          pair_metric_hover,
          fixed = TRUE
        )),
        1L,
        paste(
          "Each selected metric should appear once for",
          left_metric,
          "and",
          right_metric
        )
      )
    }

    for (context_label in names(context_expectations)) {
      expected_count <- if (context_expectations[[context_label]](
        selected_metrics
      )) 1L else 0L
      expect_identical(
        sum(grepl(
          paste0("<b>", context_label, "</b>:"),
          pair_context_hover,
          fixed = TRUE
        )),
        expected_count,
        paste(
          context_label,
          "context should follow metric applicability for",
          left_metric,
          "and",
          right_metric
        )
      )
    }
  }
}

compact_plot <- plotly::plot_ly() %>%
  plotly::add_trace(
    data = df,
    x = ~year,
    y = ~prorev,
    type = "scatter",
    mode = "lines+markers",
    name = "Tax Revenue % Total Tax Revenue",
    hovertemplate = templates$left
  ) %>%
  plotly::add_trace(
    data = df,
    x = ~year,
    y = ~toprat,
    type = "scatter",
    mode = "lines+markers",
    name = "Top Marginal Rate",
    yaxis = "y2",
    hovertemplate = templates$right
  )
compact_plot <- add_dual_axis_compact_context_trace(
  compact_plot,
  data = df,
  x_var = "year",
  context_template = templates$context,
  carrier_y = templates$context_carrier_y,
  carrier_yaxis = templates$context_carrier_yaxis
) %>%
  plotly::layout(hovermode = "x unified")
compact_plot <- plotly::plotly_build(compact_plot)

expect_identical(
  compact_plot$x$layout$hovermode,
  "x unified",
  "Compact dual-axis plots should retain Plotly's single unified x heading."
)
built_hover <- unlist(
  lapply(compact_plot$x$data, `[[`, "hovertemplate"),
  use.names = FALSE
)
expect_identical(
  length(compact_plot$x$data),
  3L,
  "Compact unified hover should build two metric traces and one context carrier."
)
expect_false(
  any(grepl(
    "Country|Revenue category|Type of tax",
    unlist(lapply(compact_plot$x$data[1:2], `[[`, "hovertemplate"))
  )),
  "Built metric traces should contain no shared context."
)
expect_true(
  all(grepl(
    "<b>Country</b>: United States",
    compact_plot$x$data[[3]]$hovertemplate,
    fixed = TRUE
  )),
  "The transparent third trace should carry all shared context."
)
expect_true(
  identical(
    compact_plot$x$data[[3]]$marker$color,
    "rgba(0,0,0,0)"
  ) &&
    !isTRUE(compact_plot$x$data[[3]]$showlegend),
  "The context carrier should have a transparent, legend-free swatch."
)
expect_true(
  all(compact_plot$x$data[[3]]$y %in% range(df$prorev, na.rm = TRUE)) &&
    (is.null(compact_plot$x$data[[3]]$yaxis) ||
      identical(compact_plot$x$data[[3]]$yaxis, "y")),
  "The carrier should remain within the left-axis range and avoid y2."
)
expect_false(
  any(grepl("Year|2020|2021|2022", built_hover)),
  "Built Plotly trace templates should leave the year to the unified heading."
)
expect_identical(
  sum(grepl("<b>Country</b>: United States", built_hover, fixed = TRUE)),
  nrow(df),
  "The three-year carrier should contain country context exactly once per year."
)

cat("OK: compact dual-axis hover composition is concise and opt-in.\n")
