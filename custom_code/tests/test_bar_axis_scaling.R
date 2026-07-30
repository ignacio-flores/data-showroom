#!/usr/bin/env Rscript

library(shiny)
library(shinyWidgets)
source("modules/create_selectors.R")
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

expect_numeric_equal <- function(actual, expected, message) {
  if (!isTRUE(all.equal(
    as.numeric(actual),
    as.numeric(expected),
    check.attributes = FALSE
  ))) {
    fail(paste0(
      message,
      "\nExpected: ", paste(expected, collapse = ", "),
      "\nActual: ", paste(actual, collapse = ", ")
    ))
  }
}

expect_error <- function(expr, pattern, message) {
  error_message <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(error) conditionMessage(error)
  )
  if (is.null(error_message) ||
      !grepl(pattern, error_message, fixed = TRUE)) {
    fail(paste0(
      message,
      "\nExpected error containing: ", pattern,
      "\nActual: ", if (is.null(error_message)) "<no error>" else error_message
    ))
  }
}

bar_options <- list(
  axis_scale_selector = TRUE,
  axis_scale_default = "auto",
  auto_log_ratio = 50,
  animation_transition_ms = 500,
  animation_frame_ms = 1400,
  animation_easing = "cubic-in-out",
  category_labels = "bar",
  axis_range_padding = 0.02
)

heavy_thresholds <- c(rep(50000, 19), 12700000)
heavy_state <- resolve_bar_axis_scale(
  heavy_thresholds,
  requested_mode = "auto",
  bar_options = bar_options
)
expect_equal(
  heavy_state$resolved,
  "compressed",
  "A €12.7M maximum against a roughly €50k median should use pseudo-log in Auto mode."
)
expect_true(
  heavy_state$ratio >= 50,
  "The heavy-tailed exemption test data should cross the configured ratio."
)

for (values in list(
  c(0, 1, 1, 1),
  c(10, 25, 55, 80),
  c(1.2, 5.5, 12.5, 24),
  c(4, 10, 18, 20)
)) {
  expect_equal(
    resolve_bar_axis_scale(
      values,
      requested_mode = "auto",
      bar_options = bar_options
    )$resolved,
    "linear",
    "Indicators, rates, and percentage distributions should stay linear in Auto mode."
  )
}

expect_equal(
  resolve_bar_axis_scale(
    c(rep(6.61e8, 19), 5.98e10),
    requested_mode = "auto",
    bar_options = bar_options
  )$resolved,
  "compressed",
  "A heavy-tailed total-revenue distribution should resolve from its active values."
)
expect_equal(
  resolve_bar_axis_scale(
    heavy_thresholds,
    requested_mode = "linear",
    bar_options = bar_options
  )$resolved,
  "linear",
  "The manual Linear override should win over Auto."
)
expect_equal(
  resolve_bar_axis_scale(
    c(10, 25, 55, 80),
    requested_mode = "log",
    bar_options = bar_options
  )$resolved,
  "log",
  "The manual Logarithmic override should win over Auto."
)
manual_linear_axis <- fixed_bar_numeric_axis_layout(
  heavy_thresholds,
  resolved_scale = "linear",
  axis = list(title = "Exemption threshold"),
  label = "Exemption threshold"
)
manual_log_axis <- fixed_bar_numeric_axis_layout(
  c(10, 25, 55, 80),
  resolved_scale = "log",
  label = "Top marginal rate",
  var_name = "top_marginal_rate"
)
expect_true(
  identical(manual_linear_axis$title, "Exemption threshold") &&
    identical(manual_log_axis$title, "Top marginal rate") &&
    identical(manual_log_axis$type, "log"),
  "Manual overrides should use clean titles and a true logarithmic axis."
)
expect_equal(
  resolve_bar_axis_scale(
    c(-10, 10, 100),
    requested_mode = "log",
    bar_options = bar_options
  )$resolved,
  "compressed",
  "Manual logarithmic mode should preserve non-positive values via compression."
)
expect_equal(
  resolve_bar_axis_scale(
    c(1, 1000),
    requested_mode = "auto",
    bar_options = bar_options
  )$resolved,
  "linear",
  "Auto mode should require at least three finite displayed values."
)

expect_numeric_equal(
  bar_pseudo_log_transform(c(-99, 0, 99)),
  c(-2, 0, 2),
  "The pseudo-log transform should preserve sign and zero."
)

pseudo_axis <- bar_pseudo_log_axis_layout(
  heavy_thresholds,
  axis = list(title = "Exemption threshold"),
  var_name = "value",
  label = "Exemption threshold",
  tick_prefix = "\u20ac"
)
expect_true(
  identical(pseudo_axis$title, "Exemption threshold"),
  "The compressed numeric axis should keep the ordinary measure title."
)
expect_true(
  all(is.finite(unlist(pseudo_axis$range))),
  "The pseudo-log axis should have one finite fixed range."
)
expect_true(
  pseudo_axis$range[[2]] >=
    max(bar_pseudo_log_transform(heavy_thresholds)),
  "The fixed pseudo-log range should include the true maximum without clipping."
)
expect_true(
  all(grepl("^\u20ac", pseudo_axis$ticktext)) &&
    any(grepl("M$", pseudo_axis$ticktext)),
  "Compressed ticks should use clean currency-prefixed original values."
)
expect_numeric_equal(
  pseudo_axis$tickvals,
  bar_pseudo_log_transform(
    bar_pseudo_log_tick_values(heavy_thresholds)
  ),
  "Pseudo-log tick positions should transform original-value tick marks."
)

selector_html <- htmltools::renderTags(createSelectors(
  data = data.frame(value = 1),
  selector_info = list(),
  bar_options = bar_options
))$html
expect_true(
  grepl('id="bar_axis_scale"', selector_html, fixed = TRUE) &&
    grepl(">Auto<", selector_html, fixed = TRUE) &&
    grepl(">Linear<", selector_html, fixed = TRUE) &&
    grepl(">Logarithmic<", selector_html, fixed = TRUE),
  "Configured bar plots should render all three Axis scale choices."
)

expect_true(
  identical(
    normalize_bar_options(NULL),
    list(
      axis_scale_selector = FALSE,
      axis_scale_default = "linear",
      auto_log_ratio = 50,
      animation_transition_ms = 400,
      animation_frame_ms = 800,
      animation_easing = "linear",
      category_labels = "inside",
      axis_range_padding = 0.05
    )
  ),
  "Configs without bar_options should retain legacy bar behavior."
)
expect_true(
  identical(
    normalize_bar_options(list(
      axis_scale_selector = TRUE,
      axis_scale_default = "auto",
      auto_log_ratio = 50
    )),
    list(
      axis_scale_selector = TRUE,
      axis_scale_default = "auto",
      auto_log_ratio = 50,
      animation_transition_ms = 400,
      animation_frame_ms = 800,
      animation_easing = "linear",
      category_labels = "inside",
      axis_range_padding = 0.05
    )
  ),
  "Existing partial bar_options configs should receive legacy animation defaults."
)
expect_error(
  validate_bar_options(
    list(animation_transition_ms = -1),
    gopts = "bar"
  ),
  "animation_transition_ms",
  "Negative animation transition durations should be rejected."
)
expect_error(
  validate_bar_options(
    list(animation_frame_ms = 0),
    gopts = "bar"
  ),
  "animation_frame_ms",
  "Non-positive animation frame durations should be rejected."
)
expect_error(
  validate_bar_options(
    list(animation_easing = "abrupt"),
    gopts = "bar"
  ),
  "animation_easing",
  "Unsupported animation easing modes should be rejected."
)
expect_error(
  validate_bar_options(
    list(category_labels = "floating"),
    gopts = "bar"
  ),
  "category_labels",
  "Unsupported category-label modes should be rejected."
)
expect_error(
  validate_bar_options(
    list(axis_range_padding = 1.1),
    gopts = "bar"
  ),
  "axis_range_padding",
  "Out-of-range numeric-axis padding should be rejected."
)

animation_data <- data.frame(
  country = c("A", "B", "C", "D", "A", "C", "E"),
  year = c(rep(2020, 4), rep(2021, 3)),
  value = c(50000, 45000, 55000, 45000, 12700000, 60000, 50000)
)
data_state <- shiny::reactiveVal(animation_data)

shiny::testServer(
  plotModuleServer,
  args = list(
    filtered_data_func = shiny::reactive(data_state()),
    x_var = "country",
    x_var_lab = "",
    y_var = "value",
    y_var_lab = "Exemption threshold",
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
    groupvars = NULL,
    bar_axis_scale = "auto",
    bar_options = bar_options
  ),
  {
    payload <- jsonlite::fromJSON(output$valuePlot, simplifyVector = FALSE)
    main_axis <- payload$x$layout$xaxis

    expect_true(
      grepl(
        "Exemption threshold",
        main_axis$title,
        fixed = TRUE
      ),
      "The animated compressed axis should retain the ordinary measure title."
    )
    expect_numeric_equal(
      main_axis$range,
      animated_bar_numeric_range(
        bar_pseudo_log_transform(animation_data$value),
        padding = 0.02
      ),
      "The configured two-percent padding should determine the main fixed range."
    )
    expect_true(
      all(vapply(
        payload$x$frames,
        function(frame) is.null(frame$layout$xaxis),
        logical(1)
      )),
      "Animation frames should contain no numeric-axis range."
    )
    expect_true(
      all(vapply(
        payload$x$frames,
        function(frame) !is.null(frame$layout$yaxis$categoryarray),
        logical(1)
      )),
      "Animation frames should retain their country-order changes."
    )
    expect_true(
      identical(payload$x$layout$yaxis$showticklabels, FALSE),
      "Country names should not be duplicated on the categorical axis."
    )
    initial_trace <- payload$x$data[[1]]
    expect_true(
      !is.null(initial_trace$text) &&
        identical(initial_trace$cliponaxis, TRUE) &&
        all(unlist(initial_trace$textposition) == "auto") &&
        all(unlist(initial_trace$insidetextanchor) == "start"),
      "Country names should follow their bars with clipped automatic placement."
    )

    animation_settings <-
      payload$x$layout$updatemenus[[1]]$buttons[[1]]$args[[2]]
    expect_true(
      identical(as.numeric(animation_settings$transition$duration), 500) &&
        identical(as.numeric(animation_settings$frame$duration), 1400) &&
        identical(animation_settings$transition$easing, "cubic-in-out") &&
        identical(animation_settings$frame$redraw, TRUE),
      "Animated bars should use slower eased transitions with redraw enabled."
    )
    expect_true(
      as.numeric(payload$x$layout$margin$l) <= 60 &&
        identical(payload$x$layout$annotations[[1]]$text, "Compressed scale"),
      "Bar-attached labels should need no gutter and compression should use a subtle note."
    )

    for (frame in payload$x$frames) {
      frame_x <- as.numeric(unlist(lapply(
        frame$data,
        function(trace) trace$x
      )))
      frame_y <- as.character(unlist(lapply(
        frame$data,
        function(trace) trace$y
      )))
      category_axis <- frame$layout$yaxis
      category_array <- as.character(unlist(category_axis$categoryarray))

      expect_true(
        setequal(frame_y, category_array) &&
          length(category_array) == length(unique(frame_y)),
        paste("Every frame category array should exactly match its bars:", frame$name)
      )
      expect_numeric_equal(
        category_axis$range,
        c(-0.5, length(category_array) - 0.5),
        paste("Every frame should bound its categorical range:", frame$name)
      )
      expect_equal(
        tail(category_array, 1),
        frame_y[[which.max(frame_x)]],
        paste("The highest-value bar should occupy the top row:", frame$name)
      )
      expect_true(
        identical(category_axis$showticklabels, FALSE),
        paste("Every frame should keep country names off the axis:", frame$name)
      )
    }
    expect_equal(
      as.character(unlist(payload$x$frames[[1]]$layout$yaxis$categoryarray)),
      c("D", "B", "A", "C"),
      "Equal values should use country name as a deterministic tie-breaker."
    )
    all_hover <- unlist(lapply(payload$x$frames, function(frame) {
      unlist(lapply(frame$data, function(trace) trace$hovertext))
    }))
    expect_true(
      any(grepl("12,700,000.00", all_hover, fixed = TRUE)),
      "Pseudo-log plotting should keep original values in tooltips."
    )
    expect_true(
      all(vapply(
        payload$x$frames,
        function(frame) {
          frame_x <- as.numeric(unlist(lapply(
            frame$data,
            function(trace) trace$x
          )))
          all(frame_x >= main_axis$range[[1]] &
                frame_x <= main_axis$range[[2]])
        },
        logical(1)
      )),
      "All animated values should remain inside the main numeric range."
    )

    full_range <- as.numeric(unlist(main_axis$range))
    data_state(subset(animation_data, country != "A"))
    session$flushReact()
    filtered_payload <- jsonlite::fromJSON(
      output$valuePlot,
      simplifyVector = FALSE
    )
    filtered_range <- as.numeric(unlist(filtered_payload$x$layout$xaxis$range))
    expect_true(
      !identical(full_range, filtered_range),
      "Changing the active countries should recompute the main fixed range."
    )
    expect_true(
      all(vapply(
        filtered_payload$x$frames,
        function(frame) is.null(frame$layout$xaxis),
        logical(1)
      )),
      "Recomputed animations should not retain stale frame ranges."
    )
    expect_true(
      all(vapply(
        filtered_payload$x$frames,
        function(frame) {
          frame_y <- as.character(unlist(lapply(
            frame$data,
            function(trace) trace$y
          )))
          category_array <- as.character(unlist(
            frame$layout$yaxis$categoryarray
          ))
          category_range <- as.numeric(unlist(
            frame$layout$yaxis$range
          ))
          setequal(frame_y, category_array) &&
            length(category_array) == length(unique(frame_y)) &&
            identical(
              category_range,
              c(-0.5, length(category_array) - 0.5)
            )
        },
        logical(1)
      )),
      "Filtered animations should contain no stale category positions."
    )
  }
)

wm2_artifact <- "data/taxw_wm2_ready.qs"
if (file.exists(wm2_artifact)) {
  wm2_data <- qs::qread(wm2_artifact)
  concepts <- c(
    "Tax Indicator",
    "Top Marginal Rate",
    "Exemption Threshold",
    "Total Revenue from Tax",
    "Total Revenue from Tax as % of Total Tax Revenue",
    "Total Revenue from Tax as % of Gross Domestic Product"
  )
  representative_currencies <- c(
    "National Currency",
    "USD",
    "Euro adjusting for inflation",
    "PPP USD"
  )
  euro_expected_modes <- c(
    "Tax Indicator" = "linear",
    "Top Marginal Rate" = "linear",
    "Exemption Threshold" = "compressed",
    "Total Revenue from Tax" = "compressed",
    "Total Revenue from Tax as % of Total Tax Revenue" = "linear",
    "Total Revenue from Tax as % of Gross Domestic Product" = "linear"
  )

  for (concept in concepts) {
    category <- if (grepl("^Total Revenue", concept)) {
      "Estate, inheritance and gift taxes (EIG)"
    } else {
      "Inheritance or estate tax"
    }

    for (currency in representative_currencies) {
      active_data <- wm2_data[
        wm2_data$d4_concept_lab == concept &
          wm2_data$xrate_lab == currency &
          wm2_data$tax_category == category &
          wm2_data$show_zero == "No" &
          wm2_data$year >= 1965 &
          wm2_data$year <= 2023 &
          is.finite(wm2_data$value) &
          wm2_data$value != 0,
        ,
        drop = FALSE
      ]

      totals <- active_data %>%
        dplyr::group_by(year, GEO_long) %>%
        dplyr::summarise(.val = sum(value, na.rm = TRUE), .groups = "drop")
      top_by_year <- totals %>%
        dplyr::group_by(year) %>%
        dplyr::arrange(dplyr::desc(.val), .by_group = TRUE) %>%
        dplyr::slice_head(n = 20) %>%
        dplyr::ungroup()
      displayed_data <- dplyr::semi_join(
        active_data,
        top_by_year,
        by = c("year", "GEO_long")
      )

      expect_true(
        nrow(displayed_data) > 0,
        paste("The real WM2 smoke selection should have data:", concept, currency)
      )
      scale_state <- resolve_bar_axis_scale(
        displayed_data$value,
        requested_mode = "auto",
        bar_options = bar_options
      )
      expect_true(
        scale_state$resolved %in% c("linear", "compressed") &&
          is.finite(scale_state$ratio),
        paste("The real WM2 smoke selection should resolve a scale:", concept, currency)
      )

      axis <- fixed_bar_numeric_axis_layout(
        displayed_data$value,
        resolved_scale = scale_state$resolved,
        label = concept,
        var_name = "value"
      )
      expect_true(
        length(axis$range) == 2 &&
          all(is.finite(unlist(axis$range))) &&
          length(axis$tickvals) == length(axis$ticktext),
        paste("The real WM2 smoke selection should build a fixed axis:", concept, currency)
      )

      if (identical(currency, "Euro adjusting for inflation")) {
        expect_equal(
          scale_state$resolved,
          unname(euro_expected_modes[[concept]]),
          paste("The real WM2 Euro selection should resolve appropriately:", concept)
        )
      }
    }
  }

  exemption_selection <- wm2_data[
    wm2_data$d4_concept_lab == "Exemption Threshold" &
      wm2_data$xrate_lab == "Euro adjusting for inflation" &
      wm2_data$tax_category == "Inheritance or estate tax" &
      wm2_data$show_zero == "No" &
      wm2_data$year >= 1965 &
      wm2_data$year <= 2023 &
      wm2_data$GEO_long != "Zimbabwe" &
      is.finite(wm2_data$value) &
      wm2_data$value != 0,
    ,
    drop = FALSE
  ]
  exemption_totals <- exemption_selection %>%
    dplyr::group_by(year, GEO_long) %>%
    dplyr::summarise(.val = sum(value, na.rm = TRUE), .groups = "drop")
  exemption_top <- exemption_totals %>%
    dplyr::group_by(year) %>%
    dplyr::arrange(dplyr::desc(.val), .by_group = TRUE) %>%
    dplyr::slice_head(n = 20) %>%
    dplyr::ungroup()
  exemption_displayed <- dplyr::semi_join(
    exemption_selection,
    exemption_top,
    by = c("year", "GEO_long")
  )
  exemption_scale <- resolve_bar_axis_scale(
    exemption_displayed$value,
    requested_mode = "auto",
    bar_options = bar_options
  )
  expect_true(
    max(exemption_displayed$value) > 12e6 &&
      max(exemption_displayed$value) < 13e6 &&
      stats::median(exemption_displayed$value) > 40e3 &&
      stats::median(exemption_displayed$value) < 70e3 &&
      identical(exemption_scale$resolved, "compressed"),
    paste0(
      "The real filtered exemption case (about €12.7M maximum and €50k ",
      "median) should resolve to pseudo-log."
    )
  )

  real_payload_result <- new.env(parent = emptyenv())
  shiny::testServer(
    plotModuleServer,
    args = list(
      filtered_data_func = shiny::reactive(exemption_selection),
      x_var = "GEO_long",
      x_var_lab = "",
      y_var = "value",
      y_var_lab = "",
      color_var = NULL,
      color_var_lab = NULL,
      facet_var = NULL,
      facet_var_lab = NULL,
      tooltip_vars = list(
        GEO_long = "Country:",
        year = "Year:",
        value = "Value:"
      ),
      hide.legend = TRUE,
      gopts = c("bar", "hbar", "animate"),
      xnum_breaks = NULL,
      extra_layer = NULL,
      color_style = NULL,
      plot_height = 700,
      groupvars = NULL,
      bar_axis_scale = "auto",
      bar_options = bar_options
    ),
    {
      real_payload_result$payload <- jsonlite::fromJSON(
        output$valuePlot,
        simplifyVector = FALSE
      )
    }
  )

  real_payload <- real_payload_result$payload
  real_axis <- real_payload$x$layout$xaxis
  real_initial_trace <- real_payload$x$data[[1]]
  expect_true(
    identical(real_axis$title, "Exemption Threshold") &&
      all(grepl("^\u20ac", unlist(real_axis$ticktext))) &&
      identical(
        real_payload$x$layout$annotations[[1]]$text,
        "Compressed scale"
      ),
    "The real exemption chart should use a clean title, Euro ticks, and a subtle scale note."
  )
  expect_true(
    !is.null(real_initial_trace$text) &&
      all(unlist(real_initial_trace$textposition) == "auto") &&
      identical(real_initial_trace$cliponaxis, TRUE) &&
      identical(real_payload$x$layout$yaxis$showticklabels, FALSE),
    "The real exemption chart should attach clipped country labels to its bars."
  )
  frame_names <- vapply(
    real_payload$x$frames,
    function(frame) as.character(frame$name),
    character(1)
  )
  churn_years <- as.character(c(
    1974, 1975, 1978, 1979, 2005, 2006,
    2010, 2011, 2017, 2018, 2019, 2020
  ))
  expect_true(
    all(churn_years %in% frame_names),
    "The real WM2 payload should include every targeted country-churn frame."
  )

  real_category_counts <- integer(length(real_payload$x$frames))
  real_main_range <- as.numeric(unlist(real_payload$x$layout$xaxis$range))
  for (frame_index in seq_along(real_payload$x$frames)) {
    frame <- real_payload$x$frames[[frame_index]]
    frame_x <- as.numeric(unlist(lapply(
      frame$data,
      function(trace) trace$x
    )))
    frame_y <- as.character(unlist(lapply(
      frame$data,
      function(trace) trace$y
    )))
    category_array <- as.character(unlist(
      frame$layout$yaxis$categoryarray
    ))
    category_range <- as.numeric(unlist(frame$layout$yaxis$range))
    real_category_counts[[frame_index]] <- length(category_array)

    expect_true(
      setequal(frame_y, category_array) &&
        length(category_array) == length(unique(frame_y)),
      paste("The real frame should contain no stale categories:", frame$name)
    )
    expect_numeric_equal(
      category_range,
      c(-0.5, length(category_array) - 0.5),
      paste("The real frame should bound its category axis:", frame$name)
    )
    expect_equal(
      tail(category_array, 1),
      frame_y[[which.max(frame_x)]],
      paste("The real frame leader should remain at the top:", frame$name)
    )
    expect_true(
      is.null(frame$layout$xaxis) &&
        all(frame_x >= real_main_range[[1]] &
              frame_x <= real_main_range[[2]]),
      paste("The real frame should inherit a non-clipping numeric range:", frame$name)
    )
  }
  expect_true(
    min(real_category_counts) >= 16 &&
      max(real_category_counts) == 20 &&
      any(real_category_counts < 20),
    "The real payload should cover stable frames containing 16 through 20 countries."
  )
} else {
  message("Skipping real WM2 scale smoke checks; ", wm2_artifact, " is not present.")
}

cat("Adaptive bar-axis scaling checks passed.\n")
