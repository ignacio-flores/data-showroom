#!/usr/bin/env Rscript

source("modules/create_selectors.R")

fail <- function(message) {
  stop(message, call. = FALSE)
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

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

expect_error <- function(expression, pattern, message) {
  error <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = identity
  )
  if (is.null(error) || !grepl(pattern, conditionMessage(error), fixed = TRUE)) {
    fail(message)
  }
}

y2_choices <- c(
  "Top marginal rate" = "toprat",
  "Revenue share" = "prorev",
  "Revenue share of GDP" = "revgdp"
)

opted_out <- dual_axis_y2_choice_state(
  y_axis_value = "prorev",
  y2_choices = y2_choices,
  current_selection = "prorev"
)
expect_identical(
  opted_out$choices,
  y2_choices,
  "Opted-out charts should retain every configured secondary-axis choice."
)
expect_identical(
  opted_out$selected,
  "prorev",
  "Opted-out charts should preserve a valid duplicate selection."
)

preserved <- dual_axis_y2_choice_state(
  y_axis_value = "prorev",
  y2_choices = y2_choices,
  current_selection = "revgdp",
  configured_default = "toprat",
  prevent_duplicate_metrics = TRUE
)
expect_identical(
  preserved$choices,
  y2_choices[c(1, 3)],
  "Duplicate prevention should remove only the selected left-axis metric."
)
expect_identical(
  preserved$selected,
  "revgdp",
  "A still-valid current secondary-axis selection should be preserved."
)
expect_identical(
  names(preserved$choices),
  c("Top marginal rate", "Revenue share of GDP"),
  "Filtering should preserve secondary-axis labels and order."
)

configured_fallback <- dual_axis_y2_choice_state(
  y_axis_value = "prorev",
  y2_choices = y2_choices,
  current_selection = "prorev",
  configured_default = "revgdp",
  prevent_duplicate_metrics = TRUE
)
expect_identical(
  configured_fallback$selected,
  "revgdp",
  "An invalid current selection should fall back to the configured default."
)

first_fallback <- dual_axis_y2_choice_state(
  y_axis_value = "toprat",
  y2_choices = y2_choices,
  current_selection = "missing",
  configured_default = "toprat",
  prevent_duplicate_metrics = TRUE
)
expect_identical(
  first_fallback$selected,
  "prorev",
  "If the configured default is excluded, the first remaining metric should be used."
)

expect_error(
  dual_axis_y2_choice_state(
    y_axis_value = "toprat",
    y2_choices = c("Top marginal rate" = "toprat"),
    prevent_duplicate_metrics = TRUE
  ),
  "No secondary-axis metric remains",
  "Duplicate prevention should fail clearly when it would leave no choices."
)

axis_vars <- list(
  y_axis = list(
    var = "prorev",
    choices = 'c("revenu", "prorev")',
    alt.names = 'c("Revenue", "Revenue share")'
  ),
  y2_axis = list(
    var = "toprat",
    choices = 'c("toprat", "prorev", "revgdp")',
    alt.names = 'c("Top marginal rate", "Revenue share", "Revenue share of GDP")'
  )
)
dual_axis_options <- list(
  prevent_duplicate_metrics = TRUE,
  hover = list(
    mode = "compact",
    context_vars = list(
      GEO_long = list(label = "Country"),
      tax_type_view = list(
        label = "Type of tax",
        show_for = c("toprat", "exempt")
      )
    )
  )
)

expect_true(
  validate_dual_axis_options(
    dual_axis_options,
    axis_vars = axis_vars,
    gopts = "dual_axis_line"
  ),
  "A valid dual-axis option schema should pass validation."
)
expect_identical(
  dual_axis_hover_context_columns(dual_axis_options),
  c("GEO_long", "tax_type_view"),
  "Configured hover context column names should be exposed for data retention."
)
expect_identical(
  selector_axis_input_choices(axis_vars$y2_axis),
  y2_choices,
  "Axis choice parsing should preserve configured labels and order."
)

impossible_axis_vars <- axis_vars
impossible_axis_vars$y_axis <- list(
  var = "toprat",
  choices = 'c("toprat")'
)
impossible_axis_vars$y2_axis <- list(
  var = "toprat",
  choices = 'c("toprat")'
)
expect_error(
  validate_dual_axis_options(
    dual_axis_options,
    axis_vars = impossible_axis_vars,
    gopts = "dual_axis_line"
  ),
  "No secondary-axis metric remains",
  "Configuration validation should reject an axis state with no remaining metric."
)

expect_true(
  validate_dual_axis_options(NULL, axis_vars = axis_vars, gopts = "dual_axis_line"),
  "Absent dual-axis options should preserve existing chart behavior."
)

message("OK: dual-axis selector options preserve valid choices and prevent duplicates.")
