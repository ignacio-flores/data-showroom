#!/usr/bin/env Rscript

library(yaml)

source("modules/create_selectors.R")

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

expect_equal(
  normalize_selector_type(" Sticky_Checkbox "),
  "sticky checkbox",
  "Selector type normalization should handle whitespace and underscores."
)
expect_equal(
  selector_checkbox_mode("checkbox"),
  "sticky",
  "Legacy checkbox should remain a sticky checkbox alias."
)
expect_equal(
  selector_checkbox_mode("sticky checkbox"),
  "sticky",
  "Explicit sticky checkbox should use sticky mode."
)
expect_equal(
  selector_checkbox_mode("reactive checkbox"),
  "reactive",
  "Reactive checkbox should use reactive mode."
)
expect_equal(
  selector_checkbox_mode("very reactive checkbox"),
  "very_reactive",
  "Very reactive checkbox should use very-reactive mode."
)
expect_true(
  selector_is_checkbox_like("very reactive checkbox"),
  "All checkbox refresh modes should render as checkbox-like picker inputs."
)
expect_false(
  selector_is_checkbox_like("selector"),
  "Single-value selector controls should not be checkbox-like."
)

choices <- c("A", "B", "C")
expect_equal(
  loose_selector_next_selection(
    "sticky checkbox",
    choices,
    current_selection = c("A", "Z"),
    initialized = TRUE
  ),
  "A",
  "Sticky checkbox should preserve selected values that remain available."
)
expect_equal(
  loose_selector_next_selection(
    "sticky checkbox",
    choices,
    current_selection = "Z",
    initialized = TRUE
  ),
  choices,
  "Sticky checkbox should fall back to all choices when selected values disappear."
)
expect_equal(
  loose_selector_next_selection(
    "sticky checkbox",
    choices,
    configured_selection = "B",
    initialized = FALSE
  ),
  "B",
  "Initial sticky checkbox selection should honor configured selected values."
)
expect_equal(
  loose_selector_next_selection(
    "reactive checkbox",
    choices,
    current_selection = "A",
    initialized = TRUE,
    refresh_all = TRUE
  ),
  choices,
  "Reactive checkbox should select all choices when a fixed-selector refresh is requested."
)
expect_equal(
  loose_selector_next_selection(
    "very reactive checkbox",
    choices,
    current_selection = "A",
    initialized = TRUE,
    refresh_all = TRUE
  ),
  choices,
  "Very reactive checkbox should select all choices when another data selector changes."
)

spaced_choices <- as.character(seq_len(9))
spaced_selection <- loose_selector_next_selection(
  "sticky checkbox",
  spaced_choices,
  select_mode = "spaced",
  initialized = FALSE
)
expect_equal(
  spaced_selection,
  spaced_choices[c(1, 3, 5, 7, 9)],
  "Initial checkbox selection should keep select: spaced behavior."
)

loose_vars <- c("source", "concept", "year")
expect_true(
  loose_selector_should_refresh_all(
    "reactive checkbox",
    initialized = TRUE,
    change_source = "__fixed__",
    own_var = "source",
    loose_vars = loose_vars
  ),
  "Reactive checkbox should refresh after fixed selector changes."
)
expect_false(
  loose_selector_should_refresh_all(
    "reactive checkbox",
    initialized = TRUE,
    change_source = "concept",
    own_var = "source",
    loose_vars = loose_vars
  ),
  "Reactive checkbox should not refresh after loose selector changes."
)
expect_true(
  loose_selector_should_refresh_all(
    "very reactive checkbox",
    initialized = TRUE,
    change_source = "concept",
    own_var = "source",
    loose_vars = loose_vars
  ),
  "Very reactive checkbox should refresh after another loose selector changes."
)
expect_false(
  loose_selector_should_refresh_all(
    "very reactive checkbox",
    initialized = TRUE,
    change_source = "source",
    own_var = "source",
    loose_vars = loose_vars
  ),
  "Very reactive checkbox should not refresh after its own user selection changes."
)
expect_false(
  loose_selector_should_refresh_all(
    "very reactive checkbox",
    initialized = TRUE,
    change_source = "concept",
    own_var = "source",
    loose_vars = loose_vars,
    unprocessed_change = FALSE
  ),
  "A data selector change should only be consumed once per checkbox."
)

config_paths <- list.files("yaml", pattern = "^config_.*\\.yaml$", full.names = TRUE)
for (config_path in config_paths) {
  config <- yaml::read_yaml(config_path)

  if (!is.null(config$loose_selectors)) {
    for (var in names(config$loose_selectors)) {
      type <- normalize_selector_type(config$loose_selectors[[var]]$type)
      expect_false(
        identical(type, "checkbox"),
        paste0(config_path, " loose selector ", var, " should explicitly use sticky checkbox.")
      )
    }
  }
}

fixed_checkbox_configs <- vapply(config_paths, function(config_path) {
  config <- yaml::read_yaml(config_path)
  if (is.null(config$fixed_selectors)) return(FALSE)
  any(vapply(config$fixed_selectors, function(info) {
    identical(normalize_selector_type(info$type), "checkbox")
  }, logical(1)))
}, logical(1))

expect_true(
  any(fixed_checkbox_configs),
  "At least one active config should continue covering legacy fixed checkbox types."
)
expect_equal(
  selector_checkbox_mode("checkbox"),
  "sticky",
  "Legacy fixed checkbox types should remain supported by the shared helper."
)

cat("Loose checkbox mode checks passed.\n")
