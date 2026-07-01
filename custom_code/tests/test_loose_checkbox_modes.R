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
expect_equal(
  selector_single_mode("selector"),
  "sticky",
  "Legacy selector should remain a sticky single-selector alias."
)
expect_equal(
  selector_single_mode("sticky selector"),
  "sticky",
  "Explicit sticky selector should use sticky mode."
)
expect_equal(
  selector_single_mode("reactive selector"),
  "reactive",
  "Reactive selector should use reactive mode."
)
expect_equal(
  selector_single_mode("very reactive selector"),
  "very_reactive",
  "Very reactive selector should use very-reactive mode."
)
expect_true(
  selector_is_checkbox_like("very reactive checkbox"),
  "All checkbox refresh modes should render as checkbox-like picker inputs."
)
expect_false(
  selector_is_checkbox_like("selector"),
  "Single-value selector controls should not be checkbox-like."
)
expect_true(
  selector_is_single_like("reactive selector"),
  "Reactive selector should render as a single-value picker input."
)
expect_true(
  selector_is_single_like("very reactive selector"),
  "Very reactive selector should render as a single-value picker input."
)
expect_true(
  selector_is_very_reactive("very reactive selector"),
  "Very reactive selector should opt into very-reactive choice filtering."
)
expect_true(
  selector_is_very_reactive("very reactive checkbox"),
  "Very reactive checkbox should opt into very-reactive choice filtering."
)
expect_true(
  selector_selects_latest(" Latest "),
  "Selector select mode normalization should recognize latest."
)
expect_equal(
  selector_latest_choice(c("2022", "2025", "2024")),
  "2025",
  "Latest selection should use numeric maximum for numeric-like choices."
)
expect_equal(
  selector_latest_choice(c("Beta", "Alpha", "Gamma")),
  "Gamma",
  "Latest selection should use the last sorted value for non-numeric choices."
)

expect_true(
  selector_values_equal(c("B", "A"), c("A", "B")),
  "Selector value signatures should be order-insensitive."
)
expect_false(
  selector_values_equal(c("A", "B"), c("A", "C")),
  "Selector value signatures should detect changed values."
)

choices <- c("A", "B", "C")
ui_signature <- loose_selector_ui_signature(choices, "A")
expect_false(
  loose_selector_ui_needs_update(ui_signature, c("C", "B", "A"), "A"),
  "Loose selector UI updates should be skipped when choices and selection are unchanged."
)
expect_true(
  loose_selector_ui_needs_update(NULL, choices, "A"),
  "Loose selector UI updates should run for selectors with no prior sent state."
)
expect_true(
  loose_selector_ui_needs_update(ui_signature, c("A", "B", "C", "D"), "A"),
  "Loose selector UI updates should run when available choices change."
)
expect_true(
  loose_selector_ui_needs_update(ui_signature, choices, c("A", "B")),
  "Loose selector UI updates should run when selected values change."
)

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
year_choices <- c("2022", "2025", "2024")
expect_equal(
  loose_selector_next_selection(
    "sticky selector",
    year_choices,
    select_mode = "latest",
    initialized = FALSE
  ),
  "2025",
  "Initial single-value selectors with select: latest should choose the latest value."
)
expect_equal(
  loose_selector_next_selection(
    "sticky selector",
    year_choices,
    current_selection = "2024",
    select_mode = "latest",
    initialized = TRUE
  ),
  "2024",
  "Latest selectors should preserve an existing user selection without a fixed-selector refresh."
)
expect_equal(
  loose_selector_next_selection(
    "reactive selector",
    year_choices,
    current_selection = "2024",
    select_mode = "latest",
    initialized = TRUE,
    refresh_selection = TRUE
  ),
  "2025",
  "Reactive latest selectors should refresh to the latest value after an upstream selector change."
)
expect_equal(
  loose_selector_next_selection(
    "reactive selector",
    choices,
    current_selection = "A",
    configured_selection = "B",
    initialized = TRUE,
    refresh_selection = TRUE
  ),
  "B",
  "Reactive selectors should recompute configured selections after fixed selector changes."
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
expect_equal(
  loose_selector_next_selection(
    "reactive checkbox",
    spaced_choices,
    current_selection = "1",
    select_mode = "spaced",
    initialized = TRUE,
    refresh_all = TRUE
  ),
  spaced_choices[c(1, 3, 5, 7, 9)],
  "Reactive checkbox refreshes should reapply select: spaced."
)
expect_equal(
  loose_selector_next_selection(
    "very reactive checkbox",
    spaced_choices,
    current_selection = "1",
    select_mode = "spaced",
    initialized = TRUE,
    refresh_all = TRUE
  ),
  spaced_choices[c(1, 3, 5, 7, 9)],
  "Very reactive checkbox refreshes should reapply select: spaced."
)
expect_equal(
  loose_selector_next_selection(
    "reactive checkbox",
    spaced_choices,
    current_selection = "1",
    initialized = TRUE,
    refresh_all = TRUE
  ),
  spaced_choices,
  "Reactive checkbox refreshes without select: spaced should still select all choices."
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
expect_true(
  loose_selector_should_refresh_selection(
    "reactive selector",
    initialized = TRUE,
    change_source = "__fixed__"
  ),
  "Reactive selectors should refresh after fixed selector changes."
)
expect_true(
  loose_selector_should_refresh_selection(
    "very reactive selector",
    initialized = TRUE,
    change_source = "__fixed__"
  ),
  "Very reactive selectors should refresh after fixed selector changes."
)
expect_false(
  loose_selector_should_refresh_selection(
    "very reactive selector",
    initialized = TRUE,
    change_source = "year"
  ),
  "Very reactive selectors should preserve valid values after loose selector changes."
)
expect_false(
  loose_selector_should_refresh_selection(
    "sticky selector",
    initialized = TRUE,
    change_source = "__fixed__"
  ),
  "Sticky selectors should preserve existing values after fixed selector changes."
)
expect_true(
  loose_selector_should_refresh_latest(
    "reactive selector",
    select_mode = "latest",
    initialized = TRUE,
    change_source = "__fixed__"
  ),
  "Reactive latest selectors should refresh after fixed selector changes."
)
expect_false(
  loose_selector_should_refresh_latest(
    "reactive selector",
    select_mode = "latest",
    initialized = TRUE,
    change_source = "year"
  ),
  "Reactive latest selectors should not refresh after their own user selection changes."
)
expect_false(
  loose_selector_should_refresh_latest(
    "selector",
    select_mode = "latest",
    initialized = TRUE,
    change_source = "__fixed__"
  ),
  "Legacy sticky selector aliases should not refresh after fixed selector changes."
)
expect_false(
  loose_selector_should_refresh_latest(
    "sticky checkbox",
    select_mode = "latest",
    initialized = TRUE,
    change_source = "__fixed__"
  ),
  "Checkbox selectors should keep their existing refresh rules."
)

loose_filter_data <- data.frame(
  year = c("2000", "2001", "2002"),
  kinship = c("Children", "Children", "Parents"),
  value = c(1, 2, 3),
  stringsAsFactors = FALSE
)
filtered_loose_data <- loose_selector_filter_data(
  loose_filter_data,
  loose_filters = list(year = c("2000", "2002")),
  loose_selectors = list(year = list()),
  selector_initialized = list(year = TRUE)
)
expect_equal(
  filtered_loose_data$year,
  c("2000", "2002"),
  "Loose filters should keep only selected values."
)
expect_true(
  is.null(loose_selector_filter_data(
    loose_filter_data,
    loose_filters = list(year = character(0)),
    loose_selectors = list(year = list()),
    selector_initialized = list(year = TRUE)
  )),
  "Initialized loose selectors with no selected values should not fall through to all values."
)
expect_equal(
  loose_selector_filter_data(
    loose_filter_data,
    loose_filters = list(),
    loose_selectors = list(year = list()),
    selector_initialized = list(year = FALSE)
  )$year,
  loose_filter_data$year,
  "Uninitialized loose selectors may be skipped before their first computed selection."
)
expect_true(
  is.null(loose_selector_filter_data(
    loose_filter_data,
    loose_filters = list(year = "1999"),
    loose_selectors = list(year = list()),
    selector_initialized = list(year = TRUE)
  )),
  "Loose filters with no active values in the data should return no data."
)

config_paths <- list.files("yaml", pattern = "^config_.*\\.yaml$", full.names = TRUE)
for (config_path in config_paths) {
  config <- yaml::read_yaml(config_path)

  for (section in c("fixed_selectors", "loose_selectors")) {
    if (is.null(config[[section]])) next
    for (var in names(config[[section]])) {
      type <- normalize_selector_type(config[[section]][[var]]$type)
      if (identical(section, "loose_selectors")) {
        expect_false(
          identical(type, "checkbox"),
          paste0(config_path, " ", section, " ", var, " should explicitly use sticky checkbox.")
        )
      }
      expect_false(
        identical(type, "selector"),
        paste0(config_path, " ", section, " ", var, " should explicitly use sticky selector or reactive selector.")
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

ft1_config <- yaml::read_yaml("yaml/config_eigt_ft1.yaml")
expect_true(
  selector_is_single_like(ft1_config$loose_selectors$kinship$type),
  "eigt-ft1 kinship should use a supported single-selector type."
)
expect_true(
  selector_is_very_reactive(ft1_config$loose_selectors$kinship$type),
  "eigt-ft1 kinship should use very-reactive choice filtering."
)

cat("Loose checkbox mode checks passed.\n")
