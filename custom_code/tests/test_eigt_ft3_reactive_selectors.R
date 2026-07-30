#!/usr/bin/env Rscript

library(qs)
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

config <- yaml::read_yaml("yaml/config_eigt_ft3.yaml")
year_selector_type <- normalize_selector_type(config$loose_selectors$year$type)

expect_equal(
  year_selector_type,
  "very reactive selector",
  "eigt-ft3 year should be a very reactive selector."
)
expect_equal(
  normalize_selector_type(config$loose_selectors$kinship$type),
  "very reactive checkbox",
  "eigt-ft3 kinship should remain a very reactive checkbox."
)
expect_equal(
  names(config$fixed_selectors),
  c("GEO_long", "tax_type_view"),
  "eigt-ft3 fixed selectors should start with Country then Tax type."
)
expect_equal(
  names(config$loose_selectors),
  c("kinship", "xrate_lab", "year"),
  "eigt-ft3 loose selectors should run Kinship then Currency then Year."
)
expect_equal(
  c(names(config$fixed_selectors), names(config$loose_selectors)),
  c("GEO_long", "tax_type_view", "kinship", "xrate_lab", "year"),
  "eigt-ft3 selector dependency order should be Country -> tax -> kinship -> currency -> year."
)

data <- qs::qread(config$data.file)
source(config$data.wrangler)

drawable_ft_rows <- function(country) {
  data[
    data$GEO_long == country &
      data$tax_type_view == "Inheritance or estate tax" &
      !is.na(data$adjlbo) &
      !is.na(data$adjmrt),
    ,
    drop = FALSE
  ]
}

india_rows <- drawable_ft_rows("India")
india_kinships <- sort(unique(india_rows$kinship))
india_kinship_selection <- loose_selector_next_selection(
  "very reactive checkbox",
  india_kinships,
  initialized = FALSE
)
expect_equal(
  india_kinship_selection,
  "Everybody",
  "India should initially select its only available kinship."
)

india_selected_rows <- india_rows[
  india_rows$kinship %in% india_kinship_selection,
  ,
  drop = FALSE
]
india_latest <- loose_selector_next_selection(
  year_selector_type,
  sort(unique(india_selected_rows$year)),
  select_mode = "latest",
  initialized = FALSE
)
expect_equal(
  india_latest,
  "1984",
  "India should expose 1984 as the latest drawable FT year."
)

expect_equal(
  sort(unique(india_selected_rows$kinship[india_selected_rows$year == india_latest])),
  "Everybody",
  "India's latest FT slice should only select Everybody."
)

ireland_rows <- drawable_ft_rows("Ireland")
ireland_kinships <- sort(unique(ireland_rows$kinship))
kinship_selection <- loose_selector_next_selection(
  "very reactive checkbox",
  ireland_kinships,
  current_selection = india_kinship_selection,
  initialized = TRUE,
  refresh_all = TRUE
)
expect_equal(
  kinship_selection,
  ireland_kinships,
  "Ireland should refresh all kinship choices after the fixed Country selector changes."
)
expect_true(
  any(kinship_selection != "Everybody"),
  "Ireland's refreshed kinships should not preserve Everybody as the sole stale choice."
)

ireland_selected_rows <- ireland_rows[
  ireland_rows$kinship %in% kinship_selection,
  ,
  drop = FALSE
]
ireland_latest <- loose_selector_next_selection(
  year_selector_type,
  sort(unique(ireland_selected_rows$year)),
  current_selection = india_latest,
  select_mode = "latest",
  initialized = TRUE,
  refresh_selection = TRUE
)
expect_equal(
  ireland_latest,
  "2024",
  "Ireland should refresh to its latest drawable FT year after a fixed-selector change."
)

ireland_latest_rows <- ireland_selected_rows[
  ireland_selected_rows$year == ireland_latest,
  ,
  drop = FALSE
]
ireland_latest_kinships <- sort(unique(ireland_latest_rows$kinship))
expect_false(
  "Everybody" %in% ireland_latest_kinships,
  "Ireland's final latest-year slice should not include stale Everybody rows."
)

final_rows <- ireland_latest_rows[
  ireland_latest_rows$kinship %in% kinship_selection,
  ,
  drop = FALSE
]
expect_true(
  nrow(final_rows) > 0,
  "India -> Ireland selector refresh should leave drawable rows."
)

message("OK: eigt-ft3 reactive selector regression checks passed.")
