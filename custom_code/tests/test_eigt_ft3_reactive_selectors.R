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

expect_equal(
  normalize_selector_type(config$loose_selectors$year$type),
  "reactive selector",
  "eigt-ft3 year should be a reactive selector."
)
expect_equal(
  normalize_selector_type(config$loose_selectors$kinship$type),
  "very reactive checkbox",
  "eigt-ft3 kinship should refresh after year changes."
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
india_latest <- loose_selector_next_selection(
  "reactive selector",
  sort(unique(india_rows$year)),
  select_mode = "latest",
  initialized = FALSE
)
expect_equal(
  india_latest,
  "1984",
  "India should expose 1984 as the latest drawable FT year."
)

india_kinships <- sort(unique(india_rows$kinship[india_rows$year == india_latest]))
expect_equal(
  india_kinships,
  "Everybody",
  "India's latest FT slice should only select Everybody."
)

ireland_rows <- drawable_ft_rows("Ireland")
ireland_latest <- loose_selector_next_selection(
  "reactive selector",
  sort(unique(ireland_rows$year)),
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

ireland_latest_rows <- ireland_rows[ireland_rows$year == ireland_latest, , drop = FALSE]
ireland_latest_kinships <- sort(unique(ireland_latest_rows$kinship))
kinship_selection <- loose_selector_next_selection(
  "very reactive checkbox",
  ireland_latest_kinships,
  current_selection = india_kinships,
  initialized = TRUE,
  refresh_all = TRUE
)

expect_equal(
  kinship_selection,
  ireland_latest_kinships,
  "Ireland should refresh kinships to the latest-year choices instead of preserving India's Everybody selection."
)
expect_false(
  "Everybody" %in% kinship_selection,
  "Ireland's latest-year kinship selection should not include stale Everybody."
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
