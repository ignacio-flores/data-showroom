#!/usr/bin/env Rscript

source("custom_code/helpers/eigt_tax_kinship.R")

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) {
    fail(message)
  }
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

test_data <- data.frame(
  GEO = c("A", "A", "B", "B", "C", "C", "C", "C", "D"),
  GEO_long = c(
    "Alpha", "Alpha",
    "Beta", "Beta",
    "Gamma", "Gamma", "Gamma", "Gamma",
    "Delta"
  ),
  year = c("2000", "2000", "2000", "2000", "2000", "2000", "2000", "2000", "2000"),
  tax_type = c(
    "Inheritance tax", "Inheritance tax",
    "Estate tax", "Estate tax",
    "Inheritance tax", "Inheritance tax",
    "Estate tax", "Estate tax",
    "Gift tax"
  ),
  kinship = c(
    "Children", "Children",
    "Children", "Children",
    "Children", "Children",
    "Children", "Children",
    "Children"
  ),
  d5_code = c(
    "a-inh-1", "a-inh-2",
    "b-est-1", "b-est-2",
    "c-inh-1", "c-inh-2",
    "c-est-1", "c-est-2",
    "d-gift-1"
  ),
  adjmrt = c(0.1, 0.2, 0.3, 0.4, 0.05, 0.1, 0.5, 0.6, 0.2),
  stringsAsFactors = FALSE
)

result <- add_eigt_inheritance_estate_view(test_data)

original_rows <- result[result$tax_type_view != eigt_inheritance_estate_tax_view, , drop = FALSE]
expect_identical(
  original_rows$tax_type_view,
  original_rows$tax_type,
  "Original rows should keep their original tax type as the selector view."
)

combined_rows <- result[result$tax_type_view == eigt_inheritance_estate_tax_view, , drop = FALSE]

expect_identical(
  sort(combined_rows$d5_code[combined_rows$GEO == "A"]),
  c("a-inh-1", "a-inh-2"),
  "Inheritance-only groups should use inheritance rows."
)

expect_identical(
  sort(combined_rows$d5_code[combined_rows$GEO == "B"]),
  c("b-est-1", "b-est-2"),
  "Estate-only groups should use estate rows."
)

expect_identical(
  sort(combined_rows$d5_code[combined_rows$GEO == "C"]),
  c("c-inh-1", "c-inh-2"),
  "Groups with both inheritance and estate should prefer inheritance rows."
)

expect_true(
  !any(combined_rows$GEO == "D"),
  "Gift rows should not be duplicated into the combined inheritance/estate view."
)

expect_true(
  setequal(
    unique(result$tax_type_view),
    c(eigt_inheritance_estate_tax_view, eigt_tax_type_choices)
  ),
  "Combined and original tax type selector values should both be available."
)

message("OK: EIGT inheritance/estate combined tax view uses the expected priority.")
