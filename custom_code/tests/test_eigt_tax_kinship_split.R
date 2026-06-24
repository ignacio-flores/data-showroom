#!/usr/bin/env Rscript

source("custom_code/helpers/eigt_tax_kinship.R")

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

labels <- c(
  "Gift Tax for Children",
  "Inheritance Tax for Everybody",
  "Estate Tax, applies to unknown",
  "Gift Tax, general government level",
  "EIG Tax, general government level"
)

split_values <- split_eigt_tax_kinship(labels)

expect_identical(
  split_values$tax_type,
  c("Gift tax", "Inheritance tax", "Estate tax", "Gift tax", NA_character_),
  "Tax type split mismatch."
)

expect_identical(
  split_values$kinship,
  c("Children", "Everybody", "Applies to unknown", "General government level", "General government level"),
  "Kinship split mismatch."
)

message("OK: EIGT tax/kinship split parser handles expected label patterns.")
