#!/usr/bin/env Rscript

source("custom_code/helpers/eigt_currency.R")

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
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

expected_choices <- c(
  "National Currency",
  "National Currency adjusting for inflation",
  "USD",
  "USD adjusting for inflation",
  "Euro",
  "Euro adjusting for inflation",
  "Yuan",
  "Yuan adjusting for inflation",
  "PPP USD",
  "PPP Euro",
  "PPP Yuan"
)
expect_identical(
  eigt_currency_choices,
  expected_choices,
  "EIGT currency choices should preserve the established WM order."
)

factors <- list(
  cpi = data.frame(
    GEO = "A",
    year = 2020,
    value = 2,
    stringsAsFactors = FALSE
  ),
  xrates_current = data.frame(
    GEO = rep("A", 3),
    year = rep(2020, 3),
    xrate_var = c("xlcusx", "xlceux", "xlcyux"),
    value = c(4, 8, 16),
    stringsAsFactors = FALSE
  ),
  xrates_2023 = data.frame(
    GEO = rep("A", 6),
    xrate_var = c("xlcusx", "xlceux", "xlcyux", "xlcusp", "xlceup", "xlcyup"),
    value = c(5, 10, 20, 25, 50, 100),
    stringsAsFactors = FALSE
  )
)

input <- data.frame(
  GEO = c("A", "A", "B", "B"),
  year = c(2020, 2020, 2020, 2020),
  concept = c("money", "rate", "money", "rate"),
  value = c(100, 50, 80, 40),
  other_money = c(200, NA, 160, NA),
  stringsAsFactors = FALSE
)
monetary_rows <- input$concept == "money"

expanded <- expand_eigt_currency_views(
  input,
  factors,
  monetary_cols = c("value", "other_money"),
  monetary_rows = monetary_rows,
  scale_divisor = 10
)

slice <- function(label, geo, concept) {
  expanded[
    expanded$xrate_lab == label &
      expanded$GEO == geo &
      expanded$concept == concept,
    ,
    drop = FALSE
  ]
}

expect_numeric_equal(
  slice("National Currency", "A", "money")$value,
  10,
  "National-currency nominal values should only apply the configured scale."
)
expect_numeric_equal(
  slice("National Currency adjusting for inflation", "A", "money")$value,
  5,
  "Real national-currency values should divide by CPI and scale."
)
expect_numeric_equal(
  slice("USD", "A", "money")$value,
  2.5,
  "Nominal USD should use the current-year market exchange rate."
)
expect_numeric_equal(
  slice("USD adjusting for inflation", "A", "money")$value,
  1,
  "Real USD should use CPI and the fixed 2023 market exchange rate."
)
expect_numeric_equal(
  slice("PPP USD", "A", "money")$value,
  0.2,
  "PPP USD should use CPI, the 2023 PPP rate, and the configured scale."
)

expected_monetary_values <- c(
  "National Currency" = 10,
  "National Currency adjusting for inflation" = 5,
  "USD" = 2.5,
  "USD adjusting for inflation" = 1,
  "Euro" = 1.25,
  "Euro adjusting for inflation" = 0.5,
  "Yuan" = 0.625,
  "Yuan adjusting for inflation" = 0.25,
  "PPP USD" = 0.2,
  "PPP Euro" = 0.1,
  "PPP Yuan" = 0.05
)
for (label in names(expected_monetary_values)) {
  expect_numeric_equal(
    slice(label, "A", "money")$value,
    expected_monetary_values[[label]],
    paste("Monetary conversion should use the established formula for", label)
  )
}

expect_numeric_equal(
  slice("USD adjusting for inflation", "A", "money")$other_money,
  2,
  "Currency expansion should transform every configured monetary column."
)

for (label in expected_choices) {
  expect_numeric_equal(
    slice(label, "A", "rate")$value,
    50,
    paste("Nonmonetary values should be invariant for", label)
  )
}

expect_true(
  is.na(slice("USD", "B", "money")$value),
  "Missing exchange rates should make only monetary converted values unavailable."
)
expect_true(
  is.na(slice("National Currency adjusting for inflation", "B", "money")$value),
  "Missing CPI should make real monetary values unavailable."
)
expect_numeric_equal(
  slice("National Currency", "B", "money")$value,
  8,
  "Nominal local-currency values should not require CPI or exchange rates."
)
expect_numeric_equal(
  slice("USD adjusting for inflation", "B", "rate")$value,
  40,
  "Missing currency factors should not affect nonmonetary values."
)

message("OK: EIGT currency expansion preserves established conversion semantics.")
