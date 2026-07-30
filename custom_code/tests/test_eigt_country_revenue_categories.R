#!/usr/bin/env Rscript

library(dplyr)

source("custom_code/helpers/eigt_tax_kinship.R")
source("custom_code/helpers/eigt_currency.R")
source("custom_code/helpers/eigt_revenue_categories.R")

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

mapped <- map_eigt_revenue_tax_category(c(
  "EIG Tax, general government level",
  "Gift Tax, general government level",
  "tg",
  "GG",
  "Estate Tax for Children",
  NA_character_
))
expect_identical(
  mapped,
  c(
    eigt_revenue_tax_category_eig,
    eigt_revenue_tax_category_gift,
    eigt_revenue_tax_category_eig,
    eigt_revenue_tax_category_gift,
    NA_character_,
    NA_character_
  ),
  "Revenue mapping should recognize warehouse labels and raw sector codes only."
)

run_wrangler <- function(path, input) {
  env <- new.env(parent = .GlobalEnv)
  env$data <- input
  sys.source(path, envir = env)
  env$data
}

concept_rows <- function(year, label, concepts, values) {
  data.frame(
    GEO = "US",
    GEO_long = "United States",
    year = year,
    d2_sector_lab = label,
    d4_concept_lab = concepts,
    value = values,
    stringsAsFactors = FALSE
  )
}

tax_concepts <- c(
  "Tax Indicator",
  "Top Marginal Rate",
  "Exemption Threshold"
)
revenue_concepts <- c(
  "Total Revenue from Tax",
  "Total Revenue from Tax as % of Total Tax Revenue",
  "Total Revenue from Tax as % of Gross Domestic Product"
)

input <- bind_rows(
  concept_rows(
    2020, "Inheritance Tax for Children", tax_concepts,
    c(1, 10, 1000000)
  ),
  concept_rows(
    2020, "Estate Tax for Everybody", tax_concepts,
    c(0, 20, 2000000)
  ),
  concept_rows(
    2020, "Gift Tax for Children", tax_concepts,
    c(1, 30, 3000000)
  ),
  concept_rows(
    2021, "Inheritance Tax for Everybody", tax_concepts,
    c(1, 11, 1100000)
  ),
  concept_rows(
    2021, "Estate Tax for Children", tax_concepts,
    c(0, 21, 2100000)
  ),
  concept_rows(
    2021, "Gift Tax for Everybody", tax_concepts,
    c(1, 31, 3100000)
  ),
  concept_rows(
    2020, "EIG Tax, general government level", revenue_concepts,
    c(100000000, 5, 0.25)
  ),
  concept_rows(
    2020, "Gift Tax, general government level", revenue_concepts,
    c(20000000, 1, 0.05)
  ),
  concept_rows(
    2021, "EIG Tax, general government level", revenue_concepts,
    c(110000000, 5.5, 0.30)
  )
)

data <- run_wrangler("custom_code/data_prep_eigt2.R", input) %>%
  filter(xrate_lab == "National Currency")

policy <- data %>% filter(d4_concept_lab == "Top Marginal Rate")
expect_identical(
  unique(as.character(policy$tax_category)),
  eigt_tax_type_view_choices,
  "Policy metrics should expose the four ordered legal tax views."
)
expect_true(
  all(!is.na(policy$tax_type_view)) &&
    all(is.na(policy$revenue_tax_category)),
  "Policy rows should populate only the legal tax-view dimension."
)

revenue <- data %>%
  filter(d4_concept_lab == "Total Revenue from Tax") %>%
  arrange(year, revenue_tax_category)
expect_identical(
  unique(as.character(revenue$tax_category)),
  eigt_revenue_tax_category_choices,
  "Revenue metrics should expose aggregate EIG before Gift when available."
)
expect_true(
  all(is.na(revenue$tax_type_view)) &&
    all(!is.na(revenue$revenue_tax_category)),
  "Revenue rows should populate only the warehouse revenue dimension."
)
expect_numeric_equal(
  revenue %>%
    filter(
      year == 2020,
      revenue_tax_category == eigt_revenue_tax_category_eig
    ) %>%
    pull(value),
  100000000,
  "Aggregate EIG revenue should retain its warehouse value."
)
expect_numeric_equal(
  revenue %>%
    filter(
      year == 2020,
      revenue_tax_category == eigt_revenue_tax_category_gift
    ) %>%
    pull(value),
  20000000,
  "Gift revenue should retain its distinct warehouse value."
)
expect_true(
  nrow(revenue %>% filter(
    year == 2021,
    revenue_tax_category == eigt_revenue_tax_category_gift
  )) == 0,
  "Missing Gift revenue should stay unavailable instead of falling back to EIG."
)

duplicates <- data %>%
  count(GEO, year, d4_concept_lab, tax_category, xrate_lab) %>%
  filter(n > 1)
expect_identical(
  nrow(duplicates),
  0L,
  "KF2 should have one observation per feature-dependent chart key."
)

cat("OK: country EIGT revenue categories remain distinct from legal tax views.\n")
