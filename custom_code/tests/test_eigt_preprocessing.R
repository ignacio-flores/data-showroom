#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(yaml)

source("custom_code/helpers/eigt_preprocessing.R")

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

expect_numeric_equal <- function(actual, expected, message) {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  if (!isTRUE(all.equal(actual, expected, check.attributes = FALSE))) {
    fail(paste0(
      message,
      "\nExpected: ", paste(expected, collapse = ", "),
      "\nActual: ", paste(actual, collapse = ", ")
    ))
  }
}

expect_true(
  is_eigt_subregion_geo("US_NC"),
  "Underscore country-subregion GEO codes should be treated as subregions."
)
expect_true(
  is_eigt_subregion_geo("US-NC"),
  "Dash country-subregion GEO codes should be treated as subregions."
)
expect_false(
  is_eigt_subregion_geo("US"),
  "Country-level GEO codes should not be treated as subregions."
)
expect_false(
  is_eigt_subregion_geo(NA_character_),
  "Missing GEO codes should not be treated as subregions."
)

kf2_config <- yaml::read_yaml("yaml/config_eigt_kf2.yaml")
expect_true(
  "GEO" %in% kf2_config$keep.col,
  "eigt-kf2 should retain GEO for subregion filtering in its wrangler."
)

normalized <- normalize_eigt_full_exemption_values(
  c(-997, -998, -997, -5),
  concept = c(
    "Exemption Threshold",
    "Exemption Threshold",
    "Upper Bound for Exemption-adjusted Tax Bracket",
    "Total Revenue from Tax"
  )
)
expect_numeric_equal(
  normalized,
  c(0, -998, -997, -5),
  "Only exemption-threshold -997 values should be normalized to zero."
)

wide_like <- data.frame(
  GEO = c("US", "US_NC", "FR", "DE"),
  varcode = c(
    "x-ic-thr-exempt-00",
    "x-ic-thr-exempt-00",
    "x-ic-thr-adjubo-01",
    "x-gg-tax-revenu-00"
  ),
  value = c(-997, -997, -997, -5),
  stringsAsFactors = FALSE
) %>%
  filter(!is_eigt_subregion_geo(GEO)) %>%
  separate(varcode, into = c("d1_code", "d2_code", "d3_code", "d4_code", "d5_code"), sep = "-") %>%
  mutate(value = normalize_eigt_full_exemption_values(value, d4_code = d4_code)) %>%
  pivot_wider(
    id_cols = GEO,
    names_from = d4_code,
    values_from = value
  ) %>%
  normalize_eigt_wide_exemptions()

expect_false(
  "US_NC" %in% wide_like$GEO,
  "Wide preprocessing should remove subregion GEO rows."
)
expect_numeric_equal(
  wide_like$exempt[wide_like$GEO == "US"],
  0,
  "Wide exemption sentinel values should become zero."
)
expect_numeric_equal(
  wide_like$adjubo[wide_like$GEO == "FR"],
  -997,
  "Wide adjubo sentinel values should not be normalized as exemptions."
)
expect_numeric_equal(
  wide_like$revenu[wide_like$GEO == "DE"],
  -5,
  "Negative revenue values should not be changed by the exemption helper."
)

wm_like <- data.frame(
  GEO = c("US", "US_NC", "FR"),
  d4_concept_lab = c(
    "Exemption Threshold",
    "Exemption Threshold",
    "Exemption Threshold"
  ),
  value = c(-997, -997, -998),
  stringsAsFactors = FALSE
) %>%
  filter(!is_eigt_subregion_geo(GEO)) %>%
  mutate(
    value = suppressWarnings(as.numeric(value)),
    value = normalize_eigt_full_exemption_values(value, concept = d4_concept_lab),
    value = if_else(!is.na(value) & value < 0, NA_real_, value)
  )

expect_false(
  "US_NC" %in% wm_like$GEO,
  "WM preprocessing should remove subregion GEO rows."
)
expect_numeric_equal(
  wm_like$value[wm_like$GEO == "US"],
  0,
  "WM preprocessing should keep full exemptions as drawable zero values."
)
expect_true(
  is.na(wm_like$value[wm_like$GEO == "FR"]),
  "Non-full-exemption negative sentinel values should keep existing NA cleanup behavior."
)

cat("EIGT preprocessing checks passed.\n")
