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

cross_kinship <- data.frame(
  GEO = c("X", "X"),
  GEO_long = c("Cross", "Cross"),
  year = c("2001", "2001"),
  tax_type = c("Inheritance tax", "Estate tax"),
  kinship = c("Children", "Everybody"),
  active = c(0, 1),
  marker = c("inheritance", "estate"),
  stringsAsFactors = FALSE
)

default_cross <- add_eigt_inheritance_estate_view(cross_kinship)
default_combined <- default_cross[
  default_cross$tax_type_view == eigt_inheritance_estate_tax_view,
  ,
  drop = FALSE
]
expect_identical(
  sort(default_combined$marker),
  c("estate", "inheritance"),
  "The default FT-compatible grouping should keep kinship-specific combined rows."
)

hidden_cross <- add_eigt_inheritance_estate_view(
  cross_kinship,
  key_cols = c("GEO", "GEO_long", "year"),
  active_col = "active",
  kinship_priority = eigt_visible_kinship_choices
)
hidden_combined <- hidden_cross[
  hidden_cross$tax_type_view == eigt_inheritance_estate_tax_view,
  ,
  drop = FALSE
]
expect_identical(
  hidden_combined$marker,
  "estate",
  "Hidden-kinship combined views should use an active estate tax when inheritance is inactive."
)

both_active <- cross_kinship
both_active$active <- 1
both_active_result <- add_eigt_inheritance_estate_view(
  both_active,
  key_cols = c("GEO", "GEO_long", "year"),
  active_col = "active",
  kinship_priority = eigt_visible_kinship_choices
)
both_active_combined <- both_active_result[
  both_active_result$tax_type_view == eigt_inheritance_estate_tax_view,
  ,
  drop = FALSE
]
expect_identical(
  both_active_combined$marker,
  "inheritance",
  "Active inheritance should take priority when both inheritance and estate taxes are active."
)

neither_active <- cross_kinship
neither_active$active <- 0
neither_active_result <- add_eigt_inheritance_estate_view(
  neither_active,
  key_cols = c("GEO", "GEO_long", "year"),
  active_col = "active",
  kinship_priority = eigt_visible_kinship_choices
)
neither_active_combined <- neither_active_result[
  neither_active_result$tax_type_view == eigt_inheritance_estate_tax_view,
  ,
  drop = FALSE
]
expect_identical(
  neither_active_combined$marker,
  "inheritance",
  "Inactive or unavailable status should retain the inheritance-first fallback."
)

status_unavailable <- cross_kinship
status_unavailable$active <- NA_real_

state_metrics <- data.frame(
  GEO = rep("US-CA", 6),
  GEO_long = rep("California", 6),
  year = rep("2020", 6),
  tax_type = rep(c("Inheritance tax", "Estate tax"), 3),
  kinship = rep("Children", 6),
  d4_concept_lab = rep(
    c("Top Marginal Rate", "Exemption Threshold", "Tax Indicator"),
    each = 2
  ),
  value = c(0, 40, 0, 12000000, 0, 0),
  stringsAsFactors = FALSE
)
metric_result <- add_eigt_inheritance_estate_metric_view(
  state_metrics[state_metrics$d4_concept_lab != "Tax Indicator", ],
  key_cols = c("GEO", "GEO_long", "year")
)
metric_combined <- metric_result[
  metric_result$tax_type_view == eigt_inheritance_estate_tax_view,
]
expect_identical(
  metric_combined$tax_type,
  c("Estate tax", "Estate tax"),
  "Combined rate and threshold should use positive estate values despite zero own-tax status."
)
expect_identical(
  metric_combined$value[
    match(c("Top Marginal Rate", "Exemption Threshold"), metric_combined$d4_concept_lab)
  ],
  c(40, 12000000),
  "Combined metric values should remain attached to their source tax."
)

both_metrics <- state_metrics[1:2, ]
both_metrics$value <- c(20, 40)
both_result <- add_eigt_inheritance_estate_metric_view(
  both_metrics,
  key_cols = c("GEO", "GEO_long", "year")
)
both_combined <- both_result[
  both_result$tax_type_view == eigt_inheritance_estate_tax_view,
]
expect_identical(
  both_combined$tax_type,
  "Inheritance tax",
  "When both source metrics are positive, the combined view should retain inheritance priority."
)

split_metrics <- state_metrics[1:4, ]
split_metrics$value <- c(20, 0, 0, 12000000)
split_result <- add_eigt_inheritance_estate_metric_view(
  split_metrics,
  key_cols = c("GEO", "GEO_long", "year")
)
split_combined <- split_result[
  split_result$tax_type_view == eigt_inheritance_estate_tax_view,
]
expect_identical(
  split_combined$tax_type[
    match(c("Top Marginal Rate", "Exemption Threshold"), split_combined$d4_concept_lab)
  ],
  c("Inheritance tax", "Estate tax"),
  "Combined rate and threshold should choose their sources independently."
)

status_unavailable_result <- add_eigt_inheritance_estate_view(
  status_unavailable,
  key_cols = c("GEO", "GEO_long", "year"),
  active_col = "active",
  kinship_priority = eigt_visible_kinship_choices
)
status_unavailable_combined <- status_unavailable_result[
  status_unavailable_result$tax_type_view ==
    eigt_inheritance_estate_tax_view,
  ,
  drop = FALSE
]
expect_identical(
  status_unavailable_combined$marker,
  "inheritance",
  "Unavailable status should retain the inheritance-first fallback."
)

canonical_input <- data.frame(
  GEO = c("Y", "Y", "Y"),
  GEO_long = c("Canonical", "Canonical", "Canonical"),
  year = c("2002", "2002", "2002"),
  tax_type = c("Gift tax", "Gift tax", "Estate tax"),
  kinship = c("Everybody", "Children", "Everybody"),
  marker = c("gift-everybody", "gift-children", "estate-everybody"),
  stringsAsFactors = FALSE
)
canonical_result <- select_eigt_canonical_kinship(canonical_input)
expect_identical(
  sort(canonical_result$marker),
  c("estate-everybody", "gift-children"),
  "Canonical kinship selection should prefer Children and fall back to Everybody."
)

message("OK: EIGT inheritance/estate combined tax view uses the expected priority.")
