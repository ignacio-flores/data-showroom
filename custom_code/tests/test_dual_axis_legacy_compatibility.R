#!/usr/bin/env Rscript

source("modules/libraries.R")

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

fixture <- expand.grid(
  year = c(2020L, 2021L),
  d3_vartype_lab = c("Ratio", "Aggregate", "Rate"),
  stringsAsFactors = FALSE
)
fixture <- fixture %>%
  dplyr::mutate(
    GEO = "FR",
    GEO_long = "France",
    source = "Guzzardi2026",
    legend = "Guzzardi and Morelli (2026)",
    d4_concept_lab = dplyr::if_else(
      d3_vartype_lab == "Rate",
      "Average Effective Tax Rate",
      "Inheritances & Gifts"
    ),
    d5_dboard_specific_lab = "Economic Flow",
    value = dplyr::case_when(
      d3_vartype_lab == "Ratio" ~ 1.5 + (year - 2020) / 10,
      d3_vartype_lab == "Aggregate" ~ 100 + (year - 2020) * 5,
      TRUE ~ 12 + (year - 2020)
    )
  )

fixture_path <- tempfile("inhe-dual-", fileext = ".csv")
on.exit(unlink(fixture_path), add = TRUE)
utils::write.csv(fixture, fixture_path, row.names = FALSE)

config <- yaml::read_yaml("yaml/config_inhe_dual.yaml")
expect_true(
  is.null(config$dual_axis_options),
  "The inheritance dual-axis chart should remain opted out of compact hover."
)
config$data.file <- fixture_path

args <- config[
  intersect(names(config), names(formals(createViz)))
]
args$graph <- "inhe_dual"
app <- do.call(createViz, args)

expect_true(
  inherits(app, "shiny.appobj"),
  "The legacy inheritance dual-axis app should construct from its unchanged config."
)

cat("OK: legacy dual-axis config and app construction remain compatible.\n")
