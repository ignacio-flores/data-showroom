#!/usr/bin/env Rscript

source("modules/create_selectors.R")

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

read_config <- function(path) {
  yaml::read_yaml(path)
}

expect_standard_inhe_without_rate <- function(path) {
  config <- read_config(path)
  choices <- selector_config_choices(config$fixed_selectors$d3_vartype_lab)
  expect_true(
    identical(choices, c("Ratio", "Aggregate")),
    paste0(path, " should expose only Ratio and Aggregate in the inheritance variable selector.")
  )
  expect_true(
    !"Rate" %in% choices,
    paste0(path, " should not expose Rate in the main inheritance visualization.")
  )
}

for (path in file.path(
  "yaml",
  c("config_inhe_single.yaml", "config_inhe_multi.yaml", "config_inhe_prev.yaml")
)) {
  expect_standard_inhe_without_rate(path)
}

dual_config <- read_config("yaml/config_inhe_dual.yaml")
dual_y_choices <- selector_parse_choices(dual_config$axis_vars$y_axis$choices)
dual_y2_choices <- selector_parse_choices(dual_config$axis_vars$y2_axis$choices)

expect_true(
  "average_effective_tax_rate" %in% dual_y_choices,
  "Dual-axis inheritance left axis should keep average_effective_tax_rate available."
)
expect_true(
  "average_effective_tax_rate" %in% dual_y2_choices,
  "Dual-axis inheritance right axis should keep average_effective_tax_rate available."
)

cat("OK: inheritance Rate is hidden from standard views and retained for dual-axis views.\n")
