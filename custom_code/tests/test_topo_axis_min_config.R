#!/usr/bin/env Rscript

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

read_config <- function(path) {
  yaml::read_yaml(path)
}

expect_x_axis_min <- function(path, expected) {
  config <- read_config(path)
  min_value <- config$axis_vars$x_axis$min
  expect_true(
    length(min_value) == 1 && identical(as.numeric(min_value), as.numeric(expected)),
    paste0(path, " should set axis_vars$x_axis$min to ", expected, ".")
  )
}

expect_no_x_axis_min <- function(path) {
  config <- read_config(path)
  expect_true(
    is.null(config$axis_vars$x_axis$min),
    paste0(path, " should not set axis_vars$x_axis$min.")
  )
}

expect_selected_wealth_type <- function(path, value) {
  config <- read_config(path)
  selector <- config$fixed_selectors$d4_concept_lab
  if (is.null(selector)) {
    selector <- config$loose_selectors$d4_concept_lab
  }
  selected <- selector$selected
  expect_true(
    value %in% selected,
    paste0(path, " should pre-select ", value, ".")
  )
}

default_topo_configs <- file.path(
  "yaml",
  c(
    "config_topo_single.yaml",
    "config_topo_multi.yaml",
    "config_topo_source.yaml",
    "config_topo_prev.yaml"
  )
)

for (path in default_topo_configs) {
  expect_x_axis_min(path, 1900)
  expect_selected_wealth_type(path, "Net Wealth")
}

specialized_topo_configs <- file.path(
  "yaml",
  c(
    "config_topo_aba1.yaml",
    "config_topo_aba2.yaml",
    "config_topo_ffba1.yaml",
    "config_topo_ffba2.yaml",
    "config_topo_ffba3.yaml"
  )
)

for (path in specialized_topo_configs) {
  expect_no_x_axis_min(path)
}

cat("OK: default topo configs set x_axis min and pre-select Net Wealth where requested.\n")
