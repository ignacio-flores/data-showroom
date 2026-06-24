#!/usr/bin/env Rscript

library(data.table)
library(yaml)

source("modules/ensureListStructure.R")

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

config_path <- "yaml/config_ineq_single.yaml"
config <- yaml::read_yaml(config_path)

env <- new.env(parent = globalenv())
invisible(list2env(config, envir = env))

for (name in c("facet_var", "facet_label_var", "loose_selectors", "new.cols")) {
  if (!exists(name, envir = env, inherits = FALSE)) {
    assign(name, NULL, envir = env)
  }
}

env$fixed_selectors <- ensureListStructure(env$fixed_selectors)
env$dt.cols <- ensureListStructure(env$dt.cols)
env$tooltip_vars <- ensureListStructure(env$tooltip_vars)
env$color_var <- env$color$var

source("modules/define_varlists.R", local = env)

loose_vars <- names(env$loose_selectors)
expect_true(
  all(loose_vars %in% env$all_vars),
  paste(
    "Loose selector variables should be part of the configured load set:",
    paste(setdiff(loose_vars, env$all_vars), collapse = ", ")
  )
)

data_cols <- names(data.table::fread(env$data.file, sep = ",", nrows = 0))
source_loose_vars <- intersect(loose_vars, data_cols)
loaded_source_cols <- intersect(env$all_vars, data_cols)

expect_true(
  all(source_loose_vars %in% loaded_source_cols),
  paste(
    "Source-backed loose selector variables should be selected from the data file:",
    paste(setdiff(source_loose_vars, loaded_source_cols), collapse = ", ")
  )
)

expect_true(
  identical(env$loose_selectors$d4_concept_lab$type, "selector"),
  "config_ineq_single d4_concept_lab should remain a single-choice loose selector."
)

message("OK: config_ineq_single loose selector columns are preserved for loading.")
