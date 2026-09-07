#!/usr/bin/env Rscript

library(data.table)

source("custom_code/helpers/warehouse_dashboard_slices.R")
source("tools/viz/entrypoint.R")

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

expect_error <- function(expr, pattern, message) {
  err <- tryCatch({
    force(expr)
    NULL
  }, error = function(e) e)
  if (is.null(err) || !grepl(pattern, conditionMessage(err), fixed = TRUE)) {
    fail(message)
  }
}

spec <- warehouse_dashboard_slice_spec()
fixture <- data.table(
  d1_dashboard = spec$dashboard,
  d1_dashboard_lab = spec$dashboard_label,
  GEO = c("GB", "FR", "US", "IT"),
  value = seq_len(nrow(spec))
)
input_file <- tempfile("warehouse-meta-", fileext = ".csv")
output_dir <- tempfile("warehouse-slices-")
fwrite(fixture, input_file)

outputs <- write_warehouse_dashboard_slices(input_file, output_dir)
expect_true(
  identical(basename(outputs), spec$output) && all(file.exists(outputs)),
  "The slice writer should create each configured dashboard output."
)

for (idx in seq_len(nrow(spec))) {
  output <- fread(outputs[[idx]])
  expect_true(
    nrow(output) == 1L &&
      identical(as.character(output$d1_dashboard), spec$dashboard[[idx]]) &&
      identical(names(output), names(fixture)),
    "Each output should retain only its dashboard rows and all source columns."
  )
}

invalid <- copy(fixture)
invalid$d1_dashboard_lab[[1]] <- "Unexpected label"
expect_error(
  validate_warehouse_dashboard_slices(invalid),
  "unknown or mismatched dashboard values",
  "The slice writer should reject an upstream dashboard taxonomy change."
)

slice_recipe <- list(
  name = "slices",
  inputs = "data/warehouse_meta_v2.csv",
  outputs = "data/inhe_warehouse_meta_v2.csv"
)
derived_recipe <- list(
  name = "derived",
  inputs = "data/inhe_warehouse_meta_v2.csv",
  outputs = "data/chart_ready.qs"
)
ordered <- order_recipes_by_dependencies(list(derived = derived_recipe, slices = slice_recipe))
expect_true(
  identical(unname(vapply(ordered, `[[`, character(1), "name")), c("slices", "derived")),
  "Nested preparation recipes should run upstream slices before derived artifacts."
)

manifest <- load_data_sources("yaml/deploy_data_sources.yaml")
entry <- list(config_file = "yaml/config_eigt_wm1.yaml")
requirements <- collect_preparation_requirements(list(entry), manifest)
expect_true(
  identical(
    unname(vapply(requirements$recipes, `[[`, character(1), "name")),
    c("dashboard_warehouse_slices", "eigt_wm_ready")
  ),
  "EIGT preparation should run dashboard slicing before its chart-ready recipe."
)
expect_true(
  "data/warehouse_meta_v2.csv" %in% requirements$direct_files,
  "The canonical warehouse should be the sole direct warehouse source."
)
expect_true(
  !"data/warehouse_meta_v2.csv" %in% target_bundle_files(entry, manifest),
  "The canonical warehouse must remain outside the published app bundle."
)

message("Warehouse dashboard slice tests passed.")
