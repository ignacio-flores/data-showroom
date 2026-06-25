#!/usr/bin/env Rscript

library(yaml)

source("modules/ensureListStructure.R")
source("modules/combine_multiple_columns.R")

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
      "\nExpected: ", paste(expected, collapse = " "),
      "\nActual: ", paste(actual, collapse = " ")
    ))
  }
}

old_ctype <- Sys.getlocale("LC_CTYPE")
new_ctype <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
on.exit({
  if (nzchar(old_ctype)) suppressWarnings(Sys.setlocale("LC_CTYPE", old_ctype))
}, add = TRUE)

expect_true(
  nzchar(new_ctype),
  "Test requires an LC_CTYPE=C locale to reproduce the old UTF-8 byte-marker failure."
)

config <- yaml::read_yaml("yaml/config_inhe_multi.yaml")
env <- new.env(parent = globalenv())
invisible(list2env(config, envir = env))

for (name in c(
  "facet_var", "facet_label_var", "loose_selectors", "new.cols",
  "keep.col", "data.encoding"
)) {
  if (!exists(name, envir = env, inherits = FALSE)) {
    assign(name, NULL, envir = env)
  }
}

env$fixed_selectors <- ensureListStructure(env$fixed_selectors)
env$dt.cols <- ensureListStructure(env$dt.cols)
env$tooltip_vars <- ensureListStructure(env$tooltip_vars)
env$color_var <- env$color$var

source("modules/define_varlists.R", local = env)
source("modules/read_dataset.R", local = env)

brulhart_labels <- unique(env$data[env$data$source == "Brulhart2018", "legend"])
expect_true(length(brulhart_labels) > 0, "Expected Brulhart2018 rows in inheritance data.")

brulhart_label <- brulhart_labels[[1]]
expect_equal(
  Encoding(brulhart_label),
  "UTF-8",
  "CSV source labels should be marked as UTF-8 after loading."
)
expect_equal(
  as.character(charToRaw(brulhart_label))[1:5],
  c("42", "72", "c3", "bc", "6c"),
  "Brulhart label should retain the UTF-8 byte sequence for u-umlaut."
)

display_values <- unique(env$data$legend)
combined_col <- names(config$new.cols)[[1]]
expect_true(
  combined_col %in% names(env$data),
  "Configured combined display label should be created during CSV loading."
)

combined_labels <- unique(env$data[env$data$source == "Brulhart2018", combined_col])
expect_true(length(combined_labels) > 0, "Expected combined Brulhart labels.")
expect_equal(
  Encoding(combined_labels[[1]]),
  "UTF-8",
  "Combined labels derived from CSV source labels should preserve UTF-8."
)

display_values <- c(display_values, combined_labels)
expect_false(
  any(grepl("<[[:xdigit:]]{2}>", display_values, ignore.case = TRUE, useBytes = TRUE)),
  "Display labels should not contain literal byte artifacts such as <c3> or <bc>."
)

message("OK: CSV source labels preserve UTF-8 under LC_CTYPE=C.")
