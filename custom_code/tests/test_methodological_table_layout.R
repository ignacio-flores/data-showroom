#!/usr/bin/env Rscript

source("modules/metaTableServer.R")

metadata <- readxl::read_excel("data/methodological_table.xlsx")
metadata <- dplyr::rename(metadata, Country = country)
metadata <- dplyr::select(metadata, -dplyr::any_of(c("area", "GEO3", "Source")))

owr_sources <- c(
  "Demographic and Health Surveys (own estimates)",
  "Living Standards Measurement Study (own estimates)",
  "WAEMU Harmonized Household Living Conditions Surveys (own estimates)"
)
owr_cases <- list(
  ineq_owr1 = metadata[metadata$GEO == "ML" & metadata$Legend %in% owr_sources, ],
  ineq_owr2 = metadata[
    metadata$GEO %in% c("SN", "NG", "ET", "TZ", "IN", "MW", "KH") &
      metadata$Legend == owr_sources[[1]],
  ]
)

for (graph in names(owr_cases)) {
  original <- owr_cases[[graph]]
  visible <- methodological_visible_data(original, graph)
  stopifnot(nrow(original) > 0L, ncol(original) == 25L, ncol(visible) == 15L)
  stopifnot(identical(names(visible), names(original)[names(original) %in% names(visible)]))
  stopifnot(all(c("Country", "Legend", "References", "Link") %in% names(visible)))
  stopifnot(!"Definition of wealth" %in% names(visible))

  lengths <- vapply(visible, function(column) {
    if (!is.character(column)) return(0)
    max(nchar(column), na.rm = TRUE)
  }, numeric(1))
  moderate <- names(lengths[lengths > 20 & lengths <= 100])
  extreme <- names(lengths[lengths > 100])
  rules <- methodological_column_defs(visible, graph, moderate, extreme)
  widget <- DT::datatable(visible, rownames = FALSE, options = list(columnDefs = rules))
  rendered_rules <- widget$x$options$columnDefs
  stopifnot(identical(rendered_rules[[1]]$targets, 1L))
  stopifnot(identical(rendered_rules[[1]]$width, "360px"))
  numeric_targets <- unlist(lapply(rendered_rules, function(rule) {
    if (is.numeric(rule$targets)) rule$targets else integer(0)
  }))
  stopifnot(all(numeric_targets >= 0L & numeric_targets < ncol(visible)))
}

with_wealth <- owr_cases$ineq_owr2
with_wealth[["Definition of wealth"]][[1]] <- "Ownership definition"
stopifnot("Definition of wealth" %in%
            names(methodological_visible_data(with_wealth, "ineq_owr2")))
stopifnot(identical(
  names(methodological_visible_data(owr_cases$ineq_owr1, "ineq_single")),
  names(metadata)
))

multi_rules <- methodological_column_defs(metadata, "ineq_multi", "Legend", character(0))
multi_widget <- DT::datatable(metadata[1, ], rownames = FALSE,
                              options = list(columnDefs = multi_rules))
stopifnot(identical(multi_widget$x$options$columnDefs[[1]]$targets, 1L))
stopifnot(identical(multi_widget$x$options$columnDefs[[1]]$width, "360px"))

cat("Methodological table layout checks passed.\n")
