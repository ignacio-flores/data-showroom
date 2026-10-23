#!/usr/bin/env Rscript

library(shiny)
source("modules/PlotServer.R")

status_data <- data.frame(
  d4_concept_lab = c("Tax Indicator", "Tax Indicator"),
  value = c(0, 1)
)
stopifnot(is_eigt_status_map(status_data, "value"))
stopifnot(!is_eigt_status_map(
  transform(status_data, d4_concept_lab = "Top Marginal Rate"),
  "value"
))

colors <- eigt_status_map_colors()
stopifnot(identical(names(colors), c("No", "Yes")))
stopifnot(!identical(colors[["No"]], colors[["Yes"]]))
stopifnot(identical(format_eigt_tooltip_value("Tax Indicator", "d4_concept_lab"),
                    "Tax Status"))
stopifnot(identical(format_eigt_tooltip_value("0", "value", "Tax Indicator"),
                    "No"))
stopifnot(identical(format_eigt_tooltip_value("1", "value", "Tax Indicator"),
                    "Yes"))
stopifnot(identical(format_eigt_tooltip_value(
  "0.00042", "value",
  "Total Revenue from Tax as % of Gross Domestic Product"
), "0.00042%"))

shiny::testServer(
  plotModuleServer,
  args = list(
    filtered_data_func = shiny::reactive(data.frame(
      state_abbr = c("CA", "TX"),
      state_name = c("California", "Texas"),
      year = c(2020, 2020),
      d4_concept_lab = c("Tax Indicator", "Tax Indicator"),
      tax_category = c("Estate tax", "Estate tax"),
      value = c(1, 0)
    )),
    x_var = NULL, x_var_lab = NULL,
    y_var = "value", y_var_lab = "Tax Status",
    color_var = "state_abbr", color_var_lab = "State",
    facet_var = NULL, facet_var_lab = NULL,
    tooltip_vars = list(
      state_name = "State:",
      d4_concept_lab = "Tax feature:",
      value = "Value:"
    ),
    hide.legend = FALSE, gopts = "map", xnum_breaks = NULL,
    extra_layer = NULL, color_style = "rcartocolor::BluGrn",
    plot_height = 700, groupvars = NULL,
    map_options = list(locationmode = "USA-states", scope = "usa",
                       projection = "albers usa")
  ),
  {
    payload <- jsonlite::fromJSON(output$valuePlot, simplifyVector = FALSE)
    hover <- unlist(lapply(payload$x$data, `[[`, "text"), use.names = FALSE)
    stopifnot(any(grepl("Tax feature: *Tax Status", hover)))
    stopifnot(any(grepl("Value: *Yes", hover)))
    stopifnot(any(grepl("Value: *No", hover)))
  }
)

message("OK: binary EIGT tax status map uses two labeled colors.")
