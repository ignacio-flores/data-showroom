#!/usr/bin/env Rscript

library(shiny)
source("modules/PlotServer.R")
source("custom_code/helpers/ineq_source_units.R")

raw <- data.frame(
  year = c(2009, 2014, 2009, 2014, 2009, 2014, 2009, 2014, 2009),
  source = c("HFCS_ineq", "HFCS_ineq", "WID_ineq", "WID_ineq",
             "HFCS_ineq", "HFCS_ineq", "WID_ineq", "WID_ineq",
             "HFCS_ineq"),
  d3_vartype_lab = c(rep("Share of Total Net Wealth", 4),
                     rep("Gini Coefficient", 4),
                     "Share of Total Net Wealth"),
  d4_concept_lab = "Net Wealth",
  percentile = c(rep("p0p50", 8), "p0p100"),
  value = c(544.789429, 535.105164, 5.1, 5.3,
            0.61, 0.62, 72.1, 73.2, 10000)
)
data <- normalize_ineq_source_units(raw)
share <- subset(data, d3_vartype_lab == "Share of Total Net Wealth" &
                  percentile == "p0p50")
gini <- subset(data, d3_vartype_lab == "Gini Coefficient")
selection <- reactiveVal(share)

check_axis <- function(payload, title, percent, max_tick) {
  axis <- payload$x$layout$yaxis
  stopifnot(identical(axis$title$text, title))
  ticks <- as.numeric(unlist(axis$tickvals))
  labels <- as.character(unlist(axis$ticktext))
  stopifnot(length(ticks) > 0L, max(ticks) <= max_tick)
  stopifnot(all(grepl("%$", labels) == percent))
}

testServer(
  plotModuleServer,
  args = list(
    filtered_data_func = reactive(selection()),
    x_var = "year", x_var_lab = "Year",
    y_var = "value", y_var_lab = "Value",
    color_var = "source", color_var_lab = "Source",
    facet_var = NULL, facet_var_lab = NULL,
    tooltip_vars = list(year = "Year:", value = "Value:"),
    hide.legend = FALSE, gopts = c("line", "point"), xnum_breaks = 5,
    extra_layer = NULL, color_style = "dichromat::DarkRedtoBlue_12",
    plot_height = 400, groupvars = "source"
  ),
  {
    read_plot <- function() jsonlite::fromJSON(output$valuePlot,
                                                simplifyVector = FALSE)
    check_axis(read_plot(), "Share of Total Net Wealth", TRUE, 20)
    selection(gini)
    session$flushReact()
    check_axis(read_plot(), "Gini Coefficient", FALSE, 100)
    selection(share)
    session$flushReact()
    check_axis(read_plot(), "Share of Total Net Wealth", TRUE, 20)
  }
)

cat("OK: inequality comparison axis remains correct after switching variables.\n")
