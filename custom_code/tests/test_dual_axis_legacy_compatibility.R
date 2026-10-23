#!/usr/bin/env Rscript

source("modules/libraries.R")
source("modules/value_transform.R")

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
      d3_vartype_lab == "Ratio" ~ 0.15 + (year - 2020) / 100,
      d3_vartype_lab == "Aggregate" ~ 1e11 + (year - 2020) * 5e9,
      TRUE ~ 0.02 + (year - 2020) / 100
    )
  )

fixture_path <- tempfile("inhe-dual-", fileext = ".csv")
on.exit(unlink(fixture_path), add = TRUE)
utils::write.csv(fixture, fixture_path, row.names = FALSE)

config <- yaml::read_yaml("yaml/config_inhe_dual.yaml")
expect_true(
  identical(config$dual_axis_options$hover$mode, "compact") &&
    is.null(config$tooltip_vars),
  "The inheritance dual-axis chart should show each selected metric once."
)
config$data.file <- fixture_path
bundle_path <- tempfile("inhe-currency-", fileext = ".qs")
on.exit(unlink(bundle_path), add = TRUE)
bundle <- list(
  currency_choices = currency_unit_currency_specs()$label,
  cpi = data.frame(GEO = "FR", year = c(2020L, 2021L), cpi = c(0.5, 0.25)),
  xrates_current = data.frame(GEO = "FR", year = c(2020L, 2021L),
                              xrate_var = "xlceux", xrate = 2),
  xrates_2023 = data.frame(GEO = "FR", xrate_var = "xlceux", xrate = 4)
)
qs::qsave(bundle, bundle_path)
config$value_transform$bundle.file <- bundle_path

currency_titles <- c(
  "Local currency, nominal prices" = "Inheritances & Gifts: LCU, nominal",
  "Local currency (2023 prices)" = "Inheritances & Gifts: LCU, 2023 prices",
  "Euro, Market Exchange rate, nominal prices" =
    "Inheritances & Gifts: Euro, nominal",
  "Euro (2023 prices)" = "Inheritances & Gifts: Euro (2023 prices)"
)
for (currency in names(currency_titles)) {
  presentation <- axis_display_presentation(
    data.frame(
      value = 1e11, d3_vartype_lab = "Aggregate",
      d4_concept_lab = "Inheritances & Gifts", xrate_lab = currency
    ),
    "value", "Value", list()
  )
  expect_true(
    identical(presentation$label, unname(currency_titles[[currency]])) &&
      identical(
        presentation$axis_title,
        sub(": ", "<br>", unname(currency_titles[[currency]]), fixed = TRUE)
      ),
    paste("Aggregate y-axis title should be concise for", currency)
  )
}
dual_presentation <- axis_display_presentation(
  data.frame(inheritances_gifts_aggregate = 1e11,
             xrate_lab = "Local currency, nominal prices"),
  "inheritances_gifts_aggregate", "Inheritances & gifts aggregate", list()
)
expect_true(
  identical(dual_presentation$label,
            "Inheritances & gifts: LCU, nominal") &&
    identical(dual_presentation$axis_title,
              "Inheritances & gifts<br>LCU, nominal"),
  "The dual-axis aggregate should use the same concise currency label."
)

for (preset in c("single", "multi", "prev")) {
  other <- yaml::read_yaml(paste0("yaml/config_inhe_", preset, ".yaml"))
  expect_true(
    identical(other$value_transform$apply_when$column, "d3_vartype_lab") &&
      identical(other$value_transform$apply_when$equals, "Aggregate") &&
      identical(other$loose_selectors$xrate_lab$selected,
                "Local currency, nominal prices"),
    paste("The", preset, "inheritance preset should convert only aggregates.")
  )
}

converted <- materialize_currency_columns(
  fixture, bundle, "Euro (2023 prices)", columns = "value",
  apply_when = list(column = "d3_vartype_lab", equals = "Aggregate")
)
expect_true(
  identical(converted$value[fixture$d3_vartype_lab == "Ratio"],
            fixture$value[fixture$d3_vartype_lab == "Ratio"]) &&
    identical(converted$value[fixture$d3_vartype_lab == "Rate"],
              fixture$value[fixture$d3_vartype_lab == "Rate"]) &&
    isTRUE(all.equal(
      converted$value[fixture$d3_vartype_lab == "Aggregate"],
      c(5e10, 1.05e11)
    )),
  "Changing currency should convert aggregates and preserve ratios and rates."
)

args <- config[
  intersect(names(config), names(formals(createViz)))
]
args$graph <- "inhe_dual"
app <- do.call(createViz, args)

expect_true(
  inherits(app, "shiny.appobj"),
  "The inheritance dual-axis app should construct with compact hover and currency choices."
)

shiny::testServer(app$serverFuncSource(), {
  session$setInputs(
    GEO_long = "France", xrate_lab = "Local currency, nominal prices",
    y_axis = "inheritances_gifts_ratio",
    y2_axis = "average_effective_tax_rate"
  )
  plot <- jsonlite::fromJSON(output[["plotModule-valuePlot"]],
                             simplifyVector = FALSE)
  hover <- unlist(lapply(plot$x$data, `[[`, "hovertemplate"), use.names = FALSE)
  expect_true(
    length(plot$x$data) == 3L &&
      any(grepl("15%", hover, fixed = TRUE)) &&
      any(grepl("2%", hover, fixed = TRUE)) &&
      !any(grepl("aggregate|Source:|Year:", hover, ignore.case = TRUE)),
    "Ratio/rate hover should show only selected, percentage-formatted metrics and country."
  )
})

shiny::testServer(app$serverFuncSource(), {
  session$setInputs(
    GEO_long = "France", xrate_lab = "Euro (2023 prices)",
    y_axis = "inheritances_gifts_aggregate",
    y2_axis = "average_effective_tax_rate"
  )
  plot <- jsonlite::fromJSON(output[["plotModule-valuePlot"]],
                             simplifyVector = FALSE)
  hover <- unlist(lapply(plot$x$data, `[[`, "hovertemplate"), use.names = FALSE)
  left_title <- plot$x$layout$yaxis$title
  if (is.list(left_title)) left_title <- left_title$text
  expect_true(
    identical(left_title, "Inheritances & gifts<br>Euro (2023 prices)") &&
      any(grepl("50B", hover, fixed = TRUE)) &&
      any(grepl("2%", hover, fixed = TRUE)) &&
      sum(grepl("Euro (2023 prices)", hover, fixed = TRUE)) == 2L,
    "Aggregate hover should use the selected currency once per year and retain the rate unit."
  )
})

cat("OK: inheritance dual-axis config and app construction remain compatible.\n")
