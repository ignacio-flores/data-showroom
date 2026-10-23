#!/usr/bin/env Rscript

library(dplyr)
library(qs)
library(yaml)
source("modules/create_selectors.R")

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
      "\nExpected: ", paste(expected, collapse = ", "),
      "\nActual: ", paste(actual, collapse = ", ")
    ))
  }
}

expect_set_equal <- function(actual, expected, message) {
  if (!setequal(actual, expected)) {
    fail(paste0(
      message,
      "\nExpected: ", paste(sort(expected), collapse = ", "),
      "\nActual: ", paste(sort(actual), collapse = ", ")
    ))
  }
}

selected_values <- function(selector) {
  selector$selected %||% NULL
}

filter_fixed <- function(data, selectors) {
  for (var in names(selectors)) {
    if (!var %in% names(data)) next
    values <- selected_values(selectors[[var]])
    if (is.null(values) || length(values) == 0) next
    data <- data[data[[var]] %in% values, , drop = FALSE]
  }
  data
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

targets <- yaml::read_yaml("yaml/deploy_targets.yaml")$targets
target_by_id <- setNames(targets, vapply(targets, `[[`, character(1), "target_id"))

expected_targets <- c("eigt-us1", "eigt-us2", "eigt-us3")
expect_true(
  all(expected_targets %in% names(target_by_id)),
  "All US-state EIGT deploy targets should be registered."
)

expected_tags <- list(
  `eigt-us1` = c("eigt", "us", "states", "map", "wm"),
  `eigt-us2` = c("eigt", "us", "states", "trends", "kf"),
  `eigt-us3` = c("eigt", "us", "states", "schedule", "ft")
)

for (target_id in expected_targets) {
  target <- target_by_id[[target_id]]
  expect_equal(target$profile, "glubsprint", paste(target_id, "should use the glubsprint profile."))
  expect_equal(target$app_name, target_id, paste(target_id, "app_name should match target_id."))
  expect_equal(
    target$auth_script,
    "auth/shiny_auth_glubsprint.R",
    paste(target_id, "should use the glubsprint auth script.")
  )
  expect_true(
    all(expected_tags[[target_id]] %in% target$tags),
    paste(target_id, "should include the expected deployment tags.")
  )
}

us1_config <- yaml::read_yaml("yaml/config_eigt_us1.yaml")
us2_config <- yaml::read_yaml("yaml/config_eigt_us2.yaml")
us3_config <- yaml::read_yaml("yaml/config_eigt_us3.yaml")

expect_true(is.null(us1_config$title), "US graph configs should not define visible titles.")
expect_true(is.null(us2_config$title), "US graph configs should not define visible titles.")
expect_true(is.null(us3_config$title), "US graph configs should not define visible titles.")

expect_equal(us1_config$color$var, "state_abbr", "US map should use state abbreviations as locations.")
expect_equal(us1_config$map_options$locationmode, "USA-states", "US map should use Plotly USA-states mode.")
expect_equal(us1_config$map_options$scope, "usa", "US map should use the USA map scope.")

expected_state_concepts <- c(
  "Tax Indicator",
  "Top Marginal Rate",
  "Exemption Threshold",
  "Total Revenue from Tax",
  "Total Revenue from Tax as % of Total Tax Revenue",
  "Total Revenue from Tax as % of Gross Domestic Product"
)

expected_tax_views <- c(
  "Inheritance or estate tax",
  "Inheritance tax",
  "Estate tax",
  "Gift tax"
)

expected_eig_revenue_category <-
  "Estate, inheritance and gift taxes (EIG)"

expect_equal(
  us1_config$fixed_selectors$d4_concept_lab$selected,
  "Total Revenue from Tax as % of Total Tax Revenue",
  "US map should default to total revenue from tax as a share of total tax revenue."
)
expect_equal(
  unlist(us1_config$fixed_selectors$d4_concept_lab$choices, use.names = FALSE),
  expected_state_concepts,
  "US map should expose all six ordered state tax features."
)
expect_equal(
  names(us1_config$fixed_selectors$d4_concept_lab$choices)[[1]],
  "Tax Status",
  "US map should display Tax Status for the tax indicator feature."
)
expect_equal(
  unlist(us2_config$fixed_selectors$d4_concept_lab$choices, use.names = FALSE),
  expected_state_concepts[-1],
  "US trend graph should expose the five ordered tax features without Tax Status."
)
expect_equal(
  names(us1_config$loose_selectors),
  c("year", "tax_category"),
  "US map should expose year and feature-dependent tax-category selectors."
)
expect_equal(
  us1_config$loose_selectors$year$type,
  "reactive selector",
  "US map year should react to the chosen tax feature."
)
expect_equal(
  us1_config$loose_selectors$year$select,
  "latest",
  "US map should select the latest year with data for each feature."
)
expect_equal(
  names(us2_config$loose_selectors),
  "tax_category",
  "US trend graph should expose one feature-dependent tax-category selector."
)
expect_equal(
  us1_config$loose_selectors$tax_category$selected,
  expected_eig_revenue_category,
  "US map should default its revenue feature to aggregate EIG."
)
expect_equal(
  unlist(us2_config$loose_selectors$tax_category$selected, use.names = FALSE),
  expected_eig_revenue_category,
  "US trend graph should default its revenue feature to aggregate EIG."
)
expect_equal(
  us2_config$loose_selectors$tax_category$type,
  "sticky selector",
  "US trend tax category should be a single-value selector."
)
expect_equal(
  us2_config$loose_selectors$tax_category$select,
  "first",
  "US trend tax categories should select the first valid category after a feature change."
)
expect_equal(
  us1_config$fixed_selectors$xrate_lab$selected,
  "USD (2023 prices)",
  "US map should retain the hidden real-USD currency."
)
expect_true(
  isTRUE(us1_config$fixed_selectors$xrate_lab$hidden),
  "US map currency should remain hidden."
)
expect_true(
  isTRUE(us2_config$fixed_selectors$xrate_lab$hidden),
  "US trend currency should remain hidden."
)
expect_equal(
  us1_config$fixed_selectors$show_zero$selected,
  "Yes",
  "US map should retain its show-zero default."
)
expect_equal(
  us2_config$fixed_selectors$show_zero$selected,
  "No",
  "US trend graph should retain its nonzero default."
)
expect_equal(
  us3_config$loose_selectors$kinship,
  NULL,
  "US full schedule should not expose a kinship selector."
)

us_configs <- list(
  "US map" = us1_config,
  "US trend graph" = us2_config
)

for (config_name in names(us_configs)) {
  config <- us_configs[[config_name]]
  surface_vars <- c(
    names(config$fixed_selectors),
    names(config$loose_selectors),
    config$color$group,
    names(config$dt.cols),
    names(config$tooltip_vars)
  )
  expect_false(
    "kinship" %in% surface_vars,
    paste(config_name, "should not expose or group by raw kinship.")
  )
  expect_true(
    "tax_category" %in% names(config$dt.cols) &&
      "tax_category" %in% names(config$tooltip_vars),
    paste(config_name, "should show the active tax category in its table and tooltip.")
  )
  expect_true(
    all(c("tax_type_view", "revenue_tax_category") %in% config$download.cols),
    paste(config_name, "downloads should preserve both category dimensions.")
  )
}

if (file.exists(us1_config$data.file)) {
  long_data <- qs::qread(us1_config$data.file)
  expect_equal(length(unique(long_data$state_abbr)), 51L, "State long data should include 50 states plus DC.")
  expect_true("DC" %in% unique(long_data$state_abbr), "State long data should include DC.")
  expect_true(!"US" %in% unique(long_data$GEO), "State long data should not include country-level US rows.")
  expect_true(
    all(grepl("^US[_-][A-Z]{2}$", unique(long_data$GEO))),
    "State long data should only include US subregion GEO codes."
  )
  expect_true("Alaska" %in% unique(long_data$state_name), "State names should use cleaned state labels.")
  expect_set_equal(
    unique(long_data$d4_concept_lab),
    expected_state_concepts,
    "State long data should contain the six approved state tax features."
  )
  expect_true(
    all(c(
      "tax_category", "tax_type_view", "revenue_tax_category",
      "d2_sector_lab", "tax_type", "kinship", "source", "varcode"
    ) %in% names(long_data)),
    "State long data should retain raw source-tax and relationship provenance."
  )
  if (is.factor(long_data$tax_type_view)) {
    expect_equal(
      levels(long_data$tax_type_view),
      expected_tax_views,
      "Normalized state tax views should retain the approved factor order."
    )
  }
  expect_equal(
    levels(long_data$revenue_tax_category),
    c(expected_eig_revenue_category, "Gift tax"),
    "State revenue categories should use the shared country-compatible order."
  )
  expect_equal(
    levels(long_data$tax_category),
    c(
      "Inheritance or estate tax",
      "Inheritance tax",
      "Estate tax",
      expected_eig_revenue_category,
      "Gift tax"
    ),
    "Feature-dependent state tax categories should preserve both subset orders."
  )

  chart_rows <- long_data %>%
    filter(show_zero == "Yes")

  duplicate_rows <- chart_rows %>%
    count(
      GEO, GEO_long, year, d4_concept_lab, tax_category,
      name = ".rows"
    ) %>%
    filter(.rows > 1)
  expect_equal(
    nrow(duplicate_rows),
    0L,
    "State chart data should have one normalized row per panel/view/feature."
  )

  tax_feature_rows <- chart_rows %>%
    filter(d4_concept_lab %in% expected_state_concepts[1:3])
  expect_set_equal(
    as.character(unique(tax_feature_rows$tax_category)),
    expected_tax_views,
    "State policy features should expose all four legal tax views."
  )
  expect_true(
    all(is.na(tax_feature_rows$revenue_tax_category)),
    "State policy rows should not carry a revenue category."
  )
  canonical_counts <- tax_feature_rows %>%
    filter(tax_type_view != "Inheritance or estate tax") %>%
    distinct(GEO, GEO_long, year, tax_type, kinship) %>%
    count(GEO, GEO_long, year, tax_type, name = ".kinships") %>%
    filter(.kinships > 1)
  expect_equal(
    nrow(canonical_counts),
    0L,
    "Each source tax should use one canonical Children-then-Everybody relationship."
  )

  status_rows <- tax_feature_rows %>%
    filter(d4_concept_lab == "Tax Indicator")
  status_coverage <- status_rows %>%
    group_by(year, tax_type_view) %>%
    summarise(.states = n_distinct(state_abbr), .groups = "drop")
  expect_equal(
    nrow(status_coverage),
    length(unique(status_rows$year)) * length(expected_tax_views),
    "Every state year should expose all three source-tax statuses plus the combined view."
  )
  expect_true(
    all(status_coverage$.states == 51),
    "Every year and normalized tax view should retain all 50 states plus DC."
  )
  expect_true(
    any(status_rows$value > 0, na.rm = TRUE),
    "Normalized status rows should retain states with an active tax."
  )
  expect_true(
    any(status_rows$value == 0, na.rm = TRUE),
    "Normalized status rows should retain explicit zero-tax states."
  )

  revenue_rows <- chart_rows %>%
    filter(d4_concept_lab %in% expected_state_concepts[4:6])
  expect_set_equal(
    unique(revenue_rows$d2_sector_lab),
    "EIG Tax, regional government level",
    "Only the regional EIG revenue series should feed normalized state views."
  )
  expect_set_equal(
    unique(revenue_rows$kinship),
    "Regional government level",
    "State revenue should retain its raw regional-government relationship."
  )
  expect_set_equal(
    as.character(unique(revenue_rows$revenue_tax_category)),
    expected_eig_revenue_category,
    "State revenue should expose only the aggregate EIG category present in the warehouse."
  )
  expect_set_equal(
    as.character(unique(revenue_rows$tax_category)),
    expected_eig_revenue_category,
    "The feature-dependent category should label state revenue as aggregate EIG."
  )
  expect_true(
    all(is.na(revenue_rows$tax_type_view)),
    "State revenue rows should never be assigned to a legal tax view."
  )

  gdp_rows <- revenue_rows %>%
    filter(d4_concept_lab == "Total Revenue from Tax as % of Gross Domestic Product")
  expect_equal(
    max(gdp_rows$year),
    2016,
    "State GDP-share series should end in 2016, its latest year with usable values."
  )
  expect_true(
    any(gdp_rows$value > 0, na.rm = TRUE),
    "State GDP-share series should retain positive observations."
  )
  trend_gdp <- filter_fixed(
    long_data,
    modifyList(us2_config$fixed_selectors, list(
      d4_concept_lab = list(
        selected = "Total Revenue from Tax as % of Gross Domestic Product"
      )
    ))
  ) %>%
    filter(tax_category == expected_eig_revenue_category)
  expect_true(
    nrow(trend_gdp) > 0 && any(trend_gdp$value > 0, na.rm = TRUE),
    "US trend GDP-share selection should produce positive values."
  )
  expect_equal(
    max(trend_gdp$year),
    2016,
    "US trend GDP-share selection should end at the latest usable year."
  )
  expect_equal(
    loose_selector_next_selection(
      "reactive selector",
      sort(unique(gdp_rows$year)),
      current_selection = 2020,
      select_mode = "latest",
      initialized = TRUE,
      refresh_selection = TRUE
    ),
    max(gdp_rows$year),
    "Choosing GDP share should move the map year to 2016."
  )

  us1_start <- filter_fixed(long_data, us1_config$fixed_selectors) %>%
    filter(tax_category == us1_config$loose_selectors$tax_category$selected)
  expect_true(nrow(us1_start) > 0, "eigt-us1 startup filters should produce rows.")

  us2_start <- filter_fixed(long_data, us2_config$fixed_selectors) %>%
    filter(tax_category %in% us2_config$loose_selectors$tax_category$selected)
  expect_true(nrow(us2_start) > 0, "eigt-us2 startup filters should produce rows.")

  for (feature in c("Top Marginal Rate", "Exemption Threshold")) {
    trend_combined <- filter_fixed(
      long_data,
      modifyList(us2_config$fixed_selectors, list(
        d4_concept_lab = list(selected = feature)
      ))
    ) %>%
      filter(
        year == 2020,
        tax_category == "Inheritance or estate tax",
        state_name %in% c("California", "Florida", "Texas")
      )
    expect_set_equal(
      trend_combined$state_name,
      c("California", "Florida", "Texas"),
      paste("US trend combined view should include non-levying states for", feature)
    )
    expect_true(
      all(trend_combined$value > 0) && all(trend_combined$tax_type == "Estate tax"),
      paste("US trend combined view should use the federal estate value for", feature)
    )
  }
} else {
  message("Skipping US-state long artifact checks; ", us1_config$data.file, " is not present.")
}

if (file.exists(us3_config$data.file)) {
  env <- new.env(parent = .GlobalEnv)
  env$data <- qs::qread(us3_config$data.file)
  sys.source(us3_config$data.wrangler, envir = env)
  ft_data <- env$data

  expect_equal(length(unique(ft_data$state_abbr)), 51L, "State FT data should include 50 states plus DC.")
  expect_set_equal(
    unique(ft_data$kinship),
    c("Children", "Everybody"),
    "State FT data should use Everybody when no Children schedule is available."
  )
  duplicate_schedule_variants <- ft_data %>%
    filter(tax_type_view != "Inheritance or estate tax") %>%
    distinct(GEO, year, tax_type, kinship) %>%
    count(GEO, year, tax_type, name = ".kinships") %>%
    filter(.kinships > 1)
  expect_equal(
    nrow(duplicate_schedule_variants),
    0L,
    "Each state-year source tax should use one schedule variant."
  )

  ft_start <- filter_fixed(ft_data, us3_config$fixed_selectors)
  latest_year <- max(suppressWarnings(as.numeric(ft_start$year)), na.rm = TRUE)
  ft_start <- ft_start %>%
    filter(as.numeric(year) == latest_year, !is.na(adjlbo), !is.na(adjmrt))

  expect_true(nrow(ft_start) > 0, "eigt-us3 startup filters should produce drawable schedule rows.")

  default_rates <- ft_start %>%
    group_by(state_name) %>%
    summarise(max_rate = max(adjmrt, na.rm = TRUE), .groups = "drop")

  federal_estate_states <- c("California", "Texas")
  positive_tax_states <- c("Massachusetts", "New York", "Washington")

  expect_true(
    all(federal_estate_states %in% default_rates$state_name),
    "eigt-us3 default should include states without their own estate tax."
  )
  expect_true(
    all(default_rates$max_rate[default_rates$state_name %in% federal_estate_states] > 0),
    "Non-levying default states should show their federal estate schedule."
  )
  federal_default_rows <- ft_start %>%
    filter(state_name %in% federal_estate_states)
  expect_true(
    all(federal_default_rows$tax_indicator == 0) &&
      all(federal_default_rows$tax_type == "Estate tax") &&
      all(federal_default_rows$kinship == "Everybody"),
    "Federal estate brackets should remain visible despite zero own-tax status."
  )
  federal_positive_brackets <- federal_default_rows %>%
    filter(adjmrt > 0)
  expect_set_equal(
    federal_positive_brackets$state_name,
    federal_estate_states,
    "Federal estate rates should remain drawable for both comparison states."
  )
  expect_true(
    all(federal_positive_brackets$adjlbo > 0),
    "Federal estate rates should retain their exemption threshold on the x-axis."
  )
  expect_true(
    all(positive_tax_states %in% default_rates$state_name),
    "eigt-us3 default should include positive-tax comparison states."
  )
  expect_true(
    all(default_rates$max_rate[default_rates$state_name %in% positive_tax_states] > 0),
    "Levying default states should retain positive state-tax schedules."
  )

  latest_combined <- ft_data %>%
    filter(
      tax_type_view == "Inheritance or estate tax",
      as.numeric(year) == latest_year,
      !is.na(adjmrt)
    ) %>%
    group_by(state_name) %>%
    summarise(max_rate = max(adjmrt, na.rm = TRUE), .groups = "drop")

  expect_true(
    all(c("California", "Florida", "Texas") %in% latest_combined$state_name),
    "Non-levying states should remain in the combined schedule view."
  )
  expect_true(
    all(latest_combined$max_rate[
      latest_combined$state_name %in% c("California", "Florida", "Texas")
    ] > 0),
    "California, Florida, and Texas should show the federal estate schedule."
  )
  expect_true(
    all(latest_combined$max_rate[
      latest_combined$state_name %in% c("New York", "Washington")
    ] > 0),
    "New York and Washington should retain positive state schedules."
  )
} else {
  message("Skipping US-state FT artifact checks; ", us3_config$data.file, " is not present.")
}

cat("EIGT US-state checks passed.\n")
