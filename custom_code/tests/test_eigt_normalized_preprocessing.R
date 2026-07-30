#!/usr/bin/env Rscript

library(dplyr)

source("custom_code/helpers/eigt_tax_kinship.R")
source("custom_code/helpers/eigt_currency.R")
source("custom_code/helpers/eigt_revenue_categories.R")

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
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

expect_numeric_equal <- function(actual, expected, message) {
  if (!isTRUE(all.equal(
    as.numeric(actual),
    as.numeric(expected),
    check.attributes = FALSE
  ))) {
    fail(paste0(
      message,
      "\nExpected: ", paste(expected, collapse = ", "),
      "\nActual: ", paste(actual, collapse = ", ")
    ))
  }
}

run_wrangler <- function(path, input) {
  env <- new.env(parent = .GlobalEnv)
  env$data <- input
  sys.source(path, envir = env)
  env$data
}

run_preview_wrangler <- function(path, input, config) {
  env <- new.env(parent = .GlobalEnv)
  env$data <- input
  env$graph <- "eigt-prev"
  env$fixed_selectors <- config$fixed_selectors
  sys.source(path, envir = env)
  env$data
}

tax_panel_rows <- function(year, label, indicator, rate, exemption) {
  data.frame(
    GEO = "US",
    GEO_long = "United States",
    year = year,
    d2_sector_lab = label,
    d4_concept_lab = c(
      "Tax Indicator",
      "Top Marginal Rate",
      "Exemption Threshold"
    ),
    value = c(indicator, rate, exemption),
    stringsAsFactors = FALSE
  )
}

revenue_panel_rows <- function(year, label, revenue, share_tax, share_gdp) {
  data.frame(
    GEO = "US",
    GEO_long = "United States",
    year = year,
    d2_sector_lab = label,
    d4_concept_lab = c(
      "Total Revenue from Tax",
      "Total Revenue from Tax as % of Total Tax Revenue",
      "Total Revenue from Tax as % of Gross Domestic Product"
    ),
    value = c(revenue, share_tax, share_gdp),
    stringsAsFactors = FALSE
  )
}

revenue_concepts <- c(
  "Total Revenue from Tax",
  "Total Revenue from Tax as % of Total Tax Revenue",
  "Total Revenue from Tax as % of Gross Domestic Product"
)

long_input <- bind_rows(
  # 2020 uses active Estate for the combined view. The lower-priority
  # Everybody inheritance panel must not displace the Children panel.
  tax_panel_rows(2020, "Inheritance Tax for Children", 0, 10, 1000000),
  tax_panel_rows(2020, "Inheritance Tax for Everybody", 1, 99, 9900000),
  tax_panel_rows(2020, "Estate Tax for Everybody", 1, 20, 2000000),
  tax_panel_rows(2020, "Gift Tax for Children", 1, 30, 3000000),
  # 2021 switches the canonical relationship and the active combined source.
  tax_panel_rows(2021, "Inheritance Tax for Everybody", 1, 11, 1100000),
  tax_panel_rows(2021, "Estate Tax for Children", 0, 21, 2100000),
  tax_panel_rows(2021, "Gift Tax for Everybody", 1, 31, 3100000),
  revenue_panel_rows(
    2020,
    "EIG Tax, general government level",
    100000000,
    5,
    0.25
  ),
  revenue_panel_rows(
    2021,
    "EIG Tax, general government level",
    110000000,
    5.5,
    0.30
  ),
  # This series must never feed the contextual revenue panel.
  revenue_panel_rows(
    2020,
    "Gift Tax, general government level",
    999000000,
    99,
    9.9
  ),
  revenue_panel_rows(
    2021,
    "Gift Tax, general government level",
    999000000,
    99,
    9.9
  )
)

kf2 <- run_wrangler("custom_code/data_prep_eigt2.R", long_input)

expect_true(
  all(c(
    "d2_sector_lab", "tax_type", "tax_type_view", "kinship",
    "revenue_tax_category", "tax_category"
  ) %in% names(kf2)),
  "KF2 should retain raw provenance and separate policy/revenue categories."
)
expect_equal(
  unique(stats::na.omit(as.character(kf2$tax_type_view))),
  eigt_tax_type_view_choices,
  "KF2 should produce the four normalized tax views in the approved order."
)
expect_equal(
  unique(stats::na.omit(as.character(kf2$revenue_tax_category))),
  eigt_revenue_tax_category_choices,
  "KF2 should produce aggregate EIG and Gift revenue in warehouse order."
)
expect_equal(
  levels(kf2$tax_category),
  eigt_tax_category_choices,
  "KF2 should use the shared feature-dependent category order."
)
expect_equal(
  as.character(unique(kf2$xrate_lab)),
  eigt_currency_choices,
  "KF2 should produce the shared 11 currency views in the approved order."
)
expect_equal(
  sort(unique(kf2$d4_concept_lab)),
  sort(c(
    "Top Marginal Rate",
    "Exemption Threshold",
    "Total Revenue from Tax",
    "Total Revenue from Tax as % of Total Tax Revenue",
    "Total Revenue from Tax as % of Gross Domestic Product"
  )),
  "KF2 should retain the normalized five-feature panel."
)

kf2_duplicates <- kf2 %>%
  count(
    GEO, year, d4_concept_lab, tax_category, xrate_lab,
    name = ".rows"
  ) %>%
  filter(.rows > 1)
expect_equal(
  nrow(kf2_duplicates),
  0L,
  "KF2 should have one row per geography/year/feature/category/currency."
)

kf2_national <- kf2 %>%
  filter(xrate_lab == "National Currency")

expect_numeric_equal(
  kf2_national %>%
    filter(
      year == 2020,
      tax_type_view == "Inheritance tax",
      d4_concept_lab == "Top Marginal Rate"
    ) %>%
    pull(value),
  10,
  "KF2 should prefer Children over Everybody for one source tax and year."
)
expect_numeric_equal(
  kf2_national %>%
    filter(
      year == 2020,
      tax_type_view == "Inheritance or estate tax",
      d4_concept_lab == "Top Marginal Rate"
    ) %>%
    pull(value),
  20,
  "KF2 should use an active Estate tax when Inheritance is inactive."
)
expect_numeric_equal(
  kf2_national %>%
    filter(
      year == 2021,
      tax_type_view == "Inheritance or estate tax",
      d4_concept_lab == "Top Marginal Rate"
    ) %>%
    pull(value),
  11,
  "KF2 should switch the combined trend to active Inheritance without a break."
)

combined_years <- kf2_national %>%
  filter(
    tax_type_view == "Inheritance or estate tax",
    d4_concept_lab == "Top Marginal Rate"
  ) %>%
  arrange(year) %>%
  pull(year)
expect_equal(
  combined_years,
  c(2020, 2021),
  "KF2 should keep one continuous combined series across kinship/source switches."
)

expect_true(
  kf2_national %>%
    filter(d4_concept_lab %in% revenue_concepts) %>%
    pull(tax_type_view) %>%
    is.na() %>%
    all(),
  "KF2 revenue rows should never be assigned to a legal tax regime."
)
expect_numeric_equal(
  kf2_national %>%
    filter(
      d4_concept_lab == "Total Revenue from Tax",
      revenue_tax_category == eigt_revenue_tax_category_eig
    ) %>%
    arrange(year) %>%
    pull(value),
  c(100000000, 110000000),
  "KF2 should retain aggregate EIG revenue under its warehouse category."
)
expect_numeric_equal(
  kf2_national %>%
    filter(
      d4_concept_lab == "Total Revenue from Tax",
      revenue_tax_category == eigt_revenue_tax_category_gift
    ) %>%
    arrange(year) %>%
    pull(value),
  c(999000000, 999000000),
  "KF2 should retain the distinct Gift-general-government revenue series."
)
expect_true(
  kf2_national %>%
    filter(d4_concept_lab %in% revenue_concepts) %>%
    count(GEO, year, d4_concept_lab, revenue_tax_category) %>%
    pull(n) %>%
    identical(rep(1L, 12)),
  "KF2 should not repeat revenue categories across legal tax views."
)

kf2_rates <- kf2 %>%
  filter(
    year == 2020,
    tax_type_view == "Estate tax",
    d4_concept_lab == "Top Marginal Rate"
  )
expect_true(
  n_distinct(kf2_rates$value) == 1,
  "KF2 currency expansion should leave nonmonetary rates unchanged."
)

kf3 <- run_wrangler("custom_code/data_prep_eigt3.R", long_input)
expect_true(
  all(c(
    "d2_sector_lab",
    "tax_type",
    "kinship",
    "revenue_d2_sector_lab"
  ) %in% names(kf3)),
  "KF3 should retain raw legal-tax, kinship, and revenue provenance."
)
kf3_duplicates <- kf3 %>%
  count(
    geo,
    year,
    tax_type_view,
    revenue_tax_category,
    xrate_lab,
    name = ".rows"
  ) %>%
  filter(.rows > 1)
expect_equal(
  nrow(kf3_duplicates),
  0L,
  "KF3 should have one row per geography/year/legal view/revenue category/currency."
)
expect_equal(
  as.character(unique(kf3$xrate_lab)),
  eigt_currency_choices,
  "KF3 should use the shared 11 currency views."
)
expect_numeric_equal(
  kf3 %>%
    filter(
      year == 2020,
      tax_type_view == "Inheritance or estate tax",
      revenue_tax_category == eigt_revenue_tax_category_eig,
      xrate_lab == "National Currency"
    ) %>%
    pull(top_marginal_rate),
  20,
  "KF3 should use the active Estate panel for the 2020 combined view."
)
expect_numeric_equal(
  kf3 %>%
    filter(
      year == 2021,
      tax_type_view == "Inheritance or estate tax",
      revenue_tax_category == eigt_revenue_tax_category_eig,
      xrate_lab == "National Currency"
    ) %>%
    pull(top_marginal_rate),
  11,
  "KF3 should use the active Inheritance panel for the 2021 combined view."
)
expect_numeric_equal(
  kf3 %>%
    filter(
      year == 2020,
      revenue_tax_category == eigt_revenue_tax_category_eig,
      xrate_lab == "National Currency"
    ) %>%
    pull(total_revenue_from_tax) %>%
    unique(),
  100,
  "KF3 should expose aggregate EIG revenue independently and scale it to millions."
)
expect_numeric_equal(
  kf3 %>%
    filter(
      year == 2020,
      revenue_tax_category == eigt_revenue_tax_category_gift,
      xrate_lab == "National Currency"
    ) %>%
    pull(total_revenue_from_tax) %>%
    unique(),
  999,
  "KF3 should retain the distinct Gift revenue category."
)
expect_equal(
  kf3 %>%
    filter(year == 2020, xrate_lab == "National Currency") %>%
    count(tax_type_view, revenue_tax_category) %>%
    nrow(),
  length(eigt_tax_type_view_choices) *
    length(eigt_revenue_tax_category_choices),
  "KF3 should build the complete 4-by-2 mixed-axis selector grid."
)

kf3_union <- run_wrangler(
  "custom_code/data_prep_eigt3.R",
  bind_rows(
    tax_panel_rows(
      2019,
      "Gift Tax for Children",
      1,
      29,
      2900000
    ),
    revenue_panel_rows(
      2022,
      "EIG Tax, general government level",
      120000000,
      6,
      0.35
    )
  )
)
kf3_union_national <- kf3_union %>%
  filter(xrate_lab == "National Currency")
expect_true(
  kf3_union_national %>%
    filter(year == 2019, tax_type_view == "Gift tax") %>%
    summarise(
      .legal_present = all(top_marginal_rate == 29),
      .revenue_missing = all(is.na(total_revenue_from_tax))
    ) %>%
    with(.legal_present && .revenue_missing),
  "KF3 legal-only years should keep legal values and leave both revenue categories NA."
)
expect_true(
  kf3_union_national %>%
    filter(
      year == 2022,
      revenue_tax_category == eigt_revenue_tax_category_eig
    ) %>%
    summarise(
      .legal_missing = all(is.na(top_marginal_rate)),
      .revenue_present = all(total_revenue_from_tax == 120)
    ) %>%
    with(.legal_missing && .revenue_present),
  "KF3 revenue-only years should keep revenue values and leave all legal views NA."
)

wide_tax_row <- function(
    year,
    label,
    status,
    rate,
    exemption) {
  d2_code <- if (grepl("^Inheritance Tax", label)) {
    "ic"
  } else if (grepl("^Estate Tax", label)) {
    "ec"
  } else {
    "gc"
  }
  data.frame(
    GEO = "US",
    GEO_long = "United States",
    year = year,
    d2_code = d2_code,
    d2_label = label,
    status = status,
    toprat = rate,
    exempt = exemption,
    revenu = NA_real_,
    prorev = NA_real_,
    revgdp = NA_real_,
    stringsAsFactors = FALSE
  )
}

wide_revenue_row <- function(year, label, revenue, share_tax, share_gdp) {
  d2_code <- if (identical(label, "EIG Tax, general government level")) {
    "tg"
  } else {
    "gg"
  }
  data.frame(
    GEO = "US",
    GEO_long = "United States",
    year = year,
    d2_code = d2_code,
    d2_label = label,
    status = NA_real_,
    toprat = NA_real_,
    exempt = NA_real_,
    revenu = revenue,
    prorev = share_tax,
    revgdp = share_gdp,
    stringsAsFactors = FALSE
  )
}

wide_input <- bind_rows(
  wide_tax_row(2020, "Inheritance Tax for Children", 0, 10, 1000000),
  wide_tax_row(2020, "Inheritance Tax for Everybody", 1, 99, 9900000),
  wide_tax_row(2020, "Estate Tax for Everybody", 1, 20, 2000000),
  wide_tax_row(2020, "Gift Tax for Children", 1, 30, 3000000),
  wide_tax_row(2021, "Inheritance Tax for Everybody", 1, 11, 1100000),
  wide_tax_row(2021, "Estate Tax for Children", 0, 21, 2100000),
  wide_tax_row(2021, "Gift Tax for Everybody", 1, 31, 3100000),
  wide_revenue_row(
    2020,
    "EIG Tax, general government level",
    100000000,
    5,
    0.25
  ),
  wide_revenue_row(
    2021,
    "EIG Tax, general government level",
    110000000,
    5.5,
    0.30
  ),
  wide_revenue_row(
    2020,
    "Gift Tax, general government level",
    999000000,
    99,
    9.9
  ),
  wide_revenue_row(
    2021,
    "Gift Tax, general government level",
    999000000,
    99,
    9.9
  )
)

kf1 <- run_wrangler(
  "custom_code/data_prep_eigt_kf1_dual.R",
  wide_input
)
expect_true(
  all(c(
    "d2_label",
    "tax_type",
    "kinship",
    "revenue_d2_label"
  ) %in% names(kf1)),
  "KF1 should retain raw legal-tax, kinship, and revenue provenance."
)
kf1_duplicates <- kf1 %>%
  count(
    GEO,
    year,
    tax_type_view,
    revenue_tax_category,
    xrate_lab,
    name = ".rows"
  ) %>%
  filter(.rows > 1)
expect_equal(
  nrow(kf1_duplicates),
  0L,
  "KF1 should have one row per geography/year/legal view/revenue category/currency."
)
expect_equal(
  as.character(unique(kf1$xrate_lab)),
  eigt_currency_choices,
  "KF1 should use the shared 11 currency views."
)
expect_numeric_equal(
  kf1 %>%
    filter(
      year == 2020,
      tax_type_view == "Inheritance or estate tax",
      revenue_tax_category == eigt_revenue_tax_category_eig,
      xrate_lab == "National Currency"
    ) %>%
    pull(toprat),
  20,
  "KF1 should use the active Estate panel for the 2020 combined view."
)
expect_numeric_equal(
  kf1 %>%
    filter(
      year == 2021,
      tax_type_view == "Inheritance or estate tax",
      revenue_tax_category == eigt_revenue_tax_category_eig,
      xrate_lab == "National Currency"
    ) %>%
    pull(toprat),
  11,
  "KF1 should use the active Inheritance panel for the 2021 combined view."
)
expect_numeric_equal(
  kf1 %>%
    filter(
      year == 2020,
      revenue_tax_category == eigt_revenue_tax_category_eig,
      xrate_lab == "National Currency"
    ) %>%
    pull(revenu) %>%
    unique(),
  100,
  "KF1 should expose aggregate EIG revenue independently and display it in millions."
)
expect_numeric_equal(
  kf1 %>%
    filter(
      year == 2020,
      revenue_tax_category == eigt_revenue_tax_category_gift,
      xrate_lab == "National Currency"
    ) %>%
    pull(revenu) %>%
    unique(),
  999,
  "KF1 should retain the distinct Gift revenue category."
)
expect_equal(
  kf1 %>%
    filter(year == 2020, xrate_lab == "National Currency") %>%
    count(tax_type_view, revenue_tax_category) %>%
    nrow(),
  length(eigt_tax_type_view_choices) *
    length(eigt_revenue_tax_category_choices),
  "KF1 should build the complete 4-by-2 mixed-axis selector grid."
)

kf1_union <- run_wrangler(
  "custom_code/data_prep_eigt_kf1_dual.R",
  bind_rows(
    wide_tax_row(
      2019,
      "Gift Tax for Children",
      1,
      29,
      2900000
    ),
    wide_revenue_row(
      2022,
      "EIG Tax, general government level",
      120000000,
      6,
      0.35
    )
  )
)
kf1_union_national <- kf1_union %>%
  filter(xrate_lab == "National Currency")
expect_true(
  kf1_union_national %>%
    filter(year == 2019, tax_type_view == "Gift tax") %>%
    summarise(
      .legal_present = all(toprat == 29),
      .revenue_missing = all(is.na(revenu))
    ) %>%
    with(.legal_present && .revenue_missing),
  "KF1 legal-only years should keep legal values and leave both revenue categories NA."
)
expect_true(
  kf1_union_national %>%
    filter(
      year == 2022,
      revenue_tax_category == eigt_revenue_tax_category_eig
    ) %>%
    summarise(
      .legal_missing = all(is.na(toprat)),
      .revenue_present = all(revenu == 120)
    ) %>%
    with(.legal_missing && .revenue_present),
  "KF1 revenue-only years should keep revenue values and leave all legal views NA."
)

preview_config <- yaml::read_yaml("yaml/config_eigt-prev.yaml")
preview_data <- run_preview_wrangler(
  "custom_code/data_prep_eigt_kf1_dual.R",
  wide_input,
  preview_config
)
expect_equal(
  as.character(unique(preview_data$xrate_lab)),
  preview_config$fixed_selectors$xrate_lab$selected,
  "eigt-prev should materialize only its one hidden currency view."
)
preview_startup <- preview_data %>%
  filter(
    GEO_long == preview_config$fixed_selectors$GEO_long$selected,
    tax_type_view == preview_config$fixed_selectors$tax_type_view$selected,
    revenue_tax_category ==
      preview_config$fixed_selectors$revenue_tax_category$selected,
    xrate_lab == preview_config$fixed_selectors$xrate_lab$selected
  )
expect_equal(
  nrow(preview_startup),
  n_distinct(preview_startup$year),
  "eigt-prev hidden fixed selectors should produce one row per year."
)

cat("OK: normalized EIGT KF preprocessing contracts are preserved.\n")
