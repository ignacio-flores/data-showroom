library(countrycode)
library(data.table)
library(dplyr)
library(qs)

source("custom_code/helpers/eigt_preprocessing.R")
source("custom_code/helpers/eigt_tax_kinship.R")
source("custom_code/helpers/eigt_currency.R")
source("custom_code/helpers/eigt_revenue_categories.R")

input_file <- "data/taxw_warehouse_meta_v2.csv"
output_file <- "data/taxw_wm2_ready.qs"

tax_feature_concepts <- c(
  "Tax Indicator",
  "Top Marginal Rate",
  "Exemption Threshold"
)

revenue_concepts <- c(
  "Total Revenue from Tax",
  "Total Revenue from Tax as % of Total Tax Revenue",
  "Total Revenue from Tax as % of Gross Domestic Product"
)

target_concepts <- c(tax_feature_concepts, revenue_concepts)
monetary_concepts <- c("Exemption Threshold", "Total Revenue from Tax")
panel_keys <- c("GEO", "GEO_long", "year")

first_non_missing <- function(x) {
  values <- x[!is.na(x)]
  if (length(values) == 0) return(NA_real_)
  values[[1]]
}

summarise_map_value <- function(value, concept) {
  values <- suppressWarnings(as.numeric(value))
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NA_real_)

  concept <- first_non_missing(concept)
  if (concept %in% tax_feature_concepts) {
    return(max(values, na.rm = TRUE))
  }

  first_non_missing(values)
}

summarise_tax_active <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (all(is.na(value))) return(NA)
  any(!is.na(value) & value > 0)
}

raw <- data.table::fread(
  input_file,
  select = c("GEO", "GEO_long", "year", "value", "d2_sector_lab", "d4_concept_lab"),
  showProgress = FALSE
) %>%
  as.data.frame() %>%
  filter(
    d4_concept_lab %in% target_concepts,
    !is.na(year),
    GEO != "VE",
    !is_eigt_subregion_geo(GEO)
  ) %>%
  mutate(
    value = suppressWarnings(as.numeric(value)),
    value = normalize_eigt_full_exemption_values(
      value,
      concept = d4_concept_lab
    ),
    value = if_else(!is.na(value) & value < 0, NA_real_, value)
  )

# Tax features use one canonical relationship regime per country-year-tax:
# Children when present, otherwise Everybody. The raw tax/kinship columns remain
# on every row as source provenance.
tax_data <- raw %>%
  filter(d4_concept_lab %in% tax_feature_concepts) %>%
  group_by(
    GEO, GEO_long, year, d4_concept_lab, d2_sector_lab
  ) %>%
  summarise(
    value = summarise_map_value(value, d4_concept_lab),
    .groups = "drop"
  ) %>%
  add_eigt_tax_kinship("d2_sector_lab") %>%
  filter(tax_type %in% eigt_tax_type_choices) %>%
  select_eigt_canonical_kinship(
    key_cols = c(panel_keys, "tax_type"),
    priority = eigt_visible_kinship_choices
  )

# Tax Indicator determines which source tax supplies the combined
# inheritance-or-estate view when status is available.
tax_status <- tax_data %>%
  filter(d4_concept_lab == "Tax Indicator") %>%
  group_by(across(all_of(c(panel_keys, "tax_type")))) %>%
  summarise(
    .tax_active = summarise_tax_active(value),
    .groups = "drop"
  )

tax_views <- tax_data %>%
  left_join(tax_status, by = c(panel_keys, "tax_type")) %>%
  add_eigt_inheritance_estate_view(
    view_col = "tax_type_view",
    key_cols = panel_keys,
    active_col = ".tax_active",
    kinship_priority = eigt_visible_kinship_choices
  ) %>%
  filter(tax_type_view %in% eigt_tax_type_view_choices) %>%
  mutate(
    revenue_tax_category = NA_character_,
    tax_category = as.character(tax_type_view)
  ) %>%
  select(-.tax_active)

# Revenue categories follow the warehouse sectors directly. Aggregate EIG and
# Gift revenue are separate observations and are never assigned to a legal tax
# regime.
revenue_data <- raw %>%
  filter(
    d2_sector_lab %in% names(eigt_revenue_sector_categories),
    d4_concept_lab %in% revenue_concepts
  ) %>%
  group_by(
    GEO, GEO_long, year, d4_concept_lab, d2_sector_lab
  ) %>%
  summarise(
    value = summarise_map_value(value, d4_concept_lab),
    .groups = "drop"
  ) %>%
  add_eigt_tax_kinship("d2_sector_lab") %>%
  add_eigt_revenue_tax_category("d2_sector_lab") %>%
  filter(!is.na(revenue_tax_category)) %>%
  mutate(
    tax_type_view = NA_character_,
    tax_category = revenue_tax_category
  )

panel <- bind_rows(tax_views, revenue_data) %>%
  mutate(
    tax_type_view = factor(
      tax_type_view,
      levels = eigt_tax_type_view_choices,
      ordered = TRUE
    ),
    revenue_tax_category = factor(
      revenue_tax_category,
      levels = eigt_revenue_tax_category_choices,
      ordered = TRUE
    ),
    tax_category = factor(
      tax_category,
      levels = eigt_tax_category_choices,
      ordered = TRUE
    )
  )

currency_factors <- load_eigt_currency_factors(
  "data/supplementary_var_long.csv"
)

data <- expand_eigt_currency_views(
  panel,
  factors = currency_factors,
  monetary_cols = "value",
  monetary_rows = panel$d4_concept_lab %in% monetary_concepts,
  scale_divisor = 1,
  geo_col = "GEO",
  year_col = "year",
  currency_col = "xrate_lab"
) %>%
  mutate(
    iso3 = countrycode(
      dplyr::recode(GEO, UK = "GB", XK = NA_character_),
      origin = "iso2c",
      destination = "iso3c"
    ),
    show_zero = "Yes"
  ) %>%
  filter(!is.na(iso3))

duplicate_rows <- data %>%
  count(
    across(all_of(c(
      panel_keys, "d4_concept_lab", "tax_category", "xrate_lab"
    ))),
    name = ".rows"
  ) %>%
  filter(.rows > 1)

if (nrow(duplicate_rows) > 0) {
  stop("Duplicate EIGT WM2 chart rows remain after normalization.", call. = FALSE)
}

data <- bind_rows(
  data,
  data %>%
    filter(!is.na(value), value != 0) %>%
    mutate(show_zero = "No")
) %>%
  arrange(
    GEO_long, year, d4_concept_lab, tax_category, xrate_lab, show_zero
  )

qs::qsave(data, output_file, preset = "fast")
message("Saved ", nrow(data), " rows to ", output_file)
