library(dplyr)
library(tidyr)

source("custom_code/helpers/eigt_preprocessing.R")
source("custom_code/helpers/eigt_tax_kinship.R")
source("custom_code/helpers/eigt_currency.R")
source("custom_code/helpers/eigt_revenue_categories.R")

tax_concepts <- c(
  "Tax Indicator",
  "Top Marginal Rate",
  "Exemption Threshold"
)
revenue_concepts <- c(
  "Total Revenue from Tax",
  "Total Revenue from Tax as % of Total Tax Revenue",
  "Total Revenue from Tax as % of Gross Domestic Product"
)
display_columns <- c(
  "top_marginal_rate",
  "exemption_threshold",
  "total_revenue_from_tax",
  "total_revenue_from_tax_as_percent_of_total_tax_revenue",
  "total_revenue_from_tax_as_percent_of_gross_domestic_product"
)
concept_codes <- c(
  "Tax Indicator" = "tax_indicator",
  "Top Marginal Rate" = "top_marginal_rate",
  "Exemption Threshold" = "exemption_threshold",
  "Total Revenue from Tax" = "total_revenue_from_tax",
  "Total Revenue from Tax as % of Total Tax Revenue" =
    "total_revenue_from_tax_as_percent_of_total_tax_revenue",
  "Total Revenue from Tax as % of Gross Domestic Product" =
    "total_revenue_from_tax_as_percent_of_gross_domestic_product"
)

first_non_na <- function(x) {
  vals <- suppressWarnings(as.numeric(x))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(NA_real_)
  }
  vals[[1]]
}

max_non_na <- function(x) {
  vals <- suppressWarnings(as.numeric(x))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(NA_real_)
  }
  max(vals)
}

ensure_numeric_columns <- function(data, columns) {
  for (column in setdiff(columns, names(data))) {
    data[[column]] <- NA_real_
  }
  data
}

join_keys <- c("GEO", "GEO_long", "year")

if (!"d2_sector" %in% names(data)) {
  data$d2_sector <- NA_character_
}

data <- data %>%
  filter(
    d4_concept_lab %in% c(tax_concepts, revenue_concepts),
    !is.na(year),
    !is_eigt_subregion_geo(GEO)
  ) %>%
  mutate(
    value = normalize_eigt_full_exemption_values(
      suppressWarnings(as.numeric(value)),
      concept = d4_concept_lab
    ),
    value = if_else(!is.na(value) & value < 0, NA_real_, value),
    revenue_tax_category = coalesce(
      map_eigt_revenue_tax_category(d2_sector),
      map_eigt_revenue_tax_category(d2_sector_lab)
    )
  ) %>%
  add_eigt_tax_kinship("d2_sector_lab")

revenue_df <- data %>%
  filter(
    !is.na(revenue_tax_category),
    d4_concept_lab %in% revenue_concepts
  ) %>%
  group_by(across(all_of(c(
    join_keys,
    "revenue_tax_category",
    "d2_sector",
    "d2_sector_lab",
    "d4_concept_lab"
  )))) %>%
  summarise(value = first_non_na(value), .groups = "drop") %>%
  mutate(.concept_code = unname(concept_codes[d4_concept_lab])) %>%
  select(-d4_concept_lab) %>%
  pivot_wider(
    names_from = .concept_code,
    values_from = value
  ) %>%
  ensure_numeric_columns(display_columns[3:5]) %>%
  rename(
    revenue_d2_sector = d2_sector,
    revenue_d2_sector_lab = d2_sector_lab
  )

tax_df <- data %>%
  filter(
    d4_concept_lab %in% tax_concepts,
    kinship %in% eigt_visible_kinship_choices,
    !is.na(tax_type)
  ) %>%
  group_by(
    across(all_of(c(
      join_keys,
      "d2_sector",
      "d2_sector_lab",
      "tax_type",
      "kinship",
      "d4_concept_lab"
    )))
  ) %>%
  summarise(value = max_non_na(value), .groups = "drop") %>%
  mutate(.concept_code = unname(concept_codes[d4_concept_lab])) %>%
  select(-d4_concept_lab) %>%
  pivot_wider(
    names_from = .concept_code,
    values_from = value
  ) %>%
  ensure_numeric_columns(c(
    "tax_indicator",
    "top_marginal_rate",
    "exemption_threshold"
  )) %>%
  select_eigt_canonical_kinship(
    key_cols = c(join_keys, "tax_type"),
    priority = c("Children", "Everybody")
  ) %>%
  mutate(
    .tax_active = if_else(
      is.na(tax_indicator),
      NA_real_,
      as.numeric(tax_indicator > 0)
    )
  ) %>%
  add_eigt_inheritance_estate_view(
    view_col = "tax_type_view",
    key_cols = join_keys,
    active_col = ".tax_active"
  ) %>%
  select(-.tax_active)

# Build the complete independent-selector grid across the union of legal and
# revenue country-years. Missing family/category values intentionally stay NA.
panel_keys <- bind_rows(
  tax_df %>% select(all_of(join_keys)),
  revenue_df %>% select(all_of(join_keys))
) %>%
  distinct()

data <- crossing(
  panel_keys,
  tax_type_view = eigt_tax_type_view_choices,
  revenue_tax_category = eigt_revenue_tax_category_choices
) %>%
  left_join(
    tax_df,
    by = c(join_keys, "tax_type_view")
  ) %>%
  left_join(
    revenue_df,
    by = c(join_keys, "revenue_tax_category")
  ) %>%
  ensure_numeric_columns(display_columns)

currency_factors <- load_eigt_currency_factors(
  "data/supplementary_var_long.csv"
)

data <- expand_eigt_currency_views(
  data,
  factors = currency_factors,
  monetary_cols = c("exemption_threshold", "total_revenue_from_tax"),
  scale_divisor = 1e6
) %>%
  rename(
    geo = GEO,
    geo_long = GEO_long
  ) %>%
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
    xrate_lab = factor(
      xrate_lab,
      levels = eigt_currency_choices,
      ordered = TRUE
    )
  ) %>%
  arrange(
    year,
    geo_long,
    tax_type_view,
    revenue_tax_category,
    xrate_lab
  )

duplicate_rows <- data %>%
  count(
    geo,
    geo_long,
    year,
    tax_type_view,
    revenue_tax_category,
    xrate_lab,
    name = ".rows"
  ) %>%
  filter(.rows > 1)

if (nrow(duplicate_rows) > 0) {
  stop("Duplicate EIGT KF3 mixed-axis rows remain after normalization.", call. = FALSE)
}
