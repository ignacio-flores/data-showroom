library(dplyr)
library(tidyr)

source("custom_code/helpers/eigt_preprocessing.R")
source("custom_code/helpers/eigt_tax_kinship.R")
source("custom_code/helpers/eigt_currency.R")
source("custom_code/helpers/eigt_revenue_categories.R")

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

first_non_na_character <- function(x) {
  vals <- as.character(x)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) {
    return(NA_character_)
  }
  vals[[1]]
}

join_keys <- c("GEO", "GEO_long", "year")

if (!"d2_code" %in% names(data)) {
  data$d2_code <- NA_character_
}

data <- data %>%
  filter(!is.na(year), !is_eigt_subregion_geo(GEO)) %>%
  mutate(
    exempt = normalize_eigt_full_exemption_values(
      suppressWarnings(as.numeric(exempt)),
      concept = "Exemption Threshold"
    ),
    exempt = if_else(!is.na(exempt) & exempt < 0, NA_real_, exempt),
    revenu = suppressWarnings(as.numeric(revenu)),
    revenu = if_else(!is.na(revenu) & revenu < 0, NA_real_, revenu),
    revenue_tax_category = coalesce(
      map_eigt_revenue_tax_category(d2_code),
      map_eigt_revenue_tax_category(d2_label)
    )
  )

# Aggregate EIG and Gift revenue are independent of the legal tax-type panel.
revenue_df <- data %>%
  filter(!is.na(revenue_tax_category)) %>%
  group_by(across(all_of(c(join_keys, "revenue_tax_category")))) %>%
  summarise(
    revenue_d2_code = first_non_na_character(d2_code),
    revenue_d2_label = first_non_na_character(d2_label),
    revenu = first_non_na(revenu),
    prorev = first_non_na(prorev),
    revgdp = first_non_na(revgdp),
    .groups = "drop"
  )

tax_df <- data %>%
  add_eigt_tax_kinship("d2_label") %>%
  filter(kinship %in% eigt_visible_kinship_choices, !is.na(tax_type)) %>%
  group_by(
    across(all_of(join_keys)),
    d2_code,
    d2_label,
    tax_type,
    kinship
  ) %>%
  summarise(
    status = max_non_na(status),
    toprat = max_non_na(toprat),
    exempt = max_non_na(exempt),
    .groups = "drop"
  ) %>%
  select_eigt_canonical_kinship(
    key_cols = c(join_keys, "tax_type"),
    priority = c("Children", "Everybody")
  ) %>%
  mutate(.tax_active = if_else(is.na(status), NA_real_, as.numeric(status > 0))) %>%
  add_eigt_inheritance_estate_view(
    view_col = "tax_type_view",
    key_cols = join_keys,
    active_col = ".tax_active"
  ) %>%
  select(-.tax_active)

# The complete grid is selector scaffolding only. Joins leave unavailable legal
# families or revenue categories missing instead of fabricating observations.
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
  )

# The embedded preview exposes only rates and revenue shares and fixes one
# hidden currency value. Avoid reading the conversion warehouse and expanding
# 11 identical nonmonetary copies for that target.
is_eigt_preview <- exists("graph", inherits = FALSE) &&
  identical(graph, "eigt-prev")

if (is_eigt_preview) {
  preview_currency <- "Euro adjusting for inflation"
  if (exists("fixed_selectors", inherits = FALSE) &&
      !is.null(fixed_selectors$xrate_lab$selected)) {
    preview_currency <- as.character(fixed_selectors$xrate_lab$selected[[1]])
  }
  data <- data %>%
    mutate(xrate_lab = preview_currency)
} else {
  currency_factors <- load_eigt_currency_factors(
    "data/supplementary_var_long.csv"
  )
  data <- expand_eigt_currency_views(
    data,
    factors = currency_factors,
    monetary_cols = c("exempt", "revenu"),
    scale_divisor = 1e6
  )
}

data <- data %>%
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
    GEO_long,
    tax_type_view,
    revenue_tax_category,
    year,
    xrate_lab
  )

duplicate_rows <- data %>%
  count(
    across(all_of(c(
      join_keys,
      "tax_type_view",
      "revenue_tax_category",
      "xrate_lab"
    ))),
    name = ".rows"
  ) %>%
  filter(.rows > 1)

if (nrow(duplicate_rows) > 0) {
  stop("Duplicate EIGT KF1 mixed-axis rows remain after normalization.", call. = FALSE)
}
