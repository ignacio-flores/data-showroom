library(data.table)
library(dplyr)
library(qs)
library(tidyr)

source("custom_code/helpers/eigt_preprocessing.R")
source("custom_code/helpers/eigt_tax_kinship.R")
source("custom_code/helpers/eigt_revenue_categories.R")

input_file <- "data/taxw_warehouse_meta_v2.csv"
long_output_file <- "data/taxw_us_state_long.qs"
ft_output_file <- "data/taxw_us_state_ft_wide.qs"

usd_2023_label <- "USD (2023 prices)"

tax_feature_concepts <- c(
  "Tax Indicator",
  "Top Marginal Rate",
  "Exemption Threshold"
)

state_revenue_concepts <- c(
  "Total Revenue from Tax",
  "Total Revenue from Tax as % of Total Tax Revenue"
)

target_concepts <- c(tax_feature_concepts, state_revenue_concepts)
revenue_concepts <- state_revenue_concepts
status_concept <- "Tax Indicator"
schedule_tax_types <- c("Gift tax", "Inheritance tax", "Estate tax")
inheritance_estate_tax_types <- c("Inheritance tax", "Estate tax")
state_panel_keys <- c("GEO", "GEO_long", "year")

monetary_concepts <- c(
  "Exemption Threshold",
  "Total Revenue from Tax"
)

state_schedule_labels <- data.frame(
  tax_type = c(
    "Gift tax", "Gift tax",
    "Estate tax", "Estate tax",
    "Inheritance tax", "Inheritance tax"
  ),
  kinship = c(
    "Children", "Everybody",
    "Children", "Everybody",
    "Children", "Everybody"
  ),
  d2_code = c("gc", "ge", "ec", "ee", "ic", "ie"),
  d2_label = c(
    "Gift Tax for Children",
    "Gift Tax for Everybody",
    "Estate Tax for Children",
    "Estate Tax for Everybody",
    "Inheritance Tax for Children",
    "Inheritance Tax for Everybody"
  ),
  stringsAsFactors = FALSE
)

state_lookup <- data.frame(
  state_abbr = c(state.abb, "DC"),
  state_name = c(state.name, "District of Columbia"),
  stringsAsFactors = FALSE
)

is_us_state_geo <- function(geo) {
  geo <- as.character(geo)
  !is.na(geo) & grepl("^US[-_][A-Z]{2}$", geo)
}

add_state_fields <- function(data) {
  data %>%
    mutate(state_abbr = sub("^US[-_]", "", GEO)) %>%
    left_join(state_lookup, by = "state_abbr") %>%
    filter(!is.na(state_name))
}

first_non_missing <- function(x) {
  vals <- x[!is.na(x)]
  if (length(vals) == 0) return(NA_real_)
  vals[[1]]
}

summarise_state_value <- function(value, concept) {
  vals <- suppressWarnings(as.numeric(value))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_real_)

  concept <- first_non_missing(concept)
  if (concept %in% tax_feature_concepts) {
    return(max(vals, na.rm = TRUE))
  }

  first_non_missing(vals)
}

first_non_missing_character <- function(x) {
  values <- as.character(x)
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NA_character_)
  values[[1]]
}

summarise_tax_active <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (all(is.na(value))) return(NA)
  any(!is.na(value) & value > 0)
}

load_us_cpi <- function() {
  data.table::fread(
    "data/supplementary_var_long.csv",
    select = c("country", "year", "variable", "value"),
    showProgress = FALSE
  ) %>%
    filter(country == "US", variable == "inyixx") %>%
    transmute(year = as.numeric(year), cpi = suppressWarnings(as.numeric(value)))
}

adjust_usd_2023_value <- function(value, concept, cpi) {
  value <- suppressWarnings(as.numeric(value))
  case_when(
    !concept %in% monetary_concepts ~ value,
    is.na(value) ~ NA_real_,
    is.na(cpi) | cpi <= 0 ~ NA_real_,
    TRUE ~ value / cpi
  )
}

first_schedule_typtax <- function(x) {
  values <- unique(x[!is.na(x)])
  if (length(values) == 0) {
    return(NA_real_)
  }
  if (length(values) > 1) {
    stop("Multiple typtax values found for one EIGT state FT schedule.", call. = FALSE)
  }
  as.numeric(values[[1]])
}

is_drawable_ft_schedule <- function(typtax, exempt, adjmrt) {
  schedule_typtax <- first_schedule_typtax(typtax)
  normal_schedule <- !is.na(schedule_typtax) && schedule_typtax %in% c(2, 4)
  full_exemption_schedule <- any(!is.na(exempt) & exempt == 0) &&
    any(!is.na(adjmrt) & adjmrt == 0)

  normal_schedule || full_exemption_schedule
}

build_state_schedule_status <- function(raw) {
  state_years <- raw %>%
    group_by(GEO, GEO_long, state_abbr, state_name, year) %>%
    summarise(source = first_non_missing(as.character(source)), .groups = "drop")

  status_observed <- raw %>%
    filter(d4_concept_lab == status_concept) %>%
    mutate(value = suppressWarnings(as.numeric(value))) %>%
    add_eigt_tax_kinship("d2_sector_lab") %>%
    filter(
      tax_type %in% schedule_tax_types,
      kinship %in% eigt_visible_kinship_choices
    ) %>%
    group_by(GEO, year, tax_type, kinship) %>%
    summarise(
      tax_indicator = max(if_else(!is.na(value) & value > 0, 1, 0), na.rm = TRUE),
      status_source = first_non_missing(as.character(source)),
      .groups = "drop"
    )

  merge(state_years, state_schedule_labels, by = NULL) %>%
    left_join(status_observed, by = c("GEO", "year", "tax_type", "kinship")) %>%
    mutate(
      tax_indicator = if_else(is.na(tax_indicator), 0, tax_indicator),
      source = coalesce(status_source, source)
    ) %>%
    select(-status_source)
}

create_zero_schedule_rows <- function(status_rows, reference_cols) {
  if (nrow(status_rows) == 0) {
    out <- data.frame(matrix(nrow = 0, ncol = length(reference_cols)))
    names(out) <- reference_cols
    return(out)
  }

  zero_brackets <- data.frame(
    d5_code = c(1, 2),
    status = c(0, 0),
    typtax = c(4, 4),
    adjmrt = c(0, 0),
    toprat = c(0, 0),
    adjlbo = c(0, 1),
    adjubo = c(1, NA_real_),
    exempt = c(0, NA_real_),
    stringsAsFactors = FALSE
  )

  zero_rows <- merge(status_rows, zero_brackets, by = NULL) %>%
    mutate(
      tax_indicator = 0,
      xrate_lab = usd_2023_label
    )

  for (col in setdiff(reference_cols, names(zero_rows))) {
    zero_rows[[col]] <- NA
  }
  zero_rows[, reference_cols, drop = FALSE]
}

apply_state_tax_indicator_to_schedules <- function(ft_data, status_lookup) {
  ft_data <- ft_data %>%
    add_eigt_tax_kinship("d2_label") %>%
    left_join(
      status_lookup %>%
        select(GEO, year, tax_type, kinship, tax_indicator),
      by = c("GEO", "year", "tax_type", "kinship")
    ) %>%
    mutate(
      tax_indicator = if_else(
        tax_type %in% schedule_tax_types & is.na(tax_indicator),
        0,
        tax_indicator
      )
    )

  inherited_or_estate_levy <- status_lookup %>%
    filter(tax_type %in% inheritance_estate_tax_types) %>%
    group_by(GEO, year, kinship) %>%
    summarise(has_inheritance_or_estate_tax = any(tax_indicator == 1), .groups = "drop")

  inheritance_estate_zero_rows <- status_lookup %>%
    filter(tax_type == "Estate tax") %>%
    left_join(inherited_or_estate_levy, by = c("GEO", "year", "kinship")) %>%
    filter(!has_inheritance_or_estate_tax) %>%
    select(-has_inheritance_or_estate_tax)

  gift_zero_rows <- status_lookup %>%
    filter(tax_type == "Gift tax", tax_indicator == 0)

  zero_schedule_rows <- bind_rows(
    inheritance_estate_zero_rows,
    gift_zero_rows
  ) %>%
    create_zero_schedule_rows(names(ft_data))

  ft_data %>%
    filter(!tax_type %in% schedule_tax_types | tax_indicator == 1) %>%
    bind_rows(zero_schedule_rows)
}

cpi <- load_us_cpi()

raw <- data.table::fread(
  input_file,
  select = c(
    "GEO", "GEO_long", "year", "source", "varcode", "value",
    "d2_sector_lab", "d4_concept_lab"
  ),
  showProgress = FALSE
) %>%
  as.data.frame() %>%
  filter(is_us_state_geo(GEO), !is.na(year)) %>%
  add_state_fields() %>%
  mutate(GEO_long = state_name)

schedule_status <- build_state_schedule_status(raw)

# Tax features use one canonical relationship regime per state-year-tax:
# Children when present, otherwise Everybody. Raw tax and kinship fields remain
# on the artifact as source provenance even though US1/US2 do not expose them.
state_tax_data <- raw %>%
  filter(d4_concept_lab %in% tax_feature_concepts) %>%
  mutate(
    value = suppressWarnings(as.numeric(value)),
    value = normalize_eigt_full_exemption_values(value, concept = d4_concept_lab),
    value = if_else(!is.na(value) & value < 0, NA_real_, value)
  ) %>%
  group_by(
    GEO, GEO_long, state_abbr, state_name, year,
    d4_concept_lab, d2_sector_lab
  ) %>%
  summarise(
    value = summarise_state_value(value, d4_concept_lab),
    source = first_non_missing_character(source),
    varcode = first_non_missing_character(varcode),
    .groups = "drop"
  ) %>%
  add_eigt_tax_kinship("d2_sector_lab") %>%
  filter(tax_type %in% eigt_tax_type_choices) %>%
  select_eigt_canonical_kinship(
    key_cols = c(state_panel_keys, "tax_type"),
    priority = eigt_visible_kinship_choices
  )

# Complete the state-year-source-tax status spine only after canonical kinship
# selection. A genuinely missing regime is an explicit zero with Everybody
# provenance; this avoids synthetic Children rows displacing observed zeros.
state_year_spine <- raw %>%
  distinct(GEO, GEO_long, state_abbr, state_name, year)

missing_state_status <- merge(
  state_year_spine,
  data.frame(
    tax_type = eigt_tax_type_choices,
    stringsAsFactors = FALSE
  ),
  by = NULL
) %>%
  anti_join(
    state_tax_data %>%
      filter(d4_concept_lab == status_concept) %>%
      distinct(across(all_of(state_panel_keys)), tax_type),
    by = c(state_panel_keys, "tax_type")
  ) %>%
  left_join(
    state_schedule_labels %>%
      filter(kinship == "Everybody") %>%
      transmute(
        tax_type,
        d2_sector_lab = d2_label
      ),
    by = "tax_type"
  ) %>%
  mutate(
    d4_concept_lab = status_concept,
    value = 0,
    source = NA_character_,
    varcode = NA_character_,
    kinship = "Everybody"
  )

state_tax_data <- bind_rows(state_tax_data, missing_state_status)

# Tax Indicator is the status spine used to choose the active source tax for
# the combined inheritance-or-estate view. Zero indicators remain in the data.
state_tax_status <- state_tax_data %>%
  filter(d4_concept_lab == status_concept) %>%
  group_by(across(all_of(c(state_panel_keys, "tax_type")))) %>%
  summarise(
    .tax_active = summarise_tax_active(value),
    .groups = "drop"
  )

state_tax_views <- state_tax_data %>%
  left_join(state_tax_status, by = c(state_panel_keys, "tax_type")) %>%
  add_eigt_inheritance_estate_view(
    view_col = "tax_type_view",
    key_cols = state_panel_keys,
    active_col = ".tax_active",
    kinship_priority = eigt_visible_kinship_choices
  ) %>%
  filter(tax_type_view %in% eigt_tax_type_view_choices) %>%
  mutate(
    revenue_tax_category = NA_character_,
    tax_category = as.character(tax_type_view)
  )

# Revenue categories follow their raw general-government sector. They are not
# legal-regime views and must never be repeated under inheritance, estate, gift,
# or the combined inheritance-or-estate feature view.
state_revenue_data <- raw %>%
  filter(
    d4_concept_lab %in% revenue_concepts,
    d2_sector_lab %in% names(eigt_revenue_sector_categories)
  ) %>%
  mutate(
    value = suppressWarnings(as.numeric(value)),
    value = if_else(!is.na(value) & value < 0, NA_real_, value)
  ) %>%
  group_by(
    GEO, GEO_long, state_abbr, state_name, year,
    d4_concept_lab, d2_sector_lab
  ) %>%
  summarise(
    value = summarise_state_value(value, d4_concept_lab),
    source = first_non_missing_character(source),
    varcode = first_non_missing_character(varcode),
    .groups = "drop"
  ) %>%
  add_eigt_tax_kinship("d2_sector_lab") %>%
  add_eigt_revenue_tax_category("d2_sector_lab") %>%
  filter(!is.na(revenue_tax_category)) %>%
  mutate(
    tax_type_view = NA_character_,
    tax_category = revenue_tax_category
  )

long_data <- bind_rows(state_tax_views, state_revenue_data) %>%
  left_join(cpi, by = "year") %>%
  mutate(
    value = adjust_usd_2023_value(value, d4_concept_lab, cpi),
    xrate_lab = usd_2023_label,
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
  ) %>%
  select(-cpi)

duplicate_state_rows <- long_data %>%
  count(
    across(all_of(c(
      state_panel_keys, "d4_concept_lab", "tax_category", "xrate_lab"
    ))),
    name = ".rows"
  ) %>%
  filter(.rows > 1)

if (nrow(duplicate_state_rows) > 0) {
  stop("Duplicate EIGT US-state chart rows remain after normalization.", call. = FALSE)
}

long_data <- long_data %>%
  mutate(show_zero = "Yes") %>%
  bind_rows(
    long_data %>%
      filter(!is.na(value), value != 0) %>%
      mutate(show_zero = "No")
  ) %>%
  arrange(
    state_name, year, d4_concept_lab, tax_category, show_zero
  )

ft_data <- raw %>%
  select(GEO, GEO_long, state_abbr, state_name, year, source, varcode, value, d2_sector_lab) %>%
  separate(
    varcode,
    into = c("d1_code", "d2_code", "d3_code", "d4_code", "d5_code"),
    sep = "-"
  ) %>%
  mutate(
    value = normalize_eigt_full_exemption_values(value, d4_code = d4_code),
    value = suppressWarnings(as.numeric(value)),
    d2_label = d2_sector_lab
  ) %>%
  pivot_wider(
    id_cols = c(
      GEO, GEO_long, state_abbr, state_name, year, source,
      d2_code, d2_label, d5_code
    ),
    names_from = d4_code,
    values_from = value
  ) %>%
  mutate(d5_code = suppressWarnings(as.numeric(d5_code))) %>%
  normalize_eigt_wide_exemptions() %>%
  left_join(cpi, by = "year")

monetary_ft_cols <- intersect(c("adjlbo", "adjubo", "exempt"), names(ft_data))
if (length(monetary_ft_cols) > 0) {
  ft_data <- ft_data %>%
    mutate(
      across(
        all_of(monetary_ft_cols),
        ~ if_else(!is.na(.x) & !is.na(cpi) & cpi > 0, .x / cpi / 1000000, NA_real_)
      )
    )
}

ft_data <- ft_data %>%
  mutate(xrate_lab = usd_2023_label) %>%
  select(-cpi) %>%
  apply_state_tax_indicator_to_schedules(schedule_status) %>%
  filter(kinship == "Children")

ft_data <- ft_data %>%
  group_by(GEO, year, d2_label) %>%
  mutate(.drawable_ft_schedule = is_drawable_ft_schedule(typtax, exempt, adjmrt)) %>%
  ungroup() %>%
  filter(.drawable_ft_schedule) %>%
  select(-.drawable_ft_schedule) %>%
  arrange(state_name, year, d2_label, d5_code)

qs::qsave(long_data, long_output_file, preset = "fast")
qs::qsave(ft_data, ft_output_file, preset = "fast")
message("Saved ", nrow(long_data), " rows to ", long_output_file)
message("Saved ", nrow(ft_data), " rows to ", ft_output_file)
