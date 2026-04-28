library(countrycode)
library(data.table)
library(dplyr)
library(qs)
library(stringr)

input_file <- "data/eigt_warehouse_meta_v2.csv"
output_file <- "data/eigt_wm1_ready.qs"

target_concepts <- c(
  "Tax Indicator",
  "Top Marginal Rate",
  "Exemption Threshold",
  "Total Revenue from Tax",
  "Total Revenue from Tax as % of Total Tax Revenue",
  "Total Revenue from Tax as % of Gross Domestic Product"
)

monetary_concepts <- c(
  "Exemption Threshold",
  "Total Revenue from Tax"
)

first_non_missing <- function(x) {
  vals <- x[!is.na(x)]
  if (length(vals) == 0) return(NA_real_)
  vals[[1]]
}

summarise_map_value <- function(value, concept) {
  vals <- suppressWarnings(as.numeric(value))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_real_)

  concept <- first_non_missing(concept)
  if (concept %in% c("Tax Indicator", "Top Marginal Rate", "Exemption Threshold")) {
    return(max(vals, na.rm = TRUE))
  }

  first_non_missing(vals)
}

data <- data.table::fread(
  input_file,
  select = c("GEO", "GEO_long", "year", "value", "d2_sector_lab", "d4_concept_lab"),
  showProgress = FALSE
) %>%
  as.data.frame()

data <- data %>%
  filter(
    d4_concept_lab %in% target_concepts,
    !is.na(year),
    !str_detect(GEO, fixed("_"))
  ) %>%
  mutate(
    value = suppressWarnings(as.numeric(value)),
    value = if_else(!is.na(value) & value < 0, NA_real_, value)
  )

supp_vars <- c(
  "inyixx",
  "xlcusx", "xlceux", "xlcyux",
  "xlcusp", "xlceup", "xlcyup"
)

supp <- data.table::fread(
  "data/supplementary_var_long.csv",
  select = c("country", "year", "variable", "value"),
  showProgress = FALSE
) %>%
  filter(variable %in% supp_vars) %>%
  mutate(year = as.numeric(year), value = as.numeric(value))

cpi <- supp %>%
  filter(variable == "inyixx") %>%
  transmute(GEO = country, year, cpi = value)

xrates_current <- supp %>%
  filter(variable %in% c("xlcusx", "xlceux", "xlcyux")) %>%
  transmute(GEO = country, year, xrate_var = variable, xrate = value)

xrates_2023 <- supp %>%
  filter(
    variable %in% c("xlcusx", "xlceux", "xlcyux", "xlcusp", "xlceup", "xlcyup"),
    year == 2023
  ) %>%
  transmute(GEO = country, xrate_var = variable, xrate = value)

adjust_value <- function(base_data, label, xrate_var = NULL, real = FALSE, current_rate = FALSE) {
  out <- base_data
  out$xrate_lab <- label

  if (isTRUE(real)) {
    out <- out %>% left_join(cpi, by = c("GEO", "year"))
  } else {
    out$cpi <- 1
  }

  if (!is.null(xrate_var)) {
    xrates <- if (isTRUE(current_rate)) xrates_current else xrates_2023
    out <- out %>%
      left_join(
        xrates %>% filter(.data$xrate_var == !!xrate_var) %>% select(-xrate_var),
        by = if (isTRUE(current_rate)) c("GEO", "year") else "GEO"
      )
  } else {
    out$xrate <- 1
  }

  out %>%
    mutate(
      value = case_when(
        !d4_concept_lab %in% monetary_concepts ~ value,
        is.na(value) ~ NA_real_,
        is.na(cpi) | cpi <= 0 | is.na(xrate) | xrate <= 0 ~ NA_real_,
        TRUE ~ value / cpi / xrate
      )
    ) %>%
    select(-cpi, -xrate)
}

data <- bind_rows(
  adjust_value(data, "National Currency"),
  adjust_value(data, "National Currency adjusting for inflation", real = TRUE),
  adjust_value(data, "USD", xrate_var = "xlcusx", current_rate = TRUE),
  adjust_value(data, "USD adjusting for inflation", xrate_var = "xlcusx", real = TRUE),
  adjust_value(data, "Euro", xrate_var = "xlceux", current_rate = TRUE),
  adjust_value(data, "Euro adjusting for inflation", xrate_var = "xlceux", real = TRUE),
  adjust_value(data, "Yuan", xrate_var = "xlcyux", current_rate = TRUE),
  adjust_value(data, "Yuan adjusting for inflation", xrate_var = "xlcyux", real = TRUE),
  adjust_value(data, "PPP USD", xrate_var = "xlcusp", real = TRUE),
  adjust_value(data, "PPP Euro", xrate_var = "xlceup", real = TRUE),
  adjust_value(data, "PPP Yuan", xrate_var = "xlcyup", real = TRUE)
)

data <- data %>%
  group_by(GEO, GEO_long, year, d4_concept_lab, d2_sector_lab, xrate_lab) %>%
  summarise(
    value = summarise_map_value(value, d4_concept_lab),
    .groups = "drop"
  ) %>%
  arrange(GEO, d4_concept_lab, d2_sector_lab, xrate_lab, desc(!is.na(value)), desc(year)) %>%
  group_by(GEO, GEO_long, d4_concept_lab, d2_sector_lab, xrate_lab) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    iso3 = countrycode(
      dplyr::recode(GEO, UK = "GB", XK = NA_character_),
      origin = "iso2c",
      destination = "iso3c"
    ),
    show_zero = "Yes"
  ) %>%
  filter(!is.na(iso3))

data <- bind_rows(
  data,
  data %>%
    filter(!is.na(value), value != 0) %>%
    mutate(show_zero = "No")
) %>%
  arrange(GEO_long, year, d4_concept_lab, d2_sector_lab, xrate_lab, show_zero)

qs::qsave(data, output_file, preset = "fast")
message("Saved ", nrow(data), " rows to ", output_file)
