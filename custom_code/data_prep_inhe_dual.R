library(dplyr)
library(tidyr)

first_unique_value <- function(x) {
  values <- unique(x[!is.na(x)])
  if (length(values) == 0) {
    return(NA_real_)
  }
  if (length(values) > 1) {
    stop("Multiple inheritance dual-axis values found for one series key.", call. = FALSE)
  }
  as.numeric(values[[1]])
}

data <- data %>%
  filter(
    source == "Guzzardi2026",
    legend == "Guzzardi and Morelli (2026)",
    d5_dboard_specific_lab == "Economic Flow",
    (
      d3_vartype_lab == "Ratio" &
        d4_concept_lab == "Inheritances & Gifts"
    ) |
      (
        d3_vartype_lab == "Aggregate" &
          d4_concept_lab == "Inheritances & Gifts"
      ) |
      (
        d3_vartype_lab == "Rate" &
          d4_concept_lab == "Average Effective Tax Rate"
      )
  ) %>%
  mutate(
    measure = case_when(
      d3_vartype_lab == "Ratio" ~ "inheritances_gifts_ratio",
      d3_vartype_lab == "Aggregate" ~ "inheritances_gifts_aggregate",
      d3_vartype_lab == "Rate" ~ "average_effective_tax_rate",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(measure)) %>%
  group_by(GEO, GEO_long, year, source, legend, measure) %>%
  summarise(value = first_unique_value(value), .groups = "drop") %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  filter(
    !is.na(inheritances_gifts_ratio) |
      !is.na(inheritances_gifts_aggregate) |
      !is.na(average_effective_tax_rate)
  ) %>%
  arrange(GEO_long, year)
