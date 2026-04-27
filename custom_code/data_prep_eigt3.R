library(dplyr)
library(janitor)
library(tidyr)

first_non_na <- function(x) {
  vals <- x[!is.na(x)]
  if (length(vals) == 0) {
    return(NA_real_)
  }
  as.numeric(vals[1])
}

target_concepts <- c(
  "Top Marginal Rate",
  "Exemption Threshold",
  "Total Revenue from Tax",
  "Total Revenue from Tax as % of Total Tax Revenue",
  "Total Revenue from Tax as % of Gross Domestic Product"
)
tax_concepts <- c("Top Marginal Rate", "Exemption Threshold")
revenue_concepts <- setdiff(target_concepts, tax_concepts)

data <- data %>%
  filter(d4_concept_lab %in% target_concepts, !is.na(year)) %>%
  mutate(value = as.numeric(value))

revenue_df <- data %>%
  filter(
    d2_sector_lab == "EIG Tax, general government level",
    d4_concept_lab %in% revenue_concepts
  ) %>%
  select(GEO, GEO_long, year, d4_concept_lab, value) %>%
  pivot_wider(
    names_from = d4_concept_lab,
    values_from = value,
    values_fn = list(value = first_non_na),
    values_fill = list(value = NA_real_)
  ) %>%
  clean_names()

tax_df <- data %>%
  filter(
    d2_sector_lab != "EIG Tax, general government level",
    d4_concept_lab %in% tax_concepts
  ) %>%
  select(GEO, GEO_long, year, d2_sector_lab, d4_concept_lab, value) %>%
  pivot_wider(
    names_from = d4_concept_lab,
    values_from = value,
    values_fn = list(value = first_non_na),
    values_fill = list(value = NA_real_)
  ) %>%
  clean_names()

data <- tax_df %>%
  left_join(
    revenue_df,
    by = c("geo", "geo_long", "year"),
    suffix = c("", "_revenue")
  )

monetary_cols <- intersect(
  c("exemption_threshold", "total_revenue_from_tax"),
  names(data)
)

if (length(monetary_cols) > 0) {
  cpi <- read.csv("data/supplementary_var_long.csv") %>%
    filter(variable == "inyixx") %>%
    transmute(geo = country, year = as.numeric(year), cpi = value)

  usd_xrate_2023 <- read.csv("data/supplementary_var_long.csv") %>%
    filter(variable == "xlcusx", year == 2023) %>%
    transmute(geo = country, usd_xrate_2023 = value)

  data <- data %>%
    left_join(cpi, by = c("geo", "year")) %>%
    left_join(usd_xrate_2023, by = "geo") %>%
    mutate(
      across(
        all_of(monetary_cols),
        ~ if_else(
          !is.na(.x) & .x >= 0 & !is.na(cpi) & cpi > 0 &
            !is.na(usd_xrate_2023) & usd_xrate_2023 > 0,
          .x / cpi / usd_xrate_2023 / 1e6,
          NA_real_
        )
      )
    ) %>%
    select(-cpi, -usd_xrate_2023)
}

data <- data %>%
  arrange(year, geo_long, d2_sector_lab)
