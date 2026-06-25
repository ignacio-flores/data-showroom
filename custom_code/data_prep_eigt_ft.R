library(dplyr)
library(zoo)

source("custom_code/helpers/eigt_tax_kinship.R")

# Fill step schedules, remove zero-rate schedules, and keep year selector-friendly.
# Currency conversion for bracket bounds is handled by value_transform at runtime.
# Right-end horizontal extension is handled in PlotServer step logic.
data <- data %>%
  arrange(d2_label, GEO, year, adjlbo, d5_code) %>%
  group_by(GEO, d2_label, year) %>%
  mutate(adjmrt = zoo::na.locf(adjmrt, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(year = as.character(year)) %>%
  group_by(GEO, d2_label, year) %>%
  mutate(`_sum_rate` = sum(adjmrt, na.rm = TRUE)) %>%
  filter(`_sum_rate` != 0) %>%
  ungroup() %>%
  add_eigt_tax_kinship("d2_label") %>%
  add_eigt_inheritance_estate_view("tax_type_view") %>%
  select(-`_sum_rate`)
