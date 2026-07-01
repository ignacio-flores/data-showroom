library(zoo)
library(dplyr)
library(readxl)
library(stringr)

source("custom_code/helpers/eigt_preprocessing.R")

#load concepts 

data <- data %>%
  filter(!is_eigt_subregion_geo(GEO)) %>%
  mutate(
    value = normalize_eigt_full_exemption_values(
      suppressWarnings(as.numeric(value)),
      concept = d4_concept_lab
    )
  ) %>%
  filter(str_detect(d4_concept_lab, regex("total|top marginal rate|exemption threshold", ignore_case = T)))
