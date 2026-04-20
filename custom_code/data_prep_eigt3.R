library(tidyr)
library(janitor)
library(stringr)
data <- data %>% 
  filter(str_detect(d4_concept_lab, 
  regex("top marginal rate|Total Revenue from Tax as % of Total Tax Revenue|Total Revenue from Tax as % of Gross Domestic Product", 
  ignore_case = T))
  ) %>%
  filter(d2_sector_lab %in% c("EIG Tax", "Inheritance Tax for Children", "Estate Tax for Children"))

data <- data %>% 
  pivot_wider(
  names_from = "d4_concept_lab", 
  id_cols = c("GEO_long", "year"), #"source" "d2_sector_lab"
  values_from = "value", 
  values_fn    = list(value = ~ .x[1]),
  values_fill  = list(value = NA)
  )
colnames(data) <- make_clean_names(colnames(data))
