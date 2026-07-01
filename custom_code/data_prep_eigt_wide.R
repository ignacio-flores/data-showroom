#use function to load sheets 
source("modules/libraries.R")
library(dplyr)
library(readr)
library(tidyr)

source("custom_code/helpers/eigt_preprocessing.R")

wide_output_file <- "data/eigt_wide.qs"
ft_output_file <- "data/eigt_ft_wide.qs"

first_schedule_typtax <- function(x) {
  values <- unique(x[!is.na(x)])
  if (length(values) == 0) {
    return(NA_real_)
  }
  if (length(values) > 1) {
    stop("Multiple typtax values found for one EIGT FT schedule.", call. = FALSE)
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

data <- read_csv("data/eigt_warehouse_meta_v2.csv")

remind_dictionary(
  file_path = file.path("data/dictionary.xlsx"),
  sheet_names = c("Sources", "d2_sector", "d3_vartype", "d4_concept", "d5_dboard_specific")
)

names(d2_sector) <- paste0("d2_", names(d2_sector)) 
names(d3_vartype) <- paste0("d3_", names(d3_vartype))
names(d4_concept) <- paste0("d4_", names(d4_concept))
names(d5_dboard_specific) <- paste0("d5_", names(d5_dboard_specific))

d2_sector <- d2_sector %>% select(-c('d2_description'))
d3_vartype <- d3_vartype %>% select(-c('d3_description'))
d4_concept <- d4_concept %>% select(-c('d4_description'))
d5_dboard_specific <- d5_dboard_specific %>% select(-c('d5_description'))

data <- data %>%
  filter(!is_eigt_subregion_geo(GEO)) %>%
  select(GEO, GEO_long, year, source, varcode, value) %>%
  separate(varcode, into = c("d1_code", "d2_code", "d3_code", "d4_code", "d5_code"), sep = "-") %>%
  mutate(value = normalize_eigt_full_exemption_values(value, d4_code = d4_code)) %>%
  left_join(d2_sector, by = 'd2_code') %>% left_join(d3_vartype, by = 'd3_code') %>% 
  left_join(d4_concept, by = 'd4_code') %>% left_join(d5_dboard_specific, by = 'd5_code')  

data <- pivot_wider(
  data, 
  id_cols = c('GEO', 'GEO_long', 'year', 'source', 'd2_code', 'd2_label', 'd5_code', 'd5_label'), 
  names_from = d4_code, 
  values_from = value) 

data <- data %>% mutate(d5_code = as.numeric(d5_code))
data <- normalize_eigt_wide_exemptions(data)

ft_data <- data %>%
  group_by(GEO, year, d2_label) %>%
  mutate(.drawable_ft_schedule = is_drawable_ft_schedule(typtax, exempt, adjmrt)) %>%
  ungroup() %>%
  filter(.drawable_ft_schedule) %>%
  select(-.drawable_ft_schedule)

qs::qsave(data, wide_output_file, preset = "fast")
qs::qsave(ft_data, ft_output_file, preset = "fast")
