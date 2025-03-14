library(tidyr)
library(dplyr)
library(readxl)

#load list of countries 
dictionary <- read_excel("data/dictionary.xlsx", sheet = "GEO")

#label original data as nominal prices
data_nom_lcu <- data %>% 
  mutate(xrate_lab = "Local currency unit", unit = "Total")

#Adjust using market xrates
xrates_mer <- read.csv("data/supplementary_var_long.csv") %>% 
  filter(variable %in% c("xlceux", "xlcusx", "xlcyux")) %>% 
  select(-c("GEO3", "GEO_WB")) %>% rename(`GEO` = country, `xrate` = value, `xrate_lab` = variable)
xrates_mer <- xrates_mer %>% mutate(xrate_lab = ifelse(xrate_lab == "xlcyux", "Yuan, Market Exchange rate, nominal prices", xrate_lab))
xrates_mer <- xrates_mer %>% mutate(xrate_lab = ifelse(xrate_lab == "xlceux", "Euro, Market Exchange rate, nominal prices", xrate_lab))
xrates_mer <- xrates_mer %>% mutate(xrate_lab = ifelse(xrate_lab == "xlcusx", "USD, Market Exchange rate, nominal prices", xrate_lab))
data_nom <- select(data_nom_lcu, -xrate_lab) %>% 
  left_join(xrates_mer, by = c("GEO", "year"), multiple = "all") %>% 
  mutate(value = value / xrate) 

#Create nominal data 
data_nom_lcu <- data_nom_lcu %>% mutate(xrate = 1)
data_nom <- rbind(data_nom, data_nom_lcu) %>%  
  left_join(dictionary, by = "GEO") %>% select(-"GEO3") %>% 
  arrange(d2_sector_lab, GEO, year, xrate_lab)

#load cpi
cpi <- read.csv("data/supplementary_var_long.csv") %>%
  filter(variable == "inyixx") %>% select(-c("GEO_WB", "GEO3", "variable")) %>%
  rename(`cpi` = value, GEO = country)

#create price-adjusted data
data_rea <- left_join(data_nom_lcu, cpi, by = c("GEO", "year")) %>%
  mutate(value = value / cpi) %>%
  arrange(legend, d2_sector_lab, GEO, year, d4_concept_lab, xrate_lab)

#load 2023 xrates 
xrates_all <- read.csv("data/supplementary_var_long.csv") %>%
  filter(variable %in% c("xlceup", "xlceux", "xlcusp", "xlcusx", "xlcyup", "xlcyux") & year == 2023) %>%
  select(-c("GEO3", "GEO_WB", "year")) %>% rename(`GEO` = country, `xrate` = value, `xrate_lab` = variable) %>%
  left_join(dictionary, by = "GEO") %>% select(-"GEO3") 
xrates_all <- xrates_all %>% mutate(xrate_lab = ifelse(xrate_lab == "xlcyux", "Yuan (2023 prices)", xrate_lab))
xrates_all <- xrates_all %>% mutate(xrate_lab = ifelse(xrate_lab == "xlceux", "Euro (2023 prices)", xrate_lab))
xrates_all <- xrates_all %>% mutate(xrate_lab = ifelse(xrate_lab == "xlcusx", "USD (2023 prices)", xrate_lab))
xrates_all <- xrates_all %>% mutate(xrate_lab = ifelse(xrate_lab == "xlcyup", "Yuan (2023 PPP)", xrate_lab))
xrates_all <- xrates_all %>% mutate(xrate_lab = ifelse(xrate_lab == "xlceup", "Euro (2023 PPP)", xrate_lab))
xrates_all <- xrates_all %>% mutate(xrate_lab = ifelse(xrate_lab == "xlcusp", "USD (2023 PPP)", xrate_lab))

#merge data_rea and xrates_all
data_rea_x <- left_join(select(data_rea, -c("xrate", "xrate_lab")), xrates_all, by = c("GEO")) %>%
  arrange(legend, d2_sector_lab, GEO, year) %>% mutate(value = value / xrate)
data_rea <- rbind(data_rea, select(data_rea_x, -Country))

data <- rbind(select(data_nom, -Country), select(data_rea, -cpi)) %>% arrange(legend, d2_sector_lab, GEO, year, d4_concept_lab, xrate_lab)

#load suplementary variables
pops <- read.csv("data/supplementary_var_long.csv") %>%
  filter(variable %in% c("npopul", "npopul_adu", "npopem", "ntaxma")) %>%
  select(-c("GEO3", "GEO_WB")) %>% rename(`GEO` = country, `pop` = value, `pop_lab` = variable) %>%
  left_join(dictionary, by = "GEO") %>% select(-"GEO3")


data <- left_join(data, pops, by = c("GEO", "year"), multiple = "all") %>%
  mutate(value = value / pop) %>%
  arrange(GEO, xrate, pop, year)
