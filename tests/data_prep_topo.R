library(tidyr)
library(dplyr)
library(readxl)

#load list of countries 
dictionary <- read_excel("data/dictionary.xlsx", sheet = "GEO")

#label original data as nominal prices
data_nom_lcu <- data %>% 
  mutate(xrate_lab = "Local currency, nominal prices")

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
rm(xrates_mer)
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
data_rea <- data_rea %>% 
  mutate(xrate_lab = ifelse(xrate_lab == "Local currency, nominal prices", "Local currency (2023 prices)", xrate_lab))
rm(data_nom_lcu, cpi)

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
rm(data_rea_x, xrates_all)

data <- rbind(select(data_nom, -Country), select(data_rea, -cpi)) %>% 
  arrange(legend, d2_sector_lab, GEO, year, d4_concept_lab, xrate_lab)
rm(data_nom, data_rea) 

#label data with macro totals
data_mac <- data %>% mutate(pop = 1, pop_lab = "Macroeconomic total")

#load population data
pops <- read.csv("data/supplementary_var_long.csv") %>%
  filter(variable %in% c("npopul", "npopul_adu", "npopem", "ntaxma")) %>%
  select(-c("GEO3", "GEO_WB")) %>% 
  rename(`GEO` = country, `pop` = value, `pop_lab` = variable) %>%
  left_join(dictionary, by = "GEO") %>% select(-c("GEO3", "Country"))
pops <- pops %>% mutate(pop_lab = ifelse(pop_lab == "npopul", "Per capita", pop_lab))
pops <- pops %>% mutate(pop_lab = ifelse(pop_lab == "npopul_adu", "Per adult", pop_lab))
pops <- pops %>% mutate(pop_lab = ifelse(pop_lab == "npopem", "Per employed population", pop_lab))
pops <- pops %>% mutate(pop_lab = ifelse(pop_lab == "ntaxma", "per tax unit", pop_lab))

data <- left_join(data, pops, by = c("GEO", "year"), multiple = "all") 
rm(pops)
data <- rbind(data, data_mac) %>% 
  mutate(value = value / pop) %>%
  arrange(GEO, xrate, pop, year)
rm(data_mac, dictionary)

#pivot wider to separate d4_concept_lab == "Net Wealth" as a separate variable  
netwea <- filter(data, d4_concept_lab == "Net Wealth") %>% 
  select("GEO", "year", "source", "d2_sector_lab", "xrate_lab", "pop_lab", "d4_concept_lab", "value") %>%
  pivot_wider(
    id_cols = c("GEO", "year", "source", "d2_sector_lab", "xrate_lab", "pop_lab"), 
    names_from = d4_concept_lab, 
    values_from = value) %>%
  filter(!is.na(`Net Wealth`)) %>%
  rename(netwea = `Net Wealth`)

#merge back to data
data <- filter(data, d4_concept_lab != "Net Wealth") %>% 
  left_join(netwea, by = c("GEO", "year", "source", "d2_sector_lab", "xrate_lab", "pop_lab")) %>%
  arrange(GEO, xrate, pop, year)
rm(netwea)
