library(zoo)
library(dplyr)
library(readxl)

#load list of countries
dictionary <- read_excel("data/dictionary.xlsx", sheet = "GEO")

#load suplementary variables (cpi)
sup <- read.csv("data/supplementary_var_long.csv") %>% filter(variable == "inyixx") %>%
  select(-c("variable")) %>% rename(`cpi` = value, `GEO` = country) %>% 
  left_join(dictionary, by = "GEO") %>% select(-c("GEO3.y", "GEO3.x"))

#adjust scale and inflation
data <- left_join(data, sup, by = c("GEO", "year")) %>%
  mutate(adjlbo = adjlbo/cpi, adjlbo = adjlbo/10^6)

#get max value by tax
empty <- data %>% select(GEO, year, tax, adjlbo) %>%
  group_by(GEO, tax) %>% mutate(max = max(adjlbo, na.rm=T)) %>%
  select(-c("adjlbo")) %>% unique() %>% rename(`adjlbo` = max) 

#replace -Inf with 0 in empty 
empty$adjlbo[empty$adjlbo == -Inf] <- 0
empty <- empty[!is.na(empty$adjlbo),]

data <-  rows_append(data, empty) %>% arrange(tax, GEO, year, adjlbo, bracket) %>%
  group_by(GEO, tax, year) %>% mutate(adjmrt = zoo::na.locf(adjmrt, na.rm = FALSE))
data <- data %>% mutate(year = as.character(year)) %>% 
  group_by(GEO, tax, year) %>% mutate(test = sum(adjmrt)) %>% filter(test != 0) %>% select(-c("test")) 
