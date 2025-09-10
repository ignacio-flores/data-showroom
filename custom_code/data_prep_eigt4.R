library(zoo)
library(dplyr)
library(readxl)
library(stringr)
#load concepts 

data <- data %>% 
  filter(str_detect(d4_concept_lab, regex("total|top marginal rate|exemption threshold", ignore_case = T)))

data <- data[!grepl("_", data$GEO), ]
library(countrycode)

# suppose you have ISO-2 codes in df$iso2
data$iso3 <- countrycode(data$GEO, origin = "iso2c", destination = "iso3c")