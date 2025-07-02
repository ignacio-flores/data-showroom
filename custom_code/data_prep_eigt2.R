library(zoo)
library(dplyr)
library(readxl)
library(stringr)
#load concepts 

data <- data %>% filter(str_detect(d4_concept_lab, regex("total|top marginal rate|exemption threshold", ignore_case = T)))