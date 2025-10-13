library(magrittr)
library(qs)
library(dplyr)

m <- unique(select(read.csv("data/topo_warehouse_meta_v1.csv"),
         d4_concept_lab, GEO_long, GEO, source, legend, source_type,
         d2_sector_lab, d2_sector_des, 
         d3_vartype_lab, d3_vartype_des,
         d4_concept_des,
         varcode, legend, longname, metadata,  
         d5_dboard_specific_lab, d5_dboard_specific_des)) %>%
  arrange(d4_concept_lab) %>% 
  rename(Legend = `legend`)

qs::qsave(m, "data/topo_meta.qs", preset = "fast")