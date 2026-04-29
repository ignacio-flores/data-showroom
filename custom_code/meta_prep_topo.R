library(magrittr)
library(qs)
library(dplyr)
library(data.table)
library(readxl)

source("custom_code/topo_metadata_bundle.R")

bundle <- build_topo_metadata_bundle(
  warehouse_file = "data/topo_warehouse_meta_v2.csv",
  dictionary_file = "data/dictionary.xlsx"
)

qs::qsave(bundle, "data/topo_metadata_bundle.qs", preset = "fast")
