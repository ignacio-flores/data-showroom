#load timekeeper
require(tictoc)

tic("loading packages")
  #load general libraries
  library(shiny)
  library(magrittr)
  library(DT)
  #load modules 
  source("modules/create_selectors.R")
  source("modules/ensureListStructure.R")
  source("modules/get_scaling_factor.R")
  source("modules/wrap_text.R")
  source("modules/remind_dictionary.R")
  source("modules/PlotServer.R")
  source("modules/dataTableServer.R")
  source("modules/downloadModule.R")
  source("modules/dataFilter.R")
  source("modules/combine_multiple_columns.R")
  source("modules/createViz.R")
  source("modules/load_config.R")
  source("modules/metaTableServer.R")
toc()