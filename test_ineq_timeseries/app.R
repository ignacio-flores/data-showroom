##########LOCAL OR DEPLOY MODE
deploy = F
##########################

#Load preliminaries
library(tictoc)
tic("loading packages")
  if (deploy == F) pf = "../common" else pf = "common"
  source(file.path(pf,"global.R"), local = T)
toc()

#define selectors
selectors <- list(
  GEO_long = list(label = "Country", selected = c("United States")),
  var_per = list(label = "Variable", selected = "Share of Total Net Wealth, p99p100"),
  d5_dboard_specific_lab = list(
    label = "Unit of analysis",
    selected = c("Individuals - Adults", "Individuals - Adults (equal split)", "Households", "Tax Units"),
    multiple = TRUE
  )
)
reactive_sel <- list(
  Data_Type = list(label = "Data type", type = "checkbox"), 
  legend = list(label = "Source", type = "checkbox")
)

#define data table
dt_cols <- list(
  GEO = "Country code",
  year = "Year",
  GEO_long = "Country",
  var_per = "Variable",
  d5_dboard_specific_lab = "Unit of Analysis",
  value = "Value",
  viz_color_var = "Source and Unit",
  legend = "Source",
  metadata = "Description",
  data_description = "Metadata", 
  Data_Type = "Data type" #THIS IS MANDATORY FOR REACTIVE SEL TO WORK (SOLVE)
)

# define tooltip
tt <- list(
  GEO_long = "Country:",
  year = "Year:",
  value = "Value:",
  legend = "Source:",
  metadata = "Explanation:",
  data_description = "Data type:"
)

# combine columns
col_combiner <- list(
  viz_color_var = c("legend", "d5_dboard_specific_lab"),
  var_per = c("d3_vartype_lab", "percentile")
)

#create visualization nº1
createViz(
  data.file = file.path(pf,"data/ineq_warehouse_meta_v1_2.csv"),
  new.cols = col_combiner,
  data.wrangler = "specific/dictionary_loader_ineq.R",
  x_var = "year",
  y_var = "value",
  color_var = "viz_color_var",
  selector_info = selectors,
  loose_selectors = reactive_sel,
  tooltip_vars = tt,
  table.display = T,
  dt.cols = dt_cols,
  download.button = T,
  common.fold.pf = pf,
  gopts = c("line", "point")
  #hide.selectors = T,
  #listen = T,
)

