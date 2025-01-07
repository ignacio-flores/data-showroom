#Load preliminaries
require(tictoc)
tic("loading packages")
  source("libraries.R", local = T)
toc()

# Load configuration file
tic("loading YAML configuration")
  config <- yaml::read_yaml("tests/config_ineq_tseries.yaml")
  selectors <- config$selectors
  reactive_sel <- config$reactive_sel
  dt_cols <- config$dt_cols
  tt <- config$tt
  col_combiner <- config$col_combiner
toc()

#create visualization nº1
createViz(
  data.file = file.path("data/ineq_warehouse_meta_v1_2.csv"),
  new.cols = col_combiner,
  data.wrangler = "tests/dictionary_loader_ineq.R",
  x_var = "year",
  y_var = "value",
  color_var = "viz_color_var",
  selector_info = selectors,
  loose_selectors = reactive_sel,
  tooltip_vars = tt,
  table.display = T,
  dt.cols = dt_cols,
  download.button = T,
  gopts = c("line", "point")
  #hide.selectors = T,
  #listen = T,
)

