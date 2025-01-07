#Load libraries and modules 
source("libraries.R", local = T)

# Load configuration file
load_config("tests/config.yaml")

#create visualization nº1
createViz(
  data.file = file.path("data/ineq_warehouse_meta_v1_2.csv"),
  new.cols = combine_col,
  data.wrangler = "tests/dictionary_loader_ineq.R",
  x_var = "year",
  y_var = "value",
  color_var = "viz_color_var",
  selector_info = fixed_sel,
  loose_selectors = reactive_sel,
  tooltip_vars = tooltip,
  table.display = T,
  dt.cols = dt_cols,
  download.button = T,
  gopts = c("line", "point")
  #hide.selectors = T,
  #listen = T,
)
