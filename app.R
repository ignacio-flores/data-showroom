#Load libraries and modules 
source("libraries.R", local = T)

# Load configuration file
load_config("tests/config.yaml")

#create visualization nº1
createViz(
  data.file = data.file,
  data.wrangler = data.wrangler,
  new.cols = new.cols,
  gopts = gopts, 
  x_var = mainvars$x.var,
  y_var = mainvars$y.var,
  color_var = mainvars$color.var,
  selector_info = fixed_sel,
  loose_selectors = reactive_sel,
  tooltip_vars = tooltip,
  table.display = T,
  dt.cols = dt_cols,
  download.button = T
  #hide.selectors = T,
  #listen = T,
)
