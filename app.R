# Load libraries and modules 
source("libraries.R", local = T)

# Load configuration file
load_config("tests/config.yaml", func = createViz)

# Run app
createViz(
  data.file = data.file,
  data.wrangler = data.wrangler,
  new.cols = new.cols,
  gopts = gopts, 
  x_var = x_var,
  y_var = y_var,
  color_var = color_var,
  selector_info = selector_info,
  loose_selectors = loose_selectors,
  tooltip_vars = tooltip_vars,
  table.display = T,
  dt.cols = dt.cols,
  download.button = download.button,
  hide.selectors = hide.selectors,
  listen = listen,
)
