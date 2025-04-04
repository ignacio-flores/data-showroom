# Load libraries and modules 
source("libraries.R", local = T)

# Load configuration file
#load_config("tests/config_ineq_single.yaml", func = createViz)
load_config("tests/config_topo_multi.yaml", func = createViz)
#load_config("tests/config_eigt.yaml", func = createViz)

# Run app
createViz(
  data.file = data.file,
  meta.file = meta.file,
  data.wrangler = data.wrangler,
  new.cols = new.cols,
  gopts = gopts, 
  x_var = x_var,
  x_var_lab = x_var_lab,
  xnum_breaks = xnum_breaks,
  y_var = y_var,
  y_var_lab = y_var_lab,
  color_var = color_var,
  color_var_lab = color_var_lab,
  facet_var = facet_var,
  facet_var_lab = facet_var_lab,
  selector_info = selector_info,
  loose_selectors = loose_selectors,
  tooltip_vars = tooltip_vars,
  table.display = T,
  dt.cols = dt.cols,
  download.button = download.button,
  hide.selectors = hide.selectors,
  listen = listen,
  extra_layer = extra_layer
)

#Comments: extra_layers work with faceted plots only (can be made independent)
#area plots (non-faceted) have stuck tooltips (solution is to make them directly
#with plotly, like faceted plots)