# Load libraries and modules 
source("modules/libraries.R")

#options(shiny.fullstacktrace = TRUE, shiny.trace = TRUE)
#check color palettes: https://r-graph-gallery.com/color-palette-finder

default_graph <- "eigt_wm1"
graph <- Sys.getenv("DATA_SHOWROOM_GRAPH", unset = "")
if (!nzchar(graph) && file.exists("active_graph.txt")) {
  graph <- trimws(readLines("active_graph.txt", n = 1, warn = FALSE))
}
if (!nzchar(graph)) graph <- default_graph
load_config(paste0("yaml/config_", graph, ".yaml"), func = createViz)

# Run app
createViz(
  graph = graph, 
  data.file = data.file,
  meta.file = meta.file,
  data.wrangler = data.wrangler,
  new.cols = new.cols,
  gopts = gopts, 
  axis_vars = axis_vars, 
  color = color, 
  facet_var = facet_var,
  facet_var_lab = facet_var_lab,
  fixed_selectors = fixed_selectors,
  loose_selectors = loose_selectors,
  tooltip_vars = tooltip_vars,
  table.display = table.display,
  dt.cols = dt.cols,
  download.button = download.button,
  hide.selectors = hide.selectors,
  listen = listen,
  extra_layer = extra_layer, 
  hide.legend = hide.legend,
  plot_height = plot_height,
  meta.loc = meta.loc, 
  keep.col = keep.col, 
  area_stack_toggle = area_stack_toggle, 
  area_stack_default = area_stack_default,
  scatter_options = scatter_options
)

#Comments: extra_layers work with faceted plots only (can be made independent)
#area plots (non-faceted) have stuck tooltips (solution is to make them directly
#with plotly, like faceted plots)
