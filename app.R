# Load libraries and modules 
source("modules/libraries.R")

options(shiny.fullstacktrace = TRUE, shiny.trace = TRUE)
trace(
  what   = "left_join",
  where  = asNamespace("dplyr"),
  tracer = quote({
    this_call <- sys.call(-1)
    cat(
      "[TRACE left_join] at ", deparse(this_call), "\n",
      "  x_class =", paste(class(x), collapse="/"), "\n",
      "  y_class =", paste(class(y), collapse="/"), "\n\n"
    )
  }),
  print = FALSE
)

#check color palettes: https://r-graph-gallery.com/color-palette-finder

# Load configuration file
load_config("yaml/config_ineq_single.yaml", func = createViz) # ready to update
#load_config("yaml/config_ineq_multi.yaml", func = createViz) # ready to update
#load_config("yaml/config_ineq_prev.yaml", func = createViz)  # ready to update
#load_config("yaml/config_topo_multi.yaml", func = createViz) # ready to update
#load_config("yaml/config_topo_single.yaml", func = createViz) # ready to update
#load_config("yaml/config_topo_single_d.yaml", func = createViz) # ready to update
#load_config("yaml/config_topo_source.yaml", func = createViz) #ready to publish 
#load_config("yaml/config_topo_ffba1.yaml", func = createViz) #ready to publish 
#load_config("yaml/config_topo_ffba2.yaml", func = createViz) #ready to publish 
#load_config("yaml/config_topo_ffba3.yaml", func = createViz) #ready to publish 
#load_config("yaml/config_topo_aba1.yaml", func = createViz) #ready to publish
#load_config("yaml/config_topo_aba2.yaml", func = createViz) #ready to publish 
#load_config("yaml/config_topo_prev.yaml", func = createViz) # ready to update
#load_config("yaml/config_eigt_ft1.yaml", func = createViz) # (add log scale, xrates, make last line horizontal) pending for publication 
#load_config("yaml/config_eigt_ft2.yaml", func = createViz) # (add log scale, xrates, make last line horizontal) pending for publication 
#load_config("yaml/config_eigt_kf2.yaml", func = createViz) # (filter options more, add currency and log scale) 

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
  table.display = table.display,
  dt.cols = dt.cols,
  download.button = download.button,
  hide.selectors = hide.selectors,
  listen = listen,
  extra_layer = extra_layer, 
  color_style = color_style,
  hide.legend = hide.legend,
  plot_height = plot_height,
  meta.loc = meta.loc, 
  groupvars = groupvars
)

#Comments: extra_layers work with faceted plots only (can be made independent)
#area plots (non-faceted) have stuck tooltips (solution is to make them directly
#with plotly, like faceted plots)