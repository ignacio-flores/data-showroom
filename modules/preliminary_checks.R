
# Parse axis variables 
if(is.null(axis_vars$x_axis$var) | is.null(axis_vars$x_axis$var)) {
  if (gopts != "map") {
    stop("Please specify both x_axis and y_axis variables")  
  }
} 

#Parse color
if (!("bar" %in% gopts)) {
  if(is.null(color$var)) {
    stop("please specify a color variable")
  } else {
    color_var <<- color$var 
  }
}

if(!is.null(color$label)) {
  color_var_lab <- color$label 
}
if(is.null(color$style)) {
  color_style <- "viridis"
} else {
  color_style <- color$style
}
if(!is.null(color$group)) {
  groupvars <- color$group
} else {
  groupvars = NULL
}

# Preprocess inputs to ensure list format 
fixed_selectors <- ensureListStructure(fixed_selectors)
if (!is.null(dt.cols)) {
  dt.cols <- ensureListStructure(dt.cols)
}
tooltip_vars <- ensureListStructure(tooltip_vars)
if (!is.null(loose_selectors)) {
  all_selectors <- c(fixed_selectors, loose_selectors)
} else {
  all_selectors <- fixed_selectors
}