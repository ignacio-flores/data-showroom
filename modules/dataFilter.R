# Module for reactive data filtering
dataFilter <- function(input, output, session,
                       data, x_var, y_var, color_var, selector_vars,
                       dt_cols, tooltip_vars, value_scale, num.conversion,
                       extra_layer = NULL) {
  reactive({
    
    data_filtered <- data
    for (var in selector_vars) {
      if (var == color_var & !is.null(extra_layer)) {
        data_filtered <- data_filtered[data_filtered[[var]] %in% unique(c(extra_layer$values, input[[var]])), ]
      } else {
        data_filtered <- data_filtered[data_filtered[[var]] %in% input[[var]], ]
      }
    }
    
    if (nrow(data_filtered) == 0) {
      return(NULL)
    } else {
      
      data_filtered <- data_filtered %>%
        select(all_of(c(dt_cols, x_var, selector_vars, y_var, color_var, names(tooltip_vars))))
      
      # Extract selected variable names
      sort_vars <- c()
      if (!is.null(facet_var) && facet_var %in% names(data_filtered)) {
        sort_vars <- c(sort_vars, facet_var)
      }
      sort_vars <- c(sort_vars, selector_vars)
      sort_vars <- c(sort_vars, x_var)
      data_sorted <- data_filtered[do.call(order, data_filtered[sort_vars]), ]
      
      # Return filtered and sorted data
      data_sorted %>%
        select(all_of(c(dt_cols, x_var, selector_vars, y_var, color_var, names(tooltip_vars))))
    }
  })
}


# 
# # Module for reactive data filtering
# dataFilter <- function(input, output, session,
#                        data, x_var, y_var, color_var, selector_vars,
#                        dt_cols, tooltip_vars, value_scale, num.conversion,
#                        extra_layer = NULL) {
#   reactive({
# 
#     data_filtered <- data
#     for (var in selector_vars) {
#       data_filtered <- data_filtered[data_filtered[[var]] %in% input[[var]], ]
#     }
# 
#     if (nrow(data_filtered) == 0) {
#       return(NULL)
#     } else {
# 
#       data_filtered %>%
#         select(all_of(c(dt_cols, x_var, selector_vars, y_var, color_var, names(tooltip_vars))))
# 
#       # Extract selected variable names
#       sort_vars <- c()
#       if (!is.null(facet_var) && facet_var %in% names(data_filtered)) {
#         sort_vars <- c(sort_vars, facet_var)
#       }
#       sort_vars <- c(sort_vars, selector_vars)
#       sort_vars <- c(sort_vars, x_var)
#       data_sorted <- data_filtered[do.call(order, data_filtered[sort_vars]), ]
# 
#       # Return filtered and sorted data
#       data_sorted %>%
#         select(all_of(c(dt_cols, x_var, selector_vars, y_var, color_var, names(tooltip_vars))))
#     }
#   })
# }
