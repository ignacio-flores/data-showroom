# Module for reactive data filtering
dataFilter <- function(input, output, session,
                       data, x_var, y_var, color_var, selector_vars,
                       dt_cols, tooltip_vars, value_scale, num.conversion) {
  reactive({
    #scaling_factor <- getScalingFactor(value_scale)
    data_filtered <- data
    for (var in selector_vars) {
      data_filtered <- data_filtered[data_filtered[[var]] %in% input[[var]], ]
    }
    if (nrow(data_filtered) == 0) {
      return(NULL)  # Return NULL if no data is available
    } else {
      data_filtered %>%
        select(all_of(c(dt_cols, x_var, selector_vars, y_var, color_var, names(tooltip_vars))))
    }
      #%>% mutate(!!y_var := .data[[y_var]] / scaling_factor)
  })
}
