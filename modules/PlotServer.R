

require(ggplot2)
require(plotly)
require(viridis)
require(scales)

plotOutputUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    uiOutput(ns("messageDisplay")),   # UI element for displaying messages
    plotlyOutput(ns("valuePlot"), width = "100%")
  )
}

# Server logic for the plot module
plotModuleServer <- function(id, filtered_data_func, x_var, x_var_lab, y_var, y_var_lab, color_var, color_var_lab, facet_var, facet_var_lab, tooltip_vars, hide.legend, gopts, xnum_breaks, extra_layer) {
  moduleServer(id, function(input, output, session) {
    
    #display message if data not available
    output$messageDisplay <- renderUI({
      df <- filtered_data_func()
      #df <- filtered_data_func
      if (is.null(df)) {
        h3("No data available for this selection", align = "center")
      } else {
        NULL  # Don't show message if data is present
      }
    })
    
    output$valuePlot <- renderPlotly({
      df <- filtered_data_func()
      req(df)
      
      # Extend last point in step plot
      if ("step" %in% gopts) {
        
        max_x <- max(df[[x_var]], na.rm = TRUE)  # Find max x value
        extension_x <- max_x * 1.1  # Extend by 5%
        
        # Identify the last x-value for each group in color_var
        last_points <- do.call(rbind, lapply(split(df, df[[color_var]]), function(sub_df) {
          last_row <- sub_df[sub_df[[x_var]] == max(sub_df[[x_var]], na.rm = TRUE), , drop = FALSE]
          last_row[[x_var]] <- extension_x  # Extend x-axis
          
          return(last_row)
          
        }))
        
        # Combine original data with the extended points
        df <- rbind(df, last_points)
        
        # delete missing color_var
        df <- df[!is.na(df[[color_var]]),]
      }
      
      # Define x-axis breaks dynamically
      if (!is.null(xnum_breaks)) {
        x_range <- range(df[[x_var]], na.rm = TRUE)
        breaks_x <- pretty(x_range, n = xnum_breaks)
      } else {
        breaks_x <- NULL
      }
      
      #define tooltip
      if (!is.null(tooltip_vars)) {
        # Check if names are provided
        if (is.null(names(tooltip_vars))) {
          # No labels provided, use variable names as labels
          tooltip_vars <- setNames(tooltip_vars, tooltip_vars)
        }
        # Construct tooltip text using labels explicitly
        df$tooltip_text <- apply(df[, names(tooltip_vars), drop = FALSE], 1, function(row) {
          paste(sapply(names(tooltip_vars), function(name) {
            paste(tooltip_vars[[name]], ": ", row[[name]])
          }), collapse = "<br>")
        })
      } else {
        # No tooltip if not specified
        df$tooltip_text <- ""
      }
      
      if (!is.null(facet_var) && facet_var != "null") {
        
        
        df <- df[!is.na(df[[facet_var]]), ]
        facet_levels <- unique(df[[facet_var]])
        
        plots <- lapply(facet_levels, function(facet_level) {
          
          df_facet <- df[df[[facet_var]] == facet_level, ]
          
          plt <- plot_ly(
            df_facet,
            x = ~get(x_var),
            y = ~get(y_var),
            color = ~get(color_var),
            colors = viridis_pal(option = "viridis")(length(unique(df[[color_var]]))),
            text = ~tooltip_text,
            hoverinfo = 'text',
            type = 'scatter',
            mode = ifelse("point" %in% gopts, "lines+markers", "lines"),
            fill = ifelse("area" %in% gopts, 'tozeroy', 'none'),
            line = list(shape = ifelse("step" %in% gopts, 'hv', 'linear')),
            showlegend = (facet_level == facet_levels[1])
          ) %>% layout(
            title = facet_level,
            xaxis = list(title = x_var_lab),
            yaxis = list(title = y_var_lab)
          )
          
          plt
        })
        
        pp <- subplot(plots, nrows = ceiling(sqrt(length(plots))), shareX = TRUE, shareY = TRUE) %>%
          layout(height = 700)
        
      } else {
        # Define plot
        p <- ggplot(df, aes(
          x = .data[[x_var]],
          y = .data[[y_var]],
          group = .data[[color_var]],
          color = .data[[color_var]],
          fill = .data[[color_var]],
          text = tooltip_text)) +
          scale_color_viridis(discrete = TRUE, option = "viridis", direction = 1, end = 0.9, alpha = 0.9) +
          scale_fill_viridis(discrete = TRUE, option = "viridis", direction = 1, end = 0.9, alpha = 0.9) +
          labs(title = "", x = "", y = "", color = color_var_lab) +
          guides(color = guide_legend(override.aes = list(alpha = 1))) +
          theme(panel.background = element_blank(), panel.grid.major = element_blank(),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12),
                axis.text.x = element_text(size = 12, angle = 40, hjust = 1),
                axis.text.y = element_text(size = 12),
                legend.title = element_text(size = 12, face = "bold"),
                legend.text = element_text(size = 12),
                legend.position = if (hide.legend) "none" else "center")
        
        # Apply facet if facet_var is not NULL
        if (!is.null(facet_var) && facet_var != "null") {
          p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "fixed")
        }
        
        #modify scale if necessary
        if (!is.null(breaks_x)) {
          p <- p + scale_x_continuous(breaks = breaks_x)
        }
        
        # ADD GRAY BACKGROUND LAYERS
        grouping_aes <- if (!is.null(facet_var) && facet_var != "null") {
          interaction(df[[color_var]], df[[facet_var]])
        } else {
          df[[color_var]]
        }
        
        # ADD GREY BACKGROUND LAYERS
        if ("area" %in% gopts) {
          p <- p + geom_area(
            data = df,
            aes(
              x = .data[[x_var]],
              y = .data[[y_var]],
              group = grouping_aes
              #text = tooltip_text
            ),
            fill = "lightgrey",
            color = NA,
            alpha = 0.6,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
        }
        
        if ("line" %in% gopts) {
          p <- p + geom_line(data = df,
                             aes(x = .data[[x_var]], y = .data[[y_var]], group = .data[[color_var]]),
                             color = "lightgray", alpha = 1, inherit.aes = FALSE,
                             linewidth = 0.6, linetype = 1)
        }
        if ("point" %in% gopts) {
          p <- p + geom_point(data = df,
                              aes(x = .data[[x_var]], y = .data[[y_var]], group = .data[[color_var]]),
                              color = "lightgray", alpha = 1, inherit.aes = FALSE, size = 0.9)
        }
        if ("step" %in% gopts) {
          p <- p + geom_step(data = df,
                             aes(x = .data[[x_var]], y = .data[[y_var]], group = .data[[color_var]]),
                             color = "lightgray", alpha = 1, inherit.aes = FALSE, size = 0.9, direction = "hv")
        }
        
        #ADD INTERACTIVE LAYERS
        if ("area" %in% gopts) {
          p <- p + geom_area(alpha = 1,
                             color = NA,  #"black",
                             inherit.aes = TRUE
          )
          p <- p + scale_y_continuous(labels = comma)
        }
        
        # Conditional addition of geom_line
        if ("line" %in% gopts) {
          p <- p + geom_line(linewidth=0.8, linetype=1, alpha=0.9)
        }
        
        # Conditional addition of geom_point
        if ("point" %in% gopts) {
          p <- p + geom_point(size = 1)
        }
        
        # Conditional addition of geom_point
        if ("step" %in% gopts) {
          p <- p + geom_step(size = 1, direction = "hv")
        }
        
        #add y title if y_var_label not empty
        if (!is.null(y_var_lab)) {
          p <- p + ylab(y_var_lab)
        }
        
        #add x title if x_var_label not empty
        if (!is.null(x_var_lab)) {
          p <- p + xlab(x_var_lab)
        }
        
        #MAKE INTERACTIVE GRAPH
        pp <- ggplotly(p, tooltip = "text", height = 700) %>%
          style(hoverinfo = "text") %>%
          layout(
            dragmode = "zoom",
            font = list(family = "Arial", size = 12, color = "#000"),
            autosize = TRUE,
            xaxis = list(zeroline = FALSE),
            yaxis = list(zeroline = FALSE),
            legend = list(
              title = list(text = ''),
              #traceorder = "grouped",
              orientation = "h",
              x = 0.5,
              itemclick = "toggleothers",
              itemdoubleclick = "none",
              xanchor = "center",
              y = -0.3)
          ) %>%
          config(displaylogo = FALSE,
                 modeBarButtonsToRemove = list(
                   "autoScale2d", "resetScale2d", "hoverClosestCartesian",
                   "toggleSpikelines", "lasso2d", "hoverCompareCartesian",
                   "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
                   "toImage", "sendDataToCloud", "hoverClosestGl2d",
                   "hoverClosestPie", "toggleHover", "resetViews",
                   "resetViewMapbox", "select2d", "zoom"
                 )
          )
        
        pp
        
      }
    })
  })
}

# 
# 
# require(ggplot2)
# require(plotly)
# require(viridis)
# require(scales)
# 
# plotOutputUI <- function(id) {
#   ns <- NS(id)
#   fluidRow(
#     uiOutput(ns("messageDisplay")),   # UI element for displaying messages
#     plotlyOutput(ns("valuePlot"), width = "100%")
#   )
# }
# 
# # Server logic for the plot module
# plotModuleServer <- function(id, filtered_data_func, x_var, x_var_lab, y_var, y_var_lab, color_var, color_var_lab, facet_var, facet_var_lab, tooltip_vars, hide.legend, gopts, xnum_breaks, extra_layer) {
#   moduleServer(id, function(input, output, session) {
# 
#     #display message if data not available
#     output$messageDisplay <- renderUI({
#       df <- filtered_data_func()
#       #df <- filtered_data_func
#       if (is.null(df)) {
#         h3("No data available for this selection", align = "center")
#       } else {
#         NULL  # Don't show anything if data is present
#       }
#     })
# 
#     output$valuePlot <- renderPlotly({
#       df <- filtered_data_func()
#       req(df)
# 
#        # Extend last point in step plot
#        if ("step" %in% gopts) {
# 
#          max_x <- max(df[[x_var]], na.rm = TRUE)  # Find max x value
#          extension_x <- max_x * 1.1  # Extend by 5%
# 
#          # Identify the last x-value for each group in color_var
#          last_points <- do.call(rbind, lapply(split(df, df[[color_var]]), function(sub_df) {
#            last_row <- sub_df[sub_df[[x_var]] == max(sub_df[[x_var]], na.rm = TRUE), , drop = FALSE]
#            last_row[[x_var]] <- extension_x  # Extend x-axis
# 
#          return(last_row)
# 
#          }))
# 
#          # Combine original data with the extended points
#          df <- rbind(df, last_points)
# 
#          # delete missing color_var
#          df <- df[!is.na(df[[color_var]]),]
#        }
# 
#       # Define x-axis breaks dynamically
#       if (!is.null(xnum_breaks)) {
#         x_range <- range(df[[x_var]], na.rm = TRUE)
#         breaks_x <- pretty(x_range, n = xnum_breaks)
#       } else {
#         breaks_x <- NULL
#       }
# 
#       #define tooltip
#       if (!is.null(tooltip_vars)) {
#         # Check if names are provided
#         if (is.null(names(tooltip_vars))) {
#           # No labels provided, use variable names as labels
#           tooltip_vars <- setNames(tooltip_vars, tooltip_vars)
#         }
#         # Construct tooltip text using labels explicitly
#         df$tooltip_text <- apply(df[, names(tooltip_vars), drop = FALSE], 1, function(row) {
#           paste(sapply(names(tooltip_vars), function(name) {
#             paste(tooltip_vars[[name]], ": ", row[[name]])
#           }), collapse = "<br>")
#         })
#       } else {
#         # No tooltip if not specified
#         df$tooltip_text <- ""
#       }
# 
#       # Define plot
#       p <- ggplot(df, aes(
#             x = .data[[x_var]],
#             y = .data[[y_var]],
#             group = .data[[color_var]],
#             color = .data[[color_var]],
#             fill = .data[[color_var]],
#             text = tooltip_text)) +
#         scale_color_viridis(discrete = TRUE, option = "viridis", direction = 1, end = 0.9, alpha = 0.9) +
#         scale_fill_viridis(discrete = TRUE, option = "viridis", direction = 1, end = 0.9, alpha = 0.9) +
#         labs(title = "", x = "", y = "", color = color_var_lab) +
#         guides(color = guide_legend(override.aes = list(alpha = 1))) +
#         theme(panel.background = element_blank(), panel.grid.major = element_blank(),
#               axis.title.x = element_text(size = 12),
#               axis.title.y = element_text(size = 12),
#               axis.text.x = element_text(size = 12, angle = 40, hjust = 1),
#               axis.text.y = element_text(size = 12),
#               legend.title = element_text(size = 12, face = "bold"),
#               legend.text = element_text(size = 12),
#               legend.position = if (hide.legend) "none" else "center")
# 
#       # Apply facet if facet_var is not NULL
#       if (!is.null(facet_var) && facet_var != "null") {
#         p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "fixed")
#       }
# 
#       #modify scale if necessary
#       if (!is.null(breaks_x)) {
#         p <- p + scale_x_continuous(breaks = breaks_x)
#       }
# 
#       # ADD GRAY BACKGROUND LAYERS
#       grouping_aes <- if (!is.null(facet_var) && facet_var != "null") {
#         interaction(df[[color_var]], df[[facet_var]])
#       } else {
#         df[[color_var]]
#       }
# 
#       # ADD GREY BACKGROUND LAYERS
#       if ("area" %in% gopts) {
#         p <- p + geom_area(
#           data = df,
#           aes(
#             x = .data[[x_var]],
#             y = .data[[y_var]],
#             group = grouping_aes
#             #text = tooltip_text
#           ),
#           fill = "lightgrey",
#           color = NA,
#           alpha = 0.6,
#           inherit.aes = FALSE,
#           show.legend = FALSE
#         )
#       }
# 
#       if ("line" %in% gopts) {
#         p <- p + geom_line(data = df,
#                     aes(x = .data[[x_var]], y = .data[[y_var]], group = .data[[color_var]]),
#                     color = "lightgray", alpha = 1, inherit.aes = FALSE,
#                     linewidth = 0.6, linetype = 1)
#       }
#       if ("point" %in% gopts) {
#         p <- p + geom_point(data = df,
#                      aes(x = .data[[x_var]], y = .data[[y_var]], group = .data[[color_var]]),
#                      color = "lightgray", alpha = 1, inherit.aes = FALSE, size = 0.9)
#       }
#       if ("step" %in% gopts) {
#         p <- p + geom_step(data = df,
#                     aes(x = .data[[x_var]], y = .data[[y_var]], group = .data[[color_var]]),
#                     color = "lightgray", alpha = 1, inherit.aes = FALSE, size = 0.9, direction = "hv")
#       }
# 
#       #ADD INTERACTIVE LAYERS
#       if ("area" %in% gopts) {
#         p <- p + geom_area(alpha = 1,
#                            color = NA,  #"black",
#                            inherit.aes = TRUE
#                            )
#         p <- p + scale_y_continuous(labels = comma)
#       }
# 
#       # Conditional addition of geom_line
#       if ("line" %in% gopts) {
#         p <- p + geom_line(linewidth=0.8, linetype=1, alpha=0.9)
#       }
# 
#       # Conditional addition of geom_point
#       if ("point" %in% gopts) {
#         p <- p + geom_point(size = 1)
#       }
# 
#       # Conditional addition of geom_point
#       if ("step" %in% gopts) {
#          p <- p + geom_step(size = 1, direction = "hv")
#       }
# 
#       #add y title if y_var_label not empty
#       if (!is.null(y_var_lab)) {
#         p <- p + ylab(y_var_lab)
#       }
# 
#       #add x title if x_var_label not empty
#       if (!is.null(x_var_lab)) {
#         p <- p + xlab(x_var_lab)
#       }
# 
#       #MAKE INTERACTIVE GRAPH
#       pp <- ggplotly(p, tooltip = "text", height = 700) %>%
#         style(hoverinfo = "text") %>%
#         layout(
#           dragmode = "zoom",
#           font = list(family = "Arial", size = 12, color = "#000"),
#           autosize = TRUE,
#           xaxis = list(zeroline = FALSE),
#           yaxis = list(zeroline = FALSE),
#           legend = list(
#             title = list(text = ''),
#             #traceorder = "grouped",
#             orientation = "h",
#             x = 0.5,
#             itemclick = "toggleothers",
#             itemdoubleclick = "none",
#             xanchor = "center",
#             y = -0.3)
#         ) %>%
#         config(displaylogo = FALSE,
#                modeBarButtonsToRemove = list(
#                  "autoScale2d", "resetScale2d", "hoverClosestCartesian",
#                  "toggleSpikelines", "lasso2d", "hoverCompareCartesian",
#                  "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
#                  "toImage", "sendDataToCloud", "hoverClosestGl2d",
#                  "hoverClosestPie", "toggleHover", "resetViews",
#                  "resetViewMapbox", "select2d", "zoom"
#                )
#          )
# 
#       # fix legend duplication if faceted plot
#       if (!is.null(facet_var) && facet_var != "null") {
#         for (i in seq_along(pp$x$data)) {
#           if (!is.null(pp$x$data[[i]]$name)) {
#             clean_name <- gsub("^\\(|,.*$", "", pp$x$data[[i]]$name)
#             # Assign cleaned name to legendgroup (common across facets)
#             pp$x$data[[i]]$legendgroup <- clean_name
#             # Keep unique trace name internally (maintain original interactivity)
#             pp$x$data[[i]]$name <- clean_name
#             # first occurrence of each legendgroup gets showlegend = TRUE, others FALSE
#             if (!exists("legend_shown")) legend_shown <- character(0)
#             if (!(clean_name %in% legend_shown)) {
#               pp$x$data[[i]]$showlegend <- TRUE
#               legend_shown <- c(legend_shown, clean_name)
#             } else {
#               pp$x$data[[i]]$showlegend <- FALSE
#             }
#           }
#         }
#       }
# 
#       # Explicitly disable hoverinfo for the grey background layers
#       for (i in seq_along(pp$x$data)) {
#         trace <- pp$x$data[[i]]
# 
#         # Remove tooltip from traces with no or empty text
#         if (is.null(trace$text) || all(trace$text == "")) {
#           pp$x$data[[i]]$hoverinfo <- "skip"
#         }
# 
#         # Remove "trace 0" legend entry explicitly
#         if (!is.null(trace$name) && trace$name %in% c("trace 0", "lightgrey", "")) {
#           pp$x$data[[i]]$showlegend <- FALSE
#           pp$x$data[[i]]$hoverinfo <- "skip"
#         }
#       }
#       pp
# 
#     })
#   })
# }