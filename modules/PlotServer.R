require(ggplot2)
require(plotly)
require(viridis)
require(paletteer)
require(scales)

plotOutputUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    uiOutput(ns("messageDisplay")),   # UI element for displaying messages
    plotlyOutput(ns("valuePlot"), width = "100%")
  )
}

# Server logic for the plot module
plotModuleServer <- function(id, filtered_data_func, x_var, x_var_lab, y_var, y_var_lab, 
                             color_var, color_var_lab, facet_var, facet_var_lab, tooltip_vars, 
                             hide.legend, gopts, xnum_breaks, extra_layer, color_style,
                             plot_height, groupvars) {
  moduleServer(id, function(input, output, session) {

    #display message if data not available
    output$messageDisplay <- renderUI({
      df <- filtered_data_func()
      if (is.null(df)) {
        h3("No data available for this selection", align = "center")
      } else {
        NULL  # Don't show message if data is present
      }
    })

    output$valuePlot <- renderPlotly({
      df <- filtered_data_func()
      req(df)
      
      #make axes dynamic if necessary 
      # print(paste0("printing input x_axis", input, "!"))
      # if (!is.null(input$x_axis)) x_var <- input$x_axis
      # if (!is.null(input$y_axis)) y_var <- input$y_axis
  
      # Extend last point in step plot
      if ("step" %in% gopts) {

        max_x <- max(df[[x_var]], na.rm = TRUE)  # Find max x value
        extension_x <- max_x * 1.1  

        # Identify the last x-value for each group in color_var
        last_points <- do.call(rbind, lapply(split(df, df[[color_var]]), function(sub_df) {
          last_row <- sub_df[sub_df[[x_var]] == max(sub_df[[x_var]], na.rm = TRUE), , drop = FALSE]
          # sub_df <- sub_df[!is.na(sub_df[[x_var]]), , drop = FALSE]
          # if (nrow(sub_df) == 0) {
          #   return(NULL)
          # }
          # last_row <- sub_df[which.max(sub_df[[x_var]]), , drop = FALSE]
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
            if (name == y_var) {
              value <- round(as.numeric(row[[name]]), 2)  # Round to 2 decimals
              formatted_value <- ifelse(
                value == floor(value),  # Check if it's an integer
                formatC(value, format = "f", big.mark = ",", digits = 0),  # No decimals
                formatC(value, format = "f", big.mark = ",", digits = 2)   # Show 2 decimals
              )
              paste(tooltip_vars[[name]], " ", formatted_value)
            }  else {
              paste(tooltip_vars[[name]], " ", row[[name]])
            }
          }), collapse = "<br>")
        })

      } else {
        # No tooltip if not specified
        df$tooltip_text <- ""
      }
   
      if (!is.null(extra_layer)){

        # Separate extra layer
        extra_df <- df %>%
          filter(!!sym(color_var) %in% extra_layer$values)

        # Remove info from df
        df <- df %>%
          filter(!(!!sym(color_var) %in% extra_layer$values))
      }
      
      #choose color palette 
      n_grp <- length(unique(df[[color_var]]))  
      pal_name <- color_style     
      pal <- as.vector(                         
        paletteer_d(
          palette   = pal_name,  
          n         = n_grp,     
          direction = 1,          # -1 to reverse
          type      = "continuous"  
        )
      )

      #use plotly directly if dealing with facets
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
            colors = pal, 
            #colors = viridis_pal(option = color_style)(length(unique(df[[color_var]]))),
            text = ~tooltip_text,
            hoverinfo = 'text',
            type = 'scatter',
            mode = ifelse("point" %in% gopts, "lines+markers", "lines"),
            fill = ifelse("area" %in% gopts, 'tozeroy', 'none'),
            line = list(shape = ifelse("step" %in% gopts, 'hv', 'linear')),
            #stackgroup = ifelse("area" %in% gopts, "one", NULL),
            height = plot_height,
            opacity = 1,
            legendgroup = ~get(color_var),
            showlegend = !hide.legend && (facet_level == facet_levels[1])
          ) %>% layout(
            dragmode = "zoom",
            xaxis = list(title = x_var_lab), 
            yaxis = list(title = y_var_lab) 
          )
          
          # Add extra layer dynamically
          if (!is.null(extra_layer)) {
            
            df_extra_facet <- extra_df[extra_df[[facet_var]] == facet_level, ]
            
            if (nrow(df_extra_facet) > 0) {
              plt <- plt %>%
                add_trace(
                  data = df_extra_facet, 
                  x = ~get(x_var),
                  y = ~get(y_var),
                  type = 'scatter',
                  mode = extra_layer$type,
                  line = list(color = "black", width = 2),
                  fill = NULL,
                  name = extra_layer$values,
                  text = ~tooltip_text,
                  hoverinfo = 'text',
                  color = I("black"), 
                  showlegend = !hide.legend && (facet_level == facet_levels[1]),
                  legendgroup = "extra_layer"
                ) %>% 
                layout(
                  dragmode = "zoom"
                )
            }
          }
          plt
        })
        
        # Compute grid layout for facets
        n_facets <- length(facet_levels)
        nrows <- ceiling(sqrt(n_facets))
        ncols <- ceiling(n_facets / nrows)
        
        # Generate subplot
        # Compute global axis limits
        x_range <- range(df[[x_var]], na.rm = TRUE)
        
         if (!is.null(extra_layer)) {
           combined_y <- c(df[[y_var]], extra_df[[y_var]])
           y_range <- range(combined_y, na.rm = TRUE)
         } else {
            y_range <- range(df[[y_var]], na.rm = TRUE)
        }
        
        # Generate subplot with fixed axis ranges
        pp <- subplot(plots, nrows = nrows, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
          layout(
            dragmode = "zoom",
            hovermode = "closest",
            #xaxis = list(fixedrange = TRUE),
            #yaxis = list(fixedrange = TRUE),
            annotations = lapply(seq_along(facet_levels), function(i) {
              list(
                x = ((i - 1) %% ncols) / ncols + 0.5 / ncols,  
                y = 1 - (floor((i - 1) / ncols) / nrows), #+ 0.05,  
                text = facet_levels[i],  
                showarrow = FALSE,  
                xanchor = "center",
                xref = "paper",  
                yref = "paper",  
                bgcolor = "white",  
                bordercolor = "lightgrey",  
                traceorder = "grouped",
                borderwidth = 1,  
                opacity = 1,
                font = list(size = 14, color = "black", family = "Arial")  
              )
            }),
            xaxis = list(
              range = x_range, 
              title = x_var_lab, 
              zeroline = FALSE 
            ),  
            yaxis = list(
              range = y_range,
              title = y_var_lab,
              zeroline = FALSE
            ),
            legend = if (!hide.legend) list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.3,
              yanchor = "top",
              itemclick = "toggleothers",
              itemdoubleclick = "exclusive"
            ) else list(),
            opacity = 1
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

      } else {
        
        #define grouping variable 
        group_expr <- if (!is.null(groupvars) && length(groupvars) > 0) {
          expr(interaction(!!!syms(groupvars), drop = TRUE))
        } else {
          sym(color_var)
        }
        
        # Single faceted plots with ggplotly 
        p <- ggplot(df, aes(
          x = .data[[x_var]],
          y = .data[[y_var]],
          group = !!group_expr,
          color = .data[[color_var]],
          fill = .data[[color_var]],
          text = tooltip_text)) +
          scale_colour_manual(values = pal) + 
          scale_fill_manual(values = pal) + 
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
                             aes(x = .data[[x_var]], y = .data[[y_var]], group = !!group_expr),
                             color = "lightgray", alpha = 1, inherit.aes = FALSE,
                             linewidth = 0.6, linetype = 1)
        }
        if ("point" %in% gopts) {
          p <- p + geom_point(data = df,
                              aes(x = .data[[x_var]], y = .data[[y_var]], group = !!group_expr),
                              color = "lightgray", alpha = 1, inherit.aes = FALSE, size = 2)
        }
        if ("step" %in% gopts) {
          p <- p + geom_step(data = df,
                             aes(x = .data[[x_var]], y = .data[[y_var]], group = !!group_expr),
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
          p <- p + geom_point(size = 2)
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
        pp <- ggplotly(p, tooltip = "text", height = plot_height) %>%
          style(hoverinfo = "text") %>%
          layout(
            dragmode = "zoom",
            font = list(family = "Arial", size = 12, color = "#000"),
            autosize = TRUE,
            xaxis = list(zeroline = FALSE),
            yaxis = list(zeroline = FALSE),
            legend = list(
              title = list(text = ''),
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
