require(ggplot2)
require(plotly)
require(viridis)
require(paletteer)
require(scales)

plot_text_style <- list(
  family = "Arial",
  color = "black",
  axis_title_size = 16,
  axis_tick_size = 16,
  legend_size = 16,
  hover_size = 18,
  facet_size = 18,
  data_label_size = 16
)

plotly_font <- function(size = plot_text_style$legend_size,
                        color = plot_text_style$color,
                        family = plot_text_style$family) {
  list(family = family, size = size, color = color)
}

plotly_axis_style <- function(axis = list(),
                              title_color = plot_text_style$color,
                              tick_color = title_color,
                              show_grid = NULL) {
  axis$titlefont <- plotly_font(plot_text_style$axis_title_size, title_color)
  axis$tickfont <- plotly_font(plot_text_style$axis_tick_size, tick_color)
  if (!is.null(show_grid)) {
    axis$showgrid <- isTRUE(show_grid)
  }
  axis
}

plotly_hoverlabel_style <- function(font_color = plot_text_style$color,
                                    bgcolor = NULL,
                                    bordercolor = NULL) {
  hoverlabel <- list(font = plotly_font(plot_text_style$hover_size, font_color))
  if (!is.null(bgcolor)) hoverlabel$bgcolor <- bgcolor
  if (!is.null(bordercolor)) hoverlabel$bordercolor <- bordercolor
  hoverlabel
}

plotly_legend_style <- function(legend = list()) {
  legend$font <- plotly_font(plot_text_style$legend_size)
  if (is.null(legend$title)) {
    legend$title <- list(font = plotly_font(plot_text_style$legend_size))
  } else if (is.list(legend$title)) {
    legend$title$font <- plotly_font(plot_text_style$legend_size)
  }
  legend
}

plotly_colorbar_style <- function(title = NULL) {
  list(
    title = list(
      text = title,
      font = plotly_font(plot_text_style$legend_size)
    ),
    tickfont = plotly_font(plot_text_style$legend_size)
  )
}

apply_plotly_axis_text_style <- function(pp, show_grid = NULL) {
  axis_names <- names(pp$x$layout)[grepl("^[xy]axis[0-9]*$", names(pp$x$layout))]
  for (axis_name in axis_names) {
    pp$x$layout[[axis_name]] <- plotly_axis_style(pp$x$layout[[axis_name]], show_grid = show_grid)
  }
  pp
}

plotOutputUI <- function(id,
                         show_stack_toggle = FALSE,
                         stacked_default = FALSE,
                         enable_only_when_area = TRUE,
                         gopts = NULL) {
  ns <- NS(id)
  fluidRow(
    uiOutput(ns("messageDisplay")),
    if (isTRUE(show_stack_toggle) &&
        (!isTRUE(enable_only_when_area) || (!is.null(gopts) && ("area" %in% gopts)))) {
      div(
        style = "margin: 10px 10px;",
        shinyWidgets::materialSwitch(
          inputId   = ns("stacked"), 
          label = "Stack areas",
          status = "primary",
          right = TRUE)
      )
    } else NULL,
    plotlyOutput(ns("valuePlot"), width = "100%")
  )
}


# Server logic for the plot module
plotModuleServer <- function(id, filtered_data_func, x_var, x_var_lab, y_var, y_var_lab, 
                             y2_var = NULL, y2_var_lab = NULL,
                             color_var = NULL, color_var_lab, facet_var, facet_var_lab,
                             facet_label_var = NULL, facet_label_truncate_after_dash = FALSE,
                             facet_label_max_words = NULL, facet_label_size = NULL,
                             tooltip_vars,
                             hide.legend, gopts, xnum_breaks, extra_layer, color_style,
                             plot_height, groupvars, stacked_default = FALSE,
                             x_scale = NULL, scatter_options = NULL, show.grid = TRUE) {
  moduleServer(id, function(input, output, session) {
    
    stack_active <- reactive({
      val <- input$stacked
      if (is.null(val)) isTRUE(stacked_default) else isTRUE(val)
    })

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
      req(df, cancelOutput = TRUE)

      resolveValue <- function(val) {
        if (is.reactive(val) || (is.function(val) && !is.null(environment(val)))) {
          return(val())
        }
        val
      }

      x_var <- resolveValue(x_var)
      x_var_lab <- resolveValue(x_var_lab)
      y_var <- resolveValue(y_var)
      y_var_lab <- resolveValue(y_var_lab)
      y2_var <- resolveValue(y2_var)
      y2_var_lab <- resolveValue(y2_var_lab)
      x_scale <- resolveValue(x_scale)
      if (is.null(x_scale)) x_scale <- "regular"
      show.grid <- resolveValue(show.grid)
      if (is.null(show.grid)) show.grid <- TRUE
      axis_show_grid <- if (isTRUE(show.grid)) NULL else FALSE
      
      #make axes dynamic if necessary 
      # print(paste0("printing input x_axis", input, "!"))
      # if (!is.null(input$x_axis)) x_var <- input$x_axis
      # if (!is.null(input$y_axis)) y_var <- input$y_axis
  
      x_limits <- NULL

      # Extend last point in step plot
      if ("step" %in% gopts) {
        # Keep only rows with valid x/color for the extension computation
        step_df <- df[!is.na(df[[x_var]]) & !is.na(df[[color_var]]), , drop = FALSE]

        if (nrow(step_df) > 0) {
          # Build step-group IDs consistent with plotting grouping
          if (!is.null(groupvars) && length(groupvars) > 0) {
            valid_groupvars <- groupvars[groupvars %in% names(step_df)]
            if (length(valid_groupvars) > 0) {
              step_group_id <- do.call(interaction, c(step_df[valid_groupvars], list(drop = TRUE, lex.order = TRUE)))
            } else {
              step_group_id <- step_df[[color_var]]
            }
          } else {
            step_group_id <- step_df[[color_var]]
          }

          # Extend to the visible right edge when breaks are requested; otherwise +10%
          x_min <- min(step_df[[x_var]], na.rm = TRUE)
          x_max <- max(step_df[[x_var]], na.rm = TRUE)
          if (!is.null(xnum_breaks)) {
            candidate_breaks <- pretty(c(x_min, x_max), n = xnum_breaks)
            candidate_breaks <- candidate_breaks[is.finite(candidate_breaks)]
            right_edge <- if (length(candidate_breaks) > 0) max(candidate_breaks) else NA_real_
            extension_x <- if (!is.na(right_edge) && right_edge > x_max) right_edge else x_max * 1.1
          } else {
            extension_x <- x_max * 1.1
          }

          # Pick one last row per step group and append an extended point
          idx_by_group <- split(seq_len(nrow(step_df)), step_group_id)
          last_idx <- vapply(
            idx_by_group,
            function(ix) ix[which.max(step_df[[x_var]][ix])],
            integer(1)
          )
          last_points <- step_df[last_idx, , drop = FALSE]
          last_points[[x_var]] <- extension_x

          # Combine original data with the extended points
          df <- rbind(df, last_points)
        }

        # Keep valid series rows for step plotting.
        # NA x rows (for example bracket code 0 rows) break the path and can hide
        # the visual tail extension, so remove them here.
        df <- df[!is.na(df[[color_var]]) & !is.na(df[[x_var]]), ]

        # Keep deterministic order within groups for path-based geoms.
        if (!is.null(groupvars) && length(groupvars) > 0) {
          order_vars <- unique(c(groupvars[groupvars %in% names(df)], x_var))
        } else {
          order_vars <- unique(c(color_var, x_var))
        }
        if (length(order_vars) > 0) {
          df <- df[do.call(order, df[order_vars]), , drop = FALSE]
        }
        x_limits <- range(df[[x_var]], na.rm = TRUE)
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
        
        format_tooltip_value <- function(value, name) {
          if (identical(name, "year")) {
            return(value)
          }

          numeric_value <- suppressWarnings(as.numeric(value))
          if (!is.na(numeric_value)) {
            return(formatC(numeric_value, format = "f", big.mark = ",", digits = 2))
          }

          value
        }

        # Construct tooltip text using labels explicitly
        df$tooltip_text <- apply(df[, names(tooltip_vars), drop = FALSE], 1, function(row) {
          paste(sapply(names(tooltip_vars), function(name) {
            paste(tooltip_vars[[name]], " ", format_tooltip_value(row[[name]], name))
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
      if (!is.null(color_var)) {
        n_grp <- length(unique(df[[color_var]]))  
      }
      
      pal_name <- color_style   
      if (!is.null(color_style) && !"bar" %in% gopts) {
        pal <- as.vector(                         
          paletteer_d(
            palette   = pal_name,  
            n         = n_grp,     
            direction = 1,          # -1 to reverse
            type      = "continuous"  
          )
        )
      }

      if ("dual_axis_line" %in% gopts) {
        validate(need(x_var %in% names(df), paste0("x_var '", x_var, "' not in df")))
        validate(need(y_var %in% names(df), paste0("y_var '", y_var, "' not in df")))
        validate(need(!is.null(y2_var) && y2_var %in% names(df), paste0("y2_var '", y2_var, "' not in df")))

        df <- df %>%
          dplyr::filter(!is.na(.data[[x_var]])) %>%
          dplyr::arrange(.data[[x_var]])

        left_ok <- any(!is.na(df[[y_var]]))
        right_ok <- any(!is.na(df[[y2_var]]))
        validate(need(left_ok || right_ok, "No data available for the selected axes"))

        left_name <- if (!is.null(y_var_lab) && nzchar(y_var_lab)) y_var_lab else y_var
        right_name <- if (!is.null(y2_var_lab) && nzchar(y2_var_lab)) y2_var_lab else y2_var
        x_name <- if (!is.null(x_var_lab) && nzchar(x_var_lab)) x_var_lab else x_var
        axis_left_col <- "#2C6DB2"
        axis_right_col <- "#E6A21A"

        fmt_num <- function(v) {
          ifelse(
            is.na(v),
            "NA",
            formatC(as.numeric(v), format = "f", digits = 2, big.mark = ",")
          )
        }

        hover_left <- paste0(
          "<b>", left_name, "</b>: ", fmt_num(df[[y_var]]),
          "<br><b>", x_name, "</b>: ", df[[x_var]],
          if ("GEO_long" %in% names(df)) paste0("<br><b>Country</b>: ", df$GEO_long) else "",
          if ("d2_label" %in% names(df)) paste0("<br><b>Tax type</b>: ", df$d2_label) else "",
          "<extra></extra>"
        )
        hover_right <- paste0(
          "<b>", right_name, "</b>: ", fmt_num(df[[y2_var]]),
          "<br><b>", x_name, "</b>: ", df[[x_var]],
          if ("GEO_long" %in% names(df)) paste0("<br><b>Country</b>: ", df$GEO_long) else "",
          if ("d2_label" %in% names(df)) paste0("<br><b>Tax type</b>: ", df$d2_label) else "",
          "<extra></extra>"
        )

        # Start with an empty widget and add only explicit traces.
        # Initializing with x but no y creates an unintended default "trace 0".
        pp <- plot_ly(height = plot_height)

        if (left_ok) {
          pp <- pp %>%
            add_trace(
              data = df,
              x = ~get(x_var),
              y = ~get(y_var),
              type = "scatter",
              mode = "lines+markers",
              name = left_name,
              showlegend = !hide.legend,
              line = list(color = axis_left_col, width = 2),
              marker = list(color = axis_left_col, size = 5),
              hovertemplate = hover_left
            )
        }

        if (right_ok) {
          pp <- pp %>%
            add_trace(
              data = df,
              x = ~get(x_var),
              y = ~get(y2_var),
              type = "scatter",
              mode = "lines+markers",
              name = right_name,
              showlegend = !hide.legend,
              yaxis = "y2",
              line = list(color = axis_right_col, width = 2),
              marker = list(color = axis_right_col, size = 5),
              hovertemplate = hover_right
            )
        }

        pp <- pp %>%
          layout(
            dragmode = "zoom",
            hovermode = "x unified",
            showlegend = !hide.legend,
            font = plotly_font(plot_text_style$legend_size),
            hoverlabel = plotly_hoverlabel_style(),
            xaxis = plotly_axis_style(list(
              title = x_var_lab,
              zeroline = FALSE,
              showgrid = FALSE,
              automargin = TRUE
            )),
            yaxis = plotly_axis_style(list(
              title = left_name,
              zeroline = FALSE,
              showgrid = FALSE,
              automargin = TRUE
            ), title_color = axis_left_col, tick_color = axis_left_col),
            yaxis2 = plotly_axis_style(list(
              title = right_name,
              overlaying = "y",
              side = "right",
              zeroline = FALSE,
              showgrid = FALSE,
              automargin = TRUE
            ), title_color = axis_right_col, tick_color = axis_right_col),
            margin = if (hide.legend) list(b = 12) else NULL,
            legend = if (!hide.legend) plotly_legend_style(list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.2
            )) else list()
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

        return(pp)
      }

      if (isTRUE(scatter_options$enabled)) {
        validate(need(x_var %in% names(df), paste0("x_var '", x_var, "' not in df")))
        validate(need(y_var %in% names(df), paste0("y_var '", y_var, "' not in df")))
        validate(need(!is.null(color_var) && color_var %in% names(df), "No color variable available"))

        df <- df %>%
          dplyr::filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))

        if (identical(x_scale, "log")) {
          df <- df %>% dplyr::filter(.data[[x_var]] > 0)
        }

        validate(need(nrow(df) > 0, "No data available for the selected axes"))

        is_percent_axis <- function(var_name) {
          grepl("percent|rate", var_name, ignore.case = TRUE)
        }

        format_axis_value <- function(value, is_percent = FALSE) {
          suffix <- if (is_percent) "%" else ""
          paste0(formatC(value, format = "f", big.mark = ",", digits = 2), suffix)
        }

        x_axis_layout <- plotly_axis_style(list(
          title = paste0(
            x_var_lab,
            if (identical(x_scale, "log")) " - Log Scale" else " - Regular Scale"
          ),
          type = if (identical(x_scale, "log")) "log" else NULL,
          zeroline = FALSE,
          automargin = TRUE
        ))
        if (is_percent_axis(x_var)) {
          x_axis_layout$ticksuffix <- "%"
          x_axis_layout$tickformat <- ".2f"
        }
        if (identical(x_scale, "log")) {
          positive_x <- df[[x_var]][is.finite(df[[x_var]]) & df[[x_var]] > 0]
          log_breaks <- pretty(log10(range(positive_x, na.rm = TRUE)), n = if (!is.null(xnum_breaks)) xnum_breaks else 8)
          tick_vals <- 10^log_breaks
          tick_vals <- tick_vals[tick_vals >= min(positive_x, na.rm = TRUE) & tick_vals <= max(positive_x, na.rm = TRUE)]
          if (length(tick_vals) > 0) {
            x_axis_layout$tickmode <- "array"
            x_axis_layout$tickvals <- tick_vals
            x_axis_layout$ticktext <- vapply(
              tick_vals,
              format_axis_value,
              character(1),
              is_percent = is_percent_axis(x_var)
            )
            x_axis_layout$ticksuffix <- NULL
            x_axis_layout$tickformat <- NULL
          }
        }

        y_axis_layout <- plotly_axis_style(list(
          title = y_var_lab,
          zeroline = FALSE,
          automargin = TRUE
        ))
        if (is_percent_axis(y_var)) {
          y_axis_layout$ticksuffix <- "%"
          y_axis_layout$tickformat <- ".2f"
        }

        point_text <- if (isTRUE(scatter_options$show_labels) && "geo_long" %in% names(df)) {
          df$point_label <- df$geo_long
          ~point_label
        } else {
          NULL
        }

        pp <- plot_ly(
          data = df,
          x = ~get(x_var),
          y = ~get(y_var),
          color = ~get(color_var),
          colors = pal,
          text = point_text,
          hovertext = ~tooltip_text,
          hoverinfo = "text",
          type = "scatter",
          mode = if (!is.null(point_text)) "markers+text" else "markers",
          textposition = "bottom center",
          textfont = plotly_font(plot_text_style$data_label_size),
          marker = list(size = 8, opacity = 0.85),
          showlegend = !hide.legend,
          height = plot_height
        ) %>%
          layout(
            dragmode = "zoom",
            hovermode = "closest",
            showlegend = !hide.legend,
            font = plotly_font(plot_text_style$legend_size),
            hoverlabel = plotly_hoverlabel_style(),
            xaxis = x_axis_layout,
            yaxis = y_axis_layout,
            margin = if (hide.legend) list(b = 50, l = 70, r = 40, t = 20) else NULL,
            legend = if (!hide.legend) plotly_legend_style(list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.2
            )) else list()
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

        return(pp)
      }

      #use plotly directly if dealing with facets
      if (!is.null(facet_var) && facet_var != "null") {
        df <- df[!is.na(df[[facet_var]]), ]
        facet_levels <- unique(df[[facet_var]])
        facet_labels <- facet_levels
        if (!is.null(facet_label_var) && facet_label_var %in% names(df)) {
          facet_labels <- vapply(facet_levels, function(facet_level) {
            labels <- unique(df[df[[facet_var]] == facet_level, facet_label_var])
            labels <- labels[!is.na(labels) & nzchar(labels)]
            if (length(labels) > 0) return(as.character(labels[[1]]))
            if ("source" %in% names(df)) {
              fallback <- unique(df[df[[facet_var]] == facet_level, "source"])
              fallback <- fallback[!is.na(fallback) & nzchar(fallback)]
              if (length(fallback) > 0) return(as.character(fallback[[1]]))
            }
            as.character(facet_level)
          }, character(1))
        }
        if (isTRUE(facet_label_truncate_after_dash)) {
          facet_labels <- trimws(sub("\\s+-\\s+.*$", "", facet_labels))
        }
        if (!is.null(facet_label_max_words)) {
          max_words <- suppressWarnings(as.integer(facet_label_max_words))
          if (!is.na(max_words) && max_words > 0) {
            facet_labels <- vapply(facet_labels, function(label) {
              words <- unlist(strsplit(trimws(label), "\\s+"))
              if (length(words) > max_words) {
                paste(c(words[seq_len(max_words)], "..."), collapse = " ")
              } else {
                label
              }
            }, character(1))
          }
        }
        facet_label_font_size <- suppressWarnings(as.numeric(facet_label_size))
        if (is.null(facet_label_size) ||
            length(facet_label_font_size) != 1 ||
            is.na(facet_label_font_size) ||
            facet_label_font_size <= 0) {
          facet_label_font_size <- plot_text_style$facet_size
        }

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
            stackgroup = if (isTRUE(stack_active())) "one" else NULL,  
            height = plot_height,
            opacity = 1,
            legendgroup = ~get(color_var),
            showlegend = !hide.legend && (facet_level == facet_levels[1])
          ) %>% layout(
            dragmode = "zoom",
            font = plotly_font(plot_text_style$legend_size),
            hoverlabel = plotly_hoverlabel_style(),
            xaxis = plotly_axis_style(list(title = x_var_lab)),
            yaxis = plotly_axis_style(list(title = y_var_lab))
          )
          
          # Add extra layer dynamically
          if (!is.null(extra_layer)) {
            
            df_extra_facet <- extra_df[extra_df[[facet_var]] == facet_level, ]
            
            if (nrow(df_extra_facet) > 0) {
              if (!isTRUE(stack_active())) {
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
                    hoverinfo = if (isTRUE(stack_active())) "skip" else "text", 
                    hoverlabel = plotly_hoverlabel_style(
                      font_color = "white",
                      bgcolor = "black",
                      bordercolor = "black"
                    ),
                    color = I("black"), 
                    showlegend = !hide.legend && (facet_level == facet_levels[1]),
                    legendgroup = "extra_layer",
                    inherit = FALSE 
                  ) %>% 
                  layout(
                    dragmode = "zoom"
                  )
              }
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
        
        if (isTRUE(stack_active()) && "area" %in% gopts) {
          stack_groups <- unique(c(facet_var, x_var))
          stack_groups <- stack_groups[stack_groups %in% names(df)]
          stacked_y <- df %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(stack_groups))) %>%
            dplyr::summarise(
              .positive_stack = sum(.data[[y_var]][.data[[y_var]] > 0], na.rm = TRUE),
              .negative_stack = sum(.data[[y_var]][.data[[y_var]] < 0], na.rm = TRUE),
              .groups = "drop"
            )
          combined_y <- c(stacked_y$.positive_stack, stacked_y$.negative_stack)
        } else {
          combined_y <- df[[y_var]]
        }

        if (!is.null(extra_layer)) {
          combined_y <- c(combined_y, extra_df[[y_var]])
        }

        y_range <- range(combined_y, na.rm = TRUE)
        if (all(is.finite(y_range)) && identical(y_range[1], y_range[2])) {
          y_range <- y_range + c(-0.5, 0.5)
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
                text = facet_labels[i],
                showarrow = FALSE,  
                xanchor = "center",
                xref = "paper",  
                yref = "paper",  
                bgcolor = "white",  
                bordercolor = "lightgrey",  
                traceorder = "grouped",
                borderwidth = 1,  
                opacity = 1,
                font = plotly_font(facet_label_font_size)
              )
            }),
            font = plotly_font(plot_text_style$legend_size),
            hoverlabel = plotly_hoverlabel_style(),
            xaxis = plotly_axis_style(list(
              range = x_range, 
              title = x_var_lab, 
              zeroline = FALSE 
            )),
            yaxis = plotly_axis_style(list(
              range = y_range,
              title = y_var_lab,
              zeroline = FALSE
            )),
            legend = if (!hide.legend) plotly_legend_style(list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.3,
              yanchor = "top",
              itemclick = "toggleothers",
              itemdoubleclick = "exclusive"
            )) else list(),
            opacity = 1
          ) %>%
          apply_plotly_axis_text_style(show_grid = axis_show_grid) %>%
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

      } else if ("map" %in% gopts) {
        
        # helper: turn a vector of colors into a plotly colorscale
        make_colorscale <- function(pal) {
          n <- length(pal)
          if (n <= 1) return(list(list(0, pal[1]), list(1, pal[n])))
          lapply(seq_len(n), function(i) list((i-1)/(n-1), pal[i]))
        }

        validate(need(color_var %in% names(df), paste0("color_var '", color_var, "' not in df")))
        validate(need(y_var %in% names(df), paste0("y_var '", y_var, "' not in df")))

        frame_var <- if ("animate" %in% gopts && "year" %in% names(df)) "year" else NULL
        map_group_vars <- unique(c(color_var, frame_var))

        df <- df %>%
          dplyr::filter(!is.na(.data[[color_var]]))

        if (any(duplicated(df[map_group_vars]))) {
          df <- df %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(map_group_vars))) %>%
            dplyr::arrange(
              dplyr::desc(!is.na(.data[[y_var]])),
              dplyr::desc(.data[[y_var]]),
              .by_group = TRUE
            ) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup()
        }
        
        zero_color <- "#F28E8E"
        zero_df <- df %>%
          dplyr::filter(!is.na(.data[[y_var]]), .data[[y_var]] == 0) %>%
          dplyr::mutate(.zero_value = 1)
        nonzero_df <- df %>%
          dplyr::filter(!is.na(.data[[y_var]]), .data[[y_var]] != 0)

        validate(need(nrow(nonzero_df) > 0 || nrow(zero_df) > 0, "No data available for this map selection"))

        pp <- plot_ly(height = plot_height, source = "map")

        if (nrow(nonzero_df) > 0) {
          z_range <- range(nonzero_df[[y_var]], na.rm = TRUE)
          if (identical(z_range[1], z_range[2])) {
            z_range <- z_range + c(-0.5, 0.5)
          }

          pp <- pp %>%
            add_trace(
              data         = nonzero_df,
              type         = "choropleth",
              locations    = ~get(color_var),
              locationmode = "ISO-3",
              z            = ~get(y_var),
              frame        = if (!is.null(frame_var)) ~get(frame_var) else NULL,
              text         = ~tooltip_text,
              hoverinfo    = "text",
              colorscale   = make_colorscale(pal),
              zmin         = z_range[1],
              zmax         = z_range[2],
              colorbar     = plotly_colorbar_style(y_var_lab),
              name         = "Non-zero"
            )
        }

        if (nrow(zero_df) > 0) {
          pp <- pp %>%
            add_trace(
              data         = zero_df,
              type         = "choropleth",
              locations    = ~get(color_var),
              locationmode = "ISO-3",
              z            = ~.zero_value,
              frame        = if (!is.null(frame_var)) ~get(frame_var) else NULL,
              text         = ~tooltip_text,
              hoverinfo    = "text",
              colorscale   = list(list(0, zero_color), list(1, zero_color)),
              zmin         = 0,
              zmax         = 1,
              showscale    = FALSE,
              name         = "Zero"
            )
        }

        pp <- pp %>%
          layout(
            font = plotly_font(plot_text_style$legend_size),
            hoverlabel = plotly_hoverlabel_style(),
            geo = list(
              projection    = list(type = "mercator"),
              showland      = TRUE,
              landcolor     = "rgb(240,240,240)",
              showcountries = TRUE,
              countrycolor  = "rgb(200,200,200)"
            ))

        if (!is.null(frame_var)) {
          pp <- pp %>%
            animation_opts(
              frame      = 400,     # ms per frame (increase -> slower; decrease -> faster)
              transition = 100,     # ms between frames
              easing     = "linear",
              redraw     = T
            ) %>%
            animation_slider(
              currentvalue = list(
                prefix = "Year: ",
                font = plotly_font(plot_text_style$data_label_size)
              ),
              pad = list(t = 30)
            )
        }

        pp <- pp %>%
          config(
            displaylogo = FALSE,
            modeBarButtonsToRemove = list(
              "autoScale2d","resetScale2d","hoverClosestCartesian",
              "toggleSpikelines","lasso2d","hoverCompareCartesian",
              "hoverClosestGeo","toImage","sendDataToCloud",
              "hoverClosestGl2d","hoverClosestPie","toggleHover",
              "resetViews","resetViewMapbox","pan", "select2d"
            )
          )
        pp
        
      } else if ("bar" %in% gopts) {
        
        # # ---- single panel bar chart ----
        # 
        # # 1) sanity checks
        # validate(need(x_var %in% names(df), paste0("x_var '", x_var, "' not in df")))
        # validate(need(y_var %in% names(df), paste0("y_var '", y_var, "' not in df")))
        # #validate(need(color_var %in% names(df), paste0("color_var '", color_var, "' not in df")))
        # 
        # df <- df %>%
        #   dplyr::filter(!is.na(.data[[y_var]]), .data[[y_var]] != 0)        
        # validate(need(nrow(df) > 0, "No non-zero data to plot"))     
        # 
        # # 2) order x by total y (descending) for a tidy ranking (global / non-animated case)
        # ord_df <- df %>%
        #   dplyr::group_by(.data[[x_var]]) %>%
        #   dplyr::summarise(.val = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop") %>%
        #   { 
        #     if ("hbar" %in% gopts) {
        #       dplyr::arrange(., .val)        # ascending if horiz
        #     } else {
        #       dplyr::arrange(., dplyr::desc(.val))  # descending otherwise
        #     }
        #   }
        # df[[x_var]] <- factor(df[[x_var]], levels = ord_df[[x_var]])
        # 
        # # 3) options
        # horiz    <- "hbar" %in% gopts
        # stacked  <- "stack" %in% gopts
        # use_anim <- ("animate" %in% gopts)
        # 
        # if (use_anim && "year" %in% names(df)) {
        #   max_n_cats <- df |>
        #     dplyr::group_by(year) |>
        #     dplyr::summarise(n = dplyr::n_distinct(.data[[x_var]]), .groups = "drop") |>
        #     dplyr::pull(n) |>
        #     max(na.rm = TRUE)
        # } else {
        #   max_n_cats <- dplyr::n_distinct(df[[x_var]])
        # }
        # px_per_bar <- 22L                                 # CHANGE: tune if you want tighter/looser bars
        # dyn_height <- max(plot_height, 80 + px_per_bar * max_n_cats)  # CHANGE: dynamic figure height
        # 
        # 
        # # 3b) per-year ordering for animation  ----------------------------------------
        # if (use_anim) {
        #   validate(                                              # CHANGE: ensure frames exist
        #     need("year" %in% names(df) && dplyr::n_distinct(df$year) > 1,
        #          "No frames to animate: 'year' missing or only one value")
        #   )
        #   
        #   ord_by_year <- df %>%
        #     dplyr::group_by(year, .data[[x_var]]) %>%
        #     dplyr::summarise(.val = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop") %>%
        #     dplyr::arrange(year, dplyr::desc(.val)) %>%
        #     dplyr::group_by(year) %>%
        #     dplyr::summarise(levels = list(.data[[x_var]]), .groups = "drop")  # CHANGE: levels per year
        #   
        #   levs_by_year <- setNames(ord_by_year$levels, as.character(ord_by_year$year))  # CHANGE
        #   first_year   <- as.character(ord_by_year$year[[1]])                            # CHANGE
        # }
        # 
        # # 4) aesthetics per orientation
        # x_aes <- if (horiz) ~get(y_var) else ~get(x_var)
        # y_aes <- if (horiz) ~get(x_var) else ~get(y_var)
        # 
        # # 5) build the bar chart
        # pp <- plot_ly(
        #   data        = df,
        #   type        = "bar",
        #   x           = x_aes,
        #   y           = y_aes,
        #   colors      = color_style,
        #   showlegend  = !hide.legend,
        #   text        = ~get(x_var),
        #   hovertext   = ~tooltip_text, 
        #   textposition = "inside",
        #   cliponaxis  = FALSE,
        #   orientation = if (horiz) "h" else "v",
        #   height      = dyn_height,
        #   #height      = plot_height,
        #   frame       = if (use_anim) ~as.factor(year) else NULL,   # CHANGE: factor frame values
        #   ids         = ~get(x_var)                                  # CHANGE: stable identity per bar
        # ) %>%
        #   layout(
        #     barmode = if (stacked) "stack" else "group",
        #     xaxis   = list(
        #       title = if (horiz) y_var_lab else x_var_lab, 
        #       zeroline = FALSE,
        #       showticklabels = horiz,
        #       autorange = TRUE, 
        #       rangemode = "tozero"
        #       ),
        #     yaxis   = list(
        #       title = if (horiz) x_var_lab else y_var_lab, 
        #       zeroline = FALSE,
        #       autorange = TRUE,                        # CHANGE: safe; categorical axis ignores range but keeps behavior consistent
        #       automargin = TRUE,    
        #       showticklabels = !horiz
        #       ),
        #     legend  = if (!hide.legend) list(
        #       orientation = "h", x = 0.5, xanchor = "center", y = -0.2
        #     ) else list()
        #   )
        # 
        # # 5b) set initial category order for the first frame --------------------------
        # if (use_anim) {
        #   if (horiz) {
        #     pp <- pp %>% layout(yaxis = list(                       # CHANGE
        #       categoryorder = "array",
        #       categoryarray = rev(levs_by_year[[first_year]])
        #     ))
        #   } else {
        #     pp <- pp %>% layout(xaxis = list(                       # CHANGE
        #       categoryorder = "array",
        #       categoryarray = levs_by_year[[first_year]]
        #     ))
        #   }
        # }
        # 
        # # 6) (optional) animation controls
        # if (use_anim) {
        #   pp <- pp %>%
        #     animation_opts(frame = 800, transition = 400, easing = "linear", redraw = TRUE) %>%  # keep redraw=TRUE
        #     animation_slider(currentvalue = list(prefix = "Year: "), pad = list(t = 20))
        # }
        # 
        # # 6b) inject per-frame category order (so order changes each step) ------------
        # if (use_anim && length(pp$x$frames) > 0) {                                    # CHANGE
        #   for (i in seq_along(pp$x$frames)) {
        #     yr <- pp$x$frames[[i]]$name
        #     levs <- levs_by_year[[as.character(yr)]]
        #     if (!is.null(levs)) {
        #       if (horiz) {
        #         pp$x$frames[[i]]$layout$yaxis <- list(
        #           categoryorder = "array",
        #           categoryarray = rev(levs),
        #           automargin = TRUE 
        #         )
        #         pp$x$frames[[i]]$layout$xaxis <- list(           # CHANGE: numeric axis per-frame
        #           autorange = TRUE,
        #           rangemode = "tozero"
        #         )
        #       } else {
        #         pp$x$frames[[i]]$layout$xaxis <- list(
        #           categoryorder = "array",
        #           categoryarray = levs,
        #           automargin = TRUE 
        #         )
        #         pp$x$frames[[i]]$layout$yaxis <- list(           # CHANGE: numeric axis per-frame
        #           autorange = TRUE,
        #           rangemode = "tozero"
        #         )
        #       }
        #     }
        #   }
        # }
        # 
        # # 7) toolbar cleanup
        # pp <- pp %>%
        #   config(
        #     displaylogo = FALSE,
        #     modeBarButtonsToRemove = list(
        #       "autoScale2d","resetScale2d","hoverClosestCartesian",
        #       "toggleSpikelines","lasso2d","hoverCompareCartesian",
        #       "zoomInGeo","zoomOutGeo","resetGeo","hoverClosestGeo",
        #       "toImage","sendDataToCloud","hoverClosestGl2d",
        #       "hoverClosestPie","toggleHover","resetViews",
        #       "resetViewMapbox","select2d","zoom"
        #     )
        #   )
        # 
        # pp
      
        # ---- single panel bar chart ----
        
        # 1) sanity checks
        validate(need(x_var %in% names(df), paste0("x_var '", x_var, "' not in df")))
        validate(need(y_var %in% names(df), paste0("y_var '", y_var, "' not in df")))
        #validate(need(color_var %in% names(df), paste0("color_var '", color_var, "' not in df")))
        
        # Drop NA / zero rows ---------------------------------------------------------
        df <- df %>%
          dplyr::filter(!is.na(.data[[y_var]]), .data[[y_var]] != 0)
        validate(need(nrow(df) > 0, "No non-zero data to plot"))
        
        # 2) options
        horiz    <- "hbar" %in% gopts
        stacked  <- "stack" %in% gopts
        use_anim <- ("animate" %in% gopts) &&
          "year" %in% names(df) &&
          dplyr::n_distinct(df$year) > 1
        
        # Choose top-N size ----------------------------------------------------------- 
        top_k <- 20   # CHANGE
        
        # Keep only top-N countries (per year if animating) --------------------------
        if (use_anim && "year" %in% names(df)) {                                       # CHANGE
          totals <- df %>%
            dplyr::group_by(year, .data[[x_var]]) %>%
            dplyr::summarise(.val = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop")
          top_by_year <- totals %>%
            dplyr::group_by(year) %>%
            dplyr::arrange(dplyr::desc(.val), .by_group = TRUE) %>%
            dplyr::slice_head(n = top_k) %>%
            dplyr::ungroup()
          df <- dplyr::semi_join(df, top_by_year, by = c("year", x_var))
        } else {                                                                        # CHANGE
          totals <- df %>%
            dplyr::group_by(.data[[x_var]]) %>%
            dplyr::summarise(.val = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop") %>%
            dplyr::arrange(dplyr::desc(.val))
          top_global <- dplyr::slice_head(totals, n = top_k)
          df <- dplyr::semi_join(df, top_global, by = x_var)
        }
        
        validate(need(nrow(df) > 0, paste0("No data after top-", top_k, " filter")))   # CHANGE
        
        # 3) compute ordering (now based on the filtered df) --------------------------
        ord_df <- df %>%
          dplyr::group_by(.data[[x_var]]) %>%
          dplyr::summarise(.val = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop") %>%
          { 
            if ("hbar" %in% gopts) {
              dplyr::arrange(., .val)                 # ascending if horiz (we reverse in categoryarray below)
            } else {
              dplyr::arrange(., dplyr::desc(.val))
            }
          }
        df[[x_var]] <- factor(df[[x_var]], levels = ord_df[[x_var]])
        
        # Dynamic height (now capped by top_k)
        if (use_anim && "year" %in% names(df)) {
          max_n_cats <- df |>
            dplyr::group_by(year) |>
            dplyr::summarise(n = dplyr::n_distinct(.data[[x_var]]), .groups = "drop") |>
            dplyr::pull(n) |>
            max(na.rm = TRUE)
        } else {
          max_n_cats <- dplyr::n_distinct(df[[x_var]])
        }
        max_n_cats <- min(max_n_cats, top_k)                                            # CHANGE
        px_per_bar <- 22L
        dyn_height <- max(plot_height, 80 + px_per_bar * max_n_cats)
        
        # Per-year levels (from filtered df)
        if (use_anim) {
          ord_by_year <- df %>%
            dplyr::group_by(year, .data[[x_var]]) %>%
            dplyr::summarise(.val = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop") %>%
            dplyr::arrange(year, dplyr::desc(.val)) %>%
            dplyr::group_by(year) %>%
            dplyr::summarise(levels = list(.data[[x_var]]), .groups = "drop")
          levs_by_year <- setNames(ord_by_year$levels, as.character(ord_by_year$year))
          first_year   <- as.character(ord_by_year$year[[1]])
        }
        
        # Aesthetics
        x_aes <- if (horiz) ~get(y_var) else ~get(x_var)
        y_aes <- if (horiz) ~get(x_var) else ~get(y_var)
        
        # Plot
        pp <- plot_ly(
          data        = df,
          type        = "bar",
          x           = x_aes,
          y           = y_aes,
          colors      = color_style,
          showlegend  = !hide.legend,
          text        = ~get(x_var),
          hovertext   = ~tooltip_text,
          textposition = "inside",
          textfont    = plotly_font(plot_text_style$data_label_size),
          cliponaxis  = FALSE,
          orientation = if (horiz) "h" else "v",
          height      = dyn_height,
          frame       = if (use_anim) ~as.factor(year) else NULL,
          ids         = ~get(x_var)
        ) %>%
          layout(
            barmode = if (stacked) "stack" else "group",
            font = plotly_font(plot_text_style$legend_size),
            hoverlabel = plotly_hoverlabel_style(),
            xaxis   = plotly_axis_style(list(
              title = if (horiz) y_var_lab else x_var_lab,
              zeroline = FALSE,
              showticklabels = horiz,
              autorange = TRUE,
              rangemode = "tozero"
            )),
            yaxis   = plotly_axis_style(list(
              title = if (horiz) x_var_lab else y_var_lab,
              zeroline = FALSE,
              autorange = TRUE,
              automargin = TRUE,
              showticklabels = !horiz
            )),
            legend  = if (!hide.legend) plotly_legend_style(list(
              orientation = "h", x = 0.5, xanchor = "center", y = -0.2
            )) else list()
          )
        
        # Initial category order for first frame (reverse for horizontal so largest at top)
        if (use_anim) {
          if (horiz) {
            pp <- pp %>% layout(yaxis = plotly_axis_style(list(
              categoryorder = "array",
              categoryarray = rev(levs_by_year[[first_year]]),
              automargin = TRUE
            )))
          } else {
            pp <- pp %>% layout(xaxis = plotly_axis_style(list(
              categoryorder = "array",
              categoryarray = levs_by_year[[first_year]],
              automargin = TRUE
            )))
          }
        }
        
        # Anim controls
        if (use_anim) {
          pp <- pp %>%
            animation_opts(frame = 800, transition = 400, easing = "linear", redraw = TRUE) %>%
            animation_slider(
              currentvalue = list(
                prefix = "Year: ",
                font = plotly_font(plot_text_style$data_label_size)
              ),
              pad = list(t = 20)
            )
        }
        
        # Per-frame category order (on filtered top-N) + numeric autorange each frame
        if (use_anim && length(pp$x$frames) > 0) {
          for (i in seq_along(pp$x$frames)) {
            yr <- pp$x$frames[[i]]$name
            levs <- levs_by_year[[as.character(yr)]]
            if (!is.null(levs)) {
              if (horiz) {
                pp$x$frames[[i]]$layout$yaxis <- plotly_axis_style(list(
                  categoryorder = "array",
                  categoryarray = rev(levs),
                  automargin = TRUE
                ))
                pp$x$frames[[i]]$layout$xaxis <- plotly_axis_style(list(autorange = TRUE, rangemode = "tozero"))
              } else {
                pp$x$frames[[i]]$layout$xaxis <- plotly_axis_style(list(
                  categoryorder = "array",
                  categoryarray = levs,
                  automargin = TRUE
                ))
                pp$x$frames[[i]]$layout$yaxis <- plotly_axis_style(list(autorange = TRUE, rangemode = "tozero"))
              }
            }
          }
        }
        
        # Toolbar
        pp <- pp %>%
          config(
            displaylogo = FALSE,
            modeBarButtonsToRemove = list(
              "autoScale2d","resetScale2d","hoverClosestCartesian",
              "toggleSpikelines","lasso2d","hoverCompareCartesian",
              "zoomInGeo","zoomOutGeo","resetGeo","hoverClosestGeo",
              "toImage","sendDataToCloud","hoverClosestGl2d",
              "hoverClosestPie","toggleHover","resetViews",
              "resetViewMapbox","select2d","zoom"
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
                text = element_text(family = plot_text_style$family, color = plot_text_style$color),
                axis.title.x = element_text(size = plot_text_style$axis_title_size),
                axis.title.y = element_text(size = plot_text_style$axis_title_size),
                axis.text.x = element_text(size = plot_text_style$axis_tick_size, angle = 40, hjust = 1),
                axis.text.y = element_text(size = plot_text_style$axis_tick_size),
                legend.title = element_text(size = plot_text_style$legend_size, face = "bold"),
                legend.text = element_text(size = plot_text_style$legend_size),
                legend.position = if (hide.legend) "none" else "center",
                strip.text = element_text(size = plot_text_style$facet_size),
                plot.margin = margin(5.5, 5.5, 5.5, 5.5)
              )

        # Apply facet if facet_var is not NULL
        if (!is.null(facet_var) && facet_var != "null") {
          p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "fixed")
        }

        # modify x scale if necessary
        if (!is.null(breaks_x) || !is.null(x_limits)) {
          if (!is.null(breaks_x) && !is.null(x_limits)) {
            p <- p + scale_x_continuous(
              breaks = breaks_x,
              limits = x_limits,
              expand = expansion(mult = c(0, 0))
            )
          } else if (!is.null(breaks_x)) {
            p <- p + scale_x_continuous(breaks = breaks_x)
          } else {
            p <- p + scale_x_continuous(
              limits = x_limits,
              expand = expansion(mult = c(0, 0))
            )
          }
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
                              color = "lightgray", alpha = 1, inherit.aes = FALSE, size = 1)
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
        pp <- ggplotly(p, tooltip = "text", height = plot_height) %>%
          style(hoverinfo = "text") %>%
          layout(
            dragmode = "zoom",
            font = plotly_font(plot_text_style$legend_size),
            hoverlabel = plotly_hoverlabel_style(),
            autosize = TRUE,
            xaxis = plotly_axis_style(list(zeroline = FALSE)),
            yaxis = plotly_axis_style(list(zeroline = FALSE)),
            legend = plotly_legend_style(list(
              title = list(text = ''),
              orientation = "h",
              x = 0.5,
              itemclick = "toggleothers",
              itemdoubleclick = "none",
              xanchor = "center",
              y = -0.3))
          ) %>%
          apply_plotly_axis_text_style(show_grid = axis_show_grid) %>%
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
