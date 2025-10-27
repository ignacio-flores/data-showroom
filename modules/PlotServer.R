require(ggplot2)
require(plotly)
require(viridis)
require(paletteer)
require(scales)

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
                             color_var = NULL, color_var_lab, facet_var, facet_var_lab, tooltip_vars, 
                             hide.legend, gopts, xnum_breaks, extra_layer, color_style,
                             plot_height, groupvars, stacked_default = FALSE) {
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
      if (!is.null(color_var)) {
        n_grp <- length(unique(df[[color_var]]))  
      }
      
      pal_name <- color_style   
      if (!is.null(color_style) & !(gopts %in% c("bar"))) {
        pal <- as.vector(                         
          paletteer_d(
            palette   = pal_name,  
            n         = n_grp,     
            direction = 1,          # -1 to reverse
            type      = "continuous"  
          )
        )
      }

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
            stackgroup = if (isTRUE(stack_active())) "one" else NULL,  
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

      } else if ("map" %in% gopts) {
        
        # helper: turn a vector of colors into a plotly colorscale
        make_colorscale <- function(pal) {
          n <- length(pal)
          if (n <= 1) return(list(list(0, pal[1]), list(1, pal[n])))
          lapply(seq_len(n), function(i) list((i-1)/(n-1), pal[i]))
        }
        
        # 1) compute a global range across ALL frames (do this BEFORE any year filtering)
        z_range <- range(df[[y_var]], na.rm = TRUE)
        
        pp <- plot_ly(
          data         = df,
          type         = "choropleth",
          locations    = ~get(color_var),
          locationmode = "ISO-3",
          z            = ~get(y_var),
          frame        = ~year,                
          text         = ~tooltip_text,
          hoverinfo    = "text",
          colorscale   = make_colorscale(pal),      
          zmin         = z_range[1],                
          zmax         = z_range[2],
          colorbar     = list(title = y_var_lab),
          height       = plot_height,
          source       = "map"
        ) %>%
          layout(
            geo = list(
              projection    = list(type = "mercator"),
              showland      = TRUE,
              landcolor     = "rgb(240,240,240)",
              showcountries = TRUE,
              countrycolor  = "rgb(200,200,200)"
            )) %>%
          animation_opts(
            frame      = 400,     # ms per frame (increase -> slower; decrease -> faster)
            transition = 100,     # ms between frames
            easing     = "linear",
            redraw     = T
          ) %>%
          animation_slider(
            currentvalue = list(prefix = "Year: "),
            pad = list(t = 30)
          ) %>%
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
        use_anim <- ("animate" %in% gopts)
        
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
          validate(
            need("year" %in% names(df) && dplyr::n_distinct(df$year) > 1,
                 "No frames to animate: 'year' missing or only one value")
          )
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
          cliponaxis  = FALSE,
          orientation = if (horiz) "h" else "v",
          height      = dyn_height,
          frame       = if (use_anim) ~as.factor(year) else NULL,
          ids         = ~get(x_var)
        ) %>%
          layout(
            barmode = if (stacked) "stack" else "group",
            xaxis   = list(
              title = if (horiz) y_var_lab else x_var_lab,
              zeroline = FALSE,
              showticklabels = horiz,
              autorange = TRUE,
              rangemode = "tozero"
            ),
            yaxis   = list(
              title = if (horiz) x_var_lab else y_var_lab,
              zeroline = FALSE,
              autorange = TRUE,
              automargin = TRUE,
              showticklabels = !horiz
            ),
            legend  = if (!hide.legend) list(
              orientation = "h", x = 0.5, xanchor = "center", y = -0.2
            ) else list()
          )
        
        # Initial category order for first frame (reverse for horizontal so largest at top)
        if (use_anim) {
          if (horiz) {
            pp <- pp %>% layout(yaxis = list(
              categoryorder = "array",
              categoryarray = rev(levs_by_year[[first_year]]),
              automargin = TRUE
            ))
          } else {
            pp <- pp %>% layout(xaxis = list(
              categoryorder = "array",
              categoryarray = levs_by_year[[first_year]],
              automargin = TRUE
            ))
          }
        }
        
        # Anim controls
        if (use_anim) {
          pp <- pp %>%
            animation_opts(frame = 800, transition = 400, easing = "linear", redraw = TRUE) %>%
            animation_slider(currentvalue = list(prefix = "Year: "), pad = list(t = 20))
        }
        
        # Per-frame category order (on filtered top-N) + numeric autorange each frame
        if (use_anim && length(pp$x$frames) > 0) {
          for (i in seq_along(pp$x$frames)) {
            yr <- pp$x$frames[[i]]$name
            levs <- levs_by_year[[as.character(yr)]]
            if (!is.null(levs)) {
              if (horiz) {
                pp$x$frames[[i]]$layout$yaxis <- list(
                  categoryorder = "array",
                  categoryarray = rev(levs),
                  automargin = TRUE
                )
                pp$x$frames[[i]]$layout$xaxis <- list(autorange = TRUE, rangemode = "tozero")
              } else {
                pp$x$frames[[i]]$layout$xaxis <- list(
                  categoryorder = "array",
                  categoryarray = levs,
                  automargin = TRUE
                )
                pp$x$frames[[i]]$layout$yaxis <- list(autorange = TRUE, rangemode = "tozero")
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
            font = list(family = "Arial", size = 12, color = "#000"),
            hoverlabel = list(
              font = list(size = 16)  
            ),
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
