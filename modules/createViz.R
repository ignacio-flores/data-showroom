createViz <- function(graph = NULL, 
                      data.file, meta.file = NULL,
                      axis_vars = NULL, 
                      new.cols = NULL,
                      tooltip_vars = NULL,
                      color = NULL,
                      fixed_selectors, 
                      loose_selectors = NULL,
                      facet_var = NULL, facet_var_lab = NULL,
                      table.display = F, dt.cols = NULL,
                      value_scale = "normal", 
                      data.wrangler = NULL, 
                      gopts = "line",
                      plot_height = 700,
                      download.button = F, 
                      hide.legend = F,
                      hide.selectors = F, 
                      listen = F,
                      num.conversion = NULL,
                      extra_layer = NULL, 
                      meta.loc = NULL, 
                      keep.col = NULL,
                      area_stack_toggle = FALSE,  
                      area_stack_default = TRUE,
                      scatter_options = NULL
                      ) {
  
  tic("loading and preliminary work")
    require(data.table)
    source("modules/preliminary_checks.R", local = TRUE)
    source("modules/define_varlists.R", local = TRUE)
    source("modules/read_dataset.R", local = TRUE)
    if (!is.null(data.wrangler)) {
      source(data.wrangler, local = TRUE)  
    }
    source("modules/load_meta.R", local = TRUE)
  toc()  

  is_dual_mode <- "dual_axis_line" %in% gopts
  is_dynamic_scatter <- isTRUE(scatter_options$enabled) &&
    ("point" %in% gopts) &&
    !is_dual_mode

  parseAxisChoices <- function(ch) {
    if (is.character(ch) && length(ch) == 1 && grepl("^c\\(", ch)) {
      return(tryCatch(eval(parse(text = ch)), error = function(e) ch))
    }
    ch
  }

  axisChoiceValues <- function(axis_info) {
    if (is.null(axis_info)) return(NULL)
    unique(c(axis_info$var, parseAxisChoices(axis_info$choices)))
  }

  axisChoiceLabelMap <- function(axis_info) {
    choices <- parseAxisChoices(axis_info$choices)
    vals <- if (!is.null(choices)) choices else axisChoiceValues(axis_info)
    if (is.null(vals)) return(setNames(character(0), character(0)))
    labs <- as.character(vals)
    alt_names <- parseAxisChoices(axis_info$alt.names)
    if (!is.null(alt_names) && length(alt_names) == length(vals)) {
      labs <- as.character(alt_names)
    }
    label_map <- stats::setNames(labs, as.character(vals))
    if (!is.null(axis_info$var) && !axis_info$var %in% names(label_map)) {
      label_map[[axis_info$var]] <- if (!is.null(axis_info$label)) axis_info$label else axis_info$var
    }
    label_map
  }

  # Pre-compute initial loose-selector selections to avoid first-render flash
  # where all values are briefly selected before observe() narrows them.
  if (!is.null(loose_selectors) && length(loose_selectors) > 0) {
    init_loose_data <- data

    if (!is.null(fixed_selectors) && length(fixed_selectors) > 0) {
      for (fvar in names(fixed_selectors)) {
        if (!fvar %in% names(init_loose_data)) next
        finfo <- fixed_selectors[[fvar]]
        if (!("selected" %in% names(finfo)) || is.null(finfo$selected)) next
        init_loose_data <- init_loose_data[init_loose_data[[fvar]] %in% finfo$selected, , drop = FALSE]
      }
    }

    for (lvar in names(loose_selectors)) {
      linfo <- loose_selectors[[lvar]]
      if (!lvar %in% names(init_loose_data)) next
      if ("selected" %in% names(linfo) && !is.null(linfo$selected)) next
      if (is.null(linfo$select)) next

      choices_data <- init_loose_data

      if (identical(lvar, "year")) {
        y_var_name <- if (!is.null(axis_vars) && !is.null(axis_vars$y_axis)) axis_vars$y_axis$var else NULL
        x_var_name <- if (!is.null(axis_vars) && !is.null(axis_vars$x_axis)) axis_vars$x_axis$var else NULL
        if (!is.null(y_var_name) && y_var_name %in% names(choices_data)) {
          choices_data <- choices_data[!is.na(choices_data[[y_var_name]]), , drop = FALSE]
        }
        if (!is.null(x_var_name) && x_var_name %in% names(choices_data)) {
          choices_data <- choices_data[!is.na(choices_data[[x_var_name]]), , drop = FALSE]
        }
      }

      choices <- sort(unique(choices_data[[lvar]]))
      choices <- choices[!is.na(choices)]

      selchoices <- choices
      if (identical(linfo$select, "random") && length(choices) > 5) {
        selchoices <- sample(choices, 5)
      } else if (identical(linfo$select, "spaced") && length(choices) > 5) {
        selchoices <- choices[seq(1, length(choices), length.out = 5)]
      }

      all_selectors[[lvar]]$selected <- selchoices
    }
  }
  
  ### Define UI
  ui <- page_fluid(
    
    # Add Bootstrap theme 
    #theme = bs_theme(version = 5),
    
    # Prevent padding
    tags$style(HTML("
      html, body {
        margin: 0;
        padding: 0;
        height: 100%;
        overflow-y: auto;
      }
      .container-fluid {
        padding-left: 0 !important;
        padding-right: 0 !important;
        padding-bottom: 0 !important;
      }
      .row {
        margin-left: 0 !important;
        margin-right: 0 !important;
      }
  ")),
      
    # Option: listening to messages when hiding selectors
    if (listen) {
      tags$head(
        tags$script(HTML("
        $(document).ready(function() {
          window.addEventListener('message', function(event) {
            try {
              var data = JSON.parse(event.data);
              if (data.type && data.type === 'updateCountry') {
                Shiny.setInputValue('externalGEO_long', data.country);
              }
            } catch(e) {
              console.error('Error parsing message data:', e);
            }
          }, false);
        });
      "))
      )
    },
    
    #prevent overflow of selectors
    tags$head(
      tags$style(HTML("
      #selectorRow {
          max-width: 100% !important;
          align-items: center; 
      }
    "))),
    
    # Conditionally add CSS to hide selectors
    tags$head(
      tags$style(
        if (hide.selectors) 
          HTML("#selectorRow { display: none; }")
        else 
          HTML("#selectorRow { display: block; }")
      )
    ),
    
    # Add custom JavaScript for expandable text
    tags$head(
      tags$script(HTML("
        $(document).on('click', '.toggle-link', function(e) {
          e.preventDefault();
          var container = $(this).closest('.expandable-text');
          container.find('.short-text').toggle();
          container.find('.full-text').toggle();
          $(this).text($(this).text() === 'Show more' ? 'Show less' : 'Show more');
        });
      "))
    ),
    
    # Place selectors in a row with a specific ID
    fluidRow(
      id = "selectorRow",
      createSelectors(
        data,
        all_selectors,
        axis_vars,
        num.conversion,
        extra_layer,
        scatter_options = if (is_dynamic_scatter) scatter_options else NULL
      )
    ),
    
    # Placeholder for conditional download button
    uiOutput("downloadButtonUI"),
    
    # Display graph and tables
    if (table.display) {
      if (is.null(meta.file)) {
        # Case 1: Show only Visualization and Data tabs (no metadata)
        fluidRow(column(width = 12, tabsetPanel(
          tabPanel("Visualization", plotOutputUI("plotModule",
                                                 show_stack_toggle     = area_stack_toggle,   
                                                 stacked_default       = area_stack_default,  
                                                 gopts                 = gopts)),
          tabPanel("Data", uiOutput("tableOrMessageUI"))
        )))
        
      } else if (meta.loc == "tab") {
        # Case 2: Metadata shown as a separate tab
        fluidRow(column(width = 12, tabsetPanel(
          tabPanel("Visualization", plotOutputUI("plotModule",
                                                 show_stack_toggle     = area_stack_toggle,   
                                                 stacked_default       = area_stack_default,  
                                                 gopts                 = gopts)),
          tabPanel("Data", uiOutput("tableOrMessageUI")),
          tabPanel("Methodological table", metaTableUI("metaModule"))
        )))
        
      } else if (meta.loc == "below") {
        tagList(
          fluidRow(
            column(width = 12, plotOutputUI("plotModule",
                                            show_stack_toggle     = area_stack_toggle,   
                                            stacked_default       = area_stack_default,  
                                            gopts                 = gopts))
          ),
          fluidRow(
            column(width = 12,
                   tags$div(style = "margin-top: 300px; "),
                   accordion(
                     open = FALSE,
                     accordion_panel(
                       title = tagList(
                         tags$h4("Read the detailed Methodological Table:"),
                         tags$p(
                           "The Methodological Table below summarizes, for each source: the type of data used, where the work sources its data, the unit of analysis, the methods of estimation, how different assets are valued, how specific types of assets are treated, any adjustments made to the data, important underlying assumptions, and more."
                         )
                       ),
                       value = "metadata",
                       metaTableUI("metaModule")
                     )
                   )
            )
          )
        )
      } else {
        # Fallback if meta.loc is something unexpected
        fluidRow(column(width = 12, plotOutputUI("plotModule",
                                                 show_stack_toggle     = area_stack_toggle,   
                                                 stacked_default       = area_stack_default,  
                                                 gopts                 = gopts)))
      }
      
    } else {
      # Case 4: No table.display, just the plot (and maybe metadata below)
      if (!is.null(meta.file) && meta.loc == "below") {
        fluidRow(column(width = 12,
                        plotOutputUI("plotModule",
                                     show_stack_toggle     = area_stack_toggle,   
                                     stacked_default       = area_stack_default,  
                                     gopts                 = gopts),
                        tags$div(style = "margin-top: 100px;", metaTableUI("metaModule"))
        ))
      } else {
        fluidRow(column(width = 12, plotOutputUI("plotModule",
                                                 show_stack_toggle     = area_stack_toggle,   
                                                 stacked_default       = area_stack_default,  
                                                 gopts                 = gopts)))
      }
    }
  )
  
  ## Define Server
  server <- function(input, output, session) {
    selected_x_var <- reactive({
      if (is_dynamic_scatter && !is.null(input$x_axis) && nzchar(input$x_axis)) {
        input$x_axis
      } else {
        axis_vars$x_axis$var
      }
    })

    selected_y_var <- reactive({
      if ((is_dual_mode || is_dynamic_scatter) && !is.null(input$y_axis) && nzchar(input$y_axis)) {
        input$y_axis
      } else {
        axis_vars$y_axis$var
      }
    })

    selected_x_scale <- reactive({
      if (is_dynamic_scatter && !is.null(input$x_axis_scale) && nzchar(input$x_axis_scale)) {
        input$x_axis_scale
      } else {
        "regular"
      }
    })

    selected_y2_var <- reactive({
      if (!is_dual_mode) return(NULL)
      if (!is.null(input$y2_axis) && nzchar(input$y2_axis)) {
        input$y2_axis
      } else {
        axis_vars$y2_axis$var
      }
    })

    selected_x_lab <- reactive({
      if (!is_dynamic_scatter) return(axis_vars$x_axis$label)
      var_name <- selected_x_var()
      lab_map <- axisChoiceLabelMap(axis_vars$x_axis)
      if (!is.null(lab_map[[var_name]])) {
        lab_map[[var_name]]
      } else if (!is.null(axis_vars$x_axis$label)) {
        axis_vars$x_axis$label
      } else {
        var_name
      }
    })

    selected_y_lab <- reactive({
      if (!(is_dual_mode || is_dynamic_scatter)) return(axis_vars$y_axis$label)
      var_name <- selected_y_var()
      lab_map <- axisChoiceLabelMap(axis_vars$y_axis)
      if (!is.null(lab_map[[var_name]])) {
        lab_map[[var_name]]
      } else if (!is.null(axis_vars$y_axis$label)) {
        axis_vars$y_axis$label
      } else {
        var_name
      }
    })

    selected_y2_lab <- reactive({
      if (!is_dual_mode) return(NULL)
      var_name <- selected_y2_var()
      lab_map <- axisChoiceLabelMap(axis_vars$y2_axis)
      if (!is.null(lab_map[[var_name]])) {
        lab_map[[var_name]]
      } else if (!is.null(axis_vars$y2_axis$label)) {
        axis_vars$y2_axis$label
      } else {
        var_name
      }
    })
    
    observeEvent(input$externalGEO_long, {
      if ("GEO_long" %in% names(fixed_selectors)) {
        updatePickerInput(session, "GEO_long", selected = input$externalGEO_long)
      }
    })
    
    # filter data with main selectors
    if (is_dual_mode) {
      filtered_data <- reactive({
        data_filtered <- data
        selector_vars <- names(fixed_selectors)

        for (var in selector_vars) {
          if (!is.null(color_var) && var == color_var && !is.null(extra_layer)) {
            data_filtered <- data_filtered[data_filtered[[var]] %in% unique(c(extra_layer$values, input[[var]])), ]
          } else {
            data_filtered <- data_filtered[data_filtered[[var]] %in% input[[var]], ]
          }
        }

        if (nrow(data_filtered) == 0) return(NULL)

        y_candidates <- unique(c(
          axisChoiceValues(axis_vars$y_axis),
          axisChoiceValues(axis_vars$y2_axis),
          selected_y_var(),
          selected_y2_var()
        ))

        cols_to_keep <- unique(c(
          names(dt.cols),
          axis_vars$x_axis$var,
          selector_vars,
          y_candidates,
          if (!is.null(color_var)) color_var,
          names(tooltip_vars)
        ))
        cols_to_keep <- cols_to_keep[!is.na(cols_to_keep) & nzchar(cols_to_keep)]
        cols_to_keep <- intersect(cols_to_keep, names(data_filtered))

        data_filtered <- data_filtered %>% select(all_of(cols_to_keep))

        sort_vars <- c()
        if (!is.null(facet_var) && facet_var %in% names(data_filtered)) {
          sort_vars <- c(sort_vars, facet_var)
        }
        sort_vars <- c(sort_vars, selector_vars, axis_vars$x_axis$var)
        sort_vars <- unique(sort_vars[sort_vars %in% names(data_filtered)])

        if (length(sort_vars) > 0) {
          data_filtered <- data_filtered[do.call(order, data_filtered[sort_vars]), ]
        }

        data_filtered
      })
    } else if (is_dynamic_scatter) {
      filtered_data <- reactive({
        data_filtered <- data
        selector_vars <- names(fixed_selectors)

        for (var in selector_vars) {
          if (!var %in% names(data_filtered) || is.null(input[[var]])) next
          data_filtered <- data_filtered[data_filtered[[var]] %in% input[[var]], , drop = FALSE]
        }

        if (nrow(data_filtered) == 0) return(NULL)

        axis_candidates <- unique(c(
          axisChoiceValues(axis_vars$x_axis),
          axisChoiceValues(axis_vars$y_axis),
          selected_x_var(),
          selected_y_var()
        ))

        cols_to_keep <- unique(c(
          names(dt.cols),
          axis_candidates,
          selector_vars,
          names(loose_selectors),
          if (!is.null(color_var)) color_var,
          names(tooltip_vars)
        ))
        cols_to_keep <- cols_to_keep[!is.na(cols_to_keep) & nzchar(cols_to_keep)]
        cols_to_keep <- intersect(cols_to_keep, names(data_filtered))

        data_filtered <- data_filtered %>% select(all_of(cols_to_keep))

        sort_vars <- unique(c(selector_vars, names(loose_selectors), selected_x_var()))
        sort_vars <- sort_vars[sort_vars %in% names(data_filtered)]
        if (length(sort_vars) > 0) {
          data_filtered <- data_filtered[do.call(order, data_filtered[sort_vars]), , drop = FALSE]
        }

        data_filtered
      })
    } else {
      filtered_data <- dataFilter(input, output, session,
        data,
        x_var = axis_vars$x_axis$var,
        y_var = axis_vars$y_axis$var,
        color_var,
        selector_vars = names(fixed_selectors),
        dt_cols = names(dt.cols), tooltip_vars, value_scale = "normal",
        extra_layer = extra_layer
      )
    }
    
    loose_filters <- reactiveValues()
    loose_selector_updating <- reactiveVal(FALSE)
    last_valid_filtered_data <- reactiveVal(NULL)
    
    # update loose selectors individually
    if (!is.null(loose_selectors)) {
      lapply(names(loose_selectors), function(var) {
        
        # compute available values based on filtered_data
        filtered_values <- reactive({
          req(filtered_data()) 
          unique(filtered_data()[[var]])
        })
        
        observe({
          req(filtered_data())
          choices_data <- filtered_data()

          # Avoid offering years that have no drawable points for the current axes.
          if (identical(var, "year")) {
            y_var_name <- selected_y_var()
            x_var_name <- selected_x_var()

            if (!is.null(y_var_name) && y_var_name %in% names(choices_data)) {
              choices_data <- choices_data %>%
                dplyr::filter(!is.na(.data[[y_var_name]]))
            }
            if (!is.null(x_var_name) && x_var_name %in% names(choices_data)) {
              choices_data <- choices_data %>%
                dplyr::filter(!is.na(.data[[x_var_name]]))
            }
          }

          choices <- sort(unique(choices_data[[var]]))
          choices <- choices[!is.na(choices)]
          selector_type <- if ("type" %in% names(loose_selectors[[var]])) {
            loose_selectors[[var]]$type
          } else {
            "select"
          }
          current_selection <- isolate(input[[var]])
          current_selection <- current_selection[current_selection %in% choices]
          configured_selection <- if ("selected" %in% names(loose_selectors[[var]])) {
            loose_selectors[[var]]$selected
          } else {
            NULL
          }
          configured_selection <- configured_selection[configured_selection %in% choices]
          selchoices <- if (length(current_selection) > 0) {
            current_selection
          } else if (length(configured_selection) > 0) {
            configured_selection
          } else if (identical(selector_type, "checkbox")) {
            choices
          } else if (length(choices) > 0) {
            choices[[1]]
          } else {
            NULL
          }
          
        # check if the selector is random and has more than 5 choices
        if (length(current_selection) == 0 && length(configured_selection) == 0 && !is.null(loose_selectors[[var]]$select)) {
          if (loose_selectors[[var]]$select == "random" & length(choices) > 5) {
            selchoices <- sample(choices, 5)
          }
          # check if the selector is equally "spaced" and has more than 5 choices
          else if (loose_selectors[[var]]$select == "spaced" & length(choices) > 5) {
            selchoices <- choices[seq(1, length(choices), length.out = 5)]
          } 
        } 
        if (length(choices) == 0) {
          choices <- character(0) 
          selchoices <- NULL
        } else if (!identical(selector_type, "checkbox") && length(selchoices) > 1) {
          selchoices <- selchoices[[1]]
        }
          loose_selector_updating(TRUE)
          freezeReactiveValue(input, var)
          updatePickerInput(
            session,
            inputId = var,
            choices = choices,
            selected = selchoices 
          )
          session$onFlushed(
            function() {
              loose_selector_updating(FALSE)
            },
            once = TRUE
          )
        })
        
        # Observer: Update filter for this specific selector when input changes
        observeEvent(input[[var]], {
          if (!is.null(input[[var]])) {
            loose_filters[[var]] <- input[[var]]  # Update the filter state
          } else {
            loose_filters[[var]] <- NULL  # Reset filter if nothing selected
          }
        })
      })
    }
    
    # Apply only active filters
    final_filtered_data <- reactive({
      req(filtered_data())

      if (isTRUE(loose_selector_updating())) {
        last_valid <- last_valid_filtered_data()
        if (!is.null(last_valid)) {
          return(last_valid)
        }
      }

      result <- filtered_data()
      if (!is.null(loose_selectors)) {
        for (var in names(loose_selectors)) {
          if (!is.null(loose_filters[[var]]) && length(loose_filters[[var]]) > 0) {
            if (!var %in% names(result)) next

            available_values <- unique(result[[var]])
            active_values <- loose_filters[[var]][loose_filters[[var]] %in% available_values]

            if (length(active_values) == 0) {
              last_valid <- last_valid_filtered_data()
              if (!is.null(last_valid)) {
                return(last_valid)
              }
              return(NULL)
            }

            result <- result %>% filter(.data[[var]] %in% active_values)
          }
        }
      }

      if (is.null(result) || nrow(result) == 0) {
        return(NULL)
      }

      last_valid_filtered_data(result)
      result
    })
    
    # Generate plot
    plotModuleServer("plotModule", reactive(final_filtered_data()),
                     x_var = if (is_dynamic_scatter) selected_x_var else axis_vars$x_axis$var, 
                     x_var_lab = if (is_dynamic_scatter) selected_x_lab else axis_vars$x_axis$label, 
                     y_var = selected_y_var,
                     y_var_lab = selected_y_lab,
                     y2_var = if (is_dual_mode) selected_y2_var else NULL,
                     y2_var_lab = if (is_dual_mode) selected_y2_lab else NULL,
                     color_var, color_var_lab, facet_var, facet_var_lab, 
                     tooltip_vars, hide.legend, gopts, 
                     xnum_breaks=axis_vars$x_axis$breaks, 
                     extra_layer, color_style, plot_height, groupvars,
                     x_scale = if (is_dynamic_scatter) selected_x_scale else NULL,
                     scatter_options = if (is_dynamic_scatter) scatter_options else NULL)
    
    # Render table
    output$tableOrMessageUI <- renderUI({
      if (table.display && is.null(dt.cols)) {
        # Display message if dt.cols is NULL
        h4("You need to specify dt.cols if table.display = TRUE. You can use formats such as c('var1', 'var2') or list('var1' = 'lab1', 'var2' = 'lab2')", style = "color: red;")
      } else {
        # Otherwise, render the table
        dataTableUI("tableModule")
      }
    })
    if (table.display && !is.null(dt.cols)) {
      dataTableServer("tableModule", reactive(final_filtered_data()), dt.cols)
    }
    
    # Render metadata table
    if (!is.null(meta.file)) {
      metaTableServer("metaModule", reactive(final_filtered_data()), meta = meth, graph = graph)
    }
    
    # Download button conditionally rendered
    output$downloadButtonUI <- renderUI({
      if (download.button) {
        fluidRow(column(width = 12, downloadModuleUI("dataDownload")))
      }
    })
    
    if (download.button) {
      downloadModuleServer("dataDownload", reactive(final_filtered_data()))
    }
    
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
}
