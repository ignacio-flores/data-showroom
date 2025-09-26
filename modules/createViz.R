createViz <- function(data.file, meta.file = NULL,
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
                      area_stack_default = TRUE   
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
    fluidRow(id = "selectorRow", createSelectors(data, all_selectors, axis_vars, num.conversion, extra_layer)),
    
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
    
    observeEvent(input$externalGEO_long, {
      if ("GEO_long" %in% names(fixed_selectors)) {
        updatePickerInput(session, "GEO_long", selected = input$externalGEO_long)
      }
    })
    
    # filter data with main selectors 
    filtered_data <- dataFilter(input, output, session, 
        data, 
        x_var = axis_vars$x_axis$var, 
        y_var = axis_vars$y_axis$var, 
        color_var, 
        selector_vars = names(fixed_selectors),
        dt_cols = names(dt.cols), tooltip_vars, value_scale = "normal",
        extra_layer = extra_layer)
    
    loose_filters <- reactiveValues()
    
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
          choices <- unique(filtered_data()[[var]])
          selchoices <- choices
          
        # check if the selector is random and has more than 5 choices
        if (!is.null(loose_selectors[[var]]$select)) {
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
        }
          updatePickerInput(
            session,
            inputId = var,
            choices = choices,
            selected = selchoices 
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
      result <- filtered_data()  
      if (!is.null(loose_selectors)) {
        for (var in names(loose_selectors)) {
          if (!is.null(loose_filters[[var]]) && length(loose_filters[[var]]) > 0) {
            result <- result %>% filter(get(var) %in% loose_filters[[var]])
            
          }
        }
      }
      result
    })
    
    # Generate plot
    plotModuleServer("plotModule", reactive(final_filtered_data()),
                     x_var = axis_vars$x_axis$var, 
                     x_var_lab = axis_vars$x_axis$label, 
                     y_var = axis_vars$y_axis$var, 
                     y_var_lab = axis_vars$y_axis$label, 
                     color_var, color_var_lab, facet_var, facet_var_lab, 
                     tooltip_vars, hide.legend, gopts, 
                     xnum_breaks=axis_vars$x_axis$breaks, 
                     extra_layer, color_style, plot_height, groupvars)
    
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
      metaTableServer("metaModule", reactive(final_filtered_data()), meta = meth)
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

