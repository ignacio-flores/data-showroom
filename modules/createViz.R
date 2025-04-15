createViz <- function(data.file, meta.file = NULL,
                      x_var, x_var_lab = NULL, xnum_breaks = NULL,
                      y_var, y_var_lab = NULL,
                      new.cols = NULL,
                      tooltip_vars = NULL,
                      color_var, color_var_lab = NULL,
                      selector_info, 
                      loose_selectors = NULL,
                      facet_var = NULL, facet_var_lab = NULL,
                      table.display = F, dt.cols = NULL,
                      download.button = F, hide.legend = F,
                      hide.selectors = F, listen = F,
                      value_scale = "normal", 
                      data.wrangler = NULL, 
                      gopts = "line", num.conversion = NULL,
                      extra_layer = NULL, 
                      color_style = "viridis", 
                      plot_height = 700,
                      meta.loc = NULL) {
  
  tic("load packages for createViz")
    require(data.table)
  toc()
  
  # Preprocess inputs to ensure list format 
  tic("perform preliminary checks")
    selector_info <- ensureListStructure(selector_info)
    if (!is.null(dt.cols)) {
      dt.cols <- ensureListStructure(dt.cols)
    }
    tooltip_vars <- ensureListStructure(tooltip_vars)
  toc()
  if (!is.null(loose_selectors)) {
    all_selectors <- c(selector_info, loose_selectors)
  } else {
    all_selectors <- selector_info
  }
  
  # Load dataset and create new vars
  tic(paste0("loading ", data.file))
  
  # Define new columns if necessary
  if (!is.null(new.cols)) {
    new_col_groups <- lapply(new.cols, function(x) x)
    new_col_names <- names(new.cols)
    all_varlists <- list(
      new_col_groups,
      x_var, y_var,
      names(selector_info),
      names(dt.cols),
      names(tooltip_vars),
      "source"
    )
  all_vars <- setdiff(unique(unlist(all_varlists)), new_col_names)
  } else {
    if (!is.null(tooltip_vars)) {
      all_varlists <- list(
        x_var, y_var,
        names(selector_info),
        names(dt.cols),
        names(tooltip_vars),
        color_var, "source"
      )
      all_vars <- unique(unlist(all_varlists))
    } else {
      all_varlists <- list(
        x_var, y_var,
        names(selector_info),
        names(dt.cols),
        color_var,
        "source")
      all_vars <- unique(unlist(all_varlists))
    }
  }
  
  #read the data  
  data_cols <- fread(data.file, sep = ",", nrows = 1) %>% colnames()
  all_vars <- intersect(unique(unlist(all_varlists)), data_cols)
  data <- fread(data.file, sep = "," , select = all_vars)
  data <- as.data.frame(data)
  rm(data_cols)

  if (!is.null(new.cols)) {
    data <- combine_multiple_columns(data,
       new.col.groups = new_col_groups,
       new.col.names = new_col_names
    )
  }
  toc()
  
  # Run ad-hoc R script if necessary 
  if (!is.null(data.wrangler)) {
    source(data.wrangler, local = TRUE)  
  }
  
  # Load metadata if necessary
  if (!is.null(meta.file)) {
    #load methodological tables
    tic("loading methodological table") 
    meth <- read_excel(meta.file) %>% 
      rename(Country = `country`) %>% 
      select(-c("area", "GEO3", "Source"))
    toc()
  } else {
    meth <- NULL 
  }
  
  ### Define UI
  ui <- page_fluid(
    
    # Add Bootstrap theme and prevent padding 
    theme = bs_theme(version = 5),
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
    
    # Place selectors in a row with a specific ID
    fluidRow(id = "selectorRow", createSelectors(data, all_selectors, num.conversion, extra_layer)),
    
    # Placeholder for conditional download button
    uiOutput("downloadButtonUI"),
    
    # Display graph and tables
    if (table.display) {
      if (is.null(meta.file)) {
        # Case 1: Show only Visualization and Data tabs (no metadata)
        fluidRow(column(width = 12, tabsetPanel(
          tabPanel("Visualization", plotOutputUI("plotModule")),
          tabPanel("Data", uiOutput("tableOrMessageUI"))
        )))
        
      } else if (meta.loc == "tab") {
        # Case 2: Metadata shown as a separate tab
        fluidRow(column(width = 12, tabsetPanel(
          tabPanel("Visualization", plotOutputUI("plotModule")),
          tabPanel("Data", uiOutput("tableOrMessageUI")),
          tabPanel("Methodological table", metaTableUI("metaModule"))
        )))
        
      } else if (meta.loc == "below") {
        tagList(
          fluidRow(
            column(width = 12, plotOutputUI("plotModule"))
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
        fluidRow(column(width = 12, plotOutputUI("plotModule")))
      }
      
    } else {
      # Case 4: No table.display, just the plot (and maybe metadata below)
      if (!is.null(meta.file) && meta.loc == "below") {
        fluidRow(column(width = 12,
                        plotOutputUI("plotModule"),
                        tags$div(style = "margin-top: 100px;", metaTableUI("metaModule"))
        ))
      } else {
        fluidRow(column(width = 12, plotOutputUI("plotModule")))
      }
    }
  )
  
  ## Define Server
  server <- function(input, output, session) {
    
    observeEvent(input$externalGEO_long, {
      if ("GEO_long" %in% names(selector_info)) {
        updatePickerInput(session, "GEO_long", selected = input$externalGEO_long)
      }
    })
    
    # filter data with main selectors 
    filtered_data <- dataFilter(input, output, session, 
        data, x_var, y_var, color_var, 
        selector_vars = names(selector_info),
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
                     x_var, x_var_lab, y_var, y_var_lab, color_var, color_var_lab, facet_var, facet_var_lab, 
                     tooltip_vars, hide.legend, gopts, xnum_breaks, extra_layer, color_style, plot_height)
    
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

