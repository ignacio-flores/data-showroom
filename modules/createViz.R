createViz <- function(data.file, x_var, y_var,
                      new.cols = NULL,
                      tooltip_vars = NULL,
                      color_var, 
                      selector_info, 
                      loose_selectors = NULL,
                      table.display = F, dt.cols = NULL,
                      download.button = F, hide.legend = F,
                      hide.selectors = F, listen = F,
                      value_scale = "normal", 
                      data.wrangler = NULL, 
                      gopts = "line", num.conversion = NULL) {
  
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
  
  # Run ad-hoc R script 
  if (!is.null(data.wrangler)) {
    source(data.wrangler, local = TRUE)  
  }
  
  ### Define UI
  ui <- fluidPage(
    
    # Option: listening to messages when hiding selectors
    if (hide.selectors && listen) {
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
    fluidRow(id = "selectorRow", createSelectors(data, all_selectors, num.conversion)),
    
    # Placeholder for conditional download button
    uiOutput("downloadButtonUI"),
    
    # Display graph and table
    if (table.display) {
      fluidRow(column(width = 12, tabsetPanel(
        tabPanel("Visualization", plotOutputUI("plotModule")),
        tabPanel("Data", uiOutput("tableOrMessageUI")),
        tabPanel("Metadata", uiOutput("tableMeta"))
      )))
    } else {
      fluidRow(column(width = 12, plotOutputUI("plotModule")))
    }
  )
  
  ## Define Server
  server <- function(input, output, session) {
    
    # filter data with main selectors 
    filtered_data <- dataFilter(input, output, session, 
        data, x_var, y_var, color_var, 
        selector_vars = names(selector_info),
        dt_cols = names(dt.cols), tooltip_vars, value_scale = "normal")
    
    # create reactive list to track current filters for loose selectors
    loose_filters <- reactiveValues() 
    
    # update loose selectors individually
    if (!is.null(loose_selectors)) {
      lapply(names(loose_selectors), function(var) {
        
        # compute available values based on filtered_data
        filtered_values <- reactive({
          req(filtered_data()) 
          unique(filtered_data()[[var]])
        })
        
        # Observer: Update the selector input choices dynamically
        observeEvent(filtered_values(), {
          updateSelectInput(
            session,
            inputId = var,                   
            choices = filtered_values(),     
            selected = filtered_values()  # Reset to all values
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
            result <- result %>% filter(.[[var]] %in% loose_filters[[var]])
          }
        }
      }
      result
    })
    
    # Generate plot
    plotModuleServer("plotModule", reactive(final_filtered_data()), x_var, y_var, color_var, tooltip_vars, hide.legend, gopts)
    
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

