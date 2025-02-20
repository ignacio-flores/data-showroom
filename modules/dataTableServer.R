dataTableUI <- function(id) {
  ns <- NS(id)
  DTOutput(ns("dataTable"))
}

dataTableServer <- function(id, filtered_data, dt.cols) {
  moduleServer(id, function(input, output, session) {
    
    output$dataTable <- renderDT({
        
      req(filtered_data())  

        dt <- filtered_data() %>% mutate(across(where(is.numeric), ~ sprintf("%.2f", .))) %>% 
            select(names(dt.cols))
        if ("data_description" %in% names(dt)) {
          dt$data_description <- gsub("<br>", "", dt$data_description)
        }
        
        # If column names are not explicitly defined, use the raw column names
        dt_colnames <- sapply(names(dt.cols), function(col) {
          if (!is.null(dt.cols[[col]])) {
            dt.cols[[col]]
          } else {
            col  # Fallback to the raw column name
          }
        })
        
        colnames(dt) <- dt_colnames
        datatable(dt, rownames = FALSE, options = list(autoWidth = TRUE)) %>% 
          formatStyle(columns = seq(0, length(dt_colnames)), `text-align` = 'left') 
    })
  })
}


