metaTableUI <- function(id) {
  ns <- NS(id)
  DTOutput(ns("metaTable"))
}

metaTableServer <- function(id, filtered_data, meta) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression to filter metadata and compute column widths dynamically
    processed_meta <- reactive({
      req(filtered_data(), meta)  # Ensure both datasets exist
      
      filtered_geo <- unique(filtered_data()$GEO)
      filtered_leg <- unique(filtered_data()$legend)
      
      # Filter meta by GEO and legend list
      filtered_meta <- meta %>% filter(GEO %in% filtered_geo, Legend %in% filtered_leg)
      
      # Compute max length of characters per column (only for character/text columns)
      max_lengths <- sapply(filtered_meta, function(col) {
        if (is.character(col)) { 
          max(nchar(col), na.rm = TRUE) 
        } else { 
          0  # Numeric columns don’t need word-length adjustments
        }
      })
      
      # Define thresholds
      moderate_threshold <- 25  
      extreme_threshold <- 200  
      
      list(
        moderate_columns = names(max_lengths[max_lengths > moderate_threshold & max_lengths <= extreme_threshold]),
        extreme_columns = names(max_lengths[max_lengths > extreme_threshold]),
        nowrap_columns = names(max_lengths[max_lengths <= moderate_threshold]),
        filtered_meta = filtered_meta  # Return the filtered dataset
      )
    })
    
    output$metaTable <- renderDT({
      data <- processed_meta()  # Get precomputed data
      filtered_meta <- data$filtered_meta  # Extract updated dataset
      
      datatable(filtered_meta, 
                rownames = FALSE, 
                options = list(
                  autoWidth = TRUE,  
                  scrollX = TRUE,    
                  columnDefs = list(
                    list(width = "200px", targets = which(names(filtered_meta) %in% data$moderate_columns)), 
                    list(width = "900px", targets = which(names(filtered_meta) %in% data$extreme_columns))   
                  ),
                  pageLength = 30    
                )) %>%
        formatStyle(columns = data$moderate_columns, `white-space` = 'normal', `word-break` = 'break-word') %>%  
        formatStyle(columns = data$extreme_columns, `white-space` = 'normal', `word-break` = 'break-word') %>%  
        formatStyle(columns = data$nowrap_columns, `white-space` = 'nowrap')  
    })
  })
}


# metaTableServer <- function(id, filtered_data, meta) {
#   moduleServer(id, function(input, output, session) {
#     
#     output$metaTable <- renderDT({
#       
#         req(filtered_data())  
#    
#         filtered_geo <- unique(filtered_data()$GEO)
#         filtered_leg <- unique(filtered_data()$legend)
#         
#         #filter meta by GEO and legend list
#         filtered_meta <- meta %>% filter(GEO %in% filtered_geo, Legend %in% filtered_leg)
#         
#         # Compute the maximum length of characters per column (only for character/text columns)
#         max_lengths <- sapply(filtered_meta, function(col) {
#           if (is.character(col)) { 
#             max(nchar(col), na.rm = TRUE) 
#           } else { 
#             0  # Numeric columns don’t need word-length adjustments
#           }
#         })
#         
#         # Define thresholds
#         moderate_threshold <- 40 
#         extreme_threshold <- 200  
#         
#         moderate_columns <- names(max_lengths[max_lengths > moderate_threshold & max_lengths <= extreme_threshold])  
#         extreme_columns <- names(max_lengths[max_lengths > extreme_threshold]) 
#         nowrap_columns <- names(max_lengths[max_lengths <= moderate_threshold])  
#         
#         # Generate table output with dynamic column width handling
#         datatable(filtered_meta, 
#                   rownames = FALSE, 
#                   options = list(
#                     autoWidth = TRUE,  
#                     scrollX = TRUE,    
#                     columnDefs = list(
#                       list(width = "200px", targets = which(names(filtered_meta) %in% moderate_columns)), 
#                       list(width = "900px", targets = which(names(filtered_meta) %in% extreme_columns))   
#                     ),
#                     pageLength = 30    
#                   )) %>%
#           formatStyle(columns = moderate_columns, `white-space` = 'normal', `word-break` = 'break-word') %>%  
#           formatStyle(columns = extreme_columns, `white-space` = 'normal', `word-break` = 'break-word') %>%  
#           formatStyle(columns = nowrap_columns, `white-space` = 'nowrap') 
#       })
#   })
# }
