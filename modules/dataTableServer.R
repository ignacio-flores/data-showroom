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

        max_lengths <- vapply(dt, function(col) {
          if (!is.character(col)) return(0)

          col <- as.character(col)
          if (all(is.na(col))) return(0)

          max(nchar(col), na.rm = TRUE)
        }, numeric(1))

        moderate_columns <- names(max_lengths[max_lengths > 20 & max_lengths <= 100])
        extreme_columns <- names(max_lengths[max_lengths > 100])
        nowrap_columns <- names(max_lengths[max_lengths <= 20])

        column_defs <- list(
          list(width = "260px", targets = which(names(dt) %in% moderate_columns) - 1),
          list(width = "460px", targets = which(names(dt) %in% extreme_columns) - 1)
        )

        text_columns <- names(dt)[vapply(dt, is.character, logical(1))]
        for (col in text_columns) {
          dt[[col]] <- vapply(dt[[col]], make_expandable, character(1), USE.NAMES = FALSE)
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
        moderate_display_columns <- unname(dt_colnames[names(dt_colnames) %in% moderate_columns])
        extreme_display_columns <- unname(dt_colnames[names(dt_colnames) %in% extreme_columns])
        nowrap_display_columns <- unname(dt_colnames[names(dt_colnames) %in% nowrap_columns])

        datatable(
          dt,
          rownames = FALSE,
          escape = FALSE,
          options = list(
            autoWidth = TRUE,
            scrollX = TRUE,
            columnDefs = column_defs
          )
        ) %>% 
          formatStyle(columns = seq(0, length(dt_colnames)), `text-align` = 'left') %>%
          formatStyle(columns = moderate_display_columns, `white-space` = 'normal', `word-break` = 'break-word') %>%
          formatStyle(columns = extreme_display_columns, `white-space` = 'normal', `word-break` = 'break-word') %>%
          formatStyle(columns = nowrap_display_columns, `white-space` = 'nowrap')
    })
  })
}
