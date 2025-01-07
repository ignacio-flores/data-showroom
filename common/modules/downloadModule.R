# Module UI Function
downloadModuleUI <- function(id) {
  ns <- NS(id)
  downloadButton(ns("downloadData"), "Download Data", style = "margin-bottom: 20px")
}

# Module Server Function
downloadModuleServer <- function(id, getData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(getData(), file, row.names = FALSE)
      }
    )
  })
}
