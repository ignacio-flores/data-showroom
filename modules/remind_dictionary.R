# Function to read all sheets of dictionary 
remind_dictionary <- function(file_path, sheet_names = NULL) {
  
  library(readxl)
  
  # List and read sheets
  if (is.null(sheet_names)) {
    sheet_names <- excel_sheets(file_path)
  }
  
  for(sheet_name in sheet_names) {
    assign(
      x = sheet_name, 
      value = read_excel(file_path, sheet = sheet_name), 
      envir = .GlobalEnv)
  }
}