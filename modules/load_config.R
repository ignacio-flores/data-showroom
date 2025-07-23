load_config <- function(file, func = NULL) {
  
  tic("loading YAML configuration")
  library(yaml)
  
  # Load the YAML configuration
  config <- yaml::read_yaml(file)
  
  # If a function is provided, filter arguments to match its formal arguments
  if (!is.null(func)) {
    func_args <- names(formals(func))  # Get argument names from the function
    filtered_config <- lapply(func_args, function(arg) config[[arg]])
    names(filtered_config) <- func_args
  } else {
    filtered_config <- config
  }
  
  # Assign the filtered configuration to the global environment
  lapply(names(filtered_config), function(name) {
    assign(name, filtered_config[[name]], envir = .GlobalEnv)
  })
  
  toc()
}
