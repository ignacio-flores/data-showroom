load_config <- function(file) {
  tic("loading YAML configuration")
  library(yaml)
  config <- yaml::read_yaml(file)
  lapply(names(config), function(name) {
    assign(name, config[[name]], envir = .GlobalEnv)
  })
  toc()
}