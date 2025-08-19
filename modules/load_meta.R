if (!is.null(meta.file)) {
  #load methodological tables
  tic("loading methodological table") 
    meth <- read_excel(meta.file) %>% rename(Country = `country`) %>% select(-c("area", "GEO3", "Source")) 
  toc()
} else {
    meth <- NULL 
}