# needs: readxl, qs, dplyr, tictoc
library(rlang)  # for .data pronoun if you use it below

if (!is.null(meta.file)) {
  tic("loading methodological table")
  
  ext <- tolower(tools::file_ext(meta.file))
  
  if (ext %in% c("xlsx", "xls")) {
    meth <- readxl::read_excel(meta.file)
  } else if (ext == "qs") {
    meth <- qs::qread(meta.file)
  } else {
    stop("Unsupported meta.file type: use .xlsx/.xls or .qs")
  }
  
  if (!inherits(meth, "data.frame")) {
    stop("The metadata file does not contain a data.frame/tibble.")
  }
  
  # Rename `country` -> `Country` only if present
  if ("country" %in% names(meth)) {
    meth <- dplyr::rename(meth, Country = .data$country)
  }
  
  # Drop columns if they exist (won't error if missing)
  meth <- dplyr::select(meth, -dplyr::any_of(c("area", "GEO3", "Source")))
  
  toc()
} else {
  meth <- NULL
}


# if (!is.null(meta.file)) {
#   #load methodological tables
#   tic("loading methodological table") 
#     meth <- read_excel(meta.file) %>% rename(Country = `country`) %>% select(-c("area", "GEO3", "Source")) 
#   toc()
# } else {
#     meth <- NULL 
# }