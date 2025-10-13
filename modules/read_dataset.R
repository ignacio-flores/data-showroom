library(data.table)
library(magrittr)

ext <- tolower(tools::file_ext(data.file))

if (ext == "csv") {
  # get column names quickly without loading data
  data_cols <- fread(data.file, sep = ",", nrows = 0) %>% colnames()
  
} else if (ext == "rds") {
  # load the object to inspect its columns
  tmp_obj <- readRDS(data.file)
  if (!is.data.frame(tmp_obj)) {
    stop("RDS file does not contain a data.frame / data.table / tibble.")
  }
  data_cols <- colnames(tmp_obj)
  
} else if (ext == "qs") {
  # qs: fast read; subset after load
  tmp_obj <- qs::qread(data.file)
  if (!is.data.frame(tmp_obj)) {
    stop("QS file does not contain a data.frame / data.table / tibble.")
  }
  data_cols <- colnames(tmp_obj)
  
} else {
  stop("Unsupported file type: use .csv, .rds, or .qs")
}

all_vars <- intersect(unique(unlist(all_varlists)), data_cols)

# read/select data
if (ext == "csv") {
  if (!is.null(keep.col)) {
    data <- fread(data.file, sep = ",", select = c(all_vars, keep.col))
  } else {
    data <- fread(data.file, sep = ",", select = all_vars)
  }
} else { # rds or qs path (tmp_obj already loaded)
  sel <- if (!is.null(keep.col)) unique(c(all_vars, keep.col)) else all_vars
  data <- tmp_obj[, sel, drop = FALSE]
  rm(tmp_obj)
}

data <- as.data.frame(data)  # (optional; you can drop this if data.table is fine)
rm(data_cols)

if (!is.null(new.cols)) {
  data <- combine_multiple_columns(
    data,
    new.col.groups = new_col_groups,
    new.col.names  = new_col_names
  )
}


# #read the data  
# data_cols <- fread(data.file, sep = ",", nrows = 1) %>% colnames()
# all_vars <- intersect(unique(unlist(all_varlists)), data_cols)
# if (!is.null(keep.col)) {
#   data <- fread(data.file, sep = "," , select = c(all_vars, keep.col))
# } else {
#   data <- fread(data.file, sep = "," , select = all_vars)
# }
# data <- as.data.frame(data)
# rm(data_cols)
# 
# if (!is.null(new.cols)) {
#   data <- combine_multiple_columns(data,
#                                    new.col.groups = new_col_groups,
#                                    new.col.names = new_col_names
#   )
# }