#read the data  
data_cols <- fread(data.file, sep = ",", nrows = 1) %>% colnames()
all_vars <- intersect(unique(unlist(all_varlists)), data_cols)
if (!is.null(keep.col)) {
  data <- fread(data.file, sep = "," , select = c(all_vars, keep.col))
} else {
  data <- fread(data.file, sep = "," , select = all_vars)
}
data <- as.data.frame(data)
rm(data_cols)

if (!is.null(new.cols)) {
  data <- combine_multiple_columns(data,
                                   new.col.groups = new_col_groups,
                                   new.col.names = new_col_names
  )
}