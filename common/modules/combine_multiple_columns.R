# Define a function to dynamically combine multiple sets of columns into specified new variables
combine_multiple_columns <- function(data, new.col.groups, new.col.names, sep = ", ") {
  # Check if the length of new.col.groups and new.col.names are the same
  if (length(new.col.groups) != length(new.col.names)) {
    stop("The number of column groups and new column names must match.")
  }
  
  # Iterate over the list of column groups
  for (i in seq_along(new.col.groups)) {
    # Ensure the column names are valid
    if (!all(new.col.groups[[i]] %in% names(data))) {
      stop(paste("One or more specified columns in group", i, "do not exist in the dataframe."))
    }
    
    # Combine the specified columns and create a new variable
    data[[new.col.names[i]]] <- do.call(paste, c(data[new.col.groups[[i]]], sep = sep))
  }
  
  # Return the modified dataframe
  return(data)
}
