tic("loading dictionary")
  #use function to load sheets 
  remind_dictionary(
    file_path = file.path(common.fold.pf, "data/dictionary.xlsx"),
    sheet_names = c("Sources", "data_type", "source_type")
  )
  #filter sources and clean 
  sce <- Sources[Sources$Section == "Wealth Inequality Trends" & !is.na(Sources$Source), c("Source", "Data_Type", "Legend", "AggSource", "Link")]
  colnames(data_type) <- c("Data_Type", "description")
  colnames(source_type) <- c("source_type_short", "source_type_description")
  sce <- merge(sce, data_type, by = "Data_Type") 
  colnames(sce) <- c("Data_Type", "source", "source_legend", "source_type_short", "link", "data_description")
  sce <- merge(sce, source_type, by = "source_type_short")
toc()
#Data_type is short version of data_description 
#Source_type_short is short version of... 

#aesthetics
tic("wraping text")
  sce$data_description <- wrap_text(sce$data_description, width = 60)
  sce$data_description <- wrap_text(sce$source_type_description, width = 60)
toc()

#merge metadata 
tic("merging metadata")
  data <- merge(data, sce, by = "source")
toc()   

tic("generating metadata_table")

  #define variables for metadata
  meta_tab <- unique(data[ c("source_legend", "GEO_long", "metadata", "data_description", "source_type_description", "d5_dboard_specific_lab", "link")])
  
  #filter metadata to variable and country 
  meta_tab <- meta_tab[
    meta_tab$metadata == "Share of total net wealth held by the richest 1% (p99p100)." & 
    meta_tab$GEO_long == "United States",]
  
  #transpose metadata 
  t_col <- t(meta_tab[, 1])
  t_df <- as.data.frame(t(meta_tab[, -1]))
  colnames(t_df) <- t_col
  t_df <- cbind(row = rownames(t_df), t_df)
  rownames(t_df) <- NULL
  for (x in 1:length(t_df)) {
    if (x > 1) {
      t_df[[x]] <- paste(t_df[[1]], t_df[[x]], sep = ": <br>")
    }
  }

toc()

