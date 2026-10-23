source("custom_code/helpers/ineq_source_units.R", local = TRUE)
data <- normalize_ineq_source_units(data)

tic("loading dictionary")
  #use function to load sheets 
  remind_dictionary(
    file_path = file.path("data/dictionary.xlsx"),
    sheet_names = c("Sources", "data_type", "source_type")
  )
  #filter sources and clean 
  ineq_sections <- c("Wealth Inequality Trends", "Wealth Inequality")
  sce <- Sources[Sources$Section %in% ineq_sections & !is.na(Sources$Source), c("Source", "Data_Type", "Legend", "AggSource", "Link")]
  colnames(data_type) <- c("Data_Type", "description")
  colnames(source_type) <- c("source_type_short", "source_type_description")
  sce$Data_Type[is.na(sce$Data_Type) | !nzchar(trimws(sce$Data_Type))] <- "Unclassified"
  sce <- merge(sce, data_type, by = "Data_Type", all.x = TRUE)
  colnames(sce) <- c("Data_Type", "source", "source_legend", "source_type_short", "link", "data_description")
  sce <- merge(sce, source_type, by = "source_type_short", all.x = TRUE)
toc()
#Data_type is short version of data_description 
#Source_type_short is short version of... 

#aesthetics
tic("wraping text")
  missing_description <- is.na(sce$data_description) | !nzchar(trimws(sce$data_description))
  sce$data_description[missing_description] <- sce$Data_Type[missing_description]
  sce$data_description <- wrap_text(sce$data_description, width = 60)
toc()

#merge metadata 
tic("merging metadata")
  data <- merge(data, sce, by = "source")
toc()   

