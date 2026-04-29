read_topo_dictionary_sheet <- function(dictionary_file, sheet) {
  as.data.frame(readxl::read_excel(dictionary_file, sheet = sheet), stringsAsFactors = FALSE)
}

select_existing_columns <- function(data, columns) {
  data[, intersect(columns, names(data)), drop = FALSE]
}

normalize_source_records <- function(sources) {
  names(sources) <- tolower(names(sources))
  names(sources) <- gsub("[^a-z0-9]+", "_", names(sources))
  names(sources) <- gsub("_$", "", names(sources))

  keep <- c(
    "section",
    "aggsource",
    "legend",
    "source",
    "link",
    "ref_link",
    "citekey",
    "metadata"
  )
  sources <- select_existing_columns(sources, keep)

  if ("section" %in% names(sources)) {
    sources <- sources[sources$section == "Wealth Topography", , drop = FALSE]
  }
  if ("aggsource" %in% names(sources)) {
    names(sources)[names(sources) == "aggsource"] <- "source_type"
  }
  if ("metadata" %in% names(sources)) {
    names(sources)[names(sources) == "metadata"] <- "source_metadata"
  }

  unique(sources)
}

build_topo_metadata_bundle <- function(
  warehouse_file = "data/topo_warehouse_meta_v2.csv",
  dictionary_file = "data/dictionary.xlsx"
) {
  if (!file.exists(warehouse_file)) {
    stop("Missing topo warehouse file: ", warehouse_file)
  }
  if (!file.exists(dictionary_file)) {
    stop("Missing dictionary file: ", dictionary_file)
  }

  series_cols <- c(
    "GEO",
    "GEO_long",
    "source",
    "source_type",
    "legend",
    "d2_sector",
    "d2_sector_lab",
    "d3_vartype",
    "d3_vartype_lab",
    "d4_concept",
    "d4_concept_lab",
    "d5_dboard_specific",
    "d5_dboard_specific_lab",
    "metadata",
    "last_update"
  )
  available_cols <- names(data.table::fread(warehouse_file, nrows = 0, showProgress = FALSE))
  series_cols <- intersect(series_cols, available_cols)
  series_index <- data.table::fread(
    warehouse_file,
    select = series_cols,
    showProgress = FALSE
  )
  series_index <- unique(as.data.frame(series_index, stringsAsFactors = FALSE))

  dictionary <- list(
    countries = select_existing_columns(
      read_topo_dictionary_sheet(dictionary_file, "GEO"),
      c("GEO", "Country", "GEO3")
    ),
    sectors = select_existing_columns(
      read_topo_dictionary_sheet(dictionary_file, "d2_sector"),
      c("code", "label", "description")
    ),
    variable_kinds = select_existing_columns(
      read_topo_dictionary_sheet(dictionary_file, "d3_vartype"),
      c("code", "label", "description")
    ),
    concepts = select_existing_columns(
      read_topo_dictionary_sheet(dictionary_file, "d4_concept"),
      c("code", "label", "description")
    ),
    dashboard_specific = select_existing_columns(
      read_topo_dictionary_sheet(dictionary_file, "d5_dboard_specific"),
      c("code", "label", "description", "dashboard")
    ),
    source_types = select_existing_columns(
      read_topo_dictionary_sheet(dictionary_file, "source_type"),
      c("label", "description")
    )
  )

  sources <- normalize_source_records(
    read_topo_dictionary_sheet(dictionary_file, "Sources")
  )

  list(
    version = 1L,
    series_index = series_index,
    dictionary = dictionary,
    sources = sources
  )
}
