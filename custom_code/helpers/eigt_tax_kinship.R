eigt_tax_type_choices <- c("Gift tax", "Inheritance tax", "Estate tax")
eigt_inheritance_estate_tax_view <- "Inheritance or estate tax"
eigt_tax_type_view_choices <- c(eigt_inheritance_estate_tax_view, eigt_tax_type_choices)
eigt_kinship_choices <- c(
  "Children",
  "Spouse",
  "Siblings",
  "Other Relatives",
  "Non-Relatives",
  "Everybody",
  "Applies to unknown",
  "General government level"
)
eigt_visible_kinship_choices <- c("Children", "Everybody")

split_eigt_tax_kinship <- function(labels) {
  labels <- as.character(labels)
  known_labels <- !is.na(labels)

  tax_type <- rep(NA_character_, length(labels))
  tax_type[known_labels & grepl("^Gift Tax", labels)] <- "Gift tax"
  tax_type[known_labels & grepl("^Inheritance Tax", labels)] <- "Inheritance tax"
  tax_type[known_labels & grepl("^Estate Tax", labels)] <- "Estate tax"

  kinship <- rep(NA_character_, length(labels))

  for_values <- known_labels & grepl(" Tax for ", labels)
  kinship[for_values] <- sub("^.* Tax for ", "", labels[for_values])

  unknown_values <- known_labels & grepl(" Tax, applies to unknown$", labels)
  kinship[unknown_values] <- "Applies to unknown"

  general_values <- known_labels & grepl(" Tax, general government level$", labels)
  kinship[general_values] <- "General government level"

  data.frame(
    tax_type = tax_type,
    kinship = kinship,
    stringsAsFactors = FALSE
  )
}

add_eigt_tax_kinship <- function(data, label_col) {
  if (!label_col %in% names(data)) {
    stop("Missing EIGT label column: ", label_col, call. = FALSE)
  }

  split_values <- split_eigt_tax_kinship(data[[label_col]])
  data$tax_type <- split_values$tax_type
  data$kinship <- split_values$kinship
  data
}

filter_eigt_visible_kinships <- function(data) {
  if (!"kinship" %in% names(data)) {
    stop("Missing EIGT kinship column.", call. = FALSE)
  }

  data[data$kinship %in% eigt_visible_kinship_choices, , drop = FALSE]
}

add_eigt_inheritance_estate_view <- function(data, view_col = "tax_type_view") {
  required_cols <- c("GEO", "GEO_long", "year", "tax_type", "kinship")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing EIGT inheritance/estate view column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  data[[view_col]] <- data$tax_type

  source_rows <- data[
    data$tax_type %in% c("Inheritance tax", "Estate tax"),
    ,
    drop = FALSE
  ]
  if (nrow(source_rows) == 0) {
    return(data)
  }

  key_cols <- c("GEO", "GEO_long", "year", "kinship")
  key_parts <- lapply(key_cols, function(col) {
    value <- as.character(source_rows[[col]])
    value[is.na(value)] <- "<NA>"
    value
  })
  group_key <- do.call(paste, c(key_parts, sep = "\r"))

  chosen_idx <- unlist(
    lapply(split(seq_len(nrow(source_rows)), group_key), function(idx) {
      types <- source_rows$tax_type[idx]
      if (any(types == "Inheritance tax")) {
        idx[types == "Inheritance tax"]
      } else {
        idx[types == "Estate tax"]
      }
    }),
    use.names = FALSE
  )

  combined_rows <- source_rows[chosen_idx, , drop = FALSE]
  combined_rows[[view_col]] <- eigt_inheritance_estate_tax_view

  rbind(data, combined_rows)
}
