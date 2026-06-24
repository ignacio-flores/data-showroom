eigt_tax_type_choices <- c("Gift tax", "Inheritance tax", "Estate tax")
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
