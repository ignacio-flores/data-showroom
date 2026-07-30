eigt_tax_type_choices <- c("Inheritance tax", "Estate tax", "Gift tax")
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

eigt_group_key <- function(data, key_cols) {
  key_parts <- lapply(key_cols, function(col) {
    value <- as.character(data[[col]])
    value[is.na(value)] <- "<NA>"
    value
  })
  do.call(paste, c(key_parts, sep = "\r"))
}

select_eigt_canonical_kinship <- function(
    data,
    key_cols = c("GEO", "GEO_long", "year", "tax_type"),
    priority = eigt_visible_kinship_choices) {
  required_cols <- unique(c(key_cols, "kinship"))
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing EIGT canonical-kinship column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  priority <- unique(as.character(priority))
  priority <- priority[!is.na(priority) & nzchar(priority)]
  if (length(priority) == 0) {
    stop("EIGT canonical-kinship priority must not be empty.", call. = FALSE)
  }

  data <- data[data$kinship %in% priority, , drop = FALSE]
  if (nrow(data) == 0) {
    return(data)
  }

  group_key <- eigt_group_key(data, key_cols)
  chosen_idx <- unlist(
    lapply(split(seq_len(nrow(data)), group_key), function(idx) {
      kinships <- as.character(data$kinship[idx])
      chosen_kinship <- priority[priority %in% kinships][[1]]
      idx[kinships == chosen_kinship]
    }),
    use.names = FALSE
  )

  data[sort(chosen_idx), , drop = FALSE]
}

add_eigt_inheritance_estate_view <- function(
    data,
    view_col = "tax_type_view",
    key_cols = NULL,
    active_col = NULL,
    kinship_priority = NULL) {
  if (is.null(key_cols)) {
    key_cols <- c("GEO", "GEO_long", "year", "kinship")
  }

  required_cols <- unique(c(key_cols, "tax_type"))
  if (!is.null(active_col)) {
    required_cols <- unique(c(required_cols, active_col))
  }
  if (!is.null(kinship_priority)) {
    required_cols <- unique(c(required_cols, "kinship"))
  }
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

  group_key <- eigt_group_key(source_rows, key_cols)

  chosen_idx <- unlist(
    lapply(split(seq_len(nrow(source_rows)), group_key), function(idx) {
      types <- source_rows$tax_type[idx]
      chosen_type <- NULL

      if (!is.null(active_col)) {
        active_values <- source_rows[[active_col]][idx]
        active <- if (is.logical(active_values)) {
          !is.na(active_values) & active_values
        } else {
          numeric_active <- suppressWarnings(as.numeric(as.character(active_values)))
          !is.na(numeric_active) & numeric_active > 0
        }
        active_types <- unique(types[active])

        if ("Inheritance tax" %in% active_types) {
          chosen_type <- "Inheritance tax"
        } else if ("Estate tax" %in% active_types) {
          chosen_type <- "Estate tax"
        }
      }

      if (is.null(chosen_type) && any(types == "Inheritance tax")) {
        chosen_type <- "Inheritance tax"
      }
      if (is.null(chosen_type)) {
        chosen_type <- "Estate tax"
      }

      selected_idx <- idx[types == chosen_type]
      if (!is.null(kinship_priority)) {
        priority <- unique(as.character(kinship_priority))
        priority <- priority[!is.na(priority) & nzchar(priority)]
        selected_kinships <- as.character(source_rows$kinship[selected_idx])
        available_priority <- priority[priority %in% selected_kinships]
        if (length(available_priority) > 0) {
          selected_idx <- selected_idx[
            selected_kinships == available_priority[[1]]
          ]
        }
      }

      if (length(selected_idx) > 0) {
        selected_idx
      } else {
        integer(0)
      }
    }),
    use.names = FALSE
  )

  combined_rows <- source_rows[chosen_idx, , drop = FALSE]
  combined_rows[[view_col]] <- eigt_inheritance_estate_tax_view

  rbind(data, combined_rows)
}
