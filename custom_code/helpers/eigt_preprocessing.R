eigt_full_exemption_sentinel <- -997
eigt_exemption_concept_label <- "Exemption Threshold"
eigt_exemption_code <- "exempt"

is_eigt_subregion_geo <- function(geo) {
  geo <- as.character(geo)
  !is.na(geo) & grepl("^[A-Za-z]{2}[-_].+", geo)
}

is_eigt_exemption_threshold <- function(concept = NULL, d4_code = NULL) {
  n <- max(length(concept), length(d4_code), 1L)

  if (is.null(concept)) {
    concept <- rep(NA_character_, n)
  } else {
    concept <- rep(as.character(concept), length.out = n)
  }

  if (is.null(d4_code)) {
    d4_code <- rep(NA_character_, n)
  } else {
    d4_code <- rep(as.character(d4_code), length.out = n)
  }

  (!is.na(concept) & concept == eigt_exemption_concept_label) |
    (!is.na(d4_code) & d4_code == eigt_exemption_code)
}

normalize_eigt_full_exemption_values <- function(value,
                                                 concept = NULL,
                                                 d4_code = NULL,
                                                 sentinel = eigt_full_exemption_sentinel) {
  numeric_value <- suppressWarnings(as.numeric(value))
  full_exemption <- is_eigt_exemption_threshold(concept, d4_code) &
    !is.na(numeric_value) &
    numeric_value == sentinel

  out <- value
  out[full_exemption] <- if (is.numeric(out) || is.integer(out)) 0 else "0"
  out
}

normalize_eigt_wide_exemptions <- function(data, exempt_col = eigt_exemption_code) {
  if (!exempt_col %in% names(data)) return(data)

  data[[exempt_col]] <- normalize_eigt_full_exemption_values(
    data[[exempt_col]],
    d4_code = exempt_col
  )
  data
}
