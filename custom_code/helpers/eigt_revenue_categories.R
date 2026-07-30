eigt_revenue_tax_category_eig <-
  "Estate, inheritance and gift taxes (EIG)"
eigt_revenue_tax_category_gift <- "Gift tax"

eigt_revenue_tax_category_choices <- c(
  eigt_revenue_tax_category_eig,
  eigt_revenue_tax_category_gift
)

eigt_revenue_sector_categories <- c(
  "EIG Tax, general government level" = eigt_revenue_tax_category_eig,
  "Gift Tax, general government level" = eigt_revenue_tax_category_gift
)

eigt_revenue_sector_code_categories <- c(
  "tg" = eigt_revenue_tax_category_eig,
  "gg" = eigt_revenue_tax_category_gift
)

# A single-feature selector uses the same field for policy and revenue rows.
# Placing aggregate EIG immediately before Gift preserves both required orders
# after unavailable factor levels are omitted:
#   policy: combined, inheritance, estate, gift
#   revenue: aggregate EIG, gift
eigt_tax_category_choices <- c(
  "Inheritance or estate tax",
  "Inheritance tax",
  "Estate tax",
  eigt_revenue_tax_category_eig,
  "Gift tax"
)

map_eigt_revenue_tax_category <- function(labels) {
  labels <- as.character(labels)
  mapped_labels <- unname(eigt_revenue_sector_categories[labels])
  mapped_codes <- unname(
    eigt_revenue_sector_code_categories[tolower(trimws(labels))]
  )
  mapped_labels[is.na(mapped_labels)] <- mapped_codes[is.na(mapped_labels)]
  mapped_labels
}

add_eigt_revenue_tax_category <- function(
    data,
    label_col,
    category_col = "revenue_tax_category") {
  if (!label_col %in% names(data)) {
    stop("Missing EIGT revenue-sector column: ", label_col, call. = FALSE)
  }

  data[[category_col]] <- map_eigt_revenue_tax_category(data[[label_col]])
  data
}
