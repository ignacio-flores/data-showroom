eigt_currency_specs <- function() {
  data.frame(
    label = c(
      "National Currency",
      "National Currency adjusting for inflation",
      "USD",
      "USD adjusting for inflation",
      "Euro",
      "Euro adjusting for inflation",
      "Yuan",
      "Yuan adjusting for inflation",
      "PPP USD",
      "PPP Euro",
      "PPP Yuan"
    ),
    real = c(
      FALSE, TRUE,
      FALSE, TRUE,
      FALSE, TRUE,
      FALSE, TRUE,
      TRUE, TRUE, TRUE
    ),
    xrate_var = c(
      NA_character_, NA_character_,
      "xlcusx", "xlcusx",
      "xlceux", "xlceux",
      "xlcyux", "xlcyux",
      "xlcusp", "xlceup", "xlcyup"
    ),
    xrate_lookup = c(
      "none", "none",
      "current", "fixed_2023",
      "current", "fixed_2023",
      "current", "fixed_2023",
      "fixed_2023", "fixed_2023", "fixed_2023"
    ),
    stringsAsFactors = FALSE
  )
}

eigt_currency_choices <- eigt_currency_specs()$label
eigt_currency_monetary_concepts <- c(
  "Exemption Threshold",
  "Total Revenue from Tax"
)

first_eigt_currency_value <- function(x) {
  values <- x[!is.na(x)]
  if (length(values) == 0) NA_real_ else as.numeric(values[[1]])
}

load_eigt_currency_factors <- function(
    supplementary_file = "data/supplementary_var_long.csv") {
  if (!file.exists(supplementary_file)) {
    stop("Missing EIGT supplementary currency file: ", supplementary_file, call. = FALSE)
  }

  required_variables <- unique(c(
    "inyixx",
    stats::na.omit(eigt_currency_specs()$xrate_var)
  ))

  supplementary <- data.table::fread(
    supplementary_file,
    select = c("country", "year", "variable", "value"),
    showProgress = FALSE
  )
  supplementary <- supplementary[variable %in% required_variables]
  supplementary[, year := suppressWarnings(as.numeric(year))]
  supplementary[, value := suppressWarnings(as.numeric(value))]

  cpi <- supplementary[
    variable == "inyixx",
    .(value = first_eigt_currency_value(value)),
    by = .(GEO = country, year)
  ]

  current_variables <- stats::na.omit(
    eigt_currency_specs()$xrate_var[
      eigt_currency_specs()$xrate_lookup == "current"
    ]
  )
  xrates_current <- supplementary[
    variable %in% current_variables,
    .(value = first_eigt_currency_value(value)),
    by = .(GEO = country, year, xrate_var = variable)
  ]

  fixed_variables <- stats::na.omit(eigt_currency_specs()$xrate_var)
  xrates_2023 <- supplementary[
    variable %in% fixed_variables & year == 2023,
    .(value = first_eigt_currency_value(value)),
    by = .(GEO = country, xrate_var = variable)
  ]

  list(
    cpi = as.data.frame(cpi),
    xrates_current = as.data.frame(xrates_current),
    xrates_2023 = as.data.frame(xrates_2023)
  )
}

match_eigt_currency_factor <- function(
    lookup,
    geo,
    year = NULL,
    xrate_var = NULL) {
  if (!is.null(xrate_var)) {
    lookup <- lookup[lookup$xrate_var == xrate_var, , drop = FALSE]
  }

  lookup_geo <- as.character(lookup$GEO)
  geo <- as.character(geo)

  if (!is.null(year)) {
    lookup_year <- suppressWarnings(as.numeric(as.character(lookup$year)))
    year <- suppressWarnings(as.numeric(as.character(year)))
    lookup_key <- paste(lookup_geo, lookup_year, sep = "\r")
    data_key <- paste(geo, year, sep = "\r")
  } else {
    lookup_key <- lookup_geo
    data_key <- geo
  }

  lookup$value[match(data_key, lookup_key)]
}

expand_eigt_currency_views <- function(
    data,
    factors,
    monetary_cols,
    monetary_rows = NULL,
    scale_divisor = 1,
    geo_col = "GEO",
    year_col = "year",
    currency_col = "xrate_lab") {
  required_factor_names <- c("cpi", "xrates_current", "xrates_2023")
  missing_factors <- setdiff(required_factor_names, names(factors))
  if (length(missing_factors) > 0) {
    stop(
      "Missing EIGT currency factor table(s): ",
      paste(missing_factors, collapse = ", "),
      call. = FALSE
    )
  }

  monetary_cols <- unique(as.character(monetary_cols))
  monetary_cols <- monetary_cols[!is.na(monetary_cols) & nzchar(monetary_cols)]
  required_cols <- unique(c(geo_col, year_col, monetary_cols))
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing EIGT currency input column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(monetary_cols) == 0) {
    stop("EIGT currency expansion requires at least one monetary column.", call. = FALSE)
  }

  scale_divisor <- suppressWarnings(as.numeric(scale_divisor))
  if (length(scale_divisor) != 1 || is.na(scale_divisor) || scale_divisor <= 0) {
    stop("EIGT currency scale_divisor must be a positive number.", call. = FALSE)
  }

  if (is.null(monetary_rows)) {
    monetary_rows <- rep(TRUE, nrow(data))
  }
  monetary_rows <- as.logical(monetary_rows)
  if (length(monetary_rows) != nrow(data) || any(is.na(monetary_rows))) {
    stop(
      "EIGT monetary_rows must be a non-missing logical vector aligned to data.",
      call. = FALSE
    )
  }

  base_data <- as.data.frame(data)
  base_data[monetary_cols] <- lapply(
    base_data[monetary_cols],
    function(value) suppressWarnings(as.numeric(as.character(value)))
  )

  geo <- base_data[[geo_col]]
  year <- base_data[[year_col]]
  specs <- eigt_currency_specs()

  expanded <- lapply(seq_len(nrow(specs)), function(spec_idx) {
    spec <- specs[spec_idx, , drop = FALSE]
    out <- base_data
    out[[currency_col]] <- spec$label[[1]]

    divisor <- rep(1, nrow(out))
    valid_factor <- rep(TRUE, nrow(out))

    if (isTRUE(spec$real[[1]])) {
      cpi <- match_eigt_currency_factor(factors$cpi, geo = geo, year = year)
      valid_cpi <- !is.na(cpi) & cpi > 0
      divisor[valid_cpi] <- divisor[valid_cpi] * cpi[valid_cpi]
      valid_factor <- valid_factor & valid_cpi
    }

    xrate_var <- spec$xrate_var[[1]]
    xrate_lookup <- spec$xrate_lookup[[1]]
    if (!is.na(xrate_var) && !identical(xrate_lookup, "none")) {
      if (identical(xrate_lookup, "current")) {
        xrate <- match_eigt_currency_factor(
          factors$xrates_current,
          geo = geo,
          year = year,
          xrate_var = xrate_var
        )
      } else {
        xrate <- match_eigt_currency_factor(
          factors$xrates_2023,
          geo = geo,
          xrate_var = xrate_var
        )
      }
      valid_xrate <- !is.na(xrate) & xrate > 0
      divisor[valid_xrate] <- divisor[valid_xrate] * xrate[valid_xrate]
      valid_factor <- valid_factor & valid_xrate
    }

    for (col in monetary_cols) {
      values <- out[[col]]
      converted <- values
      convertible <- monetary_rows & !is.na(values) & valid_factor
      unavailable <- monetary_rows & !is.na(values) & !valid_factor
      converted[convertible] <- values[convertible] /
        divisor[convertible] /
        scale_divisor
      converted[unavailable] <- NA_real_
      out[[col]] <- converted
    }

    out
  })

  result <- do.call(rbind, expanded)
  rownames(result) <- NULL
  result
}
