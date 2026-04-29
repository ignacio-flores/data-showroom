currency_unit_currency_specs <- function() {
  data.frame(
    label = c(
      "Local currency, nominal prices",
      "Yuan, Market Exchange rate, nominal prices",
      "Euro, Market Exchange rate, nominal prices",
      "USD, Market Exchange rate, nominal prices",
      "Local currency (2023 prices)",
      "Yuan (2023 prices)",
      "Euro (2023 prices)",
      "USD (2023 prices)",
      "Yuan (2023 PPP)",
      "Euro (2023 PPP)",
      "USD (2023 PPP)"
    ),
    price = c(
      "nominal", "nominal", "nominal", "nominal",
      "real", "real", "real", "real", "real", "real", "real"
    ),
    xrate_var = c(
      NA_character_,
      "xlcyux", "xlceux", "xlcusx",
      NA_character_,
      "xlcyux", "xlceux", "xlcusx",
      "xlcyup", "xlceup", "xlcusp"
    ),
    xrate_lookup = c(
      "none",
      "current", "current", "current",
      "none",
      "fixed_2023", "fixed_2023", "fixed_2023",
      "fixed_2023", "fixed_2023", "fixed_2023"
    ),
    stringsAsFactors = FALSE
  )
}

currency_unit_unit_specs <- function() {
  data.frame(
    label = c(
      "Macroeconomic total",
      "Per capita",
      "Per adult",
      "Per employed population",
      "Per tax unit",
      "Ratio to National Income"
    ),
    denominator_var = c(
      NA_character_,
      "npopul",
      "npopul_adu",
      "npopem",
      "ntaxma",
      "mnninc"
    ),
    stringsAsFactors = FALSE
  )
}

currency_unit_required_supplementary_vars <- function() {
  unique(c(
    "inyixx",
    stats::na.omit(currency_unit_currency_specs()$xrate_var),
    stats::na.omit(currency_unit_unit_specs()$denominator_var)
  ))
}

load_value_transform_bundle <- function(bundle_file) {
  if (is.null(bundle_file) || !nzchar(bundle_file)) {
    stop("value_transform$bundle.file is required for currency_unit transforms.")
  }
  if (!file.exists(bundle_file)) {
    stop(
      "Value transform bundle not found: ", bundle_file,
      ". Build the configured transform bundle before launching this config."
    )
  }
  bundle <- qs::qread(bundle_file)
  required <- c("currency_choices", "unit_choices", "cpi", "xrates_current",
                "xrates_2023", "denominators")
  missing <- setdiff(required, names(bundle))
  if (length(missing) > 0) {
    stop("Value transform bundle is missing: ", paste(missing, collapse = ", "))
  }
  bundle
}

inject_value_transform_selector_choices <- function(fixed_selectors, value_transform, bundle) {
  currency_selector <- value_transform$currency_selector %||% "xrate_lab"
  unit_selector <- value_transform$unit_selector %||% "pop_lab"

  if (!currency_selector %in% names(fixed_selectors)) {
    fixed_selectors[[currency_selector]] <- list(label = "Currency")
  }
  if (!unit_selector %in% names(fixed_selectors)) {
    fixed_selectors[[unit_selector]] <- list(label = "Unit")
  }

  fixed_selectors[[currency_selector]]$choices <- bundle$currency_choices
  fixed_selectors[[unit_selector]]$choices <- bundle$unit_choices
  fixed_selectors
}

value_transform_selector_value <- function(input, selector_info, selector_name, fallback = NULL) {
  selected <- NULL
  if (!is.null(input) && !is.null(input[[selector_name]]) && length(input[[selector_name]]) > 0) {
    selected <- input[[selector_name]][[1]]
  }
  if ((is.null(selected) || !nzchar(selected)) &&
      selector_name %in% names(selector_info) &&
      "selected" %in% names(selector_info[[selector_name]])) {
    configured <- selector_info[[selector_name]]$selected
    if (!is.null(configured) && length(configured) > 0) {
      selected <- configured[[1]]
    }
  }
  if (is.null(selected) || !nzchar(selected)) fallback else selected
}

materialize_currency_unit <- function(data, bundle, currency_label, unit_label,
                                      debt_negative = TRUE) {
  currency_specs <- currency_unit_currency_specs()
  unit_specs <- currency_unit_unit_specs()

  currency_spec <- currency_specs[currency_specs$label == currency_label, , drop = FALSE]
  if (nrow(currency_spec) != 1) {
    stop("Unknown currency label: ", currency_label)
  }

  unit_spec <- unit_specs[unit_specs$label == unit_label, , drop = FALSE]
  if (nrow(unit_spec) != 1) {
    stop("Unknown unit label: ", unit_label)
  }

  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, value := suppressWarnings(as.numeric(value))]

  if (isTRUE(debt_negative) && "d4_concept_lab" %in% names(dt)) {
    dt[d4_concept_lab == "Debt", value := -value]
  }

  dt[, xrate_lab := currency_label]
  dt[, xrate := 1]

  if (identical(currency_spec$price[[1]], "real")) {
    cpi <- data.table::as.data.table(bundle$cpi)
    dt <- merge(dt, cpi, by = c("GEO", "year"), all.x = TRUE, sort = FALSE)
    dt[, value := value / cpi]
    dt[, cpi := NULL]
  }

  xrate_var <- currency_spec$xrate_var[[1]]
  xrate_lookup <- currency_spec$xrate_lookup[[1]]
  if (!is.na(xrate_var) && !identical(xrate_lookup, "none")) {
    dt[, xrate := NULL]
    selected_xrate_var <- xrate_var
    xrates <- if (identical(xrate_lookup, "current")) {
      data.table::as.data.table(bundle$xrates_current)
    } else {
      data.table::as.data.table(bundle$xrates_2023)
    }
    xrates <- xrates[xrate_var == selected_xrate_var]
    xrates[, xrate_var := NULL]

    if (identical(xrate_lookup, "current")) {
      dt <- merge(dt, xrates, by = c("GEO", "year"), all = FALSE, sort = FALSE)
    } else {
      dt <- merge(dt, xrates, by = "GEO", all = FALSE, sort = FALSE)
    }
    dt[, value := value / xrate]
  }

  denominator_var <- unit_spec$denominator_var[[1]]
  if (is.na(denominator_var)) {
    dt[, pop := 1]
    dt[, pop_lab := unit_label]
  } else {
    selected_denominator_var <- denominator_var
    denominators <- data.table::as.data.table(bundle$denominators)
    denominators <- denominators[denominator_var == selected_denominator_var]
    denominators[, denominator_var := NULL]
    dt <- merge(dt, denominators, by = c("GEO", "year"), all = FALSE, sort = FALSE)
    dt[, pop_lab := unit_label]
    dt[, value := value / pop]
  }

  sort_cols <- intersect(c("GEO", "xrate", "pop", "year"), names(dt))
  if (length(sort_cols) > 0) {
    data.table::setorderv(dt, sort_cols)
  }

  as.data.frame(dt)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Backward-compatible aliases for older scripts/configs.
topo_currency_specs <- currency_unit_currency_specs
topo_unit_specs <- currency_unit_unit_specs
topo_required_supplementary_vars <- currency_unit_required_supplementary_vars
topo_load_conversion_bundle <- load_value_transform_bundle
topo_inject_value_selector_choices <- inject_value_transform_selector_choices
topo_selector_value <- value_transform_selector_value
topo_materialize_currency_unit <- materialize_currency_unit
