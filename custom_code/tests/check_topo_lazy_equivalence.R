library(data.table)
library(qs)

source("modules/value_transform.R")

base_file <- "data/topo_base.qs"
bundle_file <- "data/topo_conversion_bundle.qs"

if (!file.exists(base_file) || !file.exists(bundle_file)) {
  stop("Run custom_code/prepare_topo_bundle.R before checking lazy topo equivalence.")
}

legacy_selected_topo_data <- function(data, bundle, currency_label, unit_label,
                                      debt_negative = TRUE) {
  currency_specs <- currency_unit_currency_specs()
  unit_specs <- currency_unit_unit_specs()
  currency_spec <- currency_specs[currency_specs$label == currency_label, , drop = FALSE]
  unit_spec <- unit_specs[unit_specs$label == unit_label, , drop = FALSE]

  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, value := suppressWarnings(as.numeric(value))]
  if (isTRUE(debt_negative)) {
    dt[d4_concept_lab == "Debt", value := -value]
  }

  if (identical(currency_spec$price[[1]], "real")) {
    cpi <- data.table::as.data.table(bundle$cpi)
    dt <- merge(dt, cpi, by = c("GEO", "year"), all.x = TRUE, sort = FALSE)
    dt[, value := value / cpi]
    dt[, cpi := NULL]
  }

  dt[, xrate_lab := currency_label]
  dt[, xrate := 1]

  selected_xrate_var <- currency_spec$xrate_var[[1]]
  if (!is.na(selected_xrate_var)) {
    dt[, xrate := NULL]
    xrates <- if (identical(currency_spec$xrate_lookup[[1]], "current")) {
      data.table::as.data.table(bundle$xrates_current)
    } else {
      data.table::as.data.table(bundle$xrates_2023)
    }
    xrates <- xrates[xrate_var == selected_xrate_var]
    xrates[, xrate_var := NULL]
    if (identical(currency_spec$xrate_lookup[[1]], "current")) {
      dt <- merge(dt, xrates, by = c("GEO", "year"), all = FALSE, sort = FALSE)
    } else {
      dt <- merge(dt, xrates, by = "GEO", all = FALSE, sort = FALSE)
    }
    dt[, value := value / xrate]
  }

  selected_denominator_var <- unit_spec$denominator_var[[1]]
  if (is.na(selected_denominator_var)) {
    dt[, pop := 1]
    dt[, pop_lab := unit_label]
  } else {
    denominators <- data.table::as.data.table(bundle$denominators)
    denominators <- denominators[denominator_var == selected_denominator_var]
    denominators[, denominator_var := NULL]
    dt <- merge(dt, denominators, by = c("GEO", "year"), all = FALSE, sort = FALSE)
    dt[, pop_lab := unit_label]
    dt[, value := value / pop]
  }

  data.table::setorderv(dt, intersect(c("GEO", "year", "legend", "d2_sector_lab",
                                        "d4_concept_lab", "xrate_lab", "pop_lab"),
                                      names(dt)))
  as.data.frame(dt)
}

compare_selection <- function(base, bundle, currency_label, unit_label, debt_negative) {
  lazy <- materialize_currency_unit(
    base,
    bundle,
    currency_label = currency_label,
    unit_label = unit_label,
    debt_negative = debt_negative
  )
  legacy <- legacy_selected_topo_data(
    base,
    bundle,
    currency_label = currency_label,
    unit_label = unit_label,
    debt_negative = debt_negative
  )

  cols <- intersect(names(legacy), names(lazy))
  lazy <- data.table::as.data.table(lazy[, cols, drop = FALSE])
  legacy <- data.table::as.data.table(legacy[, cols, drop = FALSE])
  sort_cols <- setdiff(cols, c("value", "xrate", "pop", "metadata"))
  data.table::setorderv(lazy, sort_cols)
  data.table::setorderv(legacy, sort_cols)

  if (nrow(lazy) != nrow(legacy)) {
    stop(currency_label, " / ", unit_label, ": row count mismatch: ",
         nrow(lazy), " vs ", nrow(legacy))
  }
  comparison <- all.equal(
    as.data.frame(lazy),
    as.data.frame(legacy),
    tolerance = 1e-9,
    check.attributes = FALSE
  )
  if (!isTRUE(comparison)) {
    stop(currency_label, " / ", unit_label, ": ", paste(comparison, collapse = "; "))
  }
  message("OK: ", currency_label, " / ", unit_label,
          " / debt_negative=", debt_negative, " (", nrow(lazy), " rows)")
}

base <- qs::qread(base_file)
bundle <- load_value_transform_bundle(bundle_file)

checks <- list(
  list("Euro (2023 prices)", "Per capita", TRUE),
  list("Local currency, nominal prices", "Macroeconomic total", TRUE),
  list("USD, Market Exchange rate, nominal prices", "Ratio to National Income", TRUE),
  list("Euro (2023 prices)", "Per capita", FALSE)
)

for (check in checks) {
  compare_selection(base, bundle, check[[1]], check[[2]], check[[3]])
}

message("All lazy topo equivalence checks passed.")
