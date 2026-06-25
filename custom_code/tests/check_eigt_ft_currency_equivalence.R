library(data.table)
library(qs)
library(zoo)

source("modules/value_transform.R")

base_file <- "data/eigt_ft_wide.qs"
bundle_file <- "data/currency_conversion_bundle.qs"

if (!file.exists(base_file) || !file.exists(bundle_file)) {
  stop("Run custom_code/data_prep_eigt_wide.R and custom_code/prepare_currency_bundle.R before checking EIGT ft currency conversion.")
}

assert_ft_typtax_filter <- function(data) {
  dt <- data.table::as.data.table(data.table::copy(data))
  if (!"typtax" %in% names(dt)) {
    stop("FT data is missing typtax.")
  }

  schedule_typtax <- dt[
    ,
    .(typtax_values = list(unique(typtax[!is.na(typtax)]))),
    by = .(GEO, year, d2_label)
  ]
  invalid_multi <- schedule_typtax[lengths(typtax_values) > 1]
  if (nrow(invalid_multi) > 0) {
    stop("FT data contains schedules with multiple typtax values.")
  }

  schedule_typtax[
    ,
    schedule_typtax := vapply(
      typtax_values,
      function(values) if (length(values) == 0) NA_real_ else as.numeric(values[[1]]),
      numeric(1)
    )
  ]
  invalid <- schedule_typtax[
    is.na(schedule_typtax) | !schedule_typtax %in% c(2, 4)
  ]
  if (nrow(invalid) > 0) {
    stop("FT data contains schedules outside typtax values 2 and 4.")
  }
}

prepare_ft_schedule <- function(data) {
  dt <- data.table::as.data.table(data.table::copy(data))
  data.table::setorderv(dt, c("d2_label", "GEO", "year", "adjlbo", "d5_code"))
  dt[, adjmrt := zoo::na.locf(adjmrt, na.rm = FALSE), by = .(GEO, d2_label, year)]
  dt[, year := as.character(year)]
  dt[, .sum_rate := sum(adjmrt, na.rm = TRUE), by = .(GEO, d2_label, year)]
  dt <- dt[.sum_rate != 0]
  dt[, .sum_rate := NULL]
  dt[]
}

legacy_usd_2023 <- function(data, bundle) {
  dt <- data.table::as.data.table(data.table::copy(data))
  cpi <- data.table::as.data.table(bundle$cpi)
  usd_xrate_2023 <- data.table::as.data.table(bundle$xrates_2023)
  usd_xrate_2023 <- usd_xrate_2023[
    xrate_var == "xlcusx",
    .(GEO, usd_xrate_2023 = xrate)
  ]

  dt <- merge(dt, cpi, by = c("GEO", "year"), all.x = TRUE, sort = FALSE)
  dt <- merge(dt, usd_xrate_2023, by = "GEO", all.x = TRUE, sort = FALSE)
  dt[, adjlbo := data.table::fifelse(
    !is.na(adjlbo) & !is.na(cpi) & cpi > 0 &
      !is.na(usd_xrate_2023) & usd_xrate_2023 > 0,
    adjlbo / cpi / usd_xrate_2023 / 1e6,
    NA_real_
  )]
  dt[, c("cpi", "usd_xrate_2023") := NULL]
  prepare_ft_schedule(dt)
}

compare_columns <- function(left, right, cols, label) {
  for (col in cols) {
    if (!identical(left[[col]], right[[col]])) {
      stop(label, ": column mismatch for ", col)
    }
  }
}

base <- qs::qread(base_file)
assert_ft_typtax_filter(base)
bundle <- load_value_transform_bundle(bundle_file, require_units = FALSE)

prepared <- prepare_ft_schedule(base)
lazy_usd <- materialize_currency_columns(
  prepared,
  bundle,
  currency_label = "USD (2023 prices)",
  columns = "adjlbo",
  scale_divisor = 1e6,
  currency_selector = "xrate_lab"
)
legacy <- legacy_usd_2023(base, bundle)

key_cols <- c("GEO", "GEO_long", "year", "d2_label", "d5_code")
compare_cols <- c(key_cols, "adjmrt")

lazy_drawable <- data.table::as.data.table(lazy_usd)[!is.na(adjlbo)]
legacy_drawable <- data.table::as.data.table(legacy)[!is.na(adjlbo)]
data.table::setorderv(lazy_drawable, c(key_cols, "adjlbo"))
data.table::setorderv(legacy_drawable, c(key_cols, "adjlbo"))

if (nrow(lazy_drawable) != nrow(legacy_drawable)) {
  stop("USD (2023 prices): row count mismatch: lazy=", nrow(lazy_drawable),
       ", legacy=", nrow(legacy_drawable))
}

compare_columns(lazy_drawable, legacy_drawable, compare_cols, "USD (2023 prices)")

adjlbo_diff <- max(abs(lazy_drawable$adjlbo - legacy_drawable$adjlbo), na.rm = TRUE)
if (!is.finite(adjlbo_diff) || adjlbo_diff > 1e-8) {
  stop("USD (2023 prices): adjlbo mismatch, max diff=", adjlbo_diff)
}

local_nominal <- materialize_currency_columns(
  prepared,
  bundle,
  currency_label = "Local currency, nominal prices",
  columns = "adjlbo",
  scale_divisor = 1e6,
  currency_selector = "xrate_lab"
)

expected_local <- suppressWarnings(as.numeric(prepared$adjlbo)) / 1e6
local_diff <- max(abs(local_nominal$adjlbo - expected_local), na.rm = TRUE)
if (!is.finite(local_diff) || local_diff > 1e-12) {
  stop("Local currency, nominal prices: adjlbo mismatch, max diff=", local_diff)
}

if (!identical(unique(lazy_usd$xrate_lab), "USD (2023 prices)")) {
  stop("USD transform did not stamp xrate_lab correctly.")
}
if (!identical(unique(local_nominal$xrate_lab), "Local currency, nominal prices")) {
  stop("Local nominal transform did not stamp xrate_lab correctly.")
}

prepared_sample <- prepared[seq_len(min(nrow(prepared), 5000))]
for (currency_label in bundle$currency_choices) {
  materialized <- materialize_currency_columns(
    prepared_sample,
    bundle,
    currency_label = currency_label,
    columns = "adjlbo",
    scale_divisor = 1e6,
    currency_selector = "xrate_lab"
  )
  if (nrow(materialized) != nrow(prepared_sample)) {
    stop(currency_label, ": materialization changed row count.")
  }
  if (!identical(unique(materialized$xrate_lab), currency_label)) {
    stop(currency_label, ": materialization did not stamp xrate_lab correctly.")
  }
}

message("All EIGT ft currency equivalence checks passed.")
