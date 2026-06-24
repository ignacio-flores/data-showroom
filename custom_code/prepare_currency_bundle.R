library(data.table)
library(qs)

source("modules/value_transform.R")

input_file <- "data/supplementary_var_long.csv"
output_file <- "data/currency_conversion_bundle.qs"

if (!file.exists(input_file)) {
  stop("Missing supplementary variable file: ", input_file)
}

currency_specs <- currency_unit_currency_specs()

supp <- data.table::fread(
  input_file,
  select = c("country", "year", "variable", "value"),
  showProgress = FALSE
)
supp <- supp[variable %in% currency_columns_required_supplementary_vars()]
supp[, year := suppressWarnings(as.numeric(year))]
supp[, value := suppressWarnings(as.numeric(value))]

cpi <- supp[
  variable == "inyixx",
  .(GEO = country, year, cpi = value)
]

xrates_current <- supp[
  variable %in% stats::na.omit(currency_specs$xrate_var[currency_specs$xrate_lookup == "current"]),
  .(GEO = country, year, xrate_var = variable, xrate = value)
]

xrates_2023 <- supp[
  variable %in% stats::na.omit(currency_specs$xrate_var) & year == 2023,
  .(GEO = country, xrate_var = variable, xrate = value)
]

bundle <- list(
  version = 1L,
  currency_choices = currency_specs$label,
  cpi = as.data.frame(cpi),
  xrates_current = as.data.frame(xrates_current),
  xrates_2023 = as.data.frame(xrates_2023)
)

qs::qsave(bundle, output_file, preset = "fast")
message("Saved currency conversion bundle to ", output_file)
