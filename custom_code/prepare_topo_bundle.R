library(data.table)
library(qs)
library(readxl)

source("modules/value_transform.R")
source("custom_code/topo_metadata_bundle.R")

input_file <- "data/topo_warehouse_meta_v2.csv"
supplementary_file <- "data/supplementary_var_long.csv"
dictionary_file <- "data/dictionary.xlsx"
base_output_file <- "data/topo_base.qs"
bundle_output_file <- "data/topo_conversion_bundle.qs"
metadata_output_file <- "data/topo_metadata_bundle.qs"

if (!file.exists(input_file)) {
  stop("Missing topo warehouse file: ", input_file)
}
if (!file.exists(supplementary_file)) {
  stop("Missing supplementary variable file: ", supplementary_file)
}
if (!file.exists(dictionary_file)) {
  stop("Missing dictionary file: ", dictionary_file)
}

base_cols <- c(
  "GEO",
  "GEO_long",
  "year",
  "value",
  "source",
  "legend",
  "metadata",
  "d2_sector_lab",
  "d4_concept_lab"
)

available_cols <- names(data.table::fread(input_file, nrows = 0, showProgress = FALSE))
base_cols <- intersect(base_cols, available_cols)

base <- data.table::fread(
  input_file,
  select = base_cols,
  showProgress = FALSE
)
base[, value := suppressWarnings(as.numeric(value))]

supp <- data.table::fread(
  supplementary_file,
  select = c("country", "year", "variable", "value"),
  showProgress = FALSE
)
supp <- supp[variable %in% currency_unit_required_supplementary_vars()]
supp[, year := suppressWarnings(as.numeric(year))]
supp[, value := suppressWarnings(as.numeric(value))]

cpi <- supp[
  variable == "inyixx",
  .(GEO = country, year, cpi = value)
]

xrates_current <- supp[
  variable %in% c("xlceux", "xlcusx", "xlcyux"),
  .(GEO = country, year, xrate_var = variable, xrate = value)
]

xrates_2023 <- supp[
  variable %in% stats::na.omit(currency_unit_currency_specs()$xrate_var) & year == 2023,
  .(GEO = country, xrate_var = variable, xrate = value)
]

denominators <- supp[
  variable %in% stats::na.omit(currency_unit_unit_specs()$denominator_var),
  .(GEO = country, year, denominator_var = variable, pop = value)
]

bundle <- list(
  version = 1L,
  currency_choices = currency_unit_currency_specs()$label,
  unit_choices = currency_unit_unit_specs()$label,
  cpi = as.data.frame(cpi),
  xrates_current = as.data.frame(xrates_current),
  xrates_2023 = as.data.frame(xrates_2023),
  denominators = as.data.frame(denominators)
)

qs::qsave(as.data.frame(base), base_output_file, preset = "fast")
qs::qsave(bundle, bundle_output_file, preset = "fast")
qs::qsave(
  build_topo_metadata_bundle(input_file, dictionary_file),
  metadata_output_file,
  preset = "fast"
)

message("Saved ", nrow(base), " rows to ", base_output_file)
message("Saved conversion bundle to ", bundle_output_file)
message("Saved topo metadata bundle to ", metadata_output_file)
