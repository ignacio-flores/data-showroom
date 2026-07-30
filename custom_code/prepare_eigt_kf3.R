library(data.table)
library(qs)

input_file <- Sys.getenv(
  "DATA_SHOWROOM_TAXW_SOURCE",
  unset = "data/taxw_warehouse_meta_v2.csv"
)
output_file <- "data/taxw_kf3_ready.qs"

data <- as.data.frame(data.table::fread(
  input_file,
  select = c(
    "GEO",
    "GEO_long",
    "year",
    "value",
    "d2_sector",
    "d2_sector_lab",
    "d4_concept_lab"
  ),
  encoding = "UTF-8",
  showProgress = FALSE
))

sys.source("custom_code/data_prep_eigt3.R", envir = environment())

required_columns <- c(
  "geo",
  "geo_long",
  "year",
  "tax_type_view",
  "revenue_tax_category",
  "xrate_lab",
  "top_marginal_rate",
  "exemption_threshold",
  "total_revenue_from_tax",
  "total_revenue_from_tax_as_percent_of_total_tax_revenue",
  "total_revenue_from_tax_as_percent_of_gross_domestic_product",
  "d2_sector",
  "d2_sector_lab",
  "tax_type",
  "kinship",
  "revenue_d2_sector",
  "revenue_d2_sector_lab"
)
missing_columns <- setdiff(required_columns, names(data))
if (length(missing_columns) > 0) {
  stop(
    "KF3 artifact is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

data <- data[, required_columns, drop = FALSE]
qs::qsave(data, output_file, preset = "high")
message("Saved ", nrow(data), " rows to ", output_file)
