#!/usr/bin/env Rscript

library(qs)
library(yaml)

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

data_file <- "data/eigt_ft_wide.qs"
if (!file.exists(data_file)) {
  fail("Missing data/eigt_ft_wide.qs. Run custom_code/data_prep_eigt_wide.R first.")
}

for (config_file in c(
  "yaml/config_eigt_ft1.yaml",
  "yaml/config_eigt_ft2.yaml",
  "yaml/config_eigt_ft3.yaml"
)) {
  config <- yaml::read_yaml(config_file)
  expect_true(
    "exempt" %in% config$keep.col,
    paste(config_file, "should retain exempt for full-exemption FT schedules.")
  )
}

data <- qs::qread(data_file)
source("custom_code/data_prep_eigt_ft.R")

us_recent_spouse <- data[
  data$GEO_long == "United States" &
    as.numeric(data$year) >= 2020 &
    data$tax_type_view == "Inheritance or estate tax" &
    data$kinship == "Spouse" &
    !is.na(data$adjlbo) &
    !is.na(data$adjmrt),
  ,
  drop = FALSE
]

expect_true(
  nrow(us_recent_spouse) > 0,
  "United States recent spouse full-exemption schedules should be available in FT data."
)
expect_true(
  any(us_recent_spouse$adjmrt == 0),
  "United States recent spouse FT schedules should include a 0% marginal tax rate."
)

cat("EIGT FT full-exemption checks passed.\n")
