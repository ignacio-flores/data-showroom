#!/usr/bin/env Rscript

source("modules/wrap_text.R")

loader_env <- new.env(parent = globalenv())
loader_env$tic <- function(...) invisible(NULL)
loader_env$toc <- function(...) invisible(NULL)
loader_env$wrap_text <- wrap_text
loader_env$remind_dictionary <- function(file_path, sheet_names) {
  for (sheet_name in sheet_names) {
    assign(sheet_name, as.data.frame(readxl::read_excel(file_path, sheet = sheet_name)),
           envir = loader_env)
  }

  # An unmapped type must not remove a source from the visualization.
  new_source <- loader_env$Sources[loader_env$Sources$Source == "FED_DWA_ineq", , drop = FALSE]
  new_source$Source <- "TEST_NEW_TYPE"
  new_source$Data_Type <- "New source category"
  loader_env$Sources <- rbind(loader_env$Sources, new_source)
}

loader_env$data <- data.frame(
  source = c("ECB_DWA_ineq", "FED_DWA_ineq", "TEST_NEW_TYPE", "WID_ineq"),
  d3_vartype_lab = rep("Share of Total Net Wealth", 4),
  d4_concept_lab = rep("Net Wealth", 4),
  percentile = rep("p90p100", 4),
  value = c(55, 60, 65, 70)
)

source("custom_code/dictionary_loader_ineq.R", local = loader_env)

result <- loader_env$data
stopifnot(nrow(result) == 4L)
stopifnot(setequal(result$source, c("ECB_DWA_ineq", "FED_DWA_ineq",
                                      "TEST_NEW_TYPE", "WID_ineq")))

for (source_name in c("ECB_DWA_ineq", "FED_DWA_ineq")) {
  row <- result[result$source == source_name, , drop = FALSE]
  stopifnot(row$Data_Type == "Wealth survey and national accounts")
  stopifnot(row$data_description == row$Data_Type)
}

new_type <- result[result$source == "TEST_NEW_TYPE", , drop = FALSE]
stopifnot(new_type$Data_Type == "New source category")
stopifnot(new_type$data_description == "New source category")

known_type <- result[result$source == "WID_ineq", , drop = FALSE]
stopifnot(!is.na(known_type$data_description))
stopifnot(gsub("<br>", " ", known_type$data_description, fixed = TRUE) !=
            known_type$source_type_description)

cat("OK: inequality sources survive unmapped data types.\n")
