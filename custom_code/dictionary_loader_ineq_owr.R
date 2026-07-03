source("custom_code/dictionary_loader_ineq.R", local = TRUE)

owr_rows <- rep(FALSE, nrow(data))
if ("varcode" %in% names(data)) {
  owr_rows <- owr_rows | grepl("(^|-)owr($|-)", data$varcode, ignore.case = TRUE)
}
if ("d3_vartype_lab" %in% names(data)) {
  owr_rows <- owr_rows | data$d3_vartype_lab %in% "Ownership Rate"
}

data <- data[owr_rows, , drop = FALSE]
