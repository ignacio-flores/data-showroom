#!/usr/bin/env Rscript

source("custom_code/helpers/ineq_source_units.R")

rows <- data.frame(
  source = c("HFCS_ineq", "HFCS_ineq", "HFCS_ineq", "HFCS_ineq",
             "HFCS_ineq", "WID_ineq", "WID_ineq"),
  d3_vartype_lab = c("Share of Total Net Wealth", "Share of Total Net Wealth",
                     "Gini Coefficient", "Share of Total Net Wealth",
                     "Share of Total Net Wealth", "Share of Total Net Wealth",
                     "Gini Coefficient"),
  d4_concept_lab = c("Net Wealth", "Net Wealth", "Net Wealth",
                     "Housing & Land", "Debt", "Net Wealth", "Net Wealth"),
  percentile = c("p0p100", "p0p50", "p0p100", "p0p100", "p0p100",
                 "p0p50", "p0p100"),
  value = c(10000, 544.789429, 0.61, 84.25, -3.1, 5.1, 73.2)
)

normalized <- normalize_ineq_source_units(rows)
stopifnot(isTRUE(all.equal(normalized$value,
                           c(100, 5.44789429, 61, 84.25, -3.1, 5.1, 73.2))))
stopifnot(isTRUE(all.equal(
  normalize_ineq_source_units(normalized)$value,
  normalized$value
)))

cat("OK: HFCS inequality source units match the other sources.\n")
