normalize_ineq_source_units <- function(data) {
  required <- c("source", "d3_vartype_lab", "d4_concept_lab", "value")
  if (!all(required %in% names(data))) return(data)

  hfcs_net_wealth <- data$source == "HFCS_ineq" &
    data$d4_concept_lab == "Net Wealth"

  # HFCS net-wealth shares use 10,000 for the full distribution, while the
  # other sources use 100. Leave the other HFCS concepts on their own scale.
  share_rows <- which(hfcs_net_wealth &
    data$d3_vartype_lab == "Share of Total Net Wealth")
  total_rows <- if ("percentile" %in% names(data)) {
    share_rows[data$percentile[share_rows] == "p0p100"]
  } else {
    integer(0)
  }
  total_values <- data$value[total_rows]
  total_values <- total_values[is.finite(total_values)]
  if (length(total_values) > 0L && median(total_values) > 1000) {
    data$value[share_rows] <- data$value[share_rows] / 100
  }

  # HFCS Gini coefficients are fractions; the other sources use 0–100.
  gini_rows <- which(hfcs_net_wealth &
    data$d3_vartype_lab == "Gini Coefficient")
  gini_values <- data$value[gini_rows]
  gini_values <- gini_values[is.finite(gini_values)]
  if (length(gini_values) > 0L && median(abs(gini_values)) < 2) {
    data$value[gini_rows] <- data$value[gini_rows] * 100
  }

  data
}
