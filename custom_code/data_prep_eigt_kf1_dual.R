library(dplyr)

first_non_na <- function(x) {
  vals <- x[!is.na(x)]
  if (length(vals) == 0) {
    return(NA_real_)
  }
  as.numeric(vals[1])
}

join_keys <- intersect(c("GEO", "GEO_long", "year"), names(data))

revenue_df <- data %>%
  filter(d2_label == "EIG Tax, general government level", !is.na(year)) %>%
  group_by(across(all_of(join_keys))) %>%
  summarise(
    prorev = first_non_na(prorev),
    revgdp = first_non_na(revgdp),
    .groups = "drop"
  )

tax_df <- data %>%
  filter(!is.na(d2_label), d2_label != "EIG Tax, general government level", !is.na(year)) %>%
  group_by(across(all_of(c(join_keys, "d2_label")))) %>%
  summarise(
    toprat = first_non_na(toprat),
    .groups = "drop"
  )

data <- tax_df %>%
  left_join(revenue_df, by = join_keys) %>%
  arrange(GEO_long, d2_label, year)
