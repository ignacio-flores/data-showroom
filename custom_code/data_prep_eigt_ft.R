library(dplyr)
library(zoo)

# CPI lookup (base year = 2023)
cpi <- read.csv("data/supplementary_var_long.csv") %>%
  filter(variable == "inyixx") %>%
  transmute(GEO = country, year = as.numeric(year), cpi = value)

# 2023 market USD exchange rate (LCU per USD)
usd_xrate_2023 <- read.csv("data/supplementary_var_long.csv") %>%
  filter(variable == "xlcusx", year == 2023) %>%
  transmute(GEO = country, usd_xrate_2023 = value)

# 1) Convert bracket bounds to 2023 USD millions:
# nominal LCU -> real LCU 2023 -> real USD 2023 -> millions
data <- data %>%
  left_join(cpi, by = c("GEO", "year")) %>%
  left_join(usd_xrate_2023, by = "GEO") %>%
  mutate(
    adjlbo = if_else(
      !is.na(adjlbo) & !is.na(cpi) & cpi > 0 & !is.na(usd_xrate_2023) & usd_xrate_2023 > 0,
      adjlbo / cpi / usd_xrate_2023 / 1e6,
      NA_real_
    )
  )

# 2) Fill step schedule + 3) remove zero-rate schedules + 4) year as character.
# Right-end horizontal extension is handled in PlotServer step logic.
data <- data %>%
  arrange(d2_label, GEO, year, adjlbo, d5_code) %>%
  group_by(GEO, d2_label, year) %>%
  mutate(adjmrt = zoo::na.locf(adjmrt, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(year = as.character(year)) %>%
  group_by(GEO, d2_label, year) %>%
  mutate(`_sum_rate` = sum(adjmrt, na.rm = TRUE)) %>%
  filter(`_sum_rate` != 0) %>%
  ungroup() %>%
  select(-`_sum_rate`, -cpi, -usd_xrate_2023)
