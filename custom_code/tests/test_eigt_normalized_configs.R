#!/usr/bin/env Rscript

library(yaml)

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

expect_identical <- function(actual, expected, message) {
  if (!identical(actual, expected)) {
    fail(paste0(
      message,
      "\nExpected: ", paste(expected, collapse = ", "),
      "\nActual: ", paste(actual, collapse = ", ")
    ))
  }
}

parse_choices <- function(value) {
  if (is.character(value) && length(value) == 1 && grepl("^c\\(", value)) {
    return(eval(parse(text = value)))
  }
  value
}

tax_choices <- c(
  "Inheritance or estate tax",
  "Inheritance tax",
  "Estate tax",
  "Gift tax"
)
currency_choices <- c(
  "National Currency",
  "National Currency adjusting for inflation",
  "USD",
  "USD adjusting for inflation",
  "Euro",
  "Euro adjusting for inflation",
  "Yuan",
  "Yuan adjusting for inflation",
  "PPP USD",
  "PPP Euro",
  "PPP Yuan"
)
revenue_choices <- c(
  "Estate, inheritance and gift taxes (EIG)",
  "Gift tax"
)
combined_tax_view <- tax_choices[[1]]
combined_revenue_view <- revenue_choices[[1]]
compact_dual_axis_options <- list(
  prevent_duplicate_metrics = TRUE,
  hover = list(
    mode = "compact",
    context_vars = list(
      GEO_long = list(
        label = "Country"
      ),
      tax_type_view = list(
        label = "Type of tax",
        show_for = c("toprat", "exempt")
      ),
      revenue_tax_category = list(
        label = "Revenue category",
        show_for = c("revenu", "prorev", "revgdp")
      ),
      xrate_lab = list(
        label = "Currency",
        show_for = c("revenu", "exempt")
      )
    )
  )
)

target_ids <- c("wm1", "wm2", "kf1", "kf2", "kf3", "us1", "us2")
configs <- setNames(
  lapply(
    target_ids,
    function(id) yaml::read_yaml(paste0("yaml/config_eigt_", id, ".yaml"))
  ),
  target_ids
)

selector_info <- function(config, name) {
  if (name %in% names(config$fixed_selectors)) {
    return(config$fixed_selectors[[name]])
  }
  if (name %in% names(config$loose_selectors)) {
    return(config$loose_selectors[[name]])
  }
  NULL
}

for (target_id in c("kf1", "kf3")) {
  config <- configs[[target_id]]
  tax_selector <- selector_info(config, "tax_type_view")
  expect_true(
    !is.null(tax_selector),
    paste(target_id, "should expose the normalized tax_type_view selector.")
  )
  expect_identical(
    tax_selector$choices,
    tax_choices,
    paste(target_id, "should expose the approved tax views in order.")
  )
  revenue_selector <- selector_info(config, "revenue_tax_category")
  expect_true(
    !is.null(revenue_selector),
    paste(target_id, "should expose an independent revenue category selector.")
  )
  expect_identical(
    revenue_selector$label,
    "Revenue category",
    paste(target_id, "should label its independent revenue selector clearly.")
  )
  expect_identical(
    revenue_selector$choices,
    revenue_choices,
    paste(target_id, "should expose aggregate EIG and Gift revenue in order.")
  )
  expect_identical(
    revenue_selector$selected,
    revenue_choices[[1]],
    paste(target_id, "should default to aggregate EIG revenue.")
  )
  expect_true(
    !is.null(tax_selector$visible_when$any) &&
      !is.null(revenue_selector$visible_when$any),
    paste(target_id, "should conditionally show selectors by active axis family.")
  )
  expect_true(
    all(c("tax_type_view", "revenue_tax_category") %in% names(config$dt.cols)),
    paste(target_id, "should show both independent category dimensions in its table.")
  )
  if (identical(target_id, "kf1")) {
    expect_identical(
      config$dual_axis_options,
      compact_dual_axis_options,
      "KF1 should opt into the approved compact dual-axis interaction."
    )
    expect_true(
      is.null(config$tooltip_vars),
      "KF1 compact hover should replace the exhaustive tooltip_vars block."
    )
  } else {
    expect_true(
      all(c(
        "tax_type_view",
        "revenue_tax_category"
      ) %in% names(config$tooltip_vars)),
      "KF3 should continue showing both category dimensions in its tooltip."
    )
  }
}

for (target_id in target_ids) {
  config <- configs[[target_id]]
  surface_vars <- c(
    names(config$fixed_selectors),
    names(config$loose_selectors),
    unlist(config$color$group, use.names = FALSE),
    names(config$dt.cols),
    names(config$tooltip_vars)
  )
  expect_true(
    !"kinship" %in% surface_vars,
    paste(target_id, "should not expose or group by kinship.")
  )
  expect_true(
    all(c("tax_type", "kinship") %in% config$download.cols),
    paste(target_id, "downloads should retain raw tax and kinship provenance.")
  )
}

country_single_ids <- c("wm1", "wm2", "kf2")
for (target_id in country_single_ids) {
  config <- configs[[target_id]]
  category_selector <- selector_info(config, "tax_category")
  expect_true(
    !is.null(category_selector),
    paste(target_id, "should expose the feature-dependent Tax category selector.")
  )
  expect_identical(
    category_selector$label,
    "Tax category",
    paste(target_id, "should label the feature-dependent selector clearly.")
  )
  expect_true(
    "tax_category" %in% names(config$dt.cols) &&
      "tax_category" %in% names(config$tooltip_vars),
    paste(target_id, "should show Tax category in its table and tooltip.")
  )
  expect_true(
    all(c("tax_type_view", "revenue_tax_category") %in% config$download.cols),
    paste(target_id, "downloads should retain both category dimensions.")
  )
}

for (target_id in c("wm1", "wm2", "kf1", "kf2", "kf3")) {
  currency_selector <- selector_info(configs[[target_id]], "xrate_lab")
  expect_identical(
    currency_selector$choices,
    currency_choices,
    paste(target_id, "should expose the established 11 currency choices.")
  )
  expect_true(
    "xrate_lab" %in% names(configs[[target_id]]$dt.cols),
    paste(target_id, "should show the selected currency in its table.")
  )
  if (identical(target_id, "kf1")) {
    expect_true(
      "xrate_lab" %in%
        names(configs[[target_id]]$dual_axis_options$hover$context_vars),
      "KF1 should provide currency through compact dual-axis context."
    )
  } else {
    expect_true(
      "xrate_lab" %in% names(configs[[target_id]]$tooltip_vars),
      paste(target_id, "should show the selected currency in its tooltip.")
    )
  }
}

expect_identical(
  configs$wm1$loose_selectors$tax_category$selected,
  combined_tax_view,
  "WM1 should default to the combined inheritance-or-estate view."
)
for (target_id in c("wm2", "kf2", "us1", "us2")) {
  expect_identical(
    selector_info(configs[[target_id]], "tax_category")$selected,
    combined_revenue_view,
    paste(target_id, "should default its revenue feature to aggregate EIG.")
  )
}
for (target_id in c("wm2", "kf2")) {
  expect_identical(
    selector_info(configs[[target_id]], "tax_category")$select,
    "first",
    paste(target_id, "should select the first available feature category.")
  )
}
for (target_id in c("wm1", "wm2", "kf2", "us1", "us2")) {
  expect_identical(
    selector_info(configs[[target_id]], "tax_category")$type,
    "sticky selector",
    paste(target_id, "should expose Tax category as a single-value selector.")
  )
}
for (target_id in c("kf1", "kf3")) {
  expect_identical(
    selector_info(configs[[target_id]], "tax_type_view")$selected,
    combined_tax_view,
    paste(target_id, "should default to the combined inheritance-or-estate view.")
  )
  expect_identical(
    selector_info(configs[[target_id]], "xrate_lab")$selected,
    "Euro adjusting for inflation",
    paste(target_id, "should default to real euros.")
  )
}

expect_identical(
  parse_choices(configs$kf1$axis_vars$y_axis$choices),
  c("revenu", "prorev", "revgdp"),
  "KF1 left-axis choices should include currency-adjusted total tax revenue."
)
expect_identical(
  parse_choices(configs$kf1$axis_vars$y2_axis$choices),
  c("toprat", "exempt", "prorev", "revgdp"),
  "KF1 right-axis choices should include currency-adjusted exemption thresholds."
)
expect_true(
  all(grepl(
    "selected currency",
    c(
      configs$kf3$axis_vars$x_axis$alt.names,
      configs$kf3$axis_vars$y_axis$alt.names
    ),
    fixed = TRUE
  ) | !grepl(
    "Exemption|Revenue",
    c(
      configs$kf3$axis_vars$x_axis$alt.names,
      configs$kf3$axis_vars$y_axis$alt.names
    )
  )),
  "KF3 monetary axis labels should refer to the selected currency."
)

for (target_id in c("kf1", "kf3")) {
  config <- configs[[target_id]]
  visible_labels <- c(
    parse_choices(config$axis_vars$x_axis$alt.names),
    parse_choices(config$axis_vars$y_axis$alt.names),
    parse_choices(config$axis_vars$y2_axis$alt.names),
    unlist(config$dt.cols, use.names = FALSE),
    unlist(config$tooltip_vars, use.names = FALSE),
    unlist(config$dual_axis_options$hover$context_vars, use.names = FALSE)
  )
  visible_labels <- visible_labels[!is.na(visible_labels)]
  expect_true(
    !any(grepl("EIG Tax", visible_labels, fixed = TRUE)),
    paste(target_id, "should use revenue-category-neutral measure labels.")
  )
}

preview_config <- yaml::read_yaml("yaml/config_eigt-prev.yaml")
expect_identical(
  preview_config$dual_axis_options,
  compact_dual_axis_options,
  "eigt-prev should opt into the same approved compact dual-axis interaction."
)
expect_true(
  is.null(preview_config$tooltip_vars),
  "eigt-prev compact hover should replace the exhaustive tooltip_vars block."
)
expect_true(
  isTRUE(preview_config$hide.selectors) && isTRUE(preview_config$listen),
  "eigt-prev should remain a hidden, externally controlled preview."
)
expect_true(
  all(c(
    "GEO",
    "d2_code",
    "d2_label",
    "status",
    "toprat",
    "exempt",
    "revenu",
    "prorev",
    "revgdp"
  ) %in% preview_config$keep.col),
  "eigt-prev should retain every source column required by the KF1 wrangler."
)
expect_true(
  !"kinship" %in% c(
    names(preview_config$fixed_selectors),
    names(preview_config$loose_selectors),
    unlist(preview_config$color$group, use.names = FALSE),
    names(preview_config$dt.cols),
    names(preview_config$tooltip_vars)
  ),
  "eigt-prev should not expose or group by kinship."
)
expect_identical(
  preview_config$fixed_selectors$tax_type_view$selected,
  combined_tax_view,
  "eigt-prev should default its legal family to the combined view."
)
expect_identical(
  preview_config$fixed_selectors$revenue_tax_category$selected,
  combined_revenue_view,
  "eigt-prev should default its revenue family to aggregate EIG."
)

for (target_id in c("ft1", "ft2", "ft3", "us3")) {
  config <- yaml::read_yaml(paste0("yaml/config_eigt_", target_id, ".yaml"))
  expect_identical(
    config$fixed_selectors$tax_type_view$selected,
    combined_tax_view,
    paste(target_id, "should default its legal tax selector to the combined view.")
  )
}
expect_identical(
  preview_config$fixed_selectors$xrate_lab$selected,
  "Euro adjusting for inflation",
  "eigt-prev should fix one hidden currency view and avoid 11-fold plot duplication."
)

inhe_dual_config <- yaml::read_yaml("yaml/config_inhe_dual.yaml")
expect_true(
  is.null(inhe_dual_config$dual_axis_options),
  "inhe_dual should remain opted out of the new dual-axis interaction."
)
expect_identical(
  inhe_dual_config$tooltip_vars,
  list(
    GEO_long = "Country:",
    year = "Year:",
    inheritances_gifts_ratio = "Inheritances & gifts ratio:",
    average_effective_tax_rate = "Average effective tax rate:",
    inheritances_gifts_aggregate = "Inheritances & gifts aggregate:",
    legend = "Source:"
  ),
  "inhe_dual should retain its existing exhaustive tooltip configuration."
)

message("OK: normalized EIGT visualization configs match the approved interfaces.")
