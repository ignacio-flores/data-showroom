require(shiny)
require(shinyWidgets)

normalize_selector_type <- function(type) {
  if (is.null(type) || length(type) == 0 || is.na(type[[1]])) {
    return("select")
  }
  normalized <- tolower(trimws(as.character(type[[1]])))
  normalized <- gsub("[_-]+", " ", normalized)
  normalized <- gsub("\\s+", " ", normalized)
  if (!nzchar(normalized)) "select" else normalized
}

selector_checkbox_mode <- function(type) {
  type <- normalize_selector_type(type)
  if (identical(type, "checkbox") || identical(type, "sticky checkbox")) {
    return("sticky")
  }
  if (identical(type, "reactive checkbox")) {
    return("reactive")
  }
  if (identical(type, "very reactive checkbox")) {
    return("very_reactive")
  }
  NULL
}

selector_single_mode <- function(type) {
  type <- normalize_selector_type(type)
  if (identical(type, "selector") || identical(type, "sticky selector")) {
    return("sticky")
  }
  if (identical(type, "reactive selector")) {
    return("reactive")
  }
  if (identical(type, "very reactive selector")) {
    return("very_reactive")
  }
  NULL
}

selector_is_checkbox_like <- function(type) {
  !is.null(selector_checkbox_mode(type))
}

selector_is_single_like <- function(type) {
  !is.null(selector_single_mode(type))
}

selector_is_very_reactive <- function(type) {
  identical(selector_checkbox_mode(type), "very_reactive") ||
    identical(selector_single_mode(type), "very_reactive")
}

selector_signature_value <- function(value) {
  if (is.null(value)) return("<NULL>")
  paste(sort(as.character(value)), collapse = "\r")
}

selector_values_equal <- function(left, right) {
  identical(selector_signature_value(left), selector_signature_value(right))
}

selector_parse_choices <- function(choices) {
  if (is.character(choices) && length(choices) == 1 && grepl("^c\\(", choices)) {
    return(tryCatch(eval(parse(text = choices)), error = function(e) choices))
  }
  choices
}

selector_config_choices <- function(info) {
  if (is.null(info) || is.null(info$choices)) return(NULL)
  selector_parse_choices(info$choices)
}

selector_axis_input_choices <- function(axis_info) {
  choices <- selector_config_choices(axis_info)
  if (is.null(choices)) return(NULL)

  alt_names <- selector_parse_choices(axis_info$alt.names)
  if (!is.null(alt_names) && length(alt_names) == length(choices)) {
    names(choices) <- as.character(alt_names)
  }

  choices
}

dual_axis_prevents_duplicate_metrics <- function(dual_axis_options = NULL) {
  is.list(dual_axis_options) &&
    isTRUE(dual_axis_options$prevent_duplicate_metrics)
}

dual_axis_hover_context_columns <- function(dual_axis_options = NULL) {
  if (!is.list(dual_axis_options) ||
      !is.list(dual_axis_options$hover) ||
      is.null(dual_axis_options$hover$context_vars)) {
    return(character(0))
  }

  context_vars <- dual_axis_options$hover$context_vars
  context_names <- names(context_vars)
  if (is.null(context_names)) return(character(0))

  unique(context_names[
    !is.na(context_names) & nzchar(context_names)
  ])
}

dual_axis_y2_choice_state <- function(y_axis_value,
                                      y2_choices,
                                      current_selection = NULL,
                                      configured_default = NULL,
                                      prevent_duplicate_metrics = FALSE) {
  if (is.null(y2_choices) || length(y2_choices) == 0) {
    stop(
      "The secondary axis must have at least one configured metric choice.",
      call. = FALSE
    )
  }

  choice_values <- as.character(unname(y2_choices))
  valid_choices <- !is.na(choice_values) & nzchar(choice_values)
  y2_choices <- y2_choices[valid_choices]
  choice_values <- choice_values[valid_choices]

  if (isTRUE(prevent_duplicate_metrics)) {
    y_axis_value <- as.character(y_axis_value)
    y_axis_value <- y_axis_value[!is.na(y_axis_value) & nzchar(y_axis_value)]
    if (length(y_axis_value) > 0) {
      keep <- !choice_values %in% y_axis_value
      y2_choices <- y2_choices[keep]
      choice_values <- choice_values[keep]
    }
  }

  if (length(choice_values) == 0) {
    stop(
      paste0(
        "No secondary-axis metric remains after excluding the selected ",
        "left-axis metric."
      ),
      call. = FALSE
    )
  }

  first_valid_selection <- function(selection) {
    selection <- as.character(selection)
    selection <- selection[
      !is.na(selection) & nzchar(selection) & selection %in% choice_values
    ]
    if (length(selection) == 0) NULL else selection[[1]]
  }

  selected <- first_valid_selection(current_selection)
  if (is.null(selected)) {
    selected <- first_valid_selection(configured_default)
  }
  if (is.null(selected)) {
    selected <- choice_values[[1]]
  }

  list(
    choices = y2_choices,
    selected = selected
  )
}

validate_dual_axis_options <- function(dual_axis_options = NULL,
                                       axis_vars = NULL,
                                       gopts = NULL) {
  if (is.null(dual_axis_options)) return(invisible(TRUE))
  if (!is.list(dual_axis_options)) {
    stop("dual_axis_options must be a list.", call. = FALSE)
  }

  prevent_duplicates <- dual_axis_options$prevent_duplicate_metrics
  if (!is.null(prevent_duplicates) &&
      (!is.logical(prevent_duplicates) ||
       length(prevent_duplicates) != 1 ||
       is.na(prevent_duplicates))) {
    stop(
      "dual_axis_options$prevent_duplicate_metrics must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  hover <- dual_axis_options$hover
  if (!is.null(hover)) {
    if (!is.list(hover)) {
      stop("dual_axis_options$hover must be a list.", call. = FALSE)
    }
    if (!is.null(hover$mode)) {
      hover_mode <- as.character(hover$mode)
      if (length(hover_mode) != 1 ||
          is.na(hover_mode) ||
          !identical(tolower(trimws(hover_mode)), "compact")) {
        stop(
          "dual_axis_options$hover$mode must be 'compact'.",
          call. = FALSE
        )
      }
    }

    context_vars <- hover$context_vars
    if (!is.null(context_vars)) {
      if (!is.list(context_vars) ||
          is.null(names(context_vars)) ||
          any(is.na(names(context_vars)) | !nzchar(names(context_vars)))) {
        stop(
          paste0(
            "dual_axis_options$hover$context_vars must be a named mapping ",
            "of data columns."
          ),
          call. = FALSE
        )
      }

      for (context_name in names(context_vars)) {
        context_info <- context_vars[[context_name]]
        if (!is.list(context_info) ||
            is.null(context_info$label) ||
            length(context_info$label) != 1 ||
            is.na(context_info$label) ||
            !nzchar(as.character(context_info$label))) {
          stop(
            paste0(
              "Hover context '", context_name,
              "' must define one non-empty label."
            ),
            call. = FALSE
          )
        }
        if (!is.null(context_info$show_for)) {
          show_for <- as.character(unlist(
            context_info$show_for,
            use.names = FALSE
          ))
          if (length(show_for) == 0 ||
              any(is.na(show_for) | !nzchar(show_for))) {
            stop(
              paste0(
                "Hover context '", context_name,
                "' has an invalid show_for list."
              ),
              call. = FALSE
            )
          }
        }
      }
    }
  }

  if (dual_axis_prevents_duplicate_metrics(dual_axis_options)) {
    if (!is.null(gopts) && !"dual_axis_line" %in% gopts) {
      stop(
        paste0(
          "prevent_duplicate_metrics is only supported for ",
          "dual_axis_line plots."
        ),
        call. = FALSE
      )
    }
    if (is.null(axis_vars) || is.null(axis_vars$y_axis) ||
        is.null(axis_vars$y2_axis)) {
      stop(
        "prevent_duplicate_metrics requires y_axis and y2_axis settings.",
        call. = FALSE
      )
    }

    y2_choices <- selector_axis_input_choices(axis_vars$y2_axis)
    if (is.null(y2_choices) || length(y2_choices) == 0) {
      stop(
        paste0(
          "prevent_duplicate_metrics requires at least one configured ",
          "y2_axis choice."
        ),
        call. = FALSE
      )
    }

    y_axis_values <- unique(as.character(c(
      axis_vars$y_axis$var,
      unname(selector_axis_input_choices(axis_vars$y_axis))
    )))
    y_axis_values <- y_axis_values[
      !is.na(y_axis_values) & nzchar(y_axis_values)
    ]
    if (length(y_axis_values) == 0) {
      stop(
        "prevent_duplicate_metrics requires a configured y_axis metric.",
        call. = FALSE
      )
    }

    for (y_axis_value in y_axis_values) {
      dual_axis_y2_choice_state(
        y_axis_value = y_axis_value,
        y2_choices = y2_choices,
        configured_default = axis_vars$y2_axis$var,
        prevent_duplicate_metrics = TRUE
      )
    }
  }

  invisible(TRUE)
}

selector_select_mode <- function(select_mode = NULL) {
  if (is.null(select_mode) || length(select_mode) == 0 || is.na(select_mode[[1]])) {
    return(NULL)
  }
  normalized <- tolower(trimws(as.character(select_mode[[1]])))
  normalized <- gsub("[_-]+", " ", normalized)
  normalized <- gsub("\\s+", " ", normalized)
  if (!nzchar(normalized)) NULL else normalized
}

selector_selects_latest <- function(select_mode = NULL) {
  identical(selector_select_mode(select_mode), "latest")
}

selector_latest_choice <- function(choices) {
  choices <- choices[!is.na(choices)]
  if (length(choices) == 0) return(NULL)

  numeric_choices <- suppressWarnings(as.numeric(as.character(choices)))
  if (all(!is.na(numeric_choices))) {
    return(choices[[which.max(numeric_choices)]])
  }

  sorted_choices <- sort(choices)
  sorted_choices[[length(sorted_choices)]]
}

loose_selector_ui_signature <- function(choices, selected) {
  paste(
    selector_signature_value(choices),
    selector_signature_value(selected),
    sep = "\v"
  )
}

loose_selector_ui_needs_update <- function(previous_signature, choices, selected) {
  !identical(previous_signature, loose_selector_ui_signature(choices, selected))
}

selector_inputs_signature <- function(input, vars) {
  vars <- vars[!is.na(vars) & nzchar(vars)]
  if (length(vars) == 0) return("")
  paste(
    vapply(vars, function(var) selector_signature_value(input[[var]]), character(1)),
    collapse = "\v"
  )
}

checkbox_select_rule_choices <- function(choices, select_mode = NULL) {
  if (length(choices) == 0) return(NULL)
  if (is.null(select_mode) || length(select_mode) == 0 || is.na(select_mode[[1]])) {
    return(choices)
  }

  select_mode <- selector_select_mode(select_mode)
  if (identical(select_mode, "first")) {
    return(choices[[1]])
  }
  if (identical(select_mode, "random") && length(choices) > 5) {
    return(sample(choices, 5))
  }
  if (identical(select_mode, "spaced") && length(choices) > 5) {
    return(choices[seq(1, length(choices), length.out = 5)])
  }

  choices
}

selector_visible_when_group_condition <- function(rules, operator) {
  if (is.null(rules) || length(rules) == 0) return(NULL)
  if (is.null(names(rules)) || any(!nzchar(names(rules)))) {
    stop("visible_when groups must name each input.", call. = FALSE)
  }

  input_conditions <- lapply(names(rules), function(input_id) {
    values <- as.character(unlist(rules[[input_id]], use.names = FALSE))
    values <- values[!is.na(values)]
    if (length(values) == 0) return(NULL)

    input_ref <- paste0(
      "input[",
      jsonlite::toJSON(input_id, auto_unbox = TRUE),
      "]"
    )
    value_conditions <- vapply(values, function(value) {
      paste0(
        input_ref,
        " === ",
        jsonlite::toJSON(value, auto_unbox = TRUE)
      )
    }, character(1))

    paste0("(", paste(value_conditions, collapse = " || "), ")")
  })
  input_conditions <- Filter(Negate(is.null), input_conditions)
  if (length(input_conditions) == 0) return(NULL)

  paste0("(", paste(input_conditions, collapse = paste0(" ", operator, " ")), ")")
}

selector_visible_when_condition <- function(visible_when = NULL) {
  if (is.null(visible_when) || length(visible_when) == 0) return(NULL)
  if (!is.list(visible_when)) {
    stop("visible_when must contain an 'any' or 'all' mapping.", call. = FALSE)
  }

  supported_groups <- c("any", "all")
  unknown_groups <- setdiff(names(visible_when), supported_groups)
  if (length(unknown_groups) > 0) {
    stop(
      paste0(
        "Unsupported visible_when group(s): ",
        paste(unknown_groups, collapse = ", "),
        ". Use 'any' or 'all'."
      ),
      call. = FALSE
    )
  }

  any_condition <- selector_visible_when_group_condition(
    visible_when$any,
    operator = "||"
  )
  all_condition <- selector_visible_when_group_condition(
    visible_when$all,
    operator = "&&"
  )
  conditions <- Filter(Negate(is.null), list(any_condition, all_condition))
  if (length(conditions) == 0) {
    stop("visible_when must contain at least one input value.", call. = FALSE)
  }

  paste0("(", paste(conditions, collapse = " && "), ")")
}

single_selector_select_rule_choice <- function(choices, select_mode = NULL) {
  choices <- choices[!is.na(choices)]
  if (length(choices) == 0) return(NULL)
  if (selector_selects_latest(select_mode)) return(selector_latest_choice(choices))
  choices[[1]]
}

loose_selector_next_selection <- function(selector_type,
                                          choices,
                                          current_selection = NULL,
                                          configured_selection = NULL,
                                          select_mode = NULL,
                                          initialized = FALSE,
                                          refresh_all = FALSE,
                                          refresh_latest = FALSE,
                                          refresh_selection = FALSE) {
  choices <- choices[!is.na(choices)]
  if (length(choices) == 0) return(NULL)

  current_selection <- current_selection[current_selection %in% choices]
  configured_selection <- configured_selection[configured_selection %in% choices]

  if (selector_is_checkbox_like(selector_type)) {
    if (isTRUE(refresh_all)) return(checkbox_select_rule_choices(choices, select_mode))
    if (length(current_selection) > 0) return(current_selection)
    if (!isTRUE(initialized)) {
      if (length(configured_selection) > 0) return(configured_selection)
      return(checkbox_select_rule_choices(choices, select_mode))
    }
    return(checkbox_select_rule_choices(choices, select_mode))
  }

  if (isTRUE(refresh_selection) ||
      (isTRUE(refresh_latest) && selector_selects_latest(select_mode))) {
    if (length(configured_selection) > 0) return(configured_selection[[1]])
    return(single_selector_select_rule_choice(choices, select_mode))
  }
  if (length(current_selection) > 0) return(current_selection[[1]])
  if (length(configured_selection) > 0) return(configured_selection[[1]])
  single_selector_select_rule_choice(choices, select_mode)
}

loose_selector_should_refresh_all <- function(selector_type,
                                              initialized,
                                              change_source,
                                              own_var,
                                              loose_vars,
                                              unprocessed_change = TRUE) {
  if (!isTRUE(initialized) || !isTRUE(unprocessed_change)) return(FALSE)

  checkbox_mode <- selector_checkbox_mode(selector_type)
  if (identical(checkbox_mode, "reactive")) {
    return(identical(change_source, "__fixed__"))
  }
  if (identical(checkbox_mode, "very_reactive")) {
    return(
      identical(change_source, "__fixed__") ||
        (!is.null(change_source) && change_source %in% setdiff(loose_vars, own_var))
    )
  }

  FALSE
}

loose_selector_should_refresh_selection <- function(selector_type,
                                                    initialized,
                                                    change_source,
                                                    unprocessed_change = TRUE) {
  selector_single_mode(selector_type) %in% c("reactive", "very_reactive") &&
    isTRUE(initialized) &&
    isTRUE(unprocessed_change) &&
    identical(change_source, "__fixed__")
}

loose_selector_should_refresh_latest <- function(selector_type,
                                                 select_mode = NULL,
                                                 initialized,
                                                 change_source,
                                                 unprocessed_change = TRUE) {
  selector_selects_latest(select_mode) &&
    loose_selector_should_refresh_selection(
      selector_type,
      initialized = initialized,
      change_source = change_source,
      unprocessed_change = unprocessed_change
    )
}

loose_selector_filter_data <- function(result,
                                       loose_filters,
                                       loose_selectors,
                                       selector_initialized = NULL,
                                       exclude_vars = NULL) {
  if (is.null(result) || nrow(result) == 0) {
    return(NULL)
  }
  if (is.null(loose_selectors)) {
    return(result)
  }

  for (var in names(loose_selectors)) {
    if (var %in% exclude_vars) next
    if (!var %in% names(result)) next

    filter_values <- loose_filters[[var]]
    if (is.null(filter_values) || length(filter_values) == 0) {
      if (!is.null(selector_initialized) &&
          isTRUE(selector_initialized[[var]])) {
        return(NULL)
      }
      next
    }

    available_values <- unique(result[[var]])
    active_values <- filter_values[filter_values %in% available_values]

    if (length(active_values) == 0) {
      return(NULL)
    }

    result <- result[result[[var]] %in% active_values, , drop = FALSE]
  }

  if (is.null(result) || nrow(result) == 0) {
    return(NULL)
  }

  result
}

# Enhanced createSelectors: supports axis choice alt.names separately from selector title labels
createSelectors <- function(data,
                            selector_info,
                            axis_vars = NULL,
                            num.conversion = NULL,
                            extra_layer = NULL,
                            scatter_options = NULL,
                            bar_options = NULL,
                            dual_axis_options = NULL) {
  # Helper: parse 'c("a","b")' strings into vectors
  parseChoices <- selector_parse_choices
  
  # Layout counts
  visibleSelectors <- names(selector_info)[vapply(selector_info, function(info) {
    !isTRUE(info$hidden)
  }, logical(1))]
  baseCount     <- length(visibleSelectors)
  hasX          <- !is.null(axis_vars) && !is.null(axis_vars$x_axis$choices)
  hasY          <- !is.null(axis_vars) && !is.null(axis_vars$y_axis$choices)
  hasY2         <- !is.null(axis_vars) && !is.null(axis_vars$y2_axis$choices)
  hasXScale     <- isTRUE(scatter_options$x_scale_selector)
  hasBarScale   <- isTRUE(bar_options$axis_scale_selector)
  axisCount     <- sum(c(hasX, hasY, hasY2))
  convCount     <- if (!is.null(num.conversion)) 1 else 0
  totalControls <- baseCount + axisCount + convCount +
    if (hasXScale) 1 else 0 +
    if (hasBarScale) 1 else 0
  columns       <- min(totalControls, 4)
  colWidth      <- 12 / columns
  
  axisInputs <- list()
  
  # X-axis selector: title from x_axis$label, displayed names from x_axis$alt.names
  if (hasX) {
    x_info <- axis_vars$x_axis
    raw_ch <- parseChoices(x_info$choices)
    # Apply alternative display names if provided
    if (!is.null(x_info$alt.names)) {
      alt_names <- parseChoices(x_info$alt.names)
      if (length(alt_names) == length(raw_ch)) names(raw_ch) <- alt_names
    }
    # UI title for selector
    title_x <- if (!is.null(x_info$label) && is.character(x_info$label) && length(x_info$label) == 1)
      x_info$label else "X Axis"
    # Default selected var
    sel_x <- if (!is.null(x_info$var) && length(x_info$var) == 1)
      x_info$var else raw_ch[1]
    
    axisInputs <- c(axisInputs, list(
      column(width = colWidth,
             selectInput(
               inputId = "x_axis",
               label   = title_x,
               choices = raw_ch,
               selected= sel_x
             )
      )
    ))
  }

  if (hasXScale) {
    axisInputs <- c(axisInputs, list(
      column(width = colWidth,
             selectInput(
               inputId = "x_axis_scale",
               label = "X Axis Scale",
               choices = c("Regular Scale" = "regular", "Log Scale" = "log"),
               selected = "regular"
             )
      )
    ))
  }

  if (hasBarScale) {
    bar_scale_default <- if (
      !is.null(bar_options$axis_scale_default) &&
      length(bar_options$axis_scale_default) == 1 &&
      !is.na(bar_options$axis_scale_default) &&
      tolower(as.character(bar_options$axis_scale_default)) %in%
        c("auto", "linear", "log")
    ) {
      tolower(as.character(bar_options$axis_scale_default))
    } else {
      "linear"
    }
    axisInputs <- c(axisInputs, list(
      column(width = colWidth,
             selectInput(
               inputId = "bar_axis_scale",
               label = "Axis scale",
               choices = c(
                 "Auto" = "auto",
                 "Linear" = "linear",
                 "Logarithmic" = "log"
               ),
               selected = bar_scale_default
             )
      )
    ))
  }
  
  # Y-axis selector: title from y_axis$label, displayed names from y_axis$alt.names
  if (hasY) {
    y_info <- axis_vars$y_axis
    raw_ch <- parseChoices(y_info$choices)
    if (!is.null(y_info$alt.names)) {
      alt_names <- parseChoices(y_info$alt.names)
      if (length(alt_names) == length(raw_ch)) names(raw_ch) <- alt_names
    }
    title_y <- if (!is.null(y_info$label) && is.character(y_info$label) && length(y_info$label) == 1)
      y_info$label else "Y Axis"
    sel_y <- if (!is.null(y_info$var) && length(y_info$var) == 1)
      y_info$var else raw_ch[1]
    
    axisInputs <- c(axisInputs, list(
      column(width = colWidth,
             selectInput(
               inputId = "y_axis",
               label   = title_y,
               choices = raw_ch,
               selected= sel_y
             )
      )
    ))
  }

  # Secondary Y-axis selector: title from y2_axis$label, displayed names from y2_axis$alt.names
  if (hasY2) {
    y2_info <- axis_vars$y2_axis
    raw_ch <- selector_axis_input_choices(y2_info)
    title_y2 <- if (!is.null(y2_info$label) && is.character(y2_info$label) && length(y2_info$label) == 1)
      y2_info$label else "Y2 Axis"
    sel_y2 <- if (!is.null(y2_info$var) && length(y2_info$var) == 1)
      y2_info$var else raw_ch[1]
    if (dual_axis_prevents_duplicate_metrics(dual_axis_options)) {
      initial_y_selection <- if (hasY) {
        sel_y
      } else {
        axis_vars$y_axis$var
      }
      y2_state <- dual_axis_y2_choice_state(
        y_axis_value = initial_y_selection,
        y2_choices = raw_ch,
        configured_default = sel_y2,
        prevent_duplicate_metrics = TRUE
      )
      raw_ch <- y2_state$choices
      sel_y2 <- y2_state$selected
    }

    axisInputs <- c(axisInputs, list(
      column(width = colWidth,
             selectInput(
               inputId = "y2_axis",
               label   = title_y2,
               choices = raw_ch,
               selected= sel_y2
             )
      )
    ))
  }
  
  # Other selectors from selector_info
  selectorCols <- lapply(names(selector_info), function(var) {
    info <- selector_info[[var]]
    type <- if ("type" %in% names(info)) normalize_selector_type(info$type) else "select"
    
    # Exclude extra_layer values when var matches
    if ("choices" %in% names(info) && !is.null(info$choices)) {
      choices <- parseChoices(info$choices)
    } else if (!is.null(extra_layer) && !is.null(extra_layer$var) && var == extra_layer$var && !is.null(extra_layer$values)) {
      excl_vals <- unlist(extra_layer$values)
      choices <- sort(setdiff(unique(data[[var]]), excl_vals))
    } else if (!var %in% names(data)) {
      choices <- character(0)
    } else {
      choices <- sort(unique(data[[var]]))
    }
    
    # Default selection
    if (selector_is_checkbox_like(type)) {
      sel <- if ("selected" %in% names(info)) {
        info$selected
      } else {
        checkbox_select_rule_choices(choices, info$select)
      }
    } else {
      sel <- if ("selected" %in% names(info)) info$selected else NULL
    }
    # Title label for control
    lbl <- if ("label" %in% names(info) && is.character(info$label)) info$label else var
    
    ctrl <- if (selector_is_checkbox_like(type)) {
      pickerInput(
        inputId = var,
        label   = lbl,
        choices = choices,
        selected= sel,
        multiple= TRUE,
        options = list(
          `actions-box`          = TRUE,
          `live-search`          = TRUE,
          `dropdown-align-right` = TRUE,
          `selected-text-format` = "count",
          `count-selected-text`  = "{0} selected"
        )
      )
    } else if (selector_is_single_like(type)) {
      pickerInput(
        inputId = var,
        label   = lbl,
        choices = choices,
        selected= sel
      )
    } else {
      selectInput(
        inputId = var,
        label   = lbl,
        choices = choices,
        selected= sel,
        multiple = if ("multiple" %in% names(info)) info$multiple else FALSE
      )
    }
    if (isTRUE(info$hidden)) {
      tags$div(style = "display: none;", ctrl)
    } else {
      visible_condition <- selector_visible_when_condition(info$visible_when)
      if (is.null(visible_condition)) {
        column(width = colWidth, ctrl)
      } else {
        conditionalPanel(
          condition = visible_condition,
          ctrl,
          class = paste0("col-sm-", colWidth)
        )
      }
    }
  })
  inputs <- c(selectorCols, axisInputs)
  
  # Conversion selector if defined
  if (!is.null(num.conversion)) {
    convChoices <- sapply(num.conversion, function(x) x$label)
    convCtrl    <- selectInput(
      inputId = "conversion",
      label   = "Conversion",
      choices = convChoices
    )
    inputs <- c(inputs, list(column(width = colWidth, convCtrl)))
  }
  
  # Render row of inputs
  do.call(fluidRow, inputs)
}
