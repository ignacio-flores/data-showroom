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

selector_is_checkbox_like <- function(type) {
  !is.null(selector_checkbox_mode(type))
}

selector_signature_value <- function(value) {
  if (is.null(value)) return("<NULL>")
  paste(sort(as.character(value)), collapse = "\r")
}

selector_values_equal <- function(left, right) {
  identical(selector_signature_value(left), selector_signature_value(right))
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

  select_mode <- tolower(as.character(select_mode[[1]]))
  if (identical(select_mode, "random") && length(choices) > 5) {
    return(sample(choices, 5))
  }
  if (identical(select_mode, "spaced") && length(choices) > 5) {
    return(choices[seq(1, length(choices), length.out = 5)])
  }

  choices
}

loose_selector_next_selection <- function(selector_type,
                                          choices,
                                          current_selection = NULL,
                                          configured_selection = NULL,
                                          select_mode = NULL,
                                          initialized = FALSE,
                                          refresh_all = FALSE) {
  choices <- choices[!is.na(choices)]
  if (length(choices) == 0) return(NULL)

  current_selection <- current_selection[current_selection %in% choices]
  configured_selection <- configured_selection[configured_selection %in% choices]

  if (selector_is_checkbox_like(selector_type)) {
    if (isTRUE(refresh_all)) return(choices)
    if (length(current_selection) > 0) return(current_selection)
    if (!isTRUE(initialized)) {
      if (length(configured_selection) > 0) return(configured_selection)
      return(checkbox_select_rule_choices(choices, select_mode))
    }
    return(choices)
  }

  if (length(current_selection) > 0) return(current_selection[[1]])
  if (length(configured_selection) > 0) return(configured_selection[[1]])
  choices[[1]]
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

# Enhanced createSelectors: supports axis choice alt.names separately from selector title labels
createSelectors <- function(data,
                            selector_info,
                            axis_vars = NULL,
                            num.conversion = NULL,
                            extra_layer = NULL,
                            scatter_options = NULL) {
  # Helper: parse 'c("a","b")' strings into vectors
  parseChoices <- function(ch) {
    if (is.character(ch) && length(ch) == 1 && grepl("^c\\(", ch)) {
      tryCatch(eval(parse(text = ch)), error = function(e) {
        warning("Failed to parse choices: ", ch)
        ch
      })
    } else {
      ch
    }
  }
  
  # Layout counts
  visibleSelectors <- names(selector_info)[vapply(selector_info, function(info) {
    !isTRUE(info$hidden)
  }, logical(1))]
  baseCount     <- length(visibleSelectors)
  hasX          <- !is.null(axis_vars) && !is.null(axis_vars$x_axis$choices)
  hasY          <- !is.null(axis_vars) && !is.null(axis_vars$y_axis$choices)
  hasY2         <- !is.null(axis_vars) && !is.null(axis_vars$y2_axis$choices)
  hasXScale     <- isTRUE(scatter_options$x_scale_selector)
  axisCount     <- sum(c(hasX, hasY, hasY2))
  convCount     <- if (!is.null(num.conversion)) 1 else 0
  totalControls <- baseCount + axisCount + convCount + if (hasXScale) 1 else 0
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
    raw_ch <- parseChoices(y2_info$choices)
    if (!is.null(y2_info$alt.names)) {
      alt_names <- parseChoices(y2_info$alt.names)
      if (length(alt_names) == length(raw_ch)) names(raw_ch) <- alt_names
    }
    title_y2 <- if (!is.null(y2_info$label) && is.character(y2_info$label) && length(y2_info$label) == 1)
      y2_info$label else "Y2 Axis"
    sel_y2 <- if (!is.null(y2_info$var) && length(y2_info$var) == 1)
      y2_info$var else raw_ch[1]

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
      sel <- if ("selected" %in% names(info)) info$selected else choices
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
    } else if (identical(type, "selector")) {
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
      column(width = colWidth, ctrl)
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
