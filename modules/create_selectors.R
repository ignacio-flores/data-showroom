require(shiny)
require(shinyWidgets)

# Enhanced createSelectors: supports axis choice alt.names separately from selector title labels
createSelectors <- function(data,
                            selector_info,
                            axis_vars = NULL,
                            num.conversion = NULL,
                            extra_layer = NULL) {
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
  baseCount     <- length(selector_info)
  hasX          <- !is.null(axis_vars) && !is.null(axis_vars$x_axis$choices)
  hasY          <- !is.null(axis_vars) && !is.null(axis_vars$y_axis$choices)
  axisCount     <- sum(c(hasX, hasY))
  convCount     <- if (!is.null(num.conversion)) 1 else 0
  totalControls <- baseCount + axisCount + convCount
  columns       <- min(totalControls, 4)
  colWidth      <- 12 / columns
  
  inputs <- list()
  
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
    
    inputs <- c(inputs, list(
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
    
    inputs <- c(inputs, list(
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
  
  # Other selectors from selector_info
  selectorCols <- lapply(names(selector_info), function(var) {
    info <- selector_info[[var]]
    type <- if ("type" %in% names(info)) info$type else "select"
    
    # Exclude extra_layer values when var matches
    if (!is.null(extra_layer) && !is.null(extra_layer$var) && var == extra_layer$var && !is.null(extra_layer$values)) {
      excl_vals <- unlist(extra_layer$values)
      choices <- sort(setdiff(unique(data[[var]]), excl_vals))
    } else {
      choices <- sort(unique(data[[var]]))
    }
    
    # Default selection
    if (type == "checkbox") {
      sel <- if ("selected" %in% names(info)) info$selected else choices
    } else {
      sel <- if ("selected" %in% names(info)) info$selected else NULL
    }
    # Title label for control
    lbl <- if ("label" %in% names(info) && is.character(info$label)) info$label else var
    
    ctrl <- switch(type,
                   checkbox = pickerInput(
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
                   ),
                   selector = pickerInput(
                     inputId = var,
                     label   = lbl,
                     choices = choices,
                     selected= sel
                   ),
                   selectInput(
                     inputId = var,
                     label   = lbl,
                     choices = choices,
                     selected= sel,
                     multiple = if ("multiple" %in% names(info)) info$multiple else FALSE
                   )
    )
    column(width = colWidth, ctrl)
  })
  inputs <- c(inputs, selectorCols)
  
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

