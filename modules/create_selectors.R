require(shiny)
require(shinyWidgets)

createSelectors <- function(data, selector_info, num.conversion, extra_layer) {
  
  # Determine the number of selectors
  numSelectors <- length(selector_info)
  
  if (!is.null(num.conversion)) numSelectors <- numSelectors + 1

  # Generate each selector based on its type and properties
  selectorInputs <- lapply(names(selector_info), function(var) {
    info <- selector_info[[var]]
    inputType <- ifelse("type" %in% names(info), info$type, "select") # Default to dropdown

    # Define choices as all unique values in the column by default. 
    #choices <- sort(unique(data[[var]]))
    # Define choices explicitly removing extra_layer values if necessary
    if (exists("extra_layer") && !is.null(extra_layer$values) && var == color_var) {
      extra_layer_values <- unlist(extra_layer$values)
      # Explicitly remove extra_layer values from selector
      choices <- sort(setdiff(unique(data[[var]]), extra_layer_values))
    } else {
      choices <- sort(unique(data[[var]]))
    }
    
    
    # Default to select all if type is "checkbox" 
    selected <- if (inputType == "checkbox") {
      if ("selected" %in% names(info)) info$selected else choices
    } else {
      if ("selected" %in% names(info)) info$selected else NULL
    }
    label <- if ("label" %in% names(info)) info$label else var
    
    # Create input control based on type
    inputControl <- if (inputType == "checkbox") {
      pickerInput(
        inputId = var,
        label = label,
        choices = choices,
        selected = selected,
        multiple = TRUE,  
        options = list(
          `actions-box` = TRUE, # Adds "Select All/Deselect All" buttons
          `live-search` = TRUE, # Adds search functionality
          `dropdown-align-right` = TRUE, # Align dropdown to the right (optional)
          `selected-text-format` = "count", # Show count instead of items
          `count-selected-text` =  "{0} selected" # Show "All" if all selected
        )
      )
    } else if (inputType == "selector" ) {
      pickerInput(
        inputId = var,
        label = label,
        choices = choices,
        selected = selected
      )
    } else {
      selectInput(
        inputId = var,
        label = label,
        choices = choices,
        selected = selected,
        multiple = ifelse("multiple" %in% names(info), info$multiple, FALSE)
      )
    }

    # Define column width dynamically based on the number of selectors
    columnWidth <- 12 / min(numSelectors, 4)
    column(width = columnWidth, inputControl)
  })

  # Add the conversion selector if num.conversion is not NULL
  if (!is.null(num.conversion)) {
    conversionChoices <- sapply(unname(num.conversion), function(x) x$label)
    conversionSelector <- selectInput(
      inputId = "conversion",
      label = "Conversion",
      choices = conversionChoices
    )
    conversionColumn <- column(width = 12 / min(numSelectors, 4), conversionSelector)
    selectorInputs <- append(selectorInputs, list(conversionColumn))
  }

  # Arrange all selectors in a fluid row
  do.call(fluidRow, selectorInputs)
}

