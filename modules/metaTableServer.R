metaTableUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("metadataContainer"))
}

metaTableServer <- function(id, filtered_data, meta, graph, meta_layout = NULL) {
  moduleServer(id, function(input, output, session) {
    `%||%` <- function(x, y) {
      if (is.null(x) || length(x) == 0) y else x
    }

    is_topo_bundle <- function() {
      is.list(meta) &&
        !inherits(meta, "data.frame") &&
        !is.null(meta$series_index)
    }

    first_nonempty <- function(values) {
      values <- as.character(values)
      values <- values[!is.na(values) & nzchar(trimws(values))]
      if (length(values) == 0) return(NA_character_)
      values[[1]]
    }

    clean_text <- function(value) {
      value <- first_nonempty(value)
      if (is.na(value)) return(NULL)
      value
    }

    dict_match <- function(table, column, value) {
      if (is.null(table) || !column %in% names(table)) return(NULL)
      value <- clean_text(value)
      if (is.null(value)) return(NULL)
      rows <- table[table[[column]] == value, , drop = FALSE]
      if (nrow(rows) == 0) return(NULL)
      rows[1, , drop = FALSE]
    }

    lookup_label_description <- function(table, code = NULL, label = NULL) {
      row <- dict_match(table, "code", code)
      if (is.null(row)) row <- dict_match(table, "label", label)

      list(
        label = if (!is.null(row) && "label" %in% names(row)) clean_text(row$label) else clean_text(label),
        description = if (!is.null(row) && "description" %in% names(row)) clean_text(row$description) else NULL
      )
    }

    source_record <- function(row) {
      sources <- meta$sources
      if (is.null(sources) || nrow(sources) == 0) return(NULL)

      source_id <- clean_text(row$source)
      if (!is.null(source_id) && "source" %in% names(sources)) {
        matched <- sources[sources$source == source_id, , drop = FALSE]
        if (nrow(matched) > 0) return(matched[1, , drop = FALSE])
      }

      source_label <- clean_text(row$legend)
      if (!is.null(source_label) && "legend" %in% names(sources)) {
        matched <- sources[sources$legend == source_label, , drop = FALSE]
        if (nrow(matched) > 0) return(matched[1, , drop = FALSE])
      }

      NULL
    }

    source_url <- function(record) {
      if (is.null(record)) return(NULL)
      link <- if ("link" %in% names(record)) clean_text(record$link) else NULL
      ref_link <- if ("ref_link" %in% names(record)) clean_text(record$ref_link) else NULL
      url <- if (!is.null(link)) link else ref_link
      if (is.null(url) || !grepl("^https?://", url)) return(NULL)
      url
    }

    source_link <- function(label, url) {
      label <- clean_text(label)
      if (is.null(label)) return(NULL)
      if (is.null(url)) return(label)
      tags$a(
        href = url,
        target = "_blank",
        rel = "noopener noreferrer",
        label
      )
    }

    meta_field <- function(label, value) {
      value <- clean_text(value)
      if (is.null(value)) return(NULL)
      tags$p(tags$strong(paste0(label, ": ")), value)
    }

    source_field <- function(label, url) {
      label <- clean_text(label)
      if (is.null(label)) return(NULL)
      tags$p(tags$strong("Source: "), source_link(label, url))
    }

    order_values <- function(values, selected_values) {
      values <- unique(values[!is.na(values) & nzchar(trimws(values))])
      selected_values <- unique(selected_values[!is.na(selected_values) & nzchar(trimws(selected_values))])
      ordered <- selected_values[selected_values %in% values]
      c(ordered, sort(setdiff(values, ordered)))
    }

    processed_topo_meta <- reactive({
      req(filtered_data(), is_topo_bundle())

      filtered <- filtered_data()
      req(nrow(filtered) > 0)

      series <- meta$series_index
      match_cols <- intersect(
        c("GEO", "GEO_long", "legend", "d2_sector_lab", "d4_concept_lab"),
        intersect(names(filtered), names(series))
      )

      for (col in match_cols) {
        selected <- unique(filtered[[col]])
        selected <- selected[!is.na(selected)]
        series <- series[series[[col]] %in% selected, , drop = FALSE]
      }

      if (nrow(series) == 0) return(series)

      dict <- meta$dictionary
      enriched <- lapply(seq_len(nrow(series)), function(i) {
        row <- series[i, , drop = FALSE]

        source <- source_record(row)
        concept <- lookup_label_description(
          dict$concepts,
          code = row$d4_concept,
          label = row$d4_concept_lab
        )
        sector <- lookup_label_description(
          dict$sectors,
          code = row$d2_sector,
          label = row$d2_sector_lab
        )
        variable_type <- lookup_label_description(
          dict$dashboard_specific,
          code = row$d5_dboard_specific,
          label = row$d5_dboard_specific_lab
        )

        country <- dict_match(dict$countries, "GEO", row$GEO)
        source_type_label <- if (!is.null(source) && "source_type" %in% names(source)) {
          clean_text(source$source_type)
        } else {
          clean_text(row$source_type)
        }
        source_type <- lookup_label_description(
          dict$source_types,
          label = source_type_label
        )

        data.frame(
          GEO = clean_text(row$GEO) %||% "",
          country_label = clean_text(if (!is.null(country) && "Country" %in% names(country)) country$Country else row$GEO_long) %||% "",
          source_id = clean_text(row$source) %||% "",
          source_label = clean_text(if (!is.null(source) && "legend" %in% names(source)) source$legend else row$legend) %||% "",
          source_url = source_url(source) %||% "",
          source_type_label = clean_text(source_type$label %||% source_type_label) %||% "",
          source_type_description = clean_text(source_type$description) %||% "",
          sector_label = clean_text(sector$label) %||% "",
          sector_description = clean_text(sector$description) %||% "",
          variable_type_label = clean_text(variable_type$label) %||% "",
          variable_type_description = clean_text(variable_type$description) %||% "",
          concept_code = clean_text(row$d4_concept) %||% "",
          concept_label = clean_text(concept$label) %||% "",
          variable_description = clean_text(concept$description) %||% "",
          technical_description = clean_text(row$metadata) %||% "",
          stringsAsFactors = FALSE
        )
      })

      unique(do.call(rbind, enriched))
    })

    topo_css <- tags$style(HTML("
      .topo-meta-scroll {
        max-height: 690px;
        overflow: auto;
        border-top: 1px solid #d9d9d9;
        border-bottom: 1px solid #d9d9d9;
      }
      .topo-meta-columns {
        display: grid;
        min-width: 960px;
      }
      .topo-meta-column {
        border-left: 1px solid #d9d9d9;
        padding: 0 14px 20px;
      }
      .topo-meta-column:first-child {
        border-left: 0;
      }
      .topo-meta-heading {
        position: sticky;
        top: 0;
        z-index: 2;
        background: #fff;
        margin: 0;
        padding: 16px 0 24px;
        text-align: center;
        line-height: 1.35;
      }
      .topo-meta-entry {
        border-top: 1px solid #e5e5e5;
        padding: 10px 0 14px;
      }
      .topo-meta-entry p {
        margin: 0 0 8px;
        line-height: 1.45;
      }
      .topo-meta-entry a,
      .topo-meta-matrix a {
        color: #2b6aa0;
      }
      .topo-meta-matrix {
        border-collapse: collapse;
        min-width: 1120px;
        width: 100%;
      }
      .topo-meta-matrix th,
      .topo-meta-matrix td {
        border: 1px solid #d9d9d9;
        vertical-align: top;
        padding: 10px 14px;
        min-width: 330px;
      }
      .topo-meta-matrix th {
        position: sticky;
        top: 0;
        z-index: 2;
        background: #fff;
        text-align: center;
        line-height: 1.35;
      }
      .topo-meta-matrix th:first-child,
      .topo-meta-matrix td:first-child {
        min-width: 230px;
        width: 230px;
        position: sticky;
        left: 0;
        z-index: 1;
        background: #fff;
      }
      .topo-meta-matrix th:first-child {
        z-index: 3;
      }
      .topo-meta-row-label {
        font-weight: 600;
      }
    "))

    render_entry <- function(row) {
      tagList(
        meta_field("Country", row$country_label),
        source_field(row$source_label, clean_text(row$source_url)),
        meta_field("Source type", row$source_type_label),
        meta_field("Sector", row$sector_label),
        meta_field("Sector description", row$sector_description),
        meta_field("Variable type", row$variable_type_label),
        meta_field("Variable description", row$variable_description),
        meta_field("Variable technical description", row$technical_description)
      )
    }

    render_column_layout <- function(data) {
      filtered <- filtered_data()
      concepts <- order_values(data$concept_label, filtered$d4_concept_lab)
      if (length(concepts) == 0) {
        return(tags$p("No metadata available for the current selection."))
      }

      tags$div(
        class = "topo-meta-scroll",
        tags$div(
          class = "topo-meta-columns",
          style = paste0("grid-template-columns: repeat(", length(concepts), ", minmax(320px, 1fr));"),
          lapply(concepts, function(concept) {
            rows <- data[data$concept_label == concept, , drop = FALSE]
            tags$div(
              class = "topo-meta-column",
              tags$h3(class = "topo-meta-heading", paste("Variable:", concept)),
              lapply(seq_len(nrow(rows)), function(i) {
                tags$div(class = "topo-meta-entry", render_entry(rows[i, , drop = FALSE]))
              })
            )
          })
        )
      )
    }

    render_matrix_layout <- function(data, column_var, selected_var) {
      filtered <- filtered_data()
      row_labels <- order_values(data$concept_label, filtered$d4_concept_lab)
      column_labels <- order_values(data[[column_var]], filtered[[selected_var]])

      if (length(row_labels) == 0 || length(column_labels) == 0) {
        return(tags$p("No metadata available for the current selection."))
      }

      tags$div(
        class = "topo-meta-scroll",
        tags$table(
          class = "topo-meta-matrix",
          tags$thead(
            tags$tr(
              tags$th(""),
              lapply(column_labels, function(column_label) {
                rows <- data[data[[column_var]] == column_label, , drop = FALSE]
                if (identical(column_var, "source_label") && nrow(rows) > 0) {
                  tags$th(source_link(column_label, clean_text(rows$source_url)))
                } else {
                  tags$th(column_label)
                }
              })
            )
          ),
          tags$tbody(
            lapply(row_labels, function(row_label) {
              tags$tr(
                tags$td(class = "topo-meta-row-label", row_label),
                lapply(column_labels, function(column_label) {
                  rows <- data[
                    data$concept_label == row_label & data[[column_var]] == column_label,
                    ,
                    drop = FALSE
                  ]
                  tags$td(
                    if (nrow(rows) == 0) {
                      ""
                    } else {
                      lapply(seq_len(nrow(rows)), function(i) {
                        tags$div(class = "topo-meta-entry", render_entry(rows[i, , drop = FALSE]))
                      })
                    }
                  )
                })
              )
            })
          )
        )
      )
    }

    output$topoMeta <- renderUI({
      req(is_topo_bundle())
      data <- processed_topo_meta()
      if (is.null(data) || nrow(data) == 0) {
        return(tagList(topo_css, tags$p("No metadata available for the current selection.")))
      }

      layout <- meta_layout
      if (is.null(layout) || !nzchar(layout)) layout <- "topo_columns"

      tagList(
        topo_css,
        switch(
          layout,
          topo_source_matrix = render_matrix_layout(data, "source_label", "legend"),
          topo_country_matrix = render_matrix_layout(data, "country_label", "GEO_long"),
          render_column_layout(data)
        )
      )
    })

    output$metadataContainer <- renderUI({
      if (is_topo_bundle()) {
        uiOutput(session$ns("topoMeta"))
      } else {
        DTOutput(session$ns("metaTable"))
      }
    })

    # Reactive expression to filter metadata and compute column widths dynamically
    processed_meta <- reactive({
      req(!is_topo_bundle(), filtered_data(), meta)  # Ensure both datasets exist

      filtered_geo <- unique(filtered_data()$GEO)
      filtered_leg <- unique(filtered_data()$legend)

      # Filter meta by GEO and legend list
      if (isTRUE(substr(graph, 1, 4) == "ineq")) {
        filtered_meta <- meta %>% filter(GEO %in% filtered_geo, Legend %in% filtered_leg)
      } else {
        filtered_meta <- meta %>% filter(GEO %in% filtered_geo)
      }

      # Compute max length of characters per column (only for character/text columns)
      max_lengths <- sapply(filtered_meta, function(col) {
        if (is.character(col)) {
          max(nchar(col), na.rm = TRUE)
        } else {
          0  # Numeric columns do not need word-length adjustments
        }
      })

      # Define thresholds
      moderate_threshold <- 20
      extreme_threshold <- 100

      list(
        moderate_columns = names(max_lengths[max_lengths > moderate_threshold & max_lengths <= extreme_threshold]),
        extreme_columns = names(max_lengths[max_lengths > extreme_threshold]),
        nowrap_columns = names(max_lengths[max_lengths <= moderate_threshold]),
        filtered_meta = filtered_meta  # Return the filtered dataset
      )
    })

    output$metaTable <- renderDT({
      req(!is_topo_bundle())
      data <- processed_meta()  # Get precomputed data
      filtered_meta <- data$filtered_meta  # Extract updated dataset
      extreme_columns <- data$extreme_columns
      moderate_columns <- data$moderate_columns

      # Apply make_expandable only to extreme-length columns
      if (length(extreme_columns) > 0) {
        for (col in extreme_columns) {
          filtered_meta[[col]] <- sapply(filtered_meta[[col]], make_expandable)
        }
        for (col in moderate_columns) {
          filtered_meta[[col]] <- sapply(filtered_meta[[col]], make_expandable)
        }
      }

      column_defs <- list(
        list(width = "300px", targets = which(names(filtered_meta) %in% data$moderate_columns)),
        list(width = "400px", targets = which(names(filtered_meta) %in% data$extreme_columns))
      )

      datatable(filtered_meta,
                rownames = FALSE,
                escape = FALSE,
                extensions = c('FixedHeader', 'FixedColumns'),
                options = list(
                  autoWidth = TRUE,
                  scrollX = TRUE,
                  scrollY = "700px",
                  fixedHeader = TRUE,
                  fixedColumns = list(leftColumns = 1),
                  columnDefs = column_defs,
                  pageLength = 30
                )) %>%
        formatStyle(columns = data$moderate_columns, `white-space` = 'normal', `word-break` = 'break-word') %>%
        formatStyle(columns = data$extreme_columns, `white-space` = 'normal', `word-break` = 'break-word') %>%
        formatStyle(columns = data$nowrap_columns, `white-space` = 'nowrap')
    })
  })
}
