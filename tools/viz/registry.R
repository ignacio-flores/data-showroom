load_registry <- function(path, require_auth = TRUE) {
  if (!file.exists(path)) {
    stop(sprintf("Deployment registry not found: %s", path))
  }

  raw <- yaml::read_yaml(path)
  targets <- raw$targets
  if (is.null(targets) || !length(targets)) {
    stop(sprintf("Deployment registry has no targets: %s", path))
  }

  normalized <- lapply(seq_along(targets), function(idx) {
    entry <- targets[[idx]]
    required <- c("target_id", "graph", "app_name", "profile", "auth_script")
    missing <- required[!vapply(required, function(key) {
      value <- entry[[key]]
      !is.null(value) && nzchar(as.character(value))
    }, logical(1))]

    if (length(missing)) {
      stop(
        sprintf(
          "Target at index %s is missing required fields: %s",
          idx,
          paste(missing, collapse = ", ")
        )
      )
    }

    graph <- as.character(entry$graph)
    config_file <- sprintf("yaml/config_%s.yaml", graph)
    auth_script <- as.character(entry$auth_script)
    server <- as.character(entry$server %||% "shinyapps.io")

    list(
      target_id = as.character(entry$target_id),
      graph = graph,
      config_file = config_file,
      app_name = as.character(entry$app_name),
      profile = as.character(entry$profile),
      server = server,
      auth_script = auth_script,
      tags = as.character(entry$tags %||% character()),
      enabled = as.logical(entry$enabled %||% TRUE)
    )
  })

  target_ids <- vapply(normalized, `[[`, character(1), "target_id")
  duplicate_target_ids <- unique(target_ids[duplicated(target_ids)])
  if (length(duplicate_target_ids)) {
    stop(sprintf(
      "Duplicate target_id values in registry: %s",
      paste(duplicate_target_ids, collapse = ", ")
    ))
  }

  invisible(lapply(normalized, function(entry) {
    if (!file.exists(entry$config_file)) {
      stop(sprintf(
        "Config file not found for target '%s': %s",
        entry$target_id,
        entry$config_file
      ))
    }
    if (isTRUE(require_auth) && !file.exists(entry$auth_script)) {
      stop(sprintf(
        "Auth script not found for target '%s': %s",
        entry$target_id,
        entry$auth_script
      ))
    }
  }))

  normalized
}
validate_selectors <- function(targets, opts) {
  known_ids <- vapply(targets, `[[`, character(1), "target_id")
  known_profiles <- unique(vapply(targets, `[[`, character(1), "profile"))
  known_tags <- sort(unique(unlist(lapply(targets, `[[`, "tags"), use.names = FALSE)))

  unknown_ids <- setdiff(opts$target, known_ids)
  if (length(unknown_ids)) {
    usage_error(sprintf("Unknown target ID(s): %s", paste(unknown_ids, collapse = ", ")))
  }

  unknown_profiles <- setdiff(opts$profile, known_profiles)
  if (length(unknown_profiles)) {
    usage_error(sprintf("Unknown profile(s): %s", paste(unknown_profiles, collapse = ", ")))
  }

  unknown_tags <- setdiff(opts$tag, known_tags)
  if (length(unknown_tags)) {
    usage_error(sprintf("Unknown tag(s): %s", paste(unknown_tags, collapse = ", ")))
  }
}

selector_summary <- function(opts) {
  parts <- character()
  if (length(opts$target)) parts <- c(parts, sprintf("target=%s", paste(opts$target, collapse = ",")))
  if (length(opts$profile)) parts <- c(parts, sprintf("profile=%s", paste(opts$profile, collapse = ",")))
  if (length(opts$tag)) parts <- c(parts, sprintf("tag=%s", paste(opts$tag, collapse = ",")))
  if (isTRUE(opts$all)) parts <- c(parts, "all enabled targets")
  if (!length(parts)) "no selectors" else paste(parts, collapse = "; ")
}

select_targets <- function(targets, opts) {
  has_selector <- opts$all || length(opts$target) || length(opts$profile) || length(opts$tag)
  if (!has_selector) {
    usage_error("No deployment selector provided. Use --target, --profile, --tag, or --all.")
  }

  validate_selectors(targets, opts)

  selected <- targets

  if (opts$all || !length(opts$target)) {
    selected <- Filter(function(entry) isTRUE(entry$enabled), selected)
  }

  if (length(opts$target)) {
    selected <- Filter(function(entry) entry$target_id %in% opts$target, selected)
  }

  if (length(opts$profile)) {
    selected <- Filter(function(entry) entry$profile %in% opts$profile, selected)
  }

  if (length(opts$tag)) {
    selected <- Filter(function(entry) any(opts$tag %in% entry$tags), selected)
  }

  if (!length(selected)) {
    usage_error(sprintf("Selection did not match any deployment targets (%s).", selector_summary(opts)))
  }

  selected
}

print_selection <- function(selected, title = "Selected deployment targets:") {
  cat(title, "\n")
  for (entry in selected) {
    tags <- if (length(entry$tags)) paste(entry$tags, collapse = ",") else "-"
    cat(sprintf(
      "  - id=%s | graph=%s | app=%s | profile=%s | server=%s | tags=%s\n",
      entry$target_id, entry$graph, entry$app_name, entry$profile, entry$server, tags
    ))
  }
}

supports_ansi_output <- function() {
  isTRUE(isatty(stdout())) &&
    !identical(Sys.getenv("TERM", unset = ""), "dumb") &&
    !nzchar(Sys.getenv("NO_COLOR", unset = ""))
}

dim_text <- function(value, enabled = supports_ansi_output()) {
  if (!isTRUE(enabled)) {
    return(value)
  }
  sprintf("\033[90m%s\033[39m", value)
}

pad_right <- function(value, width) {
  sprintf(sprintf("%%-%ss", width), value)
}

print_target_table <- function(table, dim_columns = c("graph", "profile", "enabled")) {
  columns <- names(table)
  text <- lapply(table, as.character)
  names(text) <- columns
  widths <- vapply(columns, function(column) {
    max(nchar(c(column, text[[column]]), type = "width"), na.rm = TRUE)
  }, integer(1))
  ansi <- supports_ansi_output()

  format_cell <- function(value, column) {
    cell <- pad_right(value, widths[[column]])
    if (column %in% dim_columns) {
      cell <- dim_text(cell, enabled = ansi)
    }
    cell
  }

  header <- vapply(columns, function(column) format_cell(column, column), character(1))
  cat(paste(header, collapse = "  "), "\n", sep = "")

  for (row in seq_len(nrow(table))) {
    cells <- vapply(columns, function(column) {
      format_cell(text[[column]][[row]], column)
    }, character(1))
    cat(paste(cells, collapse = "  "), "\n", sep = "")
  }
}

list_deploy_targets <- function(registry = "yaml/deploy_targets.yaml",
                                target = NULL,
                                profile = NULL,
                                tag = NULL,
                                include_disabled = FALSE,
                                targets = NULL) {
  target <- normalize_selector_values(target, "target")
  profile <- normalize_selector_values(profile, "profile")
  tag <- normalize_selector_values(tag, "tag")

  if (is.null(targets)) {
    targets <- load_registry(registry, require_auth = FALSE)
  }

  if (length(target)) {
    targets <- Filter(function(entry) entry$target_id %in% target, targets)
  }

  if (length(profile)) {
    targets <- Filter(function(entry) entry$profile %in% profile, targets)
  }

  if (length(tag)) {
    targets <- Filter(function(entry) any(tag %in% entry$tags), targets)
  }

  if (!isTRUE(include_disabled)) {
    targets <- Filter(function(entry) isTRUE(entry$enabled), targets)
  }

  table <- data.frame(
    target_id = vapply(targets, `[[`, character(1), "target_id"),
    graph = vapply(targets, `[[`, character(1), "graph"),
    profile = vapply(targets, `[[`, character(1), "profile"),
    enabled = vapply(targets, `[[`, logical(1), "enabled"),
    tags = vapply(targets, function(entry) paste(entry$tags, collapse = ","), character(1)),
    stringsAsFactors = FALSE
  )

  if (!nrow(table)) {
    cat("No deployment targets match the requested filters.\n")
    return(invisible(table))
  }

  print_target_table(table)
  invisible(table)
}
