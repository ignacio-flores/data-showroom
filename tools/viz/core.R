suppressPackageStartupMessages({
  library(yaml)
})

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

conditionMessage.deploy_usage_error <- function(c) {
  c$message
}

conditionMessage.viz_cancelled <- function(c) {
  c$message
}

usage_error <- function(message) {
  stop(structure(list(message = message), class = c("deploy_usage_error", "error", "condition")))
}

cancel_viz <- function(message = "Cancelled. No preview or deployment was started.") {
  stop(structure(list(message = message), class = c("viz_cancelled", "error", "condition")))
}

normalize_rel_path <- function(path) {
  if (is.null(path) || !length(path)) return(character())
  path <- gsub("\\\\", "/", as.character(path))
  path <- sub("^\\./+", "", path)
  path[nzchar(path)]
}

is_option_token <- function(value) {
  startsWith(as.character(value), "--")
}

selector_error_name <- function(option) {
  if (startsWith(option, "--")) option else sprintf("`%s`", option)
}

normalize_selector_values <- function(value, option, allow_vector = TRUE) {
  if (is.null(value) || !length(value)) {
    return(character())
  }

  tokens <- as.character(value)
  if (any(is.na(tokens)) || any(!nzchar(trimws(tokens)))) {
    usage_error(sprintf("Missing value for %s", selector_error_name(option)))
  }
  tokens <- trimws(tokens)

  combined <- tokens[[1]]
  if (length(tokens) > 1L) {
    for (idx in 2:length(tokens)) {
      previous <- tokens[[idx - 1L]]
      current <- tokens[[idx]]
      has_boundary_comma <- grepl(",\\s*$", previous) || grepl("^\\s*,", current)
      if (!has_boundary_comma && !isTRUE(allow_vector)) {
        usage_error(sprintf(
          "Unexpected value for %s: %s. Separate multiple values with commas or repeat %s.",
          selector_error_name(option),
          current,
          option
        ))
      }
      separator <- if (has_boundary_comma) " " else ","
      combined <- paste(combined, current, sep = separator)
    }
  }

  combined <- trimws(combined)
  if (!nzchar(combined)) {
    usage_error(sprintf("Missing value for %s", selector_error_name(option)))
  }
  if (grepl("^,", combined) || grepl(",$", combined) || grepl(",\\s*,", combined)) {
    usage_error(sprintf(
      "Empty value in %s; remove leading, trailing, or repeated commas.",
      selector_error_name(option)
    ))
  }

  parts <- trimws(unlist(strsplit(combined, ",", fixed = TRUE)))
  if (any(!nzchar(parts))) {
    usage_error(sprintf(
      "Empty value in %s; remove leading, trailing, or repeated commas.",
      selector_error_name(option)
    ))
  }
  unique(parts)
}

parse_port <- function(value, option = "--preview-port") {
  if (is.null(value) || !nzchar(trimws(value))) {
    usage_error(sprintf("Missing value for %s", option))
  }
  port <- suppressWarnings(as.integer(value))
  if (is.na(port) || port < 1L || port > 65535L || !identical(as.character(port), trimws(value))) {
    usage_error(sprintf("%s must be an integer from 1 to 65535.", option))
  }
  port
}

require_namespace <- function(package, purpose) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required to %s.", package, purpose))
  }
}

read_stdin_line <- function() {
  con <- file("stdin")
  on.exit(close(con), add = TRUE)
  readLines(con, n = 1, warn = FALSE)
}

format_bytes <- function(bytes) {
  if (is.na(bytes)) return("unknown")
  units <- c("B", "KB", "MB", "GB")
  value <- as.numeric(bytes)
  idx <- 1L
  while (value >= 1024 && idx < length(units)) {
    value <- value / 1024
    idx <- idx + 1L
  }
  if (idx == 1L) {
    sprintf("%d %s", round(value), units[[idx]])
  } else {
    sprintf("%.1f %s", value, units[[idx]])
  }
}

format_time <- function(time) {
  if (is.null(time) || length(time) == 0 || is.na(time)) {
    return("missing")
  }
  format(as.POSIXct(time, origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S %Z")
}
