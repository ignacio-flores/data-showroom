preview_browser_host <- function(host) {
  if (host %in% c("0.0.0.0", "::")) "127.0.0.1" else host
}

preview_url <- function(host, port) {
  sprintf("http://%s:%s", preview_browser_host(host), port)
}

port_available <- function(port, host = "127.0.0.1") {
  if (port_accepting_connections(host, port)) {
    return(FALSE)
  }
  con <- tryCatch(serverSocket(port), error = function(e) NULL)
  if (is.null(con)) {
    return(FALSE)
  }
  close(con)
  TRUE
}

port_accepting_connections <- function(host, port) {
  con <- tryCatch(
    suppressWarnings(
      socketConnection(
        host = preview_browser_host(host),
        port = port,
        open = "a+b",
        blocking = TRUE,
        timeout = 0.25
      )
    ),
    error = function(e) NULL
  )
  if (is.null(con)) {
    return(FALSE)
  }
  close(con)
  TRUE
}

random_preview_ports <- function(count, host, used = integer()) {
  candidates <- sample(20000:49151)
  ports <- integer()
  for (candidate in candidates) {
    if (candidate %in% used || candidate %in% ports) next
    if (port_available(candidate, host)) {
      ports <- c(ports, candidate)
      if (length(ports) == count) return(ports)
    }
  }
  stop(sprintf("Could not find %s available preview port(s).", count))
}

sequential_preview_ports <- function(count, base_port, host) {
  ports <- integer()
  candidate <- base_port
  while (length(ports) < count && candidate <= 65535L) {
    if (port_available(candidate, host)) {
      ports <- c(ports, candidate)
    }
    candidate <- candidate + 1L
  }
  if (length(ports) < count) {
    stop(sprintf(
      "Could not find %s available preview port(s) at or above %s.",
      count,
      base_port
    ))
  }
  ports
}

choose_preview_ports <- function(count, host, base_port = NULL) {
  if (is.null(base_port)) {
    return(random_preview_ports(count, host))
  }
  if (count == 1L) {
    if (!port_available(base_port, host)) {
      stop(sprintf("Preview port %s is already in use.", base_port))
    }
    return(base_port)
  }
  sequential_preview_ports(count, base_port, host)
}

preview_child_expression <- function(app_dir, host, port) {
  sprintf(
    paste(
      "shiny::runApp(",
      "appDir = %s, ",
      "port = %sL, ",
      "host = %s, ",
      "launch.browser = FALSE, ",
      "quiet = FALSE, ",
      "display.mode = 'normal')"
    ),
    shQuote(normalizePath(app_dir, mustWork = TRUE)),
    port,
    shQuote(host)
  )
}

collect_process_lines <- function(process) {
  c(
    tryCatch(process$read_output_lines(), error = function(e) character()),
    tryCatch(process$read_error_lines(), error = function(e) character())
  )
}

format_preview_logs <- function(lines, n = 20L) {
  lines <- lines[nzchar(lines)]
  if (!length(lines)) {
    return("    <no startup output captured>")
  }
  paste(sprintf("    %s", tail(lines, n)), collapse = "\n")
}

start_preview_process <- function(entry, bundle, host, port) {
  rscript <- file.path(R.home("bin"), "Rscript")
  processx::process$new(
    command = rscript,
    args = c("-e", preview_child_expression(bundle$app_dir, host, port)),
    stdout = "|",
    stderr = "|",
    wd = bundle$app_dir,
    env = c(DATA_SHOWROOM_GRAPH = entry$graph),
    cleanup = FALSE
  )
}

wait_for_preview_start <- function(item, timeout = 120) {
  deadline <- Sys.time() + timeout
  logs <- character()

  repeat {
    logs <- c(logs, collect_process_lines(item$process))

    if (port_accepting_connections(item$host, item$port)) {
      return(invisible(TRUE))
    }

    if (!item$process$is_alive()) {
      status <- item$process$get_exit_status() %||% "unknown"
      stop(sprintf(
        "Preview for '%s' exited before it started (status %s):\n%s",
        item$entry$target_id,
        status,
        format_preview_logs(logs)
      ))
    }

    if (Sys.time() > deadline) {
      stop(sprintf(
        "Timed out waiting for preview '%s' to listen on %s:\n%s",
        item$entry$target_id,
        item$url,
        format_preview_logs(logs)
      ))
    }

    Sys.sleep(0.25)
  }
}

stop_preview_processes <- function(items) {
  for (item in items) {
    process <- item$process
    if (!is.null(process) && process$is_alive()) {
      try(process$interrupt(), silent = TRUE)
    }
  }

  Sys.sleep(0.5)

  for (item in items) {
    process <- item$process
    if (!is.null(process) && process$is_alive()) {
      try(process$kill_tree(), silent = TRUE)
      try(process$kill(), silent = TRUE)
    }
  }
}

drain_preview_output <- function(items) {
  invisible(lapply(items, function(item) collect_process_lines(item$process)))
}

print_preview_urls <- function(items) {
  cat("\nPreview apps:\n")
  for (item in items) {
    cat(sprintf("  - %s: %s\n", item$entry$target_id, item$url))
  }
}

open_preview_urls <- function(items) {
  for (item in items) {
    utils::browseURL(item$url)
    Sys.sleep(0.1)
  }
}

confirm_large_preview <- function(selected, opts, threshold = 5L) {
  count <- length(selected)
  if (count <= threshold || isTRUE(opts$yes)) {
    return(invisible(TRUE))
  }

  if (!isatty(stdin())) {
    usage_error(sprintf(
      "Preview matched %s targets. Re-run with yes to start them all, or narrow the selector.",
      count
    ))
  }

  prompt <- if (isTRUE(opts$launch_browser)) {
    sprintf("Start %s preview apps and open browser tabs?", count)
  } else {
    sprintf("Start %s preview apps?", count)
  }

  choice <- choose_menu_value(
    prompt,
    c("Start previews", "Cancel"),
    values = list(TRUE, FALSE),
    cancel_value = FALSE
  )
  if (!isTRUE(choice)) {
    cat("Preview cancelled. No preview apps were started.\n")
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

wait_for_preview_processes <- function(items) {
  completed <- rep(FALSE, length(items))
  statuses <- rep(NA_integer_, length(items))
  interrupted <- FALSE

  tryCatch({
    repeat {
      drain_preview_output(items)
      alive <- vapply(items, function(item) item$process$is_alive(), logical(1))
      newly_completed <- which(!alive & !completed)
      if (length(newly_completed)) {
        for (idx in newly_completed) {
          status <- items[[idx]]$process$get_exit_status()
          statuses[[idx]] <- status %||% NA_integer_
          status_label <- status %||% "unknown"
          cat(sprintf("  Preview stopped: %s (status %s)\n", items[[idx]]$entry$target_id, status_label))
        }
        completed[newly_completed] <- TRUE
      }
      if (!any(alive)) break
      Sys.sleep(1)
    }
  }, interrupt = function(e) {
    interrupted <<- TRUE
    cat("\nStopping preview apps...\n")
  })

  invisible(list(
    mode = if (interrupted) "interrupted" else "completed",
    statuses = statuses
  ))
}

preview_targets <- function(selected, data_sources, opts, quiet = FALSE) {
  if (!isTRUE(confirm_large_preview(selected, opts))) {
    return(invisible(list(
      status = 0L,
      previews = list(),
      cancelled = TRUE
    )))
  }

  require_namespace("processx", "run local previews")
  require_namespace("shiny", "run local previews")
  prepare_deployment_data(selected, data_sources, opts, quiet = quiet, operation = "preview")

  ports <- choose_preview_ports(length(selected), opts$preview_host, opts$preview_port)
  items <- list()
  temp_dirs <- character()

  on.exit({
    stop_preview_processes(items)
    unlink(temp_dirs, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  for (i in seq_along(selected)) {
    entry <- selected[[i]]
    if (!isTRUE(quiet)) {
      cat(sprintf("\n[%s/%s] Previewing '%s' (%s)...\n",
                  i, length(selected), entry$target_id, entry$app_name))
    }

    bundle <- create_temp_bundle(entry, data_sources)
    temp_dirs <- c(temp_dirs, bundle$app_dir)
    if (!isTRUE(quiet)) {
      print_created_bundle(entry, bundle)
    }

    process <- start_preview_process(entry, bundle, opts$preview_host, ports[[i]])
    items[[i]] <- list(
      entry = entry,
      bundle = bundle,
      process = process,
      host = opts$preview_host,
      port = ports[[i]],
      url = preview_url(opts$preview_host, ports[[i]])
    )
  }

  for (item in items) {
    wait_for_preview_start(item)
  }

  if (!isTRUE(quiet)) {
    print_preview_urls(items)
    cat("\nPress Ctrl+C to stop all preview apps. Nothing will be deployed.\n")
  }

  if (isTRUE(opts$launch_browser)) {
    open_preview_urls(items)
  }

  wait_result <- wait_for_preview_processes(items)
  failed <- !is.na(wait_result$statuses) & wait_result$statuses != 0L
  status <- if (identical(wait_result$mode, "interrupted")) {
    0L
  } else if (any(failed)) {
    1L
  } else {
    0L
  }

  invisible(list(
    status = status,
    previews = items
  ))
}
