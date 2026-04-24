suppressPackageStartupMessages({
  library(rsconnect)
  library(yaml)
})

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

conditionMessage.deploy_usage_error <- function(c) {
  c$message
}

usage_error <- function(message) {
  stop(structure(list(message = message), class = c("deploy_usage_error", "error", "condition")))
}

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript deploy-app.R --target <id>[,<id>...] [--dry-run]",
      "  Rscript deploy-app.R --profile <profile>[,<profile>...] [--dry-run]",
      "  Rscript deploy-app.R --tag <tag>[,<tag>...] [--dry-run]",
      "  Rscript deploy-app.R --all [--dry-run]",
      "",
      "Options:",
      "  --target <id>       Deploy one or more target IDs (comma-separated or repeated).",
      "  --profile <name>    Filter deployment targets by profile/account.",
      "  --tag <tag>         Filter deployment targets by one or more tags.",
      "  --all               Select all enabled targets (can combine with --profile/--tag).",
      "  --registry <path>   Path to deployment registry YAML.",
      "  --dry-run           Print selected targets without deploying.",
      "  --help              Show this message.",
      sep = "\n"
    )
  )
}

split_csv <- function(value) {
  parts <- trimws(unlist(strsplit(value, ",")))
  parts[nzchar(parts)]
}

parse_args <- function(args) {
  opts <- list(
    target = character(),
    profile = character(),
    tag = character(),
    all = FALSE,
    dry_run = FALSE,
    registry = "yaml/deploy_targets.yaml",
    help = FALSE
  )

  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]

    if (arg == "--help") {
      opts$help <- TRUE
      i <- i + 1
      next
    } else if (arg == "--all") {
      opts$all <- TRUE
      i <- i + 1
      next
    } else if (arg == "--dry-run") {
      opts$dry_run <- TRUE
      i <- i + 1
      next
    }

    consume_value <- function(name) {
      if (startsWith(arg, paste0(name, "="))) {
        return(sub(paste0("^", name, "="), "", arg))
      }
      if (arg == name) {
        if (i == length(args)) {
          usage_error(sprintf("Missing value for %s", name))
        }
        return(args[[i + 1]])
      }
      NULL
    }

    target_value <- consume_value("--target")
    if (!is.null(target_value)) {
      opts$target <- c(opts$target, split_csv(target_value))
      i <- i + ifelse(arg == "--target", 2, 1)
      next
    }

    profile_value <- consume_value("--profile")
    if (!is.null(profile_value)) {
      opts$profile <- c(opts$profile, split_csv(profile_value))
      i <- i + ifelse(arg == "--profile", 2, 1)
      next
    }

    tag_value <- consume_value("--tag")
    if (!is.null(tag_value)) {
      opts$tag <- c(opts$tag, split_csv(tag_value))
      i <- i + ifelse(arg == "--tag", 2, 1)
      next
    }

    registry_value <- consume_value("--registry")
    if (!is.null(registry_value)) {
      opts$registry <- registry_value
      i <- i + ifelse(arg == "--registry", 2, 1)
      next
    }

    usage_error(sprintf("Unknown argument: %s", arg))
  }

  opts$target <- unique(opts$target)
  opts$profile <- unique(opts$profile)
  opts$tag <- unique(opts$tag)
  opts
}

load_registry <- function(path) {
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
    if (!file.exists(entry$auth_script)) {
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
    stop(sprintf("Unknown target ID(s): %s", paste(unknown_ids, collapse = ", ")))
  }

  unknown_profiles <- setdiff(opts$profile, known_profiles)
  if (length(unknown_profiles)) {
    stop(sprintf("Unknown profile(s): %s", paste(unknown_profiles, collapse = ", ")))
  }

  unknown_tags <- setdiff(opts$tag, known_tags)
  if (length(unknown_tags)) {
    stop(sprintf("Unknown tag(s): %s", paste(unknown_tags, collapse = ", ")))
  }
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
    stop("Selection did not match any deployment targets.")
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

get_auth <- function(auth_script) {
  auth_env <- new.env(parent = globalenv())
  sys.source(auth_script, envir = auth_env)

  required <- c("shiny_token", "shiny_secret")
  missing <- required[!vapply(required, exists, logical(1), envir = auth_env, inherits = FALSE)]
  if (length(missing)) {
    stop(sprintf(
      "Auth script '%s' is missing: %s",
      auth_script,
      paste(missing, collapse = ", ")
    ))
  }

  list(
    shiny_account = if (exists("shiny_account", envir = auth_env, inherits = FALSE)) {
      get("shiny_account", envir = auth_env)
    } else {
      NULL
    },
    shiny_token = get("shiny_token", envir = auth_env),
    shiny_secret = get("shiny_secret", envir = auth_env)
  )
}

deploy_target <- function(entry, app_dir = ".") {
  credentials <- get_auth(entry$auth_script)
  if (!is.null(credentials$shiny_account)) {
    credentials_account <- as.character(credentials$shiny_account)
    if (nzchar(credentials_account) && !identical(credentials_account, entry$profile)) {
      stop(sprintf(
        "Auth script account '%s' does not match profile '%s' for target '%s'.",
        credentials_account, entry$profile, entry$target_id
      ))
    }
  }

  rsconnect::setAccountInfo(
    name = entry$profile,
    token = credentials$shiny_token,
    secret = credentials$shiny_secret
  )
  rsconnect::deployApp(
    appDir = app_dir,
    appName = entry$app_name,
    account = entry$profile,
    server = entry$server,
    forceUpdate = TRUE
  )
}

safe_restore_active_graph <- function(path, existed, original_lines) {
  if (existed) {
    writeLines(original_lines, path)
  } else if (file.exists(path)) {
    unlink(path)
  }
}

list_deploy_targets <- function(registry = "yaml/deploy_targets.yaml",
                                profile = NULL,
                                tag = NULL,
                                include_disabled = FALSE) {
  targets <- load_registry(registry)

  if (!is.null(profile)) {
    targets <- Filter(function(entry) entry$profile %in% profile, targets)
  }

  if (!is.null(tag)) {
    targets <- Filter(function(entry) any(tag %in% entry$tags), targets)
  }

  if (!isTRUE(include_disabled)) {
    targets <- Filter(function(entry) isTRUE(entry$enabled), targets)
  }

  table <- data.frame(
    target_id = vapply(targets, `[[`, character(1), "target_id"),
    graph = vapply(targets, `[[`, character(1), "graph"),
    app_name = vapply(targets, `[[`, character(1), "app_name"),
    profile = vapply(targets, `[[`, character(1), "profile"),
    server = vapply(targets, `[[`, character(1), "server"),
    enabled = vapply(targets, `[[`, logical(1), "enabled"),
    tags = vapply(targets, function(entry) paste(entry$tags, collapse = ","), character(1)),
    stringsAsFactors = FALSE
  )

  if (!nrow(table)) {
    cat("No deployment targets match the requested filters.\n")
    return(invisible(table))
  }

  print(table, row.names = FALSE)
  invisible(table)
}

main <- function(args = commandArgs(trailingOnly = TRUE), quiet = FALSE) {
  opts <- parse_args(args)
  if (isTRUE(opts$help)) {
    usage()
    return(invisible(list(status = 0L, selected = list(), results = list())))
  }

  targets <- load_registry(opts$registry)
  has_selector <- opts$all || length(opts$target) || length(opts$profile) || length(opts$tag)

  if (!has_selector && interactive()) {
    cat("No selector provided. Choose targets by name (target_id).\n\n")
    list_deploy_targets(registry = opts$registry)
    cat("\nExample:\n  deploy_by_target(\"inhe_multi\")\n")
    cat("  deploy_by_target(c(\"inhe_single\", \"inhe_multi\"), dry_run = TRUE)\n")
    cat("  main(c(\"--profile\", \"gregcull\", \"--dry-run\"))\n")
    return(invisible(list(status = 0L, selected = list(), results = list())))
  }

  selected <- select_targets(targets, opts)

  if (!isTRUE(quiet)) {
    print_selection(selected)
  }

  if (isTRUE(opts$dry_run)) {
    if (!isTRUE(quiet)) {
      cat("\nDry run complete. No deployment executed.\n")
    }
    return(invisible(list(status = 0L, selected = selected, results = list())))
  }

  active_graph_path <- file.path(".", "active_graph.txt")
  active_graph_existed <- file.exists(active_graph_path)
  active_graph_original <- if (active_graph_existed) {
    readLines(active_graph_path, warn = FALSE)
  } else {
    character()
  }

  on.exit(
    safe_restore_active_graph(active_graph_path, active_graph_existed, active_graph_original),
    add = TRUE
  )

  results <- vector("list", length(selected))

  for (i in seq_along(selected)) {
    entry <- selected[[i]]
    cat(sprintf("\n[%s/%s] Deploying '%s' (%s)...\n",
                i, length(selected), entry$target_id, entry$app_name))
    start_time <- Sys.time()

    writeLines(entry$graph, active_graph_path)

    result <- tryCatch({
      deploy_target(entry)
      list(
        status = "success",
        elapsed = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      )
    }, error = function(e) {
      list(
        status = "failed",
        error = conditionMessage(e),
        elapsed = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      )
    })

    results[[i]] <- c(list(entry = entry), result)
    if (identical(result$status, "success")) {
      cat(sprintf("  SUCCESS in %.1fs\n", result$elapsed))
    } else {
      cat(sprintf("  FAILED in %.1fs\n  %s\n", result$elapsed, result$error))
    }
  }

  success_count <- sum(vapply(results, function(x) identical(x$status, "success"), logical(1)))
  failed <- Filter(function(x) identical(x$status, "failed"), results)

  cat("\nDeployment summary:\n")
  cat(sprintf("  Successful: %s\n", success_count))
  cat(sprintf("  Failed: %s\n", length(failed)))

  if (length(failed)) {
    cat("  Failed targets:\n")
    for (item in failed) {
      cat(sprintf("    - %s: %s\n", item$entry$target_id, item$error))
    }
    return(invisible(list(status = 1L, selected = selected, results = results)))
  }

  invisible(list(status = 0L, selected = selected, results = results))
}

deploy_by_target <- function(target_id,
                             dry_run = FALSE,
                             registry = "yaml/deploy_targets.yaml") {
  if (!length(target_id)) {
    usage_error("`target_id` cannot be empty.")
  }

  args <- c("--target", paste(target_id, collapse = ","))
  if (isTRUE(dry_run)) {
    args <- c(args, "--dry-run")
  }
  args <- c(args, "--registry", registry)
  main(args = args)
}

cli_main <- function() {
  status <- 0L

  tryCatch({
    result <- main()
    status <<- as.integer(result$status %||% 0L)
  }, deploy_usage_error = function(e) {
    cat(sprintf("Error: %s\n\n", conditionMessage(e)))
    usage()
    status <<- 1L
  }, error = function(e) {
    cat(sprintf("Error: %s\n", conditionMessage(e)))
    status <<- 1L
  })

  quit(status = status, save = "no")
}

if (sys.nframe() == 0) {
  cli_main()
}
