suppressPackageStartupMessages({
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

normalize_rel_path <- function(path) {
  if (is.null(path) || !length(path)) return(character())
  path <- gsub("\\\\", "/", as.character(path))
  path <- sub("^\\./+", "", path)
  path[nzchar(path)]
}

split_csv <- function(value) {
  parts <- trimws(unlist(strsplit(value, ",")))
  parts[nzchar(parts)]
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
  format(time, "%Y-%m-%d %H:%M:%S %Z")
}

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript deploy-app.R --list [--target <id>[,<id>...]] [--profile <profile>] [--tag <tag>]",
      "  Rscript deploy-app.R --target <id>[,<id>...] [--dry-run]",
      "  Rscript deploy-app.R --target <id>[,<id>...] --preview [--preview-port <port>] [--no-browser]",
      "  Rscript deploy-app.R --profile <profile>[,<profile>...] [--dry-run]",
      "  Rscript deploy-app.R --tag <tag>[,<tag>...] [--dry-run]",
      "  Rscript deploy-app.R --all [--dry-run]",
      "",
      "Options:",
      "  --list                 Print deployment targets and exit without deploying.",
      "  --target <id>          Deploy one or more target IDs (comma-separated or repeated).",
      "  --profile <name>       Filter deployment targets by profile/account.",
      "  --tag <tag>            Filter deployment targets by one or more tags.",
      "  --all                  Select all enabled targets (can combine with --profile/--tag).",
      "  --include-disabled     Include disabled targets when used with --list.",
      "  --registry <path>      Path to deployment registry YAML.",
      "  --data-sources <path>  Path to data-source manifest YAML.",
      "  --source-root <path>   Override the configured data source root for this run.",
      "  --refresh-data         Refresh stale cached files/artifacts without prompting.",
      "  --use-cache            Use stale cached files/artifacts without prompting.",
      "  --dry-run              Print selected targets, data actions, and bundle contents.",
      "  --preview              Build deployment bundles and run them locally instead of deploying.",
      "  --preview-host <host>   Host for local preview (default: 127.0.0.1).",
      "  --preview-port <port>   Port for one preview; bulk previews use ports at/above it.",
      "  --no-browser           Do not open preview URLs in browser tabs.",
      "  --yes                  Skip the confirmation prompt for large bulk previews.",
      "  --help                 Show this message.",
      "",
      "Examples:",
      "  Rscript deploy-app.R --list",
      "  Rscript deploy-app.R --list --profile gregcull",
      "  Rscript deploy-app.R --list --tag eigt",
      sep = "\n"
    )
  )
}

parse_args <- function(args) {
  opts <- list(
    target = character(),
    profile = character(),
    tag = character(),
    list = FALSE,
    all = FALSE,
    include_disabled = FALSE,
    dry_run = FALSE,
    preview = FALSE,
    preview_host = "127.0.0.1",
    preview_port = NULL,
    launch_browser = TRUE,
    yes = FALSE,
    registry = "yaml/deploy_targets.yaml",
    data_sources = "yaml/deploy_data_sources.yaml",
    source_root = NULL,
    data_sources_supplied = FALSE,
    source_root_supplied = FALSE,
    refresh_data = FALSE,
    use_cache = FALSE,
    help = FALSE
  )

  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]

    if (arg == "--help") {
      opts$help <- TRUE
      i <- i + 1
      next
    } else if (arg == "--list") {
      opts$list <- TRUE
      i <- i + 1
      next
    } else if (arg == "--all") {
      opts$all <- TRUE
      i <- i + 1
      next
    } else if (arg == "--include-disabled") {
      opts$include_disabled <- TRUE
      i <- i + 1
      next
    } else if (arg == "--dry-run") {
      opts$dry_run <- TRUE
      i <- i + 1
      next
    } else if (arg == "--preview") {
      opts$preview <- TRUE
      i <- i + 1
      next
    } else if (arg == "--no-browser") {
      opts$launch_browser <- FALSE
      i <- i + 1
      next
    } else if (arg == "--yes") {
      opts$yes <- TRUE
      i <- i + 1
      next
    } else if (arg == "--refresh-data") {
      opts$refresh_data <- TRUE
      i <- i + 1
      next
    } else if (arg == "--use-cache") {
      opts$use_cache <- TRUE
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

    data_sources_value <- consume_value("--data-sources")
    if (!is.null(data_sources_value)) {
      opts$data_sources <- data_sources_value
      opts$data_sources_supplied <- TRUE
      i <- i + ifelse(arg == "--data-sources", 2, 1)
      next
    }

    source_root_value <- consume_value("--source-root")
    if (!is.null(source_root_value)) {
      opts$source_root <- source_root_value
      opts$source_root_supplied <- TRUE
      i <- i + ifelse(arg == "--source-root", 2, 1)
      next
    }

    preview_host_value <- consume_value("--preview-host")
    if (!is.null(preview_host_value)) {
      if (!nzchar(trimws(preview_host_value))) {
        usage_error("Missing value for --preview-host")
      }
      opts$preview_host <- trimws(preview_host_value)
      i <- i + ifelse(arg == "--preview-host", 2, 1)
      next
    }

    preview_port_value <- consume_value("--preview-port")
    if (!is.null(preview_port_value)) {
      opts$preview_port <- parse_port(preview_port_value)
      i <- i + ifelse(arg == "--preview-port", 2, 1)
      next
    }

    usage_error(sprintf("Unknown argument: %s", arg))
  }

  if (isTRUE(opts$dry_run) && isTRUE(opts$preview)) {
    usage_error("--dry-run and --preview cannot be used together.")
  }

  preview_option_used <- !identical(opts$preview_host, "127.0.0.1") ||
    !is.null(opts$preview_port) ||
    !isTRUE(opts$launch_browser) ||
    isTRUE(opts$yes)

  if (isTRUE(opts$include_disabled) && !isTRUE(opts$list)) {
    usage_error("--include-disabled requires --list.")
  }

  if (isTRUE(opts$list)) {
    list_conflicts <- character()
    if (isTRUE(opts$dry_run)) list_conflicts <- c(list_conflicts, "--dry-run")
    if (isTRUE(opts$preview)) list_conflicts <- c(list_conflicts, "--preview")
    if (!identical(opts$preview_host, "127.0.0.1")) list_conflicts <- c(list_conflicts, "--preview-host")
    if (!is.null(opts$preview_port)) list_conflicts <- c(list_conflicts, "--preview-port")
    if (!isTRUE(opts$launch_browser)) list_conflicts <- c(list_conflicts, "--no-browser")
    if (isTRUE(opts$yes)) list_conflicts <- c(list_conflicts, "--yes")
    if (isTRUE(opts$refresh_data)) list_conflicts <- c(list_conflicts, "--refresh-data")
    if (isTRUE(opts$use_cache)) list_conflicts <- c(list_conflicts, "--use-cache")
    if (isTRUE(opts$data_sources_supplied)) list_conflicts <- c(list_conflicts, "--data-sources")
    if (isTRUE(opts$source_root_supplied)) list_conflicts <- c(list_conflicts, "--source-root")
    if (length(list_conflicts)) {
      usage_error(sprintf("--list cannot be used with: %s", paste(list_conflicts, collapse = ", ")))
    }
  } else if (!isTRUE(opts$preview) && isTRUE(preview_option_used)) {
    usage_error("--preview-host, --preview-port, --no-browser, and --yes require --preview.")
  }

  if (isTRUE(opts$refresh_data) && isTRUE(opts$use_cache)) {
    usage_error("--refresh-data and --use-cache cannot be used together.")
  }

  opts$target <- unique(opts$target)
  opts$profile <- unique(opts$profile)
  opts$tag <- unique(opts$tag)
  opts
}

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

load_data_sources <- function(path, source_root_override = NULL) {
  if (!file.exists(path)) {
    stop(sprintf("Data source manifest not found: %s", path))
  }

  raw <- yaml::read_yaml(path)
  root_defs <- raw$source_roots %||% list()
  if (!length(root_defs)) {
    stop(sprintf("Data source manifest has no source_roots: %s", path))
  }

  roots <- lapply(names(root_defs), function(name) {
    entry <- root_defs[[name]]
    env_name <- as.character(entry$env %||% "")
    env_value <- if (nzchar(env_name)) Sys.getenv(env_name, unset = "") else ""
    root <- source_root_override %||% if (nzchar(env_value)) env_value else entry$default
    if (is.null(root) || !nzchar(as.character(root))) {
      stop(sprintf("No source root configured for '%s'.", name))
    }
    normalizePath(path.expand(as.character(root)), mustWork = FALSE)
  })
  names(roots) <- names(root_defs)

  files_raw <- raw$files %||% list()
  files <- lapply(names(files_raw), function(dest) {
    entry <- files_raw[[dest]]
    root_name <- as.character(entry$root %||% entry$source_root %||% "gcwealth")
    if (!root_name %in% names(roots)) {
      stop(sprintf("Unknown source root '%s' for data file '%s'.", root_name, dest))
    }
    source_rel <- normalize_rel_path(entry$path %||% entry$source)
    if (!length(source_rel)) {
      stop(sprintf("Data source entry '%s' is missing path/source.", dest))
    }
    list(
      path = normalize_rel_path(dest),
      root = root_name,
      source_rel = source_rel[[1]],
      source = file.path(roots[[root_name]], source_rel[[1]])
    )
  })
  names(files) <- normalize_rel_path(names(files_raw))

  recipes_raw <- raw$recipes %||% list()
  recipes <- lapply(names(recipes_raw), function(name) {
    entry <- recipes_raw[[name]]
    script <- normalize_rel_path(entry$script)
    inputs <- normalize_rel_path(unlist(entry$inputs %||% character(), use.names = FALSE))
    outputs <- normalize_rel_path(unlist(entry$outputs %||% character(), use.names = FALSE))
    if (!length(script) || !length(inputs) || !length(outputs)) {
      stop(sprintf("Recipe '%s' must define script, inputs, and outputs.", name))
    }
    list(name = name, script = script[[1]], inputs = inputs, outputs = outputs)
  })
  names(recipes) <- names(recipes_raw)

  runtime_raw <- raw$runtime_dependencies %||% list()
  runtime_dependencies <- lapply(runtime_raw, function(paths) {
    normalize_rel_path(unlist(paths, use.names = FALSE))
  })
  names(runtime_dependencies) <- normalize_rel_path(names(runtime_raw))

  list(
    path = path,
    roots = roots,
    files = files,
    recipes = recipes,
    runtime_dependencies = runtime_dependencies
  )
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

read_target_config <- function(entry) {
  yaml::read_yaml(entry$config_file)
}

value_transform_bundle_file <- function(config) {
  if (is.null(config$value_transform)) {
    return(NULL)
  }
  config$value_transform$bundle.file %||% NULL
}

runtime_dependencies_for_wrangler <- function(wrangler, data_sources) {
  if (is.null(wrangler) || !nzchar(wrangler)) {
    return(character())
  }
  data_sources$runtime_dependencies[[normalize_rel_path(wrangler)]] %||% character()
}

target_required_data_paths <- function(entry, data_sources) {
  config <- read_target_config(entry)
  wrangler <- config$data.wrangler %||% NULL
  normalize_rel_path(unique(c(
    config$data.file %||% NULL,
    config$meta.file %||% NULL,
    value_transform_bundle_file(config),
    runtime_dependencies_for_wrangler(wrangler, data_sources)
  )))
}

module_runtime_files <- function() {
  normalize_rel_path(list.files("modules", pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
}

optional_runtime_files <- function() {
  if (!dir.exists("www")) {
    return(character())
  }
  normalize_rel_path(list.files("www", recursive = TRUE, full.names = TRUE, all.files = FALSE))
}

target_bundle_files <- function(entry, data_sources) {
  config <- read_target_config(entry)
  wrangler <- config$data.wrangler %||% NULL
  normalize_rel_path(unique(c(
    "app.R",
    module_runtime_files(),
    optional_runtime_files(),
    entry$config_file,
    config$data.file %||% NULL,
    config$meta.file %||% NULL,
    value_transform_bundle_file(config),
    wrangler,
    runtime_dependencies_for_wrangler(wrangler, data_sources)
  )))
}

recipe_for_output <- function(path, data_sources) {
  path <- normalize_rel_path(path)
  for (recipe in data_sources$recipes) {
    if (path %in% recipe$outputs) {
      return(recipe)
    }
  }
  NULL
}

collect_preparation_requirements <- function(selected, data_sources) {
  required_data <- unique(unlist(
    lapply(selected, target_required_data_paths, data_sources = data_sources),
    use.names = FALSE
  ))

  queue <- required_data
  seen_paths <- character()
  direct_files <- character()
  recipe_names <- character()

  while (length(queue)) {
    path <- queue[[1]]
    queue <- queue[-1]
    if (path %in% seen_paths) next
    seen_paths <- c(seen_paths, path)

    recipe <- recipe_for_output(path, data_sources)
    if (!is.null(recipe)) {
      recipe_names <- unique(c(recipe_names, recipe$name))
      queue <- c(queue, recipe$inputs)
    } else {
      direct_files <- unique(c(direct_files, path))
    }
  }

  list(
    required_data = required_data,
    direct_files = direct_files,
    recipes = data_sources$recipes[recipe_names]
  )
}

file_mtime <- function(path) {
  if (!file.exists(path)) {
    return(as.POSIXct(NA))
  }
  file.info(path)$mtime
}

newer_than <- function(left, right) {
  !is.na(left) && !is.na(right) && left > right
}

analyze_direct_file <- function(path, data_sources) {
  mapping <- data_sources$files[[path]]
  exists_local <- file.exists(path)
  local_mtime <- file_mtime(path)

  if (is.null(mapping)) {
    return(list(
      path = path,
      source = NA_character_,
      local_mtime = local_mtime,
      source_mtime = as.POSIXct(NA),
      status = if (exists_local) "local-only" else "missing",
      action = if (exists_local) "use local" else "missing source mapping",
      error = !exists_local
    ))
  }

  source_exists <- file.exists(mapping$source)
  source_mtime <- file_mtime(mapping$source)
  if (!source_exists) {
    return(list(
      path = path,
      source = mapping$source,
      local_mtime = local_mtime,
      source_mtime = source_mtime,
      status = "missing-source",
      action = "cannot prepare",
      error = TRUE
    ))
  }

  if (!exists_local) {
    return(list(
      path = path,
      source = mapping$source,
      local_mtime = local_mtime,
      source_mtime = source_mtime,
      status = "missing-cache",
      action = "copy source",
      error = FALSE
    ))
  }

  if (newer_than(source_mtime, local_mtime)) {
    return(list(
      path = path,
      source = mapping$source,
      local_mtime = local_mtime,
      source_mtime = source_mtime,
      status = "stale-cache",
      action = "needs refresh decision",
      error = FALSE
    ))
  }

  list(
    path = path,
    source = mapping$source,
    local_mtime = local_mtime,
    source_mtime = source_mtime,
    status = "up-to-date",
    action = "reuse cache",
    error = FALSE
  )
}

input_mtime_for_recipe <- function(path, data_sources, planned = FALSE) {
  mapping <- data_sources$files[[path]]
  if (isTRUE(planned) && !is.null(mapping) && file.exists(mapping$source)) {
    return(file_mtime(mapping$source))
  }
  file_mtime(path)
}

analyze_recipe <- function(recipe, data_sources, planned = FALSE) {
  outputs_exist <- file.exists(recipe$outputs)
  missing_outputs <- recipe$outputs[!outputs_exist]
  input_mtimes <- vapply(
    recipe$inputs,
    function(path) input_mtime_for_recipe(path, data_sources, planned = planned),
    as.POSIXct(NA)
  )
  output_mtimes <- vapply(recipe$outputs, file_mtime, as.POSIXct(NA))
  newest_input <- if (all(is.na(input_mtimes))) as.POSIXct(NA) else max(input_mtimes, na.rm = TRUE)
  oldest_output <- if (all(is.na(output_mtimes))) as.POSIXct(NA) else min(output_mtimes, na.rm = TRUE)
  input_missing <- recipe$inputs[!vapply(recipe$inputs, function(path) {
    mapping <- data_sources$files[[path]]
    file.exists(path) || (!is.null(mapping) && file.exists(mapping$source))
  }, logical(1))]

  if (length(input_missing)) {
    return(list(
      name = recipe$name,
      script = recipe$script,
      inputs = recipe$inputs,
      outputs = recipe$outputs,
      status = "blocked",
      action = "missing recipe input",
      error = TRUE,
      missing_inputs = input_missing,
      newest_input_mtime = newest_input,
      oldest_output_mtime = oldest_output
    ))
  }

  if (length(missing_outputs)) {
    return(list(
      name = recipe$name,
      script = recipe$script,
      inputs = recipe$inputs,
      outputs = recipe$outputs,
      status = "missing-output",
      action = "run recipe",
      error = FALSE,
      missing_inputs = character(),
      newest_input_mtime = newest_input,
      oldest_output_mtime = oldest_output
    ))
  }

  if (newer_than(newest_input, oldest_output)) {
    return(list(
      name = recipe$name,
      script = recipe$script,
      inputs = recipe$inputs,
      outputs = recipe$outputs,
      status = "stale-output",
      action = "needs rebuild decision",
      error = FALSE,
      missing_inputs = character(),
      newest_input_mtime = newest_input,
      oldest_output_mtime = oldest_output
    ))
  }

  list(
    name = recipe$name,
    script = recipe$script,
    inputs = recipe$inputs,
    outputs = recipe$outputs,
    status = "up-to-date",
    action = "reuse artifacts",
    error = FALSE,
    missing_inputs = character(),
    newest_input_mtime = newest_input,
    oldest_output_mtime = oldest_output
  )
}

build_preparation_plan <- function(selected, data_sources, planned = TRUE) {
  requirements <- collect_preparation_requirements(selected, data_sources)
  direct <- lapply(requirements$direct_files, analyze_direct_file, data_sources = data_sources)
  names(direct) <- requirements$direct_files
  recipes <- lapply(requirements$recipes, analyze_recipe, data_sources = data_sources, planned = planned)

  list(
    requirements = requirements,
    direct = direct,
    recipes = recipes
  )
}

prep_errors <- function(prep_plan) {
  errors <- character()

  for (item in prep_plan$direct) {
    if (isTRUE(item$error)) {
      errors <- c(errors, sprintf(
        "%s: %s%s",
        item$path,
        item$action,
        if (!is.na(item$source)) sprintf(" (%s)", item$source) else ""
      ))
    }
  }

  for (item in prep_plan$recipes) {
    if (isTRUE(item$error)) {
      errors <- c(errors, sprintf(
        "%s: %s (%s)",
        item$name,
        item$action,
        paste(item$missing_inputs, collapse = ", ")
      ))
    }
  }

  errors
}

stale_items <- function(prep_plan) {
  direct <- Filter(function(item) identical(item$status, "stale-cache"), prep_plan$direct)
  recipes <- Filter(function(item) identical(item$status, "stale-output"), prep_plan$recipes)
  list(direct = direct, recipes = recipes)
}

print_preparation_plan <- function(prep_plan, data_sources, dry_run = FALSE) {
  cat("\nData preparation diagnostics:\n")
  cat(sprintf("  manifest: %s\n", data_sources$path))
  for (name in names(data_sources$roots)) {
    cat(sprintf("  source root %s: %s\n", name, data_sources$roots[[name]]))
  }

  if (length(prep_plan$direct)) {
    cat("  direct files:\n")
    for (item in prep_plan$direct) {
      source <- if (!is.na(item$source)) sprintf(" <- %s", item$source) else ""
      cat(sprintf("    - %s [%s] %s%s\n", item$path, item$status, item$action, source))
    }
  } else {
    cat("  direct files: none\n")
  }

  if (length(prep_plan$recipes)) {
    cat("  generated artifacts:\n")
    for (item in prep_plan$recipes) {
      cat(sprintf(
        "    - %s [%s] %s via %s -> %s\n",
        item$name,
        item$status,
        item$action,
        item$script,
        paste(item$outputs, collapse = ", ")
      ))
    }
  } else {
    cat("  generated artifacts: none\n")
  }

  errors <- prep_errors(prep_plan)
  if (length(errors)) {
    cat("  blocking issues:\n")
    for (error in errors) {
      cat(sprintf("    - %s\n", error))
    }
  }

  stale <- stale_items(prep_plan)
  if (dry_run && (length(stale$direct) || length(stale$recipes))) {
    cat("  stale cache note: preview/deploy will prompt unless --refresh-data or --use-cache is supplied.\n")
  }
}

stale_detail_direct <- function(item) {
  sprintf(
    "  local:  %s (%s)\n  source: %s (%s)",
    item$path,
    format_time(item$local_mtime),
    item$source,
    format_time(item$source_mtime)
  )
}

stale_detail_recipe <- function(item) {
  sprintf(
    "  outputs: %s (oldest %s)\n  inputs:  %s (newest %s)",
    paste(item$outputs, collapse = ", "),
    format_time(item$oldest_output_mtime),
    paste(item$inputs, collapse = ", "),
    format_time(item$newest_input_mtime)
  )
}

operation_verb <- function(operation) {
  if (identical(operation, "deployment")) "deploy" else operation
}

prompt_refresh <- function(label, detail = NULL, operation = "deployment") {
  if (!is.null(detail) && nzchar(detail)) {
    cat(detail, "\n", sep = "")
  }
  verb <- operation_verb(operation)
  if (!isatty(stdin())) {
    stop(sprintf(
      "Stale %s needs a refresh decision before %s. Re-run with --refresh-data or --use-cache.",
      label,
      verb
    ))
  }
  repeat {
    cat(sprintf("Refresh stale %s before %s? [y/N] ", label, verb))
    flush.console()
    answer <- readLines(file("stdin"), n = 1, warn = FALSE)
    if (!length(answer)) {
      stop(sprintf(
        "No stdin input available to decide stale %s before %s. Re-run with --refresh-data or --use-cache.",
        label,
        verb
      ))
    }
    normalized <- tolower(trimws(answer[[1]]))
    if (normalized %in% c("y", "yes")) return(TRUE)
    if (normalized %in% c("", "n", "no")) return(FALSE)
    cat("Please answer y or n.\n")
  }
}

resolve_stale_decision <- function(label,
                                   opts,
                                   detail = NULL,
                                   operation = "deployment",
                                   quiet = FALSE) {
  verb <- operation_verb(operation)
  if (isTRUE(opts$refresh_data)) {
    if (!isTRUE(quiet)) {
      cat(sprintf("Refreshing stale %s before %s (--refresh-data).\n", label, verb))
    }
    return(TRUE)
  }
  if (isTRUE(opts$use_cache)) {
    if (!isTRUE(quiet)) {
      cat(sprintf("Using cached stale %s for %s (--use-cache).\n", label, verb))
    }
    return(FALSE)
  }
  prompt_refresh(label, detail = detail, operation = operation)
}

copy_source_file <- function(item, data_sources) {
  mapping <- data_sources$files[[item$path]]
  if (is.null(mapping)) {
    stop(sprintf("No source mapping for %s.", item$path))
  }
  dir.create(dirname(item$path), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(mapping$source, item$path, overwrite = TRUE, copy.date = TRUE)
  if (!isTRUE(ok)) {
    stop(sprintf("Failed to copy %s to %s.", mapping$source, item$path))
  }
}

run_recipe <- function(recipe) {
  if (!file.exists(recipe$script)) {
    stop(sprintf("Recipe script not found: %s", recipe$script))
  }
  env <- new.env(parent = globalenv())
  sys.source(recipe$script, envir = env)
}

prepare_deployment_data <- function(selected,
                                    data_sources,
                                    opts,
                                    quiet = FALSE,
                                    operation = "deployment") {
  prep_plan <- build_preparation_plan(selected, data_sources, planned = TRUE)
  errors <- prep_errors(prep_plan)
  if (length(errors)) {
    stop(sprintf(
      "Cannot prepare deployment data:\n%s",
      paste(sprintf("  - %s", errors), collapse = "\n")
    ))
  }

  direct_refresh_decisions <- list()
  for (item in prep_plan$direct) {
    if (identical(item$status, "stale-cache")) {
      direct_refresh_decisions[[item$path]] <- resolve_stale_decision(
        item$path,
        opts,
        detail = stale_detail_direct(item),
        operation = operation,
        quiet = quiet
      )
    }
  }

  recipe_refresh_decisions <- list()
  for (item in prep_plan$recipes) {
    if (identical(item$status, "stale-output")) {
      recipe_refresh_decisions[[item$name]] <- resolve_stale_decision(
        item$name,
        opts,
        detail = stale_detail_recipe(item),
        operation = operation,
        quiet = quiet
      )
    }
  }

  for (item in prep_plan$direct) {
    if (identical(item$status, "missing-cache")) {
      if (!isTRUE(quiet)) cat(sprintf("Copying %s\n", item$path))
      copy_source_file(item, data_sources)
    } else if (identical(item$status, "stale-cache")) {
      refresh <- direct_refresh_decisions[[item$path]]
      if (isTRUE(refresh)) {
        if (!isTRUE(quiet)) cat(sprintf("Refreshing %s\n", item$path))
        copy_source_file(item, data_sources)
      } else if (!isTRUE(quiet)) {
        cat(sprintf("Using cached %s\n", item$path))
      }
    }
  }

  for (recipe in prep_plan$requirements$recipes) {
    status <- analyze_recipe(recipe, data_sources, planned = FALSE)
    if (isTRUE(status$error)) {
      stop(sprintf(
        "Cannot run recipe %s; missing inputs: %s",
        recipe$name,
        paste(status$missing_inputs, collapse = ", ")
      ))
    }
    should_run <- identical(status$status, "missing-output")
    if (identical(status$status, "stale-output")) {
      should_run <- recipe_refresh_decisions[[recipe$name]]
      if (is.null(should_run)) {
        should_run <- resolve_stale_decision(
          recipe$name,
          opts,
          detail = stale_detail_recipe(status),
          operation = operation,
          quiet = quiet
        )
      }
    }
    if (isTRUE(should_run)) {
      if (!isTRUE(quiet)) cat(sprintf("Running recipe %s (%s)\n", recipe$name, recipe$script))
      run_recipe(recipe)
    } else if (!isTRUE(quiet)) {
      cat(sprintf("Using cached artifacts for recipe %s\n", recipe$name))
    }
  }

  invisible(build_preparation_plan(selected, data_sources, planned = FALSE))
}

bundle_size_for_files <- function(files) {
  info <- file.info(files[file.exists(files)])
  sum(info$size, na.rm = TRUE)
}

print_target_bundle_plan <- function(entry, files, title = "Bundle plan") {
  existing <- files[file.exists(files)]
  missing <- setdiff(files, existing)
  total_size <- bundle_size_for_files(existing)
  cat(sprintf(
    "\n%s for %s: %s files, %s existing, %s missing, %s existing size\n",
    title,
    entry$target_id,
    length(files),
    length(existing),
    length(missing),
    format_bytes(total_size)
  ))
  for (file in files) {
    suffix <- if (file.exists(file)) "" else " [missing before preparation]"
    cat(sprintf("  - %s%s\n", file, suffix))
  }
  cat("  - active_graph.txt [generated in temp bundle]\n")
}

copy_file_to_bundle <- function(path, app_dir) {
  if (!file.exists(path)) {
    stop(sprintf("Required bundle file is missing after preparation: %s", path))
  }
  dest <- file.path(app_dir, path)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(path, dest, overwrite = TRUE, copy.date = TRUE)
  if (!isTRUE(ok)) {
    stop(sprintf("Failed to copy %s to bundle.", path))
  }
}

create_temp_bundle <- function(entry, data_sources) {
  files <- target_bundle_files(entry, data_sources)
  app_dir <- tempfile(sprintf("data-showroom-%s-", entry$target_id))
  dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

  for (file in files) {
    copy_file_to_bundle(file, app_dir)
  }
  writeLines(entry$graph, file.path(app_dir, "active_graph.txt"))

  bundle_files <- normalize_rel_path(list.files(app_dir, recursive = TRUE, full.names = FALSE))
  list(
    app_dir = app_dir,
    files = bundle_files,
    size = bundle_size_for_files(file.path(app_dir, bundle_files))
  )
}

print_created_bundle <- function(entry, bundle) {
  cat(sprintf(
    "  Bundle: %s files, %s, temp dir %s\n",
    length(bundle$files),
    format_bytes(bundle$size),
    bundle$app_dir
  ))
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

deploy_target <- function(entry, app_dir) {
  require_namespace("rsconnect", "deploy targets")

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

  if (!interactive()) {
    usage_error(sprintf(
      "--preview matched %s targets. Re-run with --yes to open them all, or narrow the selector.",
      count
    ))
  }

  repeat {
    cat(sprintf("Open %s preview apps in browser tabs? [y/N] ", count))
    flush.console()
    answer <- readLines(file("stdin"), n = 1, warn = FALSE)
    if (!length(answer)) {
      stop("Preview cancelled.")
    }
    normalized <- tolower(trimws(answer[[1]]))
    if (normalized %in% c("y", "yes")) return(invisible(TRUE))
    if (normalized %in% c("", "n", "no")) stop("Preview cancelled.")
    cat("Please answer y or n.\n")
  }
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
  require_namespace("processx", "run local previews")
  require_namespace("shiny", "run local previews")

  confirm_large_preview(selected, opts)
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

list_deploy_targets <- function(registry = "yaml/deploy_targets.yaml",
                                target = NULL,
                                profile = NULL,
                                tag = NULL,
                                include_disabled = FALSE,
                                targets = NULL) {
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

  if (isTRUE(opts$list)) {
    targets <- load_registry(opts$registry, require_auth = FALSE)
    validate_selectors(targets, opts)
    table <- list_deploy_targets(
      registry = opts$registry,
      target = opts$target,
      profile = opts$profile,
      tag = opts$tag,
      include_disabled = opts$include_disabled,
      targets = targets
    )
    return(invisible(list(status = 0L, targets = table, results = list())))
  }

  targets <- load_registry(opts$registry, require_auth = !isTRUE(opts$preview))
  data_sources <- load_data_sources(opts$data_sources, source_root_override = opts$source_root)
  has_selector <- opts$all || length(opts$target) || length(opts$profile) || length(opts$tag)

  if (!has_selector && interactive()) {
    cat("No selector provided. Choose targets by name (target_id).\n\n")
    list_deploy_targets(registry = opts$registry)
    cat("\nExample:\n  deploy_by_target(\"inhe_multi\")\n")
    cat("  deploy_by_target(c(\"inhe_single\", \"inhe_multi\"), dry_run = TRUE)\n")
    cat("  preview_by_target(c(\"inhe_single\", \"inhe_multi\"))\n")
    cat("  main(c(\"--profile\", \"gregcull\", \"--dry-run\"))\n")
    return(invisible(list(status = 0L, selected = list(), results = list())))
  }

  selected <- select_targets(targets, opts)

  if (!isTRUE(quiet)) {
    print_selection(selected)
  }

  prep_plan <- build_preparation_plan(selected, data_sources, planned = TRUE)
  if (!isTRUE(quiet)) {
    print_preparation_plan(prep_plan, data_sources, dry_run = isTRUE(opts$dry_run))
    bundle_title <- if (isTRUE(opts$dry_run)) {
      "Dry-run bundle plan"
    } else if (isTRUE(opts$preview)) {
      "Preview bundle plan"
    } else {
      "Planned bundle"
    }
    for (entry in selected) {
      print_target_bundle_plan(entry, target_bundle_files(entry, data_sources), title = bundle_title)
    }
  }

  if (isTRUE(opts$dry_run)) {
    if (!isTRUE(quiet)) {
      cat("\nDry run complete. No files copied, generated, or deployed.\n")
    }
    return(invisible(list(status = 0L, selected = selected, preparation = prep_plan, results = list())))
  }

  if (isTRUE(opts$preview)) {
    result <- preview_targets(selected, data_sources, opts, quiet = quiet)
    return(invisible(list(
      status = result$status,
      selected = selected,
      preparation = prep_plan,
      results = list(result)
    )))
  }

  prepare_deployment_data(selected, data_sources, opts, quiet = quiet, operation = "deployment")

  results <- vector("list", length(selected))
  temp_dirs <- character()
  on.exit(unlink(temp_dirs, recursive = TRUE, force = TRUE), add = TRUE)

  for (i in seq_along(selected)) {
    entry <- selected[[i]]
    cat(sprintf("\n[%s/%s] Deploying '%s' (%s)...\n",
                i, length(selected), entry$target_id, entry$app_name))
    start_time <- Sys.time()

    result <- tryCatch({
      bundle <- create_temp_bundle(entry, data_sources)
      temp_dirs <<- c(temp_dirs, bundle$app_dir)
      print_created_bundle(entry, bundle)
      deploy_target(entry, bundle$app_dir)
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
                             preview = FALSE,
                             registry = "yaml/deploy_targets.yaml",
                             data_sources = "yaml/deploy_data_sources.yaml",
                             source_root = NULL,
                             refresh_data = FALSE,
                             use_cache = FALSE,
                             preview_host = "127.0.0.1",
                             preview_port = NULL,
                             launch_browser = TRUE,
                             yes = FALSE) {
  if (!length(target_id)) {
    usage_error("`target_id` cannot be empty.")
  }
  preview_option_used <- !identical(preview_host, "127.0.0.1") ||
    !is.null(preview_port) ||
    !isTRUE(launch_browser) ||
    isTRUE(yes)
  if (!isTRUE(preview) && isTRUE(preview_option_used)) {
    usage_error("Preview options require `preview = TRUE`.")
  }

  args <- c("--target", paste(target_id, collapse = ","))
  if (isTRUE(dry_run)) {
    args <- c(args, "--dry-run")
  }
  if (isTRUE(preview)) {
    args <- c(args, "--preview")
    if (!identical(preview_host, "127.0.0.1")) {
      args <- c(args, "--preview-host", preview_host)
    }
    if (!is.null(preview_port)) {
      args <- c(args, "--preview-port", as.character(preview_port))
    }
    if (!isTRUE(launch_browser)) {
      args <- c(args, "--no-browser")
    }
    if (isTRUE(yes)) {
      args <- c(args, "--yes")
    }
  }
  if (isTRUE(refresh_data)) {
    args <- c(args, "--refresh-data")
  }
  if (isTRUE(use_cache)) {
    args <- c(args, "--use-cache")
  }
  args <- c(args, "--registry", registry, "--data-sources", data_sources)
  if (!is.null(source_root)) {
    args <- c(args, "--source-root", source_root)
  }
  main(args = args)
}

preview_by_target <- function(target_id,
                              registry = "yaml/deploy_targets.yaml",
                              data_sources = "yaml/deploy_data_sources.yaml",
                              source_root = NULL,
                              refresh_data = FALSE,
                              use_cache = FALSE,
                              host = "127.0.0.1",
                              port = NULL,
                              launch_browser = TRUE,
                              yes = FALSE) {
  if (!length(target_id)) {
    usage_error("`target_id` cannot be empty.")
  }

  deploy_by_target(
    target_id = target_id,
    preview = TRUE,
    registry = registry,
    data_sources = data_sources,
    source_root = source_root,
    refresh_data = refresh_data,
    use_cache = use_cache,
    preview_host = host,
    preview_port = port,
    launch_browser = launch_browser,
    yes = yes
  )
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
