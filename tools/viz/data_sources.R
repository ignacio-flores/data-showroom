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
