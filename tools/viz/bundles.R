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

recipe_dependency_names <- function(recipes) {
  output_owner <- character()
  for (recipe in recipes) {
    duplicate_outputs <- intersect(names(output_owner), recipe$outputs)
    if (length(duplicate_outputs)) {
      stop(sprintf(
        "Multiple recipes produce the same output(s): %s",
        paste(duplicate_outputs, collapse = ", ")
      ))
    }
    output_owner[recipe$outputs] <- recipe$name
  }

  dependencies <- lapply(recipes, function(recipe) {
    owners <- unname(output_owner[recipe$inputs])
    unique(owners[!is.na(owners)])
  })
  names(dependencies) <- vapply(recipes, `[[`, character(1), "name")
  dependencies
}

order_recipes_by_dependencies <- function(recipes) {
  if (!length(recipes)) return(recipes)

  dependencies <- recipe_dependency_names(recipes)
  remaining <- names(dependencies)
  ordered <- character()

  while (length(remaining)) {
    ready <- remaining[vapply(remaining, function(name) {
      all(!dependencies[[name]] %in% remaining)
    }, logical(1))]
    if (!length(ready)) {
      stop(sprintf(
        "Recipe dependency cycle detected among: %s",
        paste(remaining, collapse = ", ")
      ))
    }
    ordered <- c(ordered, ready)
    remaining <- setdiff(remaining, ready)
  }

  recipes[ordered]
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

  recipes <- order_recipes_by_dependencies(data_sources$recipes[recipe_names])

  list(
    required_data = required_data,
    direct_files = direct_files,
    recipes = recipes
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

analyze_recipe <- function(recipe,
                            data_sources,
                            planned = FALSE,
                            generated_outputs = character()) {
  outputs_exist <- file.exists(recipe$outputs)
  missing_outputs <- recipe$outputs[!outputs_exist]
  input_mtimes <- do.call(c, lapply(
    recipe$inputs,
    function(path) input_mtime_for_recipe(path, data_sources, planned = planned)
  ))
  output_mtimes <- do.call(c, lapply(recipe$outputs, file_mtime))
  newest_input <- if (all(is.na(input_mtimes))) as.POSIXct(NA) else max(input_mtimes, na.rm = TRUE)
  oldest_output <- if (all(is.na(output_mtimes))) as.POSIXct(NA) else min(output_mtimes, na.rm = TRUE)
  input_missing <- recipe$inputs[!vapply(recipe$inputs, function(path) {
    mapping <- data_sources$files[[path]]
    file.exists(path) ||
      path %in% generated_outputs ||
      (!is.null(mapping) && file.exists(mapping$source))
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
  generated_outputs <- unique(unlist(lapply(requirements$recipes, `[[`, "outputs"), use.names = FALSE))
  recipes <- lapply(
    requirements$recipes,
    analyze_recipe,
    data_sources = data_sources,
    planned = planned,
    generated_outputs = generated_outputs
  )

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
  choice <- choose_menu_value(
    sprintf("Refresh stale %s before %s?", label, verb),
    c("Refresh", "Use cache", "Cancel"),
    values = list("refresh", "cache", "cancel")
  )
  if (is.null(choice) || identical(choice, "cancel")) {
    cancel_viz()
  }
  identical(choice, "refresh")
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

  recipe_dependencies <- recipe_dependency_names(prep_plan$requirements$recipes)
  generated_outputs <- unique(unlist(
    lapply(prep_plan$requirements$recipes, `[[`, "outputs"),
    use.names = FALSE
  ))
  rebuilt_recipes <- character()
  for (recipe in prep_plan$requirements$recipes) {
    status <- analyze_recipe(
      recipe,
      data_sources,
      planned = FALSE,
      generated_outputs = generated_outputs
    )
    if (isTRUE(status$error)) {
      stop(sprintf(
        "Cannot run recipe %s; missing inputs: %s",
        recipe$name,
        paste(status$missing_inputs, collapse = ", ")
      ))
    }
    should_run <- identical(status$status, "missing-output")
    if (identical(status$status, "stale-output")) {
      should_run <- any(recipe_dependencies[[recipe$name]] %in% rebuilt_recipes)
      if (!should_run) {
        should_run <- recipe_refresh_decisions[[recipe$name]]
      }
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
      rebuilt_recipes <- c(rebuilt_recipes, recipe$name)
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
