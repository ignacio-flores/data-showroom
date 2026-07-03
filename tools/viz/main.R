selector_present <- function(opts) {
  opts$all || length(opts$target) || length(opts$profile) || length(opts$tag)
}

enabled_targets_for_menu <- function(targets, opts) {
  candidates <- Filter(function(entry) isTRUE(entry$enabled), targets)
  if (length(opts$profile)) {
    candidates <- Filter(function(entry) entry$profile %in% opts$profile, candidates)
  }
  if (length(opts$tag)) {
    candidates <- Filter(function(entry) any(opts$tag %in% entry$tags), candidates)
  }
  candidates
}

target_menu_labels <- function(targets) {
  c("all", vapply(targets, `[[`, character(1), "target_id"))
}

tag_menu_labels <- function(targets) {
  c("all", sort(unique(unlist(lapply(targets, `[[`, "tags"), use.names = FALSE))))
}

profile_menu_labels <- function(targets) {
  c("all", sort(unique(vapply(targets, `[[`, character(1), "profile"))))
}

apply_selector_menu <- function(opts,
                                targets,
                                menu_fn = terminal_menu,
                                stdin_interactive = isatty(stdin())) {
  if (is.null(opts$selector_menu)) {
    return(opts)
  }

  if (!isTRUE(stdin_interactive)) {
    usage_error(sprintf(
      "%s requires an interactive terminal or an explicit value.",
      opts$selector_menu
    ))
  }

  if (identical(opts$selector_menu, "target")) {
    candidates <- enabled_targets_for_menu(targets, opts)
    if (!length(candidates)) {
      usage_error("No enabled targets match the current filters.")
    }
    labels <- target_menu_labels(candidates)
    values <- c("all", vapply(candidates, `[[`, character(1), "target_id"))
    choice <- choose_menu_value("Choose target:", labels, values = values, menu_fn = menu_fn)
    if (is.null(choice)) {
      opts$cancelled <- TRUE
      return(opts)
    }
    if (identical(choice, "all")) {
      opts$all <- TRUE
    } else {
      opts$target <- choice
    }
  } else if (identical(opts$selector_menu, "tag")) {
    candidates <- Filter(function(entry) isTRUE(entry$enabled), targets)
    labels <- tag_menu_labels(candidates)
    if (length(labels) <= 1L) {
      usage_error("No tags are available in the deployment registry.")
    }
    choice <- choose_menu_value("Choose tag:", labels, values = labels, menu_fn = menu_fn)
    if (is.null(choice)) {
      opts$cancelled <- TRUE
      return(opts)
    }
    if (identical(choice, "all")) {
      opts$all <- TRUE
    } else {
      opts$tag <- choice
    }
  } else if (identical(opts$selector_menu, "profile")) {
    candidates <- Filter(function(entry) isTRUE(entry$enabled), targets)
    labels <- profile_menu_labels(candidates)
    choice <- choose_menu_value("Choose profile:", labels, values = labels, menu_fn = menu_fn)
    if (is.null(choice)) {
      opts$cancelled <- TRUE
      return(opts)
    }
    if (identical(choice, "all")) {
      opts$all <- TRUE
    } else {
      opts$profile <- choice
    }
  }

  opts$selector_menu <- NULL
  opts
}

main <- function(args = commandArgs(trailingOnly = TRUE),
                 quiet = FALSE,
                 stdin_interactive = isatty(stdin())) {
  opts <- parse_args(args)
  if (isTRUE(opts$help)) {
    usage()
    return(invisible(list(status = 0L, selected = list(), results = list())))
  }

  if (isTRUE(opts$install)) {
    return(install_viz_command(opts, quiet = quiet))
  }

  if (isTRUE(opts$uninstall)) {
    return(uninstall_viz_command(opts, quiet = quiet))
  }

  if (isTRUE(opts$list)) {
    targets <- load_registry(opts$registry, require_auth = FALSE)
    opts <- apply_selector_menu(opts, targets, stdin_interactive = stdin_interactive)
    if (isTRUE(opts$cancelled)) {
      if (!isTRUE(quiet)) {
        cat("Cancelled. No preview or deployment was started.\n")
      }
      return(invisible(list(status = 0L, targets = data.frame(), results = list(), cancelled = TRUE)))
    }
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

  targets <- load_registry(opts$registry, require_auth = isTRUE(opts$deploy))
  opts <- apply_selector_menu(opts, targets, stdin_interactive = stdin_interactive)
  if (isTRUE(opts$cancelled)) {
    if (!isTRUE(quiet)) {
      cat("Cancelled. No preview or deployment was started.\n")
    }
    return(invisible(list(status = 0L, selected = list(), results = list(), cancelled = TRUE)))
  }
  if (!selector_present(opts)) {
    usage_error("No selector provided. Use target, profile, tag, or all.")
  }

  selected <- select_targets(targets, opts)
  data_sources <- load_data_sources(opts$data_sources, source_root_override = opts$source_root)

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
  deployment <- deploy_targets_with_retries(selected, data_sources, quiet = quiet)

  invisible(list(
    status = deployment$status,
    selected = selected,
    preparation = prep_plan,
    results = deployment$results,
    attempts = deployment$attempts,
    failed = deployment$failed
  ))
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
  target_id <- normalize_selector_values(target_id, "target_id")
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
    if (!identical(preview_host, "127.0.0.1")) {
      args <- c(args, "preview-host", preview_host)
    }
    if (!is.null(preview_port)) {
      args <- c(args, "preview-port", as.character(preview_port))
    }
    if (!isTRUE(launch_browser)) {
      args <- c(args, "no-browser")
    }
    if (isTRUE(yes)) {
      args <- c(args, "yes")
    }
  }
  if (isTRUE(refresh_data)) {
    args <- c(args, "refresh-data")
  }
  if (isTRUE(use_cache)) {
    args <- c(args, "use-cache")
  }
  args <- c(args, "registry", registry, "data-sources", data_sources)
  if (!is.null(source_root)) {
    args <- c(args, "source-root", source_root)
  }
  if (!isTRUE(preview) && !isTRUE(dry_run)) {
    args <- c(args, "deploy")
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
  target_id <- normalize_selector_values(target_id, "target_id")
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

cli_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  status <- 0L

  tryCatch({
    result <- main(args)
    status <<- as.integer(result$status %||% 0L)
  }, deploy_usage_error = function(e) {
    cat(sprintf("Error: %s\n\n", conditionMessage(e)))
    usage()
    status <<- 1L
  }, viz_cancelled = function(e) {
    cat(conditionMessage(e), "\n", sep = "")
    status <<- 0L
  }, error = function(e) {
    cat(sprintf("Error: %s\n", conditionMessage(e)))
    status <<- 1L
  })

  quit(status = status, save = "no")
}
