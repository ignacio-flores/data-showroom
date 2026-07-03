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

deploy_targets_once <- function(selected, data_sources, attempt = 1L, quiet = FALSE) {
  results <- vector("list", length(selected))
  temp_dirs <- character()
  on.exit(unlink(temp_dirs, recursive = TRUE, force = TRUE), add = TRUE)

  for (i in seq_along(selected)) {
    entry <- selected[[i]]
    if (!isTRUE(quiet)) {
      prefix <- if (attempt > 1L) sprintf("Retry %s: ", attempt - 1L) else ""
      cat(sprintf("\n%s[%s/%s] Deploying '%s' (%s)...\n",
                  prefix, i, length(selected), entry$target_id, entry$app_name))
    }
    start_time <- Sys.time()

    result <- tryCatch({
      bundle <- create_temp_bundle(entry, data_sources)
      temp_dirs <<- c(temp_dirs, bundle$app_dir)
      if (!isTRUE(quiet)) {
        print_created_bundle(entry, bundle)
      }
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

    results[[i]] <- c(list(entry = entry, attempt = attempt), result)
    if (!isTRUE(quiet)) {
      if (identical(result$status, "success")) {
        cat(sprintf("  SUCCESS in %.1fs\n", result$elapsed))
      } else {
        cat(sprintf("  FAILED in %.1fs\n  %s\n", result$elapsed, result$error))
      }
    }
  }

  results
}

failed_deployments <- function(results) {
  Filter(function(x) identical(x$status, "failed"), results)
}

target_ids_from_entries <- function(entries) {
  vapply(entries, `[[`, character(1), "target_id")
}

target_ids_from_results <- function(results) {
  vapply(results, function(item) item$entry$target_id, character(1))
}

print_deployment_summary <- function(results, title = "Deployment summary:") {
  success_count <- sum(vapply(results, function(x) identical(x$status, "success"), logical(1)))
  failed <- failed_deployments(results)

  cat("\n", title, "\n", sep = "")
  cat(sprintf("  Successful: %s\n", success_count))
  cat(sprintf("  Failed: %s\n", length(failed)))

  if (length(failed)) {
    cat("  Failed targets:\n")
    for (item in failed) {
      cat(sprintf("    - %s: %s\n", item$entry$target_id, item$error))
    }
  }

  invisible(failed)
}

prompt_retry_failed_deployments <- function(failed,
                                            original_count,
                                            quiet = FALSE,
                                            stdin_interactive = isatty(stdin()),
                                            read_line = read_stdin_line,
                                            menu_fn = terminal_menu) {
  if (!length(failed) || original_count <= 1L || isTRUE(quiet) || !isTRUE(stdin_interactive)) {
    return(FALSE)
  }

  choice <- choose_menu_value(
    "Retry failed targets?",
    c("Retry failed targets", "Stop"),
    values = list(TRUE, FALSE),
    cancel_value = FALSE,
    menu_fn = menu_fn
  )
  isTRUE(choice)
}

deploy_targets_with_retries <- function(selected,
                                        data_sources,
                                        quiet = FALSE,
                                        stdin_interactive = isatty(stdin()),
                                        read_line = read_stdin_line,
                                        menu_fn = terminal_menu) {
  current <- selected
  attempt <- 1L
  all_results <- list()
  attempts <- list()
  final_failed <- list()

  repeat {
    results <- deploy_targets_once(current, data_sources, attempt = attempt, quiet = quiet)
    all_results <- c(all_results, results)
    summary_title <- if (attempt == 1L) {
      "Deployment summary:"
    } else {
      sprintf("Retry %s summary:", attempt - 1L)
    }
    failed <- if (isTRUE(quiet)) failed_deployments(results) else print_deployment_summary(results, summary_title)
    attempts[[attempt]] <- list(
      attempt = attempt,
      target_ids = target_ids_from_entries(current),
      results = results,
      failed_target_ids = target_ids_from_results(failed)
    )

    if (!length(failed)) {
      return(list(
        status = 0L,
        results = all_results,
        attempts = attempts,
        failed = list()
      ))
    }

    final_failed <- failed
    if (!prompt_retry_failed_deployments(
      failed,
      original_count = length(selected),
      quiet = quiet,
      stdin_interactive = stdin_interactive,
      read_line = read_line,
      menu_fn = menu_fn
    )) {
      return(list(
        status = 1L,
        results = all_results,
        attempts = attempts,
        failed = final_failed
      ))
    }

    current <- lapply(failed, `[[`, "entry")
    attempt <- attempt + 1L
  }
}
