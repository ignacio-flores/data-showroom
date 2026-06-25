#!/usr/bin/env Rscript

source("deploy-app.R")

# Helpers ---------------------------------------------------------------------

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_identical <- function(actual, expected, message) {
  if (!identical(actual, expected)) {
    fail(paste0(
      message,
      "\nExpected: ", paste(expected, collapse = ", "),
      "\nActual: ", paste(actual, collapse = ", ")
    ))
  }
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

expect_error <- function(expr, pattern, message) {
  err <- tryCatch({
    force(expr)
    NULL
  }, error = function(e) e)
  if (is.null(err)) {
    fail(paste0(message, "\nExpected an error, but none was raised."))
  }
  if (!grepl(pattern, conditionMessage(err), fixed = FALSE)) {
    fail(paste0(
      message,
      "\nExpected error matching: ", pattern,
      "\nActual error: ", conditionMessage(err)
    ))
  }
}

expect_output_contains <- function(output, patterns, message) {
  missing <- patterns[!vapply(patterns, function(pattern) {
    any(grepl(pattern, output, fixed = TRUE))
  }, logical(1))]
  if (length(missing)) {
    fail(paste0(
      message,
      "\nMissing output pattern(s): ", paste(missing, collapse = ", "),
      "\nOutput:\n", paste(output, collapse = "\n")
    ))
  }
}

with_mocked_globals <- function(replacements, expr) {
  if (!is.list(replacements) || is.null(names(replacements)) || any(!nzchar(names(replacements)))) {
    fail("with_mocked_globals() requires a named list of replacements.")
  }

  expr <- substitute(expr)
  env <- globalenv()
  replacement_names <- names(replacements)
  missing_marker <- structure(list(), class = "missing_global")
  originals <- lapply(replacement_names, function(name) {
    if (exists(name, envir = env, inherits = FALSE)) {
      get(name, envir = env, inherits = FALSE)
    } else {
      missing_marker
    }
  })
  names(originals) <- replacement_names

  on.exit({
    for (name in rev(replacement_names)) {
      original <- originals[[name]]
      if (inherits(original, "missing_global")) {
        if (exists(name, envir = env, inherits = FALSE)) {
          rm(list = name, envir = env)
        }
      } else {
        assign(name, original, envir = env)
      }
    }
  }, add = TRUE)

  for (name in replacement_names) {
    assign(name, replacements[[name]], envir = env)
  }

  eval(expr, envir = parent.frame())
}

covered_help_options <- character()

cover_help_options <- function(...) {
  covered_help_options <<- sort(unique(c(covered_help_options, unlist(list(...)))))
}

usage_options <- function() {
  lines <- capture.output(usage())
  sort(unique(unlist(regmatches(lines, gregexpr("--[A-Za-z0-9-]+", lines)))))
}

expect_help_options_covered <- function() {
  announced <- usage_options()
  expect_identical(
    setdiff(announced, covered_help_options),
    character(),
    "Every option announced by usage() should have explicit test coverage."
  )
  expect_identical(
    setdiff(covered_help_options, announced),
    character(),
    "Help option coverage should not include options absent from usage()."
  )
}

target_ids <- function(selected) {
  vapply(selected, `[[`, character(1), "target_id")
}

# Fixtures --------------------------------------------------------------------

fixture_targets <- list(
  list(
    target_id = "alpha",
    graph = "graph_alpha",
    app_name = "alpha-app",
    profile = "main",
    server = "shinyapps.io",
    tags = c("eigt", "kf"),
    enabled = TRUE
  ),
  list(
    target_id = "beta",
    graph = "graph_beta",
    app_name = "beta-app",
    profile = "main",
    server = "shinyapps.io",
    tags = c("eigt", "wm"),
    enabled = TRUE
  ),
  list(
    target_id = "gamma",
    graph = "graph_gamma",
    app_name = "gamma-app",
    profile = "other",
    server = "shinyapps.io",
    tags = c("topo"),
    enabled = TRUE
  ),
  list(
    target_id = "disabled",
    graph = "graph_disabled",
    app_name = "disabled-app",
    profile = "main",
    server = "shinyapps.io",
    tags = c("eigt"),
    enabled = FALSE
  )
)

# Help behavior and coverage --------------------------------------------------

cover_help_options("--help")
help_opts <- parse_args(c("--help", "--bogus", "--registry"))
expect_true(
  isTRUE(help_opts$help),
  "--help should short-circuit argument validation in parse_args()."
)

help_result <- NULL
help_output <- capture.output({
  help_result <- main(c("--help", "--bogus"))
})
expect_identical(
  help_result$status,
  0L,
  "--help should return success even when unrelated invalid args follow it."
)
expect_true(
  any(grepl("Usage:", help_output, fixed = TRUE)),
  "--help should print usage text."
)

# Parser/options --------------------------------------------------------------

cover_help_options("--target", "--profile", "--tag")
expect_identical(
  parse_args(c("--target", "alpha,beta"))$target,
  c("alpha", "beta"),
  "No-space comma target lists should parse."
)

expect_identical(
  parse_args(c("--target", "alpha,", "beta"))$target,
  c("alpha", "beta"),
  "Comma-space target lists should parse when the shell splits them."
)

expect_identical(
  parse_args(c("--target=alpha,", "beta"))$target,
  c("alpha", "beta"),
  "Equals-form comma-space target lists should parse when the shell splits them."
)

expect_identical(
  parse_args(c("--target", "alpha", "--target", "beta"))$target,
  c("alpha", "beta"),
  "Repeated target flags should parse."
)

expect_identical(
  parse_args(c("--profile", "main,", "other", "--tag=eigt,", "kf"))[c("profile", "tag")],
  list(profile = c("main", "other"), tag = c("eigt", "kf")),
  "Profile and tag selectors should share comma-space parsing."
)

expect_error(
  parse_args(c("--target", "alpha", "beta")),
  "Separate multiple values with commas",
  "Bare whitespace target lists should be rejected."
)

expect_error(
  parse_args(c("--target", "alpha,")),
  "Empty value in --target",
  "Trailing target commas should be rejected."
)

expect_error(
  parse_args(c("--target", "alpha,,beta")),
  "Empty value in --target",
  "Repeated target commas should be rejected."
)

cover_help_options("--dry-run")
dry_opts <- parse_args(c("--target", "alpha", "--dry-run"))
expect_true(
  isTRUE(dry_opts$dry_run),
  "--dry-run should parse with an explicit target selector."
)

expect_error(
  parse_args(c("--registry=")),
  "Missing value for --registry",
  "Empty registry values should be rejected."
)

expect_error(
  parse_args(c("--registry", "--dry-run")),
  "Missing value for --registry",
  "Option-like registry values should be rejected."
)

cover_help_options("--preview", "--preview-host", "--preview-port", "--no-browser", "--yes")
preview_opts <- parse_args(c(
  "--profile", "main",
  "--preview",
  "--preview-host", "0.0.0.0",
  "--preview-port", "8767",
  "--no-browser",
  "--yes"
))
expect_true(
  isTRUE(preview_opts$preview) &&
    identical(preview_opts$preview_host, "0.0.0.0") &&
    identical(preview_opts$preview_port, 8767L) &&
    !isTRUE(preview_opts$launch_browser) &&
    isTRUE(preview_opts$yes),
  "Preview options should parse together when --preview is supplied."
)

expect_error(
  parse_args(c("--preview", "--preview-port", "70000")),
  "--preview-port must be an integer",
  "Invalid preview ports should be rejected."
)

expect_error(
  parse_args(c("--target", "alpha", "--all")),
  "--all cannot be used with --target",
  "--all and --target should be mutually exclusive."
)

expect_error(
  parse_args(c("--target", "alpha", "--dry-run", "--refresh-data")),
  "--dry-run cannot be used",
  "--dry-run should not accept refresh/cache flags."
)

expect_error(
  parse_args(c("--no-browser")),
  "require --preview",
  "Preview-only flags should require --preview."
)

expect_true(
  isTRUE(confirm_large_preview(fixture_targets[1:2], preview_opts, threshold = 1L)),
  "--yes should skip large preview confirmation."
)

cover_help_options("--refresh-data", "--use-cache")
refresh_opts <- parse_args(c("--target", "alpha", "--refresh-data"))
cache_opts <- parse_args(c("--target", "alpha", "--use-cache"))
expect_true(
  isTRUE(resolve_stale_decision("fixture-cache", refresh_opts, quiet = TRUE)),
  "--refresh-data should refresh stale dependencies without prompting."
)
expect_identical(
  resolve_stale_decision("fixture-cache", cache_opts, quiet = TRUE),
  FALSE,
  "--use-cache should reuse stale dependencies without prompting."
)
expect_error(
  parse_args(c("--target", "alpha", "--refresh-data", "--use-cache")),
  "--refresh-data and --use-cache cannot be used together",
  "Refresh and cache decisions should be mutually exclusive."
)

# Selection/listing -----------------------------------------------------------

cover_help_options("--all")
opts <- parse_args(c("--all", "--profile", "main", "--tag", "eigt"))
expect_identical(
  target_ids(select_targets(fixture_targets, opts)),
  c("alpha", "beta"),
  "--all should combine with profile/tag filters and exclude disabled targets."
)

opts <- parse_args(c("--target", "alpha,", "beta"))
expect_identical(
  target_ids(select_targets(fixture_targets, opts)),
  c("alpha", "beta"),
  "Explicit target selection should preserve requested IDs."
)

opts <- parse_args(c("--profile", "main", "--tag", "eigt"))
expect_identical(
  target_ids(select_targets(fixture_targets, opts)),
  c("alpha", "beta"),
  "Bulk profile/tag selection should exclude disabled targets."
)

opts <- parse_args(c("--target", "disabled"))
expect_identical(
  target_ids(select_targets(fixture_targets, opts)),
  "disabled",
  "Explicit target selection should allow disabled targets."
)

opts <- parse_args(c("--target", "alpha", "--profile", "other"))
expect_error(
  select_targets(fixture_targets, opts),
  "Selection did not match any deployment targets",
  "Mismatched selector intersections should produce a usage error."
)

cover_help_options("--list", "--include-disabled", "--registry")
list_opts <- parse_args(c("--list", "--include-disabled", "--registry", "registry.yml"))
expect_true(
  isTRUE(list_opts$list) && isTRUE(list_opts$include_disabled) && identical(list_opts$registry, "registry.yml"),
  "--list should accept --include-disabled and --registry."
)

listed <- NULL
invisible(capture.output({
  listed <- list_deploy_targets(targets = fixture_targets)
}))
expect_identical(
  listed$target_id,
  c("alpha", "beta", "gamma"),
  "list_deploy_targets() should exclude disabled targets by default."
)

listed <- NULL
invisible(capture.output({
  listed <- list_deploy_targets(include_disabled = TRUE, targets = fixture_targets)
}))
expect_identical(
  listed$target_id,
  c("alpha", "beta", "gamma", "disabled"),
  "--include-disabled should include disabled fixture targets in list mode."
)

listed <- NULL
invisible(capture.output({
  listed <- list_deploy_targets(
    target = "alpha, beta",
    targets = fixture_targets
  )
}))
expect_identical(
  listed$target_id,
  c("alpha", "beta"),
  "list_deploy_targets() should normalize comma-string target filters."
)

listed <- NULL
invisible(capture.output({
  listed <- list_deploy_targets(
    profile = c("main", "other"),
    tag = c("topo"),
    targets = fixture_targets
  )
}))
expect_identical(
  listed$target_id,
  "gamma",
  "list_deploy_targets() should normalize vector profile/tag filters."
)

# Data source/stale decisions -------------------------------------------------

cover_help_options("--data-sources", "--source-root")
data_manifest <- tempfile("deploy-data-sources-", fileext = ".yaml")
default_root <- file.path(tempdir(), "deploy-default-root")
override_root <- file.path(tempdir(), "deploy-override-root")
writeLines(
  c(
    "source_roots:",
    "  gcwealth:",
    sprintf("    default: %s", default_root),
    "files: {}",
    "recipes: {}",
    "runtime_dependencies: {}"
  ),
  data_manifest
)
data_opts <- parse_args(c(
  "--target", "alpha",
  "--data-sources", data_manifest,
  "--source-root", override_root
))
expect_true(
  identical(data_opts$data_sources, data_manifest) &&
    identical(data_opts$source_root, override_root),
  "--data-sources and --source-root should parse as strict one-value options."
)
loaded_sources <- load_data_sources(data_opts$data_sources, source_root_override = data_opts$source_root)
expect_identical(
  loaded_sources$roots$gcwealth,
  normalizePath(override_root, mustWork = FALSE),
  "--source-root should override manifest source roots when loading data sources."
)

# Helper wrappers -------------------------------------------------------------

with_mocked_globals(
  list(main = function(args = commandArgs(trailingOnly = TRUE), quiet = FALSE) args),
  {
    deploy_args <- deploy_by_target("alpha, beta", dry_run = TRUE)
    expect_identical(
      parse_args(deploy_args)$target,
      c("alpha", "beta"),
      "deploy_by_target() should normalize comma-string target IDs."
    )

    deploy_args <- deploy_by_target(c("alpha", "beta"), dry_run = TRUE)
    expect_identical(
      parse_args(deploy_args)$target,
      c("alpha", "beta"),
      "deploy_by_target() should normalize vector target IDs."
    )

    preview_args <- preview_by_target("alpha, beta", launch_browser = FALSE)
    parsed_preview <- parse_args(preview_args)
    expect_identical(
      parsed_preview$target,
      c("alpha", "beta"),
      "preview_by_target() should normalize comma-string target IDs."
    )
    expect_true(
      isTRUE(parsed_preview$preview) && !isTRUE(parsed_preview$launch_browser),
      "preview_by_target() should preserve preview options."
    )
  }
)

# Deploy retry ----------------------------------------------------------------

run_stubbed_deploy <- function(outcomes,
                               answers = character(),
                               selected = fixture_targets[1:2],
                               stdin_interactive = TRUE,
                               quiet = FALSE) {
  calls <- character()
  answer_idx <- 0L
  call_counts <- new.env(parent = emptyenv())

  read_line <- function() {
    answer_idx <<- answer_idx + 1L
    if (answer_idx > length(answers)) {
      fail("Unexpected retry prompt.")
    }
    answers[[answer_idx]]
  }

  result <- NULL
  output <- capture.output({
    result <- with_mocked_globals(
      list(
        create_temp_bundle = function(entry, data_sources) {
          list(app_dir = tempfile(sprintf("deploy-test-%s-", entry$target_id)), files = character(), size = 0)
        },
        print_created_bundle = function(entry, bundle) invisible(NULL),
        deploy_target = function(entry, app_dir) {
          id <- entry$target_id
          calls <<- c(calls, id)
          count <- get0(id, envir = call_counts, ifnotfound = 0L) + 1L
          assign(id, count, envir = call_counts)
          target_outcomes <- outcomes[[id]] %||% "success"
          outcome <- target_outcomes[[min(count, length(target_outcomes))]]
          if (identical(outcome, "fail")) {
            stop(sprintf("%s failed on attempt %s", id, count), call. = FALSE)
          }
          invisible(TRUE)
        }
      ),
      {
        deploy_targets_with_retries(
          selected,
          data_sources = list(),
          quiet = quiet,
          stdin_interactive = stdin_interactive,
          read_line = read_line
        )
      }
    )
  })

  list(result = result, calls = calls, answers = answer_idx, output = output)
}

deploy_run <- run_stubbed_deploy(list(beta = "fail"), answers = "n")
expect_identical(
  deploy_run$result$status,
  1L,
  "Bulk deploy should return failure when the user declines retry."
)
expect_identical(
  deploy_run$calls,
  c("alpha", "beta"),
  "Declining retry should not redeploy failed targets."
)
expect_output_contains(
  deploy_run$output,
  c("Failed targets:", "- beta: beta failed on attempt 1"),
  "Failed bulk deploys should print failed target IDs and errors."
)
expect_identical(
  deploy_run$answers,
  1L,
  "Bulk deploy failure should prompt once when the user declines retry."
)

deploy_run <- run_stubbed_deploy(list(beta = c("fail", "success")), answers = "y")
expect_identical(
  deploy_run$result$status,
  0L,
  "Bulk deploy should succeed when failed targets succeed on retry."
)
expect_identical(
  deploy_run$calls,
  c("alpha", "beta", "beta"),
  "Retry should deploy only failed targets."
)
expect_identical(
  length(deploy_run$result$attempts),
  2L,
  "Retry success should record both deployment attempts."
)
expect_identical(
  deploy_run$result$attempts[[1]]$failed_target_ids,
  "beta",
  "First attempt metadata should record failed target IDs."
)
expect_identical(
  deploy_run$result$attempts[[2]]$failed_target_ids,
  character(),
  "Successful retry metadata should have no failed target IDs."
)

deploy_run <- run_stubbed_deploy(list(beta = c("fail", "fail", "success")), answers = c("y", "n"))
expect_identical(
  deploy_run$result$status,
  1L,
  "Repeated retry failure should preserve final failure status when the user stops."
)
expect_identical(
  deploy_run$calls,
  c("alpha", "beta", "beta"),
  "Repeated retry should continue to deploy only the latest failed targets."
)
expect_identical(
  target_ids_from_results(deploy_run$result$failed),
  "beta",
  "Final failed retry metadata should record the remaining failed target."
)
expect_identical(
  deploy_run$answers,
  2L,
  "Repeated retry should prompt after each failed bulk attempt."
)

deploy_run <- run_stubbed_deploy(list(beta = "fail"), stdin_interactive = FALSE)
expect_identical(
  deploy_run$result$status,
  1L,
  "Non-interactive bulk failure should return failure status."
)
expect_identical(
  deploy_run$answers,
  0L,
  "Non-interactive bulk failure should not prompt."
)
expect_true(
  !any(grepl("Retry failed targets", deploy_run$output, fixed = TRUE)),
  "Non-interactive bulk failure output should not include a retry prompt."
)

deploy_run <- run_stubbed_deploy(
  list(alpha = "fail"),
  selected = fixture_targets[1],
  stdin_interactive = TRUE
)
expect_identical(
  deploy_run$result$status,
  1L,
  "Single-target deploy failure should keep failure status."
)
expect_identical(
  deploy_run$answers,
  0L,
  "Single-target deploy failure should not prompt for retry."
)

# Help coverage guard ---------------------------------------------------------

expect_help_options_covered()

message("OK: deploy-app.R CLI help, option parsing, selection, and retry behavior are consistent.")
