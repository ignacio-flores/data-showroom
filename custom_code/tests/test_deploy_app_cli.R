#!/usr/bin/env Rscript

source("tools/viz/entrypoint.R")

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
  normalized <- gsub("^--", "", unlist(list(...)))
  covered_help_options <<- sort(unique(c(covered_help_options, normalized)))
}

usage_options <- function() {
  sort(unique(unname(cli_keyword_aliases)))
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

cover_help_options("help")
help_opts <- parse_args(c("help", "--bogus", "--registry"))
expect_true(
  isTRUE(help_opts$help),
  "help should short-circuit argument validation in parse_args()."
)

help_result <- NULL
help_output <- capture.output({
  help_result <- main(c("help", "--bogus"))
})
expect_identical(
  help_result$status,
  0L,
  "help should return success even when unrelated invalid args follow it."
)
expect_true(
  any(grepl("Usage:", help_output, fixed = TRUE)),
  "--help should print usage text."
)

# Parser/options --------------------------------------------------------------

cover_help_options("target", "profile", "tag", "deploy", "preview", "install", "uninstall", "prefix", "name", "force")
bare_preview <- parse_args(c("target", "alpha"))
expect_true(
  identical(bare_preview$target, "alpha") &&
    isTRUE(bare_preview$preview) &&
    !isTRUE(bare_preview$deploy),
  "Bare target syntax should preview by default."
)

bare_deploy <- parse_args(c("target", "alpha", "deploy"))
expect_true(
  identical(bare_deploy$target, "alpha") &&
    !isTRUE(bare_deploy$preview) &&
    isTRUE(bare_deploy$deploy),
  "Final deploy should switch the action from preview to deployment."
)

expect_error(
  parse_args(c("deploy", "target", "alpha")),
  "deploy must be the final argument",
  "deploy should be rejected unless it is the final argument."
)

install_opts <- parse_args(c("install", "prefix", tempdir(), "name", "data-viz", "force"))
expect_true(
  isTRUE(install_opts$install) &&
    identical(install_opts$install_prefix, tempdir()) &&
    identical(install_opts$install_name, "data-viz") &&
    isTRUE(install_opts$force),
  "install should parse prefix, name, and force options."
)

uninstall_opts <- parse_args(c("uninstall", "--prefix", tempdir(), "--name=data-viz"))
expect_true(
  isTRUE(uninstall_opts$uninstall) &&
    identical(uninstall_opts$install_prefix, tempdir()) &&
    identical(uninstall_opts$install_name, "data-viz"),
  "uninstall should parse dashed prefix and name options."
)

expect_error(
  parse_args(c("prefix", tempdir())),
  "require install or uninstall",
  "prefix should require install or uninstall."
)

expect_error(
  parse_args(c("install", "target", "alpha")),
  "install cannot be used",
  "install should reject preview/deploy selectors."
)

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

cover_help_options("dry-run")
dry_opts <- parse_args(c("--target", "alpha", "--dry-run"))
expect_true(
  isTRUE(dry_opts$dry_run) && !isTRUE(dry_opts$preview),
  "dry-run should parse with an explicit target selector and disable preview."
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

cover_help_options("preview-host", "preview-port", "no-browser", "yes")
preview_opts <- parse_args(c(
  "profile", "main",
  "preview-host", "0.0.0.0",
  "preview-port", "8767",
  "no-browser",
  "yes"
))
expect_true(
  isTRUE(preview_opts$preview) &&
    identical(preview_opts$preview_host, "0.0.0.0") &&
    identical(preview_opts$preview_port, 8767L) &&
    !isTRUE(preview_opts$launch_browser) &&
    isTRUE(preview_opts$yes),
  "Preview options should parse without requiring an explicit preview flag."
)

expect_error(
  parse_args(c("--preview", "--preview-port", "70000")),
  "--preview-port must be an integer",
  "Invalid preview ports should be rejected."
)

expect_error(
  parse_args(c("--target", "alpha", "--all")),
  "all cannot be used with target",
  "all and target should be mutually exclusive."
)

expect_error(
  parse_args(c("--target", "alpha", "--dry-run", "--refresh-data")),
  "dry-run cannot be used",
  "dry-run should not accept refresh/cache flags."
)

no_browser_opts <- parse_args(c("no-browser"))
expect_true(
  isTRUE(no_browser_opts$preview) && !isTRUE(no_browser_opts$launch_browser),
  "Preview-only flags should be valid because preview is the default action."
)

expect_true(
  isTRUE(confirm_large_preview(fixture_targets[1:2], preview_opts, threshold = 1L)),
  "--yes should skip large preview confirmation."
)

cover_help_options("refresh-data", "use-cache")
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
  "refresh-data and use-cache cannot be used together",
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

target_menu_opts <- apply_selector_menu(
  parse_args(c("target")),
  fixture_targets,
  menu_fn = function(prompt, choices, default = 1L) 2L,
  stdin_interactive = TRUE
)
expect_identical(
  target_menu_opts$target,
  "alpha",
  "Bare target without a value should allow interactive target selection."
)
expect_identical(
  target_menu_labels(fixture_targets),
  c("all", "alpha", "beta", "gamma", "disabled"),
  "Target menu labels should be compact target IDs only."
)

all_menu_opts <- apply_selector_menu(
  parse_args(c("target")),
  fixture_targets,
  menu_fn = function(prompt, choices, default = 1L) 1L,
  stdin_interactive = TRUE
)
expect_true(
  isTRUE(all_menu_opts$all),
  "Target menu should include all as the first option."
)

tag_menu_opts <- apply_selector_menu(
  parse_args(c("tag")),
  fixture_targets,
  menu_fn = function(prompt, choices, default = 1L) 2L,
  stdin_interactive = TRUE
)
expect_identical(
  tag_menu_opts$tag,
  "eigt",
  "Bare tag without a value should allow interactive tag selection."
)

cancelled_menu_opts <- apply_selector_menu(
  parse_args(c("target")),
  fixture_targets,
  menu_fn = function(prompt, choices, default = 1L) NA_integer_,
  stdin_interactive = TRUE
)
expect_true(
  isTRUE(cancelled_menu_opts$cancelled),
  "Escape/cancel in a selector menu should mark the run as cancelled."
)

expect_error(
  apply_selector_menu(parse_args(c("target")), fixture_targets, stdin_interactive = FALSE),
  "requires an interactive terminal",
  "Bare target without a value should require an interactive terminal."
)

fake_key_idx <- 0L
fake_keys <- c("down", "enter")
menu_choice <- NULL
invisible(capture.output({
  menu_choice <- terminal_menu(
    "Choose fixture:",
    c("first", "second"),
    stdin_interactive = TRUE,
    read_key = function() {
      fake_key_idx <<- fake_key_idx + 1L
      fake_keys[[fake_key_idx]]
    }
  )
}))
expect_identical(
  menu_choice,
  2L,
  "terminal_menu() should move with arrow keys and select with enter."
)

cancel_choice <- NULL
invisible(capture.output({
  cancel_choice <- terminal_menu(
    "Choose fixture:",
    c("first", "second"),
    stdin_interactive = TRUE,
    read_key = function() "escape"
  )
}))
expect_true(
  is.na(cancel_choice),
  "terminal_menu() should return NA when escape is pressed."
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
expect_identical(
  names(listed),
  c("target_id", "graph", "profile", "enabled", "tags"),
  "list_deploy_targets() should omit app_name and server columns from display output."
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

# Install/uninstall -----------------------------------------------------------

install_prefix <- file.path(tempdir(), sprintf("viz-install-%s", Sys.getpid()))
install_opts <- parse_args(c("install", "prefix", install_prefix))
install_result <- NULL
install_output <- capture.output({
  install_result <- install_viz_command(install_opts)
})
installed_launcher <- file.path(install_prefix, "bin", "viz")
expect_true(
  file.exists(installed_launcher) &&
    isTRUE(file.access(installed_launcher, mode = 1) == 0) &&
    identical(install_result$path, installed_launcher),
  "install should write an executable launcher under prefix/bin."
)
expect_output_contains(
  install_output,
  c("Installed viz", "export PATH="),
  "install should print the installed path and PATH hint."
)
launcher_text <- readLines(installed_launcher, warn = FALSE)
expect_true(
  any(grepl(launcher_marker, launcher_text, fixed = TRUE)) &&
    any(grepl(viz_repo_root, launcher_text, fixed = TRUE)),
  "Installed launcher should contain ownership marker and repo root."
)

launcher_help <- system2(installed_launcher, "help", stdout = TRUE, stderr = TRUE)
expect_true(
  any(grepl("Usage:", launcher_help, fixed = TRUE)),
  "Installed launcher should delegate to bin/viz."
)

launcher_list <- local({
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(tempdir())
  system2(
    installed_launcher,
    c("list", "target", "eigt-kf2"),
    stdout = TRUE,
    stderr = TRUE
  )
})
expect_true(
  any(grepl("eigt-kf2", launcher_list, fixed = TRUE)),
  "Installed launcher should work when called outside the repo root."
)

uninstall_opts <- parse_args(c("uninstall", "prefix", install_prefix))
uninstall_result <- NULL
uninstall_output <- capture.output({
  uninstall_result <- uninstall_viz_command(uninstall_opts)
})
expect_true(
  !file.exists(installed_launcher) &&
    isTRUE(uninstall_result$removed),
  "uninstall should remove a launcher installed by this checkout."
)
expect_output_contains(
  uninstall_output,
  "Removed",
  "uninstall should print the removed launcher path."
)

dir.create(dirname(installed_launcher), recursive = TRUE, showWarnings = FALSE)
writeLines("# unrelated command", installed_launcher)
expect_error(
  install_viz_command(install_opts, quiet = TRUE),
  "already exists",
  "install should refuse to overwrite unrelated commands without force."
)
expect_error(
  uninstall_viz_command(uninstall_opts, quiet = TRUE),
  "was not installed by this checkout",
  "uninstall should refuse to remove unrelated commands without force."
)
force_uninstall_opts <- parse_args(c("uninstall", "prefix", install_prefix, "force"))
invisible(uninstall_viz_command(force_uninstall_opts, quiet = TRUE))
expect_true(
  !file.exists(installed_launcher),
  "force uninstall should remove unrelated commands at the requested path."
)

# Helper wrappers -------------------------------------------------------------

with_mocked_globals(
  list(main = function(args = commandArgs(trailingOnly = TRUE), quiet = FALSE) args),
  {
    deploy_args <- deploy_by_target("alpha")
    parsed_deploy <- parse_args(deploy_args)
    expect_true(
      identical(parsed_deploy$target, "alpha") && isTRUE(parsed_deploy$deploy),
      "deploy_by_target() should append final deploy for actual deployments."
    )

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

  menu_fn <- function(prompt, choices, default = 1L) {
    answer_idx <<- answer_idx + 1L
    if (answer_idx > length(answers)) {
      fail("Unexpected retry prompt.")
    }
    answer <- answers[[answer_idx]]
    if (is.numeric(answer)) {
      return(as.integer(answer))
    }
    match(answer, choices)
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
          menu_fn = menu_fn
        )
      }
    )
  })

  list(result = result, calls = calls, answers = answer_idx, output = output)
}

deploy_run <- run_stubbed_deploy(list(beta = "fail"), answers = 2L)
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

deploy_run <- run_stubbed_deploy(list(beta = c("fail", "success")), answers = 1L)
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

deploy_run <- run_stubbed_deploy(list(beta = c("fail", "fail", "success")), answers = c(1L, 2L))
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

message("OK: viz CLI help, option parsing, selection, and retry behavior are consistent.")
