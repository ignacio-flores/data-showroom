usage <- function() {
  cat(
    paste(
      "Usage:",
      "  viz target [<id>[, <id>...]] [options] [deploy]",
      "  viz tag [<tag>[, <tag>...]] [options] [deploy]",
      "  viz profile <profile>[, <profile>...] [options] [deploy]",
      "  viz all [options] [deploy]",
      "  viz list [target <id> | profile <profile> | tag <tag>] [include-disabled]",
      "  viz install [prefix <path>] [name <command>] [force]",
      "  viz uninstall [prefix <path>] [name <command>] [force]",
      "",
      "Actions:",
      "  preview               Default action; builds bundles and runs locally.",
      "  deploy                Deploy selected app(s); must be the final argument.",
      "  list                  Print matching targets and exit.",
      "  dry-run               Print selected targets, data actions, and bundle contents.",
      "  install               Install a local launcher for this checkout.",
      "  uninstall             Remove a launcher installed by this checkout.",
      "",
      "Selectors:",
      "  target <id>           Target IDs (comma-separated, optional spaces, or repeated).",
      "  profile <name>        Filter targets by profile/account.",
      "  tag <tag>             Filter targets by tags.",
      "  all                   Select all enabled targets (can combine with profile/tag).",
      "",
      "Options:",
      "  include-disabled      Include disabled targets when used with list.",
      "  registry <path>       Path to deployment registry YAML.",
      "  data-sources <path>   Path to data-source manifest YAML.",
      "  source-root <path>    Override the configured data source root for this run.",
      "  refresh-data          Refresh stale cached files/artifacts without prompting.",
      "  use-cache             Use stale cached files/artifacts without prompting.",
      "  preview-host <host>   Host for local preview (default: 127.0.0.1).",
      "  preview-port <port>   Port for one preview; bulk previews use ports at/above it.",
      "  no-browser            Do not open preview URLs in browser tabs.",
      "  yes                   Skip the confirmation prompt for large bulk previews.",
      "  prefix <path>         Install/uninstall prefix (default: ~/.local).",
      "  name <command>        Installed command name (default: viz).",
      "  force                 Overwrite/remove an unrelated installed launcher.",
      "  help                  Show this message.",
      "",
      "All options also accept --, for example --target, --tag, and --preview-port.",
      "",
      "Examples:",
      "  viz target eigt-kf2",
      "  viz target eigt-kf2,eigt-wm2",
      "  viz tag topo no-browser",
      "  viz profile hubquin tag eigt dry-run",
      "  viz target eigt-kf2 deploy",
      "  viz tag topo deploy",
      "  viz install",
      "  viz uninstall",
      sep = "\n"
    ),
    "\n",
    sep = ""
  )
}

cli_keyword_aliases <- c(
  h = "help",
  help = "help",
  preview = "preview",
  deploy = "deploy",
  install = "install",
  uninstall = "uninstall",
  list = "list",
  ls = "list",
  target = "target",
  profile = "profile",
  tag = "tag",
  all = "all",
  "include-disabled" = "include-disabled",
  "dry-run" = "dry-run",
  dryrun = "dry-run",
  "no-browser" = "no-browser",
  yes = "yes",
  "refresh-data" = "refresh-data",
  "use-cache" = "use-cache",
  registry = "registry",
  "data-sources" = "data-sources",
  "source-root" = "source-root",
  "preview-host" = "preview-host",
  "preview-port" = "preview-port",
  prefix = "prefix",
  name = "name",
  force = "force"
)

strip_option_prefix <- function(arg) {
  sub("^--", "", as.character(arg))
}

option_key <- function(arg) {
  key <- strip_option_prefix(arg)
  key <- sub("=.*$", "", key)
  if (key %in% names(cli_keyword_aliases)) cli_keyword_aliases[[key]] else key
}

option_label <- function(key) {
  sprintf("--%s", key)
}

is_cli_keyword <- function(arg) {
  option_key(arg) %in% unname(cli_keyword_aliases)
}

starts_with_option_value <- function(arg, key) {
  startsWith(as.character(arg), paste0("--", key, "=")) ||
    startsWith(as.character(arg), paste0(key, "="))
}

option_value_from_equals <- function(arg, key) {
  sub(sprintf("^(--)?%s=", key), "", as.character(arg))
}

new_parse_opts <- function() {
  list(
    target = character(),
    profile = character(),
    tag = character(),
    list = FALSE,
    all = FALSE,
    include_disabled = FALSE,
    dry_run = FALSE,
    preview = TRUE,
    preview_supplied = FALSE,
    deploy = FALSE,
    install = FALSE,
    uninstall = FALSE,
    install_prefix = default_install_prefix(),
    install_prefix_supplied = FALSE,
    install_name = "viz",
    install_name_supplied = FALSE,
    force = FALSE,
    selector_menu = NULL,
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
}

parse_args <- function(args) {
  opts <- new_parse_opts()
  args <- as.character(args)

  if (any(vapply(args, function(arg) identical(option_key(arg), "help"), logical(1)))) {
    opts$help <- TRUE
    return(opts)
  }

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    key <- option_key(arg)

    consume_value <- function(name) {
      if (starts_with_option_value(arg, name)) {
        value <- option_value_from_equals(arg, name)
        if (!nzchar(trimws(value))) {
          usage_error(sprintf("Missing value for %s", option_label(name)))
        }
        return(list(value = trimws(value), next_i = i + 1L))
      }
      if (identical(key, name)) {
        if (i == length(args) || is_cli_keyword(args[[i + 1L]])) {
          usage_error(sprintf("Missing value for %s", option_label(name)))
        }
        value <- args[[i + 1L]]
        if (!nzchar(trimws(value))) {
          usage_error(sprintf("Missing value for %s", option_label(name)))
        }
        return(list(value = trimws(value), next_i = i + 2L))
      }
      NULL
    }

    consume_selector <- function(name) {
      if (starts_with_option_value(arg, name)) {
        tokens <- option_value_from_equals(arg, name)
        dashed <- startsWith(arg, "--")
        next_i <- i + 1L
      } else if (identical(key, name)) {
        tokens <- character()
        dashed <- startsWith(arg, "--")
        next_i <- i + 1L
      } else {
        return(NULL)
      }

      while (next_i <= length(args) && !is_cli_keyword(args[[next_i]])) {
        tokens <- c(tokens, args[[next_i]])
        next_i <- next_i + 1L
      }

      if (!length(tokens)) {
        if (isTRUE(dashed)) {
          usage_error(sprintf("Missing value for %s", option_label(name)))
        }
        return(list(value = character(), menu = name, next_i = next_i))
      }

      list(
        value = normalize_selector_values(tokens, option_label(name), allow_vector = FALSE),
        menu = NULL,
        next_i = next_i
      )
    }

    if (identical(key, "deploy")) {
      if (i != length(args)) {
        usage_error("deploy must be the final argument.")
      }
      opts$deploy <- TRUE
      opts$preview <- FALSE
      i <- i + 1L
      next
    } else if (identical(key, "install")) {
      opts$install <- TRUE
      opts$preview <- FALSE
      i <- i + 1L
      next
    } else if (identical(key, "uninstall")) {
      opts$uninstall <- TRUE
      opts$preview <- FALSE
      i <- i + 1L
      next
    } else if (identical(key, "preview")) {
      opts$preview <- TRUE
      opts$preview_supplied <- TRUE
      i <- i + 1L
      next
    } else if (identical(key, "list")) {
      opts$list <- TRUE
      opts$preview <- FALSE
      i <- i + 1L
      next
    } else if (identical(key, "all")) {
      opts$all <- TRUE
      i <- i + 1L
      next
    } else if (identical(key, "include-disabled")) {
      opts$include_disabled <- TRUE
      i <- i + 1L
      next
    } else if (identical(key, "dry-run")) {
      opts$dry_run <- TRUE
      opts$preview <- FALSE
      i <- i + 1L
      next
    } else if (identical(key, "no-browser")) {
      opts$launch_browser <- FALSE
      i <- i + 1L
      next
    } else if (identical(key, "yes")) {
      opts$yes <- TRUE
      i <- i + 1L
      next
    } else if (identical(key, "refresh-data")) {
      opts$refresh_data <- TRUE
      i <- i + 1L
      next
    } else if (identical(key, "use-cache")) {
      opts$use_cache <- TRUE
      i <- i + 1L
      next
    } else if (identical(key, "force")) {
      opts$force <- TRUE
      i <- i + 1L
      next
    }

    target_value <- consume_selector("target")
    if (!is.null(target_value)) {
      opts$target <- c(opts$target, target_value$value)
      opts$selector_menu <- target_value$menu %||% opts$selector_menu
      i <- target_value$next_i
      next
    }

    profile_value <- consume_selector("profile")
    if (!is.null(profile_value)) {
      opts$profile <- c(opts$profile, profile_value$value)
      opts$selector_menu <- profile_value$menu %||% opts$selector_menu
      i <- profile_value$next_i
      next
    }

    tag_value <- consume_selector("tag")
    if (!is.null(tag_value)) {
      opts$tag <- c(opts$tag, tag_value$value)
      opts$selector_menu <- tag_value$menu %||% opts$selector_menu
      i <- tag_value$next_i
      next
    }

    registry_value <- consume_value("registry")
    if (!is.null(registry_value)) {
      opts$registry <- registry_value$value
      i <- registry_value$next_i
      next
    }

    data_sources_value <- consume_value("data-sources")
    if (!is.null(data_sources_value)) {
      opts$data_sources <- data_sources_value$value
      opts$data_sources_supplied <- TRUE
      i <- data_sources_value$next_i
      next
    }

    source_root_value <- consume_value("source-root")
    if (!is.null(source_root_value)) {
      opts$source_root <- source_root_value$value
      opts$source_root_supplied <- TRUE
      i <- source_root_value$next_i
      next
    }

    preview_host_value <- consume_value("preview-host")
    if (!is.null(preview_host_value)) {
      opts$preview_host <- preview_host_value$value
      i <- preview_host_value$next_i
      next
    }

    preview_port_value <- consume_value("preview-port")
    if (!is.null(preview_port_value)) {
      opts$preview_port <- parse_port(preview_port_value$value)
      i <- preview_port_value$next_i
      next
    }

    prefix_value <- consume_value("prefix")
    if (!is.null(prefix_value)) {
      opts$install_prefix <- prefix_value$value
      opts$install_prefix_supplied <- TRUE
      i <- prefix_value$next_i
      next
    }

    name_value <- consume_value("name")
    if (!is.null(name_value)) {
      opts$install_name <- validate_command_name(name_value$value)
      opts$install_name_supplied <- TRUE
      i <- name_value$next_i
      next
    }

    usage_error(sprintf("Unknown argument: %s", arg))
  }

  preview_option_used <- !identical(opts$preview_host, "127.0.0.1") ||
    !is.null(opts$preview_port) ||
    !isTRUE(opts$launch_browser) ||
    isTRUE(opts$yes)

  install_action <- isTRUE(opts$install) || isTRUE(opts$uninstall)

  if (isTRUE(opts$install) && isTRUE(opts$uninstall)) {
    usage_error("install and uninstall cannot be used together.")
  }

  install_option_used <- isTRUE(opts$install_prefix_supplied) ||
    isTRUE(opts$install_name_supplied) ||
    isTRUE(opts$force)

  if (!isTRUE(install_action) && isTRUE(install_option_used)) {
    usage_error("prefix, name, and force require install or uninstall.")
  }

  if (isTRUE(install_action)) {
    conflicts <- character()
    if (length(opts$target)) conflicts <- c(conflicts, "target")
    if (length(opts$profile)) conflicts <- c(conflicts, "profile")
    if (length(opts$tag)) conflicts <- c(conflicts, "tag")
    if (isTRUE(opts$all)) conflicts <- c(conflicts, "all")
    if (isTRUE(opts$list)) conflicts <- c(conflicts, "list")
    if (isTRUE(opts$dry_run)) conflicts <- c(conflicts, "dry-run")
    if (isTRUE(opts$deploy)) conflicts <- c(conflicts, "deploy")
    if (isTRUE(opts$preview_supplied)) conflicts <- c(conflicts, "preview")
    if (isTRUE(preview_option_used)) conflicts <- c(conflicts, "preview options")
    if (isTRUE(opts$include_disabled)) conflicts <- c(conflicts, "include-disabled")
    if (isTRUE(opts$refresh_data)) conflicts <- c(conflicts, "refresh-data")
    if (isTRUE(opts$use_cache)) conflicts <- c(conflicts, "use-cache")
    if (isTRUE(opts$data_sources_supplied)) conflicts <- c(conflicts, "data-sources")
    if (isTRUE(opts$source_root_supplied)) conflicts <- c(conflicts, "source-root")
    if (!is.null(opts$selector_menu)) conflicts <- c(conflicts, opts$selector_menu)
    if (length(conflicts)) {
      usage_error(sprintf(
        "%s cannot be used with: %s",
        if (isTRUE(opts$install)) "install" else "uninstall",
        paste(conflicts, collapse = ", ")
      ))
    }
  }

  if (isTRUE(opts$deploy) && isTRUE(opts$dry_run)) {
    usage_error("deploy cannot be used with dry-run.")
  }

  if (isTRUE(opts$deploy) && (isTRUE(opts$preview_supplied) || isTRUE(preview_option_used))) {
    usage_error("deploy cannot be used with preview options.")
  }

  if (isTRUE(opts$dry_run) && isTRUE(opts$preview_supplied)) {
    usage_error("dry-run and preview cannot be used together.")
  }

  if (isTRUE(opts$dry_run) && (isTRUE(opts$refresh_data) || isTRUE(opts$use_cache))) {
    usage_error("dry-run cannot be used with refresh-data or use-cache.")
  }

  if (isTRUE(opts$all) && length(opts$target)) {
    usage_error("all cannot be used with target. Use all with profile/tag, or remove all.")
  }

  if (isTRUE(opts$include_disabled) && !isTRUE(opts$list)) {
    usage_error("include-disabled requires list.")
  }

  if (isTRUE(opts$list)) {
    list_conflicts <- character()
    if (isTRUE(opts$dry_run)) list_conflicts <- c(list_conflicts, "dry-run")
    if (isTRUE(opts$preview_supplied)) list_conflicts <- c(list_conflicts, "preview")
    if (isTRUE(opts$deploy)) list_conflicts <- c(list_conflicts, "deploy")
    if (isTRUE(preview_option_used)) list_conflicts <- c(list_conflicts, "preview options")
    if (isTRUE(opts$refresh_data)) list_conflicts <- c(list_conflicts, "refresh-data")
    if (isTRUE(opts$use_cache)) list_conflicts <- c(list_conflicts, "use-cache")
    if (isTRUE(opts$data_sources_supplied)) list_conflicts <- c(list_conflicts, "data-sources")
    if (isTRUE(opts$source_root_supplied)) list_conflicts <- c(list_conflicts, "source-root")
    if (length(list_conflicts)) {
      usage_error(sprintf("list cannot be used with: %s", paste(list_conflicts, collapse = ", ")))
    }
  }

  if (isTRUE(opts$refresh_data) && isTRUE(opts$use_cache)) {
    usage_error("refresh-data and use-cache cannot be used together.")
  }

  opts$target <- unique(opts$target)
  opts$profile <- unique(opts$profile)
  opts$tag <- unique(opts$tag)
  opts
}
