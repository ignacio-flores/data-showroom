default_install_prefix <- function() {
  file.path(path.expand("~"), ".local")
}

validate_command_name <- function(name) {
  name <- as.character(name)
  if (length(name) != 1L || !nzchar(name) || grepl("[/\\\\]", name)) {
    usage_error("name must be a single command name without path separators.")
  }
  name
}

install_bin_dir <- function(prefix) {
  file.path(normalizePath(path.expand(prefix), mustWork = FALSE), "bin")
}

install_launcher_path <- function(prefix, name) {
  file.path(install_bin_dir(prefix), validate_command_name(name))
}

launcher_marker <- "data-showroom viz launcher"

launcher_lines <- function(repo_root) {
  c(
    "#!/bin/sh",
    sprintf("# %s", launcher_marker),
    sprintf("# data-showroom-viz-root: %s", repo_root),
    sprintf("REPO_ROOT=%s", shQuote(repo_root)),
    "if [ ! -x \"$REPO_ROOT/bin/viz\" ]; then",
    "  echo \"data-showroom viz is not available at $REPO_ROOT. Reinstall from the current checkout.\" >&2",
    "  exit 1",
    "fi",
    "exec Rscript \"$REPO_ROOT/bin/viz\" \"$@\""
  )
}

read_launcher_lines <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }
  readLines(path, warn = FALSE)
}

launcher_owned_by_repo <- function(path, repo_root = viz_repo_root) {
  lines <- read_launcher_lines(path)
  if (!length(lines)) {
    return(FALSE)
  }
  has_marker <- any(grepl(launcher_marker, lines, fixed = TRUE))
  has_root <- any(grepl(sprintf("data-showroom-viz-root: %s", repo_root), lines, fixed = TRUE))
  has_marker && has_root
}

path_contains <- function(dir, path_env = Sys.getenv("PATH", unset = "")) {
  if (!nzchar(path_env)) {
    return(FALSE)
  }
  entries <- strsplit(path_env, .Platform$path.sep, fixed = TRUE)[[1]]
  entries <- entries[nzchar(entries)]
  any(normalizePath(path.expand(entries), mustWork = FALSE) %in% normalizePath(path.expand(dir), mustWork = FALSE))
}

install_viz_command <- function(opts, quiet = FALSE) {
  name <- validate_command_name(opts$install_name)
  bin_dir <- install_bin_dir(opts$install_prefix)
  launcher <- install_launcher_path(opts$install_prefix, name)

  if (file.exists(launcher) && !launcher_owned_by_repo(launcher) && !isTRUE(opts$force)) {
    usage_error(sprintf(
      "%s already exists and was not installed by this checkout. Re-run with force to overwrite it.",
      launcher
    ))
  }

  dir.create(bin_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(launcher_lines(viz_repo_root), launcher)
  Sys.chmod(launcher, mode = "0755")

  if (!isTRUE(quiet)) {
    cat(sprintf("Installed %s -> %s\n", name, launcher))
    if (!path_contains(bin_dir)) {
      cat(sprintf("Add this to your shell profile if needed:\n  export PATH=\"%s:$PATH\"\n", bin_dir))
    }
  }

  invisible(list(status = 0L, path = launcher, bin_dir = bin_dir, name = name))
}

uninstall_viz_command <- function(opts, quiet = FALSE) {
  name <- validate_command_name(opts$install_name)
  launcher <- install_launcher_path(opts$install_prefix, name)

  if (!file.exists(launcher)) {
    if (!isTRUE(quiet)) {
      cat(sprintf("No installed %s launcher found at %s\n", name, launcher))
    }
    return(invisible(list(status = 0L, path = launcher, removed = FALSE)))
  }

  if (!launcher_owned_by_repo(launcher) && !isTRUE(opts$force)) {
    usage_error(sprintf(
      "%s was not installed by this checkout. Re-run with force to remove it.",
      launcher
    ))
  }

  unlink(launcher)
  if (file.exists(launcher)) {
    stop(sprintf("Failed to remove %s.", launcher))
  }

  if (!isTRUE(quiet)) {
    cat(sprintf("Removed %s\n", launcher))
  }

  invisible(list(status = 0L, path = launcher, removed = TRUE))
}
