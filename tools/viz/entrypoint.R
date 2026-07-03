entrypoint_path <- getOption("viz.entrypoint", default = NULL)
if (is.null(entrypoint_path) || !nzchar(entrypoint_path)) {
  candidates <- c(
    sys.frame(1)$ofile,
    file.path(getwd(), "tools", "viz", "entrypoint.R")
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  entrypoint_path <- candidates[vapply(candidates, function(path) {
    file.exists(file.path(dirname(path), "core.R"))
  }, logical(1))][[1]]
}
viz_tool_dir <- dirname(normalizePath(entrypoint_path, mustWork = TRUE))
viz_repo_root <- normalizePath(file.path(viz_tool_dir, "..", ".."), mustWork = FALSE)

viz_source <- function(path) {
  source(file.path(viz_tool_dir, path), chdir = TRUE)
}

viz_source("core.R")
viz_source("prompts.R")
viz_source("cli.R")
viz_source("registry.R")
viz_source("data_sources.R")
viz_source("bundles.R")
viz_source("deploy.R")
viz_source("preview.R")
viz_source("install.R")
viz_source("main.R")
