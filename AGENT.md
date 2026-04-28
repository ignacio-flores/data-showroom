# AGENT.md

This repository is a configurable Shiny dashboard system. Treat the YAML presets
and shared modules as a compatibility-sensitive surface: one change can affect
many dashboards, plot types, data schemas, and embedded workflows.

## Core Principles

- Make small, scoped changes. Avoid broad refactors unless explicitly requested.
- Preserve backwards compatibility for existing YAML presets whenever possible.
- Treat files in `yaml/` as the app's configuration API. Do not rename or remove
  config keys without updating all affected presets and documentation.
- Before editing shared modules, identify which plot modes or workflows may be
  affected.
- Do not revert user edits or local generated files unless explicitly asked.

## Runtime Architecture

- `app.R` chooses a graph key and loads `yaml/config_<graph>.yaml`.
- `modules/load_config.R` reads YAML and assigns matching `createViz()`
  arguments into the global environment.
- `modules/createViz.R` loads data, runs optional wranglers, builds UI/server
  logic, and wires modules together.
- `modules/PlotServer.R` handles plot rendering branches for line, point, area,
  step, map, bar/hbar animation, dual-axis, faceted, and dynamic scatter charts.
- `custom_code/` contains dataset-specific wrangling. Prefer keeping
  dataset-specific transformations there rather than hard-coding them into shared
  modules.

## YAML Compatibility

When changing behavior, check whether the change affects these keys:

- `data.file`, `meta.file`, `data.wrangler`, `keep.col`
- `gopts`
- `axis_vars`
- `color`
- `fixed_selectors`, `loose_selectors`
- `facet_var`, `extra_layer`
- `tooltip_vars`, `dt.cols`
- `table.display`, `download.button`
- `hide.selectors`, `listen`
- `meta.loc`
- `area_stack_toggle`, `area_stack_default`
- `scatter_options`

Add backwards-compatible defaults for new options.

## Plot Coverage Expectations

For shared plotting/filtering changes, consider representative presets:

- Area/facet/extra layer: `topo_single`, `topo_multi`, `topo_source`
- Line/point: `ineq_single`, `inhe_single`, `topo_aba1`
- Embedded hidden-selector mode: `topo_prev`, `ineq_prev`, `inhe_prev`
- Map: `eigt_wm1`
- Animated horizontal bar: `eigt_wm2`
- Step plot: `eigt_ft1` or `eigt_ft2`
- Dual-axis line: `eigt_kf1`
- Dynamic scatter: `eigt_kf3`

Some presets require ignored local data files and may not run in every checkout.
If data is missing, report that clearly.

## Data Handling

- Do not commit large warehouse files, credentials, or generated local data unless
  explicitly requested.
- Respect `.gitignore`, especially `auth/`, `active_graph.txt`, and ignored
  warehouse data under `data/`.
- Avoid loading huge datasets unnecessarily. Preserve column-subset loading
  behavior in `modules/read_dataset.R`.
- Do not mutate source data files as part of app fixes unless the task is
  explicitly about data generation.

## Selectors and Embedded Workflows

- Be careful with selector logic. Fixed selectors, loose selectors, axis
  selectors, and external JavaScript updates interact.
- Preserve hidden-selector behavior for embedded dashboards.
- Preserve `listen` message behavior unless explicitly changing embed
  integration.
- Changes involving `GEO_long`, `geo_long`, `year`, or axis choices should be
  checked against multiple presets because naming conventions vary.

## Verification

There is no formal test suite or dependency lockfile yet. For non-trivial
changes:

- Run basic R syntax/source checks where possible.
- Run the app with at least one affected preset when local data is available.
- For shared module changes, manually reason through the representative preset
  matrix above.
- Note any preset that could not be tested because required local data is
  missing.

## Dependencies

- Do not introduce new R package dependencies without a clear need.
- If adding a dependency, update README dependency notes.
- Prefer existing packages and local module patterns.

## Deployment

- Do not edit or commit files in `auth/`.
- Use `deploy-app.R --dry-run` before deployment changes.
- Keep `yaml/deploy_targets.yaml` consistent with available config files.

## Documentation

- Update README or YAML comments when adding new config keys or changing expected
  behavior.
- Keep examples config-driven where possible.
