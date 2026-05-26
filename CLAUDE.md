# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project overview

`landisutils` is an R package that helps prepare data for, run, and
post-process [LANDIS-II](https://www.landis-ii.org/) forest-landscape
simulations. It is part of the FOR-CAST group’s toolchain and depends on
several PredictiveEcology packages (installed from GitHub `@development`
branches — see `Remotes:` in
[DESCRIPTION](https://for-cast.github.io/landisutils/DESCRIPTION)).

## Common commands

R package development is done via `devtools` / `pkgbuild` / `pkgload`:

``` r

devtools::load_all()  # load package for interactive use
devtools::document()  # regenerate NAMESPACE + man/ pages from roxygen
devtools::test()      # run full testthat suite
testthat::test_file("tests/testthat/test-ext_biomass_succession.R")  # run a single test file
devtools::check()     # R CMD check (matches the GitHub Actions workflow)
```

Testthat runs with `parallel: true` (see `Config/testthat/parallel` in
DESCRIPTION).
[tests/testthat.R](https://for-cast.github.io/landisutils/tests/testthat.R)
sets `options(landisutils.cache.path = <tempdir>)` for the suite —
individual tests typically create their own
[`withr::local_tempdir()`](https://withr.r-lib.org/reference/with_tempfile.html).

## Code style

R files are auto-formatted by [Posit
Air](https://posit-dev.github.io/air/) on save (configured in
[.vscode/settings.json](https://for-cast.github.io/landisutils/.vscode/settings.json)
and [air.toml](https://for-cast.github.io/landisutils/air.toml)):
100-col line width, 2-space indent, `skip = ["globalVariables"]`.

## Architecture

The package is organized around three R6 classes plus a family of helper
functions. Understanding these three objects is the fastest way to
orient:

### `LandisExtension` (and its subclasses) — [R/LandisExtension.R](https://for-cast.github.io/landisutils/R/LandisExtension.R)

Base R6 class representing one LANDIS-II extension’s configuration and
input files. Each supported extension is a subclass living in an
`R/ext_*.R` file
(e.g. [R/ext_biomass_succession.R](https://for-cast.github.io/landisutils/R/ext_biomass_succession.R),
[R/ext_dynamic_fire.R](https://for-cast.github.io/landisutils/R/ext_dynamic_fire.R),
[R/ext_original_fire.R](https://for-cast.github.io/landisutils/R/ext_original_fire.R),
[R/ext_social_climate_fire.R](https://for-cast.github.io/landisutils/R/ext_social_climate_fire.R),
[R/ext_forc-succession.R](https://for-cast.github.io/landisutils/R/ext_forc-succession.R),
and the output extensions `ext_output_*.R`).

Each subclass follows a consistent pattern: 1. `initialize()` stashes
parameters into private fields, sets `self$path`, `self$type` (one of
`"succession"`, `"disturbance"`, `"other"` — see `.extTypes`), and the
primary filename in `self$files[1]`. 2. Active bindings
(`active = list(...)`) validate and normalize each parameter on
assignment (e.g. enum checks, `.relPath()` normalization,
[`yesno()`](https://for-cast.github.io/landisutils/reference/yesno.md)
coercion). 3. `$write()` calls a sequence of `insert*()` helpers and
[`writeLines()`](https://rdrr.io/r/base/writeLines.html) the result to
`file.path(self$path, self$files[1])`, then `self$add_file(...)`
registers any auxiliary input files (climate, species, ecoregion, etc.)
so the scenario can collect them.

When adding a new extension, mirror an existing one closely — the
scenario assembler relies on `$type`, `$files[1]`, and the `LandisData`
header being set consistently.

### `LandisScenario` — [R/LandisScenario.R](https://for-cast.github.io/landisutils/R/LandisScenario.R) + [R/scenarios.R](https://for-cast.github.io/landisutils/R/scenarios.R)

Container for a scenario’s path, its list of `LandisExtension` objects,
and the full set of input files.
[`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md)
in [R/scenarios.R](https://for-cast.github.io/landisutils/R/scenarios.R)
is the top-level constructor: it validates inputs, writes the scenario
`.txt` via
[`insertSuccessionExtensions()`](https://for-cast.github.io/landisutils/reference/insertExtensions.md)
/
[`insertDisturbanceExtensions()`](https://for-cast.github.io/landisutils/reference/insertExtensions.md)
/
[`insertOtherExtensions()`](https://for-cast.github.io/landisutils/reference/insertExtensions.md)
(which partition extensions by `$type`), and registers
climate/ecoregion/species/extension files on the returned
`LandisScenario`. The `$replicate(n)` method copies the scenario
directory into `<path>_rep01`, `<path>_rep02`, … for Monte Carlo runs.

### `LandisClimateConfig` — [R/LandisClimateConfig.R](https://for-cast.github.io/landisutils/R/LandisClimateConfig.R) + [R/climate.R](https://for-cast.github.io/landisutils/R/climate.R) + [R/climate_data.R](https://for-cast.github.io/landisutils/R/climate_data.R)

Tracks climate library input files. Built by
[`prepClimateConfig()`](https://for-cast.github.io/landisutils/reference/prepClimateConfig.md).
Climate-data fetching helpers (via `climateR`, `appeears`, `elevatr`)
cache to `getOption("landisutils.cache.path")`, which `.onLoad()`
([R/zzz.R](https://for-cast.github.io/landisutils/R/zzz.R)) initializes
from `.climateCachePath()`.

### The `insert*()` / `prep*()` / helper layer

Two complementary families of free functions do the actual text
generation and data massaging:

- **`prep*()`** functions (e.g. `prepInitialCommunities`,
  `prepSpeciesData`, `prepEcoregionParameters`,
  `prepFireReductionParameters`, `prepClimateConfig`) take R-side data
  (often `data.table`/`data.frame` from upstream `LandR` /
  `Biomass_borealDataPrep` workflows) and convert to LANDIS-II file
  formats on disk, returning file paths or data frames ready for
  extensions.
- **`insert*()`** functions (mostly `@keywords internal`, in
  [R/ext_utils.R](https://for-cast.github.io/landisutils/R/ext_utils.R),
  [R/utils.R](https://for-cast.github.io/landisutils/R/utils.R),
  [R/scenarios.R](https://for-cast.github.io/landisutils/R/scenarios.R),
  and each `ext_*.R`) return character vectors of LANDIS-II-formatted
  text lines that the extension’s `$write()` concatenates. Foundational
  primitives live in
  [R/utils.R](https://for-cast.github.io/landisutils/R/utils.R):
  [`insertLandisData()`](https://for-cast.github.io/landisutils/reference/insertLandisData.md)
  (writes the header + `LandisData` line),
  [`insertValue()`](https://for-cast.github.io/landisutils/reference/insertValue.md),
  [`insertFile()`](https://for-cast.github.io/landisutils/reference/insertFile.md),
  plus
  [`yesno()`](https://for-cast.github.io/landisutils/reference/yesno.md)
  for logical → `"yes"`/`"no"` conversion and `.relPath()` /
  `.checkPath()` for path handling.

All generated files start with `landisutilsHeader()` warning that they
are auto-generated.

### Running simulations — [R/landis.R](https://for-cast.github.io/landisutils/R/landis.R)

[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md)
locates the LANDIS-II console via the `LANDIS_CONSOLE` env var.
`landis_run(scenario, rep, landis_console)` spawns a background R
process ([`callr::r_bg`](https://callr.r-lib.org/reference/r_bg.html))
that `cd`s into the scenario directory and runs
`dotnet <Landis.Console.dll> <scenario_file>`, writing stdout/stderr to
`<scenario_path>/log/`.

### Integration testing — [inst/integration-tests/](https://for-cast.github.io/landisutils/inst/integration-tests/)

A parser-level harness that runs package-generated scenarios through the
official LANDIS-II v8 console to validate config-file syntax beyond what
`R CMD check` and unit tests can catch. Two files:

- [inst/integration-tests/build_scenarios.R](https://for-cast.github.io/landisutils/inst/integration-tests/build_scenarios.R)
  — `Rscript build_scenarios.R <output-dir>`. Downloads upstream Core8
  reference inputs (initial communities, ecoregions, species, climate)
  and uses the package API (`BiomassSuccession$new()`,
  [`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md),
  `LandisClimateConfig`, etc.) to (re)generate the scenario `.txt` and
  per-extension config files on top of that data. Prints absolute
  scenario paths to stdout (one per line) so the caller can iterate.
- [inst/integration-tests/run_in_docker.sh](https://for-cast.github.io/landisutils/inst/integration-tests/run_in_docker.sh)
  — runs `dotnet <Landis.Console.dll> <scenario_file>` inside an
  ephemeral container of `LANDIS_IMAGE` (default
  `ghcr.io/landis-ii-foundation/landis-ii-v8-release:main`).

[.github/workflows/landis-integration.yaml](https://for-cast.github.io/landisutils/.github/workflows/landis-integration.yaml)
wires these together on push to `main`/`master` and on every PR;
scenario directories are uploaded as a 7-day artifact regardless of
pass/fail.

**Extension coverage in the Docker image** — `landis-ii-v8-release`
ships a *curated subset* of extensions: some are not registered in the
image (parsing aborts with “No extension with the name …”) and others
parse cleanly but need richer fixtures to actually run. See the comment
block at
[build_scenarios.R:162-192](https://for-cast.github.io/landisutils/inst/integration-tests/build_scenarios.R)
for the current list and rationale, and mirror that comment when you
omit a new extension. When adding a new `ext_*.R` extension, also add a
scenario block in `build_scenarios.R` covering it — unless it falls into
one of those omission categories.

## Conventions worth knowing

- **Relative paths in config files**: all paths written into LANDIS-II
  input files must be relative to the scenario/extension directory.
  Active bindings typically normalize incoming paths via
  `.relPath(value, self$path)` — preserve this when adding fields.
- **`data.table` globals**: any new symbol used in `data.table`
  non-standard evaluation needs to be listed in the
  [`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
  call in
  [R/landisutils-package.R](https://for-cast.github.io/landisutils/R/landisutils-package.R)
  (or a local call at the top of the file, as in
  [R/ext_forc-succession.R](https://for-cast.github.io/landisutils/R/ext_forc-succession.R))
  to avoid `R CMD check` NOTEs.
- **Collate order**:
  [DESCRIPTION](https://for-cast.github.io/landisutils/DESCRIPTION) has
  an explicit `Collate:` field because several `ext_*.R` files use
  `@include ext_utils.R`. Running `devtools::document()` keeps this in
  sync — don’t hand-edit.
- **Roxygen templates** live in
  [man-roxygen/](https://for-cast.github.io/landisutils/man-roxygen/)
  and are referenced via `@template param_path`,
  `@template return_file`, etc.
- **Spelling**: the `spelling` package is used (see
  [tests/spelling.R](https://for-cast.github.io/landisutils/tests/spelling.R));
  add new domain terms to
  [inst/WORDLIST](https://for-cast.github.io/landisutils/inst/WORDLIST)
  rather than suppressing the check.
- **Extension test references**: every `ext_*.R` extension must have a
  corresponding `tests/testthat/test-ext_*.R` file whose reference
  values come from the upstream LANDIS-II repo’s Core8 (v8) test input —
  i.e. its `testings/Core8-*` (or `testing/Core8-*`) directory on
  GitHub. Cite the specific path and URL in a `## NOTE:` comment at the
  top of the test, and prefer values copied verbatim from that input
  over made-up examples. PnET-Succession is the exception: it has no
  `testings/` directory upstream, so its test references
  `deploy/examples/biomass-Pnet-succession-example-v8` instead.
- **Config-syntax claims**: when a PDF user guide and the extension’s
  `src/` parser (e.g. `*Parser.cs`, `MapFileNames.cs`) or its `Core8-*`
  test inputs disagree about a placeholder/keyword/parameter name, trust
  the parser + Core8 — the PDFs are sometimes stale (e.g. the
  `Extension-Output-Wildlife-Habitat` v3 PDF documents `{wildlife-name}`
  but the parser and Core8 input both use `{wildlifeName}`).
- **Succession-backend coupling**: a LANDIS-II scenario uses **exactly
  one** succession extension, and a few disturbance/output extensions
  are tied to a specific backend. **Root Rot** and **PnET Output**
  (`OutputBiomassPnET`) consume PnET-specific cohort state (NSC, foliage
  stack, water balance) and only work with **PnET Succession** — these
  three are always used together as a stack. **Biomass Succession**
  cannot be paired with Root Rot or PnET Output. All other
  disturbance/output extensions in this package are succession-agnostic.
  The package does not enforce this; check it when assembling scenarios,
  vignettes, or integration tests.
- **Extension ↔︎ helper cross-references**: every helper for an extension
  (the R6 class itself, plus all of its `prep*()`, `insert*()`,
  `default*()`, and sub-object constructors) must be tagged with the
  same `@family <Extension> helpers` string. roxygen2 then
  auto-generates an “Other `<Extension>` helpers:” subsection in every
  member’s rendered `\seealso{}` block — this is the bidirectional
  cross-reference graph and the primary discoverability mechanism for
  users.
  - *Naming*: human-readable, suffix `helpers`
    (e.g. `@family Climate BDA helpers`,
    `@family Land Use Plus helpers`,
    `@family Biomass Succession helpers`). The exact string must match
    across members or the family splits. Helpers genuinely shared
    between extensions
    (e.g. [`suitabilityFile()`](https://for-cast.github.io/landisutils/reference/suitabilityFile.md),
    [`prepTopographyFile()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md))
    get one `@family` tag per consuming extension.
  - *When to use `@seealso` instead*: free-form prose and
    **cross-extension** references that don’t belong in a single family
    — e.g. an R6 class’s curated list of shared scenario inputs
    (`[prepClimateConfig()]`, `[prepInitialCommunities()]`,
    `[prepSpeciesData()]`).
  - *`@rdname` caveat*: when functions share an `@rdname`
    (e.g. `prepGroundSlopeFile` / `prepUphillAzimuthMap` are aliased
    into `prepTopographyFile.Rd`), put the `@family` tag **only on the
    primary** function — alias members would otherwise duplicate the
    family list inside the shared Rd’s `\seealso{}` block. After adding
    or renaming helpers, run `devtools::document()` to regenerate the
    affected `.Rd` files.
