# landisutils 0.0.16

## Vegetation dynamics: species biomass and transition plots

* New `read_biomass_c_snapshots(paths, times, run_name)` reads ForCS
  `log_BiomassC.csv` files (per-cohort, per-cell) for one or more replicates,
  filtering to requested snapshot years via `arrow::open_dataset()` lazy
  streaming so that multi-GB files never need to be fully materialised in R.
  ForCS writes `log_BiomassC.csv` unconditionally, so no additional output
  extension is required.
* New `read_biomass_output_rasters(dirs, times, species, live_map_pattern,
  run_name)` is the succession-agnostic alternative: reads per-species biomass
  rasters written by the Output.Biomass v4 extension (present in the
  `landis-ii-v8-release` Docker image). Works with any succession extension;
  requires Output.Biomass to be included in the scenario configuration.
* Both readers return an identical `data.table` schema
  (`scenario, replicate, Time, row, column, [ecoregion,] species, biomass` in
  Mg C ha^-1), so all downstream functions are source-agnostic.
* New `biomass_landscape_summary(df)` aggregates per-cell snapshot data to
  landscape-mean ± SD biomass by species per timestep.
* New `leading_species(df)` labels each cell at each snapshot by the species
  with the highest total live biomass. Ties broken alphabetically.
* New `community_label(df, n_spp, min_pct)` labels each cell by its top-`n`
  species combination (e.g. `"Hw-Sx"`); species below `min_pct` of cell total
  are dropped; zero-biomass cells are labelled `"Non-vegetated"`.
* New `transition_data(label_df, times)` builds the lodes-form `tibble`
  required by `ggalluvial`: unique label-path combinations across all snapshot
  years, with cell counts averaged across replicates.
* New `plot_species_biomass(summary_df, colours, title)` produces a stacked
  area chart of landscape-mean biomass by species over time.
* New `plot_transitions(lodes_df, colours, title)` produces a Sankey-style
  alluvial diagram (via `ggalluvial`) showing how cells move between vegetation
  types across snapshot years.
* `arrow`, `ggalluvial`, `ggplot2`, and `purrr` added to `Imports`
  (previously absent or in `Suggests`).

# landisutils 0.0.15

## `tar_landis()` / `landis_run_docker()` fixes

* `landis_run_docker()`: container names now include the calling process PID and
  a random integer suffix to prevent name collisions when multiple LANDIS
  replicates run simultaneously.
* `tar_landis()`: dependency file lists are now deduplicated before being passed
  to `landis_replicate()`. Paths are normalised to absolute form, filtered to
  existing files, then deduplicated by basename; scenario-specific files
  (under the scenario directory) take priority over cross-scenario duplicates.
  Applies to both the Docker and local run paths.
* `tar_landis()`: replicates where LANDIS-II already completed successfully are
  now skipped (idempotent re-run). Completion is detected by the presence of
  "Model run is complete" in `Landis-log.txt`. Applies to both the Docker and
  local run paths.

# landisutils 0.0.14

## ForCS Succession extension

* `insertDOMPools()` now wraps multi-word pool names in double-quotes, matching
  the LANDIS-II parser's requirement (e.g. `"Fast AG"` instead of `Fast AG`).
* ForCS v4 changed four large parameter tables from inline text to CSV file
  references. `insertEcoSppDOMParameters()`, `insertANPPTimeSeries()`,
  `insertMaxBiomassTimeSeries()`, and `insertEstablishProbabilities()` now each
  write a CSV to `path` and emit the `Keyword "filename"` reference line,
  matching the ForCS v4.0.2 input format.
* `ForCS$write()` passes `self$path` to the four CSV-writing `insert*()`
  functions and registers the resulting files via `add_file()` so
  `landis_replicate()` copies them into each replicate directory.
* The four ForCS CSV filenames are now prefixed with `ForCS_`
  (`ForCS_EcoSppDOMParameters.csv`, `ForCS_ANPPTimeSeries.csv`,
  `ForCS_MaxBiomassTimeSeries.csv`, `ForCS_EstablishProbabilities.csv`) so
  their origin is unambiguous alongside other extension files.

## Output file tracking

* All `LandisExtension` subclasses that produce fixed-name output files now
  expose an `output_files` active binding listing those files as relative paths
  (e.g. log CSVs, summary CSVs). Extensions with no fixed outputs inherit the
  base `LandisExtension$output_files` which returns `character(0)`.
* `LandisScenario` gains an `output_files` active binding that returns the two
  LANDIS-II core outputs always written to the scenario directory:
  `Landis-log.txt` and `Metadata/LANDIS-II v8.0/LANDIS-II v8.0.xml`.
* `scenario()` now writes `output_manifest.txt` to the scenario directory,
  listing all fixed-name output files declared by the scenario and its
  extensions. The manifest is registered in `scenario$files` so
  `landis_replicate()` copies it into each replicate directory.
* `tar_landis()` now reads `output_manifest.txt` from the base scenario
  directory and includes the listed files (as absolute paths per replicate) in
  the returned character vector alongside the `log/` scan and `output_dir` scan.
  This ensures `{targets}` tracks log CSVs, `Landis-log.txt`, and the Metadata
  XML explicitly, without relying on glob discovery.

## Resource tracking for simulation runs

* `landis_run_docker()` now tracks wall-clock elapsed time and peak container
  memory. Docker is launched via `callr::r_bg()` so the main thread can poll
  `docker stats --no-stream` every 2 s; the maximum observed RSS is recorded as
  peak memory. A named container (e.g. `landis-run-20260527123456`) is used for
  stats lookup and removed automatically with `--rm`. Results are printed on
  completion and written to `<scenario_dir>/log/docker_resources.log`. The
  `--user uid:gid` flag is now skipped on Windows (`id -u`/`id -g` are not
  available there; Docker Desktop on Windows does not require it).
* `landis_run_local()` now tracks wall-clock elapsed time and peak process
  memory. `system2()` is replaced by `processx::process$new()` (which exposes
  the subprocess PID and handles the working directory directly), and the main
  thread polls `ps::ps_memory_info()` every 2 s. The `ps` package is
  cross-platform, so memory tracking works on Linux, macOS, and Windows without
  any platform-specific shell commands. Results are printed on completion and
  written to `<scenario_dir>/log/local_resources.log`.
* Both functions now return a named list (`exit_code`, `elapsed_sec`,
  `peak_mem_bytes`) instead of a bare integer exit code.
* `processx` and `ps` added to `Imports` (previously available only as
  transitive dependencies of `callr`).

# landisutils 0.0.13

* `DynamicFire$write()` now calls `add_file()` for `InitialWeatherDatabase` so
  it is copied into replicate directories by `landis_replicate()`. Previously
  the weather CSV was silently absent, causing LANDIS-II to fail at runtime.
* `tar_landis()` `output_dir` now accepts a character vector of output
  subdirectory names. Pass `c("output", "fire")` when using the Dynamic Fire
  extension, which writes its maps and event/summary logs to a `fire/`
  subdirectory inside the scenario directory.

# landisutils 0.0.12

* `insertFile()`, `insertLandisData()`, `insertValue()` are now exported so
  project-level code can build custom scenario files using the same primitives
  the package uses internally.
* `landis_find_docker()` is a new helper that returns the path to
  `Landis.Console.dll` inside the container, reading
  `getOption("landisutils.docker.console")`. `landis_run_docker()` now calls
  it instead of duplicating the lookup.
* `landis_replicate()` gains a `base_seed` argument. When set, the
  `RandomNumberSeed` in each replicate's `scenario.txt` is rewritten to
  `base_seed + (rep_index - 1)`, giving every replicate a distinct but
  deterministic seed. Seeds are index-stable: adding more replicates later
  never changes the seeds of existing ones.
* `tar_landis()` gains a `base_seed` argument, passed through to
  `landis_replicate()` and baked into the command expression at factory-call
  time so `crew` workers receive the correct value.

# landisutils 0.0.11

* Pin `santoku` to its GitHub source (`hughjonesd/santoku`) after the package
  was archived on CRAN on 2026-05-15, which broke `zonal` dependency resolution
  in GitHub Actions CI.

# landisutils 0.0.10

* Remove unused PredictiveEcology packages: `LandR`, `SpaDES.core`,
  `SpaDES.tools`, `reproducible`, `scfmutils`, and `map` -- due to broken
  dependency resolution.

# landisutils 0.0.9

* `landis_run()`: fix inverted `stopifnot()` guard — the function previously
  rejected valid `LandisScenario` objects and accepted everything else.
* New `landis_run_docker()` runs a LANDIS-II simulation in an ephemeral Docker
  container (bind-mounting the scenario directory to `/sim`).
* New `landis_run_local()` runs a LANDIS-II simulation directly via `dotnet`,
  writing stdout/stderr to `<scenario_dir>/log/`.
* New `tar_landis()` factory creates a `{targets}` `format = "file"` target
  that runs LANDIS-II (locally or via Docker) and returns tracked output and
  log files.
* New package options `landisutils.docker.image` and `landisutils.run.method`
  are set by `.onLoad()`.

# landisutils 0.0.8

* fix issue with `climr` returning reference period rows - filter these when assembling data;

# landisutils 0.0.7

* `prep_monthly_weather_climr()` no longer silently advertises `"srad"` as a
  supported variable. Vignette examples updated.
* `get_elevation_rast()` gained a `tmp_dir` argument (default
  `<landisutils.cache.path>/elevatr_tiles/`) so AWS Terrain Tile downloads
  land in the package cache instead of leaking into the R session's global
  `tempdir()` (`elevatr`'s own default).
* climate test cleanup: a new `local_climate_test_cache()` test helper
  (`tests/testthat/helper-climate-cleanup.R`) routes the cache option,
  child-process `TMPDIR`, and the JVM `java.io.tmpdir` (used by `J4R` for
  `J4RServer*.log` and `hsperfdata_<user>/`) into the per-test
  `withr::local_tempdir()`, and tears down any `future::plan(multisession)`
  on exit, so `BioSIM`/`climr`/`elevatr` fetch tests no longer accumulate `/tmp`
  residue across runs.

# landisutils 0.0.6

* added a focused integration-test scenario `necn_scrpple` exercising
  `NECNSuccession` + `SocialClimateFire` plus the biomass output
  extensions (`OutputBiomass`, `OutputBiomassCommunity`, `OutputBiomassByAge`,
  `OutputBiomassReclass`); validated end-to-end on both v8 Docker images;
* fixed `OutputBiomassByAge$write()` emitting one `Species` line per element
  (the LANDIS-II parser only accepts one `Species` keyword); the species list
  is now joined with indented continuation lines as the format requires;

# landisutils 0.0.5

* added new climate-data backends for use with the LANDIS-II Climate Library:
    - daily and monthly weather from BioSIM via the `BioSIM` package
      (`prep_daily_weather()`, `prep_monthly_weather_biosim()`);
    - monthly weather from `climr` (`prep_monthly_weather_climr()`),
      including the bcgov-recommended 8-member GCM ensemble
      (`climr_ensemble_8`);
    - monthly weather from TerraClim via `climateR`
      (`prep_monthly_weather()`);
* exported the lower-level fetch and assembly helpers used by the above
  (`get_clim_daily()`, `get_clim_monthly()`, `get_clim_monthly_climr()`,
  `get_clim_monthly_terraclim()`, `get_fwi_daily()`, `get_elevation_rast()`,
  `create_locations_df()`, and the `assemble_climate_library_file*()` family);
* climate caches are now namespaced by a study-area hash so distinct study
  areas don't collide;
* added `test_ecoregionPolys` dataset to support examples and tests;
* reworked the `climate-data` vignette to demonstrate the new backends;
* fixed `BiomassSuccession` R6 class name (was `"DynamicFuels"`);
* added `cffdrs` and `digest` to `Imports`;
* added `arrow`, `BioSIM`, and `climr` to `Suggests`.

# landisutils 0.0.4

* improve docker integration tests;

# landisutils 0.0.3

* added support for the remaining LANDIS-II v8 extensions:
    - succession: DGS Succession, NECN Succession, PnET Succession;
    - disturbance: Biomass Browse, Biomass Harvest, Climate BDA, EDA
      (Epidemiological Disturbance Agent), Forest Roads Simulation, Hurricane,
      Land Use Plus, Linear Wind, Magic Harvest, Original Wind, Root Rot;
    - output: Output Biomass Community, Output Biomass-PnET, Output Biomass
      Reclass, Output Landscape Habitat, Output Local Habitat, Output Wildlife
      Habitat;
* added `Multi-Extension-Scenarios` vignette;
* miscellaneous updates and fixes to existing extensions.

# landisutils 0.0.2

* cache and batch weather data acquisition by year (#1);
* allow setting cache path using option `landisutils.cache.path`;
* change arguments `start` and `end` in `prep_*_weather()` to be integer years;
* use `R6` classes to track simulation input files;
* implemented scenario replication (#3);
* run LANDIS-II in background process via `callr`;

# landisutils 0.0.1

* initial version;
