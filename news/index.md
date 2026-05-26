# Changelog

## landisutils 0.0.12

- [`insertFile()`](https://for-cast.github.io/landisutils/reference/insertFile.md),
  [`insertLandisData()`](https://for-cast.github.io/landisutils/reference/insertLandisData.md),
  [`insertValue()`](https://for-cast.github.io/landisutils/reference/insertValue.md)
  are now exported so project-level code can build custom scenario files
  using the same primitives the package uses internally.
- [`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md)
  is a new helper that returns the path to `Landis.Console.dll` inside
  the container, reading `getOption("landisutils.docker.console")`.
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  now calls it instead of duplicating the lookup.
- [`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md)
  gains a `base_seed` argument. When set, the `RandomNumberSeed` in each
  replicate’s `scenario.txt` is rewritten to
  `base_seed + (rep_index - 1)`, giving every replicate a distinct but
  deterministic seed. Seeds are index-stable: adding more replicates
  later never changes the seeds of existing ones.
- [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  gains a `base_seed` argument, passed through to
  [`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md)
  and baked into the command expression at factory-call time so `crew`
  workers receive the correct value.

## landisutils 0.0.11

- Pin `santoku` to its GitHub source (`hughjonesd/santoku`) after the
  package was archived on CRAN on 2026-05-15, which broke `zonal`
  dependency resolution in GitHub Actions CI.

## landisutils 0.0.10

- Remove unused PredictiveEcology packages: `LandR`, `SpaDES.core`,
  `SpaDES.tools`, `reproducible`, `scfmutils`, and `map` – due to broken
  dependency resolution.

## landisutils 0.0.9

- [`landis_run()`](https://for-cast.github.io/landisutils/reference/landis_run.md):
  fix inverted [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html)
  guard — the function previously rejected valid `LandisScenario`
  objects and accepted everything else.
- New
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  runs a LANDIS-II simulation in an ephemeral Docker container
  (bind-mounting the scenario directory to `/sim`).
- New
  [`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md)
  runs a LANDIS-II simulation directly via `dotnet`, writing
  stdout/stderr to `<scenario_dir>/log/`.
- New
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  factory creates a [targets](https://docs.ropensci.org/targets/)
  `format = "file"` target that runs LANDIS-II (locally or via Docker)
  and returns tracked output and log files.
- New package options `landisutils.docker.image` and
  `landisutils.run.method` are set by `.onLoad()`.

## landisutils 0.0.8

- fix issue with `climr` returning reference period rows - filter these
  when assembling data;

## landisutils 0.0.7

- [`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
  no longer silently advertises `"srad"` as a supported variable.
  Vignette examples updated.
- [`get_elevation_rast()`](https://for-cast.github.io/landisutils/reference/get_elevation_rast.md)
  gained a `tmp_dir` argument (default
  `<landisutils.cache.path>/elevatr_tiles/`) so AWS Terrain Tile
  downloads land in the package cache instead of leaking into the R
  session’s global [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
  (`elevatr`’s own default).
- climate test cleanup: a new `local_climate_test_cache()` test helper
  (`tests/testthat/helper-climate-cleanup.R`) routes the cache option,
  child-process `TMPDIR`, and the JVM `java.io.tmpdir` (used by `J4R`
  for `J4RServer*.log` and `hsperfdata_<user>/`) into the per-test
  [`withr::local_tempdir()`](https://withr.r-lib.org/reference/with_tempfile.html),
  and tears down any `future::plan(multisession)` on exit, so
  `BioSIM`/`climr`/`elevatr` fetch tests no longer accumulate `/tmp`
  residue across runs.

## landisutils 0.0.6

- added a focused integration-test scenario `necn_scrpple` exercising
  `NECNSuccession` + `SocialClimateFire` plus the biomass output
  extensions (`OutputBiomass`, `OutputBiomassCommunity`,
  `OutputBiomassByAge`, `OutputBiomassReclass`); validated end-to-end on
  both v8 Docker images;
- fixed `OutputBiomassByAge$write()` emitting one `Species` line per
  element (the LANDIS-II parser only accepts one `Species` keyword); the
  species list is now joined with indented continuation lines as the
  format requires;

## landisutils 0.0.5

- added new climate-data backends for use with the LANDIS-II Climate
  Library:
  - daily and monthly weather from BioSIM via the `BioSIM` package
    ([`prep_daily_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md),
    [`prep_monthly_weather_biosim()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md));
  - monthly weather from `climr`
    ([`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)),
    including the bcgov-recommended 8-member GCM ensemble
    (`climr_ensemble_8`);
  - monthly weather from TerraClim via `climateR`
    ([`prep_monthly_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md));
- exported the lower-level fetch and assembly helpers used by the above
  ([`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md),
  [`get_clim_monthly()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly.md),
  [`get_clim_monthly_climr()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly_climr.md),
  [`get_clim_monthly_terraclim()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly_terraclim.md),
  [`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md),
  [`get_elevation_rast()`](https://for-cast.github.io/landisutils/reference/get_elevation_rast.md),
  [`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md),
  and the `assemble_climate_library_file*()` family);
- climate caches are now namespaced by a study-area hash so distinct
  study areas don’t collide;
- added `test_ecoregionPolys` dataset to support examples and tests;
- reworked the `climate-data` vignette to demonstrate the new backends;
- fixed `BiomassSuccession` R6 class name (was `"DynamicFuels"`);
- added `cffdrs` and `digest` to `Imports`;
- added `arrow`, `BioSIM`, and `climr` to `Suggests`.

## landisutils 0.0.4

- improve docker integration tests;

## landisutils 0.0.3

- added support for the remaining LANDIS-II v8 extensions:
  - succession: DGS Succession, NECN Succession, PnET Succession;
  - disturbance: Biomass Browse, Biomass Harvest, Climate BDA, EDA
    (Epidemiological Disturbance Agent), Forest Roads Simulation,
    Hurricane, Land Use Plus, Linear Wind, Magic Harvest, Original Wind,
    Root Rot;
  - output: Output Biomass Community, Output Biomass-PnET, Output
    Biomass Reclass, Output Landscape Habitat, Output Local Habitat,
    Output Wildlife Habitat;
- added `Multi-Extension-Scenarios` vignette;
- miscellaneous updates and fixes to existing extensions.

## landisutils 0.0.2

- cache and batch weather data acquisition by year (#1);
- allow setting cache path using option `landisutils.cache.path`;
- change arguments `start` and `end` in `prep_*_weather()` to be integer
  years;
- use `R6` classes to track simulation input files;
- implemented scenario replication (#3);
- run LANDIS-II in background process via `callr`;

## landisutils 0.0.1

- initial version;
