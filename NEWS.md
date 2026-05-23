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
