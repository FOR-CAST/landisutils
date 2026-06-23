# landisutils 0.0.50

* `calibrate_dynamic_fire()` now caps the Docker warm-pool size at the host RAM budget so a district-scale calibration landscape no longer OOMs the node by starting more containers than fit in memory: each container holds a full LANDIS landscape in memory, so per-container RAM scales with the cell count. The new `mem_per_worker_gb` config field is the per-container estimate (the per-container `--memory` limit is derived from it, +25% headroom, when not set explicitly), and `mem_fraction` (default 0.85) is the fraction of available RAM the pool may use; configs that set neither field behave exactly as before (the cap falls back to the `mem_limit` value, so small-area calibrations are unaffected).

# landisutils 0.0.49

* New `georef_landis_raster()` attaches a template raster's CRS and extent to a spatially-reference-less LANDIS-II GeoTIFF; de-duplicated from the BC_HRV and gitanyow-partial-harvest Phase-6 output-reading templates.
* New `landis_image_info()` reads the LANDIS-II Docker image reference (and `sha256` digest, when captured) used for a run; de-duplicated from the BC_HRV and gitanyow-partial-harvest report-pipeline templates.
* New `parse_landis_log_versions()` parses a `Landis-log.txt` for the console version, seed, and extension version blocks; de-duplicated from the BC_HRV and gitanyow-partial-harvest report-pipeline templates.
* New `prov_landis_container()` formats the LANDIS-II runtime image and digest as a provenance `data.frame`; de-duplicated from the gitanyow-partial-harvest report-pipeline template.
* New `prov_landis_versions()` formats the parsed LANDIS-II console and extension versions as a provenance `data.frame`; de-duplicated from the gitanyow-partial-harvest report-pipeline template.
* New `prov_run_resources()` summarises per-replicate elapsed time and peak memory (mean +/- SD) as a provenance `data.frame`; de-duplicated from the gitanyow-partial-harvest report-pipeline template.
* New `prov_stochasticity()` formats the base seed and replicate count as a provenance `data.frame`; de-duplicated from the gitanyow-partial-harvest report-pipeline template.
* New `run_calibration_validation()` re-simulates Dynamic Fire at the calibrated parameter vector to recover per-replicate goodness-of-fit statistics; de-duplicated from the BC_HRV and gitanyow-partial-harvest report-pipeline templates.

# landisutils 0.0.47

* `calibrate_dynamic_fire()` now resolves the LANDIS-II input files its pre-flight check requires the scenario template to contain -- the succession config, the species file, and the Dynamic Fire inputs -- from the template itself (new internal `.calibration_required_files()`, reusing `.calibration_succession_backend()` / `.calibration_species_file()` / `.calibration_directive_file()`) instead of asserting one project's fixed filenames. Templates that `build_calibration_scenario_template()` writes for a Biomass Succession scenario (e.g. `biomass-succession.txt`, `species-core.txt`, `initial-weather-database.csv`, `dynamic-fire-species.csv`) previously failed this check with "is missing required files: forc-succession.txt, species.txt, ..."; the legacy ForC Succession names remain the fallback defaults, so existing scenarios are unaffected.

# landisutils 0.0.46

* `build_calibration_scenario_template()` now resolves the Dynamic Fire input filenames (`InitialWeatherDatabase`, `Species_CSV_File`, `InitialFireEcoregionsMap`, `GroundSlopeFile`, `UphillSlopeAzimuthMap`) from the template scenario's `dynamic-fire.txt` directives, via the new internal `.calibration_directive_file()` (which also now backs `.calibration_species_file()`), instead of assuming fixed names. Scenarios that name the weather DB `initial-weather-database.csv` and the Dynamic Fire species table `dynamic-fire-species.csv` (rather than the assumed `initial_weather_database.csv` and `DynamicFire_Spp_Table.csv`) previously aborted the calibration scenario reconstruction with a `file.exists(...)` failure in `add_file()`; scenarios using the previous names are unaffected (those remain the fallback defaults).

# landisutils 0.0.45

* `build_calibration_spinup_scenario()` and `build_calibration_scenario_template()` now resolve the species-definitions file from the template scenario's `Species` directive (new internal `.calibration_species_file()`) instead of assuming the name `species.txt`. Scenarios that name it `species-core.txt` (rather than `species.txt`) previously aborted the calibration spinup with `fs::file_exists(species_file) is not TRUE`; scenarios that use `species.txt` are unaffected (it remains the fallback).

# landisutils 0.0.44

* `save_observed_fire_targets()` now derives `fire_sizes_ha` from the polygons' `SIZE_HA` attribute when polygons are supplied and non-empty, falling back to the points' `SIZE_HA` otherwise. The previous behaviour always read sizes from points (NFDB agency-reported sizes), which meant that callers passing higher-quality perimeter polygons (e.g. NBAC's ADJ_HA) for `primary_polys`/`secondary_polys` only got the better data into `area_by_fuel_ha`; the size distribution still came from NFDB. Now NBAC perimeters drive the size CDF too. Callers that pass NFDB polygons see no behaviour change because NFDB polys carry the same `SIZE_HA` field. Callers without polys keep the old points-driven behaviour.
* `save_observed_fire_targets()` `primary_polys` argument is now optional (was required). When `NULL`, the function falls back to points-driven sizes and skips the `area_by_fuel_ha` computation. Matches the existing optional treatment of `secondary_polys`. Existing callers passing a `SpatVector` are unaffected.

# landisutils 0.0.43

* `landis_run_docker()` gains a `startup_jitter` argument (and reads the `LANDIS_STARTUP_JITTER` environment variable when it is `NULL`): when set, each call sleeps a random `runif(0, startup_jitter)` seconds before it first touches Docker, staggering container launches so a large `crew` fleet does not overwhelm the Docker daemon (which stops answering `docker stats` and returns exit 1 under the surge) or hammer the disk backing the image layers and renv library when dozens of replicates start at once. Because the delay cannot change results, `tar_landis()` does not bake it into the `{targets}` command, so tuning it never invalidates completed replicates.

# landisutils 0.0.42

* `landis_run_docker()`, `tar_landis()`, `landis_pool_start()`, and the `calibrate_dynamic_fire()` config default for `cpu_limit` change from `4` to `2`. Empirical measurement across 90 concurrent ForCS + Dynamic Fire + Dynamic Fuels containers under live calibration shows the LANDIS-II console process is effectively single-threaded (median 1.00 cores, p99 1.11 cores, max 1.11 cores) -- the prior default of `4` overprovisioned by ~4x. The new default of `2` covers the 99th-percentile .NET-GC / threadpool burst with comfortable headroom while letting users pack more containers into the same nominal CPU budget; `1` would be tight enough to risk contention between the simulator thread and the .NET GC helper.

# landisutils 0.0.41

* Generated LANDIS-II input files no longer embed a timestamp in their `>> generated by landisutils` header (the package version is kept). The header is now byte-reproducible, so re-writing an unchanged scenario produces identical files and `tar_landis()`'s input-hash skip-check stays stable across Phase-3 rebuilds instead of needlessly re-running every replicate.

# landisutils 0.0.40

* `landis_archive_rep()` is a new exported helper that moves a completed replicate directory from scratch to its final location: a fault-tolerant `rsync -a --partial` (retry + linear backoff) copies into a sibling `.partial` staging dir on the destination filesystem, an atomic rename then publishes it so the final dir only ever appears complete, and the scratch source is deleted only afterwards (a no-op when source and destination resolve to the same path).
* `tar_landis()` gains a `work_root` argument: when set (or when the `LANDIS_SCRATCH` environment variable is non-empty at run time) each replicate is staged and run under fast, local, Docker-bind-mountable scratch and the finished rep is then moved to its final `scenario_dir/repNN` via `landis_archive_rep()`, so the value the target returns -- and everything `{targets}` tracks -- is the final location while scratch holds only transient run files; this makes runs work when `scenario_dir` lives on storage the Docker daemon cannot bind-mount (e.g. a root-squashed NFS share).

# landisutils 0.0.39

## Fix: duplicate-dispatch corruption in LANDIS-II run helpers

* `landis_run_docker()` now derives each container's name deterministically from the scenario
  directory and relies on docker's name uniqueness as a cross-worker mutex. If the same replicate
  is dispatched to two workers at once (for example when `targets` re-runs a branch after a
  false-positive worker crash while the original container is still running), the second call no
  longer starts a parallel container that `O_TRUNC`s the first run's half-written outputs. Instead
  it adopts the in-progress container, waits for it to finish (applying the post-completion
  watchdog so an orphan whose owning worker died cannot hang), and returns that run's result, so a
  duplicate dispatch is reported as success rather than destroying the replicate.
* `landis_run_local()` serializes replicate runs with an advisory `filelock` lock on the rep's
  `log/run.lock`, preventing two concurrent `dotnet` processes from corrupting the same directory.
  Adds `filelock` to Imports.

# landisutils 0.0.38

## Fix: corrupt BioSIM `BUI` values reaching downstream weather summaries

* `get_fwi_daily()` now recomputes `BUI` from `DMC`/`DC` with `cffdrs`, before (and as input to)
  the `FWI` recomputation. BioSIM `FWI_Daily` occasionally returns corrupt `BUI` (up to ~5.8e5,
  with `DMC` up to ~2.9e5); previously only `FFMC`/`ISI`/`FWI` were corrected, so corrupt `BUI`
  propagated into e.g. the Dynamic Fire weather database (where `BUI` is a column).

# landisutils 0.0.37

## Fix: `MinRelativeBiomass` dropped its first ecoregion

* `prepMinRelativeBiomass()` now emits a leading label column. `insertMinRelativeBiomass()` formats the
  table with `.collapseRow(df, i) = df[i, -1]`, which drops the first column (the ShadeClass/label slot,
  since the shade-class prefixes `1..5` are hard-coded in the output). Without a leading label column the
  drop removed the FIRST ECOREGION from the table header and every shade-class row, so a Biomass
  Succession run aborted with "Minimum relative biomass has not been defined for ecoregion 1" whenever the
  active ecoregion set started at the lowest map code. Added a `prep -> insert` regression test.

# landisutils 0.0.36

## Per-trial loss-decomposition trace in Dynamic Fire calibration

* `calibrate_dynamic_fire()` now writes a per-trial CSV alongside the existing per-iter trace.
  The new CSV (`trial_trace_<timestamp>.csv` under `out_dir`) records, for every `objfn`
  evaluation, the parameter vector, the total loss, the raw per-component loss, the weights,
  and the weighted per-component loss. The new path is returned as `trial_trace_path` in the
  function's return list. FORK workers write to per-PID sidecars merged at the end, so the
  full evaluation history is captured without locking. Useful for plotting how DEoptim trades
  off the four loss components (`count`, `size`, `area_fuel`, `severity`) as it converges --
  not just the per-iter best total. Falls back to `NA_character_` for mock / parallel-disabled
  runs that produce no rows.

# landisutils 0.0.35

## Global per-cell BioSIM monthly cache

* `prep_monthly_weather_biosim()` gains a `ref_grid` argument. When supplied with a fixed reference
  grid (e.g. a region-wide raster), the monthly BioSIM pull is cached by the grid's STABLE GLOBAL cell
  ids in one shared, accumulating store -- so overlapping / nested study areas reuse cells already
  fetched and only pull the rest (run a district, then a landscape unit within it fetches nothing).
  Without `ref_grid` the previous per-study-area (elevatr `z`) behaviour is unchanged.
* `create_locations_df()` now reprojects grid cell coordinates to lon/lat for BioSIM, so a PROJECTED
  reference grid (e.g. the aggregated rasterToMatch in an equal-area CRS) yields valid `longDeg`/`latDeg`;
  a lon/lat grid (the elevatr default) is unaffected.
* The store is climate-only (keyed by `CellID`); ecoregion grouping is applied at assemble time:
  `assemble_climate_library_file_monthly()` gains `cell_eco` (a `CellID -> EcoID` map) and `cell_ids`
  (filter to one study area's cells). `get_clim_monthly()`'s public behaviour is unchanged; its BioSIM
  fetch is factored into an internal helper shared with the new path.
* Internal: the batch-x-year pull now uses `purrr::map2()` (sequential within a call) rather than
  `furrr::future_map2()` -- the orchestrator (targets/crew) owns cross-branch parallelism, and an
  internal furrr fan-out under a parallel ambient plan overwhelmed the shared BioSIM web service into
  an uninterruptible J4R socket hang.

# landisutils 0.0.34

## `landis_run_docker()` no longer spawns a nested R session

* `landis_run_docker()` now launches the `docker run` child via
  `processx::process` instead of `callr::r_bg()`. The old approach forked a
  **full background R session** purely to shell out to `docker`; when that ran
  on a `{crew}`/`{mirai}` worker, the nested R process could be orphaned or
  crash its parent worker (`could not start R ... crashed or was killed`),
  which aborted `tar_make()` and SIGKILLed in-flight LANDIS containers
  (exit 137, mid-run). `processx` simply `exec()`s the docker CLI — the same
  lightweight child as the existing `docker stats` polls — eliminating that
  interaction. Behaviour is otherwise unchanged: `docker run` stays in the
  foreground (so the exit status is the container's), `--rm` still
  auto-removes, the post-completion watchdog and resource logging are
  identical, and the container exit code is read via `processx`'s
  `get_exit_status()`.

# landisutils 0.0.33

## Early-stopping convergence criteria in `calibrate_dynamic_fire()`

* `calibrate_dynamic_fire()` now forwards `reltol` and `steptol` to
  `DEoptim::DEoptim.control()`, with project-friendly defaults of
  `reltol = 1e-3` (0.1% relative improvement) and `steptol = 25`
  generations. DEoptim halts before `itermax` if the best-of-population
  objective fails to improve by more than `reltol` for `steptol`
  consecutive generations. The previous behaviour (run the full `itermax`
  schedule regardless of convergence) was the DEoptim default of
  `steptol = itermax`; that default left the optimiser exposed to long
  no-improvement tails which, on this project, intersected an external
  Docker daemon restart and aborted an otherwise-converged run mid-trace.
  Callers can disable early stopping by setting `cfg$steptol >= cfg$itermax`,
  or restore the upstream default by setting `cfg$steptol = NULL`.

# landisutils 0.0.32

## Post-completion watchdog in `landis_run_docker()`

* `landis_run_docker()` now watches the container's stdout for the LANDIS-II
  console marker `"Model run is complete."` and SIGTERMs the container if it
  fails to exit on its own within `post_completion_timeout_sec` seconds
  (default `300` = 5 min). Some long ForCS + Dynamic Fire scenarios with many
  output extensions log the completion marker but then spin in the .NET
  runtime shutdown path indefinitely (observed 25+ h hangs at 100% CPU,
  outputs already on disk). The watchdog stops these zombie containers and
  treats exit codes `137`/`143` as success when the completion marker was
  seen, so `tar_make()` correctly marks the affected reps complete. The sim
  outputs are byte-identical to a clean exit because the watchdog only fires
  *after* the LANDIS-II console reports completion. Pass
  `post_completion_timeout_sec = Inf` to restore the old (unbounded-wait)
  behaviour.
* `tar_landis()` gains a matching `post_completion_timeout_sec` argument
  (default `300`) that forwards to `landis_run_docker()`, so the watchdog
  grace period is tunable (or disablable with `Inf`) from the `{targets}`
  pipeline. The decision is factored into a small internal predicate,
  `.watchdog_should_stop()`, with unit coverage.

# landisutils 0.0.31

## Biomass Succession support in the Dynamic Fire calibration setup

* `build_calibration_spinup_scenario()` and `build_calibration_scenario_template()` now auto-detect the
  succession backend (ForC Succession vs Biomass Succession) from the template directory and wire the
  appropriate succession extension, calibration freeze, and output manifest. Previously both were
  hard-wired to ForC Succession (they required `forc-succession.txt`), so a Biomass Succession project
  could not build a Dynamic Fire calibration scenario. Biomass Succession is frozen for the calibration
  by setting its `Timestep` greater than `sim_years` (it has no `SpinUp` section, and no ForCS `Soil.cs`
  DOM-spinup workaround is needed); ForCS behaviour is unchanged.

# landisutils 0.0.30

## Dyadic season proportions in `insertSeasonTable()`

* `insertSeasonTable()` now quantises the `ProportionFire` column to dyadic
  `1/128` fractions before writing. The Dynamic Fire System reads these
  proportions as single-precision floats and rejects the table ("Season
  Probabilities don't add to 1.0") unless they sum to 1.0 with essentially zero
  tolerance, so arbitrary decimal proportions (e.g. observed fire counts divided
  by the total, which only sum to 1.0 in exact or double-precision arithmetic)
  failed the check unpredictably depending on the data. Dyadic `1/128` fractions
  are exactly representable in float, sum to exactly 1.0 regardless of summation
  order, and round-trip through the table formatting; the largest season absorbs
  the rounding so the values still sum to 1.0. Callers can now pass raw
  normalised season proportions and rely on the table writer for LANDIS-II
  compatibility (1/128 is ~0.8% granularity).

# landisutils 0.0.29

## Optional `SpinupCohorts` / `SpinupMortalityFraction` in `BiomassSuccession`

* `BiomassSuccession$new()` no longer force-writes `SpinupCohorts` and
  `SpinupMortalityFraction`: these keywords are absent from the Core8
  `CoreV8.0-BiomassSuccession7.0` grammar, and the LANDIS-II v8 parser aborts
  ("Found the name \"SpinupCohorts\" but expected \"MinRelativeBiomass\"") when
  they appear. They are now optional and emitted only when set to a non-`NULL`
  value, so a default `BiomassSuccession` config parses and runs against the
  stock `landis-ii-v8-release` image.

## Request throttling + backoff for BioSIM weather fetches

* BioSIM weather retrieval now staggers requests and retries with exponential
  backoff. The `BioSIM` web API can be slow or transiently unavailable under
  load, and the `BioSIM` R client exposes no timeout/retry knobs, so
  `get_clim_monthly()`, `get_clim_daily()`, and `get_fwi_daily()` route their
  `generateWeather()` calls through a shared wrapper that adds a random
  pre-request delay, exponential-backoff retries, and an optional per-attempt
  timeout (resetting the J4R client on timeout). Behaviour is tunable via the
  `landisutils.biosim.request_delay`, `landisutils.biosim.max_attempts`,
  `landisutils.biosim.backoff_base`, and `landisutils.biosim.timeout` options;
  set them where the fetch process can see them (e.g. a worker-inherited
  `.Rprofile`).

# landisutils 0.0.28

## Enable DOM spinup in calibration scenario template

* `build_calibration_scenario_template()` now patches the calibration
  scenario's `forc-succession.txt` `SpinUp` row to `1  0  1  20`
  (DOM spinup ON, biomass spinup OFF), instead of the prior
  `0  0  1  20` (both OFF). Biomass spinup stays off so the
  pre-calibration snapshot IC's `CohortBiomass` values are preserved
  verbatim, but the DOM spinup pass equilibrates ForCS's soil-pool
  state via `SpinupSoils()`. Without it, `DisturbFireFromBiomassPools`
  is left in a partly-initialised state and the first cohort that
  Dynamic Fire damages triggers a `NullReferenceException` in
  `Extension-ForCS-Succession/src/Soil.cs:DisturbanceImpactsBiomass`,
  aborting every calibration trial. Cost: ~30-60s extra startup per
  LANDIS-II trial.

# landisutils 0.0.27

## Parallel pool teardown in `landis_pool_stop()`

* `landis_pool_stop()` now passes every container name to a single
  `docker stop --time T <name1> <name2> ...` and a single
  `docker rm -f <name1> <name2> ...`, instead of looping
  one-container-at-a-time. The Docker daemon parallelises stops
  internally, so a 90-container pool teardown drops from ~15 minutes
  (90 x 10s SIGTERM deadlines, sequential) to roughly `timeout_sec`
  wall time. The function remains idempotent and tolerant of
  already-removed containers.

# landisutils 0.0.26

## `cfg$scratch_root` override for `calibrate_dynamic_fire()`

* The warm Docker pool's bind-mount source defaults to `<out_dir>/scratch`,
  but this fails when `out_dir` lives on a filesystem the Docker daemon
  cannot see (e.g. user-space autofs / sshfs / NFS mounts). The daemon
  errors with `mkdir <mount-root>: permission denied` while resolving the
  bind-mount path. `cfg$scratch_root` lets callers route the pool's
  scratch onto docker-visible local storage while keeping `out_dir` on
  the project mount so the calibration trace CSV and final outputs land
  alongside the rest of the project.

# landisutils 0.0.25

## Connection-aware default `n_cores`

* `calibrate_dynamic_fire()` now picks its default cluster size from
  `parallelly::availableCores(constraints = "connections", omit = 2)`
  when parallelly is installed, falling back to
  `parallel::detectCores() - 2L` otherwise. `detectCores()` reports the
  logical core count and ignores R's per-session connection cap (~125
  on default builds), so on very large hosts (e.g. 256-core machines)
  a naive default silently over-provisions the FORK cluster beyond
  what the R session can support. Explicitly setting `cfg$n_cores`
  short-circuits both defaults. `parallelly` is now a `Suggests`.

## Work around DEoptim 2.2.8 cluster-cleanup bug

* `calibrate_dynamic_fire()` no longer passes `parallelType = 1L` to
  `DEoptim::DEoptim()` when supplying its own FORK cluster via
  `control$cluster`. DEoptim 2.2.8's `ctrl$cluster` branch uses the
  supplied cluster without binding a local `cl` variable, but its
  post-loop cleanup unconditionally evaluates `parallel::stopCluster(cl)`
  whenever `parallelType == "parallel"`, which errors with
  `object 'cl' not found`. Leaving `parallelType` at its default
  ("none") skips that cleanup path while still triggering the parallel
  `parApply(cl = ctrl$cluster, ...)` evaluation in DEoptim's body. The
  FORK cluster lifecycle is fully managed by `calibrate_dynamic_fire()`
  via `on.exit`.

## Clamp `IgnProb` to LANDIS-II's permitted range

* `patch_fire_config()` and `apply_calibrated_ignprob()` now clamp the
  per-fuel `IgnProb` value to `[0, 1]` after applying the calibration
  multiplier. The Dynamic Fire System parser rejects any `IgnProb`
  outside this range with `Error with the input value for Fuel type
  initiation probability: Value must be between 0 and 1.0` and aborts
  the run, which previously caused every DEoptim trial whose multiplier
  pushed the product above 1.0 (e.g., `IgnProb_Conifer = 1.5` against
  the default `IgnProb = 1.0` for Conifer surfaces C1-C5/C7/M1-M4) to
  fail immediately. The multiplier ranges in the smoke test and
  `calibration_par_names()` are unchanged; clamping just makes the
  search-space boundary explicit instead of relying on every multiplier
  being <= 1.0.

## Retain failed-trial scratch directories for post-mortem

* `sim_landis()` now keeps a failing trial's per-rep scratch directory on
  disk (and prints its path) when the LANDIS-II invocation errors. The
  scratch was previously deleted unconditionally by an `on.exit` cleanup,
  which made it impossible to inspect `<trial_dir>/rep01/log/` for the
  underlying LANDIS-II stderr/stdout from a failed calibration trial.
  Successful trials are still cleaned up as before, so this only adds
  disk usage on failure. The behaviour can still be opted out of by
  passing `keep_scratch = TRUE` (which now retains the dir on both
  success and failure -- the prior `keep_scratch = FALSE` default still
  means "clean up after success only").

# landisutils 0.0.24

## Per-file input overrides on `build_calibration_scenario_template()`

* New `overrides = list()` argument lets callers substitute individual
  template files post-copy without touching the production scenario.
  Useful for cropping / aggregating specific inputs for calibration
  (e.g., a coarser fuel raster, a smaller weather DB, a substitute slope
  raster) without forking the whole template. Accepted keys:
  `ground_slope.tif`, `uphill_slope_azimuth.tif`, `fire-ecoregions.tif`,
  `initial_weather_database.csv`, `DynamicFire_Spp_Table.csv`,
  `species.txt`, `ecoregions.txt`, `ecoregions.tif`, `climate.txt`.
  `.tif` overrides also carry their `.aux.xml` / `.tfw` sidecars.
  Backward compatible: `overrides = list()` (the default) preserves the
  original "copy everything from `template_dir`" behaviour.

## Warm Docker pool resilience

* New exported `landis_pool_restart_one(pool, idx)` -- stops + removes the
  container at index `idx` and starts a fresh replacement with identical config (image,
  scratch_root bind-mount, user, cpu_limit, mem_limit) using a new
  auto-generated container name. Pool state (`$names[idx]`) is updated in
  place and also propagated to the caller's frame so loops can use the
  current container name on the next iteration.
* `landis_pool_exec()` gains `retries = 0L`. When > 0 and the exec command
  fails with non-zero status, the container is restarted via
  `landis_pool_restart_one()` and the command retried, up to `retries`
  additional attempts. Useful for long calibrations that occasionally hit
  OOM kills or daemon hiccups without wanting the whole DEoptim run to
  abort. Returns an additional `attempts` field counting actual attempts
  (1 = no retry needed; >1 = some retries consumed).
* Pool object now carries the start-time args (`user_args`, `cpu_args`,
  `mem_args`) so restarts produce containers with matching config.
* Refactor: container-creation logic moved into the internal
  `.landis_pool_start_one()` so `landis_pool_start()` and
  `landis_pool_restart_one()` share one code path.

## Pre-flight checks in `calibrate_dynamic_fire()`

The driver now runs a battery of cheap pre-flight checks at function entry
-- before starting the warm Docker pool or FORK cluster -- to catch common
config / scenario / payload errors fast. Hard errors include:

* `cfg$lower >= cfg$upper` for any parameter (lists the offending names).
* `cfg$NP < 4` (DEoptim minimum), `cfg$itermax < 1`, `cfg$n_reps < 1`.
* `cfg$weights` all zero (DEoptim would have nothing to optimise).
* Unknown `cfg$simulator` (one of `"landis"`, `"r_reimpl"`, `"mock"`).
* For `simulator = "landis"`: the calibration scenario template missing any
  of the LANDIS-II input files normally produced by
  `build_calibration_scenario_template()`.
* Observed-targets payload missing required `$primary` shape.
* Docker not available when `method = "docker"`; LANDIS console not findable
  when `method = "local"`.
* Scratch root not writable.

Soft signals (warnings / messages):

* `NP < 10 * length(par_names)` (DEoptim's own advisory, surfaced earlier).
* `cfg$weights['area_fuel'] > 0` but observed lacks `area_by_fuel_ha` /
  `fuel_code_to_base`; or `cfg$weights['severity'] > 0` but observed lacks
  `severity_dist`. In both cases the corresponding loss component
  contributes 0.

## Calibration smoke-test script

* New `inst/scripts/calibration_smoke_test.R` -- a complete 5-stage smoke
  test of the calibration plumbing (observed targets, spinup, scenario
  template, sim_landis trial, DEoptim loop) at minimal scale (NP=4,
  itermax=2, n_reps=1, n_cores=2). Useful for first-time setup, post-
  upgrade verification, and confirming Docker + DEoptim are installed
  correctly. Runs in ~5-20 min via the warm Docker pool.

  Usage:
  ```r
  source(system.file("scripts/calibration_smoke_test.R", package = "landisutils"))
  ```

## Tier 2 calibration loss components

`loss_from_stats()` now computes `L_area_fuel` and `L_severity` when the
corresponding observed components are present in the payload; previously both
were stubbed at zero. Components are still gated by their `weights` entry, so
projects that aren't ready to use them stay on the Tier 1 (count + size) loss
by default.

* `L_area_fuel` is a chi-squared distance between simulated and observed
  burn-area-by-base-fuel-type *proportions*. Simulated area-by-fuel is
  derived from each event's `init_fuel` (the ignition cell's fuel code)
  times its `DamagedSites`, mapped to the five base fuel types via
  `observed$fuel_code_to_base`. Activated when `observed$primary$area_by_fuel_ha`
  AND `observed$fuel_code_to_base` are both set; contributes 0 otherwise.

* `L_severity` is a chi-squared distance between simulated and observed
  severity-class *proportions*. Simulated severities come from each event's
  `MeanSeverity` (binned into integer classes 1..5 at half-integer
  boundaries); observed comes from `observed$primary$severity_dist`, a
  named numeric vector summing to 1. Activated when `severity_dist` is
  non-NULL.

* `save_observed_fire_targets()` gains a `severity_dist = NULL` argument
  that callers can pass to embed a prior in the observed payload (e.g.,
  the new `default_severity_prior_sturtevant2009()`).

* New exported helper: `default_severity_prior_sturtevant2009()` returns a
  named 5-element vector of severity-class proportions derived from
  Sturtevant et al. 2009. Intended as a starting point; projects should
  override with empirical priors when available.

## Dynamic Fire System extension calibration

A new function family for calibrating the LANDIS-II Dynamic Fire System
extension. The calibration tunes `SeverityCalibrationFactor`, per-season FMC
`HiProp` values, and per-base-fuel-type `IgnProb` multipliers so simulated
fires match observed regional fire statistics; the empirically-fit parameters
(`Mu`, `Sigma`, `Max`, `NumFires`, seasonal `PropFire`) are fit at the data
layer upstream and are not part of the optimisation.

* **Pure-data helpers** (no LANDIS-II invocation):
  * `calibration_par_names()` -- canonical 9-name parameter vector.
  * `parse_dynamic_fire_logs()` -- reads `fire/dynamic-fire-event-log.csv` +
    `fire/dynamic-fire-summary-log.csv` into a small per-rep stats list.
  * `patch_fire_config()` -- surgical text patch of `dynamic-fire.txt`
    (SeverityCalibrationFactor scalar; FireSizesTable HiProp columns;
    FuelTypeTable IgnProb column multiplied per base type).
  * `loss_from_stats()` -- multi-component weighted loss (count + KS-on-size
    in Tier 1; area_by_fuel + severity stubbed at weight 0 for Tier 2).
  * `apply_calibrated_ignprob()` / `apply_calibrated_hi_prop()` -- table-level
    helpers used downstream of calibration to feed calibrated values into
    production fire-config writers.

* **Observed-target builder** (one-time NFDB-derived summaries):
  * `save_observed_fire_targets()` -- writes a small `.rds` of per-ecoregion
    observed summaries. Project-agnostic: primary / secondary ecoregion
    SpatVectors + `fuel_code_to_base` mapping are all caller-provided.
  * `bc_fuel_code_to_base()` -- default fuel-code mapping for BC
    `FUEL_TYPE_CD` factor encoding (codes 1..13); pass a custom mapping
    if your project's fuel-classification raster uses a different encoding.

* **Scenario builders** for the static-landscape calibration scenario:
  * `build_calibration_spinup_scenario()` -- builds a one-off LANDIS-II
    scenario that runs ForCS with both spinup flags ON and emits a snapshot
    of the spun-up cohort community via the Output Biomass Community
    extension. The year-0 snapshot becomes the calibration IC.
  * `run_calibration_spinup()` -- blocking single-trial LANDIS-II invocation
    for the spinup; dispatches to `landis_run_local()` / `landis_run_docker()`.
  * `build_calibration_scenario_template()` -- copies a production fire
    scenario, swaps the IC for the spinup snapshot, patches ForCS for
    calibration (spinup off + frozen succession), optionally writes a fresh
    baseline `dynamic-fire.txt` inline (breaks the cycle between a
    `calibrated_fire_params`-aware production fire config and the
    calibration loop).
  * `write_landis_scenario_file()` (in `scenarios.R`) -- lower-level
    `scenario.txt` writer that takes already-written extension config-file
    paths rather than R6 extension objects. Useful when project pipelines
    write extension configs in separate steps.

* **Warm Docker pool** for calibration:
  * `landis_pool_start()` / `landis_pool_exec()` / `landis_pool_stop()` --
    a pool of detached LANDIS-II containers that `docker exec`s per DEoptim
    trial instead of `docker run --rm` per trial. Per-call env overrides
    (`HOME=/tmp`, `DOTNET_BUNDLE_EXTRACT_BASE_DIR=...`) keep dotnet from
    accreting per-user-cache state between trials. Designed for `on.exit()`
    teardown from calibration drivers.

* **Simulator backends** for the calibration loop:
  * `sim_landis()` -- per-trial LANDIS-II invocation. Takes file paths only
    (FORK-safe), copies template -> scratch dir, patches `dynamic-fire.txt`,
    runs LANDIS-II either via the warm pool or a one-off
    `landis_run_docker()` / `landis_run_local()`, parses logs.
  * `sim_r_reimpl()` -- reserved slot for a future pure-R reimplementation;
    currently errors with a not-yet-implemented message.
  * `sim_mock()` -- plausibly-shaped output for testing the calibration
    driver's control flow without Docker.

* **DEoptim driver:**
  * `calibrate_dynamic_fire()` -- orchestrates the calibration: starts a
    warm Docker pool, sets up a FORK cluster with per-worker container
    pinning, invokes `DEoptim::DEoptim()` with the multi-component loss as
    the objective, tears down pool + cluster via `on.exit()`. Gated on
    `requireNamespace("DEoptim")` (DEoptim is in Suggests; install via
    `renv::install("DEoptim")` before calling).

* **Vignette:** `vignette("Dynamic-Fire-Calibration", package = "landisutils")`
  documents the end-to-end target-wiring pattern for downstream projects.

* **Tests:** 97+ testthat expectations across calibration + pool test files.
  Docker-gated tests `skip_if_not()` when docker is unavailable; DEoptim-gated
  driver tests `skip_if_not_installed("DEoptim")`.

# landisutils 0.0.23

## `landis_run_docker()` accepts resource constraints

* New arguments to `landis_run_docker()` (and proxied through `tar_landis()`):
  * `cpu_limit = 4`: maps to `docker run --cpus`. LANDIS-II compute is
    single-threaded (~1 core), but the .NET runtime spins up 9-11 OS threads
    for GC and the thread pool, so 4 is a comfortable default. Pass `NULL` for no limit.
  * `mem_limit = "8g"`: baseline RAM cap (maps to `docker run --memory`).
    Accepts a numeric byte count or a string like `"4g"` / `"512m"`. Pass
    `NULL` (or `mem_limit = Inf`) for no limit.
  * `mem_margin = 1.5`: headroom factor applied to a previously-observed
    peak (see auto-resolution below).
* **Auto-resolution from prior resource logs.** Before running, the function
  reads any existing `<rep_dir>/log/{docker,local}_resources.log` for the
  rep. If `peak_mem_bytes * mem_margin` exceeds the baseline `mem_limit`,
  the limit is raised to that value so a rep that ran fine last time is
  never killed by the cap on a rerun. If **no** prior log exists for the
  rep (first run, or rep dir freshly deleted), the memory cap is dropped
  entirely so the first run can discover what it needs; subsequent runs
  inherit the empirically observed peak.
* The CPU limit is constant regardless of history: LANDIS-II is
  single-threaded and the .NET runtime doesn't scale with available cores.

# landisutils 0.0.22

## Resource logs now self-describe the host

* `landis_run_docker()` and `landis_run_local()` now append three additional
  lines to each rep's `docker_resources.log` / `local_resources.log`:
  ```
  host_cpu_model: <model name>          # e.g. "AMD EPYC 7702 64-Core Processor"
  host_cpu_cores: <N>                   # logical cores visible to R
  host_ram_bytes: <N>                   # total system memory in bytes
  ```
  This makes each per-rep resource log self-describing: downstream provenance
  tooling can recover not just what the rep used (`elapsed_sec`,
  `peak_mem_bytes`) but the host it ran on, important when reps are
  dispatched across a heterogeneous cluster.
* New exported helpers:
  * `host_cpu_info()` returns `list(model, n_logical, ram_bytes)`,
    cross-platform: `/proc/cpuinfo` + `/proc/meminfo` on Linux,
    `sysctl machdep.cpu.brand_string` + `hw.memsize` on macOS,
    `PROCESSOR_IDENTIFIER` env var + `wmic ComputerSystem ... TotalPhysicalMemory`
    on Windows. Logical-core count uses `parallel::detectCores()` everywhere.
    Called automatically by the run helpers.
  * `read_landis_resource_logs(run_dir)` parses any
    `*_resources.log` under `run_dir` and returns one data.frame row per
    rep with all fields. Used by downstream report tooling to summarise
    run-time / memory / host across replicates.

# landisutils 0.0.21

## `prepTopographyFile()` fills NoData with 0

* `prepTopographyFile()` (and its `prepGroundSlopeFile()` /
  `prepUphillAzimuthMap()` wrappers) now replaces NoData cells with `0`
  before writing the INT2S raster. `terra::terrain()` leaves edge cells
  as `NaN` (no full 3x3 neighbourhood), which becomes the `-32768`
  sentinel under INT2S. LANDIS-II's Dynamic Fire reader rejects any
  active cell with that value:
  ```
  Ground Slope invalid map code: -32768
  ```
  The bug was latent for grids with few active cells, but is exposed
  whenever upstream changes (e.g. corrected non-veg masks) expand the
  active area into the 1-cell edge band of the terrain raster. A flat
  default (0 deg) is safe: a single-cell edge contributes negligibly to
  Dynamic Fire's rate-of-spread.

# landisutils 0.0.20

## `tar_landis()` idempotency respects input changes

* The skip check in `tar_landis()` is now input-aware. Previously the run was
  skipped whenever `<rep_dir>/Landis-log.txt` existed and contained
  "Model run is complete", regardless of whether the inputs had changed. This
  meant that when `{targets}` correctly re-evaluated the run target after an
  upstream input change (e.g. a regenerated `initial-communities.tif` or
  `ecoregions.tif`), the surrounding command ran but the skip check still
  fired, so `landis_run_docker()` / `landis_run_local()` was never invoked
  and the rep dir kept stale outputs from the previous run.
* The fix writes a `<rep_dir>/log/input_hash.json` sidecar after each
  successful run, capturing a SHA-1 of (per-input-file MD5 + `base_seed` +
  `rep_index` + `scenario_file`). The skip check now also requires the saved
  hash to match the current input hash; any mismatch triggers a real rerun.
* New `force = FALSE` argument on `tar_landis()`. Setting `force = TRUE`
  short-circuits the skip check so LANDIS-II always runs (useful for
  debugging and one-off forced reruns without deleting rep dirs).
* **Migration:** existing rep dirs lack `log/input_hash.json`, so the first
  `tar_make()` after upgrading rebuilds every rep. This is the safe-conservative
  behaviour: we can't know whether existing outputs correspond to current
  inputs. Users who *know* a rep is current can sidestep the rerun by writing
  the hash file manually.

# landisutils 0.0.19

## `landis_run_docker()` captures image digest

* `landis_run_docker()` now writes `<scenario_dir>/log/docker_image.log`
  containing the immutable `sha256` digest of the image used for the run
  (`<repo>@sha256:<64hex>`, falling back to the local `Id` for images with no
  registry origin). Image tags are mutable; the digest is the canonical
  identifier of the bytes that actually ran. Downstream provenance tooling can
  read this sidecar to pin a run to a specific image regardless of subsequent
  tag movement.
* New `pull = FALSE` argument; when `TRUE`, `docker pull <image>` runs before
  the simulation so the captured digest reflects the current registry rather
  than a possibly-stale local copy. The argument is also exposed via
  `tar_landis(pull = ...)`.

# landisutils 0.0.18

## `leading_species()` handles non-vegetated cells

* `leading_species()` now returns `"Non-vegetated"` for cells where total
  biomass across all species is zero, instead of falling through to the
  alphabetical tiebreaker (which arbitrarily assigned `"Ac"` to defoliated
  cells). This matches the existing `community_label()` behaviour and means
  transition / alluvial plots accurately reflect fire and harvest impacts.

# landisutils 0.0.17

## Per-replicate parallel branching in `tar_landis()`

* **Breaking API change:** `tar_landis()` no longer accepts `n_reps`. It now
  takes an explicit `rep_index` argument (an unquoted upstream target symbol)
  and returns a **single** `tar_target` object (same as the original API).
* The caller creates the rep-index target explicitly inside the module `list()`
  alongside the `tar_landis()` call. This keeps both targets visible to
  tarborist's static AST analysis (`tarborist.additionalSingleTargetFactories`
  handles the run target; the literal `tar_target()` call handles the rep-index
  target):
  ```r
  list(
    tar_target(name = ..._rep_index, command = seq_len(5L), iteration = "vector"),
    landisutils::tar_landis(name = ..., rep_index = ..._rep_index, ...,
                            pattern = cross(scenario_dir, ..._rep_index))
  )
  ```
* `iteration = "vector"` on the rep-index target is what enables `cross()` to
  iterate over individual elements, giving `n_scenarios x n_reps` independent
  branches dispatched to crew workers in parallel.
* Each branch runs **one** LANDIS-II simulation and tracks only that replicate's
  output files. Previously all `n_reps` simulations ran inside a single `for`
  loop within one target branch.
* **Caching and scaling:** adding replicates only creates new branches --
  existing replicate results remain cached. Changing one replicate's inputs
  invalidates only that branch. Previously any change invalidated all replicates.

## `landis_replicate()` single-rep mode

* `landis_replicate()` gains a `rep_index` parameter for single-replicate
  creation. Pass `rep_index = i` (instead of `n_reps = N`) to create exactly
  one replicate directory (`repNN/`) without touching any others. Useful when
  each replicate is dispatched to its own crew worker.
* `n_reps` is now keyword-only; the positional second-argument form still works
  but `n_reps =` is clearer.
* The function now requires exactly one of `n_reps` or `rep_index`.
* The seed assigned when `rep_index` is used (`base_seed + rep_index - 1`)
  matches the seed assigned by the `n_reps` mode for the same index, so
  results are reproducible regardless of which call form was used.

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
