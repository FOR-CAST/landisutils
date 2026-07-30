# Collapse duplicate communities in an Output Biomass Community snapshot

Biomass Succession writes its Output Biomass Community state with **one
map code per pixel**: succession is evaluated per cell, so the writer
emits a distinct `MapCode` for every active cell and never re-collapses
cells whose cohort lists ended up identical. On a large landscape almost
all of those map codes are duplicates of one another, and the redundancy
is carried entirely by the CSV.

## Usage

``` r
dedup_community_snapshot(csv, tif, out_csv = csv, out_tif = tif, quiet = FALSE)
```

## Arguments

- csv:

  Path to the snapshot CSV (`community-input-file-<t>.csv`), with a
  `MapCode` column plus the cohort columns (typically `SpeciesName`,
  `CohortAge`, `CohortBiomass`).

- tif:

  Path to the matching map-code raster (`output-community-<t>.tif`).

- out_csv, out_tif:

  Output paths. Default to overwriting `csv` / `tif`.

- quiet:

  Suppress the summary message.

  A snapshot can also contain **empty communities**: active cells whose
  map code has no CSV rows at all (no cohorts – recently disturbed, or
  active but unforested). Biomass Succession emits these itself. They
  are active landscape, so they are collapsed to one shared code and
  left active rather than zeroed; the active-cell count is asserted
  unchanged before anything is written.

## Value

Invisibly, a list with `csv`, `tif`, `map_codes_before`,
`map_codes_after`, `rows_before`, `rows_after`, `empty_code` (the shared
empty-community code, `NA` if there were none) and `empty_cells`.

## Details

That matters because LANDIS-II reads initial communities back through
`Landis.Library.InitialCommunities.Universal.DatasetParser`, which
builds a `System.Dynamic.ExpandoObject` per row. The parser's memory
cost is a large multiple of the file size, so a snapshot that is mostly
duplicate rows can exhaust a container's `--memory` and abort the run
with `System.OutOfMemoryException` inside `ReadCSVInputFile`, before the
simulation starts. Measured on a 2.98M-active-cell landscape: 2,684,154
map codes carrying only 4,153 distinct communities – a 1,472 MB CSV that
deduplicates to roughly 2 MB.

This rewrites the pair so that one map code represents each **distinct**
community and the raster points every pixel at its community's new code.
The simulated state is unchanged: every pixel still maps to exactly the
cohort list it had before.

Two communities are the same when their cohort sets are identical,
compared on every non-`MapCode` column after ordering rows canonically
within a map code. Biomass is compared exactly – no rounding – so this
never merges cells that differ, only cells that are already identical.

## See also

[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md)

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md),
[`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md),
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md),
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
[`default_severity_prior_sturtevant2009()`](https://for-cast.github.io/landisutils/reference/default_severity_prior_sturtevant2009.md),
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md),
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
