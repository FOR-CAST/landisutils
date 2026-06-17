# Build a calibration spinup scenario directory

Materialises a self-contained LANDIS-II scenario whose only purpose is
to run the succession backend (ForC Succession or Biomass Succession,
auto-detected from the template's config file) for `duration` years,
emit a snapshot of the spun-up cohort community via the Output Biomass
Community extension, and exit.

## Usage

``` r
build_calibration_spinup_scenario(
  out_dir,
  template_dir,
  duration = 1L,
  community_output_year = 0L,
  cell_length
)
```

## Arguments

- out_dir:

  Character. Destination directory (created or overwritten).

- template_dir:

  Character. Existing production scenario directory to copy from. Must
  contain `forc-succession.txt`, `species.txt`, `ecoregions.txt`,
  `ecoregions.tif`, `climate.txt`, `ForCS_DM.txt`,
  `initial-communities.csv`, `initial-communities.tif`, and the ForCS
  data CSVs.

- duration:

  Integer. Simulation duration in years. LANDIS-II minimum 1.

- community_output_year:

  Integer. Year at which the snapshot is consumed downstream (caller
  chooses 0 for post-spinup state; default 0).

- cell_length:

  Integer. Raster cell size in metres.

## Value

Character scalar: absolute path to the written `scenario.txt`.

## Details

The Output Biomass Community extension emits at multiples of its
Timestep starting from **year 0** (post-init, pre-step), so with
Timestep = 1 and Duration = 1 we get two snapshot CSVs:
`community-input-file-0.csv` (post-spinup state – this is the one we
want) and `community-input-file-1.csv` (after one year of ANPP). The TIF
(`output-community-0.tif`) is emitted only once at year 0 – cohort
communities don't repartition in a no-disturbance run, so one raster
suffices for both years.

Note on CSV schema: LANDIS-II Output Biomass Community v3 writes a
5-column file
(`MapCode, SpeciesName, CohortAge, CohortBiomass, CohortANPP`).
LANDIS-II's initial-communities parser tolerates the extra `CohortANPP`
column, so the file is drop-in usable as `InitialCommunitiesFiles`
without post-processing.

Implementation: copy every top-level file from `template_dir` (a
production scenario directory), strip the disturbance stack from the
copied scenario.txt, add an Output Biomass Community extension, and
rewrite scenario.txt with Duration = `duration`. Rep subdirectories are
NOT copied.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md),
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
