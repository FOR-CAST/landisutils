# Build the calibration scenario template directory

Materialises a self-contained LANDIS-II scenario directory that DEoptim
workers copy from. Each per-trial worker copies this template into a
scratch dir, patches just `dynamic-fire.txt` with candidate parameters,
and runs LANDIS-II. Anything that does NOT vary across trials (ForCS
config, fire ecoregions map, ground slope, weather DB, species file,
...) lives in this template so it's built once.

## Usage

``` r
build_calibration_scenario_template(
  out_dir,
  template_dir,
  snapshot_ic_csv,
  snapshot_ic_tif,
  baseline_fire_size_table = NULL,
  baseline_fuel_type_table = NULL,
  baseline_fire_damage_table = NULL,
  baseline_seasons_sim_table = NULL,
  sim_years = 10L,
  cell_length
)
```

## Arguments

- out_dir:

  Character. Destination directory (created or overwritten).

- template_dir:

  Character. Existing production fire scenario directory to copy from.

- snapshot_ic_csv, snapshot_ic_tif:

  Character. Paths to the spun-up community CSV / TIF (return of
  [`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md)).

- baseline_fire_size_table, baseline_fuel_type_table,
  baseline_fire_damage_table, baseline_seasons_sim_table:

  data.frame or NULL. Baseline (un-calibrated) tables. When all four are
  supplied, the function writes a fresh `dynamic-fire.txt` from them.

- sim_years:

  Integer. Calibration sim duration (years). Default 10.

- cell_length:

  Integer. Raster cell size in metres.

## Value

Character scalar: absolute path to the written `scenario.txt`.

## Details

Composition:

- ForC Succession backend with both spinup flags OFF and a frozen
  Timestep (succession effectively a no-op).

- Dynamic Fire System + Dynamic Fuel System as the only disturbances.

- Initial communities point at the spun-up snapshot from
  [`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md)
  (renamed to the standard `initial-communities.csv` + `.tif` so the
  existing ForCS config references work without further modification).

- Duration = `sim_years`.

When the baseline fire-config tables are supplied (recommended), the
function overwrites the copied `dynamic-fire.txt` with a fresh
un-calibrated config built from these tables. This breaks the
otherwise-circular dependency between the production fire config and the
calibration loop (production fire config -\> calibrated_fire_params -\>
calibration -\> production fire config).

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md),
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md),
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md),
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
