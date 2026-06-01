# Parse a Dynamic Fire run's event and summary logs

Reads `<rep_dir>/fire/dynamic-fire-event-log.csv` (one row per fire
event) and `<rep_dir>/fire/dynamic-fire-summary-log.csv` (one row per
simulation year per fire ecoregion), returning a small list of summary
statistics suitable for loss-function comparison.

## Usage

``` r
parse_dynamic_fire_logs(rep_dir, pixel_area_ha = 1)
```

## Arguments

- rep_dir:

  Character. Path to the per-rep directory (the `rep01/` under the
  scenario directory). Must contain `fire/dynamic-fire-event-log.csv`
  and `fire/dynamic-fire-summary-log.csv`.

- pixel_area_ha:

  Numeric. Hectares per cell. Default `1.0`.

## Value

Named list with `n_fires_by_year` (tibble: `year`, `n_fires`),
`fire_sizes_ha` (sorted numeric vector), `events` (per-event tibble),
`total_sites_burned` (integer), `n_events` (integer).

## Details

Columns parsed (Dynamic Fire System v4):

- event-log: `Time`, `InitFireRegion`, `InitFuel`, `DamagedSites`,
  `MeanSeverity`.

- summary-log: `Time`, `NumberFires`, `TotalSitesBurned`.

Cells -\> hectares uses `pixel_area_ha` (1 ha for a 100 m x 100 m grid).

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md),
[`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md),
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md),
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
