# Overwrite FireSizesTable Sp/Sum/Fall HiProp columns with calibrated values

Replaces `SpHiProp`, `SumHiProp`, `FallHiProp` in every row of
`fire_size_table` with the calibrated triple (shared across all
ecoregion rows).

## Usage

``` r
apply_calibrated_hi_prop(fire_size_table, calibrated_fire_params)
```

## Arguments

- fire_size_table:

  data.frame as produced by a project's
  `make_fire_size_table()`-equivalent. Must have columns `SpHiProp`,
  `SumHiProp`, `FallHiProp`.

- calibrated_fire_params:

  Named numeric vector. Must include `SpHiProp`, `SumHiProp`,
  `FallHiProp` entries.

## Value

A copy of `fire_size_table` with the three HiProp columns updated.

## See also

Other Dynamic Fire calibration helpers:
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

Other Dynamic Fire helpers:
[`DynamicFire`](https://for-cast.github.io/landisutils/reference/DynamicFire.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`defaultFuelTypeTable()`](https://for-cast.github.io/landisutils/reference/defaultFuelTypeTable.md),
[`insertBuildUpIndex()`](https://for-cast.github.io/landisutils/reference/insertBuildUpIndex.md),
[`insertFireSizesTable()`](https://for-cast.github.io/landisutils/reference/insertFireSizesTable.md),
[`insertFuelTypeTable()`](https://for-cast.github.io/landisutils/reference/insertFuelTypeTable.md),
[`insertGroundSlopeFile()`](https://for-cast.github.io/landisutils/reference/insertGroundSlopeFile.md),
[`insertSeasonTable()`](https://for-cast.github.io/landisutils/reference/insertSeasonTable.md),
[`insertUphillSlopeAzimuthMap()`](https://for-cast.github.io/landisutils/reference/insertUphillSlopeAzimuthMap.md),
[`prepDynamicEcoregionTable()`](https://for-cast.github.io/landisutils/reference/prepDynamicEcoregionTable.md),
[`prepDynamicWeatherTable()`](https://for-cast.github.io/landisutils/reference/prepDynamicWeatherTable.md),
[`prepFireSizesTable()`](https://for-cast.github.io/landisutils/reference/prepFireSizesTable.md),
[`prepInitialWeatherDatabase()`](https://for-cast.github.io/landisutils/reference/prepInitialWeatherDatabase.md),
[`prepTopographyFile()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md)
