# Apply per-base-fuel-type IgnProb multipliers to a FuelTypeTable

Each row of `fuel_type_table` carries a `Base` column (one of
`"Conifer"`, `"ConiferPlantation"`, `"Deciduous"`, `"Slash"`, `"Open"`)
and an `IgnProb` column. This multiplies `IgnProb` row-wise by the
matching `IgnProb_<base>` entry in the calibrated parameter vector.
Defaults in
[`defaultFuelTypeTable()`](https://for-cast.github.io/landisutils/reference/defaultFuelTypeTable.md)
are mostly 1.0 (with `D1 = 0.5`), so a candidate range of `[0, 1.5]`
directly scales the relative ignition weighting.

## Usage

``` r
apply_calibrated_ignprob(fuel_type_table, calibrated_fire_params)
```

## Arguments

- fuel_type_table:

  data.frame from
  [`defaultFuelTypeTable()`](https://for-cast.github.io/landisutils/reference/defaultFuelTypeTable.md).
  Must have `Base` and `IgnProb` columns.

- calibrated_fire_params:

  Named numeric vector. Must include the five `IgnProb_<base>` entries
  from
  [`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md).

## Value

A copy of `fuel_type_table` with `IgnProb` updated.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
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
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)

Other Dynamic Fire helpers:
[`DynamicFire`](https://for-cast.github.io/landisutils/reference/DynamicFire.md),
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
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
