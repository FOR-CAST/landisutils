# Apply per-base-fuel-type IgnProb multipliers to a FuelTypeTable

Each row of `fuel_type_table` carries a `Base` column (one of
`"Conifer"`, `"ConiferPlantation"`, `"Deciduous"`, `"Slash"`, `"Open"`)
and an `IgnProb` column. This multiplies `IgnProb` row-wise by the
matching `IgnProb_<base>` entry in the calibrated parameter vector.

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

## Multipliers above `1 / default` are inert

LANDIS-II requires `IgnProb` in `[0, 1]`, so the product is clamped to
that range. The defaults in
[`defaultFuelTypeTable()`](https://for-cast.github.io/landisutils/reference/defaultFuelTypeTable.md)
are 1.0 for every base except `Deciduous` (`D1`), which is 0.5. A
`Conifer` multiplier above 1.0 is therefore clamped away entirely, and a
`Deciduous` multiplier of 2.0 maps to exactly the ceiling. Useful search
bounds are `[0, 1]` for the 1.0 defaults and `[0, 2]` for `Deciduous`;
anything wider searches a flat region.

This matters when reading a finished calibration. A multiplier that
comes back pinned at such a bound is **not** an estimate that wanted
more room – it is saturation, meaning the objective wanted more fire
than the maximum ignition probability can deliver. Widening the bound is
a no-op. The remaining lever is `NumFires` in the fire-size table, which
is a fixed input derived from the observed record rather than a
calibrated parameter, so a pinned multiplier is a signal to check the
count target and the objective – start with whether the simulated annual
rate is being computed over the right number of years – rather than to
re-run with a wider box.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md),
[`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md),
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md),
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
[`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md),
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
