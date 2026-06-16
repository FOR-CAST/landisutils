# Default fuel-code -\> base-fuel-type mapping (BC FUEL_TYPE_CD factor levels)

Returns the mapping used by downstream projects that use the BC
`FUEL_TYPE_CD` factor encoding for `fuel_types_rast`. Levels correspond
to: 1=B71_S-2, 2=C-2, 3=C-3, 4=C-4, 5=C-5, 6=C-6, 7=C-7, 8=D-1/2,
9=M-1/2, 10=N (non-fuel), 11=O-1a/b, 12=S-1, 13=S-3. Mapped to the five
base types accepted by
[`defaultFuelTypeTable()`](https://for-cast.github.io/landisutils/reference/defaultFuelTypeTable.md)
/
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md).

## Usage

``` r
bc_fuel_code_to_base()
```

## Value

Character vector of length 13, names "1".."13", values `"Conifer"` /
`"ConiferPlantation"` / `"Deciduous"` / `"Slash"` / `"Open"` /
`NA_character_`.

## Details

Downstream projects with a different fuel-classification raster should
pass their own mapping vector to
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md)
via the `fuel_code_to_base` argument.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
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
