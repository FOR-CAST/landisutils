# A replicate's burned area by fuel type, with its base fuel type

Returns a replicate's `area_by_fuel_ha` (from
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md))
with its `base` column, the base fuel type of each fuel type in the
run's own `FuelTypeTable`. A replicate parsed before landisutils 0.0.168
carries only `fuel_code`, the raw FuelType map value, which is the
fuel-type index + 1; its base is then decoded through
[`defaultFuelTypeTable()`](https://for-cast.github.io/landisutils/reference/defaultFuelTypeTable.md)
– right for a run whose FuelTypeTable kept the default indices – and
that assumption is announced once per session.

## Usage

``` r
area_by_fuel_base(d)
```

## Arguments

- d:

  A replicate's `area_by_fuel_ha` data frame.

## Value

`d` as a data frame with a `base` column.

## Details

This is the decoding
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md)'s
area-by-fuel term and
[`plot_calibration_area_by_fuel()`](https://for-cast.github.io/landisutils/reference/plot_calibration_area_by_fuel.md)
apply, exported so that a figure drawn outside the package reads
simulated fuel types the same way.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_damage_age()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_damage_age.md),
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`apply_calibrated_num_fires()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_num_fires.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`bc_fuel_label_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_label_to_base.md),
[`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md),
[`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md),
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md),
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
[`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md),
[`default_severity_prior_sturtevant2009()`](https://for-cast.github.io/landisutils/reference/default_severity_prior_sturtevant2009.md),
[`fire_cohort_killed()`](https://for-cast.github.io/landisutils/reference/fire_cohort_killed.md),
[`fire_cohort_mortality()`](https://for-cast.github.io/landisutils/reference/fire_cohort_mortality.md),
[`ground_fire_kill_par_names()`](https://for-cast.github.io/landisutils/reference/ground_fire_kill_par_names.md),
[`landis_dominant_cohorts()`](https://for-cast.github.io/landisutils/reference/landis_dominant_cohorts.md),
[`landis_overstory_mortality_share()`](https://for-cast.github.io/landisutils/reference/landis_overstory_mortality_share.md),
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md),
[`observed_fire_sizes()`](https://for-cast.github.io/landisutils/reference/observed_fire_sizes.md),
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`severity_cell_counts()`](https://for-cast.github.io/landisutils/reference/severity_cell_counts.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
