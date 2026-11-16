# Burned cells by severity class, pooled over replicates

The simulated severity distribution counted per burned cell, which is
what
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md)
scores with `severity_weighting = "cell"` and what
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md)
draws with `weight_by = "cells"`. Class 0 (burned with no cohort
damaged) is dropped: the observed reference is a distribution over the
five severity classes, so counting a sixth category on the simulated
side only would shift every proportion.

## Usage

``` r
severity_cell_counts(reps)
```

## Arguments

- reps:

  List of
  [`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
  results, parsed with `by_class = TRUE`.

## Value

A named numeric vector over `"1"`..`"5"`, or NULL when no replicate
carries per-class counts – so a caller can tell "not collected" from
"nothing burned", which look the same in a bare count.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_damage_age()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_damage_age.md),
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`apply_calibrated_num_fires()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_num_fires.md),
[`area_by_fuel_base()`](https://for-cast.github.io/landisutils/reference/area_by_fuel_base.md),
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
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
