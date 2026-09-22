# Share of burned area that lost its dominant cohort

The proportion of a replicate's burned cells in which the cohort holding
the most biomass was killed. This is the model-side counterpart of a
field burn-severity class defined by mortality of the structurally
dominant vegetation, and unlike the extension's own severity classes –
which are crown fraction burned – it is comparable with an observed
mortality measure. In a forest that burns hot at ground level without
crowning, the two disagree by construction.

## Usage

``` r
landis_overstory_mortality_share(rep_dir)
```

## Arguments

- rep_dir:

  Character. Path to the replicate directory.

## Value

A list with `burned_cells`, `high_cells` and `share` (NA when nothing
burned), or NULL when the replicate holds no severity maps or is missing
one of the files above.

## Details

Everything needed is in the replicate directory, because a run is a copy
of its scenario:

- `fire/severity-{t}.tif` – the severity map per timestep. The Dynamic
  Fire encoding is 0 inactive, 1 active and unburned, 2 burned with no
  cohort damaged, and severity + 2 for a damaged cell, so a cell's
  severity class is its map value MINUS TWO.

- `initial-communities.csv` / `.tif` – the cohorts on each cell.

- `species.txt` – longevity, which the damage table's age thresholds are
  shares of.

- `DynamicFire_Spp_Table.csv` – each species' fire tolerance.

- `dynamic-fire.txt` – the `FireDamageTable`, mapping severity minus
  tolerance to the oldest cohort killed, as a percentage of longevity.

A cohort dies when severity is 5 (the extension kills everything at that
severity, whatever the tolerance) or when its age is at or below that
percentage of its species' longevity. Composition is read at time zero,
so a cell that burns twice in one replicate is scored on its original
cohorts; in a calibration run, where succession is frozen, only reburns
are affected.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`apply_calibrated_num_fires()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_num_fires.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md),
[`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md),
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md),
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
[`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md),
[`default_severity_prior_sturtevant2009()`](https://for-cast.github.io/landisutils/reference/default_severity_prior_sturtevant2009.md),
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
