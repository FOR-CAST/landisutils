# Dominant cohort of each initial community

The cohort holding the most biomass on each map code of a replicate's
initial communities, among cohorts of species that have both a fire
tolerance and a longevity. These are the cohorts
[`landis_overstory_mortality_share()`](https://for-cast.github.io/landisutils/reference/landis_overstory_mortality_share.md)
scores. With
[`fire_cohort_killed()`](https://for-cast.github.io/landisutils/reference/fire_cohort_killed.md),
they score the share's `by_class` counts at any damage table without
re-reading the replicate.

## Usage

``` r
landis_dominant_cohorts(dir)
```

## Arguments

- dir:

  Character. Path to the replicate or scenario directory.

## Value

A data.frame with one row per map code that has such a cohort, in
map-code order: `MapCode` (integer), `SpeciesName` (character), and
`CohortAge`, `CohortBiomass`, `longevity` and `tolerance` (double). NULL
when one of the three files is missing or no cohort's species is in both
species files.

## Details

The files are those
[`landis_overstory_mortality_share()`](https://for-cast.github.io/landisutils/reference/landis_overstory_mortality_share.md)
reads: `initial-communities.csv`, the core species file `scenario.txt`
names (longevity) and the Dynamic Fire species table `dynamic-fire.txt`
names (fire tolerance), each falling back to the LANDIS-II example's
name when there is no directive to read. A species missing from either
species file cannot be dominant, whatever its biomass. A tie on biomass
goes to the cohort listed first.

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
