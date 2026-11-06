# Expected mortality of a cohort in a fire

The companion of
[`fire_cohort_killed()`](https://for-cast.github.io/landisutils/reference/fire_cohort_killed.md)
for species with a `GroundFireKill`. Where the value is NA the answer is
[`fire_cohort_killed()`](https://for-cast.github.io/landisutils/reference/fire_cohort_killed.md)'s,
as 0 or 1. Where a species has a value, Dynamic Fire takes it out of the
damage table below severity 5: in each burned cell all its cohorts die
together with that probability, whatever their age. This returns that
probability.

## Usage

``` r
fire_cohort_mortality(
  severity,
  age,
  longevity,
  tolerance,
  fire_damage_table,
  ground_fire_kill = NA_real_,
  engine_rule = FALSE
)
```

## Arguments

- severity:

  Numeric. Severity class: a severity map's value minus 2.

- age, longevity, tolerance:

  Numeric. The cohort's age, and its species' longevity and fire
  tolerance. Each has length 1 or the length of `severity`.

- fire_damage_table:

  data.frame. The `FireDamageTable`, in
  [`defaultFireDamageTable()`](https://for-cast.github.io/landisutils/reference/defaultFireDamageTable.md)'s
  column order: the percentage of longevity first, severity minus
  tolerance second. Rows may be in any order.

- ground_fire_kill:

  Numeric. The species' `GroundFireKill`, between 0 and 1, or NA for
  none. Length 1 or the length of `severity`.

- engine_rule:

  Logical. Make the age test in single precision, as the extension does?

## Value

Numeric, one per `severity`: the probability that the cohort dies.

## Details

Severity 5 gives 1. Severity 1 to 4 gives `ground_fire_kill`. Severity
0, a burned cell in which no cohort was damaged, gives 0. A
`ground_fire_kill` of 0 is a value (the species leaves the table and
never dies below severity 5), not an absent one.

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

## Examples

``` r
## A species that dies below severity 5 in 30 % of burned cells, at any age.
fire_cohort_mortality(c(0, 1, 4, 5), 600, 650, 1, defaultFireDamageTable(), 0.3)
#> [1] 0.0 0.3 0.3 1.0
```
