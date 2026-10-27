# Whether a fire kills a cohort

Dynamic Fire's damage rule for one cohort, vectorised over cells or
cohorts. Severity 5 kills every cohort. At severities 1 to 4, the
`FireDamageTable` row that severity minus the species' fire tolerance
picks – the row with the largest difference at or below it, as
[`base::findInterval()`](https://rdrr.io/r/base/findInterval.html) finds
it – gives the oldest cohort killed, as a percentage of longevity, and
the cohort dies when its age is at or below that share. Below the
table's smallest difference nothing dies. Any other severity, including
the 0 of a burned cell in which no cohort was damaged, kills nothing,
and so does a missing value in any input.

## Usage

``` r
fire_cohort_killed(
  severity,
  age,
  longevity,
  tolerance,
  fire_damage_table,
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

- engine_rule:

  Logical. Make the age test in single precision, as the extension does?

## Value

Logical, one per `severity`: TRUE where the cohort dies.

## Details

By default the age test is made in double precision,
`100 * age / longevity <= pct`, as
[`landis_overstory_mortality_share()`](https://for-cast.github.io/landisutils/reference/landis_overstory_mortality_share.md)
has always made it. With `engine_rule = TRUE` it is made as the
extension makes it (`Event.cs`, lines 484-490):
`pct / 100 >= age / longevity`, with `age / longevity` rounded to single
precision. The two can differ only where `age / longevity` is within
single-precision rounding of a threshold, as it is when the threshold is
a whole number of years: at 10 % of 300 years, a 30-year-old cohort dies
by the first test and survives the second.

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
## Tolerance 3 and a 100-year-old cohort of a 300-year species: severity 1 spares it, 2 and 5
## kill it.
fire_cohort_killed(c(1, 2, 5), 100, 300, 3, defaultFireDamageTable())
#> [1] FALSE  TRUE  TRUE
```
