# Compute the calibration loss from N replicate trial outputs

Combines per-replicate
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
outputs into the multi- component weighted loss against observed targets
from
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md).

## Usage

``` r
loss_from_stats(
  reps,
  observed,
  weights = c(count = 1, size = 1, area_fuel = 0, severity = 0)
)
```

## Arguments

- reps:

  List. Each element is the return value of
  [`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
  for one replicate.

- observed:

  List. Output of
  [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md).
  Must contain `$primary` (or `$fru59` back-compat alias) with
  `$lambda_obs`, `$n_fires_by_year`, `$fire_sizes_ha`. May contain
  `$primary$area_by_fuel_ha`, `$primary$severity_dist`,
  `$fuel_code_to_base`, and `$pixel_area_ha` to activate Tier 2
  components.

- weights:

  Named numeric vector. Components: `count`, `size`, `area_fuel`,
  `severity`. Missing components default to 0.

## Value

Named list with `total` (the scalar minimised by DEoptim), `components`
(per-component contributions), and `weights` (echoed weights).

## Details

Components:

- `L_count = |mean(n_fires_sim) - lambda_obs| / sd(n_fires_obs)` –
  annual-rate match against the primary ecoregion target.

- `L_size = KS_D(empirical CDF of sim sizes, empirical CDF of obs sizes)`
  – shape match for the fire-size distribution.

- `L_area_fuel`: chi-squared distance between simulated and observed
  burn-area-by-base-fuel-type *proportions*. Simulated area-by-fuel
  comes from each event's ignition fuel code times its `DamagedSites`,
  mapped to base fuel types via `observed$fuel_code_to_base`. Skipped
  (contributes 0) when either `observed$primary$area_by_fuel_ha` is NULL
  or `observed$fuel_code_to_base` is missing.

- `L_severity`: chi-squared distance between simulated and observed
  severity-class proportions. Simulated severities come from each
  event's `MeanSeverity` binned into integer classes 1..5; observed
  comes from `observed$primary$severity_dist` (a 5-element named numeric
  vector summing to 1). Skipped when observed is NULL.

All component values are unitless and non-negative; chi-squared
components use a small epsilon in the denominator to avoid division by
zero on empty observed bins.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md),
[`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md),
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md),
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
[`default_severity_prior_sturtevant2009()`](https://for-cast.github.io/landisutils/reference/default_severity_prior_sturtevant2009.md),
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
