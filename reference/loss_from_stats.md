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
  weights = c(count = 1, size = 1, size_tail = 1, area_fuel = 0, severity = 0)
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

  Named numeric vector. Components: `count`, `size`, `size_tail`,
  `area_fuel`, `severity`, `mortality`, `area_burned`. Missing
  components default to 0, so an existing caller's weights keep their
  meaning when a component is added.

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
  comes from each event's ignition fuel code times its burned cells,
  mapped to base fuel types via `observed$fuel_code_to_base`. Skipped
  (contributes 0) when either `observed$primary$area_by_fuel_ha` is NULL
  or `observed$fuel_code_to_base` is missing.

- `L_severity`: chi-squared distance between simulated and observed
  severity-class proportions. Simulated severities come from each
  event's `MeanSeverity` binned into integer classes 1..5; observed
  comes from `observed$primary$severity_dist` (a 5-element named numeric
  vector summing to 1). Skipped when observed is NULL.

- `L_mortality = |mean(share_sim) - share_obs| / share_obs` – the share
  of burned area that lost its dominant cohort, against the same share
  observed. Contributes 0 when `observed$primary$mortality_share` is
  NULL or NA, or when no replicate kept the severity maps it is measured
  from.

- `L_area_burned = |log10(area_sim / area_obs)|` – annual area burned,
  simulated against observed. No other component scores how much area
  burns: `L_area_fuel` scores how burned area is distributed across base
  fuel types, not how much there is. A log10 ratio keeps it scale-free,
  so the same weight means the same thing on study areas whose burn
  rates differ by orders of magnitude. Simulated area is summed from
  each replicate's events over the years `L_count` scores, converted
  with `observed$pixel_area_ha`; observed area is
  `sum(fire_sizes_ha) / n_years`. Contributes 0 when the observed
  payload cannot supply an annual rate, and `.AREA_BURNED_NO_FIRE` (3.0)
  when a replicate set burns nothing, since `log10(0)` would be infinite
  and DEoptim cannot rank an infinite objective.

          This term and `count` both move with the number of fires, so they
          compete for the same lever wherever a calibration scales ignition
          rates. `count` is normalised by the observed year-to-year standard
          deviation, making it steeper by about
          `lambda_obs / sd(n_fires_obs) * ln(10)`; keep
          `weights["area_burned"]` well below `weights["count"]` times that
          factor, or the fitted fire count is pulled off its own target to
          compensate for a fire-size distribution the search cannot change.

All component values are unitless and non-negative; chi-squared
components use a small epsilon in the denominator to avoid division by
zero on empty observed bins.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_damage_age()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_damage_age.md),
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
[`landis_overstory_mortality_share()`](https://for-cast.github.io/landisutils/reference/landis_overstory_mortality_share.md),
[`observed_fire_sizes()`](https://for-cast.github.io/landisutils/reference/observed_fire_sizes.md),
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
