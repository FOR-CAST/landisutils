# Compute the calibration loss from N replicate trial outputs

Combines per-replicate
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
outputs into the multi- component weighted loss against observed targets
from
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md)
(Phase 8b).

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
  [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md)
  (Phase 8b). Must contain `$fru59` with `$lambda_obs`,
  `$n_fires_by_year`, `$fire_sizes_ha`.

- weights:

  Named numeric vector. Components: `count`, `size`, `area_fuel`,
  `severity`. Missing components default to 0.

## Value

Named list with `total` (the scalar minimised by DEoptim), `components`
(per-component contributions), and `weights` (echoed weight vector).

## Details

Tier 1 implementation: `L_count` + `L_size`.

- `L_count = |mean(n_fires_sim) - lambda_obs| / sd(n_fires_obs)` –
  annual-rate match against the primary ecoregion target (default
  `fru59`).

- `L_size = KS_D(empirical CDF of sim sizes, empirical CDF of obs sizes)`
  – shape match between simulated and observed fire-size distributions.

Tier 2 (future): `L_area_fuel` and `L_severity`. Stubbed here as zeros
when the corresponding observed component is NULL; weights default to 0
so they contribute nothing until implemented.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md),
[`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md),
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md),
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
