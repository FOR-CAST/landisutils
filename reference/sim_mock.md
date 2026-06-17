# Mock simulator backend for testing the calibration driver without LANDIS-II

Returns plausibly-shaped
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
output without invoking the real simulator. The output varies with
`par_vec` so DEoptim sees a non-trivial loss surface (a few of the
calibrated parameters bias the mock's fire count and size distribution;
this is illustrative, not biophysical).

## Usage

``` r
sim_mock(
  par_vec,
  par_names = NULL,
  paths = NULL,
  sim_years = 10L,
  base_seed = 1L,
  ...
)
```

## Arguments

- par_vec:

  Numeric. Named candidate parameter vector.

- par_names:

  Character. Names in canonical order
  ([`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md)).

- paths:

  Named list. Currently unused; accepted for
  [`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md)
  signature parity.

- sim_years:

  Integer. Number of simulated years.

- base_seed:

  Integer. RNG seed for deterministic mock output.

- ...:

  Ignored. Lets callers pass `pool`, `pool_idx`, `method`, etc.

## Value

A list matching the shape of
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
output.

## Details

Use this in unit tests of
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
when Docker is not available; do NOT use for actual calibration.

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
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md),
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
