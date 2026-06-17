# Re-simulate at the calibrated parameter vector for goodness-of-fit plots

Runs `n_reps` replicate Dynamic Fire simulations at the calibrated
parameter vector and returns their per-replicate fire statistics plus
the loss against the observed targets, so a calibration report can plot
the goodness-of-fit that
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
does not retain.

## Usage

``` r
run_calibration_validation(
  scenario_template,
  best_params,
  observed_targets_path,
  cfg,
  n_reps = 20L,
  scratch_root,
  base_seed = 99999L
)
```

## Arguments

- scenario_template:

  Character path to `scenario.txt` inside the calibration scenario
  directory.

- best_params:

  Named numeric vector of calibrated parameters (the
  [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  result's `best_params`).

- observed_targets_path:

  Character path to the saved observed-targets `.rds` (the
  [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md)
  output); reattached to the return value so the report has a single
  self-contained input.

- cfg:

  The `calibration_config` list (for `sim_years`, `weights`).

- n_reps:

  Integer number of replicate simulations at `best_params`.

- scratch_root:

  Docker-visible host directory for the warm pool bind-mount (distinct
  from the main calibration scratch so concurrent runs do not collide).

- base_seed:

  Base seed for the validation reps (`base_seed + i` per rep).

## Value

A list with `reps`, `best_params`, `observed`, `pixel_area_ha`,
`sim_years`, `n_reps`, `loss`, `pool_image`, and `pool_digest`.

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
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
