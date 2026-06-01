# DEoptim driver for Dynamic Fire calibration

Sets up a warm Docker pool (for the `landis` simulator on Docker) and a
FORK cluster of `n_cores` workers, then invokes
[`DEoptim::DEoptim()`](https://rdrr.io/pkg/DEoptim/man/DEoptim.html)
with the multi-component loss as the objective. Pool + cluster are torn
down via [`on.exit()`](https://rdrr.io/r/base/on.exit.html) regardless
of success / error / interrupt.

## Usage

``` r
calibrate_dynamic_fire(observed_targets_path, scenario_template, cfg, out_dir)
```

## Arguments

- observed_targets_path:

  Character. Path to the `.rds` from
  [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md).

- scenario_template:

  Character. Path to the calibration scenario's `scenario.txt` (the
  return of
  [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md)).

- cfg:

  List. Calibration config. Expected keys:

  lower, upper

  :   Named numeric vectors keyed by
      [`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md).

  NP, itermax, strategy

  :   DEoptim control args.

  n_reps, sim_years, weights, base_seed

  :   Per-trial settings.

  n_cores, parallel

  :   Parallelism settings.

  simulator

  :   `"landis"` (default), `"r_reimpl"`, or `"mock"`.

  method

  :   `"docker"` (default) or `"local"`.

  image, cpu_limit, mem_limit, pull

  :   Pool settings (Docker only).

- out_dir:

  Character. Where to write the DEoptim trace + scratch sub-directory.
  Created if missing.

## Value

List with `best_params` (named numeric), `objective` (scalar), `deoptim`
(full DEoptim return), `trace_path` (CSV path), `cfg` (echo),
`pool_image` / `pool_digest` (provenance; NA when no pool was started).

## Details

Designed to be called from a `tar_target` with `deployment = "main"` so
the outer `targets` crew doesn't try to dispatch this as a single worker
while it manages its own internal cluster.

Per-worker container assignment: each FORK worker sets its
`LANDIS_POOL_CONTAINER_IDX` env var to its 1-based pool index.
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md)
reads this when running inside the worker.

DEoptim is gated on
[`requireNamespace("DEoptim")`](https://github.com/ArdiaD/DEoptim);
install via `renv::install("DEoptim")` before calling.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md),
[`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md),
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md),
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
