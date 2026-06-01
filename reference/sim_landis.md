# Run one DEoptim trial for a candidate parameter vector (blocking)

One calibration trial: copy `paths$scenario_template` into a scratch dir
under `paths$scratch_root`, patch `dynamic-fire.txt` with `par_vec`, run
LANDIS-II via the warm Docker pool (or a one-off Docker/local invocation
if no pool is supplied), parse the resulting Dynamic Fire logs.

## Usage

``` r
sim_landis(
  par_vec,
  par_names = NULL,
  paths,
  sim_years,
  base_seed,
  pool = NULL,
  pool_idx = NULL,
  method = NULL,
  pixel_area_ha = 1,
  keep_scratch = FALSE
)
```

## Arguments

- par_vec:

  Numeric. Named candidate parameter vector.

- par_names:

  Character or NULL. Names in canonical order
  ([`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md)).
  Used to re-attach names if DEoptim strips them when calling the
  objective function with positional args.

- paths:

  Named list of strings. Required entries:

  scenario_template

  :   Directory built by
      [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md).

  scratch_root

  :   Where per-trial dirs are created. Must equal the pool's
      `scratch_root` when `pool` is supplied. NULL =
      [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- sim_years:

  Integer. Calibration sim duration in years (informational; the actual
  Duration comes from the template's scenario.txt).

- base_seed:

  Integer. Random seed for this trial.

- pool:

  A `landis_pool` from
  [`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
  or NULL for one-off.

- pool_idx:

  Integer. 1-based container index in `pool`. Required when `pool` is
  non-NULL.

- method:

  Character. `"docker"` or `"local"`. Used only when `pool` is NULL.
  Default from `getOption("landisutils.run.method")`.

- pixel_area_ha:

  Numeric. Hectares per cell. Default 1.

- keep_scratch:

  Logical. Leave the per-trial scratch dir in place for debugging.
  Default FALSE.

## Value

The output of
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
for the trial's `rep01/`.

## Details

`paths` carries only file PATHS so the function is FORK-safe (no
terra/sf objects in the worker's environment).

Isolation between trials in the same pool container: each trial uses a
unique scratch directory;
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)
sets per-call env vars to redirect dotnet caches; the trial directory is
deleted after parsing unless `keep_scratch = TRUE`.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md),
[`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md),
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md),
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md),
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
