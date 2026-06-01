# Run the calibration spinup scenario (blocking)

Invokes LANDIS-II once against the scenario in `scenario_dir`, blocks
until completion, and verifies that the year-0 snapshot files emitted by
the Output Biomass Community extension landed on disk.

## Usage

``` r
run_calibration_spinup(
  scenario_dir,
  base_seed = 12345L,
  method = NULL,
  image = NULL,
  pull = FALSE
)
```

## Arguments

- scenario_dir:

  Character. Spinup scenario directory (containing `scenario.txt`),
  typically the return of
  [`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md).

- base_seed:

  Integer. Random seed passed to LANDIS-II via the per-rep
  `RandomNumberSeed` rewrite.

- method:

  Character. `"docker"` or `"local"`. Default from
  `getOption("landisutils.run.method")`.

- image:

  Character or NULL. Docker image (Docker only).

- pull:

  Logical. `docker pull` before running (Docker only). Default FALSE.

## Value

Character scalar: absolute path to the year-0 snapshot CSV
(`<scenario_dir>/rep01/community-input-file-0.csv`). The TIF
(`output-community-0.tif`) lives alongside.

## Details

Dispatches to
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md)
or
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
based on `method` – both are synchronous and stop on a non-zero exit, so
this wrapper only has to verify the expected files appeared.

Per LANDIS-II convention, scenarios are invoked from a numbered
replicate sub-directory (`rep01/`);
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md)
materialises that with a `base_seed`-derived `RandomNumberSeed`, then
the run happens inside it. Top-level `scenario_dir` stays clean (output
files land under `rep01/`).

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
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
