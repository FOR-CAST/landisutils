# Patch a `dynamic-fire.txt` in place with candidate calibration parameters

Surgical text replacements:

- `SeverityCalibrationFactor <x>` (single scalar line).

- `FireSizesTable` data rows: columns 8 (`SpHiProp`), 11 (`SumHiProp`),
  14 (`FallHiProp`) replaced. Shared across all ecoregion rows –
  per-ecoregion HiProp calibration would require 6 params not 3.

- `FuelTypeTable` data rows: column 4 (`IgnProb`) is multiplied by the
  base-type-specific candidate (e.g., `IgnProb_Conifer` for
  `Base == "Conifer"`). Default IgnProbs are mostly 1.0 (D1 = 0.5), so
  candidate range `[0, 1.5]` directly scales the relative-weighting.

## Usage

``` r
patch_fire_config(scenario_dir, par_vec)
```

## Arguments

- scenario_dir:

  Character. Directory containing `dynamic-fire.txt`.

- par_vec:

  Numeric. Named vector keyed by
  [`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md).

## Value

Character scalar: absolute path to the patched `dynamic-fire.txt`.

## Details

The file is patched in place; callers are expected to pass a per-trial
copy of the template so trials don't collide.

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
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
