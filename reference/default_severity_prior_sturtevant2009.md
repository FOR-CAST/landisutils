# Default severity-class prior (Sturtevant et al. 2009)

Returns a named 5-element vector of expected proportions across the
integer severity classes (1 = low, 5 = high) produced by the Dynamic
Fire System. Default values are illustrative starting points derived
from the modelled distribution in the original Dynamic Fire extension
paper; callers should override with empirical priors when available for
their specific fire regime.

## Usage

``` r
default_severity_prior_sturtevant2009()
```

## Value

Named numeric vector of length 5, summing to 1.

## References

Sturtevant, B.R., Scheller, R.M., Miranda, B.R., Shinneman, D., and
Syphard, A. 2009. Simulating dynamic and mixed-severity fire regimes: A
process-based fire extension for LANDIS-II. Ecological Modelling
220(23): 3380-3393. <https://doi.org/10.1016/j.ecolmodel.2009.07.030>

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
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
