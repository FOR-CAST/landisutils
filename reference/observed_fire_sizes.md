# Observed per-fire sizes: one per ignition point, upgraded to mapped area

One size per ignition point. Each point keeps its own `SIZE_HA` unless a
perimeter polygon from the SAME calendar year contains it, in which case
the polygon's `SIZE_HA` replaces it. For NFDB points and NBAC
perimeters, this keeps NFDB's full sample (pre-1972 fires, and small
fires NBAC does not map) while taking NBAC's satellite-derived area
wherever a fire was mapped.

## Usage

``` r
observed_fire_sizes(points, polys = NULL, min_size_ha = 0)
```

## Arguments

- points:

  SpatVector. Ignition points with `SIZE_HA` and `YEAR` columns.

- polys:

  SpatVector or NULL. Perimeter polygons with `SIZE_HA` and `YEAR`
  columns. NULL keeps every point's own size.

- min_size_ha:

  Numeric scalar. Sizes below this, and missing sizes, are dropped.
  Default `0` keeps every positive and zero size.

## Value

Numeric vector of sizes (ha), sorted ascending; at most one element per
point.

## Details

This is the fire-size rule behind
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md)'s
`fire_sizes_ha`. It is exported so that anything else derived from the
same fire record – such as a fitted fire-size distribution – uses the
same sizes as the calibration's size target, rather than a second rule
that can drift from it. Binding points and polygons as separate rows
instead would count every mapped fire twice.

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
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md),
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
