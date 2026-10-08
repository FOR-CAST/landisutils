# Observed per-fire sizes: one per ignition point, upgraded to mapped area

One size per ignition point. Each point keeps its own `SIZE_HA` unless a
perimeter polygon from the SAME calendar year contains it, in which case
the polygon's `SIZE_HA` replaces it. For NFDB points and NBAC
perimeters, this keeps NFDB's full sample (pre-1972 fires, and small
fires NBAC does not map) while taking NBAC's satellite-derived area
wherever a fire was mapped.

## Usage

``` r
observed_fire_sizes(
  points,
  polys = NULL,
  min_size_ha = 0,
  ignitions = points,
  ignition_min_ha = 1,
  conflict_m = 500,
  part_gap_m = 250
)
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

- ignitions:

  SpatVector. Ignition points with `SIZE_HA` and `YEAR` columns, in the
  CRS of `points`, tested as other fires in or near a polygon. Every
  point of `ignition_min_ha` or more that a same-year polygon contains
  must be among them, at the same coordinates. Default `points`.

- ignition_min_ha:

  Numeric scalar, not negative. Smallest ignition (ha) taken to be
  another fire. Default `1`.

- conflict_m:

  Numeric scalar, not negative. Distance (m) within which another
  ignition disputes a polygon or a group of its parts. Default `500`.

- part_gap_m:

  Numeric scalar, not negative. Largest distance (m) between parts of a
  polygon that are grouped as one fire. Default `250`.

## Value

Numeric vector of sizes (ha), sorted ascending; at most one element per
point.

## Details

A polygon can hold more than one fire: NBAC maps some neighbouring fires
of the same year as parts of one feature, whose `SIZE_HA` is the area of
them all. A point therefore takes the polygon's `SIZE_HA` only when no
other ignition of that year, of `ignition_min_ha` or more, lies inside
the polygon or within `conflict_m` of it. Otherwise the polygon's parts
are grouped, joining parts no more than `part_gap_m` apart, and the
point takes the area of its own group, measured from the geometry, when
no other such ignition lies in or within `conflict_m` of that group, or
keeps its own `SIZE_HA` when one does. A fire mapped in several parts
with no other ignition near it keeps the whole polygon's size. Because
the test measures distance to the whole polygon, pass polygons that have
not been clipped to a study area, and take `ignitions` from the whole
point record: a fire that burned into a region may have started outside
it. The defaults were chosen by comparing candidate rules with
agency-mapped fire perimeters. `conflict_m` and `part_gap_m` are in
metres whatever the CRS's linear unit; a CRS that states no unit is
taken to be in metres.

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
[`bc_fuel_label_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_label_to_base.md),
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
