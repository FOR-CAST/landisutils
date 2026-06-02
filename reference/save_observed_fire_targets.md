# Save observed fire-regime targets (NFDB-derived) for calibration loss

Pre-computes per-ecoregion observed summaries that downstream
calibration loss components
([`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md))
compare simulated fires against. Saves a single small `.rds` payload of
base R types – so DEoptim workers can read it from disk without terra/sf
in their environments.

## Usage

``` r
save_observed_fire_targets(
  primary_points,
  primary_polys,
  fire_years,
  fuel_types_rast,
  path,
  secondary_points = NULL,
  secondary_polys = NULL,
  primary_label = "primary",
  secondary_label = "secondary",
  fuel_code_to_base = bc_fuel_code_to_base(),
  severity_dist = NULL
)
```

## Arguments

- primary_points, primary_polys:

  SpatVector. NFDB ignition points and fire polygons for the primary
  ecoregion (the LANDIS simulation extent).

- fire_years:

  Integer vector. Years over which counts are normalised (denominator
  for `lambda_obs`).

- fuel_types_rast:

  SpatRaster. Integer-coded fuel-type raster covering the LANDIS
  simulation extent.

- path:

  Character. Output `.rds` path. Parent dir created if missing.

- secondary_points, secondary_polys:

  SpatVector or NULL. Same, for an optional regional-context ecoregion.
  `area_by_fuel_ha` is NOT computed for the secondary (see Details).

- primary_label, secondary_label:

  Character. Labels for the two ecoregions (e.g., `"FRU59"` /
  `"FRT12"`). Stored in the payload for reproducibility.

- fuel_code_to_base:

  Named character vector. Mapping from `fuel_types_rast` integer codes
  (as character names) to the five base fuel types from
  [`defaultFuelTypeTable()`](https://for-cast.github.io/landisutils/reference/defaultFuelTypeTable.md).
  NA values mark non-fuel codes to be excluded. Default:
  [`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md).

- severity_dist:

  Named numeric vector or NULL. Expected proportions across the 5
  Dynamic Fire severity classes (names `"1"`..`"5"`). Stored on the
  primary-ecoregion summary; consumed by
  [`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md)'s
  `L_severity` component. NULL = skip severity calibration (the loss
  contributes 0). For a literature-prior default, see
  [`default_severity_prior_sturtevant2009()`](https://for-cast.github.io/landisutils/reference/default_severity_prior_sturtevant2009.md).

## Value

Character. Absolute path to the written file.

## Details

Loss-component consumers:

- `L_count` uses `n_fires_by_year` (mean + sd for normalisation).

- `L_size` uses `fire_sizes_ha` (sorted vector; KS test against sim).

- `L_area_fuel` uses primary-ecoregion `area_by_fuel_ha` (Tier 2; weight
  0 in Tier 1).

- `L_severity` stays NULL; populate from literature priors when Tier 2
  severity matching is implemented.

Per ecoregion (`primary_ecoregion`, `secondary_ecoregion`):

- Fire counts come from NFDB IGNITION POINTS (one row = one ignition).
  NFDB polygons are sparser (only mapped for larger fires).

- Fire sizes come from NFDB points' `SIZE_HA` column (zeros dropped to
  keep the lognormal-flavoured size distribution positive).

- `area_by_fuel_ha` is computed for the PRIMARY ecoregion only via
  polygon overlay on `fuel_types_rast`. `fuel_types_rast` covers the
  LANDIS simulation domain; secondary-ecoregion polygons typically
  extend well beyond that extent, making a secondary computation
  misleading (it would just be the primary value over again).

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
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
