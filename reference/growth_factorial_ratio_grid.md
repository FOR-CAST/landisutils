# Expand a shape-and-ratio sweep into absolute ForCS parameters

The sweep is specified as growth shape, mortality shape, and `anpp_prop`
– `anpp_max` as a percentage of `biomass_max`, the same ratio that
PredictiveEcology/Biomass_speciesFactorial sweeps as `mANPPproportion`.

## Usage

``` r
growth_factorial_ratio_grid(grid, growth_params)
```

## Arguments

- grid:

  A tibble with `species` and any of `growth_shp`, `mort_shp`,
  `anpp_prop`. One row per candidate value per parameter per species.

- growth_params:

  A data frame of the parameters currently in use; see
  [`growth_calibration_design()`](https://for-cast.github.io/landisutils/reference/growth_calibration_design.md).

## Value

A grid in the absolute form
[`growth_calibration_design()`](https://for-cast.github.io/landisutils/reference/growth_calibration_design.md)
expects.

## Details

`biomass_max` is NOT swept. It is pinned to each species' current
calibrated value and the level a candidate implies is recovered
arithmetically instead; see
[`growth_inflation_factor()`](https://for-cast.github.io/landisutils/reference/growth_inflation_factor.md).
Sweeping it alongside the shapes made the two inseparable: many (shape,
level) pairs produce near-identical curves, so the score traded them off
arbitrarily and settled wherever the reference cloud's centre happened
to sit.

Pinning per species rather than at one global constant – LandR uses 5000
for every species – keeps every simulation inside the range over which
the achieved-fraction invariance was actually checked here.
`biomass_max` spans 18200 to 48028 across these six species, so a single
constant would put some of them far outside it.

## See also

Other growth calibration helpers:
[`extract_landscape_cohort_structures()`](https://for-cast.github.io/landisutils/reference/extract_landscape_cohort_structures.md),
[`growth_add_objective()`](https://for-cast.github.io/landisutils/reference/growth_add_objective.md),
[`growth_auto_window()`](https://for-cast.github.io/landisutils/reference/growth_auto_window.md),
[`growth_best_candidates()`](https://for-cast.github.io/landisutils/reference/growth_best_candidates.md),
[`growth_bin_observations()`](https://for-cast.github.io/landisutils/reference/growth_bin_observations.md),
[`growth_calibration_design()`](https://for-cast.github.io/landisutils/reference/growth_calibration_design.md),
[`growth_calibration_partition()`](https://for-cast.github.io/landisutils/reference/growth_calibration_partition.md),
[`growth_calibration_work_root()`](https://for-cast.github.io/landisutils/reference/growth_calibration_work_root.md),
[`growth_climatic_distance()`](https://for-cast.github.io/landisutils/reference/growth_climatic_distance.md),
[`growth_climatic_weight()`](https://for-cast.github.io/landisutils/reference/growth_climatic_weight.md),
[`growth_expand_over_pseudo_species()`](https://for-cast.github.io/landisutils/reference/growth_expand_over_pseudo_species.md),
[`growth_fitting_windows()`](https://for-cast.github.io/landisutils/reference/growth_fitting_windows.md),
[`growth_identifiability()`](https://for-cast.github.io/landisutils/reference/growth_identifiability.md),
[`growth_inflation_factor()`](https://for-cast.github.io/landisutils/reference/growth_inflation_factor.md),
[`growth_pseudo_species_name()`](https://for-cast.github.io/landisutils/reference/growth_pseudo_species_name.md),
[`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md),
[`growth_reference_inflection()`](https://for-cast.github.io/landisutils/reference/growth_reference_inflection.md),
[`growth_score_fit()`](https://for-cast.github.io/landisutils/reference/growth_score_fit.md),
[`growth_scoring_for()`](https://for-cast.github.io/landisutils/reference/growth_scoring_for.md),
[`growth_structure_design()`](https://for-cast.github.io/landisutils/reference/growth_structure_design.md),
[`growth_window_for()`](https://for-cast.github.io/landisutils/reference/growth_window_for.md),
[`plot_growth_calibration()`](https://for-cast.github.io/landisutils/reference/plot_growth_calibration.md),
[`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md),
[`plot_growth_factorial_sensitivity()`](https://for-cast.github.io/landisutils/reference/plot_growth_factorial_sensitivity.md),
[`read_growth_scoring()`](https://for-cast.github.io/landisutils/reference/read_growth_scoring.md),
[`scale_linetype_growth_reference()`](https://for-cast.github.io/landisutils/reference/scale_linetype_growth_reference.md),
[`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
