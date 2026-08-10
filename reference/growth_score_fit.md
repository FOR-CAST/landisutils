# Score a simulated growth curve against its references

Ranks on SHAPE alone. Each reference series is compared against the
simulated curve rescaled to that series' own plateau, so a candidate is
never rewarded for landing at the right level with the wrong trajectory,
nor penalized for the reverse – the level is recovered separately and
exactly.

## Usage

``` r
growth_score_fit(
  curve,
  ref,
  level_source = NA_character_,
  weights = c(sortie = 1, tipsy = 1, vdyp = 1, plots = 1),
  biomass_max_scale = 200
)
```

## Arguments

- curve:

  A tibble with `age`, `aboveground_c_mg_ha`, `anpp_max` and
  `biomass_max` for one combination.

- ref:

  A list from
  [`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md).

- level_source:

  Character. Which series' plateau to report a `biomass_max`
  recommendation against; `NA` picks the first available of SORTIE,
  TIPSY, plots.

- weights:

  Named numeric. Relative weight per reference series in the ranking.

- biomass_max_scale:

  Numeric. Passed to
  [`growth_inflation_factor()`](https://for-cast.github.io/landisutils/reference/growth_inflation_factor.md).

## Value

A one-row tibble of fit statistics.

## Details

Errors are normalized by each series' level (`nrmse_*`) before being
averaged across series, since a hemlock curve plateauing near 240 Mg C
ha^-1 and a pine curve near 90 would otherwise contribute incomparable
residuals.

The two reference kinds answer different questions, so their relative
weight is a judgement the calibration must not make silently. SORTIE and
TIPSY are potential yield curves for fully stocked, pure, undisturbed
stands, which is exactly what a single-cohort calibration cell is.
Ground plots are realized stands, spanning every stocking level and
disturbance history, with whole-plot volume attributed to a leading
species that holds a median of 69% of the stand. Weight them with
`weight_sortie` / `weight_plots` in `growth_scoring.csv`; setting one to
0 drops it from the ranking while leaving it on the review figures.

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
[`growth_factorial_ratio_grid()`](https://for-cast.github.io/landisutils/reference/growth_factorial_ratio_grid.md),
[`growth_fitting_windows()`](https://for-cast.github.io/landisutils/reference/growth_fitting_windows.md),
[`growth_identifiability()`](https://for-cast.github.io/landisutils/reference/growth_identifiability.md),
[`growth_inflation_factor()`](https://for-cast.github.io/landisutils/reference/growth_inflation_factor.md),
[`growth_pseudo_species_name()`](https://for-cast.github.io/landisutils/reference/growth_pseudo_species_name.md),
[`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md),
[`growth_reference_inflection()`](https://for-cast.github.io/landisutils/reference/growth_reference_inflection.md),
[`growth_scoring_for()`](https://for-cast.github.io/landisutils/reference/growth_scoring_for.md),
[`growth_smooth_observations()`](https://for-cast.github.io/landisutils/reference/growth_smooth_observations.md),
[`growth_structure_design()`](https://for-cast.github.io/landisutils/reference/growth_structure_design.md),
[`growth_window_for()`](https://for-cast.github.io/landisutils/reference/growth_window_for.md),
[`plot_growth_calibration()`](https://for-cast.github.io/landisutils/reference/plot_growth_calibration.md),
[`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md),
[`plot_growth_factorial_sensitivity()`](https://for-cast.github.io/landisutils/reference/plot_growth_factorial_sensitivity.md),
[`read_growth_scoring()`](https://for-cast.github.io/landisutils/reference/read_growth_scoring.md),
[`scale_linetype_growth_reference()`](https://for-cast.github.io/landisutils/reference/scale_linetype_growth_reference.md),
[`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
