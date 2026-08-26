# Fraction of longevity at which age-related mortality begins

Biomass Succession expresses `MortalityCurve` as a position in the
lifespan, not as a rate: v7 User Guide 2.12.4 states that 5 puts the
onset of age-related mortality at 10% of life span and 25 puts it at
85%. This inverts that definition, linearly between the two documented
endpoints.

## Usage

``` r
growth_mortality_onset_frac(mort_shp)
```

## Arguments

- mort_shp:

  Numeric `MortalityCurve`, 5 to 25.

## Value

Numeric fraction of longevity, 0.10 to 0.85.

## Details

The parameter therefore carries no absolute meaning without the
`longevity` it sits beside, which is why a value cannot be lifted from
one parameterisation into another with a different lifespan.

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
[`growth_plot_palette()`](https://for-cast.github.io/landisutils/reference/growth_plot_palette.md),
[`growth_pseudo_species_name()`](https://for-cast.github.io/landisutils/reference/growth_pseudo_species_name.md),
[`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md),
[`growth_reference_inflection()`](https://for-cast.github.io/landisutils/reference/growth_reference_inflection.md),
[`growth_score_fit()`](https://for-cast.github.io/landisutils/reference/growth_score_fit.md),
[`growth_scoring_for()`](https://for-cast.github.io/landisutils/reference/growth_scoring_for.md),
[`growth_smooth_observations()`](https://for-cast.github.io/landisutils/reference/growth_smooth_observations.md),
[`growth_structure_cell_curves()`](https://for-cast.github.io/landisutils/reference/growth_structure_cell_curves.md),
[`growth_structure_cohort_table()`](https://for-cast.github.io/landisutils/reference/growth_structure_cohort_table.md),
[`growth_structure_design()`](https://for-cast.github.io/landisutils/reference/growth_structure_design.md),
[`growth_structure_summary()`](https://for-cast.github.io/landisutils/reference/growth_structure_summary.md),
[`growth_window_for()`](https://for-cast.github.io/landisutils/reference/growth_window_for.md),
[`plot_growth_calibration()`](https://for-cast.github.io/landisutils/reference/plot_growth_calibration.md),
[`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md),
[`plot_growth_factorial_sensitivity()`](https://for-cast.github.io/landisutils/reference/plot_growth_factorial_sensitivity.md),
[`plot_growth_structures()`](https://for-cast.github.io/landisutils/reference/plot_growth_structures.md),
[`read_growth_scoring()`](https://for-cast.github.io/landisutils/reference/read_growth_scoring.md),
[`read_landscape_cohort_structures()`](https://for-cast.github.io/landisutils/reference/read_landscape_cohort_structures.md),
[`scale_linetype_growth_reference()`](https://for-cast.github.io/landisutils/reference/scale_linetype_growth_reference.md),
[`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
