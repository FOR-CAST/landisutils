# Score each observation by climatic distance from a target

Ground plots from a wider area can inform a calibration, but only in
proportion to how much their climate resembles the landscape being
modelled. This scores each observation individually, which is the point:
aggregating plots into map units first and comparing unit means makes
the comparison only as reliable as the thinnest unit, and map units
carrying a handful of plots get estimates too noisy to rank.

## Usage

``` r
growth_climatic_distance(climate, target, vars = names(target), scale = NULL)
```

## Arguments

- climate:

  A data frame with one row per observation and one column per climate
  variable.

- target:

  Named numeric vector giving the target climate.

- vars:

  Character. Variables to compare on; defaults to the names of `target`.

- scale:

  Optional named numeric vector of per-variable spreads. Defaults to
  each variable's standard deviation across `climate`.

## Value

A numeric vector of distances, one per row of `climate`.

## Details

Distance is the root-mean-square deviation across variables after
standardizing each by its spread, so that a variable measured in
millimetres does not swamp one measured in degrees.

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
[`growth_climatic_weight()`](https://for-cast.github.io/landisutils/reference/growth_climatic_weight.md),
[`growth_expand_over_pseudo_species()`](https://for-cast.github.io/landisutils/reference/growth_expand_over_pseudo_species.md),
[`growth_factorial_ratio_grid()`](https://for-cast.github.io/landisutils/reference/growth_factorial_ratio_grid.md),
[`growth_fitting_windows()`](https://for-cast.github.io/landisutils/reference/growth_fitting_windows.md),
[`growth_identifiability()`](https://for-cast.github.io/landisutils/reference/growth_identifiability.md),
[`growth_inflation_factor()`](https://for-cast.github.io/landisutils/reference/growth_inflation_factor.md),
[`growth_mortality_onset_frac()`](https://for-cast.github.io/landisutils/reference/growth_mortality_onset_frac.md),
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
