# Write a standalone calibration review bundle

Per-species review figures plus a one-page summary table, written to a
plain directory that needs no Quarto render and no pipeline knowledge to
inspect.

## Usage

``` r
write_growth_review_bundle(
  dir,
  species,
  curves,
  candidate_curves = NULL,
  references,
  reference_curves = NULL,
  best,
  windows,
  scoring_file = "growth_scoring.csv",
  params_file = "the growth-parameter table",
  smooth_plots = TRUE,
  smooth_bin = 20L,
  smooth_site = NULL
)
```

## Arguments

- dir:

  Character. Output directory; created if absent.

- species:

  Character vector of species to write.

- curves:

  Simulated curves keyed by species (the verification run).

- candidate_curves:

  Optional simulated curves for the best candidate.

- references:

  Named list of reference tables, one per species.

- reference_curves:

  Named list from
  [`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md),
  one per species, supplying the age-binned ground-plot series.

- best:

  A tibble of best-candidate parameters and fit statistics.

- windows:

  A tibble from
  [`growth_fitting_windows()`](https://for-cast.github.io/landisutils/reference/growth_fitting_windows.md).

- scoring_file, params_file:

  Character. Paths named in the bundle's `README.txt`, so a reviewer is
  pointed at the files this project actually keeps them in.

- smooth_plots:

  Logical. Overlay a spline through the ground-plot cloud on each panel,
  for comparison against the binned series. Display only; nothing is
  scored against it. See
  [`growth_smooth_observations()`](https://for-cast.github.io/landisutils/reference/growth_smooth_observations.md).

- smooth_bin, smooth_site:

  Passed to
  [`growth_smooth_observations()`](https://for-cast.github.io/landisutils/reference/growth_smooth_observations.md).
  `smooth_bin` may be a single width or a vector named by species. Set
  `smooth_site` to the location column wherever the plots are a
  permanent network, so the fit and the binned series rest on the same
  evidence.

## Value

Character vector of the written file paths.

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
[`growth_structure_design()`](https://for-cast.github.io/landisutils/reference/growth_structure_design.md),
[`growth_window_for()`](https://for-cast.github.io/landisutils/reference/growth_window_for.md),
[`plot_growth_calibration()`](https://for-cast.github.io/landisutils/reference/plot_growth_calibration.md),
[`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md),
[`plot_growth_factorial_sensitivity()`](https://for-cast.github.io/landisutils/reference/plot_growth_factorial_sensitivity.md),
[`read_growth_scoring()`](https://for-cast.github.io/landisutils/reference/read_growth_scoring.md),
[`scale_linetype_growth_reference()`](https://for-cast.github.io/landisutils/reference/scale_linetype_growth_reference.md)
