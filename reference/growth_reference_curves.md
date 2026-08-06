# Build one species' reference curves on a common age grid

Everything the scorer needs, computed once per species rather than once
per parameter combination: each reference series evaluated at the same
ages, and each series' plateau level.

## Usage

``` r
growth_reference_curves(
  reference,
  window,
  bin = 20L,
  plot_quantile = 0.5,
  min_plots = 50L,
  n_grid = 60L,
  use_tipsy = FALSE,
  use_vdyp = FALSE,
  site = NULL,
  weight = NULL
)
```

## Arguments

- reference:

  A data frame of reference observations, with columns `source`
  (`"SORTIE"`, `"TIPSY"` or `"Ground plots"`), `age` and
  `aboveground_c_mg_ha`.

- window:

  Numeric length-2. The fitting window.

- bin, plot_quantile, min_plots:

  Ground-plot controls; `min_plots` is advisory and only sets
  `plots_sparse`. See
  [`growth_bin_observations()`](https://for-cast.github.io/landisutils/reference/growth_bin_observations.md)
  and
  [`read_growth_scoring()`](https://for-cast.github.io/landisutils/reference/read_growth_scoring.md).

- n_grid:

  Integer. Number of ages in the common grid.

- use_tipsy:

  Logical. Score against TIPSY as well.

- use_vdyp:

  Logical. Score against VDYP as well. VDYP is the British Columbia
  Variable Density Yield Projection model, whose curves are natural
  (unmanaged) stand yields; it is a separate series from TIPSY, which
  projects MANAGED stands, because a natural-disturbance model wants the
  former and the distinction must not be lost in the outputs.

- site:

  Optional column name identifying the sampling location of a
  ground-plot observation. Passed to
  [`growth_bin_observations()`](https://for-cast.github.io/landisutils/reference/growth_bin_observations.md);
  when given, `n_plots` counts distinct locations rather than visits.

- weight:

  Optional column name holding a per-observation climatic weight. Passed
  to
  [`growth_bin_observations()`](https://for-cast.github.io/landisutils/reference/growth_bin_observations.md);
  see
  [`growth_climatic_weight()`](https://for-cast.github.io/landisutils/reference/growth_climatic_weight.md).

## Value

A list with `ages`, `series`, `levels`, `n_plots`, `n_bins`,
`plots_sparse`.

## Details

Interpolation uses `rule = 1`, so a series is `NA` outside its own age
range and nothing is ever scored against an extrapolation of a reference
that simply stops.

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
