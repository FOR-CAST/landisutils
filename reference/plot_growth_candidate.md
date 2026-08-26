# Plot the current parameter set against a candidate, for review

Both trajectories are drawn over the same references, with the fitting
window shaded. This is the figure to look at when deciding whether a
sweep result is worth promoting: it shows what actually changes, over
the part of the curve the objective responds to.

## Usage

``` r
plot_growth_candidate(
  species,
  current_curve,
  candidate_curve,
  reference,
  binned = NULL,
  smooth = NULL,
  current_label = "current parameters",
  candidate_label = "best candidate",
  x_max = NULL,
  mature_window = c(100L, Inf),
  subtitle = NULL,
  density = FALSE,
  density_bins = 34L,
  density_points_max = 150L
)
```

## Arguments

- species:

  Character. Modelled species code.

- current_curve, candidate_curve:

  Tibbles with `age` and `aboveground_c_mg_ha`. `candidate_curve` may be
  `NULL`.

- reference:

  A data frame of reference observations, with columns `source`
  (`"SORTIE"`, `"TIPSY"`, `"VDYP"` or `"Ground plots"`), `age` and
  `aboveground_c_mg_ha`.

- binned:

  Optional tibble from
  [`growth_bin_observations()`](https://for-cast.github.io/landisutils/reference/growth_bin_observations.md).
  An `n` column, when present, sizes the points.

- smooth:

  Optional tibble from
  [`growth_smooth_observations()`](https://for-cast.github.io/landisutils/reference/growth_smooth_observations.md),
  drawn as a fitted line and confidence band. Display only; nothing is
  scored against it.

- current_label, candidate_label:

  Character legend labels.

- x_max:

  Numeric or `NULL`. Upper age limit. `NULL` (the default) extends to
  the last age present in the data, so a longer run is never silently
  clipped.

- mature_window:

  Numeric length-2. Fitting window to shade.

- subtitle:

  Character. Overrides the default subtitle.

- density:

  Logical. Draw the ground-plot cloud as a WEIGHTED hexagonal density
  instead of one point per plot, keeping the best-matched plots drawn
  over it. Weighted via the `plot_weight` column if present, so the
  shading reads as evidence rather than as sampling effort; without that
  column it falls back to counts.

- density_bins:

  Integer. Bins across the x range of the hex grid.

- density_points_max:

  Integer. How many of the best-matched plots stay drawn individually
  over the density. A COUNT rather than a fraction of the maximum
  weight, because a fraction does not control the number drawn: the
  weight distribution differs by species, and at 60% of maximum one
  species keeps 989 plots where another keeps 65.

## Value

A `ggplot`.

## Details

The age-binned plot series is drawn as well, in blue. That series – not
the scatter behind it – is what the ground-plot term of the score is
computed against, so a candidate that looks wrong against the cloud but
right against the binned points is behaving exactly as scored.

Each binned point is sized by the number of plots behind it, because
they routinely differ by more than an order of magnitude and an
equal-sized point hides that completely. A bin holding a single plot is
not a median of anything, and the sharp reversals in the series are
usually those bins.

The points are NOT joined by line segments. Connecting them asserts a
trajectory across ages where nothing was measured, and most of the
movement that line described came from the one-plot bins.

Passing `smooth` overlays a fit through the whole cloud with a
confidence band, for comparison only – see
[`growth_smooth_observations()`](https://for-cast.github.io/landisutils/reference/growth_smooth_observations.md).
It is not scored, and the legend says so. It shares the binned points'
colour because it summarizes the same observations; glyph, not hue, is
what tells them apart.

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
[`plot_growth_factorial_sensitivity()`](https://for-cast.github.io/landisutils/reference/plot_growth_factorial_sensitivity.md),
[`plot_growth_structures()`](https://for-cast.github.io/landisutils/reference/plot_growth_structures.md),
[`read_growth_scoring()`](https://for-cast.github.io/landisutils/reference/read_growth_scoring.md),
[`read_landscape_cohort_structures()`](https://for-cast.github.io/landisutils/reference/read_landscape_cohort_structures.md),
[`scale_linetype_growth_reference()`](https://for-cast.github.io/landisutils/reference/scale_linetype_growth_reference.md),
[`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
