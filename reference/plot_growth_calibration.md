# Plot a species' growth curve against its references

Reproduces the layout of the source parameterization figures: the fitted
LANDIS-II trajectory in black, the ICH-SORTIE reference in dark grey,
the TIPSY yield curve in purple, and ground-plot observations coloured
by BEC subzone and shaped by sample-establishment type.

## Usage

``` r
plot_growth_calibration(
  species,
  curve,
  reference,
  x_max = NULL,
  mature_window = c(100L, Inf),
  density = FALSE,
  density_bins = 34L,
  density_points_max = 150L
)
```

## Arguments

- species:

  Character. Modelled species code.

- curve:

  A tibble with `age` and `aboveground_c_mg_ha`.

- reference:

  A data frame of reference observations, with columns `source`
  (`"SORTIE"`, `"TIPSY"`, `"VDYP"` or `"Ground plots"`), `age` and
  `aboveground_c_mg_ha`.

- x_max:

  Numeric or `NULL`. Upper age limit for the panel. `NULL` (the default)
  extends to the last age present in the data, so a longer run is never
  silently clipped.

- mature_window:

  Numeric length-2. Fitting window to shade; `NULL` to omit.

- density:

  Logical. Draw the ground-plot cloud as a WEIGHTED hexagonal density
  instead of one point per plot, with individual points kept only for
  the best-matched plots. Off by default, because it is worth it only
  where the cloud is dense enough to be unreadable as points: a species
  with a hundred plots gets a sparse, blocky panel that says less than
  the points did. Requires the 'hexbin' package.

- density_bins:

  Integer. Number of bins across the x range of the hexagonal grid when
  `density` is `TRUE`.

- density_points_max:

  Integer. When `density` is `TRUE`, how many of the best-matched plots
  stay drawn individually over the density. A COUNT rather than a
  fraction of the maximum weight, because a fraction does not control
  the number drawn: the weight distribution differs by species, and at
  60% of maximum one species keeps 989 plots where another keeps 65.
  Requires a `plot_weight` column; without one every plot counts
  equally.

## Value

A `ggplot`.

## Details

Deciduous plots are shown by their RAW leading species, because
cottonwood and birch are modelled as trembling aspen but observed
separately.

The fitting window is shaded, so a reviewer can see at a glance which
part of the curve the fit statistic is actually responding to.

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
[`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md),
[`plot_growth_factorial_sensitivity()`](https://for-cast.github.io/landisutils/reference/plot_growth_factorial_sensitivity.md),
[`plot_growth_structures()`](https://for-cast.github.io/landisutils/reference/plot_growth_structures.md),
[`read_growth_scoring()`](https://for-cast.github.io/landisutils/reference/read_growth_scoring.md),
[`read_landscape_cohort_structures()`](https://for-cast.github.io/landisutils/reference/read_landscape_cohort_structures.md),
[`scale_linetype_growth_reference()`](https://for-cast.github.io/landisutils/reference/scale_linetype_growth_reference.md),
[`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
