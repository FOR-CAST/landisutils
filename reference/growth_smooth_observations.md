# Smooth a ground-plot cloud for display

Fits a thin-plate spline through the observations and returns it on a
dense age grid with a pointwise confidence band. This is a VISUAL AID
ONLY: nothing in the scoring path consumes it, and
[`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md)
continues to build the scored ground-plot reference by binning. Keeping
the two separate is deliberate – swapping the scored reference changes
every `biomass_max_est` that rests on plots, which is a calibration
decision rather than a plotting one.

## Usage

``` r
growth_smooth_observations(
  obs,
  bin = 20L,
  site = NULL,
  k = NULL,
  n_grid = 200L,
  level = 0.95
)
```

## Arguments

- obs:

  A tibble with `age` and `aboveground_c_mg_ha`.

- bin, site:

  As in
  [`growth_bin_observations()`](https://for-cast.github.io/landisutils/reference/growth_bin_observations.md);
  used only to collapse repeated visits, not to summarize.

- k:

  Integer. Spline basis dimension. `NULL` derives one from the number of
  occupied bins, capped at 5, which keeps the fit from chasing
  individual plots.

- n_grid:

  Integer. Number of ages at which to evaluate the fit.

- level:

  Numeric. Confidence level for the band.

## Value

A tibble with `age`, `value`, `lo`, `hi`, and a `k` attribute; zero rows
when there are too few distinct observations to fit.

## Details

What it is for is judging the binned series. A bin holding one plot is
drawn at the same visual weight as a bin holding thirty, and the
straight lines between bins imply a trajectory the plots may not
support; a fit over the whole cloud shows how much of that movement is
real. Where the band is wide, the binned points nearby are not evidence
of anything.

Observations are collapsed by location and bin first, exactly as
[`growth_bin_observations()`](https://for-cast.github.io/landisutils/reference/growth_bin_observations.md)
does, so the fit and the binned series rest on the same evidence and any
difference between them is the summarizing method rather than the
sample.

The fit is on the IDENTITY scale. A log link is the obvious response to
right-skewed biomass, but with a handful of plots at the old end it
extrapolates violently – in the network this was built against it lifted
one species' curve to 347 Mg C/ha against a binned maximum of 238 – so
the skew is left to the confidence band to express. The band is clamped
at zero, since negative aboveground carbon is not a state a stand can be
in.

No prediction is returned outside the observed age range: a spline given
no data has nothing to say, and a curve drawn past the last plot invites
the reader to believe otherwise.

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
