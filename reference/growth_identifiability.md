# Is a swept parameter actually determined by the references?

Taking an argmin over a factorial presumes the objective surface has a
well-defined minimum. Often it does not, and the reported best
combination is then whichever cell happened to sort first rather than a
fitted value. Nothing in a ranked table distinguishes the two cases, so
this reports the distinction directly: for each swept parameter, the
range of values spanned by the best-scoring candidates and the error
spread across them.

## Usage

``` r
growth_identifiability(
  scores,
  params = c("growth_shp", "mort_shp", "anpp_prop"),
  top_frac = 0.1,
  identified_below = 0.5
)
```

## Arguments

- scores:

  A tibble from
  [`growth_score_fit()`](https://for-cast.github.io/landisutils/reference/growth_score_fit.md)
  with an `objective_rmse`, as returned by
  [`growth_add_objective()`](https://for-cast.github.io/landisutils/reference/growth_add_objective.md).

- params:

  Character vector of swept parameter columns to assess.

- top_frac:

  Numeric. Fraction of each species' ranking treated as the set of
  candidates that cannot be told apart.

- identified_below:

  Numeric. A parameter is reported as identified when its top candidates
  span no more than this fraction of the swept grid.

## Value

One row per species and parameter, with the argmin value, the range
spanned by the top candidates, the fraction of the grid that range
covers, whether the argmin sits on a grid boundary, and the relative
spread in objective across the top candidates.

## Details

A parameter whose top candidates span most of the swept grid while their
errors differ by a few percent is not being estimated. Two patterns
recur and both are worth naming in a calibration's own output:

- Mortality shape is routinely unidentified. Once a curve has reached
  its level, the shape of the approach barely moves the residual, so the
  objective is nearly flat along that axis. This appears to be inherent
  to fitting a plateau rather than a property of any one data set.

- An argmin on the edge of the swept grid means the optimum may lie
  outside it, and `boundary` flags this. It also makes any weighted
  average of the candidates biased inward by construction, which is the
  main reason to check identifiability before reaching for model
  averaging as the remedy.

Species with no scorable candidate are absent from the result; see
[`growth_best_candidates()`](https://for-cast.github.io/landisutils/reference/growth_best_candidates.md),
which reports them as an explicit refusal.

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
