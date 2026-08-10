# Best candidate per species, or an explicit refusal

A species with no scorable reference series MUST NOT produce a
recommendation. Three of the six species here are in that position:
amabilis fir, subalpine fir and hybrid spruce have too few ground plots
to bin, and their SORTIE curves are marked `available_unused` in the
project's curve-selection table because that model is not well
parameterized for them in the ICH. Ranking hundreds of indistinguishable
all-`NA` rows would return whichever combination happened to sort first,
dressed up as a result.

## Usage

``` r
growth_best_candidates(
  scores,
  growth_params,
  windows,
  scoring = NULL,
  top_frac = 0.1,
  level_frac_warn = 0.9
)
```

## Arguments

- scores:

  A tibble from
  [`growth_score_fit()`](https://for-cast.github.io/landisutils/reference/growth_score_fit.md)
  with an `objective_rmse`.

- growth_params:

  The parameters currently in use.

- windows:

  A tibble from
  [`growth_fitting_windows()`](https://for-cast.github.io/landisutils/reference/growth_fitting_windows.md).

- scoring:

  A tibble from
  [`read_growth_scoring()`](https://for-cast.github.io/landisutils/reference/read_growth_scoring.md),
  or `NULL`.

- top_frac:

  Numeric. Fraction of each species' ranking treated as
  indistinguishable from the winner, over which the level band is
  reported.

- level_frac_warn:

  Numeric. Warn when the selected candidate reached less than this
  fraction of its own asymptote, so its level is an extrapolation. `NA`
  disables the warning.

## Value

One row per species.

## Details

Those species come back with `fitted = FALSE`, no parameters, and the
values currently in use carried through untouched. That is the honest
answer, and it names what would change it: more plots, or promoting a
SORTIE curve to `used`.

## Reading the level band

Ranking is on shape alone and the level is recovered afterwards, by
dividing the reference plateau by the fraction of its own asymptote the
simulated curve reached (see
[`growth_inflation_factor()`](https://for-cast.github.io/landisutils/reference/growth_inflation_factor.md)).
That division is the whole reason `biomass_max_est` is not simply read
off the winner and trusted: its leverage on any error in the simulated
curve is `1 / achieved_frac`. A candidate that plateaued cleanly
recovers its level almost exactly, while one still climbing when the run
ended is extrapolating, and two candidates a fraction of a percent apart
in shape error can then imply levels differing by a factor of two or
more.

So `biomass_max_lo` / `biomass_max_hi` report the range of
`biomass_max_est` across the candidates that cannot be told apart from
the winner, and `level_extrapolated` flags a winner that never
approached its asymptote. A wide band is not noise to be averaged away:
it says the references do not determine the level, and the fix is a
longer run, a better-constrained reference, or a wider sweep – not a
different summary of the same surface. Use
[`growth_identifiability()`](https://for-cast.github.io/landisutils/reference/growth_identifiability.md)
to see which swept parameter is responsible.

## See also

Other growth calibration helpers:
[`extract_landscape_cohort_structures()`](https://for-cast.github.io/landisutils/reference/extract_landscape_cohort_structures.md),
[`growth_add_objective()`](https://for-cast.github.io/landisutils/reference/growth_add_objective.md),
[`growth_auto_window()`](https://for-cast.github.io/landisutils/reference/growth_auto_window.md),
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
[`growth_smooth_observations()`](https://for-cast.github.io/landisutils/reference/growth_smooth_observations.md),
[`growth_structure_design()`](https://for-cast.github.io/landisutils/reference/growth_structure_design.md),
[`growth_window_for()`](https://for-cast.github.io/landisutils/reference/growth_window_for.md),
[`plot_growth_calibration()`](https://for-cast.github.io/landisutils/reference/plot_growth_calibration.md),
[`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md),
[`plot_growth_factorial_sensitivity()`](https://for-cast.github.io/landisutils/reference/plot_growth_factorial_sensitivity.md),
[`read_growth_scoring()`](https://for-cast.github.io/landisutils/reference/read_growth_scoring.md),
[`scale_linetype_growth_reference()`](https://for-cast.github.io/landisutils/reference/scale_linetype_growth_reference.md),
[`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
