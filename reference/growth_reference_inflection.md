# Age at which a reference growth curve's increment peaks

The inflection point of a sigmoid growth curve: the age of maximum
annual increment, after which growth slows and the curve approaches its
asymptote.

## Usage

``` r
growth_reference_inflection(
  reference,
  method = c("inflection", "asymptote"),
  frac = 0.9,
  sources = c("SORTIE", "TIPSY"),
  smooth_window = 21L,
  default = 100
)
```

## Arguments

- reference:

  A data frame of reference observations, with columns `source`
  (`"SORTIE"`, `"TIPSY"` or `"Ground plots"`), `age` and
  `aboveground_c_mg_ha`.

- method:

  `"inflection"` or `"asymptote"`.

- frac:

  Numeric. Fraction of the curve maximum, for `"asymptote"`.

- sources:

  Character vector of reference sources to use, in order of preference;
  the first one present is used.

- smooth_window:

  Integer. Width of the moving average over increments.

- default:

  Numeric. Returned when no usable model reference exists.

## Value

A single age.

## Details

This is what defines "mature" for scoring purposes. A fixed age
threshold cannot: the modelled species differ several-fold in longevity
and in how fast they get there, so age 100 is past the inflection for a
fast, short-lived species and well before it for a slow, long-lived one.
Deriving the threshold from each species' own reference curve keeps the
prioritized window in the same place on every curve – the part where the
trajectory is settling toward the level it will hold.

The increment is smoothed before the maximum is taken, because SORTIE
curves are individual-tree simulations and their year-to-year increments
are noisy enough for the raw argmax to land almost anywhere.

Two definitions are available. `"inflection"` is the age of peak
increment itself, the point at which growth stops accelerating.
`"asymptote"` is the age at which the curve first reaches `frac` of its
maximum, which sits later and isolates the landing region more tightly –
an inflection-based window can start very early for a fast species (age
18 for lodgepole pine) and so still carries much of the rapid-growth
phase.

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
