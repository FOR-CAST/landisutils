# Derive one species' fitting window from where its references have support

Nobody should have to nominate an age range by hand. Both bounds are
dictated by the data and by a species attribute that is not being
fitted, so both can be read off directly.

## Usage

``` r
growth_auto_window(
  reference,
  longevity,
  age_floor = 20,
  age_quantile = 0.95,
  senescence_frac = 0.45,
  sources = c("SORTIE", "TIPSY")
)
```

## Arguments

- reference:

  A data frame of reference observations, with columns `source`
  (`"SORTIE"`, `"TIPSY"` or `"Ground plots"`), `age` and
  `aboveground_c_mg_ha`.

- longevity:

  Numeric. The species' `longevity`.

- age_floor:

  Numeric. Youngest age to score.

- age_quantile:

  Numeric. Quantile of observed plot ages to close at.

- senescence_frac:

  Numeric. Fraction of longevity at which to close.

- sources:

  Character. Modelled reference series to consider.

## Value

Numeric length-2.

## Details

The window OPENS at `age_floor`. Stands younger than that are
essentially unmeasured – the ground-plot programs do not sample them –
and it is also the range where LANDIS-II is known to overestimate
biomass for reasons that have nothing to do with these four parameters,
so scoring there would import a bias the sweep cannot fix.

The window CLOSES at the earliest of three limits: the `age_quantile` of
the observed plot ages, beyond which the cloud thins to a handful of
stands; the end of the modelled reference curve; and `senescence_frac` x
`longevity`. The last of those is the one that binds in practice.
LANDIS-II ramps mortality up as a cohort approaches `longevity` and the
curve then falls to exactly zero and stays there, so an open-ended
window scores the modelled die-off rather than the level the stand
holds. The cap is a fraction of `longevity` rather than something read
off the simulated curve, because the decline timing depends on the
mortality shape, which is itself being swept – a candidate-dependent
window would score different candidates over different ranges and could
not rank them fairly.

`senescence_frac = 0.45` is conservative: across the calibrated species
the earliest departure from 95% of peak biomass is at 0.47 x longevity,
so every species is still at its plateau throughout its window.

## See also

Other growth calibration helpers:
[`extract_landscape_cohort_structures()`](https://for-cast.github.io/landisutils/reference/extract_landscape_cohort_structures.md),
[`growth_add_objective()`](https://for-cast.github.io/landisutils/reference/growth_add_objective.md),
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
