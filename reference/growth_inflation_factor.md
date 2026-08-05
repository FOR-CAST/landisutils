# Recover the `biomass_max` a candidate implies

A ForCS cohort never quite reaches its `biomass_max`: it approaches it
asymptotically while mortality is already removing biomass, so the
plateau it actually holds is some fraction of the parameter. The current
parameter set was built as
`biomass_max = reference curve maximum x 200`, which assumes that
fraction is 1. It is not: it runs from 0.90 (trembling aspen) to 1.00
(lodgepole pine) across these species, so that rule undershoots the
intended plateau by up to 11%.

## Usage

``` r
growth_inflation_factor(achieved, biomass_max, level, biomass_max_scale = 200)
```

## Arguments

- achieved:

  Numeric. Plateau the simulated curve actually reaches, in whatever
  units the curves are expressed in.

- biomass_max:

  Numeric. The maximum-biomass parameter that simulation ran at.

- level:

  Numeric. Plateau the curve should reach; same units as `achieved`.

- biomass_max_scale:

  Numeric. Divide `biomass_max` by this to express it in the curve's own
  units. The default 200 is the ForC Succession convention:
  `biomass_max` is g m^-2 of biomass while the summary log reports g C
  m^-2, and 1 Mg C ha^-1 corresponds to 200 g m^-2 of biomass. For an
  extension whose parameter and output share units – Biomass Succession
  reports g m^-2 of biomass against a `maxBiomass` in g m^-2 – pass `1`.

## Value

A list with `achieved_frac`, `inflation`, and `biomass_max_est`.

## Details

The fraction depends only on the growth and mortality shapes and on the
ratio of `anpp_max` to `biomass_max`, not on the absolute level –
verified across a ForC Succession sweep, where combinations sharing a
ratio but differing in absolute `biomass_max` agree on the achieved
fraction to within 0.04%. That invariance is what lets the level be
recovered arithmetically instead of searched, and it is why
`biomass_max` is held fixed across the factorial.

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
