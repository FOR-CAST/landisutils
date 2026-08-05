# Condense a ground-plot cloud into an age-binned series

Bins the observations on age and takes one quantile per bin, so every
age band contributes once no matter how many plots landed in it. This is
the non-parametric alternative to fitting a growth equation through the
cloud: it assumes nothing about curve shape, which matters because the
shape being tested is ForCS's own.

## Usage

``` r
growth_bin_observations(obs, bin = 20L, probs = 0.5, site = NULL)
```

## Arguments

- obs:

  A tibble with `age` and `aboveground_c_mg_ha`.

- bin:

  Numeric. Bin width in years.

- probs:

  Numeric. Quantile to take within each bin.

- site:

  Optional column name identifying the sampling location. When given,
  repeated visits to one location are averaged within a bin before the
  quantile is taken, so `n` counts locations rather than visits. Errors
  if the named column is absent, rather than silently skipping the
  correction.

## Value

A tibble with `age` (bin mean), `value`, and `n`.

## Details

`probs = 0.5` (the median) tracks the central tendency of realized
stands. Raising it moves the series toward the upper envelope, which is
arguably where a fully stocked, single-cohort simulation belongs: the
plots span every stocking level, site quality and partial-disturbance
history, and whole-plot volume is attributed to the leading species,
which holds a median of 69% of the stand here.

Where observations come from a permanent-plot network, pass `site` so
that each location contributes one value per bin. Permanent plots are
remeasured on a schedule that reflects program history rather than
anything ecological – in the network this was built against, 78% of
locations carry more than one visit and some carry thirteen – so
treating every visit as an independent observation silently weights each
bin toward whichever locations happen to have been revisited most. That
is pseudo-replication, and it biases the quantile rather than merely
tightening it.

## See also

Other growth calibration helpers:
[`extract_landscape_cohort_structures()`](https://for-cast.github.io/landisutils/reference/extract_landscape_cohort_structures.md),
[`growth_add_objective()`](https://for-cast.github.io/landisutils/reference/growth_add_objective.md),
[`growth_auto_window()`](https://for-cast.github.io/landisutils/reference/growth_auto_window.md),
[`growth_best_candidates()`](https://for-cast.github.io/landisutils/reference/growth_best_candidates.md),
[`growth_calibration_design()`](https://for-cast.github.io/landisutils/reference/growth_calibration_design.md),
[`growth_calibration_partition()`](https://for-cast.github.io/landisutils/reference/growth_calibration_partition.md),
[`growth_calibration_work_root()`](https://for-cast.github.io/landisutils/reference/growth_calibration_work_root.md),
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
