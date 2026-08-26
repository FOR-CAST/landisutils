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
  mort_shp = NULL,
  sources = c("SORTIE", "TIPSY", "VDYP")
)
```

## Arguments

- reference:

  A data frame of reference observations, with columns `source`
  (`"SORTIE"`, `"TIPSY"`, `"VDYP"` or `"Ground plots"`), `age` and
  `aboveground_c_mg_ha`.

- longevity:

  Numeric. The species' `longevity`.

- age_floor:

  Numeric. Youngest age to score.

- age_quantile:

  Numeric. Quantile of observed plot ages to close at.

- senescence_frac:

  Numeric. Fraction of longevity at which to close, used only when
  `mort_shp` is `NULL`.

- mort_shp:

  Numeric `MortalityCurve` for this species, or `NULL`. When supplied it
  sets the cap and `senescence_frac` is ignored.

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
`longevity`. LANDIS-II ramps mortality up as a cohort approaches
`longevity` and the curve then falls to exactly zero and stays there, so
an open-ended window scores the modelled die-off rather than the level
the stand holds.

WHERE THAT CAP BELONGS IS A PROPERTY OF `MortalityCurve`. The extension
defines it as a position in the lifespan (2.12.4: 5 puts onset at 10% of
life span, 25 at 85%), so the age at which a species leaves its plateau
varies by nearly twofold across the documented range. Supply `mort_shp`
and the cap is that species' own onset, via
[`growth_mortality_onset_frac()`](https://for-cast.github.io/landisutils/reference/growth_mortality_onset_frac.md).
Measured on one calibration, the departure from 95% of peak biomass ran
0.43-0.48 x longevity at `MortalityCurve` 10, 0.63-0.70 at 15 and
0.82-0.84 at 25 – so a single fraction cannot separate a species that
breaks up early from one that holds its stand almost to the end, which
is the distinction the parameter exists to make.

The cap is at the ONSET of mortality, not at peak biomass, and is
therefore conservative: biomass keeps rising for a period after onset
while growth still exceeds mortality. That is deliberate. Peak location
depends on `GrowthCurve` as well, and `GrowthCurve` may still be swept –
a peak-based cap would then score different candidates over different
ranges and could not rank them fairly. Onset depends on `MortalityCurve`
alone, so as long as that is assigned rather than swept, every candidate
for a species sees one window.

`senescence_frac` remains as the fallback when `mort_shp` is not
supplied. Its default of 0.45 was calibrated against a parameterisation
that gave every species a `MortalityCurve` near 23; it does not
generalise, and on a set carrying 10s the earliest 95%-of-peak departure
falls to 0.433, below the cap itself.

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
[`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md),
[`plot_growth_factorial_sensitivity()`](https://for-cast.github.io/landisutils/reference/plot_growth_factorial_sensitivity.md),
[`plot_growth_structures()`](https://for-cast.github.io/landisutils/reference/plot_growth_structures.md),
[`read_growth_scoring()`](https://for-cast.github.io/landisutils/reference/read_growth_scoring.md),
[`read_landscape_cohort_structures()`](https://for-cast.github.io/landisutils/reference/read_landscape_cohort_structures.md),
[`scale_linetype_growth_reference()`](https://for-cast.github.io/landisutils/reference/scale_linetype_growth_reference.md),
[`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
