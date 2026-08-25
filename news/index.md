# Changelog

## landisutils 0.0.130

- `plot_growth_candidate(density = TRUE)` renders. 0.0.128 introduced
  the density and could not draw a bundle at all; 0.0.129 misdiagnosed
  the cause and moved the hexes to `alpha`, which did not fix it. The
  contended aesthetic is FILL: `stat_binhex()` maps
  `fill = after_stat(count)` through its DEFAULT aes, which passing a
  fixed `fill` argument does not unset, while the binned-median series
  maps `fill` to a discrete label so that its key merges with the colour
  legend. With both present ggplot2 applies the discrete scale to
  continuous counts and reports “continuous value supplied to a discrete
  scale”, naming neither layer. In density mode the binned series now
  takes a fixed fill and maps `colour` instead, which keeps its key and
  leaves `fill` to the hexes.
- The regression test BUILDS the plot rather than only constructing it.
  Both earlier releases passed their tests: a scale collision surfaces
  at render time, so a test that stops at the ggplot object cannot see
  it.

## landisutils 0.0.129

- Attempted fix for the 0.0.128 render failure by mapping the hex
  density on `alpha`. It did not work; see 0.0.130 for the actual cause.
  Superseded.

## landisutils 0.0.128

- [`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md)
  gains `density`, drawing the ground-plot cloud as a WEIGHTED hexagonal
  density with the best-matched plots kept over it.
  [`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
  turns it on per species at `density_min_plots` (500 by default), since
  a bundle routinely spans two orders of magnitude in plot count and the
  panels stop being readable well before the data run out: one
  landscape’s lodgepole pine carries 1,639 plots and the cloud is solid
  ink with a shape key per leading species. The density carries every
  plot AT ITS WEIGHT, via the `plot_weight` column, so nothing leaves
  the figure and the shading reads as evidence rather than as sampling
  effort; without that column it falls back to counts. NOTE this release
  could not render; fixed in 0.0.130.
- `density_points_max` replaces `density_point_weight` in
  [`plot_growth_calibration()`](https://for-cast.github.io/landisutils/reference/plot_growth_calibration.md),
  and is the rule in
  [`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md)
  too. A fraction of the maximum weight does not control how many points
  get drawn, because the weight distribution differs by species: at 60%
  of maximum – the old default – one species keeps 989 of its plots and
  another keeps 65, and the first buries the density it was meant to
  annotate. A count draws the same number whatever the shape of the
  distribution. This is a breaking rename for callers that set the old
  argument.
- The review bundle’s README names Biomass Succession rather than ForCS.
  It was a copied label; nothing in the bundle was ever ForCS-specific.

## landisutils 0.0.127

- [`read_landis_raster()`](https://for-cast.github.io/landisutils/reference/read_landis_raster.md)
  asks terra how it reads an ungeoreferenced file instead of assuming,
  which fixes LANDIS-II output rasters being returned VERTICALLY
  MIRRORED under terra 1.9-46 and later. LANDIS-II writes no
  geotransform, GDAL then reports the identity transform with a pixel
  height of `+1`, and every terra up to 1.9-34 honoured that by placing
  the first file row at the bottom – so this package flipped the rows
  back. terra 1.9-46 (2026-08-22) stopped doing so and returns those
  rows in file order, and the unconditional flip then introduced the
  very inversion it existed to remove. Nothing in the package had
  changed; the correction had simply become a corruption.
- The new behaviour is detected rather than gated on a version. Which
  terra release changed is not something this package can know, and a
  boundary guessed wrong fails identically but is harder to find. A
  15-row LANDIS-II community map whose codes run 3 to 17 down the file
  ships in `inst/testdata/` and is read once per session; the direction
  the values come back in is the answer. An explicit south-up
  geotransform is unaffected and still flips, which terra 1.9-46 did not
  change.
- The row-order test no longer asserts which direction terra reads in.
  That is terra’s business, not this package’s, and asserting it meant a
  terra release failed the suite of a package whose own behaviour was
  unchanged. The test now asserts the invariant that matters: what comes
  back is the order LANDIS-II wrote.

## landisutils 0.0.126

- [`plot_growth_calibration()`](https://for-cast.github.io/landisutils/reference/plot_growth_calibration.md)
  gains `density`, drawing the ground-plot cloud as a WEIGHTED hexagonal
  density with individual points kept only for the best-matched plots,
  instead of one point per plot. Off by default, so nothing changes for
  existing callers. It exists because the panels stop being readable
  well before the data run out: one landscape’s lodgepole pine carries
  3,106 plots and 57 BEC subzone legend keys, and the legend alone took
  a third of the figure while the cloud was solid ink. The density
  carries every plot, weighted, so nothing is dropped from view – what
  changes is that a thousand plots stop competing for the same ink and
  for a legend key each. Worth it only where the cloud is dense: a
  species with a hundred plots gets a sparse, blocky grid that says less
  than the points did, which is why the switch is the caller’s and not
  automatic.
- `density_bins` and `density_point_weight` control the grid resolution
  and which plots are still drawn individually (default: those at 60% of
  the species’ maximum weight or better). The per-plot colour and shape
  legends are dropped in density mode along with the points that fed
  them, and the manual shape scale with them, since a manual scale with
  no layer behind it warns about levels it cannot find.
- Requires ‘hexbin’, declared in Suggests and checked at call time.

## landisutils 0.0.125

- [`growth_bin_observations()`](https://for-cast.github.io/landisutils/reference/growth_bin_observations.md)
  DROPS a bin whose observations all carry zero weight, instead of
  emitting it with an `NA` value. The weighted quantile of an empty
  effective sample is `NA`, and such a bin is not a thin bin but an
  absent one – every observation in it was excluded by the weighting.
  Carrying it overstated `n_bins`, and because
  [`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md)
  takes the ground-plot LEVEL as a
  [`max()`](https://rdrr.io/r/base/Extremes.html) over the binned values
  inside the window, one valueless bin made the level `NA` and dropped
  every plot-scored series for that species. Latent until a window
  widened far enough to admit one: a species scored on plots alone then
  returned `n_series = 0` and an all-`NA` row, with nothing to say why.
- [`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md)
  computes that [`max()`](https://rdrr.io/r/base/Extremes.html) with
  `na.rm = TRUE` and returns `NA_real_` rather than `-Inf` when nothing
  survives, so a caller supplying its own binned series cannot
  reintroduce the same failure.

## landisutils 0.0.124

- [`growth_auto_window()`](https://for-cast.github.io/landisutils/reference/growth_auto_window.md)
  and
  [`growth_fitting_windows()`](https://for-cast.github.io/landisutils/reference/growth_fitting_windows.md)
  accept `mort_shp` and close each species’ window at that species’ own
  onset of age-related mortality, instead of at one fraction of
  longevity shared by every species. Biomass Succession defines
  `MortalityCurve` as a POSITION in the lifespan (v7 User Guide 2.12.4),
  so where a species leaves its plateau varies nearly twofold across the
  documented range – measured on one calibration, the departure from 95%
  of peak biomass ran 0.43-0.48 x longevity at `MortalityCurve` 10,
  0.63-0.70 at 15 and 0.82-0.84 at 25. A single fraction cannot separate
  a species that breaks up early from one that holds its stand almost to
  the end, which is the distinction the parameter exists to make. The
  `senescence_frac = 0.45` fallback remains for callers that do not
  supply `mort_shp`, but its default was calibrated against a
  parameterisation giving every species a `MortalityCurve` near 23 and
  does not generalise: on a set carrying 10s the earliest 95%-of-peak
  departure falls to 0.433, below the cap itself.
- [`growth_mortality_onset_frac()`](https://for-cast.github.io/landisutils/reference/growth_mortality_onset_frac.md)
  is new and exported: it inverts the User Guide’s definition of
  `MortalityCurve` to the fraction of lifespan at which age-related
  mortality begins. Callers were open-coding the same arithmetic.
- [`growth_auto_window()`](https://for-cast.github.io/landisutils/reference/growth_auto_window.md)
  no longer loses a year to floating-point representation error.
  `0.10 + (10 - 5) / 20 * 0.75` is 0.28749999999999998, so an onset
  landing mathematically on a whole year came out a hair below it and
  the inward rounding took the year before – a cap of 114 where 115 was
  meant.
