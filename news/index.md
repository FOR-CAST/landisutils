# Changelog

## landisutils 0.0.142

- new FPSM output helpers
  [`read_fps_raw_out()`](https://for-cast.github.io/landisutils/reference/read_fps_raw_out.md),
  [`fps_pools()`](https://for-cast.github.io/landisutils/reference/fps_pools.md),
  [`fps_stocks_by_pool()`](https://for-cast.github.io/landisutils/reference/fps_stocks_by_pool.md),
  [`write_fps_raw_out_parquet()`](https://for-cast.github.io/landisutils/reference/write_fps_raw_out_parquet.md)
  and
  [`open_fps_raw_out_dataset()`](https://for-cast.github.io/landisutils/reference/open_fps_raw_out_dataset.md),
  the counterparts to the ForCS `log_Summary` helpers and following the
  same read / atomic-publish / union-dataset shape.
- [`fps_pools()`](https://for-cast.github.io/landisutils/reference/fps_pools.md)
  drops the terminal simulation year by default. FPSM writes its annual
  end-of-year stock reports as types 4 and 5 up to the second-to-last
  year, then a different and partial residual set for the final year
  (types 1 and 2, decaying pools only), so carrying the final year into
  a stock series draws a collapse that did not happen. The cut is
  derived from the data – the last year carrying a type 4 or 5 report –
  not hard-coded.
- `rlang` is declared in `Suggests`.
  `tests/testthat/test-growth_structures.R` calls
  [`rlang::as_label()`](https://rlang.r-lib.org/reference/as_label.html),
  which `R CMD check` reports as an unstated dependency; the check
  workflow runs with `error_on = "warning"`, so this had been failing CI
  since 0.0.140 (9789252a, 2026-09-02). Unrelated to the FPSM work,
  fixed here because it blocks the same build.
- all FPSM amounts are reported as tonnes of carbon. FPSM sums
  `gC/m^2 * cell_length^2 * 1e-6` over cells with no division by area,
  so its output is an absolute landscape total despite the user guide
  labelling the columns `tC/ha`; the two coincide only for a 100 m cell.
  It reports carbon, never CO2e.

## landisutils 0.0.141

- new
  [`fps_run_docker()`](https://for-cast.github.io/landisutils/reference/fps_run_docker.md)
  and
  [`fps_output_files()`](https://for-cast.github.io/landisutils/reference/fps_output_files.md),
  for running the Forest Product Sector Module (FPSM) over a directory
  holding an FPSM configuration and the two ForCS flux logs it names.
  FPSM is not a running LANDIS-II extension – its `PlugIn.Run()` is an
  empty stub and the work happens in a console entry point – so it gets
  plain functions rather than a `LandisExtension` subclass, and the
  runner omits the version assertion, startup jitter, output streaming
  and post-completion watchdog that
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  needs. It keeps the image-digest capture, because that is what
  identifies the bytes that ran.
- [`fps_run_docker()`](https://for-cast.github.io/landisutils/reference/fps_run_docker.md)
  performs three pre-flight checks, each guarding a failure that is
  otherwise silent or obscure: the configured input filenames must exist
  case-exactly (the shipped FPSM examples name `log_fluxDOM.csv` beside
  `log_FluxDOM.csv`, which is harmless on Windows and fatal elsewhere);
  each flux log’s header must still match the column positions FPSM
  indexes, since it validates no headers and would read a reordered
  column as the wrong quantity; and a non-empty `FPS_log.txt` fails the
  run by default, because that file collects unallocated carbon among
  other non-fatal problems.

## landisutils 0.0.140

- [`growth_structure_summary()`](https://for-cast.github.io/landisutils/reference/growth_structure_summary.md)
  gains `start_age_breaks`, summarising WITHIN starting-age classes
  instead of pooling over them. A composition’s cells differ in two ways
  at once, the parameter combination and the ages the
  initial-communities map gave their cohorts, and the second dominates:
  measured on a 14,375-cell structure sweep, starting age explains 92 to
  99 percent of the within-composition variance against 0.2 to 3.3
  percent for the swept parameters. A band taken over the pool is
  therefore roughly ten times the parameter effect it is read as
  showing, and is mostly stand age wearing the parameters’ name. `NULL`
  (the default) pools as before.
- [`growth_structure_cell_curves()`](https://for-cast.github.io/landisutils/reference/growth_structure_cell_curves.md)
  carries `start_age`, the oldest cohort the cell begins with, which is
  what `start_age_breaks` bins. Existing callers are unaffected: it is
  an added column, and every downstream summarise names its grouping
  explicitly.
- [`plot_growth_structures()`](https://for-cast.github.io/landisutils/reference/plot_growth_structures.md)
  colours by `start_class` when the summary carries one, on a viridis
  ramp that DARKENS with starting age. Within a panel the previous
  `kind` colouring was constant, since a panel is one composition, so it
  only restated the facet strip. The ramp is truncated at 0.75 because
  viridis ends in a yellow that sits at 1.23:1 against a light page;
  multi-hue is a deliberate departure from one-hue-sequential, since
  eight steps of a single hue are not separable at panel size, and
  lightness stays monotone so the ordering survives in greyscale and
  under colour-vision deficiency. Ribbons are suppressed when
  stratified, as they already were for two variants.

## landisutils 0.0.139

- [`plot_growth_structures()`](https://for-cast.github.io/landisutils/reference/plot_growth_structures.md)
  gains `max_panels`, keeping only the compositions with the most cells
  behind them. A facet per composition is right when cells hold at most
  a couple of cohorts and a species appears in a handful of them; where
  cells carry ten to thirty age classes a species appears in over a
  hundred, and the panel renders as unreadable slivers with truncated
  facet strips and overlapping axis labels. `NULL` (the default) keeps
  all, so existing callers are unchanged. What was dropped is stated in
  the subtitle rather than left implied.

## landisutils 0.0.138

- [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  stages the RIGHT branch’s input files. When a pattern maps over
  `scenario_dir` but not over the dependency target, every branch
  receives all branches’ files, and the basename deduplication then has
  to choose; files under the branch’s own `scenario_dir` were ordered
  first so they would win. That prioritisation compared a `path_abs()`
  spelling against a `path_real()` one, so in any project reaching its
  LANDIS-II tree through a symlink the prefix test never matched, the
  ordering became a no-op, and EVERY branch staged the first-listed
  branch’s inputs. A two-batch run produced byte-identical outputs from
  two different landscapes while each branch’s own correct inputs sat
  unused on disk; the failure needs at least two branches to appear, so
  single-branch runs cannot surface it. Both sides are now resolved with
  `path_real()`.
- [`landis_dep_files()`](https://for-cast.github.io/landisutils/reference/landis_dep_files.md)
  is the resolution, pulled out of the
  [`bquote()`](https://rdrr.io/r/base/bquote.html) target command so it
  can be tested. The bug survived precisely because it was inline there,
  where no test could reach it. Exported (`@keywords internal`) because
  generated target code cannot reach an unexported name without `:::`;
  not part of the user-facing API.

## landisutils 0.0.137

- [`validate_landis_scenario()`](https://for-cast.github.io/landisutils/reference/validate_landis_scenario.md)
  scopes the initial-communities map-code check to cells the ecoregion
  map calls ACTIVE. A map code is only reachable where the cell is
  active, since LANDIS-II never resolves a community for an inactive
  one, so testing the whole raster read deliberate non-vegetated
  land-cover codes as missing communities and rejected input that runs.
  `landisbc` writes distinct herb, shrub, bryoid, exposed-land and water
  codes into inactive cells and none of them has CSV rows, because none
  of them is a community: measured on an 890,400-cell landscape, 140,597
  cells carry four such codes and not one is ecoregion-active, while
  both scenarios staged from that map run to completion. Without a
  readable ecoregions map it falls back to the whole raster, which can
  over-report but never under-report.
- This fix was written against 0.0.132 and stranded on an unpushed
  commit while the package moved on to 0.0.136, so it was absent from
  every released version. Restored here unchanged apart from a comment.

## landisutils 0.0.136

- [`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
  documents `density_min_plots`, `density_bins` and
  `density_points_max`. They were added with the hex density and left
  undocumented, which is an R CMD check WARNING; CI runs with
  `error_on = "warning"` and had been failing on it since 2026-08-25.
- Declares `count` as a global. It is computed by `stat_binhex()` and
  reached through `after_stat()`, so it never exists as a binding R CMD
  check can see.

## landisutils 0.0.135

- [`growth_structure_cell_curves()`](https://for-cast.github.io/landisutils/reference/growth_structure_cell_curves.md)
  COUNTS a repeated species in `composition` rather than repeating its
  name: `Hw` for one cohort, `Hw x3` for three. The repeated form was
  written for a design holding at most two cohorts; a structure design
  is capped on SPECIES, so a landscape whose cells carry a dozen age
  classes produced 255-character labels, unusable as a facet strip and
  near-unique per cell. The counted form has the same equivalence
  classes, so nothing regroups.
- A `species_set` column carries the species actually present, and
  [`plot_growth_structures()`](https://for-cast.github.io/landisutils/reference/plot_growth_structures.md)
  matches and counts species on it instead of parsing `composition`. The
  label carries counts now, so a bare species code no longer matches it;
  a summary without the column still falls back to `composition`.
- [`growth_structure_summary()`](https://for-cast.github.io/landisutils/reference/growth_structure_summary.md)
  and
  [`growth_structure_cohort_table()`](https://for-cast.github.io/landisutils/reference/growth_structure_cohort_table.md)
  carry `species_set` through. It is a function of `composition`, so no
  grouping changes.

## landisutils 0.0.134

- [`growth_structure_cell_curves()`](https://for-cast.github.io/landisutils/reference/growth_structure_cell_curves.md)
  gains `biomass`, because what `aboveground_c_mg_ha` MEANS differs by
  extension and cannot be detected from the data. A per-cell output log
  reports a whole-cell total that the per-cohort join then repeats, so
  it is de-duplicated (`"cell"`, the default, unchanged). A per-cohort
  community output reports each cohort’s own biomass, which has to be
  SUMMED (`"cohort"`). Getting it wrong is quiet rather than loud:
  de-duplicating per-cohort values keeps one row per DISTINCT VALUE, so
  a cell of 16 cohorts carrying 4 distinct biomasses reduces to 4 rows
  that are neither a total nor a trajectory.
- [`plot_growth_structures()`](https://for-cast.github.io/landisutils/reference/plot_growth_structures.md)
  no longer labels its categories in terms of two. A structure design is
  capped on SPECIES, not cohorts, so a one-species cell can carry a
  dozen age classes; the labels are now `single cohort`,
  `one species, multiple cohorts` and `multiple species`. The previous
  wording was correct only for a design that also happened to hold at
  most two cohorts.

## landisutils 0.0.133

- [`growth_structure_cell_curves()`](https://for-cast.github.io/landisutils/reference/growth_structure_cell_curves.md),
  [`growth_structure_summary()`](https://for-cast.github.io/landisutils/reference/growth_structure_summary.md),
  [`growth_structure_cohort_table()`](https://for-cast.github.io/landisutils/reference/growth_structure_cohort_table.md),
  [`plot_growth_structures()`](https://for-cast.github.io/landisutils/reference/plot_growth_structures.md)
  and
  [`read_landscape_cohort_structures()`](https://for-cast.github.io/landisutils/reference/read_landscape_cohort_structures.md)
  read a structure factorial back.
  [`growth_structure_design()`](https://for-cast.github.io/landisutils/reference/growth_structure_design.md)
  and
  [`growth_calibration_partition()`](https://for-cast.github.io/landisutils/reference/growth_calibration_partition.md)
  already BUILT the batched design here while every consumer reduced it
  downstream, which is what let the `map_code` renumbering be a trap
  rather than an invariant: the partitioner numbers `map_code` from 1
  WITHIN each batch, so grouping on it alone merges one cell per batch
  into a single fictional stand carrying more cohorts than any structure
  holds.
  [`growth_structure_cell_curves()`](https://for-cast.github.io/landisutils/reference/growth_structure_cell_curves.md)
  keys on `(batch, map_code)` and asserts `batch` is present, so the
  mistake is now an error in the package that creates the condition
  rather than a guard each caller has to rediscover. A single-batch run
  cannot surface it.
- A structure run’s curves carry one row per (cell, timestep, COHORT)
  because the design they join against is per cohort, while the biomass
  column is a whole-cell total, so a multi-cohort cell arrives with its
  trajectory repeated.
  [`growth_structure_cell_curves()`](https://for-cast.github.io/landisutils/reference/growth_structure_cell_curves.md)
  reduces that to one row per (cell, timestep) and attaches the cell’s
  composition as a sorted `+`-separated label, keeping a repeated
  species – two cohorts of one species is an age structure, not a
  monoculture, and collapsing to unique species would label it
  identically to a single-cohort cell.
- [`growth_fitting_windows()`](https://for-cast.github.io/landisutils/reference/growth_fitting_windows.md)
  gains `caps` and `cap_label`, an optional per-species upper bound on
  `mature_to` – e.g. the age at which an in-use reference curve
  plateaus, beyond which it carries no shape left to fit. The three
  sources now apply in increasing order of authority (derived, then
  `caps`, then `scoring`), because a hand-set bound is a constraint
  rather than a preference and must win over a cap as well as over the
  derived window. `window_source` reports which one the returned window
  came from. Previously a caller wanting a cap had to wrap this function
  and re-implement the entire `scoring` override, which is how one
  downstream copy came to carry a duplicate of it.
- [`plot_growth_structures()`](https://for-cast.github.io/landisutils/reference/plot_growth_structures.md)
  labels `linetype` only where a linetype is actually mapped. Labelling
  an aesthetic no layer uses makes ggplot2 report “Ignoring unknown
  labels” on every build.

## landisutils 0.0.132

- [`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md)
  gives a legend key only to series it actually draws. The colour scale
  carries `breaks` and `limits` so the fill and colour legends merge
  into one, and the cost of that is that a label left in the palette
  with no layer behind it renders as a key with no glyph. Both such
  labels can now be absent – `density = TRUE` replaces the per-plot
  points with a hex density carrying its own scale, and `binned = NULL`
  drops the binned series – so a bundle drawn both ways showed two
  legend entries for things not on the panel.
- `.growth_series_key()` builds the per-key styling FROM THE KEYS
  PRESENT rather than from a fixed order. With a shorter key set the
  fixed-length vectors failed inside the legend drawing with
  “replacement has 5 rows, data has 4”, which names neither layer nor
  scale.
- [`simplifyCohorts()`](https://for-cast.github.io/landisutils/reference/simplifyCohorts.md)
  CONSERVES biomass when it merges pixel groups. Each merged community
  now takes, per species, the mean of the stand totals of the pixel
  groups it pools, and divides that among the retained age classes in
  proportion to age. Previously every age class received its own scaled
  copy of the mean COHORT biomass, so a community’s biomass grew with
  the number of age classes pooled into it instead of being conserved;
  the signature was exact, with `sum(CohortBiomass)` equal to
  `max(CohortBiomass) * sum(age / max(age))` in 100% of species-in-cell
  cases. Measured on one landscape whose `cohortData` carries a median
  stand biomass of 119 t/ha against 116 t/ha observed, the initial
  communities built from it carried a median 440 t/ha and exceeded the
  succession extension’s own `maxB` by four to thirteen times; after the
  fix the same landscape gives 148 t/ha. Shares are computed over the
  DISTINCT retained age classes because
  [`prepInitialCommunities()`](https://for-cast.github.io/landisutils/reference/prepInitialCommunities.md)
  deduplicates its rows. Conservation is per merged community and the
  mean over pooled pixel groups is unweighted, so landscape biomass is
  conserved only up to that weighting.

## landisutils 0.0.131

- [`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
  writes a README describing what it ACTUALLY DREW. The panels vary with
  the arguments – points or a hex density, a binned series or none – and
  a fixed description drifts from them silently. It had been describing
  diamonds a caller could switch off by passing
  `reference_curves = NULL`, and a fitting-window rule of “0.45 x
  longevity” that stopped being how the cap is computed in 0.0.124,
  where it became the species’ own onset of age-related mortality.
- Where the binned series is not drawn, the README says so and points at
  `review-summary.csv` for the residual, rather than omitting it: the
  series is still what the score is computed against whether or not it
  is on the panel.

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
