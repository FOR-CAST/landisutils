# Changelog

## landisutils 0.0.119

- [`plot_growth_calibration()`](https://for-cast.github.io/landisutils/reference/plot_growth_calibration.md),
  [`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md)
  and
  [`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
  default `x_max` to `NULL`, meaning “as far as the data goes”, instead
  of a hard-coded 400. The calibration horizon moved to 600 years
  precisely so western hemlock (longevity 650) would reach senescence
  inside the run, and the panels were then silently cutting the last 200
  years off – the simulations were correct and the figures simply
  stopped drawing, with nothing on the axis to say the curve continued.
  Pass a number to clip deliberately, as the structure figures do.

## landisutils 0.0.118

- New exported
  [`repair_fwi_daily()`](https://for-cast.github.io/landisutils/reference/repair_fwi_daily.md),
  the single entry point for making BioSIM `FWI_Daily` output
  trustworthy: it repairs `FFMC`, `DMC` and `DC` via
  [`repair_fwi_exponent()`](https://for-cast.github.io/landisutils/reference/repair_fwi_exponent.md),
  re-derives `BUI`, `ISI` and `FWI` from the repaired codes, and
  validates the result against physical bounds.
  [`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md)
  now calls it instead of carrying its own copy of that logic. Exported
  because projects with their own BioSIM fetch need the identical
  correction, and a repair that lives in one project is a repair every
  other project rediscovers the hard way – which is what happened here.
- `BUI`, `ISI` and `FWI` are documented as DISCARDED rather than
  repaired. They carry the same artifact as the codes (`BUI` drops below
  `1e-4` whenever `DMC` does), but being pure functions of the codes
  they can simply be recomputed, which needs no threshold and cannot
  mistake a genuine extreme for corruption. Nothing is lost: on
  uncorrupted records the recomputed `BUI` reproduces BioSIM own to
  within `5e-6` relative, and `ISI` and `FWI` to within 0.8%.

## landisutils 0.0.117

- **Removed the internal `.growth_plot_summary_colour`.** Use
  `growth_plot_palette()[["summary"]]`. It was kept in 0.0.115 only
  because a co-developed project reached for it with `:::`; that project
  pins its own copy of the value, so the compatibility shim bought
  nothing but a second definition to keep in step. **A consumer calling
  `landisutils:::.growth_plot_summary_colour` will now error** –
  reaching into a namespace was never a supported contract, and the
  exported palette is the replacement. Rendered figures are unchanged
  (verified byte-identical).

## landisutils 0.0.116

- New exported
  [`repair_fwi_exponent()`](https://for-cast.github.io/landisutils/reference/repair_fwi_exponent.md),
  applied by
  [`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md)
  to `FFMC`, `DMC` and `DC` before any index is derived from them.
  BioSIM returns the FWI System values as text and drops the minus sign
  from the exponent whenever a value is small enough to be written in
  scientific notation (below `1e-4`), so a true `6.49936E-05` comes back
  as `649936` – mantissa and exponent magnitude intact, sign gone.
  `v * 10^(-2 * floor(log10(v)))` therefore recovers the original
  exactly. Only saturated fuels drive the codes that low, so the
  artifact is confined to wet, low-hazard records, which is precisely
  why it goes unnoticed: the values are large rather than missing, so no
  [`is.na()`](https://rdrr.io/r/base/NA.html) or `-9999` sentinel check
  sees them, and a single contaminated cell destroys any mean taken over
  cells or days. Reproduced against the live server on 2026-08-11 with a
  one-cell, one-year call (59 of 365 days affected); reported upstream.
- [`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md)
  repairs `DMC` and `DC`, not just `FFMC`. A corrupt `DMC` flowed
  straight into `buildup_index()` and from there into `FWI`, and the
  previous assertion block bounded `DMC`, `DC` and `BUI` from below
  only, so nothing caught it.
- [`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md)
  recovers a corrupt `FFMC` in place instead of recomputing it from the
  previous day. The old path called `fine_fuel_moisture_code()` with
  `dplyr::lag(FFMC)` over an UNGROUPED batch of many cells, so the
  “previous day” was routinely a different cell entirely, and the lag
  was taken from the same corrupt column it was correcting. The exponent
  repair is exact and needs no neighbouring record.
- [`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md)’s
  post-repair assertions gained upper bounds on `DMC`, `DC`, `BUI` and
  `FWI`. They are generous envelopes well above anything the Canadian
  FWI System produces, and exist to fail the fetch on an uncharacterised
  fault rather than to clip legitimate extremes.

## landisutils 0.0.115

- New exported
  [`growth_plot_palette()`](https://for-cast.github.io/landisutils/reference/growth_plot_palette.md),
  a named character vector of the growth figures’ colours keyed by ROLE
  (`current`, `candidate`, `plots`, `faint`, `summary`, `reference`,
  `window`, `key_outline`). The figure families here and in the projects
  that consume them are read side by side, so a colour has to mean the
  same thing in all of them; until now `steelblue4` was defined once in
  this package and again in a co-developed project, and `white`,
  `grey35`, `goldenrod2` and `black` each recurred within this file. A
  named vector because that is exactly what `scale_*_manual(values = )`
  takes, and a function rather than an exported constant so it can
  accept overrides (`growth_plot_palette(candidate = "darkorange")`)
  without breaking callers. An unknown role is an error, not a silently
  ignored typo.
- Colours only, deliberately: linetypes have
  [`scale_linetype_growth_reference()`](https://for-cast.github.io/landisutils/reference/scale_linetype_growth_reference.md)
  and the review panel’s per-series key spec is positional and specific
  to that figure, so folding all three into one table would couple
  things that change for different reasons.
- Every colour in
  [`plot_growth_calibration()`](https://for-cast.github.io/landisutils/reference/plot_growth_calibration.md)
  and
  [`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md)
  now resolves through the palette. Verified output-neutral: the
  rendered review panel is byte-identical before and after.
  `.growth_plot_summary_colour` is retained, since a co-developed
  project reaches for it directly, but is now derived from the palette
  so the two cannot drift.

## landisutils 0.0.114

- [`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md)
  draws the age-binned points ON TOP of the plot-cloud smooth instead of
  under it. The smooth is an aid; the points are what the ground-plot
  term of the score is computed against, and a line drawn over them hid
  the very values it summarizes. The points gain a white outline, which
  separates a point from the line exactly where the two coincide and the
  reader is checking agreement.
- The binned points now map `fill` (shape 23 takes its interior from
  fill), so the fill scale is given the same name, breaks and limits as
  the colour scale and ggplot2 merges the two into one series legend.
  Silencing the fill guide instead leaves that key labelled but
  glyph-less: `override.aes` can only restyle a key that some layer
  contributes, and once nothing maps colour to the series there is no
  key to restyle. `override.aes` is supplied on the colour guide only,
  since giving it to both merges correctly but warns.

## landisutils 0.0.113

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  warns when the realized worker fleet is smaller than `NP`, reporting
  the resulting number of evaluation waves, how many workers idle at the
  barrier, and the per-host split. DEoptim dispatches `NP` evaluations
  per generation and `parLapply` splits them into `length(cl)` chunks,
  so a generation costs `ceiling(NP / fleet)` evaluation times: the cost
  is a STEP function, and going from `NP` to `NP - 1` workers doubles
  the generation while leaving almost every worker idle for the second
  half. Observed as 89 workers against `NP = 90`, which turned a 3.9 h
  generation into ~7.8 h for want of one container, with nothing in the
  log to say so. This warns rather than aborts because running
  deliberately below `NP` is reasonable when a host cannot hold that
  many containers; the point is that the trade is visible and quantified
  instead of being discovered from a wall clock that is quietly doubled.

## landisutils 0.0.112

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  accepts a SUBSET of
  [`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md)
  in `cfg$lower` / `cfg$upper`, searching only the parameters named and
  ordering them canonically. 0.0.111 taught
  [`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md)
  to leave an unnamed field at its template value but left a strict
  [`setequal()`](https://rdrr.io/r/base/sets.html) at the entry point,
  so a deliberately reduced 7-parameter configuration still errored
  before the first trial.
- New internal `.par()` reads one calibrated parameter with a default.
  `par_vec[["name"]]` on an ATOMIC vector raises a subscript error for a
  missing name rather than returning `NULL`, so the previous
  `par_vec[["x"]] %||% default` idiom protected nothing – the error
  fires before `%||%` sees a value. This is the third site where that
  assumption broke once the calibrated set became a subset, after the
  fuel-base multipliers and the entry-point check.

## landisutils 0.0.111

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  scores severity on the THREE categories the observed reference
  actually distinguishes, not five. Every reference in use is 3-class at
  source – CanLaBS applies two dNBR thresholds, the BC layer is
  Low/Medium/High – and is projected onto LANDIS 1-5 by a trapezoid
  kernel that splits low across classes 1 and 2 and high across 4 and 5.
  An observed vector therefore always has the form `(a, a, b, c, c)`,
  and that equality is an artifact of the projection rather than a
  measurement, so scoring on five charged the simulator for its
  within-low and within-high shape while the observation said nothing
  about it. Measured on a 397,100-cell landscape: of 1.02 total absolute
  error against the study-area reference, 0.79 came from that split
  alone, while the aggregate low/medium/high proportions nearly matched
  (0.888 simulated against 0.810 observed). Both sides now collapse
  before scoring;
  `options(landisutils.calibration.severity_classes = 5L)` restores the
  previous behaviour for comparison.
- [`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md)
  accepts a SUBSET of
  [`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
  leaving any field whose parameter is absent at its template value.
  Requiring all nine forced every calibration to search dimensions that
  can be degenerate for the fire regime at hand: with foliar moisture
  content capped at 120% outside the mid-summer dip,
  `SpFMCLo == SpFMCHi` and `FallFMCLo == FallFMCHi`, so `SpHiProp` and
  `FallHiProp` cannot change any outcome – two of nine dimensions spent
  on pure noise, and a contributor to an objective that behaved as a
  step function.
- [`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md)
  no longer errors when a fuel row’s base type is not among the
  calibrated multipliers. `[[` on an atomic vector raises a subscript
  error for a missing name rather than returning `NULL`, so the existing
  `!is.null()` guard was dead code that held only because all five bases
  were always present.

## landisutils 0.0.110

- The `vdyp` series is now actually drawn. 0.0.109 widened the plots’
  source filter but left `.growth_reference_linetypes` at
  `c(SORTIE, TIPSY)`, and the scale is built with `limits = names(...)`
  and `na.translate = FALSE` – so a source missing from that vector is
  DROPPED from the plot rather than drawn in a default style. A VDYP
  curve was therefore scored, filtered in, and still invisible, with the
  only symptom a “Removed N rows containing missing values” warning that
  reads like harmless clipping. A test now asserts the vector covers
  every modelled source.

## landisutils 0.0.109

- The `vdyp` reference series added in 0.0.101 was only wired into
  scoring.
  [`plot_growth_calibration()`](https://for-cast.github.io/landisutils/reference/plot_growth_calibration.md)
  and the review-bundle plots filtered the modelled curve on
  `c("SORTIE", "TIPSY")`, so a VDYP-labelled curve was scored but drawn
  nowhere – the failure mode was a missing line on a figure, with
  nothing raised.
  [`growth_reference_inflection()`](https://for-cast.github.io/landisutils/reference/growth_reference_inflection.md)
  and
  [`growth_auto_window()`](https://for-cast.github.io/landisutils/reference/growth_auto_window.md)
  defaulted `sources` the same way: the former silently returned its
  `default` instead of the computed inflection (100 rather than 12 on
  the test fixture), and the latter skipped the
  `min(upper, max(model_ages))` tightening, widening the fitting window
  from 150 to 212 where the modelled curve ends before the plots do. All
  four now include `"VDYP"`.

## landisutils 0.0.108

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  verifies that every worker actually holds a RUNNING container before
  handing the fleet to DEoptim, restarting what it can and aborting –
  naming the hosts and counts – on what it cannot. `clusterCall()`
  reports only hard errors, so a container that started and then died
  left the worker silently container-less; because a failed trial is
  scored as a penalty rather than an error, the search then ran on with
  a large slice of its population frozen. A 90-worker fleet was observed
  coming up with 56 and running that way for 2.6 days with nothing
  logged. Pool containers run with `--rm`, so a dead one is removed
  rather than left in `exited` and is invisible to any after-the-fact
  check – the probe asks the daemon whether the name still resolves to a
  running container.
- [`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)
  gains a working `timeout_sec`, and
  [`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md)
  /
  [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  gain `trial_timeout_sec` to reach it. `retries` only rescues a
  simulator that EXITS; one that wedges never returns, and the
  coordinator – parked in a blocking read on that worker’s socket –
  waits with it. One search hung ~16 h before its supervisor noticed. A
  timed-out attempt now consumes a retry and restarts the container, so
  an indefinite hang becomes a bounded, self-healing failure.
- [`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)
  catches the timeout raised by processx, which is signalled as a
  condition rather than a non-zero exit status and so was not covered by
  `error_on_status = FALSE`. Left to propagate it would unwind
  `parApply` and DEoptim and discard every generation since the last
  checkpoint – strictly worse than the hang it is meant to cure.
- `trial_timeout_sec` is deliberately excluded from both the population
  and loss-config fingerprints, so it can be set on an in-flight search
  without invalidating its checkpoint.

## landisutils 0.0.107

- [`growth_smooth_observations()`](https://for-cast.github.io/landisutils/reference/growth_smooth_observations.md)
  clamps the fitted curve at zero, not just its confidence band. An
  unconstrained spline dips slightly negative at the young end wherever
  the plots themselves start above zero – visible on real hemlock data
  around age 15 – and clamping only `lo` left `lo > value` there, a band
  that did not contain its own curve.

## landisutils 0.0.106

- [`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md)
  no longer joins the age-binned points with line segments. A straight
  line between two bin medians asserts a trajectory across ages where
  nothing was measured, and most of the movement it described came from
  bins holding a single plot.
- The binned points and the fitted curve now share one colour, since
  they summarize the same observations two ways; glyph tells them apart.
  Drawing them in different hues implied two independent series.
- The review panels are taller and their legend box tighter, so four
  stacked guides no longer take half the figure. Guide order is now
  fixed rather than left to scale-construction order, which had the six
  panels of a species set arranging their legends six different ways.

## landisutils 0.0.105

- [`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md)
  sizes each age-binned point by the number of plots behind it, instead
  of drawing every bin at the same size. The counts differ by more than
  an order of magnitude – in the network this was built against, 34% of
  20-year bins hold a single plot and 66% hold fewer than five – so an
  equal-sized point presented a lone observation as a median.
  Bootstrapping the bin medians there put 3 of 4 consecutive steps for
  one species, and 4 of 5 for another, inside their own sampling
  intervals: the sharp reversals in the series are mostly its smallest
  points, and now they look like it.
- [`growth_smooth_observations()`](https://for-cast.github.io/landisutils/reference/growth_smooth_observations.md)
  fits a thin-plate spline through a ground-plot cloud and returns it
  with a confidence band, and
  [`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md)
  gains a `smooth` argument that overlays it. This is a visual aid for
  judging the binned series and nothing scores against it:
  [`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md)
  still bins, because replacing the scored reference would move every
  `biomass_max_est` that rests on plots. The fit is on the identity
  scale – a log link is the obvious response to right-skewed biomass but
  extrapolates violently from a handful of old plots, lifting one
  species’ curve to 347 Mg C/ha against a binned maximum of 238 – and it
  is never drawn outside the observed age range.
- [`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
  draws the smooth on every panel by default (`smooth_plots`), and takes
  `smooth_site` so the fit collapses repeat visits exactly as the binned
  series does. Its `README.txt` explains both additions.

## landisutils 0.0.104

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)’s
  evaluation fingerprint actually includes the scenario template now.
  The digest helper added in 0.0.75 required its argument to be a
  directory, but every caller passes the scenario FILE
  (`.../scenario.txt`) –
  [`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md)
  takes [`dirname()`](https://rdrr.io/r/base/basename.html) of the same
  value for precisely that reason – so it returned `NULL` on every real
  call and the template contributed nothing to the fingerprint. The
  failure it was written to prevent therefore persisted silently: a
  calibration relaunched after refitting the fire-size distribution
  reproduced the previous run’s fingerprint exactly, replayed five
  generations of cached losses in ten minutes, and would have reported
  the old optimum as if it were new. A silent `NULL` is
  indistinguishable from a template that genuinely has not changed, so
  the helper now accepts either a directory or a file inside one, and
  warns rather than returning `NULL` quietly when neither resolves.

## landisutils 0.0.103

- Tearing down a multi-node calibration no longer orphans every
  container when one worker is unreachable. The teardown issued a single
  cluster-wide `clusterCall()`, which is all-or-nothing – it dispatches
  to every node and waits for every reply, so one unresponsive worker
  errored the whole call, and a `try(silent = TRUE)` around it discarded
  the error. Observed twice on a 45-worker fleet, once after a
  deliberate stop and once after a clean completion; losing all 45
  rather than a subset is the signature of the single call failing
  wholesale, and nothing in the run’s output said so. Each worker’s pool
  is now stopped independently, and any that cannot be reached produce a
  warning naming them, so stragglers are visible instead of silently
  consuming a node.
- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  applies its early-stopping test every generation once the history is
  long enough to evaluate it, instead of only at `checkpoint_every`
  boundaries. The test needs a history longer than `steptol`, so at
  `checkpoint_every = 5` and `steptol = 25` the earliest checkable
  generation is 26 while the boundaries fell on 25 and 30 – a search
  that had converged by 26 ran four more generations, about eleven hours
  at this landscape’s generation time, and the overshoot grows with
  `checkpoint_every`. Blocks now shrink to land exactly on `steptol + 1`
  and advance singly thereafter, costing one small checkpoint write per
  generation.
- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  reports at startup when `steptol >= itermax`, which makes early
  stopping unreachable rather than merely unlikely: the convergence test
  can never see a history longer than the whole budget, so the search
  always runs to `itermax`. That is the documented way to disable early
  stopping, so the behaviour is unchanged – but it is indistinguishable
  from a broken stopping rule unless it is stated.
- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)’s
  documentation no longer claims that `cfg$steptol = NULL` falls through
  to DEoptim’s own `steptol = itermax`. It does not: `NULL` is treated
  as absent and yields the 25-generation default, which is the opposite
  of disabling early stopping.

## landisutils 0.0.102

- [`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md)
  derives its warm-pool settings from `cfg` instead of hard-coding them.
  It passed `mem_limit = "8g"` regardless of configuration, so on a
  397,100-active-cell landscape whose measured ForCS peak is 11.0-11.1
  GiB, all 20 validation replicates died in `ForC.SiteVars.Initialize`
  with `System.OutOfMemoryException` about 150 s in – immediately after
  a 30-generation calibration of that same scenario had completed
  successfully on its 13 GiB grant. Validation re-runs the scenario the
  search just tuned, at the same landscape size, so a grant that differs
  from the calibration’s is wrong by construction. It now also forwards
  `cfg$image` (previously resolved from a global option that a
  long-lived crew worker can hold stale, which callers were working
  around by temporarily overwriting that option) and `cfg$retries`
  (validation failed after a single attempt while the search that
  produced the parameters got three).
- The per-container RAM estimate and grant move into
  `.cfg_mem_per_worker()` and `.cfg_mem_limit()`, called by both pools.
  Computing them independently is what let the two drift apart, and the
  drift was invisible until a landscape grew past the hard-coded figure.
  Note the no-config default is a 10 GiB grant (an 8 GiB estimate plus
  25% headroom), which is *below* what a ~400,000-active-cell ForCS
  landscape needs – configs at that scale must set `mem_per_worker_gb`.

## landisutils 0.0.101

- [`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md)
  gains a `vdyp` series alongside `sortie` and `tipsy`, switched on by
  `use_vdyp` and weighted per species by a new `weight_vdyp` column in
  `growth_scoring.csv`. VDYP is British Columbia’s Variable Density
  Yield Projection model, and it needed its own slot rather than
  borrowing TIPSY’s: TIPSY projects MANAGED stands and VDYP projects
  unmanaged natural ones, which is exactly why a natural-disturbance
  model reaches for VDYP. Carrying it as TIPSY made `rmse_tipsy` a VDYP
  residual, so a reader had no way to tell from the outputs which model
  produced the number.
  [`growth_score_fit()`](https://for-cast.github.io/landisutils/reference/growth_score_fit.md)
  now reports `rmse_vdyp` separately and
  [`growth_best_candidates()`](https://for-cast.github.io/landisutils/reference/growth_best_candidates.md)
  carries it through.
- `weight_vdyp` defaults to 0 and
  [`read_growth_scoring()`](https://for-cast.github.io/landisutils/reference/read_growth_scoring.md)
  tolerates its absence, so a scoring file written before this release
  reads unchanged and no existing calibration is rescored by the series
  merely existing.
- Three upstream test-input citations pointed at paths that no longer
  resolve. The Dynamic Fire references moved with an upstream version
  bump (`testings/Core8-DynamicFire4.0` is now `4.1`), and the
  Social-Climate-Fire test cited `Core7-SCRAPPLE3.2`, which has never
  existed – `Testing/` holds 1.0, 2.0, 2.6 and 3.0. Each replacement was
  confirmed by matching content rather than by picking the nearest
  surviving directory: the season proportions and the site-mortality
  coefficients used in those tests appear verbatim in the files now
  cited. These citations are the provenance for every reference value in
  the tests they head, so one that resolves to nothing is worse than a
  broken link – it makes the values unverifiable. Two of the three were
  also reported by `R CMD check` as invalid URLs in the rendered
  vignette; the third sits in a test comment, which `R CMD check` does
  not examine.
- `Title` is in title case, per `R CMD check --as-cran`.

## landisutils 0.0.100

- `calibrate_original_fire()` is no longer exported, and errors with
  “not yet implemented” if it is reached. It has been a `## TODO` with
  an empty body since the R6 rework that created it, but it carried an
  `@export` and a generated man page the whole time – so it appeared in
  the reference index as public API, took no arguments, and returned
  `NULL` without complaint. A calibration that silently returns nothing
  is indistinguishable from one that ran and found nothing, which is the
  worst way for this particular function to fail. The Original Fire
  *extension* writer (`OriginalFire`) is implemented and tested; it is
  only the calibration that does not exist, and it stays tracked in \#2
  rather than being deleted, since `calibrate_dynamic_fire.R` points at
  it as the structure a second calibration would follow.

## landisutils 0.0.99

- `arrow`, `ggplot2`, `ggalluvial` and `cffdrs` moved from `Imports` to
  `Suggests`, cutting the hard-dependency closure from 62 packages (251
  MB) to 31 (98 MB). Most consumers of this package write LANDIS-II
  configuration files, run the model and read its rasters; they never
  open a parquet dataset, draw a figure, or recompute a Fire Weather
  Index, and until now installing it made them wait for all three
  anyway. `arrow` alone is 99 MB. `cffdrs` was the worst value for
  money: 16 packages nothing else here needs – `class`, `classInt`,
  `DBI`, `doParallel`, `e1071`, `foreach` among them – to support one
  function, and the `@importFrom` it carried was dead, naming a binding
  nothing ever called. Note that `stringr` and `purrr` are NOT
  candidates for the same treatment despite being used in one and four
  files: `tidyr` imports both, so moving them would save nothing at all.
- Every entry point that reaches one of those packages now checks for it
  first and, if it is missing, says what it was trying to do and what to
  install. Previously the failure was
  `there is no package called 'arrow'` raised from wherever the call
  stack happened to be. Most of the climate functions already guarded
  `arrow` this way; the two ForCS parquet helpers and the three
  biomass-snapshot readers and writers did not.
- A static scan now walks `R/` for calls into those packages and fails
  the test suite if the enclosing function does not guard, plus a
  companion check that none of them has drifted back into `Imports`.
  Both failures are otherwise invisible: an unguarded call passes
  `R CMD check` and every test on any machine that happens to have the
  package installed, and surfaces only for a user who does not.

## landisutils 0.0.98

- New
  [`validate_landis_scenario()`](https://for-cast.github.io/landisutils/reference/validate_landis_scenario.md)
  checks an assembled scenario directory before LANDIS-II ever sees it,
  and both
  [`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md)
  and
  [`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
  now call it by default
  (`options(landisutils.validate_scenario = FALSE)` opts out). LANDIS-II
  reports bad inputs by dying a few seconds into extension
  initialisation with a non-zero exit and EMPTY stderr, leaving the real
  message in `Landis-log.txt` inside a scratch directory; under a
  calibration warm pool that is multiplied by the pool size and can burn
  hours before anyone reads a log. The checks are the three defects that
  actually happened here in one week: a pixel type the GDAL reader
  rejects (0.0.70), an undeduplicated initial-communities CSV that
  exhausts the container’s memory inside the parser (0.0.68), and a
  vertically mirrored map (0.0.95). It validates the DIRECTORY rather
  than the `LandisExtension` objects deliberately, because the two
  assembly entry points do not share a code path: the Dynamic Fire
  calibration goes through
  [`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md),
  bulk-copying a template directory and swapping in a spinup snapshot,
  so no extension object ever describes the file the mirroring defect
  landed in.
- The mirrored-map check is what the rest exists for. A map written in
  the wrong row order has the right dimensions, the right values and the
  right totals, so nothing rejects it and the run completes with the
  vegetation displaced relative to the ecoregion, fire-region and
  topography maps – it cost a 25-generation Dynamic Fire calibration.
  Orientation metadata cannot find it either, because the mirrored file
  is written back north-up and only its content is reversed. So the
  check compares content: per-cell active-mask agreement with the
  ecoregions map, against the agreement the map’s vertically flipped
  self would score. Flipping swaps the pair exactly, so a map that
  agrees better flipped than as stored is mirrored, and no absolute
  threshold is involved. Measured on two assembled scenarios, initial
  communities scored 0.9720/0.7605 and 0.9798/0.4945 as-is versus
  flipped. Continuous maps (slope, aspect, ignition and suppression
  rasters) are exempt: 0 is a legitimate value on an active cell there,
  so the mask is not a footprint, and uphill azimuth measured 0.5929
  both ways – no discrimination to be had. A mirrored topography map
  remains undetectable this way.
- The initial-communities check is deliberately not the stricter “every
  active cell carries a map code” it looks like it should be. Measured
  on working production scenarios, 10,897 and 95,063 active ecoregion
  cells carry no initial-communities code – cells with no cohorts, which
  Biomass Succession handles – so the strict form would reject valid
  input. What it does check is that every map code present in the raster
  resolves to CSV rows, allowing the one deliberately row-less
  empty-community code
  [`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md)
  creates.
- Two per-extension contracts are checked from the written configuration
  rather than from the objects that produced it, because the calibration
  patches `dynamic-fire.txt` with candidate parameters after the writer
  has run: season `ProportionFire` values must be dyadic fractions
  summing to 1 (the parser sums them in single precision and aborts with
  “Season Probabilities don’t add to 1.0” otherwise), and the
  fire-regions map must cover every cell the core considers active (a
  core-active cell with no fire region aborts with “Unknown map code”).
  Dynamic Fuels is checked for species coverage: a modelled species
  absent from every `FuelTypes` row is simply not in the fuels model,
  silently.
- All problems are collected and reported together rather than raised
  one at a time – a scenario with three defects should not cost three
  build cycles. `validate_landis_scenario(error = FALSE)` returns them
  instead of stopping, for surveying scenarios without failing.
- [`landis_directive()`](https://for-cast.github.io/landisutils/reference/landis_directive.md)
  replaces the calibration-private config-file reader, so “read a
  directive out of a LANDIS-II config file” has one implementation.

## landisutils 0.0.97

- The core-version check now covers the Docker paths, which is where it
  was missing entirely.
  [`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md)
  has asserted the console’s major version since it gained
  `check_version`, but neither
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  nor
  [`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md)
  ever called it – and `landisutils.run.method` defaults to `"docker"`
  on Linux, so on a cluster that runs through containers the assertion
  existed and never once fired. A whole 45-worker calibration fleet
  could run against the wrong core generation with nothing to say so.
  [`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md)
  now probes one container per pool (all containers in a pool share an
  image, so the sub-second cost is paid at pool creation, not per
  replicate) and tears the pool down if it fails, rather than leaving
  containers running for a pool nobody will use;
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  probes its image before staging anything.
- New
  [`landis_assert_version()`](https://for-cast.github.io/landisutils/reference/landis_assert_version.md)
  replaces the internal `.assert_landis_major()`, so the local-console
  path and both Docker paths enforce the same rule through the same
  code. **Behaviour change:** a version that cannot be determined now
  STOPS instead of warning and proceeding. An unreadable probe is not
  evidence of a usable core – it is indistinguishable from a
  wrong-generation console that failed to announce itself, which is the
  failure this guards against. Set
  `options(landisutils.skip_version_check = TRUE)` for the cases where
  that is genuinely wanted; the opt-out is explicit and visible rather
  than silent.
- New
  [`landis_target_version()`](https://for-cast.github.io/landisutils/reference/landis_target_version.md)
  reads the `landisutils.landis.version` option, which `.onLoad()` seeds
  from a single constant. That constant is the switch to flip when v9
  supersedes v8: the runtime guard,
  [`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md)’s
  `required_major` default, the default image tag and the default
  console path all derive from it instead of repeating the number, so a
  bump moves them together rather than leaving a stale `v8` in whichever
  one got missed. Flipping it is necessary but not sufficient – the
  config writers in `ext_*.R` encode the grammar itself, so treat a bump
  as the start of that work.
- [`landis_version()`](https://for-cast.github.io/landisutils/reference/landis_version.md)
  accepts `image` and `container` alongside `console`, so “run the
  console and read its banner” has one implementation rather than one
  per execution path.

## landisutils 0.0.96

- [`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md)
  restarts the container under its existing name instead of generating a
  fresh `-r<rand>` one. `landis_pool` is a plain list, so the rename
  reached only the frame that called the restart; every frame above it –
  including whichever one owns the pool – kept the name of the container
  that had just been removed. The result was self-sustaining rather than
  transient: after one genuine container failure, that worker’s every
  subsequent
  [`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)
  addressed a container that no longer existed, failed, restarted,
  abandoned the replacement, and succeeded only on its retry. Observed
  in a 90-worker calibration as one worker accumulating an idle
  container per replicate (about 3.5 per hour) while still returning
  correct results, so nothing in the run’s own output showed it. A
  stable name is also what lets the container name serve as a
  cross-process mutex against duplicate dispatch, which the random
  suffix defeated. Restarts now tolerate the daemon briefly still
  holding the name after `docker rm -f`, waiting up to 30 s for it
  rather than sidestepping the conflict.

## landisutils 0.0.95

- [`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md)
  reads the map-code raster through
  [`read_landis_raster()`](https://for-cast.github.io/landisutils/reference/read_landis_raster.md),
  so the snapshot it rewrites keeps the row order LANDIS-II wrote. It
  used
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
  which reverses the rows of a south-up file, and wrote that back: every
  initial-communities map built from a spinup snapshot was a vertical
  MIRROR of the landscape it was taken from, putting the simulated
  vegetation in the wrong place relative to the ecoregion, fire-region
  and topography maps it has to line up with. A landscape spun up with
  an earlier version has to be rebuilt.
- [`georef_landis_raster()`](https://for-cast.github.io/landisutils/reference/georef_landis_raster.md)
  restores the written row order before stamping on the template’s CRS
  and extent, whenever it can see the file the raster came from (a path,
  or a `SpatRaster` still backed by one). Georeferencing a mirrored
  raster against rasterToMatch yields a map that is wrong everywhere
  except in its landscape totals: core-versus-buffer attribution,
  reporting-polygon summaries and every map figure. A `SpatRaster`
  already loaded into memory carries no record of how it was read and
  cannot be checked, so read LANDIS-II outputs with
  [`read_landis_raster()`](https://for-cast.github.io/landisutils/reference/read_landis_raster.md)
  and derive from there.
- New
  [`read_landis_raster()`](https://for-cast.github.io/landisutils/reference/read_landis_raster.md)
  reads a LANDIS-II output GeoTIFF in the row order LANDIS-II wrote it.
  `Landis.RasterIO.Gdal` sets no geotransform at all, which GDAL reports
  as a south-up raster (origin at (0, 0), positive pixel height), and
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  normalises south-up rasters by reversing their rows – so opening a
  LANDIS-II output directly with terra silently returns a vertical
  mirror of the simulated landscape. Whether a file is stored south-up
  is checked per file rather than assumed, so a LANDIS-II release that
  starts writing a proper geotransform will need no change here.

## landisutils 0.0.94

- [`growth_best_candidates()`](https://for-cast.github.io/landisutils/reference/growth_best_candidates.md)
  reports the range of `biomass_max_est` and `anpp_max_est` across the
  candidates that cannot be told apart from the winner, and warns when
  the winning curve never approached its own asymptote. Ranking is on
  shape and the level is recovered afterwards by dividing by that
  fraction, so an error in the simulated curve reaches the recommended
  level multiplied by `1 / achieved_frac`. In the calibration this was
  built against, a species whose best curve stopped at 66% of its
  asymptote had a level band of 32,700 to 125,000 across candidates
  spanning 15% of error, while the two species that plateaued cleanly
  had bands of 4% and 5%. A point estimate alone does not distinguish
  those cases.
- New
  [`growth_identifiability()`](https://for-cast.github.io/landisutils/reference/growth_identifiability.md)
  reports, per species and swept parameter, the range of values spanned
  by the best-scoring candidates and the error spread across them.
  Taking an argmin presumes the objective surface has a minimum; where
  it does not, the reported combination is whichever cell sorted first,
  and nothing in a ranked table says which case you are in. It also
  flags an argmin sitting on the edge of the swept grid, which means the
  optimum may lie outside it.

## landisutils 0.0.93

- [`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md)
  maps point shape to the raw species code, so a reviewer can see which
  member of a lumped group is setting the curve. Codes routinely lump
  into one modelled species and the members are not interchangeable: in
  the network this was built against, black cottonwood carries a median
  180 Mg C/ha against trembling aspen’s 53, and the highest observations
  in the pooled cloud were all cottonwood.

## landisutils 0.0.92

- [`plot_growth_factorial_sensitivity()`](https://for-cast.github.io/landisutils/reference/plot_growth_factorial_sensitivity.md)
  classifies a candidate against the calibrated value on a RELATIVE
  tolerance. The absolute `1e-6` it used before failed silently and
  badly: swept candidates are reconstructed from a rounded ratio, so the
  candidate that IS the calibrated value can differ from it by about one
  part in three thousand, which binned it as “lower” or “higher” and
  removed the reference box the figure is read against. Five of six
  species were missing that box.

## landisutils 0.0.91

- The TIPSY reference curve is now drawn as round dots rather than
  dashes. Naming the `"dotted"` linetype is not enough: R scales dash
  lengths by line width, so at the width these figures use its one-unit
  “on” segment renders as a visible dash. The curve now uses a tighter
  pattern with round line ends, which turns each “on” segment into an
  actual dot. This matters most in the legend key, the one place a
  reader has to tell the two references apart.

## landisutils 0.0.89

- New
  [`growth_climatic_distance()`](https://for-cast.github.io/landisutils/reference/growth_climatic_distance.md)
  and
  [`growth_climatic_weight()`](https://for-cast.github.io/landisutils/reference/growth_climatic_weight.md)
  let a calibration draw on ground plots from beyond the modelled
  landscape, weighted by how closely each plot’s climate resembles it.
  Distance is scored PER PLOT rather than by aggregating plots into map
  units and comparing unit means: a unit’s climate is only as well
  estimated as the number of plots inside it, so a ranking of units is
  least reliable exactly where it gets used – at the top. Resampling put
  a 14-plot unit’s rank anywhere between 10th and 26th of 137 in the
  network this was built against, while a 95-plot unit sat within four
  ranks.
- [`growth_bin_observations()`](https://for-cast.github.io/landisutils/reference/growth_bin_observations.md)
  and
  [`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md)
  gain `weight`, naming a per-observation weight column; the within-bin
  quantile is then weighted, and the returned series carries the total
  weight behind each bin so a bin resting on many barely-relevant plots
  is visible as such. A weight is preferred to a distance cut-off
  because any threshold is arbitrary and a plot just past it is not
  meaningfully different from one just inside.

## landisutils 0.0.88

- [`growth_bin_observations()`](https://for-cast.github.io/landisutils/reference/growth_bin_observations.md)
  gains `site`, naming the column that identifies a sampling location.
  Permanent-plot networks remeasure on a schedule that reflects program
  history rather than anything ecological, so treating every visit as an
  independent observation weights each age bin toward whichever
  locations happen to have been revisited most – pseudo-replication that
  biases the binned quantile rather than merely tightening it. When
  `site` is given, each location contributes one value per bin and `n`
  counts locations rather than visits. Naming a column that does not
  exist is an error, so the correction cannot be silently skipped.
- [`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md)
  gains a matching `site`, passed through to the binning, and reports
  `n_plots` as distinct locations when it is supplied – so the
  `plots_warn_below` advisory is judged on independent evidence rather
  than on visit count.

## landisutils 0.0.87

- [`prepMinRelativeBiomass()`](https://for-cast.github.io/landisutils/reference/prepMinRelativeBiomass.md)
  no longer collapses on a landscape with a SINGLE ecoregion.
  [`apply()`](https://rdrr.io/r/base/apply.html) over a one-row table
  dropped its result to a vector, so the ecoregion code was recycled
  across the five shade classes: the written table declared the same
  ecoregion five times and carried one shade row, and LANDIS-II aborted
  loading Biomass Succession with “The ecoregion N appears more than
  once”. Single-ecoregion landscapes are rare in production but are
  exactly what a growth-curve calibration builds. It also now rejects an
  unrecognized input schema with a message naming the columns it got,
  rather than failing later with “object ‘out’ not found”.

## landisutils 0.0.86

- New vignette, *Calibrating Species Growth Curves*, documenting how to
  wire a project-side growth calibration on top of the toolkit added in
  0.0.85: how the sweep is encoded as one landscape, which parameters
  map onto the canonical names for ForC Succession and Biomass
  Succession, what `biomass_max_scale` means for each, what deliberately
  stays project-side, and why the calibration belongs in its own
  [targets](https://docs.ropensci.org/targets/) project. It also records
  the three properties that keep the ranking honest, and the reasoning
  behind each, so a second implementation does not have to rediscover
  them.

## landisutils 0.0.85

- New growth-curve calibration toolkit, extracted from a ForC Succession
  project so it can be reused. It encodes a whole parameter sweep as one
  LANDIS-II landscape and scores the resulting curves against reference
  data, and is succession-agnostic: both ForCS and Biomass Succession
  key growth and mortality shape per species but maximum ANPP and
  maximum biomass per (ecoregion, species), which is exactly the
  asymmetry the encoding exploits. Callers map their extension’s
  parameter names onto the canonical `growth_shp` / `mort_shp` /
  `anpp_max` / `biomass_max`, and pass `biomass_max_scale` to say how
  the maximum-biomass parameter relates to the units its output is
  reported in.
- [`growth_calibration_design()`](https://for-cast.github.io/landisutils/reference/growth_calibration_design.md)
  builds the sweep landscape: one pseudo-species per (species, growth
  shape, mortality shape) and one pseudo-ecoregion per (max ANPP, max
  biomass), with one cell per pair, so hundreds of parameter
  combinations run in a single simulation.
  [`growth_calibration_partition()`](https://for-cast.github.io/landisutils/reference/growth_calibration_partition.md)
  splits a design too large to hold in memory into batches, always
  cutting on cell boundaries.
  [`growth_structure_design()`](https://for-cast.github.io/landisutils/reference/growth_structure_design.md)
  crosses the sweep with a landscape’s own cohort structures.
- [`growth_score_fit()`](https://for-cast.github.io/landisutils/reference/growth_score_fit.md)
  ranks candidates on curve SHAPE alone, comparing each reference series
  against the simulated curve rescaled to that series’ own plateau.
  Level is recovered separately and exactly by
  [`growth_inflation_factor()`](https://for-cast.github.io/landisutils/reference/growth_inflation_factor.md),
  which exploits the fact that the fraction of its maximum-biomass
  parameter a cohort actually achieves depends only on the shapes and
  the ANPP-to-biomass ratio, not on the absolute level. Sweeping level
  alongside shape instead lets the two trade off against each other, and
  the ranking settles wherever the reference data happen to be centred.
- [`growth_reference_curves()`](https://for-cast.github.io/landisutils/reference/growth_reference_curves.md)
  evaluates every reference series on one common age grid, so a modelled
  curve contributing hundreds of points and a plot cloud contributing a
  dozen carry equal weight. Ground-plot observations are condensed
  non-parametrically by
  [`growth_bin_observations()`](https://for-cast.github.io/landisutils/reference/growth_bin_observations.md),
  which bins on age and takes a quantile per bin; fitting an empirical
  growth equation through the cloud instead would introduce a curve
  family the succession extension does not use.
- [`growth_auto_window()`](https://for-cast.github.io/landisutils/reference/growth_auto_window.md)
  derives the age range to fit over rather than requiring one: it opens
  at an age floor below which ground-plot programs do not sample, and
  closes at the earliest of a quantile of observed plot ages, the end of
  the reference curve, and a fraction of `longevity` – before
  LANDIS-II’s senescence ramp drives the curve to zero.
  [`growth_fitting_windows()`](https://for-cast.github.io/landisutils/reference/growth_fitting_windows.md)
  applies per-species overrides on top.
- [`growth_best_candidates()`](https://for-cast.github.io/landisutils/reference/growth_best_candidates.md)
  reports `fitted = FALSE` for a species with no scorable reference
  rather than ranking indistinguishable rows and returning whichever
  sorted first, and treats a nominated level source as a constraint
  rather than a preference, so an unavailable reference yields no
  recommendation instead of a silent substitution.
- [`plot_growth_calibration()`](https://for-cast.github.io/landisutils/reference/plot_growth_calibration.md),
  [`plot_growth_candidate()`](https://for-cast.github.io/landisutils/reference/plot_growth_candidate.md),
  [`plot_growth_factorial_sensitivity()`](https://for-cast.github.io/landisutils/reference/plot_growth_factorial_sensitivity.md)
  and
  [`write_growth_review_bundle()`](https://for-cast.github.io/landisutils/reference/write_growth_review_bundle.md)
  produce a standalone review bundle for the manual step between
  calibrating and promoting parameters. Every drawn series is mapped
  rather than given a bare colour, so each one earns a legend key.

## landisutils 0.0.84

- [`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md)
  honours `LANDIS_CONSOLE` again. The condition was inverted: when the
  variable WAS set its value was discarded and replaced by the `/opt`
  filesystem search, and when it was unset the function returned `""`.
  Since `/opt` is a Linux convention that does not exist on Windows –
  where `method = "local"` is the default – a Windows user could never
  resolve the console, whatever they set. Now an explicit
  `LANDIS_CONSOLE` wins, the `/opt` search is the fallback, and a
  genuine miss returns `NA_character_` rather than `""`.
- New
  [`landis_version()`](https://for-cast.github.io/landisutils/reference/landis_version.md)
  runs a local console with no scenario file and parses the version
  banner it prints (`LANDIS-II 8.0 (8)`) before exiting on the missing
  scenario, so the expected non-zero status is not treated as failure.
  [`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md)
  now calls it by default (`check_version = TRUE`) and stops when the
  major version is not 8. v7 and v8 differ in the input formats this
  package writes – initial communities moved to a CSV + raster pair,
  ForCS gained the `SpinUp` `BiomassSpinUpFlag` column and the
  map-control block, and core `species.txt` dropped shade and fire
  tolerance – so a v7 console mis-parses v8 inputs rather than failing
  cleanly. A version that cannot be determined (no `dotnet`, unreadable
  console) only warns.
- [`insertAvailableLightBiomass()`](https://for-cast.github.io/landisutils/reference/insertAvailableLightBiomass.md)
  no longer fails on a single-ecoregion landscape. It used
  `apply(df[, -1], 2, ...)`, and with one ecoregion `df[, -1]` collapses
  to a vector, so the call died with “dim(X) must have a positive
  length”. Column-wise assignment with `drop = FALSE` fixes it. The
  emitted header row still carries the ecoregion names only, with the
  shade-class column deliberately unlabelled: LANDIS-II parses that line
  as the ecoregion list, so naming the first column makes it read the
  name as an ecoregion and abort with “Class is not an ecoregion name”.
  Surfaced by single-cell ForCS growth-curve calibration runs.

## landisutils 0.0.83

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  gains `cfg$retries` (default `0`, i.e. unchanged), threaded through
  [`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md)
  into
  [`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md).
  A single failed exec previously aborted the WHOLE search: the error
  propagates through `parApply`, unwinds DEoptim and errors the target,
  discarding every generation since the last checkpoint. A production
  search is `NP x n_reps x itermax` container executions – 90,000 at NP
  = 90, n_reps = 10, itermax = 100 – so even a very rare transient is
  near-certain to hit at least once. Observed twice in ~27 h of running:
  LANDIS-II exited 139 (SIGSEGV) about one second in, immediately after
  `Sites: N active` and before the succession extension loaded, writing
  no managed exception, with the staged trial directory verified
  byte-identical to the template apart from its own patched
  `dynamic-fire.txt`. Retrying costs nothing diagnostically – a real
  input fault (a pixel type the GDAL reader rejects, an unknown map
  code, a memory grant below the landscape’s peak) fails identically on
  every attempt and still surfaces, only later by the duration of the
  retries.

## landisutils 0.0.82

- The docker gate for tests is now a shared `helper-docker.R` rather
  than a copy inside one test file. testthat scopes a test file’s
  top-level definitions to that file, so `.docker_available()` – which
  already knew that Windows CI runners have the docker CLI but are
  configured for Windows containers only, and therefore cannot pull,
  inspect or run a Linux image such as busybox – was invisible to the
  other docker-backed test files. Those files had each rolled a weaker
  `Sys.which("docker")` check, which passes on exactly such a runner and
  then fails for a reason unrelated to the package. All docker tests now
  share the one gate.

## landisutils 0.0.81

- Two more test-only fixes for CI. The `.verify_node_images()` guard
  probed the MAIN process for `busybox:latest` while the function probes
  the cluster WORKERS – in CI the image satisfied the main-process guard
  and the function then reported it missing from the workers, so the
  test errored instead of skipping. It now probes on the workers, where
  the function looks. Separately, a
  [`paste()`](https://rdrr.io/r/base/paste.html)d `info` string was
  referenced before it was assigned (an earlier edit landed the
  reference but not the assignment), which surfaced only on the platform
  where that branch fails.

## landisutils 0.0.80

- Test fixes only, no behaviour change; all three were tests that
  encoded the development machine rather than testing the package. (1)
  The `.verify_node_images()` positive case guarded on `busybox:latest`
  being present, but testthat runs files in PARALLEL here and it is
  another file’s docker test that pulls it – so this test’s coverage was
  a race. It now ENSURES the image (pulling if needed, ~2 MB) and skips
  only if that fails. (2) The failed-sync test made its destination
  unwritable with `chmod a-w`, which does not stop writes on Windows –
  the copy succeeded there and the test asserted the opposite of what
  happened. It now uses a destination whose parent is a regular FILE,
  which cannot be created on any OS, and both backends are confirmed to
  report failure while leaving the source intact. (3) The both-backends
  test passed a LOGICAL as testthat’s `info`, which must be character:
  on Linux both branches passed so it was never evaluated, and it only
  surfaced as an error on Windows where the rsync branch legitimately
  fails. That branch is now skipped on Windows, since rsync’s inability
  to address drive-qualified paths is precisely why the other backend
  exists.

## landisutils 0.0.79

- Output streaming now works on Windows. `.stream_copy()` used `rsync`
  unconditionally, and `rsync` parses `host:path`, so a drive-qualified
  path like `C:/Users/...` reads as the remote host `C` – every sync
  silently moved nothing. This is the same defect
  [`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md)
  was fixed for in 0.0.70, reintroduced in a new code path; Windows now
  uses a direct copy, Unix keeps `rsync --files-from` (one process per
  batch, which matters when many replicates sync concurrently to a
  shared filesystem). The backend is now a `use_rsync` argument rather
  than an inline `.Platform` check, so BOTH paths are reachable from a
  test on either OS – the direct-copy path shipped broken precisely
  because it could not be exercised where the suite runs.
- Test fixes, no behaviour change: the PSOCK cluster test asserted
  exactly two workers, but the per-host caps added in 0.0.76 are RAM-
  and physical-core-aware and legitimately trim to one worker on a small
  CI runner; it now asserts the reported total matches the cluster
  actually built. The `.verify_node_images()` positive case guarded on
  `{{.Id}}` while the function reads `{{index .RepoDigests 0}}`, so an
  image built locally but never pulled satisfied the guard and then
  failed inside the function; the guard now probes exactly what the
  function probes.

## landisutils 0.0.78

- The streaming allow-list no longer excludes `TimeOfLastFire-*` or
  `TOLD-*`. They were held back in 0.0.77 on the suspicion that they
  were simulation state read back on a later timestep; checking the
  LANDIS-II v8 extension sources shows both are pure outputs, so they
  now stream like any other map. Dynamic Fire keeps `TimeOfLastFire` as
  an in-memory `ISiteVar<int>` (`SiteVars.cs`), initialised from
  `StartTime - maxAge` and written through `IOutputRaster<ShortPixel>`
  (`PlugIn.cs`); its only `OpenRaster` calls read topography and the
  fire-regions input map. Root Rot’s `PlugIn.cs` writes exclusively
  through `IOutputRaster<IntPixel>`, and its single `OpenRaster`
  (`SiteVars.cs`) reads a configured input map. (`TimeSince*` never
  applied: those are C# stand-ranking class names in
  Library-Harvest-Mgmt and Base-BDA, not raster outputs.)
- The `stream_exclude` mechanism is retained, and documented against the
  hazard that actually exists: a few extensions read TIMESTEP-TEMPLATED
  input maps at runtime – Land Use Plus resolves
  `MapNames.ReplaceTemplateVars(inputMapTemplate, CurrentTime)` and
  opens it every timestep (`Main.cs`). Such inputs normally live at the
  scenario root, which the directory scoping already excludes; if a
  scenario ever places one inside an output directory, list it in
  `stream_exclude`.

## landisutils 0.0.77

- [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  and
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  can stream completed output maps to durable storage WHILE a replicate
  runs, via `stream_to` (wired automatically by
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  whenever `work_root` puts the run on scratch). Previously a replicate
  was moved only once it finished, so peak scratch was (concurrent
  replicates) x (whole replicate). That does not scale to production
  length: a 1200-year run writing several rasters per timestep is tens
  of GB, and replicate concurrency is sized against RAM rather than
  disk, so a large enough run exhausts scratch hours in – after most of
  the compute has been paid for. Streaming leaves only the working set
  on scratch.
- Nothing is discarded: files are MOVED, not pruned. Outputs no
  downstream target reads today are still archived, because re-running
  1200 years to recover them costs far more than storing them.
- Streaming writes into the same `<final>.partial` staging directory
  that
  [`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md)
  publishes by atomic rename, so the all-or-nothing guarantee is
  unchanged – a partially streamed replicate can never be mistaken for a
  finished one by a skip-check reading the final directory.
- The allow-list is scoped by output DIRECTORY (`fire/`, `ForCS/`,
  `NECN/`, `harvest/`, `bda/`, `eda/`, `rootrot/`, `output/`,
  `outputs/`, wind, hurricane, …) rather than by map name, so it covers
  every extension instead of only the ones one project happens to run –
  LANDIS-II output maps share a `<dir>/<name>-{timestep}.<ext>`
  convention, so enumerating names would mean chasing each new
  extension. Directory scoping is also the safer shape: a bare
  `-{timestep}.tif` pattern would match `landuse-{timestep}.tif`, which
  Land Use Plus READS as input from the scenario root, so root-level
  inputs are now excluded structurally rather than by memory.
- Safety: only write-once output maps matching an allow-list are moved,
  and only for timesteps at or below `current - stream_lag_steps`
  (default 2), since LANDIS-II writes several rasters per timestep and a
  file for step `t` can still be open when the log already reports
  `Current time: t`. Local copies are removed only after `rsync` reports
  success, so a storage failure costs a retry rather than the outputs.
  `TimeOfLastFire-*` is deliberately excluded: it may be simulation
  state rather than pure output, and that has not been verified against
  the extension source. Intervals are jittered (`stream_jitter_frac`,
  default 0.25) so replicates launched together do not sync in lockstep
  and burst against one shared filesystem.

## landisutils 0.0.76

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  now caps each host’s worker count by its PHYSICAL core count as well
  as its RAM, taking whichever binds. RAM alone is not sufficient on a
  heterogeneous cluster: two hosts can carry the same ~1 TB of memory
  while differing 2.7x in cores (128 vs 48 physical, measured here), so
  a RAM-only cap admitted 45 containers to both and booked the smaller
  host to ~94% of its cores while the larger sat at ~35%. Because
  DEoptim waits for every population member, the entire generation then
  ran at the saturated host’s pace – 68 min per replicate there against
  40 min on the roomy one, throwing away roughly a third of the
  multi-node gain. Physical cores are counted (distinct
  `physical id`/`core id` pairs), not logical: SMT siblings share
  execution units, so counting threads overstates capacity 2x. Tunable
  via `cfg$cores_per_worker` (default 1, since LANDIS-II is effectively
  single-threaded) and `cfg$cpu_fraction` (default 0.85). Note the cap
  prevents over-subscription but does not redistribute: to balance a
  heterogeneous fleet, set `cfg$nodes` in proportion to each host’s
  cores.

## landisutils 0.0.75

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  now folds a content digest of the scenario template into the
  evaluation fingerprint that keys the memoization cache and the resume
  checkpoint. The template was previously absent from that fingerprint
  entirely, and `cfg$sim_years` did not stand in for it:
  [`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md)
  documents the run Duration as coming from the template’s
  `scenario.txt`, so `cfg$sim_years` is informational. A template
  rebuilt at a different Duration therefore hashed IDENTICALLY to the
  old one, and a reused `out_dir` served the previous Duration’s losses
  straight back out of the cache. Observed on a 90-member population
  where 88 members returned instantly from evaluations run at a third of
  the intended Duration – silent, and it would have produced a
  calibrated parameter set fitted almost entirely to the wrong
  simulation length. Initial communities, ecoregions, the weather
  database, the fuel and fire tables and the landscape extent were
  equally invisible; all of them now invalidate stale state. The digest
  hashes file contents rather than mtimes, so rebuilding a template that
  is byte-identical still hits the cache.

## landisutils 0.0.74

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  now launches the workers belonging to the coordinator’s OWN machine as
  `localhost`, without SSH. The coordinator normally runs on one of the
  fleet hosts, so the fleet almost always includes the local machine;
  naming it explicitly made `parallelly` open an SSH connection from a
  host to itself, which a machine’s own name need not accept (it can
  resolve to a loopback alias with no listening sshd). The result was a
  cluster setup that stalled indefinitely with no diagnostic and no
  child processes to inspect. Localised workers are also cheaper, since
  they skip SSH entirely.

## landisutils 0.0.73

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md):
  the multi-node image check now quotes the
  `docker image inspect --format` argument.
  [`system2()`](https://rdrr.io/r/base/system2.html) pastes its
  arguments together WITHOUT quoting, so `{{index .RepoDigests 0}}` was
  split into three shell words and docker exited 64 (usage error).
  Because a usage error and a genuinely absent image are both non-zero,
  the guard added in 0.0.72 reported “image is missing” for EVERY host
  and would have blocked every multi-node run. The accompanying test
  only covered the missing-image path, which fails identically either
  way; it now also asserts the positive case, so a malformed command
  cannot pass again.

## landisutils 0.0.72

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  verifies, before starting any container, that every host in
  `cfg$nodes` has the configured Docker image AND that they all resolve
  to the SAME digest. Pools start with `pull = FALSE`, so a host missing
  the image previously failed late and only on that host; worse, hosts
  holding different digests behind a mutable tag (such as
  `:ubuntu-24.04`) would not error at all – the search would simply
  evaluate some trials against one LANDIS-II build and some against
  another, mixing them into one loss surface. That is a reproducibility
  failure, and invisible in the output, which is why it is now a hard
  error naming the offending hosts.

## landisutils 0.0.71

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  gains `cfg$nodes` (a named vector like `c(host1 = 30, host2 = 30)`),
  which spreads the DEoptim search across machines. Unset – the default
  – leaves the single-node FORK path completely unchanged. This matters
  because per-node throughput is memory-bandwidth-bound, not CPU-bound:
  on a dual-socket EPYC 7702 growing the warm pool from 22 to 90
  containers bought only ~1.25x throughput while per-rep wall-clock grew
  ~5x, so a single host saturates long before its cores are busy. Since
  DEoptim dispatches exactly `NP` tasks per generation, spreading those
  same `NP` workers over more hosts both adds hosts AND returns each one
  to its unsaturated regime, so the gain exceeds the host count.
- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  gives every multi-node worker its OWN single-container pool on its OWN
  host, rather than federating one shared pool.
  [`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)
  already resolves `pool$names[idx]` against the LOCAL docker daemon, so
  a worker only ever needs a pool describing containers on the machine
  it is running on – there is no cross-host container index to map, and
  [`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)
  is unchanged. Worker pools are torn down through the cluster before it
  is stopped, since only the worker knows its own container’s name.
- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  caps each host’s worker count against THAT host’s available RAM (hosts
  in a cluster differ, and a coordinator-wide figure over-subscribes the
  smaller ones), then trims the fleet to `NP`: workers beyond `NP` never
  receive a task while still holding a container and its RAM.
- [`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md)
  and the calibration trace: per-worker trial-trace sidecars are now
  keyed by host AND pid (`worker_<host>_<pid>.csv`), not pid alone. The
  directory lives on shared storage while pids are unique per host only,
  so once workers span nodes two of them could collide and append
  interleaved rows to a single file, corrupting both the trace and the
  memoization cache built from it. The eval-cache scan matches both the
  new and legacy names, so a resumed run still folds in sidecars written
  by an older version.

## landisutils 0.0.70

- [`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md)
  now writes the remapped map-code raster with the smallest pixel type
  LANDIS-II accepts that holds the largest new code – `INT1U` (GDAL
  `Byte`), `INT2S` (`Int16`) or `INT4S` (`Int32`) – instead of `INT4U`.
  LANDIS-II opens map rasters through
  `Landis.RasterIO.Gdal.GdalInputRaster.NewInputBand`, which accepts
  only Byte / Int16 / Int32 / Float32 / Float64 and aborts the run with
  “Raster band is not byte, short, int, float, double” on anything else.
  Map codes are positive by construction, so an unsigned type is the
  natural choice and is exactly the wrong one – `UInt16` and `UInt32`
  are both rejected, as is `Int8`: every deduplicated snapshot produced
  by 0.0.68 and 0.0.69 is unreadable, failing in Biomass Succession’s
  `InitializeSites()` seconds after startup. Biomass Succession itself
  writes the snapshot as Int32, so the pair now round-trips through the
  type LANDIS-II already emits. The chosen type is returned as
  `datatype`.
- [`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md)
  now stages its copy with
  [`fs::dir_copy()`](https://fs.r-lib.org/reference/copy.html) on
  Windows instead of `rsync`. `rsync` parses `host:path`, so a
  drive-qualified path such as `C:/Users/...` reads as the remote host
  `C`, and with both source and destination drive-qualified it refuses
  to run at all: `The source and destination cannot both be remote`
  (exit 1). This failed the Windows leg of R-CMD-check while Linux and
  macOS passed. The resumable, fault-tolerant transfer `rsync` provides
  is a property of the Linux/macOS scratch-to-NFS deployment the
  function was written for, so the substitution costs Windows nothing it
  was using. Relatedly, the retry loop no longer retries exit status 1 –
  `rsync`’s “syntax or usage error” is deterministic, so retrying only
  burned the backoff (50 seconds at the defaults) before failing with
  the same message, and the reported attempt count is now the number
  actually made.
- [`landis_datatype()`](https://for-cast.github.io/landisutils/reference/landis_datatype.md)
  (new, exported) returns the smallest raster pixel type LANDIS-II can
  actually read for a given maximum value. LANDIS-II opens maps through
  a GDAL wrapper that accepts only `Byte`, `Int16`, `Int32`, `Float32`
  and `Float64`, so the unsigned integer types are unusable even though
  map codes are positive by construction – and nothing catches a bad
  choice at write time, since the model only rejects the band when the
  extension initialises.
  [`prepInitialCommunities()`](https://for-cast.github.io/landisutils/reference/prepInitialCommunities.md),
  [`prepInitialFireRegionsMap()`](https://for-cast.github.io/landisutils/reference/prepInitialFireRegionsMap.md)
  and `prepEcoregions()` now derive the type from the data through this
  helper instead of hard-coding `INT2S`, which silently capped map codes
  at 32767. A static test scans the package sources so a future
  `writeRaster()` call with a rejected type fails the suite rather than
  a simulation.

## landisutils 0.0.69

- [`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md)
  now collapses duplicate communities in the snapshot it produces, via
  [`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md)
  (new `dedup` argument, default `TRUE`). The spinup’s whole purpose is
  to hand a spun-up initial-communities pair to the calibration trials,
  and the raw pair is exactly the thing LANDIS-II cannot read back on a
  large landscape – so deduplicating at the point of production means
  every consumer, and every archived copy, gets the small form. Set
  `dedup = FALSE` to inspect the raw writer output.

## landisutils 0.0.68

- [`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md)
  (new, exported) collapses duplicate communities in an Output Biomass
  Community snapshot and remaps the map-code raster to match. Biomass
  Succession writes its state with one map code per PIXEL and never
  re-collapses cells whose cohort lists ended up identical, so a large
  landscape’s snapshot is overwhelmingly duplicate rows. That matters
  because LANDIS-II reads initial communities back through a parser that
  builds a `System.Dynamic.ExpandoObject` per row, at a memory cost many
  times the file size: a big enough snapshot exhausts the container’s
  `--memory` and aborts with `System.OutOfMemoryException` inside
  `ReadCSVInputFile` before the simulation starts. Measured on a
  2.98M-active-cell landscape: 2,684,154 map codes carrying only 4,153
  distinct communities, a 1,472 MB CSV. Communities are compared exactly
  on all cohort columns after canonical ordering, so cohort ORDER does
  not prevent collapsing and no two cells that genuinely differ are ever
  merged; every pixel still resolves to precisely the cohort list it had
  before. The rewritten pair is verified before writing – every map code
  left in the raster must exist in the CSV, or the call errors rather
  than emitting a pair LANDIS-II would reject with “Unknown map code”.

## landisutils 0.0.67

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  no longer over-subscribes host RAM when sizing the warm Docker pool.
  The cap divided the RAM budget by `cfg$mem_per_worker_gb` (the
  *estimate*), while each container was granted `--memory` of 1.25x that
  estimate as headroom – so the admitted pool could exceed physical RAM
  by 25%. On a 1007 GiB node with a 2.98M-cell landscape the cap
  admitted 27 containers at a 38 GiB grant each: 1026 GiB. The cap now
  divides by the granted per-container limit, so the invariant
  `n_containers * mem_limit <= mem_fraction * available` holds by
  construction (22 containers, 836 GiB, in that case). The same value is
  passed to
  [`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
  so the figure the pool is capped against and the figure each container
  is granted cannot drift apart.

## landisutils 0.0.66

- [`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md)
  now runs its `n_reps` goodness-of-fit replicates in parallel across
  the warm Docker pool
  ([`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html),
  one FORK child per replicate) instead of a serial
  [`lapply()`](https://rdrr.io/r/base/lapply.html).
  [`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md)
  already allocates one container per replicate, so the previous serial
  loop left all but one container idle and made validation roughly
  `n_reps` times slower than the pool was sized for; wall-clock now
  tracks a single replicate rather than their sum.
  [`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md)
  is FORK-safe (paths only), and a failed replicate now surfaces as an
  error instead of a malformed `reps` list reaching
  [`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md).

## landisutils 0.0.65

- `ForestRoadsSimulation$write()` now registers its two required input
  rasters (`RasterOfBuildableZones`, `InitialRoadNetworkMap`) via
  `add_file()`, so they are appended to `$files` and picked up by
  scenario collection / replicate staging (mirroring `DynamicFire` and
  `BiomassHarvest`). Previously only the config file was tracked, so the
  input rasters were silently omitted when a scenario was staged into
  per-replicate run directories.

## landisutils 0.0.64

- `BiomassHarvest`: fix the `EventLog` / `SummaryLog` default paths. The
  bare relative-string defaults (`"biomass-harvest/log.csv"`) were
  mangled by the `.relPath()` active binding whenever the extension
  `path` was deep or absolute (e.g. an absolute scratch dir), producing
  broken `../../` paths in the written config. They now default to
  `NULL` and resolve to full scenario paths in `initialize()` (mirroring
  `PrescriptionMaps`), so `.relPath()` renders them as scenario-relative
  `biomass-harvest/log.csv` again. `path = "."` is unchanged.

## landisutils 0.0.63

- [`read_forcs_log_summary()`](https://for-cast.github.io/landisutils/reference/read_forcs_log_summary.md),
  [`write_forcs_log_summary_parquet()`](https://for-cast.github.io/landisutils/reference/write_forcs_log_summary_parquet.md),
  and
  [`open_forcs_log_summary_dataset()`](https://for-cast.github.io/landisutils/reference/open_forcs_log_summary_dataset.md)
  process ForCS `log_Summary.csv` ecosystem-carbon outputs: read one
  scenario’s replicates into a tibble (masking to core cells), write one
  replicate to a partitioned parquet at
  `<scenario>/_aggregates/forcs_log_summary/replicate=<rep>/part-0.parquet`
  (atomic publish via a temporary +
  [`fs::file_move()`](https://fs.r-lib.org/reference/file_move.html),
  optional `staging_dir` for per-host scratch), and open one or more
  per-scenario roots as one lazy Arrow dataset (single root, or a
  `UnionDataset` across scenarios). Consolidated from the FOR-CAST
  project-side post-processing pipelines.

## landisutils 0.0.62

- [`write_biomass_c_snapshots_parquet()`](https://for-cast.github.io/landisutils/reference/write_biomass_c_snapshots_parquet.md)
  writes one replicate’s `log_BiomassC.csv` (read via
  [`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md))
  to a partitioned parquet at
  `<scenario>/_aggregates/biomass_snapshots/replicate=<rep>/part-0.parquet`,
  the layout
  [`read_biomass_c_snapshots_for_scenario()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots_for_scenario.md)
  reads back. The publish is atomic (write to a temporary then
  [`fs::file_move()`](https://fs.r-lib.org/reference/file_move.html)
  into place) so many replicate writers can run concurrently against an
  NFS output directory without a reader seeing a partial file; an
  optional `staging_dir` keeps the interim bytes on per-host scratch.
  Consolidated from the FOR-CAST project-side post-processing pipelines.

## landisutils 0.0.61

- [`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md)
  and
  [`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md)
  rewritten from per-group [`{}`](https://rdrr.io/r/base/Paren.html) R
  evaluation to vectorised `data.table` pipelines:
  `sum(biomass) by = ...` totals, `setorder()` + `rowid()` per-cell
  rank, `dcast()` species pivot, and vectorised
  [`paste()`](https://rdrr.io/r/base/paste.html) +
  [`gsub()`](https://rdrr.io/r/base/grep.html) for the “-”-joined label.
  Correctness is identical to the previous implementations on the
  FOR-CAST validation fixtures; runtime drops from ~90 min to ~5 s on an
  8M-row single-scenario input (~1000x speedup). The previous per-group
  `.SD` idiom scaled O(n_cells) in R-level evaluations; the new pipeline
  stays linear but at C-level cost, so multi-rep multi-scenario
  post-processing aggregators no longer bottleneck the pipeline.
- New
  [`read_biomass_c_snapshots_for_scenario()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots_for_scenario.md)
  helper: opens `<scenario_dir>/_aggregates/biomass_snapshots` as an
  Arrow dataset partitioned by `replicate`, collects the full contents,
  and returns `NULL` on missing / empty datasets. Lives in the package
  (rather than as a project-side helper) so
  [targets](https://docs.ropensci.org/targets/) treats it as an
  installed-package function and does not invalidate consuming targets
  on future cosmetic refactors of the collect logic.

## landisutils 0.0.60

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  checkpoint/resume (0.0.59) now scopes both the resume and the
  memoization cache to a *loss-config fingerprint* – a hash of the
  weights, per-trial sim settings (`n_reps`, `sim_years`, `base_seed`,
  `simulator`, `method`, Docker `image`), and the observed targets – in
  addition to the population fingerprint. Previously, reusing a single
  `out_dir` (e.g. the persistent `outputs/calibration/`) across
  calibrations that changed the weights or observations could silently
  seed the cache with stale loss values and resume a checkpoint whose
  carried best-so-far was computed under the old config, poisoning the
  new run’s objective. Now a config change self-invalidates the
  checkpoint + cache instead of resuming stale state, so `out_dir` need
  not be cleared by hand. `cfg$resume = "never"` additionally starts
  from an empty cache (a true clean slate; it previously still folded in
  prior-run trial-trace rows). The trial-trace CSVs gain an `eval_fp`
  column recording the fingerprint. No behaviour change when
  `cfg$checkpoint_every` is unset.

## landisutils 0.0.59

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  gains opt-in checkpoint/resume: set `cfg$checkpoint_every = K` to run
  the DEoptim search in blocks of K generations, persisting the
  population and best-so-far to `out_dir` (`checkpoint.rds`,
  `best_params_so_far.rds`) between blocks so an interrupted run (crash,
  node reboot, kill) resumes instead of restarting from generation 1.
  Previously evaluated points are memoized via the trial-trace CSVs so
  resumes and per-block initial-population re-evaluations skip their
  (expensive) simulator runs. Resume is fingerprint-guarded (parameter
  names + bounds + NP) and controlled by `cfg$resume` (`"auto"` default,
  `"never"`, `"force"`); when `cfg$checkpoint_every` is unset the
  behaviour is unchanged (a single monolithic `DEoptim()` call). The
  trial-trace CSVs now record the `par_*` and `total` columns at full
  round-trippable precision.

## landisutils 0.0.58

- [`plot_species_growth_curves()`](https://for-cast.github.io/landisutils/reference/plot_species_growth_curves.md)
  is a new Biomass Succession calibration diagnostic: it overlays the
  fitted LANDIS-version growth curve (`BscaledNonLinear`) on the PSP
  biomass observations behind it, faceted by species and coloured by
  ecoregion (the `speciesGrowthCurvesLandis` / `speciesGrowthCurvesPSP`
  outputs of `Biomass_speciesParameters` run in LANDIS mode).

## landisutils 0.0.57

- Bug fix:
  [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md)
  previously used “polygons IF supplied, else points” to pick
  `fire_sizes_ha` – the moment any NBAC polygons were passed in, ALL
  NFDB sizes were silently dropped. That excluded pre-1972 fires (NBAC’s
  coverage starts in 1972) and small fires that NFDB recorded but NBAC’s
  MAFM pipeline does not map (Landsat 30 m detection floor), biasing the
  observed size distribution toward larger fires and making the `L_size`
  KS comparison apples-to-oranges. The function now starts from NFDB’s
  `SIZE_HA` and swaps in NBAC’s `SIZE_HA` only where an NBAC polygon in
  the SAME calendar year contains the NFDB ignition point
  (point-in-polygon via
  [`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)).
  Year-mismatched polygons are explicitly skipped. One new regression
  test covers the four cases: (1) point inside same-year polygon -\>
  NBAC swap; (2) point with no polygon -\> NFDB kept; (3) point inside a
  polygon’s bbox but year-mismatched -\> NFDB kept; (4) pre-NBAC point
  -\> NFDB kept. Existing payload-shape test updated to match the new
  semantics.

## landisutils 0.0.56

- Bug fix:
  [`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md)
  (and `LandisScenario$replicate()`) guarded the input-file copy block
  with `if (!fs::dir_exists(rep_dir))`, so a rep dir left behind by a
  failed run would silently keep its stale input files on the next call
  – no re-stage, no re-seed. LANDIS-II then ran against an old
  `dynamic-fire.txt` / `scenario.txt` even though `tar_make` had rebuilt
  those files in the scenario template, surfacing as fast LANDIS-II
  parse errors that pointed to “fixed” config bugs. The guard is
  removed; the rep dir is always re-created and input files are always
  overwritten with `overwrite = TRUE`. LANDIS-II output artefacts
  (`Landis-log.txt`, `log_*.csv`, etc.) survive across re-stages because
  they’re not in `src_files`. Two regression tests: (1) re-staging an
  existing rep dir picks up updated input content; (2) output artefacts
  in the rep dir are preserved across a re-stage.

- Failure-side scratch cleanup: the
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  run-and-archive wrapper now also calls
  [`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md)
  from the `tryCatch` error handler around the LANDIS run, so a failed
  rep’s partial outputs (`Landis-log.txt`, `docker_stdout.log`,
  `docker_stderr.log`, per-extension log subdirs) are rsync’d to NFS for
  inspection and the scratch rep dir is then deleted. Previously failed
  reps lingered on scratch indefinitely – a real problem on shared
  workers where four projects’ calibrations + sims share `/mnt/scratch`.
  The original LANDIS error is re-thrown so `tar_make` still marks the
  target as errored; if the failure-archive itself errors (rsync missing
  / NFS unmounted), the scratch dir is left intact and a warning is
  emitted. The success path is unchanged:
  [`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md)
  rsyncs scratch -\> NFS then deletes the scratch source (the existing
  test “landis_archive_rep() moves a completed rep and deletes the
  scratch source” covers both paths since the helper doesn’t care
  whether the rep completed).

  Hard-kill caveat: SIGKILL of the R worker (or `screen quit` leaving an
  orphan that gets `kill -9`’d) skips R-side cleanup entirely; the
  docker container is still removed by `processx`’s child-process
  cleanup + `--rm`, but the scratch rep dir survives. That path is rare
  and is the price of robust failure handling – a manual `rm -rf` on the
  affected scratch path is the recovery.

## landisutils 0.0.55

- Bug fix: `.preflight_calibrate()`’s `cfg$weights` whitelist (added in
  v0.0.45) was never updated when v0.0.51 added the `size_tail` loss
  component, so any `cfg$weights` entry named `size_tail` was silently
  stripped with the warning
  `cfg$weights has unrecognized components (ignored): size_tail` and
  DEoptim ran with the tail term effectively disabled. The gitanyow
  re-calibration (9h 13m, 2026-06-25, after the v0.0.54 cell-gate fix)
  hit this and reported the warning but landed a calibrated parameter
  set with no upper-tail signal. Whitelist now includes `size_tail`, and
  the two stale `cfg$weights %||%` defaults elsewhere in
  `calibrate_dynamic_fire.R` also include `size_tail`. Two new
  regression tests: (1) `size_tail` in weights passes preflight without
  warning; (2) genuinely-unknown weight names still warn.

## landisutils 0.0.54

- Bug fix: `.chi_sq_area_by_fuel()`’s gate for the cell-based
  attribution path (`all(has_cell_attr)`, added in v0.0.52) was
  strict-NULL – it treated a zero-event rep (correctly returning
  `area_by_fuel_ha = NULL` because
  [`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
  finds no severity tifs to integrate) as “this rep lacks cell-based
  output” and fell back to the legacy event-`InitFuel` attribution for
  the whole trial. In low-fire-rate calibrations (the gitanyow FRU59
  case: ~1 event per rep-year, so most trials had at least one
  zero-event rep) the cell-based path NEVER engaged in practice – the
  loss surface was dominated by the legacy-attribution chi-sq the
  cell-based path was added to replace. Worse, the gate was stochastic
  across DEoptim trials: trials where every rep happened to fire
  returned a tiny chi-sq via cell-based; trials with any zero-event rep
  jumped 100x to the legacy path – so DEoptim ratcheted on a gate flip,
  not on real parameter response. A real run on gitanyow (8h 29m
  DEoptim, 2026-06-24) ended with `area_fuel = 29.74` dominating a total
  loss of 34.5, with the cell-based recompute giving only `0.0095`. New
  gate: a rep is cell-capable if `area_by_fuel_ha` is populated OR
  `nrow(events) == 0L`; zero-event reps are dropped before binding (they
  contribute nothing to `sim_area_by_base`), and the legacy fallback is
  reserved for reps that HAVE events but are missing the cell-based
  summary (mock simulator, Dynamic Fuels disabled, payloads from \<
  0.0.52). Two new regression tests lock the distinction in.

## landisutils 0.0.53

- Bug fix:
  [`.read_burned_area_by_fuel()`](https://for-cast.github.io/landisutils/reference/dot-read_burned_area_by_fuel.md)
  (consumed by
  [`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
  and `.chi_sq_area_by_fuel()`) used `severity > 0` to identify burned
  cells, but the Dynamic Fire severity raster encodes `0` = inactive,
  `1` = active-but-unburned, `>= 2` = burned (value is the severity
  class). `> 0` therefore selected every active cell in the landscape,
  attributing the whole-landscape fuel composition (not the burn) to
  `sim$area_by_fuel_ha`. The bug silently mis-trained the `L_area_fuel`
  calibration component for the entire v0.0.52 lifespan and mis-rendered
  any `fig-area-by-fuel` panels that consumed the per-rep field.
  Switched to `severity > 1` (matches the consuming projects’ own
  readers); docstrings and the regression test (with `severity == 1`
  cells in the fixture) make the distinction explicit.

## landisutils 0.0.52

- [`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
  now also returns `area_by_fuel_ha` – a per-rep tibble of `fuel_code`,
  `cells`, `area_ha` – computed by intersecting the per-timestep Dynamic
  Fire `severity-{t}.tif` rasters (cells with severity \> 0 = burned)
  with the matching Dynamic Fuels `FuelType-{t}.tif` rasters in
  `<rep_dir>/fire/`. Cell-fuel-timestep is the unit of accounting, so a
  cell that burns in two timesteps contributes twice (matches NBAC’s
  per-perimeter accounting). Returns NULL when the severity/FuelType
  pairs aren’t on disk (mock simulators, missing Dynamic Fuels, etc.)
  and `.chi_sq_area_by_fuel()` then falls back to the legacy event-based
  attribution.

- `.chi_sq_area_by_fuel()` (consumed by
  [`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md)’s
  `L_area_fuel` component) now prefers the per-rep `area_by_fuel_ha`
  cell-based attribution when every rep has it, and falls back to the
  legacy event-based attribution (each event’s `DamagedSites` counted
  toward its `InitFuel` only) when any rep is missing it. The legacy
  attribution biased simulated burn area toward dominant-cover fuel
  because fires ignite where there’s igniteable fuel and then spread
  anywhere – a structural attribution mismatch with the observed side
  (which attributes by burned-cell fuel). DEoptim runs with all reps
  using LANDIS-II output will now train against a directly comparable
  target. Also adds Laplace smoothing to obs_p (same `alpha = 0.01`
  default as the severity chi-sq, override via
  `getOption("landisutils.calibration.area_fuel_smoothing_alpha", 0.01)`)
  to bound chi-sq contributions from empty observed bins.

## landisutils 0.0.51

- [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md)
  gains a `min_size_ha` argument (default `1.0`) that truncates
  `fire_sizes_ha` at a lower bound and persists the value in the output
  payload. NFDB / NBAC are effectively left-censored at ~1 ha (small
  fires under-reported); without a floor, the KS comparison in
  [`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md)
  compared the sim’s full size distribution against a truncated observed
  sample. Set `min_size_ha = 0` to restore prior behaviour.

- [`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md)
  reads `observed$min_size_ha` and applies the same lower truncation to
  pooled `sim_sizes` so the KS statistic compares like-with-like.

- [`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md)
  now subsamples `sim_sizes` down to `length(obs_sizes)` (deterministic
  seed; override via
  `getOption("landisutils.calibration.subsample_seed", 12345L)`) before
  the KS test whenever `length(sim_sizes) >= 2 * length(obs_sizes)`.
  Removes the “more samples -\> better-resolved tail -\> bigger KS gap”
  artifact that was driving DEoptim toward body-matching at the expense
  of tail fidelity.

- [`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md)
  adds a new loss component,
  `size_tail = |log10(sim_q95) - log10(obs_q95)|` (default weight `1`),
  explicitly checking the upper-tail magnitude. KS distance routinely
  under-weights the tail on log-scaled data; `size_tail` gives DEoptim a
  separate gradient to close the q95 gap. Components vector is now
  `c(count, size, size_tail, area_fuel, severity)` (a new entry;
  downstream code that introspects `components` names needs updating).

- `.chi_sq_severity()` switched from `pmax(obs_p, 1e-6)` to
  Laplace-smoothed denominators (`alpha = 0.01` per bin by default;
  override via
  `getOption("landisutils.calibration.severity_smoothing_alpha", 0.01)`).
  The previous regulariser gave any empty observed severity class a ~1e6
  multiplier on its chi-sq contribution, so even 1% of simulated mass in
  an empty observed bin dominated every other component of the loss;
  smoothing caps the worst-case contribution at a meaningful magnitude
  without otherwise distorting the comparison.

## landisutils 0.0.50

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  now caps the Docker warm-pool size at the host RAM budget so a
  district-scale calibration landscape no longer OOMs the node by
  starting more containers than fit in memory: each container holds a
  full LANDIS landscape in memory, so per-container RAM scales with the
  cell count. The new `mem_per_worker_gb` config field is the
  per-container estimate (the per-container `--memory` limit is derived
  from it, +25% headroom, when not set explicitly), and `mem_fraction`
  (default 0.85) is the fraction of available RAM the pool may use;
  configs that set neither field behave exactly as before (the cap falls
  back to the `mem_limit` value, so small-area calibrations are
  unaffected).

## landisutils 0.0.49

- New
  [`georef_landis_raster()`](https://for-cast.github.io/landisutils/reference/georef_landis_raster.md)
  attaches a template raster’s CRS and extent to a
  spatially-reference-less LANDIS-II GeoTIFF; de-duplicated from the
  BC_HRV and gitanyow-partial-harvest Phase-6 output-reading templates.
- New
  [`landis_image_info()`](https://for-cast.github.io/landisutils/reference/landis_image_info.md)
  reads the LANDIS-II Docker image reference (and `sha256` digest, when
  captured) used for a run; de-duplicated from the BC_HRV and
  gitanyow-partial-harvest report-pipeline templates.
- New
  [`parse_landis_log_versions()`](https://for-cast.github.io/landisutils/reference/parse_landis_log_versions.md)
  parses a `Landis-log.txt` for the console version, seed, and extension
  version blocks; de-duplicated from the BC_HRV and
  gitanyow-partial-harvest report-pipeline templates.
- New
  [`prov_landis_container()`](https://for-cast.github.io/landisutils/reference/prov_landis_container.md)
  formats the LANDIS-II runtime image and digest as a provenance
  `data.frame`; de-duplicated from the gitanyow-partial-harvest
  report-pipeline template.
- New
  [`prov_landis_versions()`](https://for-cast.github.io/landisutils/reference/prov_landis_versions.md)
  formats the parsed LANDIS-II console and extension versions as a
  provenance `data.frame`; de-duplicated from the
  gitanyow-partial-harvest report-pipeline template.
- New
  [`prov_run_resources()`](https://for-cast.github.io/landisutils/reference/prov_run_resources.md)
  summarises per-replicate elapsed time and peak memory (mean +/- SD) as
  a provenance `data.frame`; de-duplicated from the
  gitanyow-partial-harvest report-pipeline template.
- New
  [`prov_stochasticity()`](https://for-cast.github.io/landisutils/reference/prov_stochasticity.md)
  formats the base seed and replicate count as a provenance
  `data.frame`; de-duplicated from the gitanyow-partial-harvest
  report-pipeline template.
- New
  [`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md)
  re-simulates Dynamic Fire at the calibrated parameter vector to
  recover per-replicate goodness-of-fit statistics; de-duplicated from
  the BC_HRV and gitanyow-partial-harvest report-pipeline templates.

## landisutils 0.0.47

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  now resolves the LANDIS-II input files its pre-flight check requires
  the scenario template to contain – the succession config, the species
  file, and the Dynamic Fire inputs – from the template itself (new
  internal `.calibration_required_files()`, reusing
  [`.calibration_succession_backend()`](https://for-cast.github.io/landisutils/reference/dot-calibration_succession_backend.md)
  / `.calibration_species_file()` / `.calibration_directive_file()`)
  instead of asserting one project’s fixed filenames. Templates that
  [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md)
  writes for a Biomass Succession scenario
  (e.g. `biomass-succession.txt`, `species-core.txt`,
  `initial-weather-database.csv`, `dynamic-fire-species.csv`) previously
  failed this check with “is missing required files:
  forc-succession.txt, species.txt, …”; the legacy ForC Succession names
  remain the fallback defaults, so existing scenarios are unaffected.

## landisutils 0.0.46

- [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md)
  now resolves the Dynamic Fire input filenames
  (`InitialWeatherDatabase`, `Species_CSV_File`,
  `InitialFireEcoregionsMap`, `GroundSlopeFile`,
  `UphillSlopeAzimuthMap`) from the template scenario’s
  `dynamic-fire.txt` directives, via the new internal
  `.calibration_directive_file()` (which also now backs
  `.calibration_species_file()`), instead of assuming fixed names.
  Scenarios that name the weather DB `initial-weather-database.csv` and
  the Dynamic Fire species table `dynamic-fire-species.csv` (rather than
  the assumed `initial_weather_database.csv` and
  `DynamicFire_Spp_Table.csv`) previously aborted the calibration
  scenario reconstruction with a `file.exists(...)` failure in
  `add_file()`; scenarios using the previous names are unaffected (those
  remain the fallback defaults).

## landisutils 0.0.45

- [`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md)
  and
  [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md)
  now resolve the species-definitions file from the template scenario’s
  `Species` directive (new internal `.calibration_species_file()`)
  instead of assuming the name `species.txt`. Scenarios that name it
  `species-core.txt` (rather than `species.txt`) previously aborted the
  calibration spinup with `fs::file_exists(species_file) is not TRUE`;
  scenarios that use `species.txt` are unaffected (it remains the
  fallback).

## landisutils 0.0.44

- [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md)
  now derives `fire_sizes_ha` from the polygons’ `SIZE_HA` attribute
  when polygons are supplied and non-empty, falling back to the points’
  `SIZE_HA` otherwise. The previous behaviour always read sizes from
  points (NFDB agency-reported sizes), which meant that callers passing
  higher-quality perimeter polygons (e.g. NBAC’s ADJ_HA) for
  `primary_polys`/`secondary_polys` only got the better data into
  `area_by_fuel_ha`; the size distribution still came from NFDB. Now
  NBAC perimeters drive the size CDF too. Callers that pass NFDB
  polygons see no behaviour change because NFDB polys carry the same
  `SIZE_HA` field. Callers without polys keep the old points-driven
  behaviour.
- [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md)
  `primary_polys` argument is now optional (was required). When `NULL`,
  the function falls back to points-driven sizes and skips the
  `area_by_fuel_ha` computation. Matches the existing optional treatment
  of `secondary_polys`. Existing callers passing a `SpatVector` are
  unaffected.

## landisutils 0.0.43

- [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  gains a `startup_jitter` argument (and reads the
  `LANDIS_STARTUP_JITTER` environment variable when it is `NULL`): when
  set, each call sleeps a random `runif(0, startup_jitter)` seconds
  before it first touches Docker, staggering container launches so a
  large `crew` fleet does not overwhelm the Docker daemon (which stops
  answering `docker stats` and returns exit 1 under the surge) or hammer
  the disk backing the image layers and renv library when dozens of
  replicates start at once. Because the delay cannot change results,
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  does not bake it into the
  [targets](https://docs.ropensci.org/targets/) command, so tuning it
  never invalidates completed replicates.

## landisutils 0.0.42

- [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
  [`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
  and the
  [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  config default for `cpu_limit` change from `4` to `2`. Empirical
  measurement across 90 concurrent ForCS + Dynamic Fire + Dynamic Fuels
  containers under live calibration shows the LANDIS-II console process
  is effectively single-threaded (median 1.00 cores, p99 1.11 cores, max
  1.11 cores) – the prior default of `4` overprovisioned by ~4x. The new
  default of `2` covers the 99th-percentile .NET-GC / threadpool burst
  with comfortable headroom while letting users pack more containers
  into the same nominal CPU budget; `1` would be tight enough to risk
  contention between the simulator thread and the .NET GC helper.

## landisutils 0.0.41

- Generated LANDIS-II input files no longer embed a timestamp in their
  `>> generated by landisutils` header (the package version is kept).
  The header is now byte-reproducible, so re-writing an unchanged
  scenario produces identical files and
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)’s
  input-hash skip-check stays stable across Phase-3 rebuilds instead of
  needlessly re-running every replicate.

## landisutils 0.0.40

- [`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md)
  is a new exported helper that moves a completed replicate directory
  from scratch to its final location: a fault-tolerant
  `rsync -a --partial` (retry + linear backoff) copies into a sibling
  `.partial` staging dir on the destination filesystem, an atomic rename
  then publishes it so the final dir only ever appears complete, and the
  scratch source is deleted only afterwards (a no-op when source and
  destination resolve to the same path).
- [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  gains a `work_root` argument: when set (or when the `LANDIS_SCRATCH`
  environment variable is non-empty at run time) each replicate is
  staged and run under fast, local, Docker-bind-mountable scratch and
  the finished rep is then moved to its final `scenario_dir/repNN` via
  [`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md),
  so the value the target returns – and everything
  [targets](https://docs.ropensci.org/targets/) tracks – is the final
  location while scratch holds only transient run files; this makes runs
  work when `scenario_dir` lives on storage the Docker daemon cannot
  bind-mount (e.g. a root-squashed NFS share).

## landisutils 0.0.39

### Fix: duplicate-dispatch corruption in LANDIS-II run helpers

- [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  now derives each container’s name deterministically from the scenario
  directory and relies on docker’s name uniqueness as a cross-worker
  mutex. If the same replicate is dispatched to two workers at once (for
  example when `targets` re-runs a branch after a false-positive worker
  crash while the original container is still running), the second call
  no longer starts a parallel container that `O_TRUNC`s the first run’s
  half-written outputs. Instead it adopts the in-progress container,
  waits for it to finish (applying the post-completion watchdog so an
  orphan whose owning worker died cannot hang), and returns that run’s
  result, so a duplicate dispatch is reported as success rather than
  destroying the replicate.
- [`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md)
  serializes replicate runs with an advisory `filelock` lock on the
  rep’s `log/run.lock`, preventing two concurrent `dotnet` processes
  from corrupting the same directory. Adds `filelock` to Imports.

## landisutils 0.0.38

### Fix: corrupt BioSIM `BUI` values reaching downstream weather summaries

- [`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md)
  now recomputes `BUI` from `DMC`/`DC` with `cffdrs`, before (and as
  input to) the `FWI` recomputation. BioSIM `FWI_Daily` occasionally
  returns corrupt `BUI` (up to ~5.8e5, with `DMC` up to ~2.9e5);
  previously only `FFMC`/`ISI`/`FWI` were corrected, so corrupt `BUI`
  propagated into e.g. the Dynamic Fire weather database (where `BUI` is
  a column).

## landisutils 0.0.37

### Fix: `MinRelativeBiomass` dropped its first ecoregion

- [`prepMinRelativeBiomass()`](https://for-cast.github.io/landisutils/reference/prepMinRelativeBiomass.md)
  now emits a leading label column.
  [`insertMinRelativeBiomass()`](https://for-cast.github.io/landisutils/reference/insertMinRelativeBiomass.md)
  formats the table with `.collapseRow(df, i) = df[i, -1]`, which drops
  the first column (the ShadeClass/label slot, since the shade-class
  prefixes `1..5` are hard-coded in the output). Without a leading label
  column the drop removed the FIRST ECOREGION from the table header and
  every shade-class row, so a Biomass Succession run aborted with
  “Minimum relative biomass has not been defined for ecoregion 1”
  whenever the active ecoregion set started at the lowest map code.
  Added a `prep -> insert` regression test.

## landisutils 0.0.36

### Per-trial loss-decomposition trace in Dynamic Fire calibration

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  now writes a per-trial CSV alongside the existing per-iter trace. The
  new CSV (`trial_trace_<timestamp>.csv` under `out_dir`) records, for
  every `objfn` evaluation, the parameter vector, the total loss, the
  raw per-component loss, the weights, and the weighted per-component
  loss. The new path is returned as `trial_trace_path` in the function’s
  return list. FORK workers write to per-PID sidecars merged at the end,
  so the full evaluation history is captured without locking. Useful for
  plotting how DEoptim trades off the four loss components (`count`,
  `size`, `area_fuel`, `severity`) as it converges – not just the
  per-iter best total. Falls back to `NA_character_` for mock /
  parallel-disabled runs that produce no rows.

## landisutils 0.0.35

### Global per-cell BioSIM monthly cache

- [`prep_monthly_weather_biosim()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
  gains a `ref_grid` argument. When supplied with a fixed reference grid
  (e.g. a region-wide raster), the monthly BioSIM pull is cached by the
  grid’s STABLE GLOBAL cell ids in one shared, accumulating store – so
  overlapping / nested study areas reuse cells already fetched and only
  pull the rest (run a district, then a landscape unit within it fetches
  nothing). Without `ref_grid` the previous per-study-area (elevatr `z`)
  behaviour is unchanged.
- [`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md)
  now reprojects grid cell coordinates to lon/lat for BioSIM, so a
  PROJECTED reference grid (e.g. the aggregated rasterToMatch in an
  equal-area CRS) yields valid `longDeg`/`latDeg`; a lon/lat grid (the
  elevatr default) is unaffected.
- The store is climate-only (keyed by `CellID`); ecoregion grouping is
  applied at assemble time:
  [`assemble_climate_library_file_monthly()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_monthly.md)
  gains `cell_eco` (a `CellID -> EcoID` map) and `cell_ids` (filter to
  one study area’s cells).
  [`get_clim_monthly()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly.md)’s
  public behaviour is unchanged; its BioSIM fetch is factored into an
  internal helper shared with the new path.
- Internal: the batch-x-year pull now uses
  [`purrr::map2()`](https://purrr.tidyverse.org/reference/map2.html)
  (sequential within a call) rather than
  [`furrr::future_map2()`](https://furrr.futureverse.org/reference/future_map2.html)
  – the orchestrator (targets/crew) owns cross-branch parallelism, and
  an internal furrr fan-out under a parallel ambient plan overwhelmed
  the shared BioSIM web service into an uninterruptible J4R socket hang.

## landisutils 0.0.34

### `landis_run_docker()` no longer spawns a nested R session

- [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  now launches the `docker run` child via
  [`processx::process`](http://processx.r-lib.org/reference/process.md)
  instead of
  [`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html). The
  old approach forked a **full background R session** purely to shell
  out to `docker`; when that ran on a
  [crew](https://wlandau.github.io/crew/)/[mirai](https://mirai.r-lib.org)
  worker, the nested R process could be orphaned or crash its parent
  worker (`could not start R ... crashed or was killed`), which aborted
  `tar_make()` and SIGKILLed in-flight LANDIS containers (exit 137,
  mid-run). `processx` simply `exec()`s the docker CLI — the same
  lightweight child as the existing `docker stats` polls — eliminating
  that interaction. Behaviour is otherwise unchanged: `docker run` stays
  in the foreground (so the exit status is the container’s), `--rm`
  still auto-removes, the post-completion watchdog and resource logging
  are identical, and the container exit code is read via `processx`’s
  `get_exit_status()`.

## landisutils 0.0.33

### Early-stopping convergence criteria in `calibrate_dynamic_fire()`

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  now forwards `reltol` and `steptol` to
  [`DEoptim::DEoptim.control()`](https://rdrr.io/pkg/DEoptim/man/DEoptim.control.html),
  with project-friendly defaults of `reltol = 1e-3` (0.1% relative
  improvement) and `steptol = 25` generations. DEoptim halts before
  `itermax` if the best-of-population objective fails to improve by more
  than `reltol` for `steptol` consecutive generations. The previous
  behaviour (run the full `itermax` schedule regardless of convergence)
  was the DEoptim default of `steptol = itermax`; that default left the
  optimiser exposed to long no-improvement tails which, on this project,
  intersected an external Docker daemon restart and aborted an
  otherwise-converged run mid-trace. Callers can disable early stopping
  by setting `cfg$steptol >= cfg$itermax`, or restore the upstream
  default by setting `cfg$steptol = NULL`.

## landisutils 0.0.32

### Post-completion watchdog in `landis_run_docker()`

- [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  now watches the container’s stdout for the LANDIS-II console marker
  `"Model run is complete."` and SIGTERMs the container if it fails to
  exit on its own within `post_completion_timeout_sec` seconds (default
  `300` = 5 min). Some long ForCS + Dynamic Fire scenarios with many
  output extensions log the completion marker but then spin in the .NET
  runtime shutdown path indefinitely (observed 25+ h hangs at 100% CPU,
  outputs already on disk). The watchdog stops these zombie containers
  and treats exit codes `137`/`143` as success when the completion
  marker was seen, so `tar_make()` correctly marks the affected reps
  complete. The sim outputs are byte-identical to a clean exit because
  the watchdog only fires *after* the LANDIS-II console reports
  completion. Pass `post_completion_timeout_sec = Inf` to restore the
  old (unbounded-wait) behaviour.
- [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  gains a matching `post_completion_timeout_sec` argument (default
  `300`) that forwards to
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
  so the watchdog grace period is tunable (or disablable with `Inf`)
  from the [targets](https://docs.ropensci.org/targets/) pipeline. The
  decision is factored into a small internal predicate,
  `.watchdog_should_stop()`, with unit coverage.

## landisutils 0.0.31

### Biomass Succession support in the Dynamic Fire calibration setup

- [`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md)
  and
  [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md)
  now auto-detect the succession backend (ForC Succession vs Biomass
  Succession) from the template directory and wire the appropriate
  succession extension, calibration freeze, and output manifest.
  Previously both were hard-wired to ForC Succession (they required
  `forc-succession.txt`), so a Biomass Succession project could not
  build a Dynamic Fire calibration scenario. Biomass Succession is
  frozen for the calibration by setting its `Timestep` greater than
  `sim_years` (it has no `SpinUp` section, and no ForCS `Soil.cs`
  DOM-spinup workaround is needed); ForCS behaviour is unchanged.

## landisutils 0.0.30

### Dyadic season proportions in `insertSeasonTable()`

- [`insertSeasonTable()`](https://for-cast.github.io/landisutils/reference/insertSeasonTable.md)
  now quantises the `ProportionFire` column to dyadic `1/128` fractions
  before writing. The Dynamic Fire System reads these proportions as
  single-precision floats and rejects the table (“Season Probabilities
  don’t add to 1.0”) unless they sum to 1.0 with essentially zero
  tolerance, so arbitrary decimal proportions (e.g. observed fire counts
  divided by the total, which only sum to 1.0 in exact or
  double-precision arithmetic) failed the check unpredictably depending
  on the data. Dyadic `1/128` fractions are exactly representable in
  float, sum to exactly 1.0 regardless of summation order, and
  round-trip through the table formatting; the largest season absorbs
  the rounding so the values still sum to 1.0. Callers can now pass raw
  normalised season proportions and rely on the table writer for
  LANDIS-II compatibility (1/128 is ~0.8% granularity).

## landisutils 0.0.29

### Optional `SpinupCohorts` / `SpinupMortalityFraction` in `BiomassSuccession`

- `BiomassSuccession$new()` no longer force-writes `SpinupCohorts` and
  `SpinupMortalityFraction`: these keywords are absent from the Core8
  `CoreV8.0-BiomassSuccession7.0` grammar, and the LANDIS-II v8 parser
  aborts (“Found the name "SpinupCohorts" but expected
  "MinRelativeBiomass"”) when they appear. They are now optional and
  emitted only when set to a non-`NULL` value, so a default
  `BiomassSuccession` config parses and runs against the stock
  `landis-ii-v8-release` image.

### Request throttling + backoff for BioSIM weather fetches

- BioSIM weather retrieval now staggers requests and retries with
  exponential backoff. The `BioSIM` web API can be slow or transiently
  unavailable under load, and the `BioSIM` R client exposes no
  timeout/retry knobs, so
  [`get_clim_monthly()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly.md),
  [`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md),
  and
  [`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md)
  route their `generateWeather()` calls through a shared wrapper that
  adds a random pre-request delay, exponential-backoff retries, and an
  optional per-attempt timeout (resetting the J4R client on timeout).
  Behaviour is tunable via the `landisutils.biosim.request_delay`,
  `landisutils.biosim.max_attempts`, `landisutils.biosim.backoff_base`,
  and `landisutils.biosim.timeout` options; set them where the fetch
  process can see them (e.g. a worker-inherited `.Rprofile`).

## landisutils 0.0.28

### Enable DOM spinup in calibration scenario template

- [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md)
  now patches the calibration scenario’s `forc-succession.txt` `SpinUp`
  row to `1 0 1 20` (DOM spinup ON, biomass spinup OFF), instead of the
  prior `0 0 1 20` (both OFF). Biomass spinup stays off so the
  pre-calibration snapshot IC’s `CohortBiomass` values are preserved
  verbatim, but the DOM spinup pass equilibrates ForCS’s soil-pool state
  via `SpinupSoils()`. Without it, `DisturbFireFromBiomassPools` is left
  in a partly-initialised state and the first cohort that Dynamic Fire
  damages triggers a `NullReferenceException` in
  `Extension-ForCS-Succession/src/Soil.cs:DisturbanceImpactsBiomass`,
  aborting every calibration trial. Cost: ~30-60s extra startup per
  LANDIS-II trial.

## landisutils 0.0.27

### Parallel pool teardown in `landis_pool_stop()`

- [`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md)
  now passes every container name to a single
  `docker stop --time T <name1> <name2> ...` and a single
  `docker rm -f <name1> <name2> ...`, instead of looping
  one-container-at-a-time. The Docker daemon parallelises stops
  internally, so a 90-container pool teardown drops from ~15 minutes (90
  x 10s SIGTERM deadlines, sequential) to roughly `timeout_sec` wall
  time. The function remains idempotent and tolerant of already-removed
  containers.

## landisutils 0.0.26

### `cfg$scratch_root` override for `calibrate_dynamic_fire()`

- The warm Docker pool’s bind-mount source defaults to
  `<out_dir>/scratch`, but this fails when `out_dir` lives on a
  filesystem the Docker daemon cannot see (e.g. user-space autofs /
  sshfs / NFS mounts). The daemon errors with
  `mkdir <mount-root>: permission denied` while resolving the bind-mount
  path. `cfg$scratch_root` lets callers route the pool’s scratch onto
  docker-visible local storage while keeping `out_dir` on the project
  mount so the calibration trace CSV and final outputs land alongside
  the rest of the project.

## landisutils 0.0.25

### Connection-aware default `n_cores`

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  now picks its default cluster size from
  `parallelly::availableCores(constraints = "connections", omit = 2)`
  when parallelly is installed, falling back to
  `parallel::detectCores() - 2L` otherwise. `detectCores()` reports the
  logical core count and ignores R’s per-session connection cap (~125 on
  default builds), so on very large hosts (e.g. 256-core machines) a
  naive default silently over-provisions the FORK cluster beyond what
  the R session can support. Explicitly setting `cfg$n_cores`
  short-circuits both defaults. `parallelly` is now a `Suggests`.

### Work around DEoptim 2.2.8 cluster-cleanup bug

- [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  no longer passes `parallelType = 1L` to
  [`DEoptim::DEoptim()`](https://rdrr.io/pkg/DEoptim/man/DEoptim.html)
  when supplying its own FORK cluster via `control$cluster`. DEoptim
  2.2.8’s `ctrl$cluster` branch uses the supplied cluster without
  binding a local `cl` variable, but its post-loop cleanup
  unconditionally evaluates `parallel::stopCluster(cl)` whenever
  `parallelType == "parallel"`, which errors with
  `object 'cl' not found`. Leaving `parallelType` at its default
  (“none”) skips that cleanup path while still triggering the parallel
  `parApply(cl = ctrl$cluster, ...)` evaluation in DEoptim’s body. The
  FORK cluster lifecycle is fully managed by
  [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  via `on.exit`.

### Clamp `IgnProb` to LANDIS-II’s permitted range

- [`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md)
  and
  [`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md)
  now clamp the per-fuel `IgnProb` value to `[0, 1]` after applying the
  calibration multiplier. The Dynamic Fire System parser rejects any
  `IgnProb` outside this range with
  `Error with the input value for Fuel type initiation probability: Value must be between 0 and 1.0`
  and aborts the run, which previously caused every DEoptim trial whose
  multiplier pushed the product above 1.0 (e.g., `IgnProb_Conifer = 1.5`
  against the default `IgnProb = 1.0` for Conifer surfaces
  C1-C5/C7/M1-M4) to fail immediately. The multiplier ranges in the
  smoke test and
  [`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md)
  are unchanged; clamping just makes the search-space boundary explicit
  instead of relying on every multiplier being \<= 1.0.

### Retain failed-trial scratch directories for post-mortem

- [`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md)
  now keeps a failing trial’s per-rep scratch directory on disk (and
  prints its path) when the LANDIS-II invocation errors. The scratch was
  previously deleted unconditionally by an `on.exit` cleanup, which made
  it impossible to inspect `<trial_dir>/rep01/log/` for the underlying
  LANDIS-II stderr/stdout from a failed calibration trial. Successful
  trials are still cleaned up as before, so this only adds disk usage on
  failure. The behaviour can still be opted out of by passing
  `keep_scratch = TRUE` (which now retains the dir on both success and
  failure – the prior `keep_scratch = FALSE` default still means “clean
  up after success only”).

## landisutils 0.0.24

### Per-file input overrides on `build_calibration_scenario_template()`

- New `overrides = list()` argument lets callers substitute individual
  template files post-copy without touching the production scenario.
  Useful for cropping / aggregating specific inputs for calibration
  (e.g., a coarser fuel raster, a smaller weather DB, a substitute slope
  raster) without forking the whole template. Accepted keys:
  `ground_slope.tif`, `uphill_slope_azimuth.tif`, `fire-ecoregions.tif`,
  `initial_weather_database.csv`, `DynamicFire_Spp_Table.csv`,
  `species.txt`, `ecoregions.txt`, `ecoregions.tif`, `climate.txt`.
  `.tif` overrides also carry their `.aux.xml` / `.tfw` sidecars.
  Backward compatible: `overrides = list()` (the default) preserves the
  original “copy everything from `template_dir`” behaviour.

### Warm Docker pool resilience

- New exported `landis_pool_restart_one(pool, idx)` – stops + removes
  the container at index `idx` and starts a fresh replacement with
  identical config (image, scratch_root bind-mount, user, cpu_limit,
  mem_limit) using a new auto-generated container name. Pool state
  (`$names[idx]`) is updated in place and also propagated to the
  caller’s frame so loops can use the current container name on the next
  iteration.
- [`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)
  gains `retries = 0L`. When \> 0 and the exec command fails with
  non-zero status, the container is restarted via
  [`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md)
  and the command retried, up to `retries` additional attempts. Useful
  for long calibrations that occasionally hit OOM kills or daemon
  hiccups without wanting the whole DEoptim run to abort. Returns an
  additional `attempts` field counting actual attempts (1 = no retry
  needed; \>1 = some retries consumed).
- Pool object now carries the start-time args (`user_args`, `cpu_args`,
  `mem_args`) so restarts produce containers with matching config.
- Refactor: container-creation logic moved into the internal
  `.landis_pool_start_one()` so
  [`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md)
  and
  [`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md)
  share one code path.

### Pre-flight checks in `calibrate_dynamic_fire()`

The driver now runs a battery of cheap pre-flight checks at function
entry – before starting the warm Docker pool or FORK cluster – to catch
common config / scenario / payload errors fast. Hard errors include:

- `cfg$lower >= cfg$upper` for any parameter (lists the offending
  names).
- `cfg$NP < 4` (DEoptim minimum), `cfg$itermax < 1`, `cfg$n_reps < 1`.
- `cfg$weights` all zero (DEoptim would have nothing to optimise).
- Unknown `cfg$simulator` (one of `"landis"`, `"r_reimpl"`, `"mock"`).
- For `simulator = "landis"`: the calibration scenario template missing
  any of the LANDIS-II input files normally produced by
  [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md).
- Observed-targets payload missing required `$primary` shape.
- Docker not available when `method = "docker"`; LANDIS console not
  findable when `method = "local"`.
- Scratch root not writable.

Soft signals (warnings / messages):

- `NP < 10 * length(par_names)` (DEoptim’s own advisory, surfaced
  earlier).
- `cfg$weights['area_fuel'] > 0` but observed lacks `area_by_fuel_ha` /
  `fuel_code_to_base`; or `cfg$weights['severity'] > 0` but observed
  lacks `severity_dist`. In both cases the corresponding loss component
  contributes 0.

### Calibration smoke-test script

- New `inst/scripts/calibration_smoke_test.R` – a complete 5-stage smoke
  test of the calibration plumbing (observed targets, spinup, scenario
  template, sim_landis trial, DEoptim loop) at minimal scale (NP=4,
  itermax=2, n_reps=1, n_cores=2). Useful for first-time setup, post-
  upgrade verification, and confirming Docker + DEoptim are installed
  correctly. Runs in ~5-20 min via the warm Docker pool.

  Usage:

  ``` r

  source(system.file("scripts/calibration_smoke_test.R", package = "landisutils"))
  ```

### Tier 2 calibration loss components

[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md)
now computes `L_area_fuel` and `L_severity` when the corresponding
observed components are present in the payload; previously both were
stubbed at zero. Components are still gated by their `weights` entry, so
projects that aren’t ready to use them stay on the Tier 1 (count + size)
loss by default.

- `L_area_fuel` is a chi-squared distance between simulated and observed
  burn-area-by-base-fuel-type *proportions*. Simulated area-by-fuel is
  derived from each event’s `init_fuel` (the ignition cell’s fuel code)
  times its `DamagedSites`, mapped to the five base fuel types via
  `observed$fuel_code_to_base`. Activated when
  `observed$primary$area_by_fuel_ha` AND `observed$fuel_code_to_base`
  are both set; contributes 0 otherwise.

- `L_severity` is a chi-squared distance between simulated and observed
  severity-class *proportions*. Simulated severities come from each
  event’s `MeanSeverity` (binned into integer classes 1..5 at
  half-integer boundaries); observed comes from
  `observed$primary$severity_dist`, a named numeric vector summing to 1.
  Activated when `severity_dist` is non-NULL.

- [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md)
  gains a `severity_dist = NULL` argument that callers can pass to embed
  a prior in the observed payload (e.g., the new
  [`default_severity_prior_sturtevant2009()`](https://for-cast.github.io/landisutils/reference/default_severity_prior_sturtevant2009.md)).

- New exported helper:
  [`default_severity_prior_sturtevant2009()`](https://for-cast.github.io/landisutils/reference/default_severity_prior_sturtevant2009.md)
  returns a named 5-element vector of severity-class proportions derived
  from Sturtevant et al. 2009. Intended as a starting point; projects
  should override with empirical priors when available.

### Dynamic Fire System extension calibration

A new function family for calibrating the LANDIS-II Dynamic Fire System
extension. The calibration tunes `SeverityCalibrationFactor`, per-season
FMC `HiProp` values, and per-base-fuel-type `IgnProb` multipliers so
simulated fires match observed regional fire statistics; the
empirically-fit parameters (`Mu`, `Sigma`, `Max`, `NumFires`, seasonal
`PropFire`) are fit at the data layer upstream and are not part of the
optimisation.

- **Pure-data helpers** (no LANDIS-II invocation):

  - [`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md)
    – canonical 9-name parameter vector.
  - [`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
    – reads `fire/dynamic-fire-event-log.csv` +
    `fire/dynamic-fire-summary-log.csv` into a small per-rep stats list.
  - [`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md)
    – surgical text patch of `dynamic-fire.txt`
    (SeverityCalibrationFactor scalar; FireSizesTable HiProp columns;
    FuelTypeTable IgnProb column multiplied per base type).
  - [`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md)
    – multi-component weighted loss (count + KS-on-size in Tier 1;
    area_by_fuel + severity stubbed at weight 0 for Tier 2).
  - [`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md)
    /
    [`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md)
    – table-level helpers used downstream of calibration to feed
    calibrated values into production fire-config writers.

- **Observed-target builder** (one-time NFDB-derived summaries):

  - [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md)
    – writes a small `.rds` of per-ecoregion observed summaries.
    Project-agnostic: primary / secondary ecoregion SpatVectors +
    `fuel_code_to_base` mapping are all caller-provided.
  - [`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md)
    – default fuel-code mapping for BC `FUEL_TYPE_CD` factor encoding
    (codes 1..13); pass a custom mapping if your project’s
    fuel-classification raster uses a different encoding.

- **Scenario builders** for the static-landscape calibration scenario:

  - [`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md)
    – builds a one-off LANDIS-II scenario that runs ForCS with both
    spinup flags ON and emits a snapshot of the spun-up cohort community
    via the Output Biomass Community extension. The year-0 snapshot
    becomes the calibration IC.
  - [`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md)
    – blocking single-trial LANDIS-II invocation for the spinup;
    dispatches to
    [`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md)
    /
    [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md).
  - [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md)
    – copies a production fire scenario, swaps the IC for the spinup
    snapshot, patches ForCS for calibration (spinup off + frozen
    succession), optionally writes a fresh baseline `dynamic-fire.txt`
    inline (breaks the cycle between a `calibrated_fire_params`-aware
    production fire config and the calibration loop).
  - [`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
    (in `scenarios.R`) – lower-level `scenario.txt` writer that takes
    already-written extension config-file paths rather than R6 extension
    objects. Useful when project pipelines write extension configs in
    separate steps.

- **Warm Docker pool** for calibration:

  - [`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md)
    /
    [`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)
    /
    [`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md)
    – a pool of detached LANDIS-II containers that `docker exec`s per
    DEoptim trial instead of `docker run --rm` per trial. Per-call env
    overrides (`HOME=/tmp`, `DOTNET_BUNDLE_EXTRACT_BASE_DIR=...`) keep
    dotnet from accreting per-user-cache state between trials. Designed
    for [`on.exit()`](https://rdrr.io/r/base/on.exit.html) teardown from
    calibration drivers.

- **Simulator backends** for the calibration loop:

  - [`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md)
    – per-trial LANDIS-II invocation. Takes file paths only (FORK-safe),
    copies template -\> scratch dir, patches `dynamic-fire.txt`, runs
    LANDIS-II either via the warm pool or a one-off
    [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
    /
    [`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
    parses logs.
  - [`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
    – reserved slot for a future pure-R reimplementation; currently
    errors with a not-yet-implemented message.
  - [`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md)
    – plausibly-shaped output for testing the calibration driver’s
    control flow without Docker.

- **DEoptim driver:**

  - [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
    – orchestrates the calibration: starts a warm Docker pool, sets up a
    FORK cluster with per-worker container pinning, invokes
    [`DEoptim::DEoptim()`](https://rdrr.io/pkg/DEoptim/man/DEoptim.html)
    with the multi-component loss as the objective, tears down pool +
    cluster via [`on.exit()`](https://rdrr.io/r/base/on.exit.html).
    Gated on
    [`requireNamespace("DEoptim")`](https://github.com/ArdiaD/DEoptim)
    (DEoptim is in Suggests; install via `renv::install("DEoptim")`
    before calling).

- **Vignette:**
  [`vignette("Dynamic-Fire-Calibration", package = "landisutils")`](https://for-cast.github.io/landisutils/articles/Dynamic-Fire-Calibration.md)
  documents the end-to-end target-wiring pattern for downstream
  projects.

- **Tests:** 97+ testthat expectations across calibration + pool test
  files. Docker-gated tests `skip_if_not()` when docker is unavailable;
  DEoptim-gated driver tests `skip_if_not_installed("DEoptim")`.

## landisutils 0.0.23

### `landis_run_docker()` accepts resource constraints

- New arguments to
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  (and proxied through
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)):
  - `cpu_limit = 4`: maps to `docker run --cpus`. LANDIS-II compute is
    single-threaded (~1 core), but the .NET runtime spins up 9-11 OS
    threads for GC and the thread pool, so 4 is a comfortable default.
    Pass `NULL` for no limit.
  - `mem_limit = "8g"`: baseline RAM cap (maps to
    `docker run --memory`). Accepts a numeric byte count or a string
    like `"4g"` / `"512m"`. Pass `NULL` (or `mem_limit = Inf`) for no
    limit.
  - `mem_margin = 1.5`: headroom factor applied to a previously-observed
    peak (see auto-resolution below).
- **Auto-resolution from prior resource logs.** Before running, the
  function reads any existing
  `<rep_dir>/log/{docker,local}_resources.log` for the rep. If
  `peak_mem_bytes * mem_margin` exceeds the baseline `mem_limit`, the
  limit is raised to that value so a rep that ran fine last time is
  never killed by the cap on a rerun. If **no** prior log exists for the
  rep (first run, or rep dir freshly deleted), the memory cap is dropped
  entirely so the first run can discover what it needs; subsequent runs
  inherit the empirically observed peak.
- The CPU limit is constant regardless of history: LANDIS-II is
  single-threaded and the .NET runtime doesn’t scale with available
  cores.

## landisutils 0.0.22

### Resource logs now self-describe the host

- [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  and
  [`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md)
  now append three additional lines to each rep’s `docker_resources.log`
  / `local_resources.log`:

      host_cpu_model: <model name>          # e.g. "AMD EPYC 7702 64-Core Processor"
      host_cpu_cores: <N>                   # logical cores visible to R
      host_ram_bytes: <N>                   # total system memory in bytes

  This makes each per-rep resource log self-describing: downstream
  provenance tooling can recover not just what the rep used
  (`elapsed_sec`, `peak_mem_bytes`) but the host it ran on, important
  when reps are dispatched across a heterogeneous cluster.

- New exported helpers:

  - [`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md)
    returns `list(model, n_logical, ram_bytes)`, cross-platform:
    `/proc/cpuinfo` + `/proc/meminfo` on Linux,
    `sysctl machdep.cpu.brand_string` + `hw.memsize` on macOS,
    `PROCESSOR_IDENTIFIER` env var +
    `wmic ComputerSystem ... TotalPhysicalMemory` on Windows.
    Logical-core count uses
    [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
    everywhere. Called automatically by the run helpers.
  - `read_landis_resource_logs(run_dir)` parses any `*_resources.log`
    under `run_dir` and returns one data.frame row per rep with all
    fields. Used by downstream report tooling to summarise run-time /
    memory / host across replicates.

## landisutils 0.0.21

### `prepTopographyFile()` fills NoData with 0

- [`prepTopographyFile()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md)
  (and its
  [`prepGroundSlopeFile()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md)
  /
  [`prepUphillAzimuthMap()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md)
  wrappers) now replaces NoData cells with `0` before writing the INT2S
  raster.
  [`terra::terrain()`](https://rspatial.github.io/terra/reference/terrain.html)
  leaves edge cells as `NaN` (no full 3x3 neighbourhood), which becomes
  the `-32768` sentinel under INT2S. LANDIS-II’s Dynamic Fire reader
  rejects any active cell with that value:

      Ground Slope invalid map code: -32768

  The bug was latent for grids with few active cells, but is exposed
  whenever upstream changes (e.g. corrected non-veg masks) expand the
  active area into the 1-cell edge band of the terrain raster. A flat
  default (0 deg) is safe: a single-cell edge contributes negligibly to
  Dynamic Fire’s rate-of-spread.

## landisutils 0.0.20

### `tar_landis()` idempotency respects input changes

- The skip check in
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  is now input-aware. Previously the run was skipped whenever
  `<rep_dir>/Landis-log.txt` existed and contained “Model run is
  complete”, regardless of whether the inputs had changed. This meant
  that when [targets](https://docs.ropensci.org/targets/) correctly
  re-evaluated the run target after an upstream input change (e.g. a
  regenerated `initial-communities.tif` or `ecoregions.tif`), the
  surrounding command ran but the skip check still fired, so
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  /
  [`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md)
  was never invoked and the rep dir kept stale outputs from the previous
  run.
- The fix writes a `<rep_dir>/log/input_hash.json` sidecar after each
  successful run, capturing a SHA-1 of (per-input-file MD5 +
  `base_seed` + `rep_index` + `scenario_file`). The skip check now also
  requires the saved hash to match the current input hash; any mismatch
  triggers a real rerun.
- New `force = FALSE` argument on
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md).
  Setting `force = TRUE` short-circuits the skip check so LANDIS-II
  always runs (useful for debugging and one-off forced reruns without
  deleting rep dirs).
- **Migration:** existing rep dirs lack `log/input_hash.json`, so the
  first `tar_make()` after upgrading rebuilds every rep. This is the
  safe-conservative behaviour: we can’t know whether existing outputs
  correspond to current inputs. Users who *know* a rep is current can
  sidestep the rerun by writing the hash file manually.

## landisutils 0.0.19

### `landis_run_docker()` captures image digest

- [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  now writes `<scenario_dir>/log/docker_image.log` containing the
  immutable `sha256` digest of the image used for the run
  (`<repo>@sha256:<64hex>`, falling back to the local `Id` for images
  with no registry origin). Image tags are mutable; the digest is the
  canonical identifier of the bytes that actually ran. Downstream
  provenance tooling can read this sidecar to pin a run to a specific
  image regardless of subsequent tag movement.
- New `pull = FALSE` argument; when `TRUE`, `docker pull <image>` runs
  before the simulation so the captured digest reflects the current
  registry rather than a possibly-stale local copy. The argument is also
  exposed via `tar_landis(pull = ...)`.

## landisutils 0.0.18

### `leading_species()` handles non-vegetated cells

- [`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md)
  now returns `"Non-vegetated"` for cells where total biomass across all
  species is zero, instead of falling through to the alphabetical
  tiebreaker (which arbitrarily assigned `"Ac"` to defoliated cells).
  This matches the existing
  [`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md)
  behaviour and means transition / alluvial plots accurately reflect
  fire and harvest impacts.

## landisutils 0.0.17

### Per-replicate parallel branching in `tar_landis()`

- **Breaking API change:**
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  no longer accepts `n_reps`. It now takes an explicit `rep_index`
  argument (an unquoted upstream target symbol) and returns a **single**
  `tar_target` object (same as the original API).

- The caller creates the rep-index target explicitly inside the module
  [`list()`](https://rdrr.io/r/base/list.html) alongside the
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  call. This keeps both targets visible to tarborist’s static AST
  analysis (`tarborist.additionalSingleTargetFactories` handles the run
  target; the literal `tar_target()` call handles the rep-index target):

  ``` r

  list(
    tar_target(name = ..._rep_index, command = seq_len(5L), iteration = "vector"),
    landisutils::tar_landis(name = ..., rep_index = ..._rep_index, ...,
                            pattern = cross(scenario_dir, ..._rep_index))
  )
  ```

- `iteration = "vector"` on the rep-index target is what enables
  `cross()` to iterate over individual elements, giving
  `n_scenarios x n_reps` independent branches dispatched to crew workers
  in parallel.

- Each branch runs **one** LANDIS-II simulation and tracks only that
  replicate’s output files. Previously all `n_reps` simulations ran
  inside a single `for` loop within one target branch.

- **Caching and scaling:** adding replicates only creates new branches –
  existing replicate results remain cached. Changing one replicate’s
  inputs invalidates only that branch. Previously any change invalidated
  all replicates.

### `landis_replicate()` single-rep mode

- [`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md)
  gains a `rep_index` parameter for single-replicate creation. Pass
  `rep_index = i` (instead of `n_reps = N`) to create exactly one
  replicate directory (`repNN/`) without touching any others. Useful
  when each replicate is dispatched to its own crew worker.
- `n_reps` is now keyword-only; the positional second-argument form
  still works but `n_reps =` is clearer.
- The function now requires exactly one of `n_reps` or `rep_index`.
- The seed assigned when `rep_index` is used
  (`base_seed + rep_index - 1`) matches the seed assigned by the
  `n_reps` mode for the same index, so results are reproducible
  regardless of which call form was used.

## landisutils 0.0.16

### Vegetation dynamics: species biomass and transition plots

- New `read_biomass_c_snapshots(paths, times, run_name)` reads ForCS
  `log_BiomassC.csv` files (per-cohort, per-cell) for one or more
  replicates, filtering to requested snapshot years via
  [`arrow::open_dataset()`](https://arrow.apache.org/docs/r/reference/open_dataset.html)
  lazy streaming so that multi-GB files never need to be fully
  materialised in R. ForCS writes `log_BiomassC.csv` unconditionally, so
  no additional output extension is required.
- New
  `read_biomass_output_rasters(dirs, times, species, live_map_pattern, run_name)`
  is the succession-agnostic alternative: reads per-species biomass
  rasters written by the Output.Biomass v4 extension (present in the
  `landis-ii-v8-release` Docker image). Works with any succession
  extension; requires Output.Biomass to be included in the scenario
  configuration.
- Both readers return an identical `data.table` schema
  (`scenario, replicate, Time, row, column, [ecoregion,] species, biomass`
  in Mg C ha^-1), so all downstream functions are source-agnostic.
- New `biomass_landscape_summary(df)` aggregates per-cell snapshot data
  to landscape-mean ± SD biomass by species per timestep.
- New `leading_species(df)` labels each cell at each snapshot by the
  species with the highest total live biomass. Ties broken
  alphabetically.
- New `community_label(df, n_spp, min_pct)` labels each cell by its
  top-`n` species combination (e.g. `"Hw-Sx"`); species below `min_pct`
  of cell total are dropped; zero-biomass cells are labelled
  `"Non-vegetated"`.
- New `transition_data(label_df, times)` builds the lodes-form `tibble`
  required by `ggalluvial`: unique label-path combinations across all
  snapshot years, with cell counts averaged across replicates.
- New `plot_species_biomass(summary_df, colours, title)` produces a
  stacked area chart of landscape-mean biomass by species over time.
- New `plot_transitions(lodes_df, colours, title)` produces a
  Sankey-style alluvial diagram (via `ggalluvial`) showing how cells
  move between vegetation types across snapshot years.
- `arrow`, `ggalluvial`, `ggplot2`, and `purrr` added to `Imports`
  (previously absent or in `Suggests`).

## landisutils 0.0.15

### `tar_landis()` / `landis_run_docker()` fixes

- [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md):
  container names now include the calling process PID and a random
  integer suffix to prevent name collisions when multiple LANDIS
  replicates run simultaneously.
- [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md):
  dependency file lists are now deduplicated before being passed to
  [`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md).
  Paths are normalised to absolute form, filtered to existing files,
  then deduplicated by basename; scenario-specific files (under the
  scenario directory) take priority over cross-scenario duplicates.
  Applies to both the Docker and local run paths.
- [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md):
  replicates where LANDIS-II already completed successfully are now
  skipped (idempotent re-run). Completion is detected by the presence of
  “Model run is complete” in `Landis-log.txt`. Applies to both the
  Docker and local run paths.

## landisutils 0.0.14

### ForCS Succession extension

- [`insertDOMPools()`](https://for-cast.github.io/landisutils/reference/insertDOMPools.md)
  now wraps multi-word pool names in double-quotes, matching the
  LANDIS-II parser’s requirement (e.g. `"Fast AG"` instead of
  `Fast AG`).
- ForCS v4 changed four large parameter tables from inline text to CSV
  file references.
  [`insertEcoSppDOMParameters()`](https://for-cast.github.io/landisutils/reference/insertEcoSppDOMParameters.md),
  [`insertANPPTimeSeries()`](https://for-cast.github.io/landisutils/reference/insertANPPTimeSeries.md),
  [`insertMaxBiomassTimeSeries()`](https://for-cast.github.io/landisutils/reference/insertMaxBiomassTimeSeries.md),
  and
  [`insertEstablishProbabilities()`](https://for-cast.github.io/landisutils/reference/insertEstablishProbabilities.md)
  now each write a CSV to `path` and emit the `Keyword "filename"`
  reference line, matching the ForCS v4.0.2 input format.
- `ForCS$write()` passes `self$path` to the four CSV-writing `insert*()`
  functions and registers the resulting files via `add_file()` so
  [`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md)
  copies them into each replicate directory.
- The four ForCS CSV filenames are now prefixed with `ForCS_`
  (`ForCS_EcoSppDOMParameters.csv`, `ForCS_ANPPTimeSeries.csv`,
  `ForCS_MaxBiomassTimeSeries.csv`, `ForCS_EstablishProbabilities.csv`)
  so their origin is unambiguous alongside other extension files.

### Output file tracking

- All `LandisExtension` subclasses that produce fixed-name output files
  now expose an `output_files` active binding listing those files as
  relative paths (e.g. log CSVs, summary CSVs). Extensions with no fixed
  outputs inherit the base `LandisExtension$output_files` which returns
  `character(0)`.
- `LandisScenario` gains an `output_files` active binding that returns
  the two LANDIS-II core outputs always written to the scenario
  directory: `Landis-log.txt` and
  `Metadata/LANDIS-II v8.0/LANDIS-II v8.0.xml`.
- [`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md)
  now writes `output_manifest.txt` to the scenario directory, listing
  all fixed-name output files declared by the scenario and its
  extensions. The manifest is registered in `scenario$files` so
  [`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md)
  copies it into each replicate directory.
- [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  now reads `output_manifest.txt` from the base scenario directory and
  includes the listed files (as absolute paths per replicate) in the
  returned character vector alongside the `log/` scan and `output_dir`
  scan. This ensures [targets](https://docs.ropensci.org/targets/)
  tracks log CSVs, `Landis-log.txt`, and the Metadata XML explicitly,
  without relying on glob discovery.

### Resource tracking for simulation runs

- [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  now tracks wall-clock elapsed time and peak container memory. Docker
  is launched via
  [`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html) so the
  main thread can poll `docker stats --no-stream` every 2 s; the maximum
  observed RSS is recorded as peak memory. A named container
  (e.g. `landis-run-20260527123456`) is used for stats lookup and
  removed automatically with `--rm`. Results are printed on completion
  and written to `<scenario_dir>/log/docker_resources.log`. The
  `--user uid:gid` flag is now skipped on Windows (`id -u`/`id -g` are
  not available there; Docker Desktop on Windows does not require it).
- [`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md)
  now tracks wall-clock elapsed time and peak process memory.
  [`system2()`](https://rdrr.io/r/base/system2.html) is replaced by
  `processx::process$new()` (which exposes the subprocess PID and
  handles the working directory directly), and the main thread polls
  [`ps::ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.html)
  every 2 s. The `ps` package is cross-platform, so memory tracking
  works on Linux, macOS, and Windows without any platform-specific shell
  commands. Results are printed on completion and written to
  `<scenario_dir>/log/local_resources.log`.
- Both functions now return a named list (`exit_code`, `elapsed_sec`,
  `peak_mem_bytes`) instead of a bare integer exit code.
- `processx` and `ps` added to `Imports` (previously available only as
  transitive dependencies of `callr`).

## landisutils 0.0.13

- `DynamicFire$write()` now calls `add_file()` for
  `InitialWeatherDatabase` so it is copied into replicate directories by
  [`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md).
  Previously the weather CSV was silently absent, causing LANDIS-II to
  fail at runtime.
- [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  `output_dir` now accepts a character vector of output subdirectory
  names. Pass `c("output", "fire")` when using the Dynamic Fire
  extension, which writes its maps and event/summary logs to a `fire/`
  subdirectory inside the scenario directory.

## landisutils 0.0.12

- [`insertFile()`](https://for-cast.github.io/landisutils/reference/insertFile.md),
  [`insertLandisData()`](https://for-cast.github.io/landisutils/reference/insertLandisData.md),
  [`insertValue()`](https://for-cast.github.io/landisutils/reference/insertValue.md)
  are now exported so project-level code can build custom scenario files
  using the same primitives the package uses internally.
- [`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md)
  is a new helper that returns the path to `Landis.Console.dll` inside
  the container, reading `getOption("landisutils.docker.console")`.
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  now calls it instead of duplicating the lookup.
- [`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md)
  gains a `base_seed` argument. When set, the `RandomNumberSeed` in each
  replicate’s `scenario.txt` is rewritten to
  `base_seed + (rep_index - 1)`, giving every replicate a distinct but
  deterministic seed. Seeds are index-stable: adding more replicates
  later never changes the seeds of existing ones.
- [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  gains a `base_seed` argument, passed through to
  [`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md)
  and baked into the command expression at factory-call time so `crew`
  workers receive the correct value.

## landisutils 0.0.11

- Pin `santoku` to its GitHub source (`hughjonesd/santoku`) after the
  package was archived on CRAN on 2026-05-15, which broke `zonal`
  dependency resolution in GitHub Actions CI.

## landisutils 0.0.10

- Remove unused PredictiveEcology packages: `LandR`, `SpaDES.core`,
  `SpaDES.tools`, `reproducible`, `scfmutils`, and `map` – due to broken
  dependency resolution.

## landisutils 0.0.9

- [`landis_run()`](https://for-cast.github.io/landisutils/reference/landis_run.md):
  fix inverted [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html)
  guard — the function previously rejected valid `LandisScenario`
  objects and accepted everything else.
- New
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  runs a LANDIS-II simulation in an ephemeral Docker container
  (bind-mounting the scenario directory to `/sim`).
- New
  [`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md)
  runs a LANDIS-II simulation directly via `dotnet`, writing
  stdout/stderr to `<scenario_dir>/log/`.
- New
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  factory creates a [targets](https://docs.ropensci.org/targets/)
  `format = "file"` target that runs LANDIS-II (locally or via Docker)
  and returns tracked output and log files.
- New package options `landisutils.docker.image` and
  `landisutils.run.method` are set by `.onLoad()`.

## landisutils 0.0.8

- fix issue with `climr` returning reference period rows - filter these
  when assembling data;

## landisutils 0.0.7

- [`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
  no longer silently advertises `"srad"` as a supported variable.
  Vignette examples updated.
- [`get_elevation_rast()`](https://for-cast.github.io/landisutils/reference/get_elevation_rast.md)
  gained a `tmp_dir` argument (default
  `<landisutils.cache.path>/elevatr_tiles/`) so AWS Terrain Tile
  downloads land in the package cache instead of leaking into the R
  session’s global [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
  (`elevatr`’s own default).
- climate test cleanup: a new `local_climate_test_cache()` test helper
  (`tests/testthat/helper-climate-cleanup.R`) routes the cache option,
  child-process `TMPDIR`, and the JVM `java.io.tmpdir` (used by `J4R`
  for `J4RServer*.log` and `hsperfdata_<user>/`) into the per-test
  [`withr::local_tempdir()`](https://withr.r-lib.org/reference/with_tempfile.html),
  and tears down any `future::plan(multisession)` on exit, so
  `BioSIM`/`climr`/`elevatr` fetch tests no longer accumulate `/tmp`
  residue across runs.

## landisutils 0.0.6

- added a focused integration-test scenario `necn_scrpple` exercising
  `NECNSuccession` + `SocialClimateFire` plus the biomass output
  extensions (`OutputBiomass`, `OutputBiomassCommunity`,
  `OutputBiomassByAge`, `OutputBiomassReclass`); validated end-to-end on
  both v8 Docker images;
- fixed `OutputBiomassByAge$write()` emitting one `Species` line per
  element (the LANDIS-II parser only accepts one `Species` keyword); the
  species list is now joined with indented continuation lines as the
  format requires;

## landisutils 0.0.5

- added new climate-data backends for use with the LANDIS-II Climate
  Library:
  - daily and monthly weather from BioSIM via the `BioSIM` package
    ([`prep_daily_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md),
    [`prep_monthly_weather_biosim()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md));
  - monthly weather from `climr`
    ([`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)),
    including the bcgov-recommended 8-member GCM ensemble
    (`climr_ensemble_8`);
  - monthly weather from TerraClim via `climateR`
    ([`prep_monthly_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md));
- exported the lower-level fetch and assembly helpers used by the above
  ([`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md),
  [`get_clim_monthly()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly.md),
  [`get_clim_monthly_climr()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly_climr.md),
  [`get_clim_monthly_terraclim()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly_terraclim.md),
  [`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md),
  [`get_elevation_rast()`](https://for-cast.github.io/landisutils/reference/get_elevation_rast.md),
  [`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md),
  and the `assemble_climate_library_file*()` family);
- climate caches are now namespaced by a study-area hash so distinct
  study areas don’t collide;
- added `test_ecoregionPolys` dataset to support examples and tests;
- reworked the `climate-data` vignette to demonstrate the new backends;
- fixed `BiomassSuccession` R6 class name (was `"DynamicFuels"`);
- added `cffdrs` and `digest` to `Imports`;
- added `arrow`, `BioSIM`, and `climr` to `Suggests`.

## landisutils 0.0.4

- improve docker integration tests;

## landisutils 0.0.3

- added support for the remaining LANDIS-II v8 extensions:
  - succession: DGS Succession, NECN Succession, PnET Succession;
  - disturbance: Biomass Browse, Biomass Harvest, Climate BDA, EDA
    (Epidemiological Disturbance Agent), Forest Roads Simulation,
    Hurricane, Land Use Plus, Linear Wind, Magic Harvest, Original Wind,
    Root Rot;
  - output: Output Biomass Community, Output Biomass-PnET, Output
    Biomass Reclass, Output Landscape Habitat, Output Local Habitat,
    Output Wildlife Habitat;
- added `Multi-Extension-Scenarios` vignette;
- miscellaneous updates and fixes to existing extensions.

## landisutils 0.0.2

- cache and batch weather data acquisition by year (#1);
- allow setting cache path using option `landisutils.cache.path`;
- change arguments `start` and `end` in `prep_*_weather()` to be integer
  years;
- use `R6` classes to track simulation input files;
- implemented scenario replication (#3);
- run LANDIS-II in background process via `callr`;

## landisutils 0.0.1

- initial version;
