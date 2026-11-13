# Plot projected fire-regime multipliers against a baseline

One panel per quantity (fire count, mean fire size and area burned,
say), each on a log scale of multipliers of a baseline regime, which is
the dashed rule at 1. The x axis is a free grouping column:

## Usage

``` r
plot_fire_regime_multipliers(
  factors,
  x,
  series,
  x_end = NULL,
  panel = NULL,
  colours = NULL,
  baseline = 1
)
```

## Arguments

- factors:

  Data frame, one row per value: the columns named by `x`, `series` and
  (optionally) `x_end` and `panel`, plus `quantity`, `estimate` and,
  optionally, `lower` and `upper`. `estimate` must be positive (log
  scale). Quantities are drawn in the order of `quantity`'s factor
  levels, or of first appearance.

- x, series:

  Names of the x and colour columns.

- x_end:

  Optional name of the column where each value's step ends.

- panel:

  Optional name of a second facet column.

- colours:

  Optional named colours, one per series. Without them up to three
  series take the calibration palette's `simulated`, `observed` and
  `alternate` roles; more need `colours`.

- baseline:

  The multiplier of the baseline regime, drawn as the dashed rule.

## Value

A ggplot object with no title or axis titles.

## Details

- numeric, with `x_end` given (climate periods stepped through one run):
  each value holds from `x` to `x_end`, drawn as a step, with `lower` to
  `upper` shaded, and the last value labelled at the end of its step;

- anything else (stationary climate envelopes, say): a point per series
  at each category, with `lower` to `upper` as a bar.

With `panel`, the facets are `quantity ~ panel` (a fire zone, say). The
facet is an ordinary
[`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
or
[`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html):
adding another facet to the returned plot replaces it.

The plot draws what it is given. Whether the multipliers are a design's
projected means or simulated values, and how they were derived, is for
the caller's title and caption to say.

## See also

Other calibration plots:
[`calibration_box_stats()`](https://for-cast.github.io/landisutils/reference/calibration_box_stats.md),
[`calibration_events()`](https://for-cast.github.io/landisutils/reference/calibration_events.md),
[`calibration_plot_palette()`](https://for-cast.github.io/landisutils/reference/calibration_plot_palette.md),
[`plot_calibration_area_by_fuel()`](https://for-cast.github.io/landisutils/reference/plot_calibration_area_by_fuel.md),
[`plot_calibration_convergence()`](https://for-cast.github.io/landisutils/reference/plot_calibration_convergence.md),
[`plot_calibration_fire_counts()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_counts.md),
[`plot_calibration_fire_sizes()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_sizes.md),
[`plot_calibration_groups()`](https://for-cast.github.io/landisutils/reference/plot_calibration_groups.md),
[`plot_calibration_loss()`](https://for-cast.github.io/landisutils/reference/plot_calibration_loss.md),
[`plot_calibration_replicates()`](https://for-cast.github.io/landisutils/reference/plot_calibration_replicates.md),
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md),
[`plot_calibration_severity_by_size()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity_by_size.md),
[`replicate_percentile()`](https://for-cast.github.io/landisutils/reference/replicate_percentile.md)

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  f <- expand.grid(
    start = c(2026, 2041, 2071),
    series = c("A", "B"),
    quantity = c("count", "size", "area"),
    stringsAsFactors = FALSE
  )
  f$end <- c(2041, 2071, 2101)[match(f$start, c(2026, 2041, 2071))]
  f$estimate <- 1 + 0.2 * match(f$start, unique(f$start)) * match(f$series, c("A", "B"))
  plot_fire_regime_multipliers(f, x = "start", series = "series", x_end = "end")
}
```
