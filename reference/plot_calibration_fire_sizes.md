# Plot observed against simulated fire sizes

Empirical cumulative distributions on a log size axis, or exceedance
curves (the share of fires at least each size) on log-log axes. Both
sides are truncated at the observed `min_size_ha` floor when the summary
records one, matching the truncation the objective's size term applies.

## Usage

``` r
plot_calibration_fire_sizes(
  stats,
  scale = c("ecdf", "exceedance"),
  by_zone = FALSE
)
```

## Arguments

- stats:

  A
  [`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md)
  summary.

- scale:

  `"ecdf"` (cumulative share, the default) or `"exceedance"`.

- by_zone:

  Logical. One panel per fire zone, from the payload's `$zones` (see
  [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md));
  a simulated fire belongs to the fire region its event log names. Zones
  with no weight in the objective are labelled "(not calibrated)".

## Value

A ggplot object.

## Details

The exceedance scale is the one that shows the large-fire end: on a
cumulative axis the top few percent of fires, which carry most of the
area burned, are squeezed against 1. It marks the observed 95th and 99th
percentiles as dashed lines.

## See also

Other calibration plots:
[`calibration_box_stats()`](https://for-cast.github.io/landisutils/reference/calibration_box_stats.md),
[`calibration_events()`](https://for-cast.github.io/landisutils/reference/calibration_events.md),
[`calibration_plot_palette()`](https://for-cast.github.io/landisutils/reference/calibration_plot_palette.md),
[`plot_calibration_area_by_fuel()`](https://for-cast.github.io/landisutils/reference/plot_calibration_area_by_fuel.md),
[`plot_calibration_convergence()`](https://for-cast.github.io/landisutils/reference/plot_calibration_convergence.md),
[`plot_calibration_fire_counts()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_counts.md),
[`plot_calibration_groups()`](https://for-cast.github.io/landisutils/reference/plot_calibration_groups.md),
[`plot_calibration_loss()`](https://for-cast.github.io/landisutils/reference/plot_calibration_loss.md),
[`plot_calibration_replicates()`](https://for-cast.github.io/landisutils/reference/plot_calibration_replicates.md),
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md),
[`plot_calibration_severity_by_size()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity_by_size.md),
[`plot_fire_regime_multipliers()`](https://for-cast.github.io/landisutils/reference/plot_fire_regime_multipliers.md),
[`replicate_percentile()`](https://for-cast.github.io/landisutils/reference/replicate_percentile.md)
