# Plot observed against simulated burn-severity classes

Plot observed against simulated burn-severity classes

## Usage

``` r
plot_calibration_severity(
  stats,
  weight_by = c("events", "area"),
  classes = c(3, 5)
)
```

## Arguments

- stats:

  A
  [`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md)
  summary carrying `$observed$primary$severity_dist`.

- weight_by:

  `"events"` (one vote per fire) or `"area"` (weight each fire by the
  area it burned). See Weighting.

- classes:

  `3` to collapse to low/medium/high before plotting – the three
  categories a thresholded observed reference usually distinguishes – or
  `5` for the raw Dynamic Fire classes.

## Value

A ggplot object.

## Weighting

`weight_by` is the important argument. The objective's severity term
counts fire EVENTS: it bins each event's mean severity and gives every
fire one vote, whatever its size (`weight_by = "events"`, the default,
so the figure matches what was scored). An observed severity reference
is almost always a share of burned AREA – so many pixels or hectares in
each class – and where severity rises with fire size the two summaries
of the same simulation can differ sharply, and can even disagree in sign
about which way the model is off. `weight_by = "area"` weights each
event by the area it burned, which is the like-for-like comparison
against an area-weighted reference.

Both options still summarise each fire by its mean severity, so neither
recovers the within-fire variation that a per-pixel reference retains.

## See also

Other calibration plots:
[`calibration_events()`](https://for-cast.github.io/landisutils/reference/calibration_events.md),
[`calibration_plot_palette()`](https://for-cast.github.io/landisutils/reference/calibration_plot_palette.md),
[`plot_calibration_area_by_fuel()`](https://for-cast.github.io/landisutils/reference/plot_calibration_area_by_fuel.md),
[`plot_calibration_convergence()`](https://for-cast.github.io/landisutils/reference/plot_calibration_convergence.md),
[`plot_calibration_fire_counts()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_counts.md),
[`plot_calibration_fire_sizes()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_sizes.md),
[`plot_calibration_loss()`](https://for-cast.github.io/landisutils/reference/plot_calibration_loss.md),
[`plot_calibration_severity_by_size()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity_by_size.md)
