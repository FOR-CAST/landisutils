# Plot observed against simulated fire sizes

Empirical cumulative distributions on a log size axis. Both sides are
truncated at the observed `min_size_ha` floor when the summary records
one, matching the truncation the objective's size term applies.

## Usage

``` r
plot_calibration_fire_sizes(stats)
```

## Arguments

- stats:

  A
  [`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md)
  summary.

## Value

A ggplot object.

## See also

Other calibration plots:
[`calibration_events()`](https://for-cast.github.io/landisutils/reference/calibration_events.md),
[`calibration_plot_palette()`](https://for-cast.github.io/landisutils/reference/calibration_plot_palette.md),
[`plot_calibration_area_by_fuel()`](https://for-cast.github.io/landisutils/reference/plot_calibration_area_by_fuel.md),
[`plot_calibration_convergence()`](https://for-cast.github.io/landisutils/reference/plot_calibration_convergence.md),
[`plot_calibration_fire_counts()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_counts.md),
[`plot_calibration_loss()`](https://for-cast.github.io/landisutils/reference/plot_calibration_loss.md),
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md),
[`plot_calibration_severity_by_size()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity_by_size.md)
