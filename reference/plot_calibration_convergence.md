# Plot the DEoptim search trace

Draws the best objective value reached by each generation. When `stats`
is supplied, the loss it records is added as a point at the last
generation: the search reports the minimum of many noisy evaluations and
so is optimistic about its own winner, and re-simulating that winner at
a higher replicate count is the honest measurement. The gap between line
and point is that optimism.

## Usage

``` r
plot_calibration_convergence(trace, stats = NULL)
```

## Arguments

- trace:

  The DEoptim trace: either a path to the CSV
  [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  writes (columns generation and best value) or a data frame with those
  two columns in that order.

- stats:

  Optional
  [`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md)
  summary, used for the re-checked loss point and the replicate count in
  its label.

## Value

A ggplot object.

## See also

Other calibration plots:
[`calibration_events()`](https://for-cast.github.io/landisutils/reference/calibration_events.md),
[`calibration_plot_palette()`](https://for-cast.github.io/landisutils/reference/calibration_plot_palette.md),
[`plot_calibration_area_by_fuel()`](https://for-cast.github.io/landisutils/reference/plot_calibration_area_by_fuel.md),
[`plot_calibration_fire_counts()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_counts.md),
[`plot_calibration_fire_sizes()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_sizes.md),
[`plot_calibration_loss()`](https://for-cast.github.io/landisutils/reference/plot_calibration_loss.md),
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md),
[`plot_calibration_severity_by_size()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity_by_size.md)
