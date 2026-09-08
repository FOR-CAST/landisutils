# Plot simulated fire size against fire severity

Each simulated fire as a point, with the mean severity of each size
class drawn over it. Where the line rises, the severity distribution
depends on how the fires are weighted, and
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md)'s
`weight_by` argument stops being cosmetic: a size class holding a small
share of the fires can hold most of the area.

## Usage

``` r
plot_calibration_severity_by_size(
  stats,
  breaks = c(0, 10, 100, 1000, 10000, Inf)
)
```

## Arguments

- stats:

  A
  [`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md)
  summary.

- breaks:

  Size-class boundaries in hectares, passed to
  [`base::cut()`](https://rdrr.io/r/base/cut.html). The default spans
  four orders of magnitude.

## Value

A ggplot object. Its `size_summary` attribute holds the per-class share
of fires, share of area, and mean severity.

## See also

Other calibration plots:
[`calibration_events()`](https://for-cast.github.io/landisutils/reference/calibration_events.md),
[`calibration_plot_palette()`](https://for-cast.github.io/landisutils/reference/calibration_plot_palette.md),
[`plot_calibration_area_by_fuel()`](https://for-cast.github.io/landisutils/reference/plot_calibration_area_by_fuel.md),
[`plot_calibration_convergence()`](https://for-cast.github.io/landisutils/reference/plot_calibration_convergence.md),
[`plot_calibration_fire_counts()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_counts.md),
[`plot_calibration_fire_sizes()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_sizes.md),
[`plot_calibration_loss()`](https://for-cast.github.io/landisutils/reference/plot_calibration_loss.md),
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md)
