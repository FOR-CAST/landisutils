# Mid-rank percentile of a value among simulated replicates

`100 * (mean(sim < x) + 0.5 * mean(sim == x))`: the share of replicates
below `x`, counting ties as half. Pass both sides on the scale a check
uses; a value compared with rounded totals should be rounded the same
way first, so a tie is a tie.

## Usage

``` r
replicate_percentile(sim, x)
```

## Arguments

- sim:

  Numeric, one value per replicate. Non-finite values are dropped.

- x:

  Numeric, the value(s) to place.

## Value

Numeric, one percentile per element of `x`, or `NA` where `x` is not
finite or `sim` holds no finite value.

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
[`plot_fire_regime_multipliers()`](https://for-cast.github.io/landisutils/reference/plot_fire_regime_multipliers.md)

## Examples

``` r
replicate_percentile(1:10, c(0.5, 5, 5.5, 11))
#> [1]   0  45  50 100
```
