# Box-and-whisker statistics with whiskers at chosen quantiles

The quartiles and median for the box, and whiskers at the `whiskers`
quantiles rather than at 1.5 times the interquartile range, so that a
whisker can mark the range a calibration check uses (its 5th and 95th
percentiles, say).

## Usage

``` r
calibration_box_stats(x, whiskers = c(0.05, 0.95))
```

## Arguments

- x:

  Numeric. Non-finite values are dropped.

- whiskers:

  The quantiles the whiskers reach: two probabilities, the lower at most
  0.25 and the upper at least 0.75.

## Value

A list: `stats`, a one-row data frame with `ymin`, `lower`, `middle`,
`upper` and `ymax` (the names `ggplot2::geom_boxplot(stat = "identity")`
takes), and `out`, the values beyond the whiskers.

## Details

All five values are
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) type 7:
INTERPOLATED quantiles, not order statistics. With 40 values the 5th
percentile falls between the second and third smallest, so a check
defined as "the second lowest replicate" will not match the lower
whisker exactly.

## See also

Other calibration plots:
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
[`plot_fire_regime_multipliers()`](https://for-cast.github.io/landisutils/reference/plot_fire_regime_multipliers.md),
[`replicate_percentile()`](https://for-cast.github.io/landisutils/reference/replicate_percentile.md)

## Examples

``` r
calibration_box_stats(1:20, whiskers = c(0.1, 0.9))
#> $stats
#>   ymin lower middle upper ymax
#> 1  2.9  5.75   10.5 15.25 18.1
#> 
#> $out
#> [1]  1  2 19 20
#> 
```
