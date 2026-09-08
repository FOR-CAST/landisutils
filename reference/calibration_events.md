# Pool the per-replicate fire-event logs

Binds every replicate's event table into one data frame and adds the
burned area of each event in hectares. One row per simulated fire event.

## Usage

``` r
calibration_events(stats)
```

## Arguments

- stats:

  A
  [`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md)
  summary.

## Value

A data frame with `year`, `eco`, `init_fuel`, `sites`, `mean_severity`
and `area_ha`.

## Details

`mean_severity` is the value Dynamic Fire writes to its event log: the
mean severity over all of the event's sites, which is why it can fall
below 1. Note that it and `sites` (the log's `DamagedSites`) are not on
the same denominator.

## See also

Other calibration plots:
[`calibration_plot_palette()`](https://for-cast.github.io/landisutils/reference/calibration_plot_palette.md),
[`plot_calibration_area_by_fuel()`](https://for-cast.github.io/landisutils/reference/plot_calibration_area_by_fuel.md),
[`plot_calibration_convergence()`](https://for-cast.github.io/landisutils/reference/plot_calibration_convergence.md),
[`plot_calibration_fire_counts()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_counts.md),
[`plot_calibration_fire_sizes()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_sizes.md),
[`plot_calibration_loss()`](https://for-cast.github.io/landisutils/reference/plot_calibration_loss.md),
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md),
[`plot_calibration_severity_by_size()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity_by_size.md)
