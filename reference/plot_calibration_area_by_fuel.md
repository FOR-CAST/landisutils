# Plot observed against simulated area burned by base fuel type

Plotted as shares rather than totals, because the objective's
area-by-fuel term renormalises both sides over the base fuels shared
between them.

## Usage

``` r
plot_calibration_area_by_fuel(stats)
```

## Arguments

- stats:

  A
  [`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md)
  summary carrying `$observed$primary$area_by_fuel_ha`.

## Value

A ggplot object.

## Details

Simulated base fuel types are the `base` column of each replicate's
`area_by_fuel_ha`, which
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md)
takes from the run's own `FuelTypeTable`. A replicate parsed before
landisutils 0.0.168 has no `base` column and is decoded through
[`defaultFuelTypeTable()`](https://for-cast.github.io/landisutils/reference/defaultFuelTypeTable.md),
with a warning, as in
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md).

## See also

Other calibration plots:
[`calibration_events()`](https://for-cast.github.io/landisutils/reference/calibration_events.md),
[`calibration_plot_palette()`](https://for-cast.github.io/landisutils/reference/calibration_plot_palette.md),
[`plot_calibration_convergence()`](https://for-cast.github.io/landisutils/reference/plot_calibration_convergence.md),
[`plot_calibration_fire_counts()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_counts.md),
[`plot_calibration_fire_sizes()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_sizes.md),
[`plot_calibration_loss()`](https://for-cast.github.io/landisutils/reference/plot_calibration_loss.md),
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md),
[`plot_calibration_severity_by_size()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity_by_size.md)
