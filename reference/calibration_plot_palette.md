# Colour roles for the calibration diagnostic plots

Returns the named colours the `plot_calibration_*()` functions use, with
optional per-role overrides. The defaults are checked for colour-vision
separation: `observed`/`simulated`/`alternate` clear the usual
deuteranopia and tritanopia thresholds against each other and against
the plot surface. Replacing them is supported but unchecked.

## Usage

``` r
calibration_plot_palette(...)
```

## Arguments

- ...:

  Named overrides, e.g. `simulated = "red"`. Unknown role names are an
  error, so a typo cannot silently leave the default in place.

## Value

A named character vector of colours, with roles `observed`, `simulated`,
`alternate`, `ink`, `muted` and `grid`.

## See also

Other calibration plots:
[`calibration_events()`](https://for-cast.github.io/landisutils/reference/calibration_events.md),
[`plot_calibration_area_by_fuel()`](https://for-cast.github.io/landisutils/reference/plot_calibration_area_by_fuel.md),
[`plot_calibration_convergence()`](https://for-cast.github.io/landisutils/reference/plot_calibration_convergence.md),
[`plot_calibration_fire_counts()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_counts.md),
[`plot_calibration_fire_sizes()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_sizes.md),
[`plot_calibration_loss()`](https://for-cast.github.io/landisutils/reference/plot_calibration_loss.md),
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md),
[`plot_calibration_severity_by_size()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity_by_size.md)
