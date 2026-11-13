# Plot simulated replicates against reference values

One box of per-replicate values
([`calibration_box_stats()`](https://for-cast.github.io/landisutils/reference/calibration_box_stats.md):
quartiles and median, whiskers at `whiskers`, the replicates beyond them
as dots), with each reference beside it in a column of its own, labelled
with its value and its
[`replicate_percentile()`](https://for-cast.github.io/landisutils/reference/replicate_percentile.md)
among the replicates. Dashed lines at the whiskers' ends run across the
panel, so each reference reads against them.

## Usage

``` r
plot_calibration_replicates(
  values,
  references = NULL,
  whiskers = c(0.05, 0.95),
  label = NULL,
  name = "Simulated"
)
```

## Arguments

- values:

  Numeric, one value per replicate.

- references:

  Optional data frame with `name` and `value`, one row per reference
  value (a recorded value, a target), in the order to draw them.

- whiskers:

  See
  [`calibration_box_stats()`](https://for-cast.github.io/landisutils/reference/calibration_box_stats.md).

- label:

  Optional function turning a value into its label text; the default
  prints three significant figures. Units are the caller's.

- name:

  The label of the replicates' column.

## Value

A ggplot object with no title or axis titles.

## Details

Whether the whiskers are an acceptance range or only a spread is the
caller's to say: a calibration that scores pooled statistics, not
replicates, should say the box is a spread.

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
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md),
[`plot_calibration_severity_by_size()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity_by_size.md),
[`plot_fire_regime_multipliers()`](https://for-cast.github.io/landisutils/reference/plot_fire_regime_multipliers.md),
[`replicate_percentile()`](https://for-cast.github.io/landisutils/reference/replicate_percentile.md)

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  plot_calibration_replicates(
    c(3, 5, 6, 8, 9, 10, 11, 12, 14, 16, 20, 26, 39),
    data.frame(name = c("Record", "Target"), value = c(15.3, 13.5))
  )
}
```
