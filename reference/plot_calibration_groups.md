# Plot observed against simulated spreads, group by group

For each group, an observed and a simulated box side by side
([`calibration_box_stats()`](https://for-cast.github.io/landisutils/reference/calibration_box_stats.md)),
with diamonds at the values the caller compares. Groups are drawn in the
order of `spread$group`'s factor levels, or of first appearance.

## Usage

``` r
plot_calibration_groups(
  spread,
  reference = NULL,
  whiskers = c(0.05, 0.95),
  labels = c(observed = "Observed", simulated = "Simulated")
)
```

## Arguments

- spread:

  Data frame with `group`, `source` (`"observed"` or `"simulated"`) and
  `value`, one row per draw or replicate.

- reference:

  Optional data frame, one row per group: `group`, and any of `observed`
  (the observed value compared), `simulated` (the simulated value
  compared, e.g. pooled over replicates), `lower` and `upper` (an
  interval around the observed value) and `note` (text printed above the
  group).

- whiskers:

  See
  [`calibration_box_stats()`](https://for-cast.github.io/landisutils/reference/calibration_box_stats.md).

- labels:

  Legend labels for the two sources.

## Value

A ggplot object with no title or axis titles.

## Details

A group with no observed values in `spread` (a record with one value and
no resampling) has its observed side drawn from `reference`: a point,
and a whisker-only bar where `lower` and `upper` are given. Observed
values beyond the whiskers are not dotted, since resampling draws are
not observations; simulated replicates beyond them are. A diamond marks
a compared value only on a side drawn as a box. Whether a group passes
any check is the caller's to say, through `note`.

## See also

Other calibration plots:
[`calibration_box_stats()`](https://for-cast.github.io/landisutils/reference/calibration_box_stats.md),
[`calibration_events()`](https://for-cast.github.io/landisutils/reference/calibration_events.md),
[`calibration_plot_palette()`](https://for-cast.github.io/landisutils/reference/calibration_plot_palette.md),
[`plot_calibration_area_by_fuel()`](https://for-cast.github.io/landisutils/reference/plot_calibration_area_by_fuel.md),
[`plot_calibration_convergence()`](https://for-cast.github.io/landisutils/reference/plot_calibration_convergence.md),
[`plot_calibration_fire_counts()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_counts.md),
[`plot_calibration_fire_sizes()`](https://for-cast.github.io/landisutils/reference/plot_calibration_fire_sizes.md),
[`plot_calibration_loss()`](https://for-cast.github.io/landisutils/reference/plot_calibration_loss.md),
[`plot_calibration_replicates()`](https://for-cast.github.io/landisutils/reference/plot_calibration_replicates.md),
[`plot_calibration_severity()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity.md),
[`plot_calibration_severity_by_size()`](https://for-cast.github.io/landisutils/reference/plot_calibration_severity_by_size.md),
[`plot_fire_regime_multipliers()`](https://for-cast.github.io/landisutils/reference/plot_fire_regime_multipliers.md),
[`replicate_percentile()`](https://for-cast.github.io/landisutils/reference/replicate_percentile.md)

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  spread <- data.frame(
    group = rep(c("A", "B"), each = 60),
    source = rep(rep(c("observed", "simulated"), each = 30), 2),
    value = c(seq(0.2, 0.5, length.out = 30), seq(0.25, 0.45, length.out = 30),
              seq(0.1, 0.4, length.out = 30), seq(0, 0.6, length.out = 30))
  )
  plot_calibration_groups(spread, data.frame(group = c("A", "B"), note = c("inside", "inside")))
}
```
