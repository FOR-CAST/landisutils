# calibration_plot_palette() rejects unknown and unnamed overrides

    Code
      calibration_plot_palette(nope = "red")
    Condition
      Error:
      ! unknown palette role(s): nope. Known roles: observed, simulated, alternate, ink, muted, grid.

---

    Code
      calibration_plot_palette("red")
    Condition
      Error:
      ! overrides must be named, e.g. calibration_plot_palette(simulated = 'red').

# plot_calibration_convergence() errors on a missing file

    Code
      plot_calibration_convergence("no-such-trace.csv")
    Condition
      Error:
      ! trace file not found: no-such-trace.csv

# plot_calibration_severity() honours the class count

    Code
      plot_calibration_severity(stats, classes = 4)
    Condition
      Error:
      ! `classes` must be 3 or 5.

# weight_by = 'cells' needs replicates parsed with their per-class counts

    Code
      plot_calibration_severity(make_calibration_stats(), "cells")
    Condition
      Error:
      ! `weight_by = "cells"` needs replicates parsed with `by_class = TRUE`; these carry no per-class cell counts.

# by_zone needs a payload with zones

    Code
      plot_calibration_fire_sizes(make_calibration_stats(), by_zone = TRUE)
    Condition
      Error:
      ! `by_zone = TRUE` needs an observed payload with zones (save_observed_fire_targets(zones = )).

# malformed inputs are rejected with a useful message

    Code
      plot_calibration_loss(list())
    Condition
      Error:
      ! `stats` must carry a `$loss` element.

---

    Code
      plot_calibration_fire_sizes(list(reps = list()))
    Condition
      Error:
      ! `stats` must be a run_calibration_validation() summary (a list with `reps` and `observed`).

---

    Code
      plot_calibration_severity(stats)
    Condition
      Error:
      ! `stats$observed$primary` is missing: severity_dist.

# loss_from_stats() refuses a missing lambda_obs instead of misaligning components

    Code
      loss_from_stats(stats$reps, stats$observed)
    Condition
      Error:
      ! `observed$primary$lambda_obs` must be a single finite number; loss_from_stats() cannot score the count component without it.

# loss_from_stats() refuses weights that would silently misalign

    Code
      loss_from_stats(stats$reps, stats$observed, weights = NULL)
    Condition
      Error:
      ! `weights` must be a non-empty, fully named, non-NA numeric vector over: count, size, size_tail, area_fuel, severity, mortality, area_burned, size_p99.

---

    Code
      loss_from_stats(stats$reps, stats$observed, weights = c(1, 1))
    Condition
      Error:
      ! `weights` must be a non-empty, fully named, non-NA numeric vector over: count, size, size_tail, area_fuel, severity, mortality, area_burned, size_p99.

---

    Code
      loss_from_stats(stats$reps, stats$observed, weights = c(cnt = 2, size = 1))
    Condition
      Error:
      ! unknown loss weight(s): cnt. Known components: count, size, size_tail, area_fuel, severity, mortality, area_burned, size_p99.

---

    Code
      loss_from_stats(stats$reps, stats$observed, weights = c(count = NA_real_))
    Condition
      Error:
      ! `weights` must be a non-empty, fully named, non-NA numeric vector over: count, size, size_tail, area_fuel, severity, mortality, area_burned, size_p99.

