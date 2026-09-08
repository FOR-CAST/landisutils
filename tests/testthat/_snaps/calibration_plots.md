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

