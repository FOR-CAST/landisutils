# the cell area comes from the scenario's CellLength and must agree with it

    Code
      .resolve_pixel_area_ha(dir, 1)
    Condition
      Error:
      ! pixel_area_ha = 1 disagrees with the scenario's CellLength, which gives 1.44 ha per cell

# observed targets built on a different grid are refused

    Code
      .check_observed_pixel_area(list(pixel_area_ha = 1), dir)
    Condition
      Error:
      ! The observed targets were built on 1 ha cells but the scenario's CellLength gives 1.44 ha; rebuild the targets on the simulation grid

# patch_fire_config() refuses a damage-age multiplier that is negative or too large

    Code
      patch_fire_config(scenario_dir, c(DamageAgeMultiplier = -0.5))
    Condition
      Error:
      ! DamageAgeMultiplier must be a non-negative finite number, not -0.5

---

    Code
      patch_fire_config(scenario_dir, c(DamageAgeMultiplier = 1.5))
    Condition
      Error:
      ! DamageAgeMultiplier of 1.5 scales the fire damage table past 100 % of longevity, which the user guide (2.16.2) does not allow

# a simulated fire in a region the payload does not name is an error

    Code
      loss_from_stats(fx$reps, fx$observed)
    Condition
      Error:
      ! Simulated fires burned in fire region(s) FRT13-hi that the observed payload does not name (it names A, B). The FireSizesTable's EcoName and the payload's zones must be the same zoning.

# an ignition in a zone the table does not name is an error

    Code
      .zone_payload(pts, .zone_table())
    Condition
      Error:
      ! Ignition point(s) lie in zone code(s) 9 that `zones` does not name.

# a zone name must be letters and digits

    Code
      .zone_payload(.zone_points(c(1, 2), c(2001, 2002)), zones)
    Condition
      Error:
      ! Zone names must be letters and digits only, so that they match the fire-region names Dynamic Fire writes to its event log; not: FRT13-hi

# no calibrated zone with fires is an error

    Code
      .zone_payload(.zone_points(c(1, 2), c(2001, 2002)), zones)
    Condition
      Error:
      ! No calibrated zone has an ignition in `fire_years`, so the zones cannot be weighted.

# severity_weighting 'cell' errors when no replicate carries by_class

    Code
      loss_from_stats(list(.sev_fixture()), .sev_observed, weights = c(severity = 1),
      severity_weighting = "cell")
    Condition
      Error:
      ! severity weighting "cell" needs the per-class burned-cell counts, and no replicate carries them. Parse with `by_class = TRUE` (see `parse_options` of run_calibration_validation() and sim_landis()), which must be set BEFORE the run: a replicate is parsed once and then deleted.

# .scenario_template_digest() warns instead of silently returning NULL

    Code
      x <- .scenario_template_digest("/nonexistent/scenario-template")
    Condition
      Warning:
      scenario template '/nonexistent/scenario-template' is neither a directory nor a file; the evaluation fingerprint cannot see the template, so cached losses will survive a template change.

