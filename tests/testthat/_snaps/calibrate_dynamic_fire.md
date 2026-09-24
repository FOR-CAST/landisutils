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

# .scenario_template_digest() warns instead of silently returning NULL

    Code
      x <- .scenario_template_digest("/nonexistent/scenario-template")
    Condition
      Warning:
      scenario template '/nonexistent/scenario-template' is neither a directory nor a file; the evaluation fingerprint cannot see the template, so cached losses will survive a template change.

