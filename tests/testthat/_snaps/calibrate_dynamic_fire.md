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

