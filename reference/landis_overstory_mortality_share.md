# Share of burned area that lost its dominant cohort

The proportion of a replicate's burned cells in which the cohort holding
the most biomass was killed. This is the model-side counterpart of a
field burn-severity class defined by mortality of the structurally
dominant vegetation, and unlike the extension's own severity classes –
which are crown fraction burned – it is comparable with an observed
mortality measure. In a forest that burns hot at ground level without
crowning, the two disagree by construction.

## Usage

``` r
landis_overstory_mortality_share(
  rep_dir,
  by_class = FALSE,
  engine_rule = FALSE
)
```

## Arguments

- rep_dir:

  Character. Path to the replicate directory.

- by_class:

  Logical. Also return the burned cells counted by timestep, map code
  and severity class?

- engine_rule:

  Logical. Make the damage table's age test in single precision, as the
  extension does, rather than in double precision? See
  [`fire_cohort_killed()`](https://for-cast.github.io/landisutils/reference/fire_cohort_killed.md).

## Value

A list with `burned_cells`, `high_cells` and `share` (NA when nothing
burned), or NULL when the replicate holds no severity maps, is missing
one of the files above, or has no cohort of a species with both a
tolerance and a longevity. With `by_class = TRUE` the list also holds
`by_class`, a tibble of integer columns `timestep`, `MapCode`, `class`
and `cells`: the burned cells of the severity map of each timestep, by
the cell's map code (NA where the initial-communities map has none) and
severity class (0 for a cell burned with no cohort damaged), in that
order. Its `cells` sum to `burned_cells`, and the `cells` of the rows in
which
[`fire_cohort_killed()`](https://for-cast.github.io/landisutils/reference/fire_cohort_killed.md)
kills the
[`landis_dominant_cohorts()`](https://for-cast.github.io/landisutils/reference/landis_dominant_cohorts.md)
cohort of `MapCode` sum to `high_cells`.

## Details

Everything needed is in the replicate directory, because a run is a copy
of its scenario:

- `fire/severity-{t}.tif` – the severity map per timestep. The Dynamic
  Fire encoding is 0 inactive, 1 active and unburned, 2 burned with no
  cohort damaged, and severity + 2 for a damaged cell, so a cell's
  severity class is its map value MINUS TWO.

- `initial-communities.csv` / `.tif` – the cohorts on each cell.

- the core species file – longevity, which the damage table's age
  thresholds are shares of. Its name is read from `scenario.txt`'s
  `Species` directive, since `prepSpeciesData(type = "core")` names it
  `species-core.txt` while the LANDIS-II examples use `species.txt`; the
  latter is the fallback when there is no directive to read.

- the Dynamic Fire per-species table – each species' fire tolerance. Its
  name is read from `dynamic-fire.txt`'s `Species_CSV_File` directive,
  since `DynamicFire$new()` names it `dynamic-fire-species.csv` while
  this package's own `df_input()` default and the LANDIS-II examples use
  `DynamicFire_Spp_Table.csv`; the latter is the fallback when there is
  no directive to read.

- `dynamic-fire.txt` – the `FireDamageTable`, mapping severity minus
  tolerance to the oldest cohort killed, as a percentage of longevity.

The dominant cohort of each map code is the one
[`landis_dominant_cohorts()`](https://for-cast.github.io/landisutils/reference/landis_dominant_cohorts.md)
returns, and whether it died is decided by
[`fire_cohort_killed()`](https://for-cast.github.io/landisutils/reference/fire_cohort_killed.md):
it dies when severity is 5 (the extension kills everything at that
severity, whatever the tolerance) or when its age is at or below the
percentage of its species' longevity that the damage table gives for
severity minus tolerance. A cell on a map code with no dominant cohort
counts as burned but never as killed. Composition is read at time zero,
so a cell that burns twice in one replicate is scored on its original
cohorts; in a calibration run, where succession is frozen, only reburns
are affected.

## See also

Other Dynamic Fire calibration helpers:
[`apply_calibrated_damage_age()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_damage_age.md),
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
[`apply_calibrated_num_fires()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_num_fires.md),
[`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md),
[`bc_fuel_label_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_label_to_base.md),
[`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md),
[`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md),
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md),
[`calibration_par_names()`](https://for-cast.github.io/landisutils/reference/calibration_par_names.md),
[`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md),
[`default_severity_prior_sturtevant2009()`](https://for-cast.github.io/landisutils/reference/default_severity_prior_sturtevant2009.md),
[`fire_cohort_killed()`](https://for-cast.github.io/landisutils/reference/fire_cohort_killed.md),
[`landis_dominant_cohorts()`](https://for-cast.github.io/landisutils/reference/landis_dominant_cohorts.md),
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md),
[`observed_fire_sizes()`](https://for-cast.github.io/landisutils/reference/observed_fire_sizes.md),
[`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md),
[`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md),
[`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md),
[`run_calibration_validation()`](https://for-cast.github.io/landisutils/reference/run_calibration_validation.md),
[`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md),
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md),
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md),
[`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md)
