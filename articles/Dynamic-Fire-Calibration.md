# Calibrating the Dynamic Fire System Extension

``` r

library(landisutils)
```

## Scope

This vignette covers the **calibration of the LANDIS-II Dynamic Fire
System extension** – adjusting `SeverityCalibrationFactor`, the
per-season FMC `HiProp` values, and per-base-fuel-type `IgnProb`
multipliers so simulated fires match observed regional fire statistics.

It does **not** cover the empirical fits already done at the data layer
(`Mu`, `Sigma`, `Max`, `NumFires`, seasonal `PropFire`) – those come
from NFDB lognormal MLE / count summaries and don’t need an optimisation
loop.

## Function families

The calibration is built from three layers:

| Layer | Purpose | Functions |
|----|----|----|
| Observation | Pre-compute observed targets from NFDB | [`save_observed_fire_targets()`](https://for-cast.github.io/landisutils/reference/save_observed_fire_targets.md) (+ [`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md) default; pass your own if fuel codes differ) |
| Scenario | Build a static-landscape calibration scenario (post-spinup IC; succession frozen; dynamic-fire baseline) | [`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md), [`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md), [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md) |
| Optimiser | Run DEoptim against the calibration scenario, applying patches per trial | [`patch_fire_config()`](https://for-cast.github.io/landisutils/reference/patch_fire_config.md), [`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md), [`parse_dynamic_fire_logs()`](https://for-cast.github.io/landisutils/reference/parse_dynamic_fire_logs.md), [`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md) (or [`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md) for testing), [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md) |

Once calibrated, the best parameter vector flows back to a production
fire config via
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md) +
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md)
and the `SeverityCalibrationFactor` scalar.

## Required project-side inputs

The library is project-agnostic but expects you to provide:

1.  **NFDB-style SpatVectors** clipped to your primary fire regime (and
    optionally a secondary, broader regime for context) – with `YEAR`
    and `SIZE_HA` columns on the points.
2.  **A fuel-type raster** covering your LANDIS simulation extent, with
    a mapping from integer codes to base fuel types (`Conifer`,
    `ConiferPlantation`, `Deciduous`, `Slash`, `Open`). Use
    [`bc_fuel_code_to_base()`](https://for-cast.github.io/landisutils/reference/bc_fuel_code_to_base.md)
    if you’re working with BC `FUEL_TYPE_CD` factor levels.
3.  **A production scenario directory** with ForCS / Dynamic Fuels /
    Dynamic Fire configs already written. The calibration scenario
    template is built by copying this and patching.
4.  **A spun-up IC**: produced by
    [`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md) +
    [`run_calibration_spinup()`](https://for-cast.github.io/landisutils/reference/run_calibration_spinup.md)
    – one LANDIS-II run during pipeline build.

## End-to-end function sketch

The functions are designed to be composed inside a project’s `targets`
pipeline (or any other workflow framework). Below is the typical wiring;
each numbered block is one or more `tar_target()` calls in practice.

``` r

## 1. Observed fire targets (NFDB-derived; cheap; no LANDIS-II).
save_observed_fire_targets(
  primary_points   = nfdb_point_fru59,
  primary_polys    = nfdb_poly_fru59,
  secondary_points = nfdb_point_frt12, ## optional regional context
  secondary_polys  = nfdb_poly_frt12,
  fire_years       = 1950L:2023L,
  fuel_types_rast  = fuel_types_rast,
  path             = "outputs/calibration/observed_fire_targets.rds",
  primary_label    = "FRU59",
  secondary_label  = "FRT12"
  ## fuel_code_to_base defaults to bc_fuel_code_to_base(); override if needed
)

## 2. Pre-calibration spinup: one LANDIS-II run with both ForCS spinup flags ON
##    to populate biomass; snapshot via the Output Biomass Community extension.
##    The snapshot's year-0 CSV+TIF become the calibration IC.
build_calibration_spinup_scenario(
  out_dir      = "LANDIS-II/_calibration_spinup",
  template_dir = "LANDIS-II/phase_2_ICH",          ## any non-fire scenario will do
  duration     = 1L,
  cell_length  = 100L
)
spinup_csv <- run_calibration_spinup(
  scenario_dir = "LANDIS-II/_calibration_spinup",
  base_seed    = 12345L,
  method       = "docker"
)
spinup_tif <- file.path(dirname(spinup_csv), "output-community-0.tif")

## 3. Calibration scenario template: copy from a production fire scenario, swap
##    in the spinup IC, patch ForCS to skip succession, write a fresh baseline
##    dynamic-fire.txt from the supplied tables.
build_calibration_scenario_template(
  out_dir                    = "LANDIS-II/_calibration",
  template_dir               = "LANDIS-II/phase_2_ICH_fire",
  snapshot_ic_csv            = spinup_csv,
  snapshot_ic_tif            = spinup_tif,
  baseline_fire_size_table   = fire_size_table,
  baseline_fuel_type_table   = defaultFuelTypeTable(),
  baseline_fire_damage_table = defaultFireDamageTable(),
  baseline_seasons_sim_table = seasons_sim_table,
  sim_years                  = 10L,
  cell_length                = 100L
)

## 4. DEoptim driver: starts a warm Docker pool, runs the calibration, tears
##    down on exit. Returns best_params (a named numeric of length 9) + the
##    DEoptim result object + a trace CSV.
cfg <- list(
  lower = c(
    SeverityCalibrationFactor = 0.5,
    SpHiProp = 0, SumHiProp = 0, FallHiProp = 0,
    IgnProb_Conifer = 0, IgnProb_ConiferPlantation = 0,
    IgnProb_Deciduous = 0, IgnProb_Slash = 0, IgnProb_Open = 0
  ),
  upper = c(
    SeverityCalibrationFactor = 2.5,
    SpHiProp = 1, SumHiProp = 1, FallHiProp = 1,
    IgnProb_Conifer = 1.5, IgnProb_ConiferPlantation = 1.5,
    IgnProb_Deciduous = 1.5, IgnProb_Slash = 1.5, IgnProb_Open = 1.5
  ),
  NP = 60L, itermax = 100L, n_reps = 5L, sim_years = 10L,
  weights = c(count = 1, size = 1, area_fuel = 0, severity = 0),
  simulator = "landis", method = "docker",
  n_cores = max(1L, parallel::detectCores() - 2L), parallel = TRUE,
  base_seed = 12345L
)
result <- calibrate_dynamic_fire(
  observed_targets_path = "outputs/calibration/observed_fire_targets.rds",
  scenario_template     = "LANDIS-II/_calibration/scenario.txt",
  cfg                   = cfg,
  out_dir               = "outputs/calibration"
)
print(result$best_params)

## 5. Apply calibrated params back to the production fire config (e.g., when
##    writing dynamic-fire.txt for the production scenarios).
calibrated <- result$best_params
production_fuel_type_table <- apply_calibrated_ignprob(
  defaultFuelTypeTable(), calibrated
)
production_fire_size_table <- apply_calibrated_hi_prop(
  fire_size_table, calibrated
)
production_severity_factor <- calibrated[["SeverityCalibrationFactor"]]
```

## Wiring in `targets`

The library doesn’t ship a target factory (deliberate – target naming
conventions vary across consuming projects). Adopt the helpers as plain
function calls inside `tar_target()`. Three things to watch for:

- **Cycle avoidance.** If your production `tar_target` that writes
  `dynamic-fire.txt` consumes `calibrated_fire_params` (the result of
  [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)),
  then the calibration’s scenario template must NOT transitively depend
  on that target. Use
  [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md)’s
  `baseline_*` arguments to write a fresh un-calibrated dynamic-fire.txt
  inline, and depend on the per-extension *file* targets that populate
  your production scenario directory directly, NOT on your
  scenario-assembly target.

- **Deployment mode.** The calibration target should run on the main
  process (`deployment = "main"` in `targets`), not on a `crew` worker,
  because the function internally manages its own FORK cluster of
  `n_cores` workers + a warm Docker pool of `n_cores` containers. A crew
  worker trying to spawn its own cluster would compete for cores.

- **DEoptim is a Suggest.** Install via `renv::install("DEoptim")`
  before running the calibration target.

## Per-trial cost and lever knobs

[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md)
per-trial cost is dominated by the LANDIS-II Docker run (~1-3 minutes
including dotnet startup). Total wall-clock for production calibration:
roughly `NP * itermax * n_reps * per-trial / n_cores`. The warm Docker
pool (started internally by
[`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md))
saves container-creation overhead but not dotnet startup, so the
dominant cost remains the LANDIS-II simulation itself. Practical levers
if calibration takes too long:

- Cut `sim_years` to 5 (instead of 10): roughly half the per-trial cost.
- Aggregate `fuel_types_rast` to a coarser resolution (e.g., 200 m
  cells) before passing to
  [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md):
  ~4x speedup.
- Reduce `NP` × `itermax`; DEoptim’s evolutionary search compensates
  somewhat for fewer evaluations.

A pure-R reimplementation
([`sim_r_reimpl()`](https://for-cast.github.io/landisutils/reference/sim_r_reimpl.md),
currently a stub) would remove the dotnet bottleneck entirely but
introduces its own maintenance burden tracking LANDIS-II’s evolving
algorithm. See the design notes in `_tmp_dynamic_fire_calibration.md` of
the gitanyow-partial-harvest project.

## Testing without Docker

[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md)
returns plausibly-shaped per-trial output without invoking LANDIS-II,
suitable for end-to-end testing of the DEoptim driver’s control flow:

``` r

cfg$simulator <- "mock"
cfg$NP <- 6L
cfg$itermax <- 2L
cfg$parallel <- FALSE
calibrate_dynamic_fire(
  observed_targets_path = "outputs/calibration/observed_fire_targets.rds",
  scenario_template     = "LANDIS-II/_calibration/scenario.txt",
  cfg                   = cfg,
  out_dir               = tempdir()
)
```

The mock varies its output with `par_vec` so DEoptim sees a non-trivial
loss surface (good for catching driver-wiring regressions). It is NOT a
substitute for the real LANDIS-II simulator – the parameter optima under
[`sim_mock()`](https://for-cast.github.io/landisutils/reference/sim_mock.md)
are not meaningful.

## See also

- [`vignette("Dynamic-Fire-Fuels", package = "landisutils")`](https://for-cast.github.io/landisutils/articles/Dynamic-Fire-Fuels.md)
  – preparing the Dynamic Fire and Dynamic Fuels extension inputs in the
  first place.
- [`?DynamicFire`](https://for-cast.github.io/landisutils/reference/DynamicFire.md)
  – the R6 class that backs the un-calibrated baseline config writer
  used inside
  [`build_calibration_scenario_template()`](https://for-cast.github.io/landisutils/reference/build_calibration_scenario_template.md).
- [`?landis_pool_start`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md)
  – the warm Docker pool that
  [`calibrate_dynamic_fire()`](https://for-cast.github.io/landisutils/reference/calibrate_dynamic_fire.md)
  starts internally; also usable standalone.
