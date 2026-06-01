# Patch a ForC Succession config for calibration use (internal)

Two surgical replacements in `forc-succession.txt`:

- `Timestep N` -\> `Timestep <sim_years + 1>`. Makes ForCS skip its
  succession step for the duration of the calibration run (no growth, no
  establishment, no mortality) so each DEoptim trial is purely a
  fire-on-fixed-landscape experiment.

- `SpinUp` data row -\> `0 0 1 20`. The calibration IC is already
  post-spinup (from the snapshot of
  [`build_calibration_spinup_scenario()`](https://for-cast.github.io/landisutils/reference/build_calibration_spinup_scenario.md)),
  so per-trial spinup would be wasted compute. DOM equilibration is also
  skipped – fire severity depends on cohort biomass, not DOM.

## Usage

``` r
.patch_forcs_for_calibration(path, sim_years)
```

## Arguments

- path:

  Character. Absolute path to `forc-succession.txt`.

- sim_years:

  Integer. Calibration sim duration (years).

## Value

The patched lines, invisibly (also written to `path`).

## Details

Patches the file in place. Caller is expected to pass the calibration
scenario's own copy of `forc-succession.txt` (not the production one).
