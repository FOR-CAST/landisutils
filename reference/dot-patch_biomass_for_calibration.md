# Freeze Biomass Succession for the calibration scenario (the Biomass-Succession analog of [`.patch_forcs_for_calibration()`](https://for-cast.github.io/landisutils/reference/dot-patch_forcs_for_calibration.md)).

Biomass Succession has no `SpinUp` section and (unlike ForCS) no
`Soil.cs` DisturbFireFromBiomassPools path, so there is no DOM-spinup /
NullReferenceException workaround to apply. The only requirement is that
succession does NOT change the (spun-up) fuel landscape during the short
calibration sims, so the fire behaviour reflects the candidate Dynamic
Fire parameters rather than vegetation change. We freeze it by setting
the succession `Timestep` greater than the calibration `sim_years`: the
extension initialises but its first scheduled succession event falls
beyond the run Duration, so it never executes and the
initial-communities biomass is held static.

## Usage

``` r
.patch_biomass_for_calibration(path, sim_years)
```
