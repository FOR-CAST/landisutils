# Prepare PnET-Succession Generic Parameters file

Writes a `PnETGenericParameters`-format text file (§7 of the user
guide). Any parameter omitted falls back to the defaults provided in
`PnETGenericDefaultParameters.txt` shipped with the extension. Allowed
parameter names: `MaxCanopyLayers`, `LayerThreshRatio`, `PARunits`,
`IMAX`, `DVPD1`, `DVPD2`, `BFolResp`, `MaintResp`, `TOroot`, `TOwood`,
`Q10`, `FolLignin`, `KWdLit`, `InitialNSC`, `CFracBiomass`,
`PrecipEvents`, `PrecipEventsWithReplacement`, `ETExtCoeff`,
`RETCropCoeff`, `PreventEstablishment`, `Wythers`, `DTEMP`, `MaxPest`,
`AmaxFrac`, `InvertPest`, `SoilIceDepth`, `LeakageFrostDepth`,
`FrostFactor`, `InitialCommunitiesSpinup`, `SpinupWaterStress`,
`Parallel`, `CohortStacking`, `CanopySumScale`, `CO2HalfSatEff`.

## Usage

``` r
prepPnETGenericParameters(
  params = NULL,
  path,
  filename = "PnETGenericParameters.txt"
)
```

## Arguments

- params:

  Named list (or two-column `data.frame` with columns `name` and
  `value`) of generic-parameter settings. Names must be drawn from the
  allowed parameter list above. Boolean values are coerced via
  [`yesno()`](https://for-cast.github.io/landisutils/reference/yesno.md).

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character, specifying a filename (will be appended to `path`).

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.

## See also

Other PnET Succession helpers:
[`PnETSuccession`](https://for-cast.github.io/landisutils/reference/PnETSuccession.md),
[`defaultPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/defaultPnETDisturbanceReductions.md),
[`prepPNEToutputsites()`](https://for-cast.github.io/landisutils/reference/prepPNEToutputsites.md),
[`prepPnETClimateFile()`](https://for-cast.github.io/landisutils/reference/prepPnETClimateFile.md),
[`prepPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/prepPnETDisturbanceReductions.md),
[`prepPnETEcoregionParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETEcoregionParameters.md),
[`prepPnETSpeciesParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETSpeciesParameters.md)
