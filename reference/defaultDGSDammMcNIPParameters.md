# Default DGS Succession DAMM-McNiP scalar parameters

Returns a named list with a default value for each of the required
DAMM-McNiP scalar parameters (`InitialMicrobialC`, `InitialMicrobialN`,
`InitialEnzymeConc`, `ActEnergySOMDepoly`, `ActEnergyDOCUptake`,
`ExpConstSOMDepoly`, `ExpConstDOCUptake`, `FractionSOMUnprotect`,
`CNEnzymes`, `KmSOMDepoly`, `KmDOCUptake`, `EnzTurnRate`,
`MicrobialTurnRate`, `CarbonUseEfficiency`, `PropEnzymeSOM`,
`PropCEnzymeProduction`, `PropNEnzymeProduction`,
`FractDeadMicrobialBiomassSOM`, `MMConstantO2`, `DiffConstantO2`,
`DiffConstantSOMLiquid`, `FractionVolumeO2`, `DOCFraction`,
`DONFraction`, `FractionLitterToDOC`), intended as a starting point for
calibration. Users should tune these values to their study system;
defaults are taken from the upstream LANDIS-II DGS v2.0 single-cell
Alaska test input
(`testing/Core8-DGS_version2.0/DGS_Succession_AKinput_020725.txt`).

## Usage

``` r
defaultDGSDammMcNIPParameters()
```

## Value

Named list of numeric values.

## See also

Other DGS Succession helpers:
[`DGSSuccession`](https://for-cast.github.io/landisutils/reference/DGSSuccession.md),
[`insertDGSFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertDGSFireReductionParameters.md),
[`insertDGSHarvestReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertDGSHarvestReductionParameters.md),
[`prepDGSFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/prepDGSFireReductionParameters.md)
