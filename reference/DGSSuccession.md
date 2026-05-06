# DGS (DAMM-McNiP, GIPL, SHAW) Succession Extension

R6 class representing the LANDIS-II DGS Succession (v2.0) extension's
main input file. DGS integrates a NECN-style vegetation dynamics model
with the DAMM-McNiP soil C/N model, the SHAW physically-based hydrology
/ energy balance model, and the GIPL deep-soil permafrost model.

The DGS extension must be run with Social Climate Fire (v4.01) and the
Output Reclass extension (v4.0).

## References

LANDIS-II DGS Succession v2.0 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-DGS-Succession/blob/master/docs/LANDIS-II%20DGS%20Succession%20v2.0%20User%20Guide.pdf>

## See also

Helpers that prepare inputs for this extension:
[`prepDGSFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/prepDGSFireReductionParameters.md).
Shared scenario inputs:
[`prepClimateConfig()`](https://for-cast.github.io/landisutils/reference/prepClimateConfig.md),
[`prepInitialCommunities()`](https://for-cast.github.io/landisutils/reference/prepInitialCommunities.md),
[`prepSpeciesData()`](https://for-cast.github.io/landisutils/reference/prepSpeciesData.md).

Other DGS Succession helpers:
[`defaultDGSDammMcNIPParameters()`](https://for-cast.github.io/landisutils/reference/defaultDGSDammMcNIPParameters.md),
[`insertDGSFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertDGSFireReductionParameters.md),
[`insertDGSHarvestReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertDGSHarvestReductionParameters.md),
[`prepDGSFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/prepDGSFireReductionParameters.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `DGSSuccession`

## Active bindings

- `CalibrateMode`:

  Logical, or character indicating "yes" or "no".

- `ClimateConfigFile`:

  Character. Relative file path.

- `AtmosphericNSlope`:

  Numeric.

- `AtmosphericNIntercept`:

  Numeric.

- `InitialCommunities`:

  Character. Relative file path.

- `InitialCommunitiesMap`:

  Character. Relative file path.

- `Latitude`:

  Numeric (degrees).

- `ShawGiplConfigFile`:

  Character. Relative file path.

- `ShawGiplFiles`:

  Character vector of relative file paths.

- `SoilMaps`:

  Named list of relative file paths. Required keys: `SoilDepthMapName`,
  `SoilDrainMapName`, `SoilBaseFlowMapName`, `SoilStormFlowMapName`,
  `SoilFieldCapacityMapName`, `SoilWiltingPointMapName`,
  `SoilPercentSandMapName`, `SoilPercentClayMapName`,
  `SoilBulkDensityMapName`, `SoilParticleDensityMapName`,
  `InitialSOC_PrimaryMapName`, `InitialSON_PrimaryMapName`,
  `InitialDeadWoodSurfaceMapName`, `InitialDeadCoarseRootsMapName`.

- `InitialFineFuels`:

  Numeric in `[0, 1]`.

- `InitialMineralN`:

  Numeric (g m-2).

- `DenitrificationRate`:

  Numeric in `[0, 1]`.

- `WaterDecayFunction`:

  Character. One of `"Linear"` or `"Ratio"`.

- `DammMcNIPParameters`:

  Named list of numeric scalar parameters. Required keys:
  `InitialMicrobialC`, `InitialMicrobialN`, `InitialEnzymeConc`,
  `ActEnergySOMDepoly`, `ActEnergyDOCUptake`, `ExpConstSOMDepoly`,
  `ExpConstDOCUptake`, `FractionSOMUnprotect`, `CNEnzymes`,
  `KmSOMDepoly`, `KmDOCUptake`, `EnzTurnRate`, `MicrobialTurnRate`,
  `CarbonUseEfficiency`, `PropEnzymeSOM`, `PropCEnzymeProduction`,
  `PropNEnzymeProduction`, `FractDeadMicrobialBiomassSOM`,
  `MMConstantO2`, `DiffConstantO2`, `DiffConstantSOMLiquid`,
  `FractionVolumeO2`, `DOCFraction`, `DONFraction`,
  `FractionLitterToDOC`.

- `SeedingAlgorithm`:

  Character. Dispersal algorithm to use. One of `"WardSeedDispersal"`,
  `"NoDispersal"`, `"UniversalDispersal"`.

- `ProbabilityEstablishAdjust`:

  Numeric. Default `1.0`.

- `SpeciesParameters`:

  Character. Relative file path.

- `FireReductionParameters`:

  `data.frame`.

- `HarvestReductionParameters`:

  `data.frame`.

## Methods

### Public methods

- [`DGSSuccession$new()`](#method-DGSSuccession-initialize)

- [`DGSSuccession$write()`](#method-DGSSuccession-write)

- [`DGSSuccession$clone()`](#method-DGSSuccession-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `DGSSuccession$new()`

#### Usage

    DGSSuccession$new(
      path,
      Timestep = 1L,
      CalibrateMode = NULL,
      ClimateConfigFile = NULL,
      AtmosphericNSlope = NULL,
      AtmosphericNIntercept = NULL,
      InitialCommunities = NULL,
      InitialCommunitiesMap = NULL,
      Latitude = NULL,
      ShawGiplConfigFile = NULL,
      ShawGiplFiles = NULL,
      SoilMaps = NULL,
      InitialFineFuels = NULL,
      InitialMineralN = NULL,
      DenitrificationRate = NULL,
      WaterDecayFunction = NULL,
      DammMcNIPParameters = NULL,
      SeedingAlgorithm = NULL,
      ProbabilityEstablishAdjust = 1,
      SpeciesParameters = NULL,
      FireReductionParameters = NULL,
      HarvestReductionParameters = NULL
    )

#### Arguments

- `path`:

  Character. Directory path for the extension input files.

- `Timestep`:

  Integer (years).

- `CalibrateMode`:

  Logical, or character indicating "yes" or "no". When `TRUE`, an
  additional `DGS-calibrate-log.csv` is produced; intended for
  single-cell runs only.

- `ClimateConfigFile`:

  Character. Relative file path to the climate library configuration
  file (see §2.4 of the user guide).

- `AtmosphericNSlope`:

  Numeric. Slope for the linear N-deposition model
  (`Total N = AtmosphericNSlope * precipitation + AtmosphericNIntercept`).
  The user-guide prose refers to this as `AtmosNslope`, but the parser
  keyword is `AtmosphericNSlope`.

- `AtmosphericNIntercept`:

  Numeric. Intercept for the linear N-deposition model. The user-guide
  prose calls this `AtmosNinter`, but the parser keyword is
  `AtmosphericNIntercept`.

- `InitialCommunities`:

  Character. Relative file path to the initial communities text file
  (see §7 of the user guide).

- `InitialCommunitiesMap`:

  Character. Relative file path to the initial communities raster.

- `Latitude`:

  Numeric. Study-site latitude (degrees).

- `ShawGiplConfigFile`:

  Character. Relative file path to the SHAW/GIPL configuration file that
  names the SHAW/GIPL input files (see §3 of the user guide).

- `ShawGiplFiles`:

  (Optional) Character vector of additional SHAW/GIPL input files (e.g.,
  `ListThus`, `ShawGeneralInputs`, `ShawPlantTypes`, `ShawSoilTypes`,
  `GiplProperties`, `Unfrozen.txt`) to register so the scenario can
  collect them.

- `SoilMaps`:

  Named list of relative file paths for the required soil physical and
  initial soil C/N / dead-wood map inputs. Required keys:
  `SoilDepthMapName`, `SoilDrainMapName`, `SoilBaseFlowMapName`,
  `SoilStormFlowMapName`, `SoilFieldCapacityMapName`,
  `SoilWiltingPointMapName`, `SoilPercentSandMapName`,
  `SoilPercentClayMapName`, `SoilBulkDensityMapName`,
  `SoilParticleDensityMapName`, `InitialSOC_PrimaryMapName`,
  `InitialSON_PrimaryMapName`, `InitialDeadWoodSurfaceMapName`,
  `InitialDeadCoarseRootsMapName`.

- `InitialFineFuels`:

  Numeric in `[0, 1]`. Fraction of initial dead wood allocated to fine
  fuels.

- `InitialMineralN`:

  Numeric. Initial mineral N (\$g/m^2\$).

- `DenitrificationRate`:

  Numeric in `[0, 1]`. Monthly fraction of mineral N lost through
  volatilization and denitrification.

- `WaterDecayFunction`:

  Character. One of `"Linear"` or `"Ratio"`.

- `DammMcNIPParameters`:

  Named list of numeric scalar parameters for the DAMM-McNiP soil C/N
  block. Required keys (all must be supplied): `InitialMicrobialC`,
  `InitialMicrobialN`, `InitialEnzymeConc`, `ActEnergySOMDepoly`,
  `ActEnergyDOCUptake`, `ExpConstSOMDepoly`, `ExpConstDOCUptake`,
  `FractionSOMUnprotect`, `CNEnzymes`, `KmSOMDepoly`, `KmDOCUptake`,
  `EnzTurnRate`, `MicrobialTurnRate`, `CarbonUseEfficiency`,
  `PropEnzymeSOM`, `PropCEnzymeProduction`, `PropNEnzymeProduction`,
  `FractDeadMicrobialBiomassSOM`, `MMConstantO2`, `DiffConstantO2`,
  `DiffConstantSOMLiquid`, `FractionVolumeO2`, `DOCFraction`,
  `DONFraction`, `FractionLitterToDOC`.

- `SeedingAlgorithm`:

  Character. Dispersal algorithm to use. One of `"WardSeedDispersal"`,
  `"NoDispersal"`, `"UniversalDispersal"`.

- `ProbabilityEstablishAdjust`:

  Numeric. Multiplier applied to the probability of establishment.
  Default `1.0`.

- `SpeciesParameters`:

  Character. Relative file path to the species-parameters CSV (see §2.54
  of the user guide).

- `FireReductionParameters`:

  `data.frame` with columns `FireSeverity`, `CoarseDebrisReduction`,
  `FineLitterReduction`, `CohortWoodReduction`, `CohortLeafReduction`,
  `OrganicHorizonReduction`. Required even when no fire extension is
  used.

- `HarvestReductionParameters`:

  (Optional) `data.frame` with columns `PrescriptionName`,
  `DeadWoodReduction`, `DeadLitterReduction`, `SOMReduction`,
  `CohortWoodRemoval`, `CohortLeafRemoval`. The `SOMReduction` column is
  required by the parser even though the user guide (§2.56) does not
  list it.

------------------------------------------------------------------------

### `DGSSuccession$write()`

Write extension inputs to disk

#### Usage

    DGSSuccession$write()

------------------------------------------------------------------------

### `DGSSuccession$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DGSSuccession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
