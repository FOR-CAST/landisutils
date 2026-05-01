# NECN Succession Extension

NECN Succession Extension

NECN Succession Extension

## References

LANDIS-II Net Ecosystem CN Succession v8 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-NECN-Succession/blob/master/docs/LANDIS-II%20Net%20Ecosystem%20CN%20Succession%20v8%20User%20Guide.pdf>

## See also

Helpers that prepare inputs for this extension:
[`prepNECNFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/prepNECNFireReductionParameters.md).
Shared scenario inputs:
[`prepClimateConfig()`](https://for-cast.github.io/landisutils/reference/prepClimateConfig.md),
[`prepInitialCommunities()`](https://for-cast.github.io/landisutils/reference/prepInitialCommunities.md),
[`prepSpeciesData()`](https://for-cast.github.io/landisutils/reference/prepSpeciesData.md).

Other NECN Succession helpers:
[`insertNECNFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertNECNFireReductionParameters.md),
[`insertNECNHarvestReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertNECNHarvestReductionParameters.md),
[`prepNECNFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/prepNECNFireReductionParameters.md)

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `NECNSuccession`

## Active bindings

- `SeedingAlgorithm`:

  Character. Dispersal algorithm to use. One of `"WardSeedDispersal"`,
  `"NoDispersal"`, `"UniversalDispersal"`.

- `InitialCommunitiesCSV`:

  Character. Relative file path.

- `InitialCommunitiesMap`:

  Character. Relative file path.

- `ClimateConfigFile`:

  Character. Relative file path.

- `SoilMaps`:

  Named list of relative file paths. Required keys: `SoilDepthMapName`,
  `SoilDrainMapName`, `SoilBaseFlowMapName`, `SoilStormFlowMapName`,
  `SoilFieldCapacityMapName`, `SoilWiltingPointMapName`,
  `SoilPercentSandMapName`, `SoilPercentClayMapName`,
  `InitialSOM1CsurfMapName`, `InitialSOM1NsurfMapName`,
  `InitialSOM1CsoilMapName`, `InitialSOM1NsoilMapName`,
  `InitialSOM2CMapName`, `InitialSOM2NMapName`, `InitialSOM3CMapName`,
  `InitialSOM3NMapName`, `InitialDeadWoodSurfaceMapName`,
  `InitialDeadCoarseRootsMapName`.

- `OptionalClimateMaps`:

  Named list of relative file paths. Allowed keys: `SlopeMapName`,
  `AspectMapName`, `NormalSWAMapName`, `NormalCWDMapName`,
  `NormalTempMapName`.

- `CalibrateMode`:

  Logical, or character indicating "yes" or "no".

- `SmokeModelOutputs`:

  Logical, or character indicating "yes" or "no".

- `Write_SWA_Maps`:

  Logical, or character indicating "yes" or "no".

- `Write_CWD_Maps`:

  Logical, or character indicating "yes" or "no".

- `Write_Temperature_Maps`:

  Logical, or character indicating "yes" or "no".

- `Write_Species_Drought_Maps`:

  Logical, or character indicating "yes" or "no".

- `WaterDecayFunction`:

  Character. One of `"Linear"` or `"Ratio"`.

- `ProbabilityEstablishAdjust`:

  Numeric.

- `InitialMineralN`:

  Numeric (g m-2).

- `InitialFineFuels`:

  Numeric in `[0, 1]`.

- `AtmosphericNSlope`:

  Numeric.

- `AtmosphericNIntercept`:

  Numeric.

- `Latitude`:

  Numeric (degrees).

- `DenitrificationRate`:

  Numeric in `[0, 1]`.

- `DecayRateSurf`:

  Numeric in `[0, 1]`.

- `DecayRateSOM1`:

  Numeric in `[0, 1]`.

- `DecayRateSOM2`:

  Numeric in `[0, 1]`.

- `DecayRateSOM3`:

  Numeric in `[0, 1]`.

- `GrassThresholdMultiplier`:

  Numeric.

- `OutputMaps`:

  Named list. See table 3 of the user guide. Allowed names:
  `ANPPMapName`, `ANPPMapFrequency`, `ANEEMapName`, `ANEEMapFrequency`,
  `SoilCarbonMapName`, `SoilNitrogenMapName`, `TotalCMapName`.
  `*MapName` values are file-name templates and must contain the literal
  `{timestep}` placeholder (user guide §2.29) – LANDIS-II replaces it
  with the simulation year, e.g. `"NECN/ANPP-{timestep}.tif"`.

- `CreateInputCommunityMaps`:

  Logical, or character indicating "yes" or "no".

- `VariableOverrides`:

  Named list. Allowed names: `Stormflow`, `WaterFactor1`,
  `WaterFactor2`, `AnaerobicFactor1`, `AnaerobicFactor2`,
  `AnaerobicFactor3`.

- `SpeciesParameters`:

  Character. Relative file path.

- `DroughtMortalityParameters`:

  Character. Relative file path.

- `FireReductionParameters`:

  `data.frame`.

- `HarvestReductionParameters`:

  `data.frame`.

## Methods

### Public methods

- [`NECNSuccession$new()`](#method-NECNSuccession-new)

- [`NECNSuccession$write()`](#method-NECNSuccession-write)

- [`NECNSuccession$clone()`](#method-NECNSuccession-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    NECNSuccession$new(
      path,
      Timestep = 10L,
      SeedingAlgorithm = NULL,
      InitialCommunitiesCSV = NULL,
      InitialCommunitiesMap = NULL,
      ClimateConfigFile = NULL,
      SoilMaps = NULL,
      OptionalClimateMaps = NULL,
      CalibrateMode = NULL,
      SmokeModelOutputs = NULL,
      Write_SWA_Maps = NULL,
      Write_CWD_Maps = NULL,
      Write_Temperature_Maps = NULL,
      Write_Species_Drought_Maps = NULL,
      WaterDecayFunction = NULL,
      ProbabilityEstablishAdjust = 1,
      InitialMineralN = NULL,
      InitialFineFuels = NULL,
      AtmosphericNSlope = NULL,
      AtmosphericNIntercept = NULL,
      Latitude = NULL,
      DenitrificationRate = NULL,
      DecayRateSurf = NULL,
      DecayRateSOM1 = NULL,
      DecayRateSOM2 = NULL,
      DecayRateSOM3 = NULL,
      GrassThresholdMultiplier = NULL,
      OutputMaps = NULL,
      CreateInputCommunityMaps = NULL,
      VariableOverrides = NULL,
      SpeciesParameters = NULL,
      DroughtMortalityParameters = NULL,
      FireReductionParameters = NULL,
      HarvestReductionParameters = NULL
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `SeedingAlgorithm`:

  Character. Dispersal algorithm to use. One of `"WardSeedDispersal"`,
  `"NoDispersal"`, `"UniversalDispersal"`.

- `InitialCommunitiesCSV`:

  Character. Relative file path to the initial communities CSV (see §4
  of the user guide).

- `InitialCommunitiesMap`:

  Character. Relative file path to the initial communities raster.

- `ClimateConfigFile`:

  Character. Relative file path to the climate library configuration
  file.

- `SoilMaps`:

  Named list of relative file paths for required soil physical and
  initial soil organic matter / dead wood map inputs. Keys must match
  the NECN keywords (Tables 1 and 2 of the user guide):
  `SoilDepthMapName`, `SoilDrainMapName`, `SoilBaseFlowMapName`,
  `SoilStormFlowMapName`, `SoilFieldCapacityMapName`,
  `SoilWiltingPointMapName`, `SoilPercentSandMapName`,
  `SoilPercentClayMapName`, `InitialSOM1CsurfMapName`,
  `InitialSOM1NsurfMapName`, `InitialSOM1CsoilMapName`,
  `InitialSOM1NsoilMapName`, `InitialSOM2CMapName`,
  `InitialSOM2NMapName`, `InitialSOM3CMapName`, `InitialSOM3NMapName`,
  `InitialDeadWoodSurfaceMapName`, `InitialDeadCoarseRootsMapName`.

- `OptionalClimateMaps`:

  (Optional) Named list of relative file paths for any of
  `SlopeMapName`, `AspectMapName`, `NormalSWAMapName`,
  `NormalCWDMapName`, `NormalTempMapName`.

- `CalibrateMode`:

  Logical, or character indicating "yes" or "no".

- `SmokeModelOutputs`:

  Logical, or character indicating "yes" or "no".

- `Write_SWA_Maps`:

  Logical, or character indicating "yes" or "no".

- `Write_CWD_Maps`:

  Logical, or character indicating "yes" or "no".

- `Write_Temperature_Maps`:

  Logical, or character indicating "yes" or "no".

- `Write_Species_Drought_Maps`:

  Logical, or character indicating "yes" or "no".

- `WaterDecayFunction`:

  Character. One of `"Linear"` or `"Ratio"`.

- `ProbabilityEstablishAdjust`:

  Numeric. Multiplier applied to the probability of establishment.
  Default `1.0`.

- `InitialMineralN`:

  Numeric. Initial mineral N (\$g/m^2\$).

- `InitialFineFuels`:

  Numeric in `[0, 1]`. Fraction of initial dead wood allocated to fine
  fuels.

- `AtmosphericNSlope`:

  Numeric. Slope for linear N-deposition model.

- `AtmosphericNIntercept`:

  Numeric. Intercept for linear N-deposition model.

- `Latitude`:

  Numeric. Study-site latitude (degrees).

- `DenitrificationRate`:

  Numeric in `[0, 1]`. Monthly fraction of mineral N lost through
  ammonia volatilization and denitrification.

- `DecayRateSurf`:

  Numeric in `[0, 1]`. Max decay rate of SOM1-surface pool.

- `DecayRateSOM1`:

  Numeric in `[0, 1]`. Max decay rate of SOM1-soil pool.

- `DecayRateSOM2`:

  Numeric in `[0, 1]`. Max decay rate of SOM2 (slow) pool.

- `DecayRateSOM3`:

  Numeric in `[0, 1]`. Max decay rate of SOM3 (passive) pool.

- `GrassThresholdMultiplier`:

  (Optional) Numeric. Grass/tree competition multiplier; see user guide
  §2.28.

- `OutputMaps`:

  (Optional) Named list of optional output-map keywords and values. See
  table 3 of the user guide. Allowed names: `ANPPMapName`,
  `ANPPMapFrequency`, `ANEEMapName`, `ANEEMapFrequency`,
  `SoilCarbonMapName`, `SoilNitrogenMapName`, `TotalCMapName`.
  `*MapName` values are file-name templates and must contain the literal
  `{timestep}` placeholder (user guide §2.29), e.g.
  `"NECN/ANPP-{timestep}.tif"`.

- `CreateInputCommunityMaps`:

  Logical, or character indicating "yes" or "no".

- `VariableOverrides`:

  (Optional) Named list of numeric override values. Allowed names:
  `Stormflow`, `WaterFactor1`, `WaterFactor2`, `AnaerobicFactor1`,
  `AnaerobicFactor2`, `AnaerobicFactor3`.

- `SpeciesParameters`:

  Character. Relative file path to the species-parameters CSV (see
  §2.32, Table 4, of the user guide).

- `DroughtMortalityParameters`:

  (Optional) Character. Relative file path to the
  drought-mortality-parameters CSV (see §2.33, Table 5, of the user
  guide).

- `FireReductionParameters`:

  `data.frame` with columns `FireSeverity`, `CoarseDebrisReduction`,
  `FineLitterReduction`, `CohortWoodReduction`, `CohortLeafReduction`,
  `SOMReduction`. Required even when no fire extension is used.

- `HarvestReductionParameters`:

  (Optional) `data.frame` with columns `PrescriptionName`,
  `DeadWoodReduction`, `DeadLitterReduction`, `CohortWoodRemoval`,
  `CohortLeafRemoval`.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    NECNSuccession$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    NECNSuccession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
