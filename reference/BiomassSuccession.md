# Biomass Succession Extension

Biomass Succession Extension

Biomass Succession Extension

## References

LANDIS-II Biomass Succession v7 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/docs/LANDIS-II%20Biomass%20Succession%20v7%20User%20Guide.pdf>

## See also

Helpers that prepare inputs for this extension:
[`prepMinRelativeBiomass()`](https://for-cast.github.io/landisutils/reference/prepMinRelativeBiomass.md),
[`prepEcoregionParameters()`](https://for-cast.github.io/landisutils/reference/prepEcoregionParameters.md),
[`prepSpeciesEcoregionDataFile()`](https://for-cast.github.io/landisutils/reference/prepSpeciesEcoregionDataFile.md),
[`prepFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/prepFireReductionParameters.md),
[`prepHarvestReductionParameters()`](https://for-cast.github.io/landisutils/reference/prepHarvestReductionParameters.md).
Shared scenario inputs:
[`prepClimateConfig()`](https://for-cast.github.io/landisutils/reference/prepClimateConfig.md),
[`prepInitialCommunities()`](https://for-cast.github.io/landisutils/reference/prepInitialCommunities.md),
[`prepSpeciesData()`](https://for-cast.github.io/landisutils/reference/prepSpeciesData.md).

Other Biomass Succession helpers:
[`insertEcoregionParameters()`](https://for-cast.github.io/landisutils/reference/insertEcoregionParameters.md),
[`insertFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertFireReductionParameters.md),
[`insertHarvestReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertHarvestReductionParameters.md),
[`insertMinRelativeBiomass()`](https://for-cast.github.io/landisutils/reference/insertMinRelativeBiomass.md),
[`insertSpeciesEcoregionDataFile()`](https://for-cast.github.io/landisutils/reference/insertSpeciesEcoregionDataFile.md),
[`insertSufficientLight()`](https://for-cast.github.io/landisutils/reference/insertSufficientLight.md),
[`prepEcoregionParameters()`](https://for-cast.github.io/landisutils/reference/prepEcoregionParameters.md),
[`prepFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/prepFireReductionParameters.md),
[`prepHarvestReductionParameters()`](https://for-cast.github.io/landisutils/reference/prepHarvestReductionParameters.md),
[`prepMinRelativeBiomass()`](https://for-cast.github.io/landisutils/reference/prepMinRelativeBiomass.md),
[`prepSpeciesEcoregionDataFile()`](https://for-cast.github.io/landisutils/reference/prepSpeciesEcoregionDataFile.md)

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `BiomassSuccession`

## Active bindings

- `SeedingAlgorithm`:

  Character. Dispersal algorithm to use. One of `"WardSeedDispersal"`,
  `"NoDispersal"`, `"UniversalDispersal"`.

- `InitialCommunitiesFiles`:

  Character. Relative file paths.

- `ClimateConfigFile`:

  Character. Relative file path.

- `CalibrateMode`:

  Logical, or character indicating "yes" or "no".

- `SpinupCohorts`:

  Logical, or character indicating "yes" or "no".

- `SpinupMortalityFraction`:

  Real.

- `MinRelativeBiomass`:

  `data.frame`.

- `SufficientLight`:

  `data.frame`.

- `SpeciesDataFile`:

  Character. Relative file path.

- `EcoregionParameters`:

  `data.frame`.

- `SpeciesEcoregionDataFile`:

  Character. Relative file path.

- `FireReductionParameters`:

  `data.frame`.

- `HarvestReductionParameters`:

  `data.frame`.

## Methods

### Public methods

- [`BiomassSuccession$new()`](#method-BiomassSuccession-new)

- [`BiomassSuccession$write()`](#method-BiomassSuccession-write)

- [`BiomassSuccession$clone()`](#method-BiomassSuccession-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    BiomassSuccession$new(
      path = NULL,
      Timestep = 10L,
      SeedingAlgorithm = NULL,
      InitialCommunitiesFiles = NULL,
      ClimateConfigFile = NULL,
      CalibrateMode = NULL,
      SpinupCohorts = NULL,
      SpinupMortalityFraction = NULL,
      MinRelativeBiomass = NULL,
      SufficientLight = NULL,
      SpeciesDataFile = NULL,
      EcoregionParameters = NULL,
      SpeciesEcoregionDataFile = NULL,
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

- `InitialCommunitiesFiles`:

  Character. Relative file paths.

- `ClimateConfigFile`:

  Character. Relative file path.

- `CalibrateMode`:

  Logical, or character indicating "yes" or "no".

- `SpinupCohorts`:

  Logical, or character indicating "yes" or "no".

- `SpinupMortalityFraction`:

  Real.

- `MinRelativeBiomass`:

  `data.frame`.

- `SufficientLight`:

  `data.frame`.

- `SpeciesDataFile`:

  Character. Relative file path.

- `EcoregionParameters`:

  `data.frame`.

- `SpeciesEcoregionDataFile`:

  Character. Relative file path.

- `FireReductionParameters`:

  `data.frame`.

- `HarvestReductionParameters`:

  `data.frame`.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    BiomassSuccession$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BiomassSuccession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
