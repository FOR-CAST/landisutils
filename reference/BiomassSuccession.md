# Biomass Succession Extension

Biomass Succession Extension

Biomass Succession Extension

## References

LANDIS-II Biomass Succession v7 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/docs/LANDIS-II%20Biomass%20Succession%20v7%20User%20Guide.pdf>

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `DynamicFuels`

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

- [`BiomassSuccession$new()`](#method-DynamicFuels-new)

- [`BiomassSuccession$write()`](#method-DynamicFuels-write)

- [`BiomassSuccession$clone()`](#method-DynamicFuels-clone)

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
