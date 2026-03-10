# Forest Carbon Succession (ForCS) Extension

Forest Carbon Succession (ForCS) Extension

Forest Carbon Succession (ForCS) Extension

## References

LANDIS-II Social-Climate-Fire v4 User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Social-Climate-Fire/blob/master/docs/LANDIS-II%20Social-Climate-Fire%20v4%20User%20Guide.pdf>

TODO

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `ForC Succession`

## Active bindings

- `SeedingAlgorithm`:

  Character. Dispersal algorithm to use. One of `"WardSeedDispersal"`,
  `"NoDispersal"`, `"UniversalDispersal"`.

- `ClimateFile`:

  Character. Relative file path.

- `InitialCommunitiesFiles`:

  Character. Relative file paths.

- `DisturbanceMatrixFile`:

  Character. Relative file path.

- `SnagFile`:

  (Optional) Character. Relative file path.

- `OutputTables`:

  `data.frame`.

- `SoilSpinupControls`:

  `data.frame`.

- `AvailableLightBiomass`:

  `data.frame`.

- `LightEstablishmentTable`:

  `data.frame`.

- `SpeciesParameters`:

  `data.frame`.

- `DOMPools`:

  `data.frame`.

- `EcoSppDOMParameters`:

  `data.frame`.

- `ForCSProportions`:

  `data.frame`.

- `ANPPTimeSeries`:

  `data.frame`.

- `MaxBiomassTimeSeries`:

  `data.frame`.

- `EstablishProbabilities`:

  `data.frame`.

- `RootDynamics`:

  `data.frame`.

## Methods

### Public methods

- [`ForCS$new()`](#method-ForC%20Succession-new)

- [`ForCS$write()`](#method-ForC%20Succession-write)

- [`ForCS$clone()`](#method-ForC%20Succession-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    ForCS$new(
      path,
      Timestep = 1,
      SeedingAlgorithm = NULL,
      ClimateFile = NULL,
      InitialCommunitiesFiles = NULL,
      DisturbanceMatrixFile = NULL,
      SnagFile = NULL,
      OutputTables = NULL,
      SoilSpinupControls = NULL,
      AvailableLightBiomass = NULL,
      LightEstablishmentTable = NULL,
      SpeciesParameters = NULL,
      DOMPools = NULL,
      EcoSppDOMParameters = NULL,
      ForCSProportions = NULL,
      ANPPTimeSeries = NULL,
      MaxBiomassTimeSeries = NULL,
      EstablishProbabilities = NULL,
      RootDynamics = NULL
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `SeedingAlgorithm`:

  Character. Dispersal algorithm to use. One of `"WardSeedDispersal"`,
  `"NoDispersal"`, `"UniversalDispersal"`.

- `ClimateFile`:

  Character. Relative file path.

- `InitialCommunitiesFiles`:

  Character. Relative file paths.

- `DisturbanceMatrixFile`:

  Character. Relative file path.

- `SnagFile`:

  (Optional) Character. Relative file path.

- `OutputTables`:

  `data.frame`.

- `SoilSpinupControls`:

  `data.frame`.

- `AvailableLightBiomass`:

  `data.frame`.

- `LightEstablishmentTable`:

  `data.frame`.

- `SpeciesParameters`:

  `data.frame`.

- `DOMPools`:

  `data.frame`.

- `EcoSppDOMParameters`:

  `data.frame`.

- `ForCSProportions`:

  `data.frame`.

- `ANPPTimeSeries`:

  `data.frame`.

- `MaxBiomassTimeSeries`:

  `data.frame`.

- `EstablishProbabilities`:

  `data.frame`.

- `RootDynamics`:

  `data.frame`.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    ForCS$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ForCS$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## See vignette for example usage
```
