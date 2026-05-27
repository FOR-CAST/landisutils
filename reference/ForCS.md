# Forest Carbon Succession (ForCS) Extension

Forest Carbon Succession (ForCS) Extension

## References

LANDIS-II Forest Carbon Succession (CForC) v4.0.2 User Guide
<https://github.com/LANDIS-II-Foundation/Extension-ForCS-Succession/blob/master/deploy/installer/LANDIS-II%20CForC%20Succession%20v4.0.2%20User%20Guide%20September%202025.pdf>

## See also

Helpers that prepare inputs for this extension:
[`prepClimateFile()`](https://for-cast.github.io/landisutils/reference/prepClimateFile.md),
[`prepDisturbanceMatrixFile()`](https://for-cast.github.io/landisutils/reference/prepDisturbanceMatrixFile.md),
[`prepSnagFile()`](https://for-cast.github.io/landisutils/reference/prepSnagFile.md).
Shared scenario inputs:
[`prepInitialCommunities()`](https://for-cast.github.io/landisutils/reference/prepInitialCommunities.md),
[`prepSpeciesData()`](https://for-cast.github.io/landisutils/reference/prepSpeciesData.md).

Other ForCS helpers:
[`insertANPPTimeSeries()`](https://for-cast.github.io/landisutils/reference/insertANPPTimeSeries.md),
[`insertAvailableLightBiomass()`](https://for-cast.github.io/landisutils/reference/insertAvailableLightBiomass.md),
[`insertDOMPools()`](https://for-cast.github.io/landisutils/reference/insertDOMPools.md),
[`insertEcoSppDOMParameters()`](https://for-cast.github.io/landisutils/reference/insertEcoSppDOMParameters.md),
[`insertEstablishProbabilities()`](https://for-cast.github.io/landisutils/reference/insertEstablishProbabilities.md),
[`insertForCSMapControl()`](https://for-cast.github.io/landisutils/reference/insertForCSMapControl.md),
[`insertForCSProportions()`](https://for-cast.github.io/landisutils/reference/insertForCSProportions.md),
[`insertLightEstablishmentTable()`](https://for-cast.github.io/landisutils/reference/insertLightEstablishmentTable.md),
[`insertMaxBiomassTimeSeries()`](https://for-cast.github.io/landisutils/reference/insertMaxBiomassTimeSeries.md),
[`insertOutputTables()`](https://for-cast.github.io/landisutils/reference/insertOutputTables.md),
[`insertRootDynamics()`](https://for-cast.github.io/landisutils/reference/insertRootDynamics.md),
[`insertSpeciesParameters()`](https://for-cast.github.io/landisutils/reference/insertSpeciesParameters.md),
[`insertSpinUp()`](https://for-cast.github.io/landisutils/reference/insertSpinUp.md),
[`prepClimateFile()`](https://for-cast.github.io/landisutils/reference/prepClimateFile.md),
[`prepDisturbanceMatrixFile()`](https://for-cast.github.io/landisutils/reference/prepDisturbanceMatrixFile.md),
[`prepSnagFile()`](https://for-cast.github.io/landisutils/reference/prepSnagFile.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `ForCS`

## Active bindings

- `SeedingAlgorithm`:

  Character. Dispersal algorithm to use. One of `"WardSeedDispersal"`,
  `"NoDispersal"`, `"UniversalDispersal"`.

- `ForCSClimateFile`:

  Character. Relative file path.

- `InitialCommunitiesFiles`:

  Character. Relative file paths.

- `DisturbanceMatrixFile`:

  Character. Relative file path.

- `SnagFile`:

  (Optional) Character. Relative file path.

- `OutputTables`:

  `data.frame`.

- `ForCSMapControl`:

  `data.frame`.

- `MapOutputInterval`:

  Integer.

- `SpinUp`:

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

- `output_files`:

  Character vector of ForCS-specific log CSV files produced at run time,
  relative to the scenario directory (root level). These fixed names are
  written by the ForCS extension regardless of scenario configuration.
  See
  [LandisExtension](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
  for the contract.

## Methods

### Public methods

- [`ForCS$new()`](#method-ForCS-initialize)

- [`ForCS$write()`](#method-ForCS-write)

- [`ForCS$clone()`](#method-ForCS-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `ForCS$new()`

#### Usage

    ForCS$new(
      path,
      Timestep = 1,
      SeedingAlgorithm = NULL,
      ForCSClimateFile = NULL,
      InitialCommunitiesFiles = NULL,
      DisturbanceMatrixFile = NULL,
      SnagFile = NULL,
      OutputTables = NULL,
      ForCSMapControl = NULL,
      MapOutputInterval = NULL,
      SpinUp = NULL,
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

- `ForCSClimateFile`:

  Character. Relative file path. Mean-annual-temperature climate file
  specific to ForCS (see
  [`prepClimateFile()`](https://for-cast.github.io/landisutils/reference/prepClimateFile.md)).

- `InitialCommunitiesFiles`:

  Character. Two-element vector of relative file paths: the
  initial-communities text/CSV and the initial-communities raster.

- `DisturbanceMatrixFile`:

  Character. Relative file path.

- `SnagFile`:

  (Optional) Character. Relative file path. May be `NULL`.

- `OutputTables`:

  `data.frame` corresponding to `ForCSOutput` (one row, four columns:
  Biomass, DOM_Pools, Fluxes, Summary intervals).

- `ForCSMapControl`:

  `data.frame` (one row, seven columns: `BiomassC`, `SDOMC`, `NBP`,
  `NEP`, `NPP`, `RH`, `ToFPS` toggles).

- `MapOutputInterval`:

  Integer. Map output interval (years).

- `SpinUp`:

  `data.frame` (one row, four columns: On/Off Flag, Biomass Spin-up
  Flag, Tolerance %, Max Iterations).

- `AvailableLightBiomass`:

  `data.frame`.

- `LightEstablishmentTable`:

  `data.frame`.

- `SpeciesParameters`:

  `data.frame` with 10 columns: `Species`, `LeafLong`, `MortalShape`,
  `MerchMinAge`, `MerchCurveA`, `MerchCurveB`, `PropNonMerchToFastAG`,
  `GrowthShape`, `ShadeTolerance`, `FireTolerance`.

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

### `ForCS$write()`

Write extension inputs to disk

#### Usage

    ForCS$write()

------------------------------------------------------------------------

### `ForCS$clone()`

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
