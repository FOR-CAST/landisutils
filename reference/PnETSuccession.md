# PnET-Succession Extension

Builds the LANDIS-II PnET-Succession v6.0 extension input file
(compatible with LANDIS-II v8). The top-level `PnET-Succession`
parameters are written via this class' `$write()` method; auxiliary
inputs (PnET generic / species / ecoregion parameters, disturbance
reductions, Saxton-and-Rawls soil parameters, climate, and PnET output
sites) are written by the companion `prep*()` helpers.

## References

LANDIS-II PnET-Succession v6.0 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-PnET-Succession/blob/master/deploy/docs/LANDIS-II%20PnET-Succession%20v6.0%20User%20Guide%20Jan21%202026.pdf>

## See also

Helpers that prepare inputs for this extension:
[`prepPnETGenericParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETGenericParameters.md),
[`prepPnETSpeciesParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETSpeciesParameters.md),
[`prepPnETEcoregionParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETEcoregionParameters.md),
[`prepPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/prepPnETDisturbanceReductions.md),
[`prepPnETClimateFile()`](https://for-cast.github.io/landisutils/reference/prepPnETClimateFile.md),
[`prepPNEToutputsites()`](https://for-cast.github.io/landisutils/reference/prepPNEToutputsites.md).
Shared scenario inputs:
[`prepClimateConfig()`](https://for-cast.github.io/landisutils/reference/prepClimateConfig.md),
[`prepInitialCommunities()`](https://for-cast.github.io/landisutils/reference/prepInitialCommunities.md).

Other PnET Succession helpers:
[`defaultPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/defaultPnETDisturbanceReductions.md),
[`prepPNEToutputsites()`](https://for-cast.github.io/landisutils/reference/prepPNEToutputsites.md),
[`prepPnETClimateFile()`](https://for-cast.github.io/landisutils/reference/prepPnETClimateFile.md),
[`prepPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/prepPnETDisturbanceReductions.md),
[`prepPnETEcoregionParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETEcoregionParameters.md),
[`prepPnETGenericParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETGenericParameters.md),
[`prepPnETSpeciesParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETSpeciesParameters.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `PnETSuccession`

## Active bindings

- `StartYear`:

  Integer. Climate year in which the simulation begins.

- `SeedingAlgorithm`:

  Character. Dispersal algorithm to use. One of `"WardSeedDispersal"`,
  `"NoDispersal"`, `"UniversalDispersal"`.

- `Latitude`:

  Numeric (optional). Degrees.

- `PNEToutputsites`:

  Character (optional). Relative file path.

- `InitialCommunities`:

  Character. Relative file path.

- `InitialCommunitiesMap`:

  Character. Relative file path.

- `LitterMap`:

  Character (optional). Relative file path.

- `WoodyDebrisMap`:

  Character (optional). Relative file path.

- `PnETGenericParameters`:

  Character (optional). Relative file path.

- `PnETSpeciesParameters`:

  Character. Relative file path.

- `EcoregionParameters`:

  Character. Relative file path.

- `DisturbanceReductions`:

  Character (optional). Relative file path.

- `ClimateConfigFile`:

  Character (optional). Relative file path.

- `SaxtonAndRawlsParameters`:

  Character (optional). Relative file path.

- `CohortBinSize`:

  Integer (optional). Must be `>= Timestep`.

## Methods

### Public methods

- [`PnETSuccession$new()`](#method-PnETSuccession-initialize)

- [`PnETSuccession$write()`](#method-PnETSuccession-write)

- [`PnETSuccession$clone()`](#method-PnETSuccession-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `PnETSuccession$new()`

#### Usage

    PnETSuccession$new(
      path = NULL,
      Timestep = 10L,
      StartYear = NULL,
      SeedingAlgorithm = NULL,
      Latitude = NULL,
      PNEToutputsites = NULL,
      InitialCommunities = NULL,
      InitialCommunitiesMap = NULL,
      LitterMap = NULL,
      WoodyDebrisMap = NULL,
      PnETGenericParameters = NULL,
      PnETSpeciesParameters = NULL,
      EcoregionParameters = NULL,
      DisturbanceReductions = NULL,
      ClimateConfigFile = NULL,
      SaxtonAndRawlsParameters = NULL,
      CohortBinSize = NULL
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Succession timestep in years (recommended \<= 10).

- `StartYear`:

  Integer. Climate year in which the simulation begins.

- `SeedingAlgorithm`:

  Character. Dispersal algorithm to use. One of `"WardSeedDispersal"`,
  `"NoDispersal"`, `"UniversalDispersal"`.

- `Latitude`:

  (Optional) Numeric. Global study-site latitude in degrees. Can
  alternatively be set per-ecoregion in the `EcoregionParameters` file.

- `PNEToutputsites`:

  (Optional) Character. Relative file path to the `PNEToutputsites`
  input file (see §12 of the user guide).

- `InitialCommunities`:

  Character. Relative file path to the initial communities CSV (see §4
  of the user guide).

- `InitialCommunitiesMap`:

  Character. Relative file path to the initial communities raster.

- `LitterMap`:

  (Optional) Character. Relative file path to the initial leaf litter
  map (\$g/m^2\$).

- `WoodyDebrisMap`:

  (Optional) Character. Relative file path to the initial dead woody
  debris map (\$g/m^2\$).

- `PnETGenericParameters`:

  (Optional) Character. Relative file path to the PnET generic
  parameters file (§7).

- `PnETSpeciesParameters`:

  Character. Relative file path to the PnET species parameters file
  (§8).

- `EcoregionParameters`:

  Character. Relative file path to the PnET ecoregion parameters file
  (§9).

- `DisturbanceReductions`:

  (Optional) Character. Relative file path to the disturbance reductions
  file (§10).

- `ClimateConfigFile`:

  (Optional) Character. Relative file path to the LANDIS-II climate
  library configuration file. When omitted, the climate file(s)
  referenced from `EcoregionParameters` are used.

- `SaxtonAndRawlsParameters`:

  (Optional) Character. Relative file path to an override
  Saxton-and-Rawls soil parameter file.

- `CohortBinSize`:

  (Optional) Integer. Number of years represented by an age cohort; must
  be `>= Timestep`.

------------------------------------------------------------------------

### `PnETSuccession$write()`

Write extension inputs to disk

#### Usage

    PnETSuccession$write()

------------------------------------------------------------------------

### `PnETSuccession$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PnETSuccession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
