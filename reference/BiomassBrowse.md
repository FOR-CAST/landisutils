# Biomass Browse Extension

Biomass Browse Extension

## LANDIS-II v8 compatibility

Biomass Browse has not yet been updated by the LANDIS-II developers for
LANDIS-II v8; the most recent upstream release (v2.0, May 2022) targets
the v7 core only. This R6 class tracks the v2.0 schema and exercises the
input-file generation, but `$write()`-produced files cannot currently be
run through a LANDIS-II v8 console. Constructing a `BiomassBrowse`
object emits a one-time
[`warning()`](https://rdrr.io/r/base/warning.html) to remind users of
this. The warning will be removed once a v8-compatible Biomass Browse
release is available.

## References

LANDIS-II Browse Disturbance v2.0 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Biomass-Browse/blob/master/docs/LANDIS-II%20Biomass%20Browse%20v2.0%20User%20Guide.pdf>

## See also

Other Biomass Browse helpers:
[`insertBrowseDynamicPopulation()`](https://for-cast.github.io/landisutils/reference/insertBrowseDynamicPopulation.md),
[`insertBrowseSpeciesTable()`](https://for-cast.github.io/landisutils/reference/insertBrowseSpeciesTable.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `BiomassBrowse`

## Active bindings

- `SpeciesTable`:

  `data.frame` with the six columns enumerated in
  `.browseSpeciesTableCols`.

- `ZoneMap`:

  Character. Relative path to the population-zones raster.

- `BrowseMethod`:

  Character. One of `"Population"`, `"BDI"`.

- `DefinedPopulationFile`:

  Character. Relative path.

- `DynamicPopulation`:

  List of dynamic-population parameters, or `NULL`. Required keys (all
  numeric): `RMin`, `RMax`, `MortalityMin`, `MortalityMax`,
  `PredationMin`, `PredationMax`, `HarvestMin`, `HarvestMax`.

- `ConsumptionRate`:

  Integer.

- `ANPPForageProp`:

  Numeric in `[0, 1]`.

- `MinBrowsePropinReach`:

  Numeric in `[0, 1]`.

- `BrowseBiomassThreshold`:

  Numeric in `[0, 1]`.

- `BrowseBiomassThresholdMin`:

  Numeric in `[0, 1]`.

- `BrowseBiomassThresholdMax`:

  Numeric in `[0, 1]`.

- `EscapeBrowsePropLong`:

  Numeric in `[0, 1]`.

- `CalibrateMode`:

  Character `"ON"`/`"OFF"`.

- `GrowthReduction`:

  Character `"ON"`/`"OFF"`.

- `Mortality`:

  Character `"ON"`/`"OFF"`.

- `CountNonForageinSitePref`:

  Character `"TRUE"`/`"FALSE"` (set from a logical).

- `UseInitBiomassAsForage`:

  Character `"TRUE"`/`"FALSE"` (set from a logical).

- `ForageInReachMethod`:

  One of `"Ordered"`, `"LinearEachCohort"`.

- `ForageQuantity`:

  Integer (neighborhood radius).

- `SitePreference`:

  Integer (neighborhood radius).

- `SitePrefMapNames`:

  Character. Output filename pattern.

- `SiteForageMapNames`:

  Character. Output filename pattern.

- `SiteHSIMapNames`:

  Character. Output filename pattern.

- `SitePopulationMapNames`:

  Character. Output filename pattern.

- `BiomassRemovedMapNames`:

  Character. Output filename pattern.

- `LogFile`:

  Character. Relative file path.

- `output_files`:

  Character vector of output files (relative paths from the scenario
  directory) that this extension is expected to produce at run time.
  Subclasses override this to return their extension-specific log files,
  event CSVs, etc. These paths are collected by
  [`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md)
  and written to `output_manifest.txt` so
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  can track them explicitly without relying on
  [`list.files()`](https://rdrr.io/r/base/list.files.html) discovery.

  Map files whose names depend on the timestep (e.g. `BiomassC-10.tif`)
  are NOT included here; they are discovered by
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)'s
  `output_dir` scan instead.

## Methods

### Public methods

- [`BiomassBrowse$new()`](#method-BiomassBrowse-initialize)

- [`BiomassBrowse$write()`](#method-BiomassBrowse-write)

- [`BiomassBrowse$clone()`](#method-BiomassBrowse-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `BiomassBrowse$new()`

#### Usage

    BiomassBrowse$new(
      path,
      Timestep = 1L,
      SpeciesTable = NULL,
      ZoneMap = NULL,
      BrowseMethod = "Population",
      DefinedPopulationFile = NULL,
      DynamicPopulation = NULL,
      ConsumptionRate = NULL,
      ANPPForageProp = 0.66,
      MinBrowsePropinReach = NULL,
      BrowseBiomassThreshold = NULL,
      BrowseBiomassThresholdMin = NULL,
      BrowseBiomassThresholdMax = NULL,
      EscapeBrowsePropLong = NULL,
      CalibrateMode = NULL,
      GrowthReduction = NULL,
      Mortality = NULL,
      CountNonForageinSitePref = NULL,
      UseInitBiomassAsForage = NULL,
      ForageInReachMethod = NULL,
      ForageQuantity = NULL,
      SitePreference = NULL,
      SitePrefMapNames = NULL,
      SiteForageMapNames = NULL,
      SiteHSIMapNames = NULL,
      SitePopulationMapNames = NULL,
      BiomassRemovedMapNames = NULL,
      LogFile = "browse/browse-log.csv"
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Years between browse updates (typically 1; §4.2.1).

- `SpeciesTable`:

  `data.frame` with six columns matching §4.2.2: `Species` (character),
  `Preference`, `GrowthReductionThreshold`, `GrowthReductionMaximum`,
  `MortalityThreshold`, `MortalityMaximum` (all numeric in `[0, 1]`).

- `ZoneMap`:

  Character. Relative path to the population-zones raster (§4.2.3).

- `BrowseMethod`:

  Character. One of `"Population"` or `"BDI"` (§4.2.4).

- `DefinedPopulationFile`:

  Character. Relative path to the population / BDI definition file
  (§4.2.5).

- `DynamicPopulation`:

  List of dynamic-population parameters (§4.2.6) when running in dynamic
  mode; pass `NULL` for static/defined population. Required keys (all
  numeric): `RMin`, `RMax`, `MortalityMin`, `MortalityMax`,
  `PredationMin`, `PredationMax`, `HarvestMin`, `HarvestMax`.

- `ConsumptionRate`:

  Integer (kg/yr/individual; §4.2.7).

- `ANPPForageProp`:

  Numeric in `[0, 1]`; proportion of `ANPP` that counts as forage
  (§4.2.8; default 0.66).

- `MinBrowsePropinReach`:

  Numeric in `[0, 1]`; minimum proportion of browse within reach for a
  cohort to be browsed (§4.2.9).

- `BrowseBiomassThreshold`:

  Numeric in `[0, 1]`; proportion of ecoregion max biomass at which
  cohorts begin to escape browse (§4.2.12).

- `BrowseBiomassThresholdMin, BrowseBiomassThresholdMax`:

  (Optional) Numeric in `[0, 1]`; thresholds for the
  `ForageInReachMethod = "LinearEachCohort"` form (§4.2.10–4.2.11).

- `EscapeBrowsePropLong`:

  Numeric in `[0, 1]`; proportion of longevity at which cohorts are
  considered escaped from browse (§4.2.13).

- `CalibrateMode`:

  Character (`"ON"` / `"OFF"`); optional, default `"OFF"` (§4.2.14).

- `GrowthReduction`:

  Character (`"ON"` / `"OFF"`); optional, default `"ON"` (§4.2.15).

- `Mortality`:

  Character (`"ON"` / `"OFF"`); optional, default `"ON"` (§4.2.16).

- `CountNonForageinSitePref`:

  Logical; optional, default `FALSE` (§4.2.17).

- `UseInitBiomassAsForage`:

  Logical; optional, default `FALSE` (§4.2.18).

- `ForageInReachMethod`:

  Character; one of `"Ordered"` or `"LinearEachCohort"`; optional,
  default `"Ordered"` (§4.2.19).

- `ForageQuantity`:

  (Optional) Integer; HSI neighborhood radius for forage-quantity
  component (§4.2.20.1). Either this or `SitePreference` (or both) must
  be provided to compute HSI maps.

- `SitePreference`:

  (Optional) Integer; HSI neighborhood radius for site-preference
  component (§4.2.20.2).

- `SitePrefMapNames, SiteForageMapNames, SiteHSIMapNames, SitePopulationMapNames, BiomassRemovedMapNames`:

  (Optional) Character. Output filename patterns; each must contain
  `{timestep}` (§4.2.21).

- `LogFile`:

  Character. Relative path to the events CSV log (§4.2.22).

------------------------------------------------------------------------

### `BiomassBrowse$write()`

Write extension inputs to disk

#### Usage

    BiomassBrowse$write()

------------------------------------------------------------------------

### `BiomassBrowse$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BiomassBrowse$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
