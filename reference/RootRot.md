# Root Rot Extension

Root Rot Extension

## LANDIS-II v8 compatibility

Root Rot has not yet been updated by the LANDIS-II developers for
LANDIS-II v8; the most recent upstream release targets the v7 core only.
This R6 class is provided to track the v1.0 schema and exercise the
input-file generation, but `$write()`-produced files cannot currently be
run through a LANDIS-II v8 console. Constructing a `RootRot` object
emits a one-time [`warning()`](https://rdrr.io/r/base/warning.html) to
remind users of this. The warning will be removed once a v8-compatible
Root Rot release is available

## References

LANDIS-II Root Rot v1.0 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Root-Rot/blob/master/Docs/LANDIS-II%20Root%20Rot%20v1.0%20User%20Guide.pdf>

## See also

Other Root Rot helpers:
[`insertRootRotSpeciesSusceptibility()`](https://for-cast.github.io/landisutils/reference/insertRootRotSpeciesSusceptibility.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `RootRot`

## Active bindings

- `Pathogen`:

  (Optional) Character. Annotated alongside the header.

- `InputMap`:

  (Optional) Character. Initial infection raster path.

- `SpeciesSusceptibility`:

  `data.frame` with columns `Species`, `Index1`, `Index2`.

- `LethalTemp`:

  Numeric `<= 0` (°C). Minimum air temperature below which the pathogen
  cannot survive.

- `MinSoilTemp`:

  Numeric (°C). Minimum soil temperature below which the pathogen cannot
  transition Uninfected -\> Infected.

- `PhWet`:

  Numeric. Pressure head (wet condition threshold).

- `PhDry`:

  Numeric. Pressure head (dry condition threshold).

- `PhMax`:

  Numeric. Pressure head (maximum threshold).

- `MinProbID`:

  Numeric in `[0, 1]`.

- `MaxProbDI`:

  Numeric in `[0, 1]`.

- `OutputMapName`:

  Character. Infection-status output raster pattern; must contain
  `{timestep}`.

- `TOLDMapName`:

  Character. Time-of-Last-Disease output raster pattern; must contain
  `{timestep}`.

- `LethalTempMapName`:

  Character. Lethal-temperature output raster pattern; must contain
  `{timestep}`.

- `TotalBiomassRemovedMapName`:

  Character. Total biomass-removed output pattern; must contain
  `{timestep}`.

- `SpeciesBiomassRemovedMapName`:

  Character. Per-species biomass- removed output pattern; must contain
  `{species}` and `{timestep}`.

- `EventLog`:

  (Optional) Character. Relative file path for the events CSV log;
  `NULL` disables.

- `SummaryLog`:

  (Optional) Character. Relative file path for the summary CSV log;
  `NULL` disables.

## Methods

### Public methods

- [`RootRot$new()`](#method-RootRot-initialize)

- [`RootRot$write()`](#method-RootRot-write)

- [`RootRot$clone()`](#method-RootRot-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `RootRot$new()`

#### Usage

    RootRot$new(
      path,
      Timestep = NULL,
      Pathogen = NULL,
      InputMap = NULL,
      SpeciesSusceptibility = NULL,
      LethalTemp = NULL,
      MinSoilTemp = NULL,
      PhWet = NULL,
      PhDry = NULL,
      PhMax = NULL,
      MinProbID = NULL,
      MaxProbDI = NULL,
      OutputMapName = NULL,
      TOLDMapName = NULL,
      LethalTempMapName = NULL,
      TotalBiomassRemovedMapName = NULL,
      SpeciesBiomassRemovedMapName = NULL,
      EventLog = "rootrot/events.csv",
      SummaryLog = "rootrot/summary.csv"
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Years between updates.

- `Pathogen`:

  (Optional) Character. Pathogen name to annotate alongside the
  `LandisData` header (e.g. `"Phytophthora cinnamomi"`).

- `InputMap`:

  (Optional) Character. Initial infection raster path. When omitted, the
  model assumes all cells begin Uninfected.

- `SpeciesSusceptibility`:

  `data.frame` with columns `Species`, `Index1` (initial
  susceptibility), `Index2` (secondary susceptibility); both indices in
  `[0, 1]`.

- `LethalTemp`:

  Numeric `<= 0` (°C). Minimum air temperature below which the pathogen
  cannot survive.

- `MinSoilTemp`:

  Numeric (°C). Minimum soil temperature below which the pathogen cannot
  transition Uninfected → Infected.

- `PhWet, PhDry, PhMax`:

  Numeric `> 0` (m). Pressure-head thresholds: wet, dry, and
  extremely-dry conditions.

- `MinProbID, MaxProbDI`:

  Numeric in `[0, 1]`. Min infection→diseased probability and max
  diseased→infected probability.

- `OutputMapName`:

  (Optional) Character. Infection-status output raster pattern; must
  contain `{timestep}`.

- `TOLDMapName`:

  (Optional) Character. Time-of-Last-Disease output raster pattern; must
  contain `{timestep}`.

- `LethalTempMapName`:

  (Optional) Character. Lethal-temperature output raster pattern; must
  contain `{timestep}`.

- `TotalBiomassRemovedMapName`:

  (Optional) Character. Total biomass- removed output raster pattern;
  must contain `{timestep}`.

- `SpeciesBiomassRemovedMapName`:

  (Optional) Character. Per-species biomass-removed output raster
  pattern; must contain both `{species}` and `{timestep}`.

- `EventLog`:

  (Optional) Character. Relative file path for the events CSV log; pass
  `NULL` to disable.

- `SummaryLog`:

  (Optional) Character. Relative file path for the summary CSV log; pass
  `NULL` to disable.

------------------------------------------------------------------------

### `RootRot$write()`

Write extension inputs to disk

#### Usage

    RootRot$write()

------------------------------------------------------------------------

### `RootRot$clone()`

The objects of this class are cloneable with this method.

#### Usage

    RootRot$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
