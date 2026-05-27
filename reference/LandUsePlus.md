# Land Use Plus Extension

Land Use Plus Extension

## References

LANDIS-II Land Use Plus v4 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Land-Use-Plus/blob/master/docs/LANDIS-II%20Land%20Use%20Plus%20v4%20User%20Guide.pdf>

## See also

Other Land Use Plus helpers:
[`cohortSelector()`](https://for-cast.github.io/landisutils/reference/cohortSelector.md),
[`insertCohortSelector()`](https://for-cast.github.io/landisutils/reference/insertCohortSelector.md),
[`insertLandCoverChange()`](https://for-cast.github.io/landisutils/reference/insertLandCoverChange.md),
[`insertLandUseType()`](https://for-cast.github.io/landisutils/reference/insertLandUseType.md),
[`landCoverChange()`](https://for-cast.github.io/landisutils/reference/landCoverChange.md),
[`landUseType()`](https://for-cast.github.io/landisutils/reference/landUseType.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `LandUsePlus`

## Active bindings

- `InputMaps`:

  Character. Template for per-timestep land-use rasters; must contain
  the literal placeholder `{timestep}` and end in `.tif` (LU+ v4 only
  accepts GeoTIFF).

- `SiteLog`:

  (Optional) Character. Relative file path.

- `ExternalScript`:

  (Optional) Character.

- `ExternalExecutable`:

  (Optional) Character.

- `LandUses`:

  List of `LandUseType` objects.

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

- [`LandUsePlus$new()`](#method-LandUsePlus-initialize)

- [`LandUsePlus$add_land_use()`](#method-LandUsePlus-add_land_use)

- [`LandUsePlus$write()`](#method-LandUsePlus-write)

- [`LandUsePlus$clone()`](#method-LandUsePlus-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `LandUsePlus$new()`

#### Usage

    LandUsePlus$new(
      path,
      Timestep = NULL,
      InputMaps = NULL,
      SiteLog = NULL,
      ExternalScript = NULL,
      ExternalExecutable = NULL,
      LandUses = list()
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Years between land-use evaluations.

- `InputMaps`:

  Character. Template path for the per-timestep land-use rasters (e.g.
  `"landuse-{timestep}.tif"`).

- `SiteLog`:

  (Optional) Character. Relative path to the per-site CSV change log.

- `ExternalScript, ExternalExecutable`:

  (Optional) Character. Used to enable the LANDIS-Pause hook for an
  external script (user guide sections 3.2.5 and 3.2.6).

- `LandUses`:

  List of `LandUseType` objects (see
  [`landUseType()`](https://for-cast.github.io/landisutils/reference/landUseType.md)).

------------------------------------------------------------------------

### `LandUsePlus$add_land_use()`

#### Usage

    LandUsePlus$add_land_use(value)

#### Arguments

- `value`:

  `LandUseType` object to append to `$LandUses`.

------------------------------------------------------------------------

### `LandUsePlus$write()`

Write extension inputs to disk

#### Usage

    LandUsePlus$write()

------------------------------------------------------------------------

### `LandUsePlus$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LandUsePlus$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
