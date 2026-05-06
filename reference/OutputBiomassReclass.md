# Biomass Reclassification Output Extension

Biomass Reclassification Output Extension

## References

LANDIS-II Output Biomass Reclassification v4 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-Reclass/blob/master/docs/LANDIS-II%20Biomass%20Reclass%20Output%20v4%20User%20Guide.pdf>

## See also

Other Biomass Reclass Output helpers:
[`insertReclassMaps()`](https://for-cast.github.io/landisutils/reference/insertReclassMaps.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OutputBiomassReclass`

## Active bindings

- `ReclassMaps`:

  Nested named list. See `$initialize()` for the expected structure.

- `MapFileNames`:

  Character. Output filename pattern; must contain `{reclass-map-name}`
  and `{timestep}`.

## Methods

### Public methods

- [`OutputBiomassReclass$new()`](#method-OutputBiomassReclass-initialize)

- [`OutputBiomassReclass$write()`](#method-OutputBiomassReclass-write)

- [`OutputBiomassReclass$clone()`](#method-OutputBiomassReclass-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `OutputBiomassReclass$new()`

#### Usage

    OutputBiomassReclass$new(
      path = NULL,
      Timestep = 10L,
      ReclassMaps = NULL,
      MapFileNames = NULL
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `ReclassMaps`:

  Nested named list. Top-level names are reclassification map names (no
  spaces). Each element is itself a named list whose names are forest
  types and whose values are character vectors of species codes. Species
  prefixed with `-` are subtracted from the forest type's dominance
  value. See user guide §2.3.

- `MapFileNames`:

  Character. Output-map filename pattern; must contain the literals
  `{reclass-map-name}` and `{timestep}`.

------------------------------------------------------------------------

### `OutputBiomassReclass$write()`

Write extension inputs to disk

#### Usage

    OutputBiomassReclass$write()

------------------------------------------------------------------------

### `OutputBiomassReclass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputBiomassReclass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
