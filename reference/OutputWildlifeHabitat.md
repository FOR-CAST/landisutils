# Wildlife Habitat Output Extension

Wildlife Habitat Output Extension

## References

LANDIS-II Output Wildlife Habitat v3 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Output-Wildlife-Habitat/blob/master/docs/LANDIS-II%20Wildlife%20Habitat%20Output%20v3%20User%20Guide.pdf>

## See also

[`suitabilityFile()`](https://for-cast.github.io/landisutils/reference/suitabilityFile.md)
(also used by
[OutputLocalHabitat](https://for-cast.github.io/landisutils/reference/OutputLocalHabitat.md)).

Other Wildlife Habitat Output helpers:
[`insertSuitabilityFiles()`](https://for-cast.github.io/landisutils/reference/insertSuitabilityFiles.md),
[`suitabilityFile()`](https://for-cast.github.io/landisutils/reference/suitabilityFile.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OutputWildlifeHabitat`

## Active bindings

- `OutputTimestep`:

  Integer. Output frequency (years); must be `>= Timestep`.

- `MapFileNames`:

  Character. Output filename pattern; must contain `{wildlifeName}` and
  `{timestep}`.

- `SuitabilityFiles`:

  List of `SuitabilityFile` objects, or a character vector of relative
  filenames (auto-wrapped).

## Methods

### Public methods

- [`OutputWildlifeHabitat$new()`](#method-OutputWildlifeHabitat-initialize)

- [`OutputWildlifeHabitat$write()`](#method-OutputWildlifeHabitat-write)

- [`OutputWildlifeHabitat$clone()`](#method-OutputWildlifeHabitat-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `OutputWildlifeHabitat$new()`

#### Usage

    OutputWildlifeHabitat$new(
      path,
      Timestep = 1L,
      OutputTimestep = NULL,
      MapFileNames = NULL,
      SuitabilityFiles = list()
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Inner simulation interval (years).

- `OutputTimestep`:

  Integer. Output frequency (years); must be `>= Timestep`.

- `MapFileNames`:

  Character. Output filename pattern; must contain the literals
  `{wildlifeName}` and `{timestep}`.

- `SuitabilityFiles`:

  List of `SuitabilityFile` sub-objects (see
  [`suitabilityFile()`](https://for-cast.github.io/landisutils/reference/suitabilityFile.md))
  or a character vector of filenames (auto-wrapped).

------------------------------------------------------------------------

### `OutputWildlifeHabitat$write()`

Write extension inputs to disk

#### Usage

    OutputWildlifeHabitat$write()

------------------------------------------------------------------------

### `OutputWildlifeHabitat$clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputWildlifeHabitat$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
