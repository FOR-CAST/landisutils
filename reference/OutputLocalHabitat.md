# Local Habitat Suitability Output Extension

Local Habitat Suitability Output Extension

Local Habitat Suitability Output Extension

## References

LANDIS-II Local Habitat Suitability Output v3 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Local-Habitat-Suitability-Output/blob/master/docs/LANDIS-II%20Local%20Habitat%20Suitability%20Output%20v3%20User%20Guide.pdf>

## See also

Other Local Habitat Output helpers:
[`insertSuitabilityFiles()`](https://for-cast.github.io/landisutils/reference/insertSuitabilityFiles.md),
[`suitabilityFile()`](https://for-cast.github.io/landisutils/reference/suitabilityFile.md)

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OutputLocalHabitat`

## Active bindings

- `OutputTimestep`:

  Integer. Output frequency (years); must be `>= Timestep`.

- `MapFileNames`:

  Character. Output filename pattern; must contain `{HabitatName}` and
  `{timestep}`.

- `SuitabilityFiles`:

  List of `SuitabilityFile` objects, or a character vector of relative
  filenames (auto-wrapped).

## Methods

### Public methods

- [`OutputLocalHabitat$new()`](#method-OutputLocalHabitat-new)

- [`OutputLocalHabitat$write()`](#method-OutputLocalHabitat-write)

- [`OutputLocalHabitat$clone()`](#method-OutputLocalHabitat-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    OutputLocalHabitat$new(
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
  `{HabitatName}` and `{timestep}`.

- `SuitabilityFiles`:

  List of `SuitabilityFile` sub-objects (see
  [`suitabilityFile()`](https://for-cast.github.io/landisutils/reference/suitabilityFile.md))
  or a character vector of filenames (auto-wrapped).

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    OutputLocalHabitat$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputLocalHabitat$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
