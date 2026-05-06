# Landscape Habitat Output Extension

Landscape Habitat Output Extension

## References

LANDIS-II Output Landscape Habitat v2 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Output-Landscape-Habitat/blob/master/docs/LANDIS-II%20Landscape%20Habitat%20Output%20v2%20User%20Guide.pdf>

## See also

Other Landscape Habitat Output helpers:
[`insertLandscapeHabitatClimateVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatClimateVariables.md),
[`insertLandscapeHabitatDerivedLocalVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatDerivedLocalVariables.md),
[`insertLandscapeHabitatDistanceVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatDistanceVariables.md),
[`insertLandscapeHabitatLocalVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatLocalVariables.md),
[`insertLandscapeHabitatNeighborhoodVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatNeighborhoodVariables.md),
[`insertLandscapeHabitatSpeciesModels()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatSpeciesModels.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OutputLandscapeHabitat`

## Active bindings

- `LocalVariables`:

  (Optional) Named list of `data.frame`s.

- `DerivedLocalVariables`:

  (Optional) Named character vector.

- `NeighborhoodVariables`:

  (Optional) `data.frame`.

- `ClimateVariables`:

  (Optional) `data.frame`.

- `DistanceVariables`:

  (Optional) `data.frame`.

- `SpeciesModels`:

  Named list of `data.frame`s with columns `Parameter`, `Type`, `Value`.

- `LocalVarMapFileNames`:

  Character. Must contain the literal placeholders `{local-var-name}`
  and `{timestep}`.

- `NeighborVarMapFileNames`:

  Character. Must contain the literal placeholders `{neighbor-var-name}`
  and `{timestep}`.

- `ClimateVarMapFileNames`:

  Character. Must contain the literal placeholders `{climate-var-name}`
  and `{timestep}`.

- `DistanceVarMapFileNames`:

  Character. Must contain the literal placeholders `{distance-var-name}`
  and `{timestep}`.

- `SpeciesMapFileNames`:

  Character. Must contain the literal placeholders `{species-name}` and
  `{timestep}`.

- `SpeciesLogFileNames`:

  Character. Must contain the literal placeholder `{species-name}`.

- `LogFile`:

  Character.

## Methods

### Public methods

- [`OutputLandscapeHabitat$new()`](#method-OutputLandscapeHabitat-initialize)

- [`OutputLandscapeHabitat$write()`](#method-OutputLandscapeHabitat-write)

- [`OutputLandscapeHabitat$clone()`](#method-OutputLandscapeHabitat-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `OutputLandscapeHabitat$new()`

#### Usage

    OutputLandscapeHabitat$new(
      path,
      Timestep = NULL,
      LocalVariables = NULL,
      DerivedLocalVariables = NULL,
      NeighborhoodVariables = NULL,
      ClimateVariables = NULL,
      DistanceVariables = NULL,
      SpeciesModels = NULL,
      LocalVarMapFileNames = NULL,
      NeighborVarMapFileNames = NULL,
      ClimateVarMapFileNames = NULL,
      DistanceVarMapFileNames = NULL,
      SpeciesMapFileNames = NULL,
      SpeciesLogFileNames = NULL,
      LogFile = "output/landscape-habitat/landscape_habitat_log.csv"
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Years between updates.

- `LocalVariables`:

  (Optional) Named list mapping reclass map names to data frames with
  columns `ForestType`, `AgeRange`, `Species`. Each row defines one
  forest-type/age/species rule.

- `DerivedLocalVariables`:

  (Optional) Named character vector. Names are derived-variable names;
  values are arithmetic expressions referencing the local variables
  (e.g. `"reclass2[LowlandCon] + reclass2[LowlandHdwd]"`).

- `NeighborhoodVariables`:

  (Optional) `data.frame` with columns `Name`, `LocalVar`,
  `NeighborRadius`, `Transform`.

- `ClimateVariables`:

  (Optional) `data.frame` with columns `Name`, `Year`, `Months`,
  `Source`, `ClimateVar`, `Transform`.

- `DistanceVariables`:

  (Optional) `data.frame` with columns `Name`, `LocalVar`, `Transform`.

- `SpeciesModels`:

  Named list. Each name is a species name; each value is a `data.frame`
  with columns `Parameter`, `Type`, `Value` describing the
  logistic-regression terms (use `"intercept"` for the intercept row).

- `LocalVarMapFileNames`:

  (Optional) Character. Output template for local-variable maps; must
  contain the literal placeholders `{local-var-name}` and `{timestep}`.

- `NeighborVarMapFileNames`:

  (Optional) Character. Output template for neighborhood maps; must
  contain the literal placeholders `{neighbor-var-name}` and
  `{timestep}`.

- `ClimateVarMapFileNames`:

  (Optional) Character. Output template for climate maps; must contain
  the literal placeholders `{climate-var-name}` and `{timestep}`.

- `DistanceVarMapFileNames`:

  (Optional) Character. Output template for distance maps; must contain
  the literal placeholders `{distance-var-name}` and `{timestep}`.

- `SpeciesMapFileNames`:

  (Optional) Character. Output template for species-model maps; must
  contain the literal placeholders `{species-name}` and `{timestep}`.

- `SpeciesLogFileNames`:

  (Optional) Character. Output template for species log files; must
  contain the literal placeholder `{species-name}`.

- `LogFile`:

  Character. Relative file path for the main log.

------------------------------------------------------------------------

### `OutputLandscapeHabitat$write()`

Write extension inputs to disk

#### Usage

    OutputLandscapeHabitat$write()

------------------------------------------------------------------------

### `OutputLandscapeHabitat$clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputLandscapeHabitat$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
