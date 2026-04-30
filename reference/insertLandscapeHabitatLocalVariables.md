# Specify the `LocalVariables` block for the Landscape Habitat extension

Specify the `LocalVariables` block for the Landscape Habitat extension

## Usage

``` r
insertLandscapeHabitatLocalVariables(x)
```

## Arguments

- x:

  Named list mapping reclass map names to `data.frame`s with columns
  `ForestType`, `AgeRange`, `Species`.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Landscape Habitat Output helpers:
[`OutputLandscapeHabitat`](https://for-cast.github.io/landisutils/reference/OutputLandscapeHabitat.md),
[`insertLandscapeHabitatClimateVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatClimateVariables.md),
[`insertLandscapeHabitatDerivedLocalVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatDerivedLocalVariables.md),
[`insertLandscapeHabitatDistanceVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatDistanceVariables.md),
[`insertLandscapeHabitatNeighborhoodVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatNeighborhoodVariables.md),
[`insertLandscapeHabitatSpeciesModels()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatSpeciesModels.md)
