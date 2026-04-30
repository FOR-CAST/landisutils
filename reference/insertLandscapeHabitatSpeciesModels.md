# Specify the `SpeciesModels` block for the Landscape Habitat extension

Specify the `SpeciesModels` block for the Landscape Habitat extension

## Usage

``` r
insertLandscapeHabitatSpeciesModels(x)
```

## Arguments

- x:

  Named list whose names are species names and values are `data.frame`s
  with columns `Parameter`, `Type`, `Value` (one row per term, including
  `Parameter = "intercept"`).

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Landscape Habitat Output helpers:
[`OutputLandscapeHabitat`](https://for-cast.github.io/landisutils/reference/OutputLandscapeHabitat.md),
[`insertLandscapeHabitatClimateVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatClimateVariables.md),
[`insertLandscapeHabitatDerivedLocalVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatDerivedLocalVariables.md),
[`insertLandscapeHabitatDistanceVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatDistanceVariables.md),
[`insertLandscapeHabitatLocalVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatLocalVariables.md),
[`insertLandscapeHabitatNeighborhoodVariables()`](https://for-cast.github.io/landisutils/reference/insertLandscapeHabitatNeighborhoodVariables.md)
