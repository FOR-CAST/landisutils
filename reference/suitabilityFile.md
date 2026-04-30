# Construct a habitat-suitability file reference

Wraps a path to a habitat-suitability definition file. Used by
[OutputLocalHabitat](https://for-cast.github.io/landisutils/reference/OutputLocalHabitat.md)
and
[OutputWildlifeHabitat](https://for-cast.github.io/landisutils/reference/OutputWildlifeHabitat.md).

## Usage

``` r
suitabilityFile(path)
```

## Arguments

- path:

  Character. Relative path (from the extension directory) to the
  suitability definition file.

## Value

A list of class `"SuitabilityFile"`.

## See also

Other Local Habitat Output helpers:
[`OutputLocalHabitat`](https://for-cast.github.io/landisutils/reference/OutputLocalHabitat.md),
[`insertSuitabilityFiles()`](https://for-cast.github.io/landisutils/reference/insertSuitabilityFiles.md)

Other Wildlife Habitat Output helpers:
[`OutputWildlifeHabitat`](https://for-cast.github.io/landisutils/reference/OutputWildlifeHabitat.md),
[`insertSuitabilityFiles()`](https://for-cast.github.io/landisutils/reference/insertSuitabilityFiles.md)
