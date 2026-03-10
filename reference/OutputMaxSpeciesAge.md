# Max Species Age Extension

Max Species Age Extension

Max Species Age Extension

## References

LANDIS-II Output Max Species Age v4 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Output-Max-Species-Age/blob/master/docs/LANDIS-II%20Output%20Max%20Species%20Age%20v4%20User%20Guide.pdf>

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OutputMaxSpeciesAge`

## Active bindings

- `MapNames`:

  Character. File pattern for writing outputs to disk.

- `Species`:

  Character vector of species names, or "all".

## Methods

### Public methods

- [`OutputMaxSpeciesAge$new()`](#method-OutputMaxSpeciesAge-new)

- [`OutputMaxSpeciesAge$write()`](#method-OutputMaxSpeciesAge-write)

- [`OutputMaxSpeciesAge$clone()`](#method-OutputMaxSpeciesAge-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    OutputMaxSpeciesAge$new(path, Timestep = 10, MapNames = NULL, Species = NULL)

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `MapNames`:

  Character. File pattern for writing outputs to disk.

- `Species`:

  Character vector of species names, or "all".

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    OutputMaxSpeciesAge$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputMaxSpeciesAge$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
max_species_age <- OutputMaxSpeciesAge$new(
  path = tempdir(),
  Timestep = 10,
  MapNames = NULL, # use default
  Species = c("pinubank", "acersacc", "tiliamer")
)

max_species_age$write()
```
