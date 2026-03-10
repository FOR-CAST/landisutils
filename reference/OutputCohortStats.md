# Cohort Statistics Extension

Cohort Statistics Extension

Cohort Statistics Extension

## References

LANDIS-II Cohort Statistics v4 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Output-Cohort-Statistics/blob/master/docs/LANDIS-II%20Cohort%20Statistics%20v4%20User%20Guide.pdf>

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OutputCohortStats`

## Active bindings

- `SpeciesAgeStats`:

  Named character vector specifying the `stats` and `species`.

- `SpeciesAgeMapNames`:

  Character. File pattern for writing outputs to disk.

- `SiteAgeStats`:

  Named character vector specifying the `stats`.

- `SiteAgeMapNames`:

  Character. File pattern for writing outputs to disk.

- `SiteSpeciesStats`:

  Named character vector specifying the `stats`.

- `SiteSpeciesMapNames`:

  Character. File pattern for writing outputs to disk.

## Methods

### Public methods

- [`OutputCohortStats$new()`](#method-OutputCohortStats-new)

- [`OutputCohortStats$write()`](#method-OutputCohortStats-write)

- [`OutputCohortStats$clone()`](#method-OutputCohortStats-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    OutputCohortStats$new(
      path = NULL,
      Timestep = 10L,
      SpeciesAgeMapNames = NULL,
      SpeciesAgeStats = list(species = NULL, stats = NULL),
      SiteAgeMapNames = NULL,
      SiteAgeStats = list(stats = NULL),
      SiteSpeciesMapNames = NULL,
      SiteSpeciesStats = list(stats = NULL)
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `SpeciesAgeMapNames`:

  Character. File pattern for writing outputs to disk.

- `SpeciesAgeStats`:

  Named list specifying the `species` and `stats` (e.g.,
  `list(species = c("querrub", "pinustro"), stats = c("MAX", "MIN"))`)

- `SiteAgeMapNames`:

  Character. File pattern for writing outputs to disk.

- `SiteAgeStats`:

  Named list specifying the `stats` (e.g.,
  `list(stats = c("MAX", "RICH"))`)

- `SiteSpeciesMapNames`:

  Character. File pattern for writing outputs to disk.

- `SiteSpeciesStats`:

  Named list specifying the `stats` (e.g., `list(stats = c("RICH"))`).

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    OutputCohortStats$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputCohortStats$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# cohort_statistics <- OutputCohortStats$new(
#   path = tempdir(),
#   Timestep = 10,
#   SpeciesAgeStats = list(species = c("querrub", "pinustro"), stats = c("MAX")),
#   SiteAgeStats = list(stats = c("MAX", "MED", "SD", "RICH", "EVEN")),
#   SiteSpeciesStats = list(stats = c("RICH"))
# )
#
# cohort_satistics$write()
#
```
