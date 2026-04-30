# Species Data File

Species Data File

## Usage

``` r
prepSpeciesData(df = NULL, type = NULL, path = NULL, filename = NULL)
```

## Arguments

- df:

  data.frame corresponding to the species data table

- type:

  character, corresponding to one of the following types: - "core":
  generates core species data (`.txt`) file; - "fire": generates `.csv`
  version for use with fire extensions; - "succession": generates `.csv`
  version for use with succession extensions;

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character, specifying a filename (will be appended to `path`).

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.

## See also

Used by succession extensions
([BiomassSuccession](https://for-cast.github.io/landisutils/reference/BiomassSuccession.md),
[DGSSuccession](https://for-cast.github.io/landisutils/reference/DGSSuccession.md),
[ForCS](https://for-cast.github.io/landisutils/reference/ForCS.md),
[NECNSuccession](https://for-cast.github.io/landisutils/reference/NECNSuccession.md))
when `type = "core"` or `"succession"`, and by fire extensions
([OriginalFire](https://for-cast.github.io/landisutils/reference/OriginalFire.md),
[SocialClimateFire](https://for-cast.github.io/landisutils/reference/SocialClimateFire.md))
when `type = "fire"`.
