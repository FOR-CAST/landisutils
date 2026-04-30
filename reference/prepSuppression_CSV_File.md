# Prepare `Suppression_CSV_File` for Social-Climate-Fire extension

Prepare `Suppression_CSV_File` for Social-Climate-Fire extension

## Usage

``` r
prepSuppression_CSV_File(df, path, filename = "suppression.csv")
```

## Arguments

- df:

  data.frame corresponding to `Suppression_CSV_File` table

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character, specifying a filename (will be appended to `path`).

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.

## See also

Other Social Climate Fire helpers:
[`SocialClimateFire`](https://for-cast.github.io/landisutils/reference/SocialClimateFire.md),
[`insertDeadWoodTable()`](https://for-cast.github.io/landisutils/reference/insertDeadWoodTable.md),
[`insertLadderFuelSpeciesList()`](https://for-cast.github.io/landisutils/reference/insertLadderFuelSpeciesList.md),
[`prepTopographyFile()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md)
