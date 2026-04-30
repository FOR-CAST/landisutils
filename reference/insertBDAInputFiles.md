# Specify the `BDAInputFiles` block in the main Climate BDA file

Specify the `BDAInputFiles` block in the main Climate BDA file

## Usage

``` r
insertBDAInputFiles(files)
```

## Arguments

- files:

  Character vector of relative paths to per-agent input files. The
  keyword is emitted on the same line as the first file; subsequent
  files appear on subsequent lines (matches the reference parser in
  `Extension-Base-BDA/src/InputParameterParser.cs`).

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Climate BDA helpers:
[`ClimateBDA`](https://for-cast.github.io/landisutils/reference/ClimateBDA.md),
[`bdaAgent()`](https://for-cast.github.io/landisutils/reference/bdaAgent.md),
[`insertAgentHeader()`](https://for-cast.github.io/landisutils/reference/insertAgentHeader.md),
[`insertBDASpeciesParameters()`](https://for-cast.github.io/landisutils/reference/insertBDASpeciesParameters.md),
[`insertDispersal()`](https://for-cast.github.io/landisutils/reference/insertDispersal.md),
[`insertDisturbanceModifiers()`](https://for-cast.github.io/landisutils/reference/insertDisturbanceModifiers.md),
[`insertEcoregionModifiers()`](https://for-cast.github.io/landisutils/reference/insertEcoregionModifiers.md),
[`insertIgnoredSpecies()`](https://for-cast.github.io/landisutils/reference/insertIgnoredSpecies.md),
[`insertIntensityClasses()`](https://for-cast.github.io/landisutils/reference/insertIntensityClasses.md),
[`insertNeighborhood()`](https://for-cast.github.io/landisutils/reference/insertNeighborhood.md),
[`insertOutbreakPattern()`](https://for-cast.github.io/landisutils/reference/insertOutbreakPattern.md),
[`writeAgentFile()`](https://for-cast.github.io/landisutils/reference/writeAgentFile.md)
