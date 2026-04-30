# Write a per-agent Climate BDA input file

Write a per-agent Climate BDA input file

## Usage

``` r
writeAgentFile(agent, base_path)
```

## Arguments

- agent:

  `BDAAgent` object (see
  [`bdaAgent()`](https://for-cast.github.io/landisutils/reference/bdaAgent.md)).

- base_path:

  Character. Path to the extension's directory; the per-agent file is
  written to `<base_path>/bda/<agent$name>.txt`.

## Value

Character. The relative path to the written file (relative to
`base_path`), suitable for inclusion in the main file's `BDAInputFiles`
block.

## See also

Other Climate BDA helpers:
[`ClimateBDA`](https://for-cast.github.io/landisutils/reference/ClimateBDA.md),
[`bdaAgent()`](https://for-cast.github.io/landisutils/reference/bdaAgent.md),
[`insertAgentHeader()`](https://for-cast.github.io/landisutils/reference/insertAgentHeader.md),
[`insertBDAInputFiles()`](https://for-cast.github.io/landisutils/reference/insertBDAInputFiles.md),
[`insertBDASpeciesParameters()`](https://for-cast.github.io/landisutils/reference/insertBDASpeciesParameters.md),
[`insertDispersal()`](https://for-cast.github.io/landisutils/reference/insertDispersal.md),
[`insertDisturbanceModifiers()`](https://for-cast.github.io/landisutils/reference/insertDisturbanceModifiers.md),
[`insertEcoregionModifiers()`](https://for-cast.github.io/landisutils/reference/insertEcoregionModifiers.md),
[`insertIgnoredSpecies()`](https://for-cast.github.io/landisutils/reference/insertIgnoredSpecies.md),
[`insertIntensityClasses()`](https://for-cast.github.io/landisutils/reference/insertIntensityClasses.md),
[`insertNeighborhood()`](https://for-cast.github.io/landisutils/reference/insertNeighborhood.md),
[`insertOutbreakPattern()`](https://for-cast.github.io/landisutils/reference/insertOutbreakPattern.md)
