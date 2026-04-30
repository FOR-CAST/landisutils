# Write a per-agent EDA input file

Write a per-agent EDA input file

## Usage

``` r
writeEDAAgentFile(agent, base_path)
```

## Arguments

- agent:

  `EDAAgent` object (see
  [`edaAgent()`](https://for-cast.github.io/landisutils/reference/edaAgent.md)).

- base_path:

  Character. Path to the extension's directory; the per- agent file is
  written to `<base_path>/eda/<agent$name>.txt`.

## Value

Character. The relative path to the written file.

## See also

Other EDA helpers:
[`EDA`](https://for-cast.github.io/landisutils/reference/EDA.md),
[`edaAgent()`](https://for-cast.github.io/landisutils/reference/edaAgent.md),
[`insertEDADisturbanceModifiers()`](https://for-cast.github.io/landisutils/reference/insertEDADisturbanceModifiers.md),
[`insertEDAIgnoredSpecies()`](https://for-cast.github.io/landisutils/reference/insertEDAIgnoredSpecies.md),
[`insertEDAInputFiles()`](https://for-cast.github.io/landisutils/reference/insertEDAInputFiles.md),
[`insertEDASpeciesParameters()`](https://for-cast.github.io/landisutils/reference/insertEDASpeciesParameters.md)
