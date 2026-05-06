# PnET Biomass Output Extension

Companion output extension to
[PnETSuccession](https://for-cast.github.io/landisutils/reference/PnETSuccession.md);
both share the same user guide. §11 of the v6.0 guide documents the
`LandisData "Output-PnET"` input file format and the supported output
keywords (e.g. `Biomass`, `LeafAreaIndex`, the `Monthly*` keywords,
etc.); §11.5 covers the output-file-name templates and the `{species}` /
`{timestep}` placeholders.

## References

LANDIS-II PnET-Succession v6.0 Extension User Guide (§11 covers the
Output-PnET input file)
<https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-PnET/blob/master/docs/LANDIS-II%20PnET-Succession%20v6.0%20User%20Guide%20Jan21%202026.pdf>

## See also

[PnETSuccession](https://for-cast.github.io/landisutils/reference/PnETSuccession.md)
(this extension's required succession backend; output fields here
consume PnET-specific cohort state).

Other PnET Output helpers:
[`defaultPnETOutputFiles()`](https://for-cast.github.io/landisutils/reference/defaultPnETOutputFiles.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OutputBiomassPnET`

## Active bindings

- `Species`:

  Character vector of species codes, or `"all"` / `"none"`.

- `Outputs`:

  Named list mapping LANDIS-II output keywords to file patterns.

## Methods

### Public methods

- [`OutputBiomassPnET$new()`](#method-OutputBiomassPnET-initialize)

- [`OutputBiomassPnET$write()`](#method-OutputBiomassPnET-write)

- [`OutputBiomassPnET$clone()`](#method-OutputBiomassPnET-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `OutputBiomassPnET$new()`

#### Usage

    OutputBiomassPnET$new(path, Timestep = NULL, Species = "all", Outputs = NULL)

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Years between output snapshots.

- `Species`:

  Character vector of species codes, or `"all"` / `"none"`.

- `Outputs`:

  Named list mapping LANDIS-II output keywords (any of `Biomass`,
  `BelowgroundBiomass`, `WoodBiomass`, `FoliageBiomass`, `RootBiomass`,
  `WoodySenescence`, `FoliageSenescence`, `AgeDistribution`,
  `CohortsPerSpecies`, `CohortBalance`, `LeafAreaIndex`,
  `MonthlyAvgLAI`, `Water`, `MonthlyAvgWater`, `MonthlyEvap`,
  `MonthlyInterception`, `MonthlyActualTrans`, `MonthlyPotentialTrans`,
  `MonthlyPotentialEvap`, `MonthlyLeakage`, `MonthlyRunoff`,
  `MonthlyAET`, `AET`, `AETAvg`, `PET`, `AnnualPsn`, `MonthlyNetPsn`,
  `MonthlyGrossPsn`, `MonthlyFolResp`, `MonthlyMaintResp`, `NSC`,
  `WoodyDebris`, `Litter`, `SubCanopyPAR`, `Albedo`,
  `MonthlyActiveLayerDepth`, `MonthlyFrostDepth`, `MonthlyAvgSnowPack`,
  `EstablishmentTable`, `EstablishmentProbability`, `Establishment`,
  `MortalityTable`, `SiteMossDepth`) to character file patterns. Each
  pattern may contain `{timestep}`, `{species}`, `{month}`, or `{year}`
  placeholders depending on the keyword. Use
  [`defaultPnETOutputFiles()`](https://for-cast.github.io/landisutils/reference/defaultPnETOutputFiles.md)
  for a sensible default set.

------------------------------------------------------------------------

### `OutputBiomassPnET$write()`

Write extension inputs to disk

#### Usage

    OutputBiomassPnET$write()

------------------------------------------------------------------------

### `OutputBiomassPnET$clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputBiomassPnET$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
