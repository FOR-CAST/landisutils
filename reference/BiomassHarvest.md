# Biomass Harvest Extension

Biomass Harvest Extension

## References

LANDIS-II Biomass Harvest v7 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Biomass-Harvest/blob/master/docs/LANDIS-II%20Harvest%20v7%20User%20Guide.pdf>

## See also

Other Biomass Harvest helpers:
[`harvestPrescription()`](https://for-cast.github.io/landisutils/reference/harvestPrescription.md),
[`insertCohortsRemoved()`](https://for-cast.github.io/landisutils/reference/insertCohortsRemoved.md),
[`insertEconomicRankTable()`](https://for-cast.github.io/landisutils/reference/insertEconomicRankTable.md),
[`insertFireHazardTable()`](https://for-cast.github.io/landisutils/reference/insertFireHazardTable.md),
[`insertForestTypeTable()`](https://for-cast.github.io/landisutils/reference/insertForestTypeTable.md),
[`insertHarvestImplementations()`](https://for-cast.github.io/landisutils/reference/insertHarvestImplementations.md),
[`insertMultipleRepeat()`](https://for-cast.github.io/landisutils/reference/insertMultipleRepeat.md),
[`insertPlant()`](https://for-cast.github.io/landisutils/reference/insertPlant.md),
[`insertPrescription()`](https://for-cast.github.io/landisutils/reference/insertPrescription.md),
[`insertSingleRepeat()`](https://for-cast.github.io/landisutils/reference/insertSingleRepeat.md),
[`insertSiteSelection()`](https://for-cast.github.io/landisutils/reference/insertSiteSelection.md),
[`insertStandRanking()`](https://for-cast.github.io/landisutils/reference/insertStandRanking.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `BiomassHarvest`

## Active bindings

- `ManagementAreas`:

  Character. Relative file path.

- `Stands`:

  Character. Relative file path.

- `Prescriptions`:

  List of `HarvestPrescription` objects.

- `HarvestImplementations`:

  `data.frame` with required columns `MgmtArea`, `Prescription`,
  `HarvestArea`, and optional columns `BeginTime`, `EndTime`.

- `PrescriptionMaps`:

  (Optional) Character. File pattern with literal `{timestep}`.

- `BiomassMaps`:

  (Optional) Character. File pattern with literal `{timestep}`.

- `EventLog`:

  Character. Relative file path.

- `SummaryLog`:

  Character. Relative file path.

- `output_files`:

  Character vector of output files (relative paths from the scenario
  directory) that this extension is expected to produce at run time.
  Subclasses override this to return their extension-specific log files,
  event CSVs, etc. These paths are collected by
  [`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md)
  and written to `output_manifest.txt` so
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  can track them explicitly without relying on
  [`list.files()`](https://rdrr.io/r/base/list.files.html) discovery.

  Map files whose names depend on the timestep (e.g. `BiomassC-10.tif`)
  are NOT included here; they are discovered by
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)'s
  `output_dir` scan instead.

## Methods

### Public methods

- [`BiomassHarvest$new()`](#method-BiomassHarvest-initialize)

- [`BiomassHarvest$add_prescription()`](#method-BiomassHarvest-add_prescription)

- [`BiomassHarvest$write()`](#method-BiomassHarvest-write)

- [`BiomassHarvest$clone()`](#method-BiomassHarvest-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `BiomassHarvest$new()`

#### Usage

    BiomassHarvest$new(
      path,
      Timestep = NULL,
      ManagementAreas = NULL,
      Stands = NULL,
      Prescriptions = list(),
      HarvestImplementations = NULL,
      PrescriptionMaps = NULL,
      BiomassMaps = NULL,
      EventLog = "biomass-harvest/log.csv",
      SummaryLog = "biomass-harvest/summarylog.csv"
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `ManagementAreas`:

  Character. Relative file path to the management areas raster.

- `Stands`:

  Character. Relative file path to the stands raster.

- `Prescriptions`:

  List of `HarvestPrescription` objects (see
  [`harvestPrescription()`](https://for-cast.github.io/landisutils/reference/harvestPrescription.md)).

- `HarvestImplementations`:

  `data.frame` with columns `MgmtArea`, `Prescription`, `HarvestArea`,
  and optionally `BeginTime` and `EndTime`.

- `PrescriptionMaps`:

  (Optional) Character. Filename pattern for prescription output maps;
  must contain the literal `{timestep}`.

- `BiomassMaps`:

  (Optional) Character. Filename pattern for biomass-removed output
  maps; must contain the literal `{timestep}`.

- `EventLog`:

  Character. Relative file path for the event-level CSV log.

- `SummaryLog`:

  Character. Relative file path for the summary CSV log.

------------------------------------------------------------------------

### `BiomassHarvest$add_prescription()`

#### Usage

    BiomassHarvest$add_prescription(value)

#### Arguments

- `value`:

  `HarvestPrescription` object to append to `$Prescriptions`.

------------------------------------------------------------------------

### `BiomassHarvest$write()`

Write extension inputs to disk

#### Usage

    BiomassHarvest$write()

------------------------------------------------------------------------

### `BiomassHarvest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BiomassHarvest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
