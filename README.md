# landisutils

<!-- badges: start -->
![](man/figures/lifecycle-experimental.svg)
[![R-CMD-check](https://github.com/FOR-CAST/landisutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FOR-CAST/landisutils/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Tools and utilities for preparing data for and running LANDIS-II simulations, and post-processing their outputs.

1. prepare data in R and convert to LANDIS-II-compatible formats;
2. programmatically create and write LANDIS-II extension input files;
3. programmatically create and write LANDIS-II scenario and other simulation files;
4. extraction of data from output logs;
5. other post-processing tools;

> [!NOTE]
> **This package is a work in progress.**
> Testing, feedback and help are greatly appreciated!

The list of supported LANDIS-II extensions tracks the [official v8 extension list](https://www.landis-ii.org/extensions).
Tables below are grouped by extension type and sorted alphabetically; use the file outline (top-right on github.com) to jump between sections.

### Succession extensions

| **LANDIS-II extension name**                                                                              | **`landisutils` class** |
| --------------------------------------------------------------------------------------------------------- | ----------------------- |
| 🌳 [Biomass Succession](https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession)             | `BiomassSuccession`     |
| 🌱 [DGS Succession](https://github.com/LANDIS-II-Foundation/Extension-DGS-Succession)                     | `DGSSuccession`         |
| ♻️ [Forest Carbon Succession (ForCS)](https://github.com/LANDIS-II-Foundation/Extension-ForCS-Succession) | `ForCS`                 |
| 🍂 [NECN Succession](https://github.com/LANDIS-II-Foundation/Extension-NECN-Succession)                   | `NECNSuccession`        |
| ☀️ [PnET Succession](https://github.com/LANDIS-II-Foundation/Extension-PnET-Succession)                   | `PnETSuccession`        |

### Disturbance extensions

| **LANDIS-II extension name**                                                                                  | **`landisutils` class**   |
| ------------------------------------------------------------------------------------------------------------- | ------------------------- |
| 🔥 [BFOLDS Fire](https://www.fire-regime-model.com/BFOLDS%20Fire%20Regime%20Module%20v2.0%20User%20Guide.pdf) | _coming soon_             |
| 🦌 [Biomass Browse](https://github.com/LANDIS-II-Foundation/Extension-Biomass-Browse)                         | `BiomassBrowse` [^v7only] |
| 🪓 [Biomass Harvest](https://github.com/LANDIS-II-Foundation/Extension-Biomass-Harvest)                       | `BiomassHarvest`          |
| 🪲 [Climate Biological Disturbance Agents](https://github.com/LANDIS-II-Foundation/Extension-Base-BDA)        | `ClimateBDA`              |
| 🪵 [Dynamic Biomass Fuel System](https://github.com/LANDIS-II-Foundation/Extension-Dynamic-Biomass-Fuels)     | `DynamicFuels`            |
| 🔥 [Dynamic Fire System](https://github.com/LANDIS-II-Foundation/Extension-Dynamic-Fire-System)               | `DynamicFire`             |
| 🦠 [Epidemiological Disturbance Agents](https://github.com/LANDIS-II-Foundation/Extension-Base-EDA)           | `EDA`                     |
| 🌀 [Hurricanes](https://github.com/LANDIS-II-Foundation/Extension-Biomass-Hurricane)                          | `Hurricane`               |
| 🌪️ [Linear Wind](https://github.com/LANDIS-II-Foundation/Extension-LinearWind)                                | `LinearWind`              |
| ✨ [Magic Harvest](https://github.com/Klemet/LANDIS-II-Magic-Harvest)                                         | `MagicHarvest`            |
| 🔥 [Original Fire](https://github.com/LANDIS-II-Foundation/Extension-Base-Fire)                               | `OriginalFire`            |
| 💨 [Original Wind](https://github.com/LANDIS-II-Foundation/Extension-Base-Wind)                               | `OriginalWind`            |
| 🍄 [Root Rot](https://github.com/LANDIS-II-Foundation/Extension-Root-Rot)                                     | `RootRot` [^v7only]       |
| 🔥 [Social-Climate Fire](https://github.com/LANDIS-II-Foundation/Extension-Social-Climate-Fire)               | `SocialClimateFire`       |

### Other extensions

| **LANDIS-II extension name**                                                                                              | **`landisutils` class**  |
| ------------------------------------------------------------------------------------------------------------------------- | ------------------------ |
| 🌐 [Biomass Community Output](https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-Community)                 | `OutputBiomassCommunity` |
| 📊 [Biomass Output](https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass)                                     | `OutputBiomass`          |
| 🗂️ [Biomass Reclassification Output](https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-Reclass)            | `OutputBiomassReclass`   |
| 📅 [Biomass-by-Age Output](https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-By-Age)                       | `OutputBiomassByAge`     |
| 📈 [Cohort Statistics Output](https://github.com/LANDIS-II-Foundation/Extension-Output-Cohort-Statistics)                 | `OutputCohortStats`      |
| 📦 [Forest Product Sector](https://github.com/CarenD/Forest_Product_Sector_Module)                                        | _coming soon_            |
| 🛣️ [Forest Road Simulator](https://github.com/Klemet/LANDIS-II-Forest-Roads-Simulation-extension)                         | `ForestRoadsSimulation`  |
| 🗺️ [Land Use Plus](https://github.com/LANDIS-II-Foundation/Extension-Land-Use-Plus)                                       | `LandUsePlus`            |
| 🏞️ [Landscape Habitat Output](https://github.com/LANDIS-II-Foundation/Extension-Output-Landscape-Habitat)                 | `OutputLandscapeHabitat` |
| 🐿️ [Local Habitat Suitability Output](https://github.com/LANDIS-II-Foundation/Extension-Local-Habitat-Suitability-Output) | `OutputLocalHabitat`     |
| 🕰️ [Maximum Species Age Output](https://github.com/LANDIS-II-Foundation/Extension-Output-Max-Species-Age)                 | `OutputMaxSpeciesAge`    |
| ☀️ [PnET Output](https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-PnET)                                   | `OutputBiomassPnET`      |
| 🐾 [Wildlife Habitat Output](https://github.com/LANDIS-II-Foundation/Extension-Output-Wildlife-Habitat)                   | `OutputWildlifeHabitat`  |

## Installation

You can install the development version of `landisutils` like so:

``` r
remotes::install_github("FOR-CAST/landisutils")
```

## Preparing data

Use `prep*()` functions to convert input data to LANDIS-II data formats or auxiliary config files.

```r
## e.g., to prepare the initial communities .csv and raster files
init_comm_files <- prepInitialCommunities(cohortData, pixelGroupMap, tmp_pth)
```

## Creating LANDIS-II input files

For any of the supported extensions, use `new()` method to create an object which can be used to produce configuration files.
The configuration can be defined all at once, or built up in sequence, or modified to produce alternate configurations.
If producing several configurations, be sure to update the `path` for each configuration to ensure they will not be overwritten.

```r
## e.g., to prepare Biomass Succession inputs
ext_biomass_succession <- BiomassSuccession$new(
  path = tmp_pth,
  Timestep = 10,
  SeedingAlgorithm = "WardSeedDispersal",
  InitialCommunitiesFiles = init_comm_files,
  ClimateConfigFile = clim_file,
  CalibrateMode = NULL, ## optional
  SpinupCohorts = FALSE, ## optional; v7.1
  SpinupMortalityFraction = 0.05, ## v7.1
  MinRelativeBiomass = min_rel_b,
  SufficientLight = suff_light,
  SpeciesDataFile = spp_file,
  EcoregionParameters = erp_df,
  SpeciesEcoregionDataFile = spperd_file,
  FireReductionParameters = frp_df,
  HarvestReductionParameters = hrp_df
)
```

Once your configuration is ready to write to disk for use with LANDIS-II:

```r
## show files associated with this configuration
ext_biomass_succession$files

## write the main extension configuration / input file
ext_biomass_succession$write()
```

### Manual file creation (advanced use)

Use `insert*()` functions when generating LANDIS-II input text files manually.

``` r
insertInitialCommunities(init_comm_files)
```

## Creating LANDIS-II scenario files

Use `scenario()` to construct scenario files:

```r
## other extensions can be added to be run once created per above
scenario(
  cell_length = 250,
  duration = 200,
  extensions = list(
    succession = list(ext_biomass_succession),
    disturbance = list(),
    other = list()
  ),
  name = "my_scenario_a",
  path = file.path("path", "to", "project")
)
```

## Working with simulation outputs

_Coming soon!_

[^v7only]: Not yet updated by the LANDIS-II developers for LANDIS-II v8; the most recent upstream release targets the v7 core only. The `landisutils` R6 class tracks the latest published input-file schema (Biomass Browse v2.0, Root Rot v1.0), but `$write()`-produced files cannot currently be run through a LANDIS-II v8 console. Constructing one of these objects emits a one-time `warning()` to make this explicit.
