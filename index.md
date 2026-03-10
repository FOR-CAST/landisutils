# landisutils

Tools and utilities for preparing data for and running LANDIS-II
simulations, and post-processing their outputs.

1.  prepare data in R and convert to LANDIS-II-compatible formats;
2.  programmatically create and write LANDIS-II extension input files;
3.  programmatically create and write LANDIS-II scenario and other
    simulation files;
4.  extraction of data from output logs;
5.  other post-processing tools;

> \[!NOTE\] **This package is a work in progress.** Testing, feedback
> and help are greatly appreciated!

The following LANDIS-II extensions are currently supported:

| **LANDIS-II Succession Extension** | **`landisutils` class** |
|------------------------------------|-------------------------|
| Biomass Succession                 | `BiomassSuccession`     |
| Forest Carbon Succession (ForCS)   | `ForCS`                 |
| NECN Succession                    | *coming soon*           |
| PnET Succession                    | *coming soon*           |
| DGS Succession                     | *coming soon*           |

| **LANDIS-II Disturbance Extension**   | **`landisutils` class** |
|---------------------------------------|-------------------------|
| Original Fire                         | `OriginalFire`          |
| Dynamic Biomass Fuels                 | `DynamicFuels`          |
| Dynamic Fire System                   | `DynamicFire`           |
| Social-Climate-Fire                   | `SocialClimateFire`     |
|                                       |                         |
| Linear Wind                           | *coming soon*           |
| Original Wind                         | *coming soon*           |
|                                       |                         |
| Climate Biological Disturbance Agents | *coming soon*           |
| Epidemiological Disturbance Agents    | *coming soon*           |
| Hurricanes                            | *coming soon*           |
| Land Use Plus                         | *coming soon*           |
| DGS Succession                        | *coming soon*           |

| **LANDIS-II Other Extension**    | **`landisutils` class** |
|----------------------------------|-------------------------|
| Biomass Output                   | `OutputBiomass`         |
| Biomass-by-Age Output            | `OutputBiomassByAge`    |
| Biomass Reclassification Output  | *coming soon*           |
| Cohort Statistics Output         | `OutputCohortStats`     |
| Landscape Habitat Output         | *coming soon*           |
| Local Habitat Suitability Output | *coming soon*           |
| Maximum Species Age Output       | `OutputMaxSpeciesAge`   |
| PnET Output                      | *coming soon*           |

## Installation

You can install the development version of `landisutils` like so:

``` r
remotes::install_github("FOR-CAST/landisutils")
```

## Preparing data

Use `prep*()` functions to convert input data to LANDIS-II data formats
or auxiliary config files.

``` r
## e.g., to prepare the initial communities .csv and raster files
init_comm_files <- prepInitialCommunities(cohortData, pixelGroupMap, tmp_pth)
```

## Creating LANDIS-II input files

For any of the supported extensions, use `new()` method to create an
object which can be used to produce configuration files. The
configuration can be defined all at once, or built up in sequence, or
modified to produce alternate configurations. If producing several
configurations, be sure to update the `path` for each configuration to
ensure they will not be overwritten.

``` r
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

Once your configuration is ready to write to disk for use with
LANDIS-II:

``` r
## show files associated with this configuration
ext_biomass_succession$files

## write the main extension configuration / input file
ext_biomass_succession$write()
```

### Manual file creation (advanced use)

Use `insert*()` functions when generating LANDIS-II input text files
manually.

``` r
insertInitialCommunities(init_comm_files)
```

## Creating LANDIS-II scenario files

Use
[`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md)
to construct scenario files:

``` r
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

*Coming soon!*
