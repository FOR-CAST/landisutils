# landisutils 0.0.5

* added new climate-data backends for use with the LANDIS-II Climate Library:
    - daily and monthly weather from BioSIM via the `BioSIM` package
      (`prep_daily_weather()`, `prep_monthly_weather_biosim()`);
    - monthly weather from `climr` (`prep_monthly_weather_climr()`),
      including the bcgov-recommended 8-member GCM ensemble
      (`climr_ensemble_8`);
    - monthly weather from TerraClim via `climateR`
      (`prep_monthly_weather()`);
* exported the lower-level fetch and assembly helpers used by the above
  (`get_clim_daily()`, `get_clim_monthly()`, `get_clim_monthly_climr()`,
  `get_clim_monthly_terraclim()`, `get_fwi_daily()`, `get_elevation_rast()`,
  `create_locations_df()`, and the `assemble_climate_library_file*()` family);
* climate caches are now namespaced by a study-area hash so distinct study
  areas don't collide;
* added `test_ecoregionPolys` dataset to support examples and tests;
* reworked the `climate-data` vignette to demonstrate the new backends;
* fixed `BiomassSuccession` R6 class name (was `"DynamicFuels"`);
* added `cffdrs` and `digest` to `Imports`;
* added `arrow`, `BioSIM`, and `climr` to `Suggests`.

# landisutils 0.0.4

* improve docker integration tests;

# landisutils 0.0.3

* added support for the remaining LANDIS-II v8 extensions:
    - succession: DGS Succession, NECN Succession, PnET Succession;
    - disturbance: Biomass Browse, Biomass Harvest, Climate BDA, EDA
      (Epidemiological Disturbance Agent), Forest Roads Simulation, Hurricane,
      Land Use Plus, Linear Wind, Magic Harvest, Original Wind, Root Rot;
    - output: Output Biomass Community, Output Biomass-PnET, Output Biomass
      Reclass, Output Landscape Habitat, Output Local Habitat, Output Wildlife
      Habitat;
* added `Multi-Extension-Scenarios` vignette;
* miscellaneous updates and fixes to existing extensions.

# landisutils 0.0.2

* cache and batch weather data acquisition by year (#1);
* allow setting cache path using option `landisutils.cache.path`;
* change arguments `start` and `end` in `prep_*_weather()` to be integer years;
* use `R6` classes to track simulation input files;
* implemented scenario replication (#3);
* run LANDIS-II in background process via `callr`;

# landisutils 0.0.1

* initial version;
