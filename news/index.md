# Changelog

## landisutils 0.0.3

- added support for the remaining LANDIS-II v8 extensions:
  - succession: DGS Succession, NECN Succession, PnET Succession;
  - disturbance: Biomass Browse, Biomass Harvest, Climate BDA, EDA
    (Epidemiological Disturbance Agent), Forest Roads Simulation,
    Hurricane, Land Use Plus, Linear Wind, Magic Harvest, Original Wind,
    Root Rot;
  - output: Output Biomass Community, Output Biomass-PnET, Output
    Biomass Reclass, Output Landscape Habitat, Output Local Habitat,
    Output Wildlife Habitat;
- added `Multi-Extension-Scenarios` vignette;
- miscellaneous updates and fixes to existing extensions.

## landisutils 0.0.2

- cache and batch weather data acquisition by year (#1);
- allow setting cache path using option `landisutils.cache.path`;
- change arguments `start` and `end` in `prep_*_weather()` to be integer
  years;
- use `R6` classes to track simulation input files;
- implemented scenario replication (#3);
- run LANDIS-II in background process via `callr`;

## landisutils 0.0.1

- initial version;
