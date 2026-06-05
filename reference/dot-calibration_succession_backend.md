# Detect the succession backend of a (calibration) scenario directory and return the per-backend bits the calibration spinup/template need: the LandisData extension name, the config filename, the calibration freeze/spinup patcher, and any fixed-name succession logs to track. ForCS and Biomass Succession are supported; the rest of the calibration setup (Output Biomass Community snapshot, Dynamic Fire/Fuels, fire logs) is backend-independent.

Detect the succession backend of a (calibration) scenario directory and
return the per-backend bits the calibration spinup/template need: the
LandisData extension name, the config filename, the calibration
freeze/spinup patcher, and any fixed-name succession logs to track.
ForCS and Biomass Succession are supported; the rest of the calibration
setup (Output Biomass Community snapshot, Dynamic Fire/Fuels, fire logs)
is backend-independent.

## Usage

``` r
.calibration_succession_backend(dir)
```
