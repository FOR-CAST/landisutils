# Run a LANDIS-II simulation from the R session

Run a LANDIS-II simulation from the R session

## Usage

``` r
landis_process(scenario_file, scenario_path, landis_console)

landis_run(scenario = NULL, rep = NULL, landis_console = NULL)
```

## Arguments

- scenario_file:

  character specifying the filename of a scenario file

- scenario_path:

  character specifying the path to the directory containing the scenario
  file

- landis_console:

  character, specifying path to LANDIS-II console executable.

- scenario:

  `LandisScenario` object

- rep:

  integer, replicate id

## Value

callr background R process

## Note

Users should call `landis_run()` (which wraps `landis_process`) rather
than calling `landis_process` directly.

## See also

- <https://callr.r-lib.org/index.html#background-r-processes>
