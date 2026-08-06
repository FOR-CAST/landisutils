# Validate a generated LANDIS-II scenario directory before running it

Checks a fully assembled scenario directory for the input defects that
LANDIS-II either reports unhelpfully or does not report at all. It is a
pure-R guard: no Docker, no simulation. Run it once per scenario, not
once per replicate – replicates copy an already-validated directory.

## Usage

``` r
validate_landis_scenario(
  path,
  scenario_file = "scenario.txt",
  error = TRUE,
  max_ic_csv_mb = 200
)
```

## Arguments

- path:

  Character. Path to the assembled scenario directory.

- scenario_file:

  Character. Name of the master scenario file within `path`. Defaults to
  `"scenario.txt"`.

- error:

  Logical. When `TRUE` (default), stop with every problem found. When
  `FALSE`, return them instead – use this to survey scenarios without
  failing, e.g. when introducing a new check.

- max_ic_csv_mb:

  Numeric. Size above which an initial-communities CSV is reported. The
  LANDIS-II parser builds one `ExpandoObject` per row and costs a large
  multiple of the file size, so a per-pixel (undeduplicated) snapshot
  aborts with `System.OutOfMemoryException` before the simulation
  starts.

## Value

Invisibly, a character vector of problems – empty when the scenario is
clean. With `error = TRUE` a non-empty result is raised instead.

## Details

Checks performed:

- **Existence and non-emptiness** of every input file referenced by
  `scenario.txt` and by each extension configuration it names. Files
  LANDIS-II will *write* are excluded, via `output_manifest.txt` and an
  internal list of output-naming directives.

- **Pixel type** of every map: LANDIS-II opens rasters through
  `Landis.RasterIO.Gdal.GdalInputRaster.NewInputBand`, which accepts
  only GDAL `Byte`, `Int16`, `Int32`, `Float32` and `Float64`. See
  [`landis_datatype()`](https://for-cast.github.io/landisutils/reference/landis_datatype.md).

- **Dimensions**: every map must match the ecoregions map.

- **Orientation**: a map stored in the wrong row order relative to the
  ecoregions map is detected by comparing per-cell mask agreement
  against the agreement its vertically flipped self would achieve. See
  below.

- **Initial-communities integrity**: every map code present in the
  raster resolves to rows in the initial-communities CSV, allowing the
  one deliberately row-less empty-community code that
  [`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md)
  creates; and the CSV is small enough that the LANDIS-II parser will
  not exhaust the container's memory reading it.

## Detecting a mirrored map

A map written in the wrong row order is the most dangerous defect in
this set, because nothing rejects it: dimensions, values and totals are
all correct, and the run completes with the vegetation displaced
relative to the ecoregion, fire-region and topography maps. It cost a
25-generation Dynamic Fire calibration.

Orientation metadata cannot catch it. The mirrored map is written back
by `terra` and is north-up; only its *content* is reversed. So the check
compares content: for each code map, the fraction of cells whose
active/inactive state matches the ecoregions map, against the same
fraction for the map's flipped self. A correctly oriented map scores
higher as-is; a mirrored one scores higher flipped, and flipping swaps
the pair exactly. No absolute threshold is involved, which is what makes
this robust: it is two measurements of the same landscape rather than a
tuned constant.

Measured on the two assembled BC_HRV scenarios, as-is versus flipped:
initial communities 0.9720/0.7605 and 0.9798/0.4945; fire ecoregions
1.0000/0.7799 and 1.0000/0.4940.

Note this is deliberately NOT the stricter "every active cell carries a
map code". Measured on those same working scenarios, 10,897 and 95,063
active ecoregion cells carry no initial-communities code – they are
cells with no cohorts, which Biomass Succession handles – so the strict
form would reject valid production input.

Maps read through
[`read_landis_raster()`](https://for-cast.github.io/landisutils/reference/read_landis_raster.md),
never
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
so the comparison is made in the row order LANDIS-II itself will read.

## See also

[`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md),
[`landis_datatype()`](https://for-cast.github.io/landisutils/reference/landis_datatype.md),
[`read_landis_raster()`](https://for-cast.github.io/landisutils/reference/read_landis_raster.md),
[`dedup_community_snapshot()`](https://for-cast.github.io/landisutils/reference/dedup_community_snapshot.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md),
[`landis_assert_version()`](https://for-cast.github.io/landisutils/reference/landis_assert_version.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`landis_target_version()`](https://for-cast.github.io/landisutils/reference/landis_target_version.md),
[`landis_version()`](https://for-cast.github.io/landisutils/reference/landis_version.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)

## Examples

``` r
if (FALSE) { # \dontrun{
validate_landis_scenario("LANDIS-II/hrv_biomass_fire/Chine")

## survey without failing, when introducing a new check
validate_landis_scenario(scenario_dir, error = FALSE)
} # }
```
