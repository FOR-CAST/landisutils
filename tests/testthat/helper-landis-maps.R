## Builders for a minimal but REAL LANDIS-II scenario directory.
##
## Unlike the stub fixtures used elsewhere in this suite (zero-byte files created only so that
## `add_file()`'s existence check passes), these write actual rasters and actual CSVs, because
## validate_landis_scenario() reads pixel types, dimensions and map codes. They live in a helper
## because testthat scopes top-level definitions per file.

## A small, deliberately ASYMMETRIC active footprint: active in the top rows only, so a vertically
## mirrored map disagrees with it strongly. A symmetric footprint would make the orientation check
## untestable (and, on a real landscape, undiagnosable).
.landis_test_mask <- function(nr = 6L, nc = 5L) {
  m <- matrix(FALSE, nrow = nr, ncol = nc)
  m[seq_len(ceiling(nr / 2)), ] <- TRUE
  m[1L, nc] <- FALSE ## break left-right symmetry too
  m
}

.landis_write_map <- function(file, values, nr, nc, datatype = NULL) {
  r <- terra::rast(
    nrows = nr,
    ncols = nc,
    xmin = 0,
    xmax = nc * 100,
    ymin = 0,
    ymax = nr * 100,
    crs = "EPSG:3005"
  )
  terra::values(r) <- as.integer(values)
  if (is.null(datatype)) {
    datatype <- landis_datatype(max(values, na.rm = TRUE))
  }
  terra::writeRaster(r, file, datatype = datatype, overwrite = TRUE)
  invisible(file)
}

#' Materialise a minimal valid Biomass Succession scenario directory
#'
#' Returns the directory path. The caller can then break exactly one thing and
#' assert that `validate_landis_scenario()` reports it.
#'
#' @noRd
local_landis_scenario <- function(nr = 6L, nc = 5L, .local_envir = parent.frame()) {
  dir <- withr::local_tempdir("test_validate_", .local_envir = .local_envir)
  mask <- .landis_test_mask(nr, nc)
  n <- nr * nc

  ## ecoregion codes 1/2 on active cells, 0 elsewhere (the LANDIS-II inactive convention)
  eco <- ifelse(as.vector(t(mask)), rep(c(1L, 2L), length.out = n), 0L)
  .landis_write_map(fs::path(dir, "ecoregions.tif"), eco, nr, nc)

  ## initial-communities codes 1..3 on active cells; every code resolves in the CSV
  ic <- ifelse(as.vector(t(mask)), rep(c(1L, 2L, 3L), length.out = n), 0L)
  .landis_write_map(fs::path(dir, "initial-communities.tif"), ic, nr, nc)
  data.table::fwrite(
    data.frame(
      MapCode = c(1L, 1L, 2L, 3L),
      SpeciesName = c("abiebals", "picemari", "abiebals", "pinubank"),
      CohortAge = c(10L, 30L, 50L, 20L),
      CohortBiomass = c(500L, 1200L, 3000L, 800L)
    ),
    fs::path(dir, "initial-communities.csv")
  )

  ## a continuous map: 0 is a legitimate value ON active cells, which is why it must be exempt from
  ## the orientation check
  slope <- ifelse(as.vector(t(mask)), rep(c(0L, 0L, 12L), length.out = n), 0L)
  .landis_write_map(fs::path(dir, "ground_slope.tif"), slope, nr, nc)

  ## fire regions must cover every cell the core considers active
  fire <- ifelse(as.vector(t(mask)), 1L, 0L)
  .landis_write_map(fs::path(dir, "fire-ecoregions.tif"), fire, nr, nc)

  writeLines(
    c(
      'LandisData  "Ecoregions"',
      ">> Active  Code  Name   Description",
      "   yes     1     eco1   first",
      "   yes     2     eco2   second"
    ),
    fs::path(dir, "ecoregions.txt")
  )
  writeLines(
    c(
      'LandisData  "Species"',
      ">> Name      Longevity",
      "   abiebals  200",
      "   picemari  250",
      "   pinubank  100"
    ),
    fs::path(dir, "species.txt")
  )
  writeLines(
    c(
      'LandisData  "Biomass Succession"',
      "Timestep    10",
      "InitialCommunities      initial-communities.csv",
      "InitialCommunitiesMap   initial-communities.tif"
    ),
    fs::path(dir, "biomass-succession.txt")
  )
  writeLines(
    c(
      'LandisData  "Dynamic Fire System"',
      "Timestep    1",
      "InitialFireEcoregionsMap   fire-ecoregions.tif",
      "GroundSlopeFile            ground_slope.tif",
      "",
      "SeasonTable",
      ">> Name      Status    Fire       Curing   Proportion",
      ">> ---------------------------------------------------",
      ## dyadic: 32/128, 64/128, 32/128 -- exact in single precision, sums to exactly 1
      "Spring    LeafOff    0.25         0    1",
      "Summer    LeafOn     0.50        51    1",
      "Fall      LeafOff    0.25       100    1",
      ""
    ),
    fs::path(dir, "dynamic-fire.txt")
  )
  writeLines(
    c(
      'LandisData  "Dynamic Fuel System"',
      "Timestep    1",
      "",
      "FuelTypes",
      ">> Fuel Type    Base Fuel    Age Range    Species",
      ">> ---------    ---------    ---------    ----------------",
      "2    Conifer      0 to 500    abiebals  picemari",
      "3    Conifer      0 to 900    pinubank",
      ""
    ),
    fs::path(dir, "dynamic-fuels.txt")
  )

  write_landis_scenario_file(
    path = dir,
    duration = 10L,
    cell_length = 100L,
    species_file = fs::path(dir, "species.txt"),
    ecoregions_files = c(fs::path(dir, "ecoregions.txt"), fs::path(dir, "ecoregions.tif")),
    succession_ext_files = c("Biomass Succession" = fs::path(dir, "biomass-succession.txt")),
    disturbance_ext_files = c(
      "Dynamic Fire System" = fs::path(dir, "dynamic-fire.txt"),
      "Dynamic Fuel System" = fs::path(dir, "dynamic-fuels.txt")
    ),
    output_manifest = c("Landis-log.txt", "fire/dynamic-fire-event-log.csv"),
    validate = FALSE ## the caller decides when to validate
  )

  invisible(as.character(dir))
}
