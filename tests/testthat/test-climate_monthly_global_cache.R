## Global per-cell monthly cache: a fixed reference grid gives stable GLOBAL CellIDs, so overlapping
## study-area pulls share one accumulating store and only ever fetch cells not already cached.

test_that("global per-cell cache fetches only missing cells across overlapping study areas", {
  skip_if_not_installed("terra")
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")
  skip_if_not_installed("BioSIM")

  withr::local_options(landisutils.cache.path = withr::local_tempdir())

  ## fixed reference grid, elevation values = cell number (arbitrary but valid). Built lon/lat then
  ## PROJECTED to BC Albers so the grid is projected like the real climate_ref_grid -- this exercises
  ## create_locations_df()'s reprojection of grid coords to lon/lat for BioSIM (longDeg/latDeg).
  ref_ll <- terra::rast(
    xmin = -123,
    xmax = -122,
    ymin = 53,
    ymax = 54,
    resolution = 0.1,
    crs = "EPSG:4326"
  )
  terra::values(ref_ll) <- as.numeric(seq_len(terra::ncell(ref_ll)))
  ref <- terra::project(ref_ll, "EPSG:3005")
  expect_false(terra::is.lonlat(ref)) ## guard: the grid must be projected for this test to mean anything

  mk_poly <- function(xmin, xmax, eco) {
    sf::st_sf(
      ecoregionGroup = eco,
      geometry = sf::st_sfc(
        sf::st_polygon(list(rbind(
          c(xmin, 53),
          c(xmax, 53),
          c(xmax, 53.6),
          c(xmin, 53.6),
          c(xmin, 53)
        ))),
        crs = 4326
      )
    )
  }
  saA <- mk_poly(-123, -122.4, "A") ## west
  saB <- mk_poly(-122.6, -122, "B") ## east; overlaps A in [-122.6, -122.4]

  ## record which CellIDs the (mocked) BioSIM fetch is asked for
  seen <- new.env()
  seen$ids <- integer(0)
  fake_fetch <- function(locations, year, rcp, clim_model) {
    seen$ids <- c(seen$ids, locations$ID)
    grid <- expand.grid(CellID = locations$ID, MONTH = 1:12)
    grid$YEAR <- year
    grid$TotalPrcp <- 50
    grid$MeanTmin <- -5
    grid$MeanTmax <- 5
    grid$MeanTair <- 0
    grid$MeanRelH <- 60
    grid$TotalRadiation <- 100
    grid$WndS <- 2
    grid$WndD <- 180
    grid
  }
  testthat::local_mocked_bindings(.fetch_clim_monthly_batch = fake_fetch, .package = "landisutils")

  cells_of <- function(sa) {
    sort(unique(do.call(rbind, create_locations_df(ref, sa, "ecoregionGroup"))$ID))
  }
  cellsA_all <- cells_of(saA)
  cellsB_all <- cells_of(saB)
  overlap <- intersect(cellsA_all, cellsB_all)
  expect_gt(length(overlap), 0) ## the study areas genuinely overlap

  ## run A first: fetches all of A's cells
  seen$ids <- integer(0)
  outA <- prep_monthly_weather_biosim(
    vars = c("temp", "prcp"),
    years = 2000L,
    studyArea = saA,
    id = "ecoregionGroup",
    ref_grid = ref
  )
  expect_setequal(sort(unique(seen$ids)), cellsA_all)

  ## run B next (overlaps A): fetches ONLY B's cells not already cached
  seen$ids <- integer(0)
  outB <- prep_monthly_weather_biosim(
    vars = c("temp", "prcp"),
    years = 2000L,
    studyArea = saB,
    id = "ecoregionGroup",
    ref_grid = ref
  )
  fetchedB <- sort(unique(seen$ids))
  expect_false(any(overlap %in% fetchedB)) ## overlap reused, not re-fetched
  expect_setequal(fetchedB, setdiff(cellsB_all, cellsA_all)) ## only the genuinely-new cells

  ## assembled output is well-formed and carries B's ecoregion
  expect_true(all(c("Year", "Month", "Variable") %in% names(outB)))
  expect_true("B" %in% names(outB))
})
