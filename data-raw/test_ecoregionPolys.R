## Package test data covering a small random study area in central BC.
## Run interactively (NOT during package build) when the BEC reference
## layer changes upstream.
## Requires SpaDES.tools (PredictiveEcology/SpaDES.tools@development),
## which is a *build-time* dep for this script only and are not needed at user runtime.

test_ecoregionPolys <- withr::with_options(
  list(reproducible.useCache = FALSE),
  suppressWarnings({
    ## ignorable: "attribute variables are assumed to be spatially constant
    ## throughout all geometries"
    studyArea <- terra::vect(cbind(-122.00, 52.14), crs = "epsg:4326") |>
      terra::project("epsg:3005") |> ## BC Albers for use with bcdata
      SpaDES.tools::randomStudyArea(seed = 60, size = 1e08) |>
      sf::st_as_sf()

    ecoregionPolys <- bcdata::bcdc_query_geodata("f358a53b-ffde-4830-a325-a5a03ff672c3") |>
      dplyr::filter(INTERSECTS(studyArea)) |>
      dplyr::collect() |>
      sf::st_set_agr("constant") |>
      sf::st_crop(studyArea) |>
      sf::st_intersection(studyArea) |>
      sf::st_make_valid() |>
      sf::st_transform(paste(
        "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
        "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      )) |>
      dplyr::group_by(MAP_LABEL) |>
      dplyr::summarise(geometry = sf::st_union(geometry)) |>
      dplyr::ungroup() |>
      sf::st_collection_extract(warn = FALSE) |>
      dplyr::mutate(PolyID = dplyr::row_number(), MAP_LABEL = NULL, .before = "geometry") |>
      sf::st_as_sf()

    ecoregionPolys
  })
)

# plot(test_ecoregionPolys)

usethis::use_data(test_ecoregionPolys, overwrite = TRUE)
