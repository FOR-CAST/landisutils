studyAreaBC <- terra::vect(cbind(-122.14, 52.14), crs = "epsg:4326") |>
  terra::project(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
    "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")) |>
  SpaDES.tools::randomStudyArea(seed = 60, size = 1e10)

test_ecoregionPolys <- suppressWarnings({
  ## we can safely ignore the following warnings:
  ## "attribute variables are assumed to be spatially constant throughout all geometries"
  scfmutils::prepInputsFireRegimePolys(studyArea = studyAreaBC, type = "BECNDT")
})

usethis::use_data(test_ecoregionPolys, overwrite = TRUE)
