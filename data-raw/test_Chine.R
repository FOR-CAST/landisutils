studyAreaName <- "Chine"

d_proj <- file.path("~/GitHub/BC_HRV")
d_ins <- file.path(d_proj, "inputs")
d_outs <- file.path(d_proj, "outputs")
d_runs <- file.path(
  d_outs,
  glue::glue("{studyAreaName}_landis_LH_hrv_NDTBEC_FRT_res125")
)

## sim files use relative path to inputs and outputs,
## so make sure it points to right place e.g., during tests
if (!dir.exists("inputs")) {
  file.symlink(d_ins, "inputs")
}
if (!dir.exists("outputs")) {
  file.symlink(d_outs, "outputs")
}

f1 <- file.path(d_runs, "simOutPreamble_Chine.rds")
f2 <- file.path(d_runs, "simOutDataPrep_Chine.rds")

sim1 <- SpaDES.core::loadSimList(f1)
sim2 <- SpaDES.core::loadSimList(f2)

## initial communities
Chine_cohortData <- sim2[["cohortData"]]
Chine_pixelGroupMap <- sim2[["pixelGroupMap"]] |>
  terra::wrap()

## ecoregion
Chine_ecoregion <- sim2[["ecoregion"]]
Chine_ecoregionMap <- sim2[["ecoregionMap"]] |>
  terra::wrap()
Chine_ecoregionPolys <- terra::as.polygons(Chine_ecoregionMap) |>
  sf::st_as_sf()
Chine_ecoregionPolys$ecoregion <- paste0(Chine_ecoregionPolys$ecoregion, "_81") ## append lcc code

## fireRegimePolys
Chine_fireRegimePolys <- sim2[["fireRegimePolys"]]

## other
Chine_minRelativeB <- sim2[["minRelativeB"]]
Chine_species <- sim2[["species"]]
Chine_speciesEcoregion <- sim2[["speciesEcoregion"]]
Chine_speciesLayers <- sim2[["speciesLayers"]] |>
  terra::wrap()
Chine_standAgeMap <- sim2[["standAgeMap"]] |>
  terra::crop(sim2[["speciesLayers"]]) |>
  terra::wrap()
Chine_sufficientLight <- sim2[["sufficientLight"]]

rm(sim1)
rm(sim2)

usethis::use_data(Chine_cohortData, overwrite = TRUE)
usethis::use_data(Chine_ecoregion, overwrite = TRUE)
usethis::use_data(Chine_ecoregionMap, overwrite = TRUE)
usethis::use_data(Chine_ecoregionPolys, overwrite = TRUE)
usethis::use_data(Chine_fireRegimePolys, overwrite = TRUE)
usethis::use_data(Chine_minRelativeB, overwrite = TRUE)
usethis::use_data(Chine_pixelGroupMap, overwrite = TRUE)
usethis::use_data(Chine_species, overwrite = TRUE)
usethis::use_data(Chine_speciesEcoregion, overwrite = TRUE)
usethis::use_data(Chine_speciesLayers, overwrite = TRUE)
usethis::use_data(Chine_standAgeMap, overwrite = TRUE)
usethis::use_data(Chine_sufficientLight, overwrite = TRUE)
