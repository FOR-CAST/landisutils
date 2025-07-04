# Chine ---------------------------------------------------------------------------------------

#' Test objects for Chine landscape unit
#'
#' @format ## `Chine_cohortData`
#' A `data.table` with columns `speciesCode`, `ecoregionGroup`, `age`, `B`,
#' `pixelGroup`, `totalBiomass`.
#'
#' @rdname Chine_data
"Chine_cohortData"

#' @format ## `Chine_pixelGroupMap`
#' A `SpatRaster` with `pixelGroup` ids.
#'
#' @rdname Chine_data
"Chine_pixelGroupMap"

#' @format ## `Chine_ecoregion`
#' A `data.table` with columns: `active`, `ecoregionGroup`.
#'
#' @rdname Chine_data
"Chine_ecoregion"

#' @format ## `Chine_ecoregionMap`
#' A `SpatRaster` with ecoregion values.
#' Use `terra::unwrap()` to work with this raster.
#'
#' @rdname Chine_data
"Chine_ecoregionMap"

#' @format ## `Chine_ecoregionPolys`
#' A `SpatVector` with 10 polygons geometry and 1 attribute (`field`).
#'
#' @rdname Chine_data
"Chine_ecoregionPolys"

#' @format ## `Chine_fireRegimePolys`
#' A `SpatVector` with 2 polygons geometries and 36 attributes (`fields`).
#'
#' @rdname Chine_data
"Chine_fireRegimePolys"

#' @format ## `Chine_minRelativeB`
#' A `data.frame` with columns: `ecoregionGroup`, `X1`, `X2`, `X3`, `X4`, `X5`.
#'
#' @rdname Chine_data
"Chine_minRelativeB"

#' @format ## `Chine_species`
#' A `data.table` with columns: `species`, `Area`, `longevity`, `sexualmature`,
#' `shadetolerance`, `firetolerance`, `seeddistance_eff`, `seeddistance_max`,
#' `resproutprob`, `resproutage_min`, `resproutage_max`, `postfireregen`, `leaflongevity`,
#' `wooddecayrate`, `mortalityshape`, `growthcurve`, `leafLignin`, `hardsoft`, `speciesCode`,
#' `mANPPproportion`, `inflationFactor`, `growthCurveSource`.
#'
#' @rdname Chine_data
"Chine_species"

#' @format ## `speciesCode`
#' A `data.table` with columns: `ecoregionGroup`, `speciesCode`, `establishprob`, `maxB`,
#' `maxANPP`, `year`.
#'
#' @rdname Chine_data
"Chine_speciesEcoregion"

#' @format ## `Chine_speciesLayers`
#' A `SpatRaster` with 12 layers corresponding to species percent-cover.
#' Use `terra::unwrap()` to work with this raster.
#'
#' @rdname Chine_data
"Chine_speciesLayers"

#' @format ## `Chine_standAgeMap`
#' A `SpatRaster` with stand age values.
#' Use `terra::unwrap()` to work with this raster.
#'
#' @rdname Chine_data
"Chine_standAgeMap"

#' @format ## `Chine_sufficientLight`
#' A `data.frame` with rows corresponding to shade tolerance class, and columns:
#' `speciesshadetolerance`, `X0`, `X1`, `X2`, `X3`, `X4`, `X5`.
#'
#' @rdname Chine_data
"Chine_sufficientLight"

# other ---------------------------------------------------------------------------------------

#' Test ecoregion polygons
#'
#' For a random BC study area using BEC zones.
#'
#' @format ## `test_ecoregionPolys`
#' A `SpatVector` with 1 polygons geometry and 1 attribute (`field`)
#'
"test_ecoregionPolys"
