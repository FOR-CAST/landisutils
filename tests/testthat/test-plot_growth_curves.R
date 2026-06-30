testthat::test_that("plot_species_growth_curves overlays a fitted line on PSP points, faceted by species", {
  curves <- data.frame(
    species = "Pseu_men",
    standAge = c(20L, 50L, 90L),
    BscaledNonLinear = c(2000, 8000, 12000)
  )
  psp <- data.frame(
    speciesTemp = c("Pseu_men", "Pseu_men", "Other"),
    standAge = c(30L, 60L, 40L),
    biomass = c(2500, 9000, 3000),
    ecoregion = c("SBSdw1", "SBSmw", "ICHmm")
  )

  p <- plot_species_growth_curves(curves, psp)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_length(p$layers, 2L)
  testthat::expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  testthat::expect_s3_class(p$layers[[2]]$geom, "GeomLine")
  testthat::expect_s3_class(p$facet, "FacetWrap")
})

testthat::test_that("plot_species_growth_curves errors on missing required columns", {
  testthat::expect_snapshot(
    error = TRUE,
    plot_species_growth_curves(data.frame(species = 1), data.frame(speciesTemp = 1))
  )
})
