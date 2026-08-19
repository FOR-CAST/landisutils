test_that("plot_growth_factorial_sensitivity drops parameters the design did not sweep", {
  scores <- expand.grid(
    growth_shp = c(0.2, 0.8),
    mort_shp = 15,
    anpp_prop = c(2, 5),
    species = "Sp1",
    stringsAsFactors = FALSE
  )
  scores$nrmse_shape <- seq_len(nrow(scores)) / 10
  current <- data.frame(
    species = "Sp1",
    growth_shp = 0.5,
    mort_shp = 23,
    anpp_max = 500,
    biomass_max = 10000
  )

  p <- plot_growth_factorial_sensitivity(scores, current)
  expect_setequal(levels(p$data$parameter), c("Growth shape", "Max. ANPP (% of max. biomass)"))

  scores$growth_shp <- 0.8
  scores$anpp_prop <- 5
  expect_snapshot(plot_growth_factorial_sensitivity(scores, current), error = TRUE)
})

test_that("a parameter fixed PER SPECIES is not mistaken for a swept one", {
  ## globally varied (10 and 25), locally constant -- the case the design actually produces
  scores <- rbind(
    expand.grid(
      growth_shp = c(0.2, 0.8),
      mort_shp = 10,
      anpp_prop = c(2, 5),
      species = "Sp1",
      stringsAsFactors = FALSE
    ),
    expand.grid(
      growth_shp = c(0.2, 0.8),
      mort_shp = 25,
      anpp_prop = c(2, 5),
      species = "Sp2",
      stringsAsFactors = FALSE
    )
  )
  scores$nrmse_shape <- seq_len(nrow(scores)) / 10
  current <- data.frame(
    species = c("Sp1", "Sp2"),
    growth_shp = 0.5,
    mort_shp = 23,
    anpp_max = 500,
    biomass_max = 10000
  )

  p <- plot_growth_factorial_sensitivity(scores, current)
  expect_false("Mortality shape" %in% levels(p$data$parameter))
})
