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

test_that("plot_growth_calibration(density = TRUE) bins the plot cloud instead of drawing points", {
  skip_if_not_installed("hexbin")
  reference <- tibble::tibble(
    source = "Ground plots",
    age = seq(20, 200, length.out = 60),
    aboveground_c_mg_ha = seq(10, 180, length.out = 60),
    bec_label = "ICHmc",
    leading_raw = "PL",
    plot_weight = seq(0.05, 1, length.out = 60)
  )
  curve <- tibble::tibble(age = 1:200, aboveground_c_mg_ha = seq(1, 180, length.out = 200))

  pts <- plot_growth_calibration("Pl", curve, reference, mature_window = c(20, 150))
  den <- plot_growth_calibration("Pl", curve, reference, mature_window = c(20, 150), density = TRUE)

  stats_of <- function(p) vapply(p$layers, \(l) class(l$stat)[[1L]], character(1))
  expect_false(any(stats_of(pts) == "StatBinhex"))
  expect_true(any(stats_of(den) == "StatBinhex"))
  ## The density panel keeps the well-matched plots as points and drops the
  ## per-plot colour and shape legends with them.
  expect_null(den$labels$colour)
  expect_null(den$labels$shape)
  expect_equal(den$labels$colour %||% NA_character_, NA_character_)
})

test_that("plot_growth_candidate density caps points by COUNT, not weight fraction", {
  set.seed(1)
  n <- 400L
  obs <- data.frame(
    source = "Ground plots",
    age = runif(n, 20, 150),
    aboveground_c_mg_ha = runif(n, 10, 200),
    leading_raw = "AA",
    ## Skewed so a fraction-of-maximum rule would keep nearly everything.
    plot_weight = c(rep(1, 350L), runif(50L, 0, 0.2))
  )
  cur <- data.frame(age = 1:150, aboveground_c_mg_ha = seq(0, 100, length.out = 150))

  p <- plot_growth_candidate(
    species = "Sp1",
    current_curve = cur,
    candidate_curve = NULL,
    reference = obs,
    density = TRUE,
    density_points_max = 25L
  )
  ## the point layer over the density is the one with 25 rows
  n_drawn <- vapply(p$layers, \(l) nrow(l$data %||% data.frame()), integer(1))
  expect_true(25L %in% n_drawn)
  expect_false(350L %in% n_drawn)
})

test_that("the review bundle README names Biomass Succession, not ForCS", {
  sp <- "Sp1"
  obs <- data.frame(
    source = "Ground plots",
    age = seq(20, 140, 10),
    aboveground_c_mg_ha = seq(10, 130, 10),
    leading_raw = "AA"
  )
  cur <- data.frame(species = sp, age = 1:150, aboveground_c_mg_ha = seq(0, 100, length.out = 150))
  dir <- withr::local_tempdir()
  write_growth_review_bundle(
    dir = dir,
    species = sp,
    curves = cur,
    references = stats::setNames(list(obs), sp),
    best = data.frame(
      species = sp,
      level_source_used = "plots",
      level_source_requested = "plots",
      n_plots = 13L,
      n_bins = 3L,
      plots_sparse = FALSE
    ),
    windows = data.frame(species = sp, mature_from = 20, mature_to = 140)
  )
  readme <- readLines(file.path(dir, "README.txt"), warn = FALSE)
  expect_match(readme[[1L]], "Biomass Succession")
  expect_false(any(grepl("ForCS", readme, fixed = TRUE)))
})

test_that("plot_growth_candidate BUILDS in both cloud modes with a binned series present", {
  ## Regression for 0.0.128/0.0.129: the unit tests constructed the plot object but never built it,
  ## so a scale collision that only surfaces at render time shipped twice. stat_binhex() maps
  ## `fill = after_stat(count)` through its default aes, which a fixed `fill` argument does not
  ## unset, and the binned series maps `fill` to a discrete label; both present, ggplot2 applies the
  ## discrete scale to continuous counts. Building is the assertion that matters here.
  set.seed(1)
  n <- 700L
  obs <- data.frame(
    source = "Ground plots",
    age = runif(n, 20, 150),
    aboveground_c_mg_ha = runif(n, 10, 200),
    leading_raw = "AA",
    plot_weight = runif(n, 0, 1)
  )
  binned <- data.frame(
    age = seq(25, 145, 20),
    value = seq(20, 120, length.out = 7),
    n = c(10, 40, 80, 60, 30, 10, 5)
  )
  cur <- data.frame(age = 1:150, aboveground_c_mg_ha = seq(0, 100, length.out = 150))

  for (dens in c(TRUE, FALSE)) {
    p <- plot_growth_candidate(
      species = "Sp1",
      current_curve = cur,
      candidate_curve = NULL,
      reference = obs,
      binned = binned,
      density = dens
    )
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("the bundle README describes what was actually drawn", {
  obs <- data.frame(
    source = "Ground plots",
    age = seq(20, 140, 2),
    aboveground_c_mg_ha = seq(10, 130, 2),
    leading_raw = "AA"
  )
  cur <- data.frame(
    species = "Sp1",
    age = 1:150,
    aboveground_c_mg_ha = seq(0, 100, length.out = 150)
  )
  best <- data.frame(
    species = "Sp1",
    level_source_used = "plots",
    level_source_requested = "plots",
    n_plots = nrow(obs),
    n_bins = 7L,
    plots_sparse = FALSE,
    window_source = "derived"
  )
  win <- data.frame(species = "Sp1", mature_from = 20, mature_to = 140)
  rc <- stats::setNames(
    list(list(binned = data.frame(age = seq(25, 125, 20), value = seq(20, 120, length.out = 6)))),
    "Sp1"
  )

  readme <- function(reference_curves) {
    d <- withr::local_tempdir()
    write_growth_review_bundle(
      dir = d,
      species = "Sp1",
      curves = cur,
      references = stats::setNames(list(obs), "Sp1"),
      reference_curves = reference_curves,
      best = best,
      windows = win
    )
    readLines(file.path(d, "README.txt"), warn = FALSE)
  }

  with_binned <- readme(rc)
  without <- readme(NULL)
  expect_true(any(grepl("BLUE DIAMONDS", with_binned, fixed = TRUE)))
  expect_false(any(grepl("BLUE DIAMONDS", without, fixed = TRUE)))
  expect_true(any(grepl("binned series is NOT drawn", without, fixed = TRUE)))
  ## The window cap stopped being 0.45 x longevity in 0.0.124; the text must not still say so.
  expect_false(any(grepl("0.45", with_binned, fixed = TRUE)))
})

test_that("only series that are drawn get a legend key", {
  set.seed(1)
  n <- 700L
  obs <- data.frame(
    source = "Ground plots",
    age = runif(n, 20, 150),
    aboveground_c_mg_ha = runif(n, 10, 200),
    leading_raw = "AA",
    plot_weight = runif(n, 0, 1)
  )
  binned <- data.frame(
    age = seq(25, 145, 20),
    value = seq(20, 120, length.out = 7),
    n = c(10, 40, 80, 60, 30, 10, 5)
  )
  cur <- data.frame(age = 1:150, aboveground_c_mg_ha = seq(0, 100, length.out = 150))
  keys <- function(bn, dens) {
    p <- plot_growth_candidate(
      species = "Sp1",
      current_curve = cur,
      candidate_curve = NULL,
      reference = obs,
      binned = bn,
      density = dens
    )
    p$scales$get_scales("colour")$limits
  }

  ## The colour scale carries breaks/limits so the fill and colour legends merge, so a label left in
  ## the palette with no layer behind it renders as a key with no glyph.
  expect_false("ground plots" %in% keys(binned, TRUE))
  expect_true("ground plots" %in% keys(binned, FALSE))
  expect_false("ground plots, age-binned (scored)" %in% keys(NULL, TRUE))
  expect_true("ground plots, age-binned (scored)" %in% keys(binned, FALSE))
})
