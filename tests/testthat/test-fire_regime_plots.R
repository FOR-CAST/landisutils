## Three periods for two series and three quantities; only the count carries an interval.
.regime_factors <- function() {
  f <- expand.grid(
    start = c(2026, 2041, 2071),
    series = c("gcm", "hadgem"),
    quantity = c("count", "size", "area"),
    stringsAsFactors = FALSE
  )
  f$end <- c(2041, 2071, 2101)[match(f$start, c(2026, 2041, 2071))]
  f$estimate <- 1 +
    0.25 * (match(f$start, c(2026, 2041, 2071)) - 1) * match(f$series, c("gcm", "hadgem"))
  f$lower <- ifelse(f$quantity == "count" & f$start > 2026, f$estimate * 0.8, NA)
  f$upper <- ifelse(f$quantity == "count" & f$start > 2026, f$estimate * 1.2, NA)
  f
}

test_that("periods with ends draw steps, with intervals shaded", {
  skip_if_not_installed("ggplot2")
  f <- .regime_factors()
  built <- ggplot2::ggplot_build(plot_fire_regime_multipliers(
    f,
    x = "start",
    series = "series",
    x_end = "end"
  ))
  expect_identical(built$plot$facet$params$nrow, 1L)
  expect_identical(levels(built$layout$layout$quantity), c("count", "size", "area"))
  rects <- built$data[[2]]
  expect_equal(nrow(rects), 4L)
  path <- built$data[[3]]
  ## Each value is held from its start to its end: two points per value.
  expect_equal(nrow(path), 2L * nrow(f))
  labs <- built$data[[4]]
  expect_true(all(grepl("x$", labs$label)))
  ## The y axis is log10 of the multipliers.
  expect_equal(range(10^path$y), range(f$estimate))
})

test_that("categories draw points with bars, and a panel adds a facet row", {
  skip_if_not_installed("ggplot2")
  f <- .regime_factors()
  f$envelope <- c("normal", "ssp245", "ssp585")[match(f$start, c(2026, 2041, 2071))]
  f$zone <- rep(c("z1", "z2"), length.out = nrow(f))
  gg <- plot_fire_regime_multipliers(f, x = "envelope", series = "series", panel = "zone")
  built <- ggplot2::ggplot_build(gg)
  expect_s3_class(built$plot$facet, "FacetGrid")
  expect_equal(nrow(built$data[[3]]), nrow(f))
  expect_equal(nrow(built$data[[2]]), sum(!is.na(f$lower)))
})

test_that("multiplier inputs are checked", {
  skip_if_not_installed("ggplot2")
  f <- .regime_factors()
  expect_error(plot_fire_regime_multipliers(f, x = "start", series = "nope"), "columns")
  g <- f
  g$estimate[1] <- 0
  expect_error(plot_fire_regime_multipliers(g, x = "start", series = "series"), "positive")
  f$series <- rep(letters[1:4], length.out = nrow(f))
  expect_error(plot_fire_regime_multipliers(f, x = "start", series = "series"), "need `colours`")
  expect_error(
    plot_fire_regime_multipliers(f, x = "start", series = "series", colours = c(a = "red")),
    "names no colour for: b, c, d"
  )
})
