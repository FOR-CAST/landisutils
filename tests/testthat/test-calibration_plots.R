## A minimal run_calibration_validation()-shaped summary. Severity rises with
## fire size, and one rep holds a single very large fire, so the event- and
## area-weighted severity summaries differ -- which is what the weighting tests
## turn on.
make_calibration_stats <- function(n_reps = 2L) {
  reps <- lapply(seq_len(n_reps), function(i) {
    sites <- c(2L, 5L, 20L, 100L, 5000L)
    list(
      n_fires_by_year = data.frame(year = 1:5, n_fires = c(3L, 5L, 4L, 6L, 2L) + i),
      fire_sizes_ha = sites * 1.44,
      events = data.frame(
        year = 1:5,
        eco = "FRT1",
        init_fuel = c(1L, 1L, 3L, 3L, 1L),
        sites = sites,
        mean_severity = c(0.8, 1.4, 2.6, 3.6, 4.7)
      ),
      total_sites_burned = sum(sites),
      n_events = 5L,
      area_by_fuel_ha = data.frame(
        fuel_code = c(2L, 9L), ## FuelType map values: fuel-type index + 1
        fuel_index = c(1L, 8L),
        base = c("Conifer", "Deciduous"),
        cells = c(5102L, 120L),
        area_ha = c(7346.88, 172.8)
      )
    )
  })
  list(
    reps = reps,
    observed = list(
      primary = list(
        n_fires_by_year = data.frame(year = 1:5, n = c(2L, 9L, 1L, 12L, 4L)),
        lambda_obs = 5.6,
        fire_sizes_ha = c(1, 3, 9, 40, 800, 20000),
        area_by_fuel_ha = data.frame(base = c("Conifer", "Deciduous"), area_ha = c(9000, 500)),
        severity_dist = c("1" = 0.18, "2" = 0.18, "3" = 0.53, "4" = 0.055, "5" = 0.055)
      ),
      fuel_code_to_base = c("1" = "Conifer", "3" = "Deciduous"),
      min_size_ha = 1
    ),
    pixel_area_ha = 1.44,
    sim_years = 5L,
    n_reps = n_reps,
    loss = list(
      total = 1.05,
      components = list(count = 0.13, size = 0.30, severity = 0.44),
      weights = list(count = 2, size = 1, severity = 1)
    )
  )
}

test_that("calibration_plot_palette() returns roles and takes overrides", {
  pal <- calibration_plot_palette()
  expect_named(pal, c("observed", "simulated", "alternate", "ink", "muted", "grid"))
  expect_equal(unname(calibration_plot_palette(simulated = "red")[["simulated"]]), "red")
})

test_that("calibration_plot_palette() rejects unknown and unnamed overrides", {
  expect_snapshot(error = TRUE, calibration_plot_palette(nope = "red"))
  expect_snapshot(error = TRUE, calibration_plot_palette("red"))
})

test_that("calibration_events() pools replicates and adds area", {
  stats <- make_calibration_stats(n_reps = 3L)
  ev <- calibration_events(stats)
  expect_equal(nrow(ev), 15L)
  expect_equal(ev$area_ha, ev$sites * 1.44)
})

test_that("the plots build, not merely construct", {
  skip_if_not_installed("ggplot2")
  stats <- make_calibration_stats()
  trace <- data.frame(generation = 1:6, best_loss = c(2, 1.8, 1.8, 1.4, 1.2, 1.2))
  plots <- list(
    convergence = plot_calibration_convergence(trace, stats),
    loss = plot_calibration_loss(stats),
    sizes = plot_calibration_fire_sizes(stats),
    counts = plot_calibration_fire_counts(stats),
    fuel = plot_calibration_area_by_fuel(stats),
    severity = plot_calibration_severity(stats),
    by_size = plot_calibration_severity_by_size(stats)
  )
  for (nm in names(plots)) {
    expect_s3_class(plots[[nm]], "ggplot")
    expect_no_error(ggplot2::ggplot_build(plots[[nm]]))
  }
})

test_that("plot_calibration_area_by_fuel() takes simulated fuel types from the replicates", {
  skip_if_not_installed("ggplot2")
  stats <- make_calibration_stats()
  ## A table for the OBSERVED raster's codes that, applied to the simulated map values, would name
  ## other fuel types -- as the BC code table named C-5 ConiferPlantation.
  stats$observed$fuel_code_to_base <- c("2" = "Open", "9" = "Slash")
  p <- plot_calibration_area_by_fuel(stats)
  sim <- p$data[p$data$source == "simulated", ]
  expect_equal(sim$base, c("Conifer", "Deciduous"))
  expect_equal(sim$area, c(5102, 120) * 1.44 * 2, tolerance = 1e-6)
})

test_that("plot_calibration_convergence() reads a trace from file", {
  skip_if_not_installed("ggplot2")
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(iter = 1:4, best_value = c(3, 2, 2, 1.5)), path, row.names = FALSE)
  p <- plot_calibration_convergence(path)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_equal(nrow(p$data), 4L)
})

test_that("plot_calibration_convergence() errors on a missing file", {
  expect_snapshot(error = TRUE, plot_calibration_convergence("no-such-trace.csv"))
})

test_that("severity weighting changes the simulated distribution", {
  skip_if_not_installed("ggplot2")
  stats <- make_calibration_stats()
  by_event <- plot_calibration_severity(stats, weight_by = "events")
  by_area <- plot_calibration_severity(stats, weight_by = "area")
  sim_event <- by_event$data$prop[by_event$data$source == "simulated"]
  sim_area <- by_area$data$prop[by_area$data$source == "simulated"]
  expect_equal(sum(sim_event), 1)
  expect_equal(sum(sim_area), 1)
  ## the single 5000-cell fire is the most severe, so area weighting moves mass
  ## from Low into High
  expect_gt(sim_area[3], sim_event[3])
  expect_lt(sim_area[1], sim_event[1])
})

test_that("plot_calibration_severity() honours the class count", {
  skip_if_not_installed("ggplot2")
  stats <- make_calibration_stats()
  expect_equal(
    levels(plot_calibration_severity(stats, classes = 3)$data$class),
    c("Low", "Medium", "High")
  )
  expect_equal(levels(plot_calibration_severity(stats, classes = 5)$data$class), as.character(1:5))
  expect_snapshot(error = TRUE, plot_calibration_severity(stats, classes = 4))
})

test_that("plot_calibration_severity_by_size() carries a size summary", {
  skip_if_not_installed("ggplot2")
  stats <- make_calibration_stats()
  p <- plot_calibration_severity_by_size(stats)
  summ <- attr(p, "size_summary")
  expect_equal(sum(summ$share_n), 1)
  expect_equal(sum(summ$share_area), 1)
  ## the class holding most of the area holds few of the fires -- the property
  ## that makes weight_by matter
  big <- which.max(summ$share_area)
  expect_gt(summ$share_area[big], 0.9)
  expect_lt(summ$share_n[big], 0.3)
})

test_that("plot_calibration_loss() renames components when asked", {
  skip_if_not_installed("ggplot2")
  stats <- make_calibration_stats()
  p <- plot_calibration_loss(stats, labels = c(count = "Fires per year"))
  expect_true("Fires per year" %in% levels(p$data$label))
  ## weighted, not raw: count carries weight 2
  expect_equal(p$data$weighted[p$data$component == "count"], 0.26)
})

test_that("malformed inputs are rejected with a useful message", {
  expect_snapshot(error = TRUE, plot_calibration_loss(list()))
  expect_snapshot(error = TRUE, plot_calibration_fire_sizes(list(reps = list())))
  stats <- make_calibration_stats()
  stats$observed$primary$severity_dist <- NULL
  expect_snapshot(error = TRUE, plot_calibration_severity(stats))
})

test_that("the summary log's Time 0 row never reaches the count rate or the plot", {
  skip_if_not_installed("ggplot2")
  stats <- make_calibration_stats()
  ## simulate a pre-fix cached payload: prepend the initial-state row every rep used to carry
  with0 <- stats
  with0$reps <- lapply(with0$reps, function(r) {
    r$n_fires_by_year <- rbind(data.frame(year = 0L, n_fires = 0L), r$n_fires_by_year)
    r
  })

  clean <- unlist(lapply(stats$reps, function(r) r$n_fires_by_year$n_fires))
  p0 <- plot_calibration_fire_counts(with0)
  p1 <- plot_calibration_fire_counts(stats)
  expect_equal(sort(p0$data$n[p0$data$source == "simulated"]), sort(clean))
  expect_equal(p0$data, p1$data)
  ## absolute, not just path-vs-path: the failure mode is a wrong NUMBER, so pin the mean to the
  ## known mean over years 1..N. Comparing the two paths alone would pass if both were broken.
  expect_equal(mean(p0$data$n[p0$data$source == "simulated"]), mean(clean))
  expect_false(any(p0$data$n[p0$data$source == "simulated"] == 0))

  ## and the loss must score the same rate either way -- the row is not a year
  w <- c(count = 1, size = 0, size_tail = 0, area_fuel = 0, severity = 0)
  l0 <- loss_from_stats(with0$reps, with0$observed, weights = w)
  l1 <- loss_from_stats(stats$reps, stats$observed, weights = w)
  expect_equal(l0$components[["count"]], l1$components[["count"]])
  ## and pin it to the rate computed by hand over years 1..N only
  rate <- mean(vapply(stats$reps, function(r) mean(r$n_fires_by_year$n_fires), numeric(1)))
  expected <- abs(rate - stats$observed$primary$lambda_obs) /
    stats::sd(stats$observed$primary$n_fires_by_year$n)
  expect_equal(l0$components[["count"]], expected)
})

test_that(".drop_initial_timestep() is idempotent and safe on odd input", {
  d <- data.frame(year = 0:3, n_fires = c(0L, 5L, 6L, 7L))
  once <- landisutils:::.drop_initial_timestep(d)
  expect_equal(once$year, 1:3)
  expect_equal(landisutils:::.drop_initial_timestep(once), once)
  expect_null(landisutils:::.drop_initial_timestep(NULL))
  empty <- d[0, ]
  expect_equal(landisutils:::.drop_initial_timestep(empty), empty)
  no_year <- data.frame(n_fires = 1:3)
  expect_equal(landisutils:::.drop_initial_timestep(no_year), no_year)
})

test_that("loss_from_stats() refuses a missing lambda_obs instead of misaligning components", {
  stats <- make_calibration_stats()
  stats$observed$primary$lambda_obs <- NULL
  expect_snapshot(error = TRUE, loss_from_stats(stats$reps, stats$observed))
})

test_that("loss_from_stats() refuses weights that would silently misalign", {
  stats <- make_calibration_stats()
  ## NULL leaves `w` at its zero initialisation, so the total collapses to 0 while every component
  ## is computed correctly -- the failure this guard exists for.
  expect_snapshot(error = TRUE, loss_from_stats(stats$reps, stats$observed, weights = NULL))
  expect_snapshot(error = TRUE, loss_from_stats(stats$reps, stats$observed, weights = c(1, 1)))
  ## an unknown name GROWS `w` past `components`, and the multiply then recycles
  expect_snapshot(
    error = TRUE,
    loss_from_stats(stats$reps, stats$observed, weights = c(cnt = 2, size = 1))
  )
  expect_snapshot(
    error = TRUE,
    loss_from_stats(stats$reps, stats$observed, weights = c(count = NA_real_))
  )
})

test_that("loss_from_stats() weights a subset without touching the others", {
  stats <- make_calibration_stats()
  full <- loss_from_stats(
    stats$reps,
    stats$observed,
    weights = c(count = 1, size = 1, size_tail = 1, area_fuel = 1, severity = 1)
  )
  just_count <- loss_from_stats(stats$reps, stats$observed, weights = c(count = 1))
  ## components never depend on the weights; only the total does
  expect_equal(just_count$components, full$components)
  expect_equal(just_count$total, unname(full$components[["count"]]))
  expect_named(just_count$weights, .LOSS_COMPONENTS)
})

test_that("calibration_box_stats() puts the whiskers at the requested quantiles", {
  b <- calibration_box_stats(c(NA, Inf, 1:20), whiskers = c(0.1, 0.9))
  expect_equal(
    unlist(b$stats, use.names = FALSE),
    stats::quantile(1:20, c(0.1, 0.25, 0.5, 0.75, 0.9), names = FALSE)
  )
  expect_named(b$stats, c("ymin", "lower", "middle", "upper", "ymax"))
  expect_equal(b$out, c(1, 2, 19, 20))
  ## Interpolated, not order statistics: the 5th percentile of 1:40 lies between 2 and 3.
  expect_equal(calibration_box_stats(1:40)$stats$ymin, 2.95)
  expect_length(calibration_box_stats(rep(0.4, 5))$out, 0L)
  expect_error(calibration_box_stats(1:10, c(0.3, 0.9)), "at most 0.25")
  expect_error(calibration_box_stats(c(NA, NaN)), "no finite value")
})

test_that("replicate_percentile() counts ties as half", {
  expect_equal(replicate_percentile(1:10, c(0.5, 5, 5.5, 11)), c(0, 45, 50, 100))
  expect_equal(replicate_percentile(c(1, 2, 2, 3), 2), 50)
  expect_identical(replicate_percentile(c(NA, NaN), 1), NA_real_)
  expect_identical(replicate_percentile(1:3, NA), NA_real_)
})

test_that("plot_calibration_replicates() sets each reference beside the box", {
  skip_if_not_installed("ggplot2")
  v <- c(3, 5, 6, 8, 9, 10, 10.5, 11, 12, 14, 16, 20, 26, 39)
  refs <- data.frame(name = c("Record", "Target"), value = c(15.3, 13.5))
  gg <- plot_calibration_replicates(v, refs, label = function(x) sprintf("%.1f ha", x))
  layers <- ggplot2::ggplot_build(gg)$data
  q <- stats::quantile(v, c(0.05, 0.25, 0.5, 0.75, 0.95), names = FALSE)
  expect_equal(layers[[1]]$yintercept, q[c(1, 5)])
  box <- layers[[3]]
  expect_equal(unlist(box[1, c("ymin", "lower", "middle", "upper", "ymax")], use.names = FALSE), q)
  expect_equal(sort(layers[[4]]$y), v[v < q[[1]] | v > q[[5]]])
  expect_equal(layers[[5]]$x, c(2, 3))
  expect_identical(
    layers[[6]]$label,
    c("15.3 ha\n71st percentile\nof the replicates", "13.5 ha\n64th percentile\nof the replicates")
  )
  ## Without references only the box and its whisker lines are drawn.
  expect_length(ggplot2::ggplot_build(plot_calibration_replicates(v))$data, 4L)
  expect_error(plot_calibration_replicates(v, data.frame(value = 1)), "`name` and `value`")
})

## Two groups: A with draws on both sides, B with a single recorded value and an interval.
.group_spread <- function() {
  rbind(
    data.frame(group = "A", source = "observed", value = seq(0.2, 0.5, length.out = 200)),
    data.frame(
      group = "A",
      source = "simulated",
      value = c(seq(0.25, 0.45, length.out = 38), 0.05, 0.9)
    ),
    data.frame(group = "B", source = "simulated", value = seq(0.1, 0.3, length.out = 40))
  )
}

test_that("plot_calibration_groups() draws boxes where there are values and points where not", {
  skip_if_not_installed("ggplot2")
  ref <- data.frame(
    group = c("A", "B"),
    observed = c(0.35, 0.17),
    simulated = c(0.36, 0.2),
    lower = c(NA, 0.1),
    upper = c(NA, 0.3),
    note = c("inside", "not gated")
  )
  built <- ggplot2::ggplot_build(plot_calibration_groups(.group_spread(), ref))
  boxes <- built$data[[1]]
  ## A: observed left of simulated; B: simulated only.
  expect_equal(boxes$x, c(0.8, 1.2, 2.2))
  ## B's observed side: a whisker-only bar and a point from `reference`.
  expect_equal(c(built$data[[2]]$ymin, built$data[[2]]$ymax), c(0.1, 0.3))
  expect_equal(built$data[[3]]$y, 0.17)
  ## Only simulated replicates beyond the whiskers are dotted: two each side of 40 (type 7).
  s <- .group_spread()
  beyond <- function(v) {
    q <- stats::quantile(v, c(0.05, 0.95), names = FALSE)
    sort(v[v < q[[1]] | v > q[[2]]])
  }
  dots <- built$data[[4]]
  expect_setequal(dots$x, c(1.2, 2.2))
  expect_equal(
    sort(dots$y[dots$x == 1.2]),
    beyond(s$value[s$group == "A" & s$source == "simulated"])
  )
  expect_equal(
    sort(dots$y[dots$x == 2.2]),
    beyond(s$value[s$group == "B" & s$source == "simulated"])
  )
  ## The diamonds: each group's compared values, on its boxes only (B's observed side is a point).
  expect_equal(built$data[[5]]$y, c(0.35, 0.36, 0.2))
  expect_identical(built$data[[6]]$label, c("inside", "not gated"))
  expect_identical(built$layout$panel_params[[1]]$x$get_labels(), c("A", "B"))
})

test_that("plot_calibration_groups() works without a reference and checks its inputs", {
  skip_if_not_installed("ggplot2")
  s <- .group_spread()
  s$group <- factor(s$group, levels = c("B", "A"))
  built <- ggplot2::ggplot_build(plot_calibration_groups(s))
  expect_identical(built$layout$panel_params[[1]]$x$get_labels(), c("B", "A"))
  expect_length(built$data, 2L)
  s$source[1] <- "record"
  expect_error(plot_calibration_groups(s), "\"observed\" or \"simulated\"")
  expect_error(plot_calibration_groups(data.frame(group = "A")), "`group`, `source` and `value`")
})
