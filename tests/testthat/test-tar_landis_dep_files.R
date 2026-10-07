make_batch <- function(root, name, n) {
  d <- fs::dir_create(fs::path(root, name))
  for (f in c("initial-communities.tif", "ecoregions.tif")) {
    writeLines(paste(name, f, n), fs::path(d, f))
  }
  as.character(fs::dir_ls(d))
}

test_that("landis_dep_files() prefers files under this scenario dir", {
  root <- withr::local_tempdir()
  b1 <- make_batch(root, "batch01", 12000)
  b2 <- make_batch(root, "batch02", 782)

  got <- landis_dep_files(list(c(b1, b2)), fs::path(root, "batch02"))

  expect_length(got, 2L)
  expect_true(all(grepl("batch02", got)))
  expect_setequal(basename(got), c("initial-communities.tif", "ecoregions.tif"))
})

test_that("landis_dep_files() resolves symlinks on BOTH sides", {
  real <- withr::local_tempdir()
  b1 <- make_batch(real, "batch01", 12000)
  b2 <- make_batch(real, "batch02", 782)

  ## Reach the same tree through a symlink, as a project whose LANDIS-II is one.
  link_root <- withr::local_tempdir()
  link <- fs::path(link_root, "LANDIS-II")
  fs::link_create(real, link)

  ## deps spelled through the LINK, scenario_dir also through the link.
  linked <- c(
    as.character(fs::path(link, "batch01", basename(b1))),
    as.character(fs::path(link, "batch02", basename(b2)))
  )
  got <- landis_dep_files(list(linked), fs::path(link, "batch02"))

  ## Without resolving both sides the prefix test never matches and batch01
  ## wins the basename dedup, staging the wrong landscape.
  expect_length(got, 2L)
  expect_true(all(grepl("batch02", got)))
})

test_that("landis_dep_files() drops missing files and tolerates empty deps", {
  root <- withr::local_tempdir()
  b1 <- make_batch(root, "batch01", 1)

  got <- landis_dep_files(
    list(c(b1, fs::path(root, "batch01", "absent.tif"))),
    fs::path(root, "batch01")
  )
  expect_setequal(basename(got), c("initial-communities.tif", "ecoregions.tif"))

  expect_length(landis_dep_files(list(), fs::path(root, "batch01")), 0L)
  expect_length(landis_dep_files(list(character(0)), fs::path(root, "batch01")), 0L)
})

test_that("landis_dep_files() does not match a sibling sharing the prefix", {
  root <- withr::local_tempdir()
  make_batch(root, "phase_2_ICH", 1)
  b <- make_batch(root, "phase_2_ICH_fire", 2)

  got <- landis_dep_files(
    list(c(as.character(fs::dir_ls(fs::path(root, "phase_2_ICH"))), b)),
    fs::path(root, "phase_2_ICH_fire")
  )
  expect_true(all(grepl("phase_2_ICH_fire", got)))
})

## Scenario directories side by side. A pattern that maps over the scenario directory but not over
## the dependency targets hands every branch every scenario's files.
make_scenario <- function(root, name, files = c("scenario.txt", "species.txt")) {
  d <- fs::dir_create(fs::path(root, name))
  for (f in files) {
    writeLines(paste(name, f), fs::path(d, f))
  }
  as.character(fs::path(d, files))
}

test_that("landis_dep_files() stages none of another scenario's files", {
  root <- withr::local_tempdir()
  only <- make_scenario(root, "ForCS_only")
  wind <- make_scenario(root, "ForCS_wind", c("scenario.txt", "species.txt", "original-wind.txt"))
  sd <- fs::path(root, "ForCS_only")

  got <- landis_dep_files(list(c(wind, only)), sd)

  ## landisutils 0.0.168 also staged ForCS_wind/original-wind.txt, which ForCS_only lacks
  expect_identical(as.character(got), as.character(fs::path_real(only)))
  rep_dir <- landis_replicate(sd, rep_index = 1L, files = got)
  expect_setequal(basename(fs::dir_ls(rep_dir)), c("scenario.txt", "species.txt"))
  ## kept aside, in deps order, for landis_rep_is_current()
  expect_identical(attr(got, "foreign"), as.character(fs::path_real(wind)))
})

test_that("landis_dep_files() still stages shared files that sit in no scenario directory", {
  root <- withr::local_tempdir()
  landis <- fs::dir_create(fs::path(root, "LANDIS-II"))
  only <- make_scenario(landis, "ForCS_only")
  wind <- make_scenario(landis, "ForCS_wind", c("scenario.txt", "original-wind.txt"))
  ## directly in the scenarios' parent, and outside it; species.txt also exists under ForCS_only
  beside <- make_scenario(root, "LANDIS-II", c("climate.txt", "species.txt"))
  outside <- make_scenario(root, "data", "weather.csv")

  got <- landis_dep_files(list(c(wind, only), beside, outside), fs::path(landis, "ForCS_only"))

  expect_identical(as.character(got), as.character(fs::path_real(c(only, beside[[1L]], outside))))
  expect_identical(attr(got, "foreign"), as.character(fs::path_real(wind)))
  expect_null(attr(
    landis_dep_files(list(only, outside), fs::path(landis, "ForCS_only")),
    "foreign"
  ))
})

test_that("adding a scenario leaves another scenario's staged files and hash unchanged", {
  root <- withr::local_tempdir()
  only <- make_scenario(root, "ForCS_only")
  wind <- make_scenario(root, "ForCS_wind", c("scenario.txt", "species.txt", "original-wind.txt"))
  fire <- make_scenario(root, "ForCS_fire", c("scenario.txt", "species.txt", "dynamic-fire.txt"))
  sd <- fs::path(root, "ForCS_wind")

  before <- landis_dep_files(list(c(only, wind)), sd)
  after <- landis_dep_files(list(c(only, fire, wind)), sd)

  ## landisutils 0.0.168 staged ForCS_fire/dynamic-fire.txt into ForCS_wind once ForCS_fire joined
  expect_identical(as.character(after), as.character(before))
  expect_identical(
    landis_input_hash(after, 12345L, 1L, "scenario.txt"),
    landis_input_hash(before, 12345L, 1L, "scenario.txt")
  )
})
