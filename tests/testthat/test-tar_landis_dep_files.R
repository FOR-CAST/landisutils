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
