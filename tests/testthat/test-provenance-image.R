test_that(".image_digest_line() hands the console the image and the --format template whole", {
  ## system2() runs through a shell, and the template has spaces: unquoted, it reached docker as
  ## several words, so every digest capture failed.
  skip_on_os("windows")
  console <- withr::local_tempfile(fileext = ".sh")
  writeLines(c("#!/bin/sh", 'printf "%s|%s\\n" "$3" "$5"'), console)
  Sys.chmod(console, "0755")
  expect_identical(
    .image_digest_line("ghcr.io/org/image:tag", console = console),
    "ghcr.io/org/image:tag|{{if .RepoDigests}}{{index .RepoDigests 0}}{{else}}{{.Id}}{{end}}"
  )
})

test_that(".image_digest_line() returns NA when the console fails", {
  skip_on_os("windows")
  console <- withr::local_tempfile(fileext = ".sh")
  writeLines(c("#!/bin/sh", "exit 1"), console)
  Sys.chmod(console, "0755")
  expect_identical(.image_digest_line("img:tag", console = console), NA_character_)
})

test_that("landis_image_info() does not report a sidecar without a digest as captured", {
  run_dir <- withr::local_tempdir()
  log_dir <- fs::dir_create(fs::path(run_dir, "rep01", "log"))
  writeLines(
    "# ghcr.io/org/image:tag (digest unavailable: not present in local image cache?)",
    fs::path(log_dir, "docker_image.log")
  )
  expect_identical(
    landis_image_info(run_dir),
    list(image_name = "ghcr.io/org/image:tag", image_digest = NA_character_, source = "recorded")
  )
  expect_match(prov_landis_container(run_dir)$Value[[1L]], "digest not captured", fixed = TRUE)

  writeLines("ghcr.io/org/image@sha256:abc", fs::path(log_dir, "docker_image.log"))
  expect_identical(
    landis_image_info(run_dir),
    list(image_name = "ghcr.io/org/image", image_digest = "sha256:abc", source = "captured")
  )
})
