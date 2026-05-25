## Set up a scoped climate cache for a single test_that() block. Everything
## ephemeral that the test or any subprocess it spawns writes ends up inside
## `tmp_pth`, so withr cleans it up when the test_that scope exits.
##
## - landisutils.cache.path: routes the package's own cache writes (Arrow
##   datasets, climr cache subdir, elevatr_tiles/ from get_elevation_rast).
## - TMPDIR: child R processes (future multisession workers) resolve their own
##   tempdir() under tmp_pth at startup.
## - _JAVA_OPTIONS=-Djava.io.tmpdir=tmp_pth: JVMs started by J4R/BioSIM put
##   J4RServer*.log and hsperfdata_<user>/ under tmp_pth instead of /tmp
##   (verified by inspecting J4R's jar — the log path is built from
##   System.getProperty("java.io.tmpdir")).
local_climate_test_cache <- function(prefix, .local_envir = parent.frame()) {
  tmp_pth <- withr::local_tempdir(prefix, .local_envir = .local_envir)

  withr::local_options(landisutils.cache.path = tmp_pth, .local_envir = .local_envir)

  withr::local_envvar(
    c(TMPDIR = tmp_pth, "_JAVA_OPTIONS" = sprintf("-Djava.io.tmpdir=%s", tmp_pth)),
    .local_envir = .local_envir
  )

  withr::defer(
    if (requireNamespace("future", quietly = TRUE)) {
      try(future::plan(future::sequential), silent = TRUE)
    },
    envir = .local_envir
  )

  invisible(tmp_pth)
}
