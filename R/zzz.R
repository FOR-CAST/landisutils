.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  ## Derive everything that embeds the core generation from `.landis_version_default`
  ## (R/landis_version.R) rather than repeating the number. Flipping that one constant
  ## when v9 supersedes v8 then moves the runtime guard, the default image tag and the
  ## default console path together, instead of leaving a stale "v8" in whichever of
  ## them got missed.
  .v <- .landis_version_default
  opts.landisutils <- list(
    landisutils.cache.path = .climateCachePath(),
    landisutils.landis.version = .v,
    landisutils.docker.console = sprintf(
      "/opt/landis-ii/Core-Model-v%d-LINUX/build/Release/Landis.Console.dll",
      .v
    ),
    landisutils.docker.image = sprintf(
      "ghcr.io/landis-ii-foundation/landis-ii-v%d-release:main",
      .v
    ),
    landisutils.run.method = if (.Platform$OS.type == "windows") "local" else "docker"
  )
  toset <- !(names(opts.landisutils) %in% names(opts))
  if (any(toset)) {
    options(opts.landisutils[toset])
  }

  invisible()
}
