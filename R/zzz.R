.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  opts.landisutils <- list(
    landisutils.cache.path = .climateCachePath(),
    landisutils.docker.console = "/opt/landis-ii/Core-Model-v8-LINUX/build/Release/Landis.Console.dll",
    landisutils.docker.image = "ghcr.io/landis-ii-foundation/landis-ii-v8-release:main",
    landisutils.run.method = if (.Platform$OS.type == "windows") "local" else "docker"
  )
  toset <- !(names(opts.landisutils) %in% names(opts))
  if (any(toset)) {
    options(opts.landisutils[toset])
  }

  invisible()
}
