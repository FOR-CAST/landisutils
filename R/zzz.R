.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  opts.landisutils <- list(
    ## none yet
  )
  toset <- !(names(opts.landisutils) %in% names(opts))
  if (any(toset)) options(opts.landisutils[toset])

  invisible()
}
