# LANDIS-II version ---------------------------------------------------------------------------

#' @keywords internal
.landisVersions <- c(7L, 8L)

#' @keywords internal
.landisVersionsText <- function() {
   paste(.landisVersions, collapse = " or ")
 }

#' Get, set or test the target LANDIS-II version
#'
#' @return [checkVersion] return `TRUE` if the `version` is valid.
#' Will signal an error if an invalid `version` value is supplied.
#'
#' @export
#' @rdname landisVersion
checkVersion <- function(version) {
  if (!is.null(version) && version %in% .landisVersions) {
    return(TRUE)
  } else {
    stop(glue::glue("invalid version. must be one of {.landisVersionsText()}"))
  }
}

#' @param version numeric or `NULL`.
#' If `version` is `NULL`, [landisVersion] will get the currently set LANDIS-II version.
#' If `version` is a valid numeric (i.e., one of `r .landisVersionsText()`),
#' then [landisVersion] sets this LANDIS-II version.
#'
#' @template LANDIS_version
#'
#' @returns #' [landisVersion] checks the version and returns an integer
#' (`r .landisVersionsText()`) corresponding to the LANDIS-II version currently set.
#'
#' @export
#' @rdname landisVersion
landisVersion <- function(version = NULL) {
  if (is.null(version)) {
    version <- getOption("landisutils.version", 7L) ## TODO: set 8 as default once working
  } else {
    version <- as.integer(version)
    checkVersion(version)
    options(landisutils.version = version)
  }

  return(version)
}

# extensions ----------------------------------------------------------------------------------

#' @keywords internal
.extTypes <- c("succession", "disturbance", "other") ## 'other' includes output extensions
