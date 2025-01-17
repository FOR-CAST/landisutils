#' Specify Extension `Timestep`
#'
#' @template return_insert
#'
#' @export
#'
insertTimestep <- function(t = 1) {
  glue::glue("TimeStep    {t}")
}
