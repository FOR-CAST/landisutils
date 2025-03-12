#' Specify Extension `Timestep`
#'
#' @template return_insert
#'
#' @export
#'
insertTimestep <- function(t = NULL) {
  t %||% 1L

  glue::glue("TimeStep    {t}")
}
