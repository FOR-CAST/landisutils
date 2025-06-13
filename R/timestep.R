#' Specify Extension `Timestep`
#'
#' @param t integer, corresponding to `Timestep` (years).
#'
#' @template return_insert
#'
#' @export
#'
insertTimestep <- function(t = NULL) {
  t %||% 1L

  c(
    glue::glue("Timestep    {t}"),
    glue::glue("") ## add blank line after each item group)
  )
}
