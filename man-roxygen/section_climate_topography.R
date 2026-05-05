#' @section Topographic considerations:
#'   The climate data sources used by this package (BioSIM, Daymet, TerraClim)
#'   are **not PRISM-derived**.
#'   PRISM (<https://prism.oregonstate.edu/>) applies topographically-aware
#'   interpolation (slope, aspect, elevation, coastal proximity, temperature
#'   inversions) when downscaling station observations to a grid
#'   (Daly et al. 2008); the sources wrapped here use simpler interpolation
#'   schemes that can produce **large discrepancies in areas of complex
#'   topography** (mountainous terrain, steep elevation gradients, rain
#'   shadows): inter-product differences of 5-60% in annual precipitation
#'   (Henn et al. 2018) and >6 \eqn{^\circ}C in temperature (Walton & Hall
#'   2018) have been documented in the western US.
#'   Carefully evaluate the suitability of the chosen source for your study
#'   area, especially in topographically heterogeneous landscapes.
#'
#' @references
#' Daly, C., Halbleib, M., Smith, J.I., Gibson, W.P., Doggett, M.K.,
#'   Taylor, G.H., Curtis, J., & Pasteris, P.P. (2008). Physiographically
#'   sensitive mapping of climatological temperature and precipitation across
#'   the conterminous United States. *International Journal of Climatology*,
#'   28(15), 2031-2064. \doi{10.1002/joc.1688}
#'
#' Henn, B., Newman, A.J., Livneh, B., Daly, C., & Lundquist, J.D. (2018).
#'   An assessment of differences in gridded precipitation datasets in complex terrain.
#'   *Journal of Hydrology*, 556, 1205-1219. \doi{10.1016/j.jhydrol.2017.03.008}
