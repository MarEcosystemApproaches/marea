#' Plot a marea spatiotemporal data layer
#'
#' Plot for `marea_st` classes of objects using `ggplot()`. Objects can
#' currently be `glorys_bottom_temperature`. Calls `pacea::plot.pacea(st)` with
#' Maritimes-specific defaults.
#'
#' Gives a quick visualization of data, specifying month(s) and year(s). For
#'  more options and configurable plots see vignette.
#'
#' @param x a `marea_st` object, which is an `sf` object
#' @param months.plot character or numeric vector to indicate which months to include (e.g. `c(1, 2)`, `c("April", "may")`, `c(1, "April")`)
#' @param years.plot vector of years to include, from 1993 to 2019
#' @param ... other arguments to be passed on, but not currently used (`?ggplot`
#'   says the same thing); this should remove a R-CMD-check warning.
#'
#' @return plot of the spatial data to the current device (returns nothing)
#'
#' @importFrom sf st_drop_geometry st_transform
#' @importFrom dplyr left_join select mutate arrange
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes theme_bw theme geom_sf scale_fill_gradientn guides guide_colorbar guide_legend labs facet_grid facet_wrap xlab ylab
#' @importFrom pals jet cividis ocean.oxy plasma ocean.algae ocean.tempo
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot(glorys_bottom_temperature)
#' }
plot.marea_st <- function(x,
                          ...){
  pacea:::plot.pacea_st(x,
                        bc = FALSE,
                        eez = FALSE,
                        ...)
}
