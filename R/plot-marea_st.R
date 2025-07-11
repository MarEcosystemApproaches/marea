#' Plot a marea spatiotemporal data layer
#'
#' Quickly visualize spatiotemporal data from the Maritimes region using `ggplot2`.
#' This function is designed for objects of class `marea_st`, such as `glorys_bottom_temperature`.
#' It uses Maritimes-specific defaults and calls `pacea::plot.pacea_st()` internally.
#'
#' You can specify which months and years to display. For more options and customizable plots, see the package vignette.
#'
#' @param x A `marea_st` object (an `sf` object containing spatial and temporal data).
#' @param months.plot Character or numeric vector indicating which months to include (e.g., `c(1, 2)`, `c("April", "May")`, or `c(1, "April")`).
#' @param years.plot Numeric vector of years to include (from 1993 to 2019).
#' @param ... Additional arguments (currently not used, included for compatibility).
#'
#' @return A plot of the spatial data is displayed. Nothing is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' plot(glorys_bottom_temperature)
#' }
plot.marea_st <- function(x,
                          ...){
  .plot_pacea_st(x, bc = FALSE, eez = FALSE, ...)
}

# --- TODOs for developers (not part of user documentation) ---
# Utilize months.plot and years.plot
# Add more options for customizing the plot
# maritimes specific defaults needed?