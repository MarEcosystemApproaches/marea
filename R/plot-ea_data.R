#' Plot an ea_data object with multiple styles
#'
#' Creates a time series plot for an `ea_data` object.  Several plotting
#' styles are available:
#'   * "default": simple line + points (same as before)
#'   * "ribbon": line + shaded confidence interval ribbon (requires `low` & `high`)
#'   * "plain": line only, no points or ribbon
#'   * "pacea": mimic old pacea_biomass plot (ribbon + bold line + points)
#'   * "red_blue": color line segments & points red when value >= 0, blue when < 0
#'
#' @param x An `ea_data` object.
#' @param style Character; one of "default", "ribbon", "plain", "pacea_biomass", or "red_blue".
#' @param ... Additional arguments passed to the underlying geoms
#'   (`geom_line`, `geom_point`, `geom_ribbon`).
#' @return A ggplot object.
#' @export
plot.ea_data <- function(x,
                         style = c("default", "ribbon", "plain", "pacea_biomass", "red_blue"),
                         ...) {
  style <- match.arg(style)
  df <- x$data
  m <- x$meta
  
  # Base ggplot mapping
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$value))
  labs <- list(
    title    = paste(m$data_type, " for ", m$region),
    subtitle = paste("Source:", m$source_citation),
    x        = "Year",
    y        = paste0(m$data_type, " (", m$units, ")")
  )
  
  # Add geoms by style
  switch(style,
         default = {
           p <- p +
             ggplot2::geom_line(linewidth = 1, ...) +
             ggplot2::geom_point(size = 2, ...)
         },
         
         ribbon = {
           if (!all(c("low","high") %in% names(df))) {
             stop("Style 'ribbon' requires 'low' and 'high' columns in data.", call. = FALSE)
           }
           p <- p +
             ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$low, ymax = .data$high),
                                  fill = "grey80", alpha = 0.5, ...) +
             ggplot2::geom_line(linewidth = 1, ...) +
             ggplot2::geom_point(size = 2, ...)
         },
         
         plain = {
           p <- p + ggplot2::geom_line(linewidth = 1, ...)
         },
         
         pacea_biomass = {
           # pacea_biomass code
         },
         
         red_blue = {
          # red blue plot code
         }
  )
  
  p + do.call(ggplot2::labs, labs) + ggplot2::theme_bw()
}