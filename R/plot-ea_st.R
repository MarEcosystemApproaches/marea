#' Plot an ea_st object with multiple styles
#'
#' Creates a spatial plot for an `ea_st` object. Styles:
#'   * "fill": geom_sf fill by value
#'   * "contour": contour lines of value
#'   * "bubble": bubble plot (point size ∝ value)
#'
#' @param x An `ea_st` object.
#' @param style Character; one of "fill", "contour", or "bubble".
#' @param ... Additional args passed to the geoms.
#' @return A ggplot object.
#' @export
plot.ea_st <- function(x,
                       style = c("fill", "contour", "bubble"),
                       ...) {
  style <- match.arg(style)
  meta <- attr(x, "meta")
  geom_col <- attr(x, "sf_column")
  df <- x
  
  # Base plot
  p <- ggplot2::ggplot(df)
  
  switch(style,
         fill = {
           p <- p +
             ggplot2::geom_sf(ggplot2::aes(fill = .data$value), color = NA, ...) +
             ggplot2::scale_fill_viridis_c(name = meta$units)
         },
         contour = {
           # Convert to gridded data frame
           coords <- sf::st_coordinates(sf::st_centroid(df))
           df2 <- cbind(data.frame(coords), value = df$value)
           p <- p +
             ggplot2::geom_contour(data = df2,
                                   ggplot2::aes(x = X, y = Y, z = value),
                                   color = "grey30", ...) +
             ggplot2::labs(color = meta$units)
         },
         bubble = {
           # Bubble size proportional to value
           cent <- sf::st_centroid(df)
           coords <- sf::st_coordinates(cent)
           df3 <- cbind(data.frame(coords), value = df$value)
           p <- p +
             ggplot2::geom_point(data = df3,
                                 ggplot2::aes(x = X, y = Y, size = .data$value),
                                 color = "steelblue", ...) +
             ggplot2::scale_size_continuous(name = meta$units)
         }
  )
  
  p + ggplot2::labs(
    title    = meta$data_type,
    subtitle = paste(meta$time_descriptor, "| Source:", meta$source_citation),
    x        = NULL, y = NULL
  ) + ggplot2::theme_bw()
}