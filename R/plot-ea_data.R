#' Plot an ea_data object with multiple styles
#'
#' Creates a time series plot for an `ea_data` object.  Several plotting
#' styles are available:
#'   * "default": simple line + points (same as before)
#'   * "ribbon": line + shaded confidence interval ribbon (requires `low` & `high`)
#'   * "plain": line only, no points or ribbon
#'   * "biomass": mimic pacea biomass plot (ribbon + bold line + points)
#'   * "anomaly": bar plot with red for positive values, blue for negative
#'
#' @param x An `ea_data` object.
#' @param style Character; one of "default", "ribbon", "plain", "biomass", or "red_blue".
#' @param ... Additional arguments passed to the underlying geoms
#' (`geom_line`, `geom_point`, `geom_ribbon`).
#' @return A ggplot object.
# plot.ea_data <- function(x,
#                          style = c("default",
#                                    "ribbon",
#                                    "plain", 
#                                    "biomass",
#                                    "anomaly"),
#                          ...) {
#   style <- match.arg(style)
#   df <- x$data
#   m <- x$meta
#   
#   # Base ggplot mapping
#   p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$value))
#   labs <- list(
#     title    = paste(m$data_type, " for ", m$region),
#     subtitle = paste("Source:", m$source_citation),
#     x        = "Year",
#     y        = paste0(m$data_type, " (", m$units, ")")
#   )
#   
#   # Add geoms by style
#   switch(style,
#          default = {
#            p <- p +
#              ggplot2::geom_line(linewidth = 1, ...) +
#              ggplot2::geom_point(size = 2, ...)
#          },
#          
#          ribbon = {
#            if (!all(c("low","high") %in% names(df))) {
#              stop("Style 'ribbon' requires 'low' and 'high' columns in data.", call. = FALSE)
#            }
#            p <- p +
#              ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$low, ymax = .data$high),
#                                   fill = "grey80", alpha = 0.5, ...) +
#              ggplot2::geom_line(linewidth = 1, ...) +
#              ggplot2::geom_point(size = 2, ...)
#          },
#          
#          plain = {
#            p <- p + ggplot2::geom_line(linewidth = 1, ...)
#          },
#          
#          biomass = {
#            # Check if this is actually biomass data
#            if (!is.null(m$data_type) && !grepl("biomass", m$data_type, ignore.case = TRUE)) {
#              warning("Style 'biomass' is intended for biomass data, but data_type is: ", 
#                      m$data_type, call. = FALSE)
#            }
#            # Validate required columns
#            required_cols <- c("value")
#            missing_cols <- setdiff(required_cols, names(df))
#            if (length(missing_cols) > 0) {
#              stop("Style 'biomass' requires columns: ", 
#                   paste(missing_cols, collapse = ", "), call. = FALSE)
#            }
#            
#            # Check if we have uncertainty columns
#            has_uncertainty <- all(c("low", "high") %in% names(df))
#            
#            if (has_uncertainty) {
#              # Plot with uncertainty ribbon + bold line + points
#              p <- p +
#                ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$low, ymax = .data$high),
#                                     fill = "lightblue", alpha = 0.1, ...) +
#                ggplot2::geom_line(color = "black", linewidth = 2, linetype = 1, ...) +
#                ggplot2::geom_point(color = "black", size = 2, shape = 16, ...)
#            } else {
#              # Plot without uncertainty (just bold line + points)
#              p <- p +
#                ggplot2::geom_line(color = "black", linewidth = 2, linetype = 1, ...) +
#                ggplot2::geom_point(color = "black", size = 2, shape = 16, ...)
#            }
#            
#            # Add uncertainty lines if available (mimicking the dashed uncertainty lines)
#            if (has_uncertainty) {
#              p <- p +
#                ggplot2::geom_line(ggplot2::aes(y = .data$low), 
#                                   color = "blue", linetype = 3, linewidth = 1, ...) +
#                ggplot2::geom_line(ggplot2::aes(y = .data$high), 
#                                   color = "blue", linetype = 3, linewidth = 1, ...)
#            }
#          },
#          
#          anomaly = {
#            # Check if this is actually anomaly data
#            if (!is.null(m$data_type) && !grepl("anomaly", m$data_type, ignore.case = TRUE)) {
#              warning("Style 'anomaly' is intended for anomaly data, but data_type is: ", 
#                      m$data_type, call. = FALSE)
#            }
#            # Validate required columns for anomaly plotting
#            required_cols <- c("value")
#            missing_cols <- setdiff(required_cols, names(df))
#            if (length(missing_cols) > 0) {
#              stop("Style 'anomaly' requires columns: ", 
#                   paste(missing_cols, collapse = ", "), call. = FALSE)
#            }
#            
#            # Create a color variable based on positive/negative values
#            df$bar_color <- ifelse(df$value >= 0, "positive", "negative")
#            
#            # Create bar plot with red for positive, blue for negative
#            p <- p +
#              ggplot2::geom_col(
#                ggplot2::aes(fill = df$bar_color),
#                width = 0.8,
#                ...
#              ) +
#              ggplot2::scale_fill_manual(
#                values = c("positive" = "red", "negative" = "blue"),
#                guide = "none"  # Hide legend since colors are self-explanatory
#              ) +
#              ggplot2::geom_hline(
#                yintercept = 0, 
#                color = "black", 
#                linewidth = 0.5
#              ) +
#              ggplot2::theme_bw() +
#              ggplot2::theme(
#                panel.grid.minor = ggplot2::element_blank(),
#                panel.grid.major.x = ggplot2::element_blank()
#              )
#            
#            # Add uncertainty bars if available (as error bars on top of columns)
#            if (all(c("low", "high") %in% names(df))) {
#              p <- p +
#                ggplot2::geom_errorbar(
#                  ggplot2::aes(ymin = .data$low, ymax = .data$high),
#                  width = 0.3,
#                  color = "black",
#                  linewidth = 0.5,
#                  ...
#                )
#            }
#          }
#   )
#   
#   p + do.call(ggplot2::labs, labs) + ggplot2::theme_bw()
# }