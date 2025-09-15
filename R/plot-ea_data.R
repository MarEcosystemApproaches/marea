#' Plot an ea_data object with multiple styles
#'
#' @description
#' Creates a time series plot for an `ea_data` object. Several plotting styles
#' are available, allowing for flexible visualization of time series data,
#' uncertainty, and anomalies.
#' @details
#' The available styles are:
#'   * `"default"`: A simple line plot with points.
#'   * `"ribbon"`: A line plot with points and a shaded confidence interval
#'     ribbon (requires `low` and `high` columns in the data).
#'   * `"plain"`: A line plot without points or any other embellishments.
#'   * `"biomass"`: A style that mimics `pacea` biomass plots, featuring a bold
#'     line, points, and an optional uncertainty ribbon.
#'   * `"anomaly"`: A bar plot where positive values are colored red and
#'     negative values are blue, suitable for anomaly time series.
#'
#' @param x An `ea_data` object.
#' @param y Ignored. Included for consistency with the generic `plot` method.
#' @param style Character; one of `"default"`, `"ribbon"`, `"plain"`, `"biomass"`, or `"anomaly"`.
#' @param ... Additional arguments passed to the underlying geoms
#'   (`geom_line`, `geom_point`, `geom_ribbon`, `geom_col`, `geom_errorbar`).
#'
#' @return A `ggplot` object.
#'
#' @export
#' @rdname plot-ea_data
#' @aliases plot,ea_data,missing-method
#'
#' @examples
#' # Create sample data with uncertainty
#' df <- data.frame(
#'   year = 2000:2010,
#'   biomass_t = rlnorm(11, meanlog = 5, sdlog = 0.3)
#' )
#' df$low <- df$biomass_t * 0.8
#' df$high <- df$biomass_t * 1.2
#'
#' # Create an ea_data object
#' biomass_obj <- ea_data(df,
#'                        value_col = "biomass_t",
#'                        data_type = "Haddock Biomass",
#'                        region = "Georges Bank",
#'                        location_descriptor = "5Z",
#'                        units = "tonnes")
#'
#' # Use different plotting styles
#' plot(biomass_obj, style = "default")
#' plot(biomass_obj, style = "ribbon")
#' plot(biomass_obj, style = "biomass")
#' 
setGeneric("plot")

#' @rdname plot-ea_data
#' @export
setMethod("plot", signature(x = "ea_data", y = "missing"),
          function(x,
                   style = c("default",
                             "ribbon",
                             "plain",
                             "biomass",
                             "anomaly"),
                   ...) {
            style <- match.arg(style)
            
            # Access slots using the S4 accessor methods for robustness
            df <- x[["data"]]
            m <- x[["meta"]]
            
            # TODO make a reasonable default for data with multiple value columns
            # (plot first value column)
            #rename to value so that plotting code can call it easily
            
            # # gather value columns
             val_ind <- grep(names(df), pattern = '_value$')

             if (length(val_ind) == 0) {
               stop("No value column found in data. Value column must end with '_value'.", call. = FALSE)
             } else if (length(val_ind) > 1) {
               warning("Multiple value columns found. Using the first one: ", names(df)[val_ind[1]], call. = FALSE)
               df <- df %>%
                 dplyr::rename(value = dplyr::all_of(names(df)[val_ind[1]]))
             } else {
               df <- df %>%
                 dplyr::rename(value = dplyr::all_of(names(df)[val_ind]))
             }
            # 
            # Base ggplot mapping
            p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$value)) # TODO update to plot val col determined above either first or only
            labs <- list(
              title    = paste(m$data_type, "for", m$region),
              subtitle = paste("Source:", m$source_citation),
              x        = "Year",
              y        = paste0(m$data_type, " (", m$units, ")")
            )
            
            # Add geoms by style
            p <- switch(
              style,
              default = {
                p +
                  ggplot2::geom_line(linewidth = 1, ...) +
                  ggplot2::geom_point(size = 2, ...)
              },
              
              ribbon = {
                if (!all(c("low", "high") %in% names(df))) {
                  stop("Style 'ribbon' requires 'low' and 'high' columns in data.", call. = FALSE)
                }
                p +
                  ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$low, ymax = .data$high),
                                       fill = "grey80", alpha = 0.5, ...) +
                  ggplot2::geom_line(linewidth = 1, ...) +
                  ggplot2::geom_point(size = 2, ...)
              },
              
              plain = {
                p + ggplot2::geom_line(linewidth = 1, ...)
              },
              
              biomass = {
                if (!is.null(m$data_type) && !grepl("biomass", m$data_type, ignore.case = TRUE)) {
                  warning("Style 'biomass' is intended for biomass data, but data_type is: ",
                          m$data_type, call. = FALSE)
                }
                
                has_uncertainty <- all(c("low", "high") %in% names(df))
                
                # Add ribbon if uncertainty is present
                if (has_uncertainty) {
                  p <- p +
                    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$low, ymax = .data$high),
                                         fill = "lightblue", alpha = 0.3, ...)
                }
                
                # Add main line and points
                p <- p +
                  ggplot2::geom_line(color = "black", linewidth = 1.5, ...) +
                  ggplot2::geom_point(color = "black", size = 2.5, shape = 16, ...)
                
                # Add dashed lines for uncertainty bounds
                if (has_uncertainty) {
                  p <- p +
                    ggplot2::geom_line(ggplot2::aes(y = .data$low),
                                       color = "blue", linetype = "dashed", linewidth = 0.8, ...) +
                    ggplot2::geom_line(ggplot2::aes(y = .data$high),
                                       color = "blue", linetype = "dashed", linewidth = 0.8, ...)
                }
                p # Return the modified plot
              },
              
              anomaly = {
                if (!is.null(m$data_type) && !grepl("anomaly", m$data_type, ignore.case = TRUE)) {
                  warning("Style 'anomaly' is intended for anomaly data, but data_type is: ",
                          m$data_type, call. = FALSE)
                }
                
                # Create a color variable based on positive/negative values
                df$bar_color <- ifelse(df$value >= 0, "positive", "negative")
                
                # Create bar plot with red for positive, blue for negative
                p <- p +
                  ggplot2::geom_col(ggplot2::aes(fill = df$bar_color), ...) +
                  ggplot2::scale_fill_manual(
                    values = c("positive" = "red", "negative" = "blue"),
                    guide = "none" # Hide legend
                  ) +
                  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5)
                
                # Add error bars for uncertainty if available
                if (all(c("low", "high") %in% names(df))) {
                  p <- p +
                    ggplot2::geom_errorbar(
                      ggplot2::aes(ymin = .data$low, ymax = .data$high),
                      width = 0.3, color = "black", linewidth = 0.5, ...
                    )
                }
                p # Return the modified plot
              }
            )
            
            # Apply labs and theme to the final plot
            p + do.call(ggplot2::labs, labs) + ggplot2::theme_bw()
          })
