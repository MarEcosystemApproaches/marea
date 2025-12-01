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
#'     ribbon (requires `lower` and `upper` columns in the data).
#'   * `"plain"`: A line plot without points or any other embellishments.
#'   * `"biomass"`: A style that mimics `pacea` biomass plots, featuring a bold
#'     line, points, and an optional uncertainty ribbon.
#'   * `"anomaly"`: A bar plot where positive values are colored red and
#'     negative values are blue, suitable for anomaly time series.
#'   * `"histogram"`: A simple bar plot showing values by year. Creates a 
#'     single-layer plot that can be easily customized with additional geoms
#'     like trend lines or reference lines.
#'   * `"indicator"`: Ecosystem indicator style with mean reference line and 
#'     trend analysis. Shows long-term mean and recent 5-year period highlighting.
#'   * `"indicator_ref"`: Indicator style with 1991-2020 climate reference period. 
#'     Shows standardized anomalies relative to climate normal period.
#'   * `"diversity"`: Specialized style for diversity indices with regime 
#'     change detection and period comparisons.
#'   * `"temperature_regime"`: Temperature anomaly visualization with regime 
#'     shift detection, warm/cold period highlighting, and trend analysis.
#'   * `"nao_enhanced"`: Enhanced NAO visualization with phase indicators,
#'     regime periods, and standardized anomaly coloring.
#'     
#' @param x An `ea_data` object.
#' @param y Ignored. Included for consistency with the generic `plot` method.
#' @param style Character; One of: `"default"`, `"ribbon"`, 
#'   `"plain"`, `"biomass"`, `"anomaly"`, `"histogram"`, `"indicator"`, 
#'   `"indicator_ref"`, `"diversity"`, `"temperature_regime"`, `"nao_enhanced"`.
#' @param reference_period Numeric vector of length 2. Years defining reference 
#'   period for standardized anomalies. Default is c(1991, 2020) for climate 
#'   consistency. Used with `"indicator_ref"` and `"temperature_regime"` styles.
#' @param sd_threshold Numeric. Number of standard deviations for threshold lines
#'  and point classification in indicator styles. Default is 1 (+/-1 SD). Use 0.5
#'  for tighter bounds or larger values for wider bounds.
#' @param highlight_recent Logical. Whether to highlight the most recent 5 years.
#'   Default is TRUE for indicator styles.
#' @param show_trend Logical. Whether to add trend line and statistics. Default
#'   is TRUE for indicator styles.
#' @param regime_threshold Numeric. Threshold for regime change detection in 
#'   standardized units. Default is 0.5 standard deviations.
#' @param mean_line_color Color for mean reference line. Default is "red".
#' @param trend_line_color Color for trend line. Default is "blue".
#' @param warm_color Color for warm/positive anomalies. Default is "red".
#' @param cold_color Color for cold/negative anomalies. Default is "blue".
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
#' df$lower <- df$biomass_t * 0.8
#' df$upper <- df$biomass_t * 1.2
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
#' plot(biomass_obj, style = "histogram")
#' 
#' # Histogram with custom additions
#' plot(biomass_obj, style = "histogram") +
#'   ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red")
#' 
setGeneric("plot")

#' @rdname plot-ea_data
#' @export
setMethod("plot", signature(x = "ea_data", y = "missing"),
          function(x,
                   style = c("default", "ribbon", "plain", "biomass", "anomaly",
                             "histogram", "indicator", "indicator_ref", "diversity", 
                             "temperature_regime", "nao_enhanced"),
                   reference_period = c(1991, 2020),
                   sd_threshold = 1, 
                   highlight_recent = TRUE,
                   show_trend = TRUE,
                   regime_threshold = 0.5,
                   mean_line_color = "red",
                   trend_line_color = "blue", 
                   warm_color = "red",
                   cold_color = "blue",
                   ...) {
            style <- match.arg(style)
            
            # Access slots using the S4 accessor methods for robustness
            df <- x[["data"]]
            m <- x[["meta"]]
            
            # Rename value column
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
            
            # Base ggplot mapping
            p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$value))
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
                if (!all(c("lower", "upper") %in% names(df))) {
                  stop("Style 'ribbon' requires 'lower' and 'upper' columns in data.", call. = FALSE)
                }
                p +
                  ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
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
                    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                                         fill = "lightblue", alpha = 0.3, ...)
                }
                
                # Add main line and points
                p <- p +
                  ggplot2::geom_line(color = "black", linewidth = 1.5, ...) +
                  ggplot2::geom_point(color = "black", size = 2.5, shape = 16, ...)
                
                # Add dashed lines for uncertainty bounds
                if (has_uncertainty) {
                  p <- p +
                    ggplot2::geom_line(ggplot2::aes(y = .data$lower),
                                       color = "blue", linetype = "dashed", linewidth = 0.8, ...) +
                    ggplot2::geom_line(ggplot2::aes(y = .data$upper),
                                       color = "blue", linetype = "dashed", linewidth = 0.8, ...)
                }
                p
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
                    guide = "none"
                  ) +
                  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5)
                
                # Add error bars for uncertainty if available
                if (all(c("lower", "upper") %in% names(df))) {
                  p <- p +
                    ggplot2::geom_errorbar(
                      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                      width = 0.3, color = "black", linewidth = 0.5, ...
                    )
                }
                p
              },
              
              histogram = {
                # Simple histogram/bar plot with year on x-axis
                # Uses fill = "steelblue" as default, can be overridden via ...
                # Creates a clean, single-layer base that users can add to
                p +
                  ggplot2::geom_col(fill = "steelblue", color = NA, width = 0.8, ...)
              },
              
              # MAReco-inspired styles
              indicator = {
                # Calculate statistics
                long_mean <- mean(df$value, na.rm = TRUE)
                sd_value <- sd(df$value, na.rm = TRUE)
                recent_years <- tail(df$year, 5)
                recent_data <- df[df$year %in% recent_years, ]
                
                # Create color classification for points BEFORE creating base plot
                df$point_color <- ifelse(df$value > long_mean + sd_threshold * sd_value, "above_sd",
                                         ifelse(df$value < long_mean - sd_threshold * sd_value, "below_sd", "within_sd"))
                df$point_color <- factor(df$point_color, levels = c("below_sd", "within_sd", "above_sd"))
                
                p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$value))
                
                p <- p +
                  ggplot2::geom_line(color = "black", linewidth = 0.8, ...) +
                  ggplot2::geom_point(ggplot2::aes(color = .data$point_color), ...) +
                  ggplot2::geom_hline(yintercept = long_mean, color = "darkgreen", 
                                      linetype = "dashed", linewidth = 1)+
                  ggplot2::geom_hline(yintercept = long_mean + sd_threshold * sd_value, color = "darkgreen", 
                                      linetype = "dotted", linewidth = 0.8) +  # +SD threshold
                  ggplot2::geom_hline(yintercept = long_mean - sd_threshold * sd_value, color = "darkgreen", 
                                      linetype = "dotted", linewidth = 0.8) +  # -SD threshold
                  scale_color_manual(
                    values = c("above_sd" = "orange", "below_sd" = "blue", "within_sd" = "black"),
                    labels = c(paste0("Below -", sd_threshold, " SD"), 
                               paste0("Within +/-", sd_threshold, " SD"), 
                               paste0("Above +", sd_threshold, " SD")),
                    name = "Status",
                    na.translate = FALSE
                  )
                  

                  
                # Highlight recent period
                if (highlight_recent && nrow(recent_data) > 0) {
                  p <- p +
                    annotate(geom = "rect",
                             xmin = min(recent_years), xmax = max(recent_years),
                             ymin = -Inf, ymax = Inf,
                             fill = "purple2", alpha = 0.2)
                }
                
                # Add trend line
                if (show_trend) {
                  p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, 
                                                color = trend_line_color, linewidth = 0.8, ...)
                }
                
                # Update labels
                labs$subtitle <- paste0(labs$subtitle, 
                                        sprintf(" | Long-term mean: %.2f %s", 
                                                long_mean, m$units))
                p
              },
              
              indicator_ref = {
                # Filter data for reference period (1991-2020)
                ref_data <- df[df$year >= reference_period[1] & df$year <= reference_period[2], ]
                
                if (nrow(ref_data) == 0) {
                  stop("No data available for reference period ", 
                       reference_period[1], "-", reference_period[2], call. = FALSE)
                }
                
                # Calculate mean and standard deviation for reference period
                ref_mean <- mean(ref_data$value, na.rm = TRUE)
                ref_sd <- sd(ref_data$value, na.rm = TRUE)
                
                # Determine the last 5 years in the data
                end_year <- max(df$year, na.rm = TRUE)
                start_year <- end_year - 4
                recent_data <- df[df$year >= start_year, ]
                
                # Create color classification for points BEFORE creating base plot
                df$point_color <- ifelse(df$value > ref_mean + sd_threshold * ref_sd, "above_sd",
                                         ifelse(df$value < ref_mean - sd_threshold * ref_sd, "below_sd", "within_sd"))
                df$point_color <- factor(df$point_color, levels = c("below_sd", "within_sd", "above_sd"))
                # Recreate base plot with updated df
                p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$value))
                
                p <- p +
                  ggplot2::geom_path(color = "black", linewidth = 0.8, ...) +
                  ggplot2::geom_hline(yintercept = ref_mean, color = 'darkgreen', 
                                      linetype = "dashed", linewidth = 1) +
                  ggplot2::geom_hline(yintercept = ref_mean + sd_threshold * ref_sd, color = "darkgreen", 
                                      linewidth = 0.8) +  # +SD threshold
                  ggplot2::geom_hline(yintercept = ref_mean - sd_threshold * ref_sd, color = "darkgreen", 
                                      linewidth = 0.8)  # -SD threshold
                
                # Highlight recent period (last 5 years)
                if (highlight_recent && nrow(recent_data) > 0) {
                  p <- p +
                    ggplot2::annotate(geom = "rect",
                                      xmin = start_year, xmax = end_year,
                                      ymin = -Inf, ymax = Inf,
                                      fill = "purple2", alpha = 0.2)
                }
                
                # Add colored points based on whether they're outside 1 SD
                p <- p +
                  ggplot2::geom_point(ggplot2::aes(color = .data$point_color), size = 2.5) +
                  ggplot2::scale_color_manual(
                    values = c("above_sd" = "orange", "below_sd" = "blue", "within_sd" = "black"),
                    labels = c(paste0("Below -", sd_threshold, " SD"),
                               paste0("Within +/-", sd_threshold, " SD"),
                               paste0("Above +", sd_threshold, " SD")),
                    name = "Status",
                    na.translate = FALSE
                  )
                
                # Add trend line
                if (show_trend) {
                  p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, 
                                                color = trend_line_color, linewidth = 0.8, ...)
                }
                
                # Update labels
                labs$subtitle <- paste0(labs$subtitle, 
                                        sprintf(" | Reference period (%d-%d): %.2f +/- %.2f %s",
                                                reference_period[1], reference_period[2],
                                                ref_mean, ref_sd, m$units))
                p
              },
              
              diversity = {
                # Diversity-specific visualization with regime detection
                df_smooth <- df
                if (nrow(df_smooth) > 10) {
                  # Apply smoothing for regime detection
                  df_smooth$smooth_value <- stats::smooth.spline(df_smooth$year, df_smooth$value, df = 5)$y
                  
                  p <- p +
                    ggplot2::geom_line(color = "grey70", linewidth = 0.8, alpha = 0.7, ...) +
                    ggplot2::geom_line(ggplot2::aes(y = .data$smooth_value), 
                                       color = "black", linewidth = 1.2, ...) +
                    ggplot2::geom_point(size = 1.2, alpha = 0.8, ...)
                } else {
                  p <- p +
                    ggplot2::geom_line(color = "black", linewidth = 1, ...) +
                    ggplot2::geom_point(size = 2, ...)
                }
                
                # Add mean line
                diversity_mean <- mean(df$value, na.rm = TRUE)
                p <- p +
                  ggplot2::geom_hline(yintercept = diversity_mean, color = mean_line_color, 
                                      linetype = "dashed", linewidth = 1)
                
                labs$subtitle <- paste0(labs$subtitle, 
                                        sprintf(" | Mean diversity: %.3f", diversity_mean))
                p
              },
              
              temperature_regime = {
                # Temperature regime analysis with warm/cold periods
                temp_mean <- mean(df$value, na.rm = TRUE)
                temp_sd <- stats::sd(df$value, na.rm = TRUE)
                
                # Create regime indicators
                df$regime <- ifelse(df$value > temp_mean + regime_threshold * temp_sd, "warm",
                                    ifelse(df$value < temp_mean - regime_threshold * temp_sd, "cold", "normal"))
                df$regime_color <- factor(df$regime, levels = c("cold", "normal", "warm"))
                
                p <- p +
                  ggplot2::geom_line(color = "black", linewidth = 0.8, alpha = 0.7, ...) +
                  ggplot2::geom_point(ggplot2::aes(color = .data$regime_color), size = 2.5, ...) +
                  ggplot2::scale_color_manual(
                    values = c("cold" = cold_color, "normal" = "grey50", "warm" = warm_color),
                    name = "Regime"
                  ) +
                  ggplot2::geom_hline(yintercept = temp_mean, color = "black", 
                                      linetype = "solid", linewidth = 0.8) +
                  ggplot2::geom_hline(yintercept = temp_mean + c(-1, 1) * regime_threshold * temp_sd,
                                      color = "grey50", linetype = "dashed", alpha = 0.7)
                
                # Add trend line
                if (show_trend) {
                  p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE, 
                                                color = trend_line_color, alpha = 0.3, ...)
                }
                
                labs$subtitle <- paste0(labs$subtitle, 
                                        sprintf(" | Mean: %.2f C, Regime threshold: +/-%.1f SD",
                                                temp_mean, regime_threshold))
                p
              },
              
              nao_enhanced = {
                # Enhanced NAO visualization with phase indicators
                if (!grepl("NAO|North Atlantic Oscillation", m$data_type, ignore.case = TRUE)) {
                  warning("Style 'nao_enhanced' is intended for NAO data, but data_type is: ",
                          m$data_type, call. = FALSE)
                }
                
                # Define NAO phases
                df$nao_phase <- ifelse(df$value > 1, "strong_positive",
                                       ifelse(df$value > 0, "positive", 
                                              ifelse(df$value < -1, "strong_negative", "negative")))
                df$phase_color <- factor(df$nao_phase, 
                                         levels = c("strong_negative", "negative", "positive", "strong_positive"))
                
                p <- p +
                  ggplot2::geom_col(ggplot2::aes(fill = .data$phase_color), width = 0.8, ...) +
                  ggplot2::scale_fill_manual(
                    values = c("strong_negative" = "#053061", "negative" = "#4393c3", 
                               "positive" = "#d6604d", "strong_positive" = "#67001f"),
                    name = "NAO Phase",
                    labels = c("Strong Negative", "Negative", "Positive", "Strong Positive")
                  ) +
                  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1) +
                  ggplot2::geom_hline(yintercept = c(-1, 1), color = "grey30", 
                                      linetype = "dashed", alpha = 0.8)
                
                # Add smoothed trend line
                if (show_trend && nrow(df) > 10) {
                  p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE, 
                                                color = "black", alpha = 0.3, span = 0.3, ...)
                }
                
                labs$subtitle <- paste0(labs$subtitle, " | Phases: |NAO| > 1 = Strong, 0 < |NAO| < 1 = Moderate")
                p
              }
            )
            
            # Apply labels and theme to the final plot
            p + do.call(ggplot2::labs, labs) + ggplot2::theme_bw()
          }
)

# Helper functions for MAReco-style calculations
#' Calculate indicator statistics for reference periods
#' @noRd
calculate_indicator_stats <- function(data, reference_period = c(1991, 2020)) {
  if (length(reference_period) != 2) {
    stop("reference_period must be a vector of length 2 (start_year, end_year)")
  }
  
  ref_data <- data[data$year >= reference_period[1] & data$year <= reference_period[2], ]
  all_data <- data
  
  list(
    reference = list(
      mean = mean(ref_data$value, na.rm = TRUE),
      sd = stats::sd(ref_data$value, na.rm = TRUE),
      n = nrow(ref_data)
    ),
    longterm = list(
      mean = mean(all_data$value, na.rm = TRUE),
      sd = stats::sd(all_data$value, na.rm = TRUE),
      n = nrow(all_data)
    ),
    recent_5yr = {
      recent_years <- tail(sort(all_data$year), 5)
      recent_data <- all_data[all_data$year %in% recent_years, ]
      list(
        mean = mean(recent_data$value, na.rm = TRUE),
        years = recent_years
      )
    }
  )
}

#' Detect regime shifts in time series data
#' @noRd
detect_regime_shifts <- function(data, threshold = 0.5) {
  if (nrow(data) < 10) {
    return(data$regime <- rep("insufficient_data", nrow(data)))
  }
  
  mean_val <- mean(data$value, na.rm = TRUE)
  sd_val <- stats::sd(data$value, na.rm = TRUE)
  
  data$regime <- ifelse(data$value > mean_val + threshold * sd_val, "high",
                        ifelse(data$value < mean_val - threshold * sd_val, "low", "normal"))
  
  return(data$regime)
}
