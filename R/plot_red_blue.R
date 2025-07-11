#' Plot a red/blue anomaly time series (smoothed)
#'
#' Internal function to plot a time series with red shading above zero and blue below zero, using linear interpolation for smooth transitions.
#'
#' @param obj_lub A `pacea_index` object with a `date` column (as `Date`).
#' @param value The column to plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param type Type of plot.
#' @param ... Additional arguments passed to `plot()`.
#'
#' @return A plot is displayed. Nothing is returned.
#' @author Andrew Edwards
#' @examples
#' \dontrun{
#' # see plot.pacea_index()
#' }
plot_red_blue <- function(obj_lub,
                          value,
                          xlab,
                          ylab,
                          type,
                          ...){
  # Check that the required S3 method exists
  if (!exists("plot.pacea_index", where = asNamespace("pacea"), mode = "function")) {
    stop("Required S3 method 'plot.pacea_index' not found in pacea package")
  }
  # TODO: Check if 0 is within range, or test it works for all positive anomalies
  
  obj_lub_interp_list <- approx(x = obj_lub$date,
                                y = obj_lub[[value]],
                                xout = seq(min(obj_lub$date),
                                           max(obj_lub$date),
                                           "days"))
  
  obj_lub_interp <- tibble::tibble(date = obj_lub_interp_list$x,
                                   y = obj_lub_interp_list$y)
  names(obj_lub_interp)[2] <- value
  
  obj_lub_interp$y_pos <- ifelse(obj_lub_interp[[value]] >= 0,
                                 obj_lub_interp[[value]],
                                 0)
  obj_lub_interp$y_neg <- ifelse(obj_lub_interp[[value]] < 0,
                                 obj_lub_interp[[value]],
                                 0)
  
  plot(obj_lub_interp$date,
       obj_lub_interp[[value]],
       type = type,
       xlab = xlab,
       ylab = ylab,
       ...)
  abline(h = 0)
  
  polygon(c(obj_lub_interp$date[1],
            obj_lub_interp$date,
            tail(obj_lub_interp$date, 1)),
          c(0,
            obj_lub_interp$y_pos,
            0),
          col = "red")
  
  polygon(c(obj_lub_interp$date[1],
            obj_lub_interp$date,
            tail(obj_lub_interp$date, 1)),
          c(0,
            obj_lub_interp$y_neg,
            0),
          col = "blue")
  invisible()
}

# --- TODOs for developers (not part of user documentation) ---
# TODO: Check if 0 is within range, or test it works for all positive anomalies in plot_red_blue
