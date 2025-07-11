




#' Plot a red/blue anomaly time series (bar style)
#'
#' Internal function to plot a time series as red and blue bars, without smoothing.
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
plot_red_blue_bar <- function(obj_lub,
                              value,
                              xlab,
                              ylab,
                              type,
                              ...){
  # Check that the required S3 method exists
  if (!exists("plot.pacea_index", where = asNamespace("pacea"), mode = "function")) {
    stop("Required S3 method 'plot.pacea_index' not found in pacea package")
  }
  # TODO: Check if 0 is within range

  obj_lub$y_pos <- ifelse(obj_lub[[value]] >= 0,
                          obj_lub[[value]],
                          0)
  obj_lub$y_neg <- ifelse(obj_lub[[value]] < 0,
                          obj_lub[[value]],
                          0)
  bar_col <- ifelse(obj_lub[[value]] >= 0,
                    "red",
                    "blue")

  plot(obj_lub$date,
       obj_lub[[value]],
       type = "h",
       xlab = xlab,
       ylab = ylab,
       col = bar_col,
       lend = 1,
       ...)
  abline(h = 0)

  invisible()
}

# --- TODOs for developers (not part of user documentation) ---
# TODO: Check if 0 is within range in plot_red_blue_bar

