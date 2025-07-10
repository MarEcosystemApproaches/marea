
#' Internal wrapper for pacea's plot.pacea_st method
#' @noRd
#' @import pacea
.plot_pacea_st <- function(x, ...) {
  # Check that the required S3 method exists
  if (!exists("plot.pacea_st", where = asNamespace("pacea"), mode = "function")) {
    stop("Required S3 method 'plot.pacea_st' not found in pacea package")
  }
  # This makes it explicit that we're relying on pacea's S3 method
  if (!inherits(x, "pacea_st")) {
    stop("x must be of class 'pacea_st'")
  }
  plot(x, ...)
}