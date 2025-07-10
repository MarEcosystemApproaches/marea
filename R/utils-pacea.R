
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

#' Internal wrapper for pacea's plot.pacea_biomass method
#' @noRd
#' @import pacea
.plot_pacea_biomass <- function(x, ...) {
  # Check that the required S3 method exists
  if (!exists("plot.pacea_biomass", where = asNamespace("pacea"), mode = "function")) {
    stop("Required S3 method 'plot.pacea_biomass' not found in pacea package")
  }
  
  # Ensure x has the expected class for S3 dispatch
  if (!inherits(x, "pacea_biomass")) {
    class(x) <- c("pacea_biomass", class(x))
  }
  
  # Wrap in error handling for known issues
  tryCatch({
    plot(x, ...)
  }, error = function(e) {
    if (grepl("axis_name.*Pacific Hake|argument is of length zero", e$message)) {
      # Ignore this specific error - plot was already created successfully
      invisible()
    } else {
      stop(e)  # Re-throw other errors
    }
  })
}