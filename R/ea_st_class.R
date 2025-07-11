# Preamble: Ensure required packages are available
# This class depends on 'sf' for its core structure and 'ggplot2' for plotting.
if (!requireNamespace("sf", quietly = TRUE)) {
  stop("The 'sf' package is required for the 'ea_st' class. Please install it.")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("The 'ggplot2' package is recommended for plotting. Please install it.")
}

# -----------------------------------------------------------------------------
# 1. Helper/User-Facing Constructor with Validation
# -----------------------------------------------------------------------------
#' Create an Ecosystem Approach Spatio-Temporal (ea_st) object
#'
#' This function creates a standardized object for spatio-temporal data,
#' typically gridded data like model outputs or interpolated observations.
#' It builds upon the 'sf' class, ensuring compatibility with the entire
#' spatial R ecosystem.
#'
#' The constructor standardizes the object by renaming the user-specified
#' value column to `value` internally. This simplifies all other S3 methods.
#'
#' @param data An `sf` object. It must contain a geometry column.
#' @param value_col A character string specifying the name of the column in
#'   `data` that contains the primary numeric values to be analyzed and plotted.
#' @param data_type A character string describing the data (e.g., "Bottom Temperature",
#'   "Surface Salinity", "Chlorophyll-a Concentration").
#' @param region A character string. The DFO region or general ocean basin
#'   (e.g., "Maritimes", "Scotian Shelf", "Pacific").
#' @param time_descriptor A character string describing the temporal nature of the
#'   data (e.g., "January 2018 Monthly Mean", "Annual Climatology 1990-2020").
#' @param units A character string for the 'value' column (e.g., "°C", "PSU").
#' @param source_citation A character string citing the data origin.
#' @param ... Additional metadata fields to be stored in the `meta` list.
#'
#' @return An object of class `ea_st`, which also inherits from `sf`, `tbl_df`,
#'   `tbl`, and `data.frame`.
#' @export
#' @examples
#' # Create a sample sf object
#' # A 3x3 grid over a fictional area
#' grid_points <- expand.grid(x = seq(1, 3), y = seq(1, 3))
#' sample_sf <- sf::st_as_sf(grid_points, coords = c("x", "y"), crs = 4326)
#'
#' # Add some fictional data, with a non-standard column name 'temp_c'
#' sample_sf$temp_c <- c(5.1, 5.3, 5.4, 6.0, 6.2, 6.1, 5.8, 5.9, 5.7)
#' sample_sf$month <- "January"
#'
#' # Create an ea_st object, mapping 'temp_c' to be the value column
#' temp_obj <- ea_st(
#'   data = sample_sf,
#'   value_col = "temp_c",
#'   data_type = "Simulated Bottom Temperature",
#'   region = "Maritimes",
#'   time_descriptor = "January Snapshot",
#'   units = "°C",
#'   source_citation = "Internal Simulation"
#' )
#'
#' # Print the object to see the summary and structure
#' print(temp_obj)
#'
#' # The primary data column is now consistently named 'value'
#' head(temp_obj)
#'
#' # The plot method works without needing to know the original column name
#' plot(temp_obj)

ea_st <- function(data,
                  value_col,
                  data_type,
                  region,
                  time_descriptor,
                  units,
                  source_citation = "No citation provided",
                  ...) {
  
  # --- Input Validation ---
  if (!inherits(data, "sf")) {
    stop("`data` must be an `sf` object.", call. = FALSE)
  }
  if (!is.character(value_col) || length(value_col) != 1) {
    stop("`value_col` must be a single character string.", call. = FALSE)
  }
  if (!value_col %in% names(data)) {
    stop(paste0("Column '", value_col, "' not found in the data."), call. = FALSE)
  }
  if (!is.numeric(data[[value_col]])) {
    stop(paste0("Column '", value_col, "' must be numeric."), call. = FALSE)
  }
  
  # --- Standardize the data frame ---
  # Rename the user-specified value column to the standard name `value`
  names(data)[names(data) == value_col] <- "value"
  
  # --- Assemble Metadata ---
  meta <- list(
    data_type = as.character(data_type),
    region = as.character(region),
    time_descriptor = as.character(time_descriptor),
    units = as.character(units),
    source_citation = as.character(source_citation),
    original_value_col = value_col, # Store the original name for reference
    ...
  )
  
  # --- Construct the Object ---
  # Set the attributes on the data itself
  attr(data, "meta") <- meta
  
  # Add the 'ea_st' class in front of the existing sf classes
  class(data) <- c("ea_st", class(data))
  
  return(data)
}


# -----------------------------------------------------------------------------
# 2. S3 Methods for the `ea_st` Class
# -----------------------------------------------------------------------------

#' @export
print.ea_st <- function(x, ...) {
  meta <- attr(x, "meta")
  
  cat("--- Ecosystem Approach Spatio-Temporal (ea_st) Object ---\n")
  cat("Data Type:      ", meta$data_type, "\n")
  cat("Time:           ", meta$time_descriptor, "\n")
  cat("Region:         ", meta$region, "\n")
  cat("Units:          ", meta$units, " (in 'value' column, originally '", meta$original_value_col, "')\n", sep = "")
  cat("-----------------------------------------------------------\n")
  
  # Leverage the excellent default print method from sf
  # To avoid recursion, temporarily remove the 'ea_st' class
  class(x) <- setdiff(class(x), "ea_st")
  print(x, ...)
  
  invisible(x)
}

#' @export
summary.ea_st <- function(object, ...) {
  meta <- attr(object, "meta")
  
  cat("--- Summary of ea_st Object ---\n")
  cat("Metadata:\n")
  cat("  Data Type: ", meta$data_type, "\n")
  cat("  Region:    ", meta$region, "\n")
  cat("  Time:      ", meta$time_descriptor, "\n")
  cat("  Source:    ", meta$source_citation, "\n\n")
  
  cat("Spatial Information (from sf):\n")
  # Print the bounding box and CRS from the underlying sf object
  print(sf::st_geometry(object))
  cat("\n")
  
  cat("Summary of 'value' column (Units: ", meta$units, "):\n", sep = "")
  print(summary(object$value))
  
  invisible(list(meta = meta, value_summary = summary(object$value)))
}



#' Subsetting for ea_st objects
#'
#' Ensures that subsetting operations preserve the `ea_st` class and its
#' metadata attribute. It relies on the robust subsetting methods from `sf`.
#'
#' @param x An object of class `ea_st`.
#' @param i Row indices to subset.
#' @param j Column indices to subset.
#' @param ... Additional arguments passed to `sf`'s subsetting method.
#' @return A new, subsetted `ea_st` object.
#' @export
`[.ea_st` <- function(x, i, j, ...) {
  # Preserve the metadata attribute through subsetting
  meta_attr <- attr(x, "meta")
  
  # Use the next method in the class hierarchy (sf's subsetting)
  # This is the correct way to call the parent method
  class(x) <- setdiff(class(x), "ea_st")
  subset_obj <- NextMethod()
  
  # Re-apply the metadata and class to the result
  attr(subset_obj, "meta") <- meta_attr
  class(subset_obj) <- c("ea_st", class(subset_obj))
  
  return(subset_obj)
}