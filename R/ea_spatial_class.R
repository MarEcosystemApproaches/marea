# Preamble: Ensure required packages are available
# This class depends on 'sf' for its core structure and 'ggplot2' for plotting.
if (!requireNamespace("sf", quietly = TRUE)) {
  stop("The 'sf' package is required for the 'ea_spatial' class. Please install it.")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("The 'ggplot2' package is recommended for plotting. Please install it.")
}

# -----------------------------------------------------------------------------
# 1. Helper/User-Facing Constructor with Validation
# -----------------------------------------------------------------------------
#' Create an Ecosystem Approach Spatio-Temporal (ea_spatial) object
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
#' @return An object of class `ea_spatial`, which also inherits from `sf`, `tbl_df`,
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
#' # Create an ea_spatial object, mapping 'temp_c' to be the value column
#' temp_obj <- ea_spatial(
#'   data = sample_sf,
#'   value_col = "temp_c",
#'   data_type = "Simulated Bottom Temperature",
#'   region = "Maritimes",
#'   time_descriptor = "January Snapshot",
#'   units = "°C",
#'   source_citation = "Internal Simulation"
#' )
#'
ea_spatial <- function(data,
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
  # Create a list structure like ea_data, but with sf data
  structure(
    list(
      data = data,
      meta = meta
    ),
    class = c("ea_spatial", "list")
  )
}


# -----------------------------------------------------------------------------
# 2. S3 Methods for the `ea_spatial` Class
# -----------------------------------------------------------------------------

#' @export
print.ea_spatial <- function(x, ...) {
  cat("--- Ecosystem Approach Spatio-Temporal (ea_spatial) Object ---\n")
  cat("Data Type:      ", x$meta$data_type, "\n")
  cat("Time:           ", x$meta$time_descriptor, "\n")
  cat("Region:         ", x$meta$region, "\n")
  cat("Units:          ", x$meta$units, " (in 'value' column, originally '", x$meta$original_value_col, "')\n", sep = "")
  cat("-----------------------------------------------------------\n")
  
  # Print the sf data component
  print(x$data, ...)
  
  invisible(x)
}

#' @export
summary.ea_spatial <- function(object, ...) {
  cat("--- Summary of ea_spatial Object ---\n")
  cat("Metadata:\n")
  cat("  Data Type: ", object$meta$data_type, "\n")
  cat("  Region:    ", object$meta$region, "\n")
  cat("  Time:      ", object$meta$time_descriptor, "\n")
  cat("  Source:    ", object$meta$source_citation, "\n\n")
  
  cat("Spatial Information (from sf):\n")
  # Print the bounding box and CRS from the underlying sf object
  print(sf::st_geometry(object$data))
  cat("\n")
  
  # If there's a time_descriptor column, show unique values
  if ("time_descriptor" %in% names(object$data)) {
    unique_times <- unique(object$data$time_descriptor)
    cat("Time periods: ", paste(unique_times, collapse = ", "), "\n")
    cat("Number of time periods: ", length(unique_times), "\n\n")
  }
  
  cat("Summary of 'value' column (Units: ", object$meta$units, "):\n", sep = "")
  print(summary(object$data$value))
  
  invisible(list(meta = object$meta, value_summary = summary(object$data$value)))
}

#' Subsetting for ea_spatial objects
#'
#' Ensures that subsetting operations preserve the `ea_spatial` class and its
#' metadata. It handles subsetting of the sf data component while maintaining
#' the list structure with data and meta components.
#'
#' @param x An object of class `ea_spatial`.
#' @param i Row indices to subset.
#' @param j Column indices to subset.
#' @param ... Additional arguments passed to sf's subsetting method.
#' @return A new, subsetted `ea_spatial` object.
#' @export
`[.ea_spatial` <- function(x, i, j, ...) {
  # Subset the sf data component
  if (missing(j)) {
    subset_data <- x$data[i, , ...]
  } else {
    subset_data <- x$data[i, j, ...]
  }
  
  # Recreate the ea_spatial object with the same metadata but subsetted data
  structure(
    list(
      data = subset_data,
      meta = x$meta
    ),
    class = c("ea_spatial", "list")
  )
}

