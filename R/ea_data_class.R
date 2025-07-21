# Preamble: Ensure required packages are available
# We use ggplot2 for plotting and dplyr for data manipulation.
if (!requireNamespace("ggplot2", quietly = TRUE)) {
message("The 'ggplot2' package is recommended for plotting. Please install it.")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
stop("The 'dplyr' package is required for the 'ea_data' constructor. Please install it.")
}

# -----------------------------------------------------------------------------
# 1. Low-level Constructor (for internal use)
# -----------------------------------------------------------------------------
# This function creates the object without validation. It's fast and simple.
new_ea_data <- function(data = tibble::tibble(), meta = list()) {
  stopifnot(is.data.frame(data))
  stopifnot(is.list(meta))
  
  structure(
    list(
      data = data,
      meta = meta
    ),
    class = c("ea_data", "list")
  )
}

# -----------------------------------------------------------------------------
# 2. Helper/User-Facing Constructor with Validation
# -----------------------------------------------------------------------------
#' Create an Ecosystem Approach (EA) data object
#'
#' This is the main user-facing function to create a standardized data object
#' for ecosystem time series. It combines a data frame of observations with a
#' list of essential metadata and performs validation to ensure the object is
#' well-formed. The constructor standardizes the object by renaming the
#' user-specified value column to `value` internally.
#'
#' @param data A data.frame or tibble containing the time series data. Must
#' contain a 'year' column and the column specified by `value_col`.
#' @param value_col A character string specifying the name of the column in `data`
#' that contains the primary numeric values to be analyzed.
#' @param data_type A character string describing the data (e.g., "Commercial Catch",
#' "Survey Index", "Bottom Temperature", "Chlorophyll-a").
#' @param region A character string. The DFO region or general ocean basin
#' (e.g., "Pacific", "Maritimes", "Scotian Shelf").
#' @param location_descriptor A character string. A specific identifier for the
#' data's spatial context (e.g., "4X", "WCVI", "Station P").
#' @param units A character string for the 'value' column (e.g., "tonnes", "kg/tow", "degC").
#' @param species A character string. The common or scientific name of the species.
#' This is optional and should be omitted for non-biological data.
#' @param source_citation A character string citing the data origin.
#' @param ... Additional metadata fields to be stored in the `meta` list.
#'
#' @return An object of class `ea_data`.
#' @export
#' @examples
#' # Example 1: Environmental Data with a custom value column name 'avg_temp'
#' temp_df <- data.frame(
#' year = 1995:2015,
#' avg_temp = 7.5 + cumsum(rnorm(21, 0, 0.1)),
#' month = "August"
#' )
#'
#' bottom_temp_obj <- ea_data(
#' data = temp_df,
#' value_col = "avg_temp",  # <-- Specify the value column here
#' data_type = "Annual Bottom Temperature",
#' region = "Scotian Shelf",
#' location_descriptor = "4X Survey Area",
#' units = "degC",
#' source_citation = "DFO RV Survey Database (2023)"
#' )
#'
#' # Example 2: Fisheries Data where the value column is already named 'value'
#' catch_df <- data.frame(
#' year = 2000:2010,
#' value = round(runif(11, 500, 800)),
#' area = "5A/B"
#' )
#'
#' rockfish_obj <- ea_data(
#' data = catch_df,
#' value_col = "value", # <-- Still specify it, even if it's the default name
#' data_type = "Commercial Catch",
#' region = "Pacific",
#' location_descriptor = "BC North Coast",
#' units = "tonnes",
#' species = "Yelloweye Rockfish",
#' source_citation = "DFO Catch Monitoring Program (2023)"
#' )
ea_data <- function(data,
                    value_col,
                    data_type,
                    region,
                    location_descriptor,
                    units,
                    species = NA_character_,
                    source_citation = "No citation provided",
                    ...) {
  
  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.", call. = FALSE)
  }
  if (missing(value_col) || !is.character(value_col) || length(value_col) != 1) {
    stop("`value_col` must be a single character string specifying the value column name.", call. = FALSE)
  }
  if (!value_col %in% names(data)) {
    stop(paste0("Column '", value_col, "' not found in the data."), call. = FALSE)
  }
  if (!"year" %in% names(data)) {
    stop("`data` must contain a 'year' column.", call. = FALSE)
  }
  if (!is.numeric(data[[value_col]]) || !is.numeric(data$year)) {
    stop(paste0("Columns 'year' and '", value_col, "' must be numeric."), call. = FALSE)
  }
  
  # --- Standardize Data: Rename the value column ---
  # This uses dplyr::rename and non-standard evaluation correctly.
  # The original name is kept for the metadata.
  original_value_col <- value_col
  data <- dplyr::rename(data, value = !!original_value_col)
  
  # --- Assemble Metadata ---
  meta <- list(
    data_type = as.character(data_type),
    region = as.character(region),
    location_descriptor = as.character(location_descriptor),
    units = as.character(units),
    species = as.character(species),
    source_citation = as.character(source_citation),
    original_value_col = original_value_col, # Store original name for reference
    ...
  )
  
  # Use the low-level constructor to build the object
  new_ea_data(data = tibble::as_tibble(data), meta = meta)
}

# 3. S3 methods


#'  @export
print.ea_data <- function(x, ...) {
  cat("--- Ecosystem Approach (EA) Data Object --- \n")
  cat("Class:    ", class(x)[1], "\n")
  cat("Data Type:    ", x$meta$data_type, "\n")
  if (!is.na(x$meta$species) && !is.null(x$meta$species)) {
    cat("Species:    ", x$meta$species, "\n")
  }
  cat("Location:    ", x$meta$location_Descriptor, " (", x$meta$region, " Region ) \n")
  cat("Time Range:    ", min(x$data$year, na.rm = TRUE), " - ", max(x$data$year, na.rm = TRUE), "\n")
  cat("Units:    ", x$meta$units, "\n")
  cat("--------------------------------------------\n")
  cat("Data Preview:\n")
  print.data.frame(head(x$data), ...)
  invisible(x)
}


#' @export
summary.ea_data <- function(object, ...) {
  summary_stats <- summary(object$data$value)
  
  cat("--- Summary of ea_data ---\n")
  cat("Metadata:\n")
  cat("  Data Type: ", object$meta$data_type, "\n")
  if (!is.na(object$meta$species) && !is.null(object$meta$species) && object$meta$species != "") {
    cat("  Species: ", object$meta$species, "\n")
  }
  cat("  Location:  ", object$meta$location_descriptor, "\n")
  cat("  Region:  ", object$meta$region, "\n")
  cat("  Source:  ", object$meta$source_citation, "\n\n")
  
  cat("Data Overview:\n")
  cat("  Time range: ", min(object$data$year, na.rm = TRUE), " to ", max(object$data$year, na.rm = TRUE), "\n")
  cat("  Number of observations: ", nrow(object$data), "\n\n")
  
  cat("Summary of 'value' column (Units: ", object$meta$units, "):\n", sep = "")
  print(summary_stats)
  
  invisible(list(meta = object$meta, value_summary = summary_stats))
}



#' Subsetting for ea_data objects
#'
#' Allows for row-based subsetting of the data component while preserving
#' the metadata and class. Column subsetting is disabled to maintain integrity.
#'
#' @param x An object of class `ea_data`.
#' @param i Row indices to subset.
#' @param j Column indices (ignored).
#' @param ... Additional arguments (ignored).
#' @return A new, subsetted `ea_data` object.
#' @export
#' 
`[.ea_data` <- function(x, i, j, ...) {
  if (!missing(j)) {
    warning("Column subsetting (`j`) is not supported and will be ignored to preserve object structure.")
  }
  
  # Subset the data frame by rows
  subset_data <- x$data[i, , drop = FALSE]
  
  # Re-create the object with the same metadata but subsetted data
  new_ea_data(data = subset_data, meta = x$meta)
}
