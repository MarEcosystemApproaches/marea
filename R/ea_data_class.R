# Preamble: Ensure required packages are available
# We use ggplot2 for plotting and dplyr for data manipulation.
if (!requireNamespace("ggplot2", quietly = TRUE)) {
message("The 'ggplot2' package is recommended for plotting. Please install it.")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
stop("The 'dplyr' package is required for the 'ea_data' constructor. Please install it.")
}



#' S4 Class for Environmental Assessment Data
#'
#' The `ea_data` class is designed to store and manage environmental assessment data,
#' including both metadata and a structured data frame. It supports standardized
#' column naming and includes validation to ensure required fields are present.
#'
#' @slot meta A named list containing metadata such as data type, region, units, species, etc.
#' @slot data A `data.frame` or `tibble` containing the actual data, including at minimum
#'   a `year` column and a `value` column.
#'
#' @export
setClass(
  Class = "ea_data",
  slots = list(
    meta = "list",
    data = "data.frame"
  )
)


#' Create an `ea_data` S4 Object
#'
#' Constructs an `ea_data` object from a data frame and associated metadata.
#' The function validates the presence and type of required columns, standardizes
#' the value column, and stores metadata in a structured format.
#'
#' @param data A `data.frame` containing at least a `year` column and a numeric column
#'   specified by `value_col`.
#' @param value_col A character string naming the column in `data` that contains the numeric values.
#' @param data_type A character string describing the type of data (e.g., "temperature").
#' @param region A character string indicating the geographic region.
#' @param location_descriptor A character string describing the location (e.g., "bottom", "surface").
#' @param units A character string indicating the units of measurement.
#' @param species Optional. A character string naming the species (default is `NA_character_`).
#' @param source_citation Optional. A character string providing the data source citation.
#' @param ... Additional metadata fields to include in the `meta` slot.
#'
#' @return An object of class `ea_data`.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(year = 2000:2005, temp = rnorm(6))
#' obj <- ea_data(df, value_col = "temp", data_type = "temperature",
#'                region = "Scotian Shelf", location_descriptor = "bottom",
#'                units = "°C")
#' }
#'
#' @export
setGeneric("ea_data", function(data, value_col, data_type, region,
                               location_descriptor, units, species = NA_character_,
                               source_citation = "No citation provided", ...) {
  standardGeneric("ea_data")
})
setMethod("ea_data", signature(data = "data.frame", value_col = "character"),
          function(data, value_col, data_type, region,
                   location_descriptor, units, species = NA_character_,
                   source_citation = "No citation provided", ...) {
            
            # --- Validation ---
            if (!value_col %in% names(data)) {
              stop(paste0("Column '", value_col, "' not found in the data."), call. = FALSE)
            }
            if (!"year" %in% names(data)) {
              stop("`data` must contain a 'year' column.", call. = FALSE)
            }
            if (!is.numeric(data[[value_col]]) || !is.numeric(data$year)) {
              stop(paste0("Columns 'year' and '", value_col, "' must be numeric."), call. = FALSE)
            }
          
            
            # --- Standardize ---
            original_value_col <- value_col
            data <- dplyr::rename(data, value = !!original_value_col)
            
            # --- Metadata ---
            meta <- list(
              data_type = as.character(data_type),
              region = as.character(region),
              location_descriptor = as.character(location_descriptor),
              units = as.character(units),
              species = as.character(species),
              source_citation = as.character(source_citation),
              original_value_col = original_value_col,
              ...
            )
            
            # --- Construct S4 object ---
            new("ea_data", data = tibble::as_tibble(data), meta = meta)
          })




#' Access Elements of an `ea_data` Object
#'
#' Provides access to metadata or data columns using `[[` syntax.
#'
#' @param x An `ea_data` object.
#' @param i A character string naming an element in the `meta` or `data` slot.
#' @param ... Additional arguments (ignored).
#'
#' @return The requested element from the `meta` or `data` slot.
#'
#' @export
setMethod(
  f = "[[",
  signature(x = "ea_data", i = "ANY"),
  definition = function(x, i, ...) {
    metadataNames <- sort(names(x@meta))
    dataNames <- sort(names(x@data))
    
    if (i == "meta") {
      return(x@meta)
    } else if (i == "data") {
      return(x@data)
    } else if (i %in% metadataNames) {
      return(x@meta[[i]])
    } else if (i %in% dataNames) {
      return(x@data[[i]])
    } else {
      stop(sprintf("Element '%s' not found in 'meta' or 'data'", i))
    }
  }
)


#' Subset Rows of an `ea_data` Object
#'
#' Subsets the rows of the `data` slot while preserving the `meta` slot.
#' Column subsetting is not supported and will be ignored.
#'
#' @param x An `ea_data` object.
#' @param i Row indices or logical vector.
#' @param j Ignored (column subsetting not supported).
#' @param ... Additional arguments (ignored).
#' @param drop Logical. Ignored to preserve object structure.
#'
#' @return A new `ea_data` object with subsetted rows.
#'
#' @export
setMethod(
  f = "[",
  signature(x = "ea_data", i = "ANY", j = "ANY"),
  definition = function(x, i, j, ..., drop = FALSE) {
    warning("Column subsetting (`j`) is not supported and will be ignored to preserve object structure.")
    subset_data <- x@data[i, , drop = FALSE]
    new("ea_data", data = subset_data, meta = x@meta)
  }
)


#' Subset an `ea_data` Object by Column and Value(s)
#'
#' Filters the `data` slot of an `ea_data` object based on a column and one or more values.
#'
#' @param x An `ea_data` object.
#' @param column A character string naming the column in the `data` slot to filter by.
#' @param value A value or vector of values to match in the specified column.
#'
#' @return A new `ea_data` object containing only the matching rows.
#'
#' @examples
#' \dontrun{
#' ea.subset(azmp_bottom_temperature, "year", 1953)
#' ea.subset(azmp_bottom_temperature, "region", c("Scotian Shelf", "Gulf"))
#' }
#'
#' @export
ea.subset <- function(x, column, value) {
  if (!inherits(x, "ea_data")) stop("x must be an ea_data object.")
  if (!column %in% names(x@data)) stop(paste("Column", column, "not found in data."))
  
  # Subset rows where column matches value(s)
  rows <- x[[column]] %in% value
  
  # Return new ea_data object
  new("ea_data", data = x@data[rows, , drop = FALSE], meta = x@meta)
}




#' @title Validity Method for ea_data
#' @name ea_data-validity
#' @description Ensures that the `data` slot contains both `year` and `value` columns.
#' @param object An `ea_data` object.
#' @return TRUE if valid, otherwise a character string describing the issue.
#' @keywords internal
setValidity("ea_data", function(object) {
  if (!"year" %in% names(object@data)) return("Missing 'year' column in data")
  if (!"value" %in% names(object@data)) return("Missing 'value' column in data")
  TRUE
})
