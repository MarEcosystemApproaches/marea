

#' An S4 Class for Environmental Assessment Data
#'
#' @description
#' The `ea_data` class is a container for environmental assessment time series
#' data. It encapsulates a data frame with the core data and a list of
#' metadata, ensuring that essential columns (`year`, `value`) are present and
#' providing a standardized interface for accessing and manipulating the data.
#'
#' @slot meta A named list containing metadata. Expected fields include
#'   `data_type`, `region`, `location_descriptor`, `units`, `species`,
#'   `source_citation`, and `original_value_col`.
#' @slot data A `data.frame` or `tibble` containing the time series data. Must
#'   include at least a `year` column and a `value` column.
#'
#' @seealso \code{\link{ea_data}} for the constructor function.
#' @seealso \code{\link{ea.subset}} for a filtering helper function.
#'
#' @name ea_data-class
#' @rdname ea_data-class
#' @export
setClass(
  Class = "ea_data",
  slots = list(
    meta = "list",
    data = "data.frame"
  )
)


#' Constructor for `ea_data` Objects
#'
#' @description
#' Creates an `ea_data` object from a data frame and associated metadata.
#' This function serves as the primary constructor for the \code{\link{ea_data-class}}.
#' It validates the presence and type of required columns, standardizes the
#' value column name to `value`, and stores metadata in the `meta` slot.
#'
#' @param data A `data.frame` containing at least a `year` column and a numeric
#'   column specified by `value_col`.
#' @param value_col A character string naming the column in `data` that
#'   contains the numeric values to be standardized as `value`.
#' @param data_type A character string describing the type of data (e.g., "temperature").
#' @param region A character string indicating the geographic region.
#' @param location_descriptor A character string describing the location
#'   (e.g., "bottom", "surface").
#' @param units A character string indicating the units of measurement.
#' @param species `[character(1)]` Optional. A character string naming the species
#'   (default is `NA_character_`).
#' @param source_citation `[character(1)]` Optional. A character string providing
#'   the data source citation.
#' @param ... Additional metadata fields to be stored in the `meta` slot as a
#'   named list.
#'
#' @return An object of class `ea_data`.
#'
#' @examples
#' df <- data.frame(year = 2000:2005, temp_c = rnorm(6))
#' obj <- ea_data(df,
#'                value_col = "temp_c",
#'                data_type = "temperature",
#'                region = "Scotian Shelf",
#'                location_descriptor = "bottom",
#'                units = "°C",
#'                project = "AZMP")
#'
#' # Access metadata
#' obj[["region"]]
#'
#' # Access data
#' obj[["data"]]
#'
#' @export
setGeneric("ea_data", function(data, value_col, data_type, region,
                               location_descriptor, units, species = NA_character_,
                               source_citation = "No citation provided", ...) {
  standardGeneric("ea_data")
})

#' @rdname ea_data
#' @export
setMethod("ea_data", signature(data = "data.frame", value_col = "list"),
          function(data, value_col, data_type, region,
                   location_descriptor, units, species = NA_character_,
                   source_citation = "No citation provided", ...) {
            
            # --- Validation ---
            for (i in 1:length(value_col)) {
              val <- value_col[[i]]
                 if (!val %in% names(data)) {
                  stop(paste0("Column '", val, "' not found in the data."), call. = FALSE)
              }
            }
            if (!"year" %in% names(data)) {
              stop("`data` must contain a 'year' column.", call. = FALSE)
            }
            if (!is.numeric(data[[value_col]]) || !is.numeric(data$year)) {
              stop(paste0("Columns 'year' and '", value_col, "' must be numeric."), call. = FALSE)
            }
            
            
            # --- Standardize ---
            # rename all columns from value_col (list) to originalname_value
            for (i in 1:length(value_col)) {
              val <- value_col[[i]]
              new_name <- paste0(val, "_value")
              data <- dplyr::rename(data, !!new_name := .data[[val]])
            }
            
            # --- Metadata ---
            meta <- list(
              data_type = as.character(data_type),
              region = as.character(region),
              location_descriptor = as.character(location_descriptor),
              units = as.character(units),
              species = as.character(species),
              source_citation = as.character(source_citation),
              original_value_col = value_col, # TODO check that this prints okay as a list
              ...
            )
            
            # --- Construct S4 object ---
            new("ea_data", data = tibble::as_tibble(data), meta = meta)
          })


#' Extract or Access Parts of an ea_data Object
#'
#' @description
#' Provides access to metadata or data columns using the `[[` operator. This
#' allows for intuitive extraction of both metadata fields (e.g., `obj[["region"]]`)
#' and data vectors (e.g., `obj[["year"]]`).
#'
#' @param x An `ea_data` object.
#' @param i A character string specifying the element to extract. This can be:
#'   \itemize{
#'     \item `"meta"` to retrieve the entire metadata list.
#'     \item `"data"` to retrieve the entire data frame.
#'     \item The name of a specific metadata field (e.g., `"units"`).
#'     \item The name of a column in the data frame (e.g., `"value"`).
#'   }
#'
#' @return The requested element.
#'
#' @aliases [[,ea_data-method
#' @export
#' @rdname S4-accessors
#'
#' @examples
#' df <- data.frame(year = 2000:2005, temp_c = rnorm(6))
#' obj <- ea_data(df, value_col = "temp_c", data_type = "temperature",
#'                region = "Scotian Shelf", location_descriptor = "bottom",
#'                units = "°C")
#'
#' # Get the units
#' obj[["units"]]
#'
#' # Get the year vector
#' obj[["year"]]
#'
setMethod(
  f = "[[",
  signature(x = "ea_data", i = "ANY"),
  definition = function(x, i) {
    if (i == "meta") {
      return(x@meta)
    } else if (i == "data") {
      return(x@data)
    } else if (i %in% names(x@meta)) {
      return(x@meta[[i]])
    } else if (i %in% names(x@data)) {
      return(x@data[[i]])
    } else {
      stop(sprintf("Element '%s' not found in 'meta' or 'data' slots", i), call. = FALSE)
    }
  }
)


#' Subset an ea_data Object by Row
#'
#' @description
#' Subsets the rows of the `data` slot using the `[` operator, while preserving
#' the object's structure and metadata.
#'
#' @details
#' Column subsetting (via `j`) is not supported and will be ignored with a
#' warning, as altering the column structure could invalidate the object
#' (e.g., by removing the `year` or `value` columns).
#'
#' @param x An `ea_data` object.
#' @param i Row indices (numeric, logical, or character) to subset the data.
#' @param j Ignored. A warning is issued if `j` is specified.
#' @param drop Ignored. The method always returns an `ea_data` object.
#'
#' @return A new `ea_data` object with subsetted rows in its `data` slot.
#'
#' @aliases [,ea_data-method
#' @export
#' @rdname S4-accessors
#'
#' @examples
#' df <- data.frame(year = 2000:2005, temp_c = rnorm(6))
#' obj <- ea_data(df, value_col = "temp_c", data_type = "temperature",
#'                region = "Scotian Shelf", location_descriptor = "bottom",
#'                units = "°C")
#'
#' # Subset the first three rows
#' obj[1:3, ]
setMethod(
  f = "[",
  signature(x = "ea_data", i = "ANY", j = "missing", drop = "missing"),
  definition = function(x, i, j, ..., drop = FALSE) {
    # Check if j was passed despite the signature
    if (!missing(j)) {
      warning("Column subsetting (`j`) is ignored to preserve object structure.", call. = FALSE)
    }
    subset_data <- x@data[i, , drop = FALSE]
    new("ea_data", data = subset_data, meta = x@meta)
  }
)

#' @param ... Additional arguments (ignored).
#' @rdname S4-accessors
#' @export
setMethod(
  f = "[",
  signature(x = "ea_data", i = "ANY", j = "ANY", drop = "ANY"),
  definition = function(x, i, j, ..., drop = FALSE) {
    warning("Column subsetting (`j`) is ignored to preserve object structure.", call. = FALSE)
    callNextMethod(x, i) # Pass to the [i, missing] method
  }
)


#' Filter an `ea_data` Object by Column and Value
#'
#' @description
#' A convenient helper function to filter the `data` slot of an `ea_data` object
#' based on matching values in a specified column.
#'
#' @param x An `ea_data` object.
#' @param column A character string naming the column in the `data` slot to filter by.
#' @param value A value or vector of values to match in the specified column.
#'
#' @return A new `ea_data` object containing only the matching rows.
#'
#' @examples
#' df <- data.frame(year = rep(2000:2002, 2),
#'                  temp_c = rnorm(6),
#'                  group = rep(c("A", "B"), each = 3))
#' obj <- ea_data(df, value_col = "temp_c", data_type = "temperature",
#'                region = "Test Region", location_descriptor = "surface",
#'                units = "°C")
#'
#' # Filter for a single year
#' ea.subset(obj, "year", 2001)
#'
#' # Filter for specific groups
#' ea.subset(obj, "group", "B")
#'
#' @export
ea.subset <- function(x, column, value) {
  if (!inherits(x, "ea_data")) stop("x must be an ea_data object.", call. = FALSE)
  if (!column %in% names(x@data)) stop(paste("Column", column, "not found in data."), call. = FALSE)
  
  # Subset rows where column matches value(s)
  rows <- x@data[[column]] %in% value
  
  # Return new ea_data object
  new("ea_data", data = x@data[rows, , drop = FALSE], meta = x@meta)
}


#' Timeseries Data Validity
#' 
#' @description
#' The validity method for the `ea_data` class ensures that the `data` slot
#' contains the required `year` and `value` columns. This check is performed
#' automatically upon object creation with `new()`.
#' @param object An `ea_data` object.
#' @return `TRUE` if the object is valid, otherwise a character string
#'   describing the validation failure.
#' @keywords internal
#' @name ea_data-validity
setValidity("ea_data", function(object) {
  errors <- character()
  if (!"year" %in% names(object@data)) {
    errors <- c(errors, "Missing 'year' column in the data slot.")
  }
  # check for at least one column that ends with _value
  value_cols <- grep("_value$", names(object@data), value = TRUE)
  if (length(value_cols) == 0) {
    errors <- c(errors, "Missing 'value' column in the data slot.")
  }
  
  if (length(errors) == 0) TRUE else errors
})