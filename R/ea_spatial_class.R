# Preamble: Ensure required packages are available
# This class depends on 'sf' for its core structure and 'ggplot2' for plotting.
if (!requireNamespace("sf", quietly = TRUE)) {
  stop("The 'sf' package is required for the 'ea_spatial' class. Please install it.")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("The 'ggplot2' package is recommended for plotting. Please install it.")
}

#' S4 Class for Spatio-Temporal Environmental Assessment Data
#'
#' The `ea_spatial` class stores spatio-temporal environmental assessment data
#' using spatial formats from the `sf`, `stars`, or `terra` packages. It includes
#' metadata and a standardized `value` column for analysis and visualization.
#'
#' @slot meta A named list containing metadata such as data type, region, units, etc.
#' @slot data A spatial object of class `sf`, `stars`, or `SpatRaster`.
#'
#' @export
setClass(
  Class = "ea_spatial",
  slots = list(
    meta = "list",
    data = "ANY"  # Accepts sf, stars, or SpatRaster
  )
)


#' Create an `ea_spatial` S4 Object
#'
#' Constructs an `ea_spatial` object from a spatial object (`sf`, `stars`, or `SpatRaster`)
#' and associated metadata. The function validates the presence and type of required
#' columns or layers, standardizes the value column (if applicable), and stores metadata.
#'
#' @param data A spatial object of class `sf`, `stars`, or `SpatRaster`.
#' @param value_col A character string naming the column or layer containing numeric values.
#' @param data_type A character string describing the type of data.
#' @param region A character string indicating the geographic region.
#' @param time_descriptor A character string describing the temporal nature of the data.
#' @param units A character string indicating the units of measurement.
#' @param source_citation A character string providing the data source citation.
#' @param ... Additional metadata fields to include in the `meta` slot.
#'
#' @return An object of class `ea_spatial`.
#'
#' @export
setGeneric("ea_spatial", function(data, value_col, data_type, region,
                                  time_descriptor, units,
                                  source_citation = "No citation provided", ...) {
  standardGeneric("ea_spatial")
})

setMethod("ea_spatial", signature(data = "ANY", value_col = "character"),
          function(data, value_col, data_type, region,
                   time_descriptor, units,
                   source_citation = "No citation provided", ...) {
            
            # --- Validation and Standardization ---
            if (inherits(data, "sf")) {
              if (!value_col %in% names(data)) {
                stop(paste0("Column '", value_col, "' not found in the sf object."), call. = FALSE)
              }
              if (!is.numeric(data[[value_col]])) {
                stop(paste0("Column '", value_col, "' must be numeric."), call. = FALSE)
              }
              names(data)[names(data) == value_col] <- "value"
              
            } else if (inherits(data, "stars")) {
              if (!value_col %in% names(data)) {
                stop(paste0("Layer '", value_col, "' not found in the stars object."), call. = FALSE)
              }
              names(data)[names(data) == value_col] <- "value"
              
            } else if (inherits(data, "SpatRaster")) {
              if (!value_col %in% names(data)) {
                stop(paste0("Layer '", value_col, "' not found in the SpatRaster object."), call. = FALSE)
              }
              names(data)[names(data) == value_col] <- "value"
              
            } else {
              stop("`data` must be of class `sf`, `stars`, or `SpatRaster`.", call. = FALSE)
            }
            
            # --- Metadata ---
            meta <- list(
              data_type = as.character(data_type),
              region = as.character(region),
              time_descriptor = as.character(time_descriptor),
              units = as.character(units),
              source_citation = as.character(source_citation),
              original_value_col = value_col,
              ...
            )
            
            # --- Construct S4 object ---
            new("ea_spatial", data = data, meta = meta)
          })

#' @title Validity Method for ea_spatial
#' @name ea_spatial-validity
#' @description Ensures that the `data` slot is a supported spatial object and contains a `value` layer or column.
#' @param object An `ea_spatial` object.
#' @return TRUE if valid, otherwise a character string describing the issue.
#' @keywords internal
setValidity("ea_spatial", function(object) {
  data <- object@data
  if (!inherits(data, c("sf", "stars", "SpatRaster"))) {
    return("The 'data' slot must be an object of class `sf`, `stars`, or `SpatRaster`.")
  }
  if (!"value" %in% names(data)) {
    return("The 'data' object must contain a 'value' column or layer.")
  }
  TRUE
})



#' Access Elements of an `ea_spatial` Object
#'
#' Provides access to metadata or data layers/columns using `[[` syntax.
#'
#' @param x An `ea_spatial` object.
#' @param i A character string naming an element in the `meta` or `data` slot.
#' @param ... Additional arguments (ignored).
#'
#' @return The requested element from the `meta` or `data` slot.
#'
#' @export
setMethod(
  f = "[[",
  signature(x = "ea_spatial", i = "ANY"),
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
      if (inherits(x@data, "sf") || inherits(x@data, "stars")) {
        return(x@data[[i]])
      } else if (inherits(x@data, "SpatRaster")) {
        return(terra::values(x@data[[i]]))
      }
    } else {
      stop(sprintf("Element '%s' not found in 'meta' or 'data'", i))
    }
  }
)

#' Subset Rows of an `ea_spatial` Object
#'
#' Subsets the rows of the `data` slot while preserving the `meta` slot.
#' Column subsetting is not supported and will be ignored.
#'
#' @param x An `ea_spatial` object.
#' @param i Row indices or logical vector.
#' @param j Ignored (column subsetting not supported).
#' @param ... Additional arguments (ignored).
#' @param drop Logical. Ignored to preserve object structure.
#'
#' @return A new `ea_spatial` object with subsetted rows.
#'
#' @export
setMethod(
  f = "[",
  signature(x = "ea_spatial", i = "ANY", j = "ANY"),
  definition = function(x, i, j, ..., drop = FALSE) {
    warning("Column subsetting (`j`) is not supported and will be ignored to preserve object structure.")
    
    if (inherits(x@data, "sf") || inherits(x@data, "stars")) {
      subset_data <- x@data[i, , drop = FALSE]
    } else if (inherits(x@data, "SpatRaster")) {
      subset_data <- x@data[i]
    } else {
      stop("Unsupported spatial data type for subsetting.")
    }
    
    new("ea_spatial", data = subset_data, meta = x@meta)
  }
)
#' Subset an `ea_spatial` Object by Column or Layer and Value(s)
#'
#' Filters the `data` slot of an `ea_spatial` object based on a column or layer
#' and one or more values.
#'
#' @param x An `ea_spatial` object.
#' @param column A character string naming the column or layer in the `data` slot to filter by.
#' @param value A value or vector of values to match in the specified column/layer.
#'
#' @return A new `ea_spatial` object containing only the matching rows or cells.
#'
#' @examples
#' \dontrun{
#' ea.subset(temp_obj, "region", "Maritimes")
#' ea.subset(temp_obj, "value", c(5.5, 5.6))
#' }
#'
#' @export
ea.subset <- function(x, column, value) {
  if (!inherits(x, "ea_spatial")) stop("x must be an ea_spatial object.")
  if (!column %in% names(x@data)) stop(paste("Column or layer", column, "not found in data."))
  
  if (inherits(x@data, "sf") || inherits(x@data, "stars")) {
    rows <- x[[column]] %in% value
    subset_data <- x@data[rows, , drop = FALSE]
  } else if (inherits(x@data, "SpatRaster")) {
    layer <- x@data[[column]]
    mask <- layer %in% value
    subset_data <- terra::mask(x@data, mask)
  } else {
    stop("Unsupported spatial data type for subsetting.")
  }
  
  new("ea_spatial", data = subset_data, meta = x@meta)
}
