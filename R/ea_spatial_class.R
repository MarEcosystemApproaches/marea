
#' An S4 Class for Spatio-Temporal Environmental Assessment Data
#'
#' @description
#' The `ea_spatial` class is a container for environmental assessment data that
#' has a spatial component. It is designed to be flexible, supporting vector
#' data via `sf` objects and raster data via `stars` or `terra::SpatRaster`
#' objects.
#'
#' It standardizes the data by encapsulating a spatial object in the `data` slot
#' and a list of descriptive information in the `meta` slot.
#'
#' @slot meta A named list containing metadata. Expected fields include
#'   `data_type`, `region`, `time_descriptor`, `units`, `source_citation`,
#'   and `original_value_col`.
#' @slot data A spatial object. Must be of class `sf`, `stars`, or `SpatRaster`.
#'   The object is expected to have a column or layer named `value` containing
#'   the primary data.
#'
#' @seealso \code{\link{ea_spatial}} for the constructor function.
#' @seealso \code{\link{ea.subset.spatial}} for a filtering helper function.
#'
#' @name ea_spatial-class
#' @rdname ea_spatial-class
#' @export
setClass(
  Class = "ea_spatial",
  slots = list(
    meta = "list",
    data = "ANY"  # Constrained by the validity check
  )
)

#' Constructor for `ea_spatial` Objects
#'
#' @description
#' Creates an `ea_spatial` object from a spatial data object (`sf`, `stars`, or
#' `SpatRaster`) and associated metadata.
#'
#' This function validates the input data, standardizes the primary data column
#' or layer to be named `value`, and populates the `meta` slot. It is the
#' primary constructor for the \code{\link{ea_spatial-class}}.
#'
#' @param data A spatial object of class `sf`, `stars`, or `SpatRaster`.
#' @param value_col `[character(1)]` The name of the column (for `sf`) or
#'   layer/attribute (for `stars` or `SpatRaster`) that contains the primary
#'   numeric values. This column/layer will be renamed to `"value"`.
#' @param data_type `[character(1)]` A description of the data
#'   (e.g., "sea surface temperature", "Chlorophyll-a concentration").
#' @param region `[character(1)]` The geographic region the data represents.
#' @param time_descriptor `[character(1)]` A description of the temporal aspect
#'   of the data (e.g., "July 2021 climatology", "Annual mean 2020").
#' @param units `[character(1)]` The units of the `value` column/layer.
#' @param source_citation `[character(1)]` Optional. A citation for the data source.
#' @param ... Additional metadata fields to be stored in the `meta` slot.
#'
#' @return An object of class `ea_spatial`.
#'
#' @examples
#' # Example with an 'sf' object (requires sf package)
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   # Create a sample sf object
#'   pts <- list(
#'     sf::st_point(c(-63.5, 44.6)),
#'     sf::st_point(c(-66.1, 44.9)),
#'     sf::st_point(c(-64.0, 45.1))
#'   )
#'   sf_df <- sf::st_sf(
#'     year = c(2001, 2001, 2002),
#'     temp = c(12.1, 12.3, 13.0),
#'     geometry = sf::st_sfc(pts, crs = 4326)
#'   )
#'
#'   obj_sf <- ea_spatial(
#'     data = sf_df,
#'     value_col = "temp",
#'     data_type = "Temperature",
#'     region = "Scotian Shelf",
#'     time_descriptor = "Point samples",
#'     units = "degC"
#'   )
#'   print(obj_sf)
#' }
#'
#' @export
setGeneric("ea_spatial", function(data, value_col, data_type, region,
                                  time_descriptor, units,
                                  source_citation = "No citation provided", ...) {
  standardGeneric("ea_spatial")
})

#' @rdname ea_spatial
#' @export
setMethod("ea_spatial", signature(data = "ANY", value_col = "character"),
          function(data, value_col, data_type, region,
                   time_descriptor, units,
                   source_citation = "No citation provided", ...) {
            
            # --- Validation and Standardization ---
            if (!inherits(data, c("sf", "stars", "SpatRaster"))) {
              stop("`data` must be of class `sf`, `stars`, or `SpatRaster`.", call. = FALSE)
            }
            
            if (!value_col %in% names(data)) {
              stop(sprintf("Column/layer '%s' not found in the data object.", value_col), call. = FALSE)
            }
            
            # Check for numeric only for sf, as stars/SpatRaster handle types differently
            if (inherits(data, "sf") && !is.numeric(data[[value_col]])) {
              stop(paste0("Column '", value_col, "' must be numeric for sf objects."), call. = FALSE)
            }
            
            names(data)[names(data) == value_col] <- "value"
            
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


#' Extract or Access Parts of an ea_spatial Object
#'
#' @description
#' Provides access to metadata and data using the `[[` operator. This allows
#' for intuitive extraction of metadata fields, the full data object, or
#' specific columns/layers within the data object.
#'
#' @param x An `ea_spatial` object.
#' @param i A character string specifying the element to extract. This can be:
#'   \itemize{
#'     \item `"meta"` to retrieve the entire metadata list.
#'     \item `"data"` to retrieve the entire spatial object.
#'     \item The name of a specific metadata field (e.g., `"units"`).
#'     \item The name of a column or layer in the data object (e.g., `"value"`).
#'   }
#' @param ... Additional arguments (ignored).
#'
#' @return The requested element. For `SpatRaster` layers, this returns the
#'   raster values as a vector via `terra::values()`.
#'
#' @aliases [[,ea_spatial-method
#' @export
#' @rdname S4-spatial-accessors
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   # Create a sample sf object
#'   pts <- list(sf::st_point(c(-63, 44)), sf::st_point(c(-66, 45)))
#'   sf_df <- sf::st_sf(temp = c(12, 13), geometry = sf::st_sfc(pts, crs = 4326))
#'   obj <- ea_spatial(sf_df, "temp", "temp", "region", "time", "C")
#'
#'   # Get a metadata field
#'   obj[["units"]]
#'
#'   # Get the 'value' vector from the data
#'   obj[["value"]]
#' }
setMethod(
  f = "[[",
  signature(x = "ea_spatial", i = "ANY"),
  definition = function(x, i, ...) {
    if (i == "meta") {
      return(x@meta)
    } else if (i == "data") {
      return(x@data)
    } else if (i %in% names(x@meta)) {
      return(x@meta[[i]])
    } else if (i %in% names(x@data)) {
      # Handle different object types
      if (inherits(x@data, "SpatRaster")) {
        value_matrix <- x@data[[i]]
        return(as.vector(value_matrix))
        } else {
        # Works for both sf and stars
        return(x@data[[i]])
      }
    } else {
      stop(sprintf("Element '%s' not found in 'meta' or 'data'", i), call. = FALSE)
    }
  }
)

#' Subset an ea_spatial Object
#'
#' @description
#' Subsets the `data` slot of an `ea_spatial` object using the `[` operator,
#' while preserving the object's structure and metadata.
#'
#' @details
#' This method is primarily intended for spatial or feature-based subsetting
#' (i.e., by row index or by another spatial object). Column subsetting (via `j`)
#' is not supported for `sf` or `stars` objects to prevent accidental removal of
#' the required `value` or geometry columns.
#'
#' @param x An `ea_spatial` object.
#' @param i Row/feature indices (numeric, logical), or a spatial object (`sf`,
#'   `sfc`) to use for spatial subsetting.
#' @param j Ignored. A warning is issued if `j` is specified for `sf`/`stars`.
#' @param ... Additional arguments passed to the underlying subset method.
#' @param drop Logical. Passed to the underlying subset method.
#'
#' @return A new `ea_spatial` object with a subsetted `data` slot.
#'
#' @aliases [,ea_spatial-method
#' @export
#' @rdname S4-spatial-accessors
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   pts <- list(sf::st_point(c(-63, 44)), sf::st_point(c(-66, 45)))
#'   sf_df <- sf::st_sf(temp = c(12, 13), geometry = sf::st_sfc(pts, crs = 4326))
#'   obj <- ea_spatial(sf_df, "temp", "temp", "region", "time", "C")
#'
#'   # Subset the first feature
#'   obj[1, ]
#' }
setMethod(
  f = "[",
  signature(x = "ea_spatial", i = "ANY", j = "ANY"),
  definition = function(x, i, j, ..., drop = FALSE) {
    
    # Handle different object types
    if (inherits(x@data, c("sf", "stars"))) {
      if (!missing(j)) {
        warning("Column subsetting (`j`) is ignored to preserve object structure.", call. = FALSE)
      }
      # Use the [i, ] syntax for sf and stars
      subset_data <- x@data[i, , drop = drop, ...]
    } else if (inherits(x@data, "SpatRaster")) {
      # Capture the layer arguments passed via ...
      dots <- list(...)
      
      # The base case is subsetting all rows and columns
      if (missing(i)) i <- TRUE
      if (missing(j)) j <- TRUE
      
      # If layers were passed, use them. Otherwise, use all layers.
      if (length(dots) > 0) {
        # Assumes the first element in ... is the layer index
        subset_data <- x@data[i, j, dots[[1]], drop = drop]
      } else {
        # No layers specified, subset rows/cols from all layers
        subset_data <- x@data[i, j, , drop = drop]
      }
    } else {
      stop("Unsupported spatial data type for '[' subsetting.", call. = FALSE)
    }
    
    new("ea_spatial", data = subset_data, meta = x@meta)
  }
)

#' Spatial Data Validity
#' 
#' @description
#' The validity method for the `ea_spatial` class ensures that:
#' 1. The `data` slot contains a supported spatial object (`sf`, `stars`, `SpatRaster`).
#' 2. The spatial object contains a `value` column or layer.
#' @param object An `ea_spatial` object.
#' @return `TRUE` if valid, otherwise a character vector of error messages.
#' @keywords internal
#' @name ea_spatial-validity
setValidity("ea_spatial", function(object) {
  errors <- character()
  data <- object@data
  
  if (!inherits(data, c("sf", "stars", "SpatRaster"))) {
    msg <- "The 'data' slot must be an object of class `sf`, `stars`, or `SpatRaster`."
    errors <- c(errors, msg)
  }
  
  if (!"value" %in% names(data)) {
    msg <- "The 'data' object must contain a 'value' column or layer."
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) TRUE else errors
})

#' Filter an `ea_spatial` Object by Attribute
#'
#' @description
#' A helper function to filter the features or cells of an `ea_spatial` object
#' based on the values in a specified attribute column or layer.
#'
#' @details
#' This masks the `data` slot of the `ea_spatial` object, setting the attributes
#' to `NA` for features or cells that do not match the specified value(s).
#' This is useful for subsetting spatial data based on specific criteria,
#' such as filtering by region or time. It is different than the ea.subset method for 
#' ea_data which removes features that do not match the criteria.
#'
#' @param x An `ea_spatial` object.
#' @param column `[character(1)]` The name of the attribute column or layer to filter by.
#' @param value A value or vector of values to match in the specified column/layer.
#'
#' @return A new `ea_spatial` object containing only the matching features or cells.
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE) && requireNamespace("dplyr", quietly = TRUE)) {
#'   pts <- list(sf::st_point(c(-63, 44)), sf::st_point(c(-66, 45)))
#'   sf_df <- sf::st_sf(
#'     temp = c(12, 13),
#'     region_id = c("A", "B"),
#'     geometry = sf::st_sfc(pts, crs = 4326)
#'   )
#'   obj <- ea_spatial(sf_df, "temp", "temp", "region", "time", "C")
#'
#'   # Filter for a specific region_id
#'   ea.subset.spatial(obj, "region_id", "B")
#' }
#'
#' @export
ea.subset.spatial <- function(x, column, value) {
  if (!inherits(x, "ea_spatial")) stop("x must be an ea_spatial object.", call. = FALSE)
  if (!column %in% names(x@data)) stop(paste("Column/layer", column, "not found in data."), call. = FALSE)
  
  data_obj <- x@data
  
  if (inherits(data_obj, "sf")) {
    # For sf, mask attribute data for features not matching the condition.
    # Geometries are preserved, but attributes are set to NA.
    col_data <- sf::st_drop_geometry(data_obj)[[column]]
    # Identify rows where the condition is NOT met.
    rows_to_mask <- !(col_data %in% value)
    
    subset_data <- data_obj
    
    # Get all column names except for the geometry column.
    geom_col_name <- attr(subset_data, "sf_column")
    attr_cols <- setdiff(names(subset_data), geom_col_name)
    
    # Set the attributes of the masked rows to NA.
    # A loop is used to safely handle different column data types.
    for (col in attr_cols) {
      # Coerce to vector to avoid issues with factors
      if (is.factor(subset_data[[col]])) {
        subset_data[[col]] <- as.character(subset_data[[col]])
      }
      subset_data[[col]][rows_to_mask] <- NA
    }
  } else if (inherits(data_obj, "stars")) {
    # For stars, we create a logical mask and apply it.
    # The mask itself is a stars object.
    mask <- data_obj[[column]] %in% value
    # Applying the mask 
    mask_for_na <- !mask
    subset_data <- data_obj
    for (attr_name in names(subset_data)) {
      subset_data[[attr_name]][!mask] <- NA
      } 
    } else if (inherits(data_obj, "SpatRaster")) {
      # This should be verified by a spatial person
      # For SpatRaster, we create a mask and apply it.
      # Create a mask where the values match
      mask <- as.vector(data_obj[[column]]) %in% as.vector(value) # this is not working as expected
      subset_data <- data_obj
      for (attr_name in names(subset_data)) {
        subset_data[[attr_name]][!mask] <- NA
      }
         } else {
    stop("Unsupported spatial data type for subsetting.", call. = FALSE)
  }
  
  new("ea_spatial", data = subset_data, meta = x@meta)
}