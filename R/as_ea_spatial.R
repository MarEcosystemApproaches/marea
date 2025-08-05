#' @title Coerce a spatial object to an `ea_spatial` S4 object
#'
#' @description
#' Converts a spatial object (`sf`, `stars`, or `SpatRaster`) into a validated `ea_spatial` S4 object.
#' Metadata is extracted from attributes and can be overridden via arguments.
#'
#' @param x A spatial object of class `sf`, `stars`, or `SpatRaster`.
#' @param value_col Name of the column or layer to use as the primary value. If NULL, auto-selects if only one candidate exists.
#' @param ... Additional metadata overrides passed to the `ea_spatial()` constructor.
#'
#' @return A validated `ea_spatial` S4 object.
#'
#' @export
as_ea_spatial <- function(x, value_col = NULL, ...) {
  # if (methods::is(x, "ea_spatial")) return(x)
  
  if (!inherits(x, c("sf", "stars", "SpatRaster"))) {
    stop("Object must be of class 'sf', 'stars', or 'SpatRaster'.", call. = FALSE)
  }
  
  # Determine candidate columns/layers
  if (inherits(x, "sf")) {
    geom_col <- attr(x, "sf_column")
    candidate_cols <- setdiff(names(x), geom_col)
  } else {
    candidate_cols <- names(x)
  }
  
  # Auto-select value_col if not provided
  if (is.null(value_col)) {
    if (length(candidate_cols) == 1) {
      value_col <- candidate_cols[1]
    } else {
      stop("Multiple candidate columns/layers found. Please specify `value_col`.", call. = FALSE)
    }
  }
  
  if (!value_col %in% candidate_cols) {
    stop(paste("Column or layer", value_col, "not found in object."), call. = FALSE)
  }
  
  # Extract metadata from attributes and user input
  user_args <- list(...)
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  attrs <- attributes(x)
  
  data_type   <- user_args$data_type        %||% attrs$long_name   %||% attrs$axis_name   %||% class(x)[1]
  region      <- user_args$region           %||% attrs$region      %||% "Not specified"
  time_desc   <- user_args$time_descriptor  %||% attrs$time_descriptor %||% value_col
  units       <- user_args$units            %||% attrs$units       %||% ""
  citation    <- user_args$source_citation  %||% attrs$citation    %||% attrs$source %||% "pacea object"
  
  # Remove standard arguments from user_args to avoid duplication
  standard_args <- c("data_type", "region", "time_descriptor", "units", "source_citation")
  additional_meta <- user_args[!names(user_args) %in% standard_args]
  
  # Build constructor arguments
  call_args <- list(
    data = x,
    value_col = value_col,
    data_type = data_type,
    region = region,
    time_descriptor = time_desc,
    units = units,
    source_citation = citation
  )
  
  # Add additional metadata
  call_args <- c(call_args, additional_meta)
  
  # Construct and validate the S4 object
  obj <- do.call(ea_spatial, call_args)
  validObject(obj)
  obj
}
