#' @title Coerce a data frame to an `ea_data` S4 object
#'
#' @description
#' Converts a data frame or compatible object into an `ea_data` S4 object,
#' handling multiple potential value columns robustly and validating the result.
#'
#' @param x A data frame or similar object to convert.
#' @param value_col The name of the column to use as the primary "value" column.
#'   If not specified, the function will attempt to determine the value column automatically.
#' @param ... Additional metadata arguments passed to the `ea_data()` constructor.
#'
#' @return A validated `ea_data` S4 object.
#'
#' @export
as_ea_data <- function(x, value_col = NULL, ...) {

  df <- as.data.frame(x)
  
  # Step 1: Determine value column
  if (is.null(value_col)) {
    candidates <- intersect(c("median", "val", "anom", "anomaly", "mean", "value"), names(df))
    if (length(candidates) > 0){
      value_col <- candidates
      
    } else {
    
      stop("Could not determine value column. Please supply value_col argument.", call. = FALSE)
    }
  }
  
  
  # Step 2: Extract metadata from attributes or user input
  user_args <- list(...)
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  attrs <- attributes(x)
  
  data_type   <- user_args$data_type        %||% attrs$long_name   %||% attrs$axis_name   %||% class(x)[1]
  region      <- user_args$region           %||% attrs$region      %||% "Not specified"
  location    <- user_args$location_descriptor %||% attrs$stock_name  %||% attrs$location  %||% "Not specified"
  units       <- user_args$units            %||% attrs$units       %||% ""
  species     <- user_args$species          %||% attrs$species     %||% NA_character_
  citation    <- user_args$source_citation  %||% attrs$citation    %||% attrs$source  %||% "Not specified"    
  time_desc   <- user_args$time_descriptor  %||% attrs$time_descriptor %||% "Not specified"
  
  # Remove the arguments we've explicitly handled from the ... list
  handled_args <- c("data_type", "region", "location_descriptor", "units", 
                    "species", "source_citation", "time_descriptor")
  remaining_args <- user_args[!names(user_args) %in% handled_args]
  
  # Step 5: Build and validate the S4 object
  obj <- do.call(ea_data, c(
    list(
      data = df,
      value_col = value_col,
      data_type = data_type,
      region = region,
      location_descriptor = location,
      units = units,
      species = species,
      source_citation = citation,
      time_descriptor = time_desc
    ),
    remaining_args
  ))
  
  validObject(obj)
  obj
}
