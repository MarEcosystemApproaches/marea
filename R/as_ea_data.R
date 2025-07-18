#' @title Coerce a pacea object to ea_data (robust multiple-value handling)
#' @export
as_ea_data <- function(pacea_obj, value_col = NULL, ...) {
  if (inherits(pacea_obj, "ea_data")) return(pacea_obj)
  df <- as.data.frame(pacea_obj)
  
  # Step 1: figure out which column to use as primary "value"
  if (is.null(value_col)) {
    candidates <- intersect(c("median", "val", "anom", "anomaly", "mean", "value"), names(df))
    if ("value" %in% candidates) {
      value_col <- "value"
    } else if (length(candidates) == 1) {
      value_col <- candidates[1]
    } else if (length(candidates) > 1) {
      warning(
        "Multiple candidate value columns found: ", paste(candidates, collapse=", "),
        "; using first: ", candidates[1], ". You can override with value_col=."
      )
      value_col <- candidates[1]
    } else {
      stop("Could not determine value column. Please supply value_col argument.", call. = FALSE)
    }
  }
  
  # Step 2: If value_col isn't 'value', but 'value' already exists, rename it to 'value_orig'
  if (value_col != "value" && "value" %in% names(df)) {
    names(df)[names(df) == "value"] <- "value_orig"
  }
  
  # Step 3: Only rename if value_col != 'value'
  if (value_col != "value") {
    names(df)[names(df) == value_col] <- "value"
  }
  
  original_value_col <- value_col
  
  # Extract user-supplied arguments
  user_args <- list(...)
  # -- Metadata 
  attrs <- attributes(pacea_obj)
  `%||%` <- function(x, y) if (is.null(x)) y else x
  data_type   <- user_args$data_type %||% attrs$long_name   %||% attrs$axis_name   %||% class(pacea_obj)[1]
  region      <- user_args$region %||% attrs$region      %||% "Not specified"
  location    <- user_args$location %||% attrs$stock_name  %||% data_type
  units       <- user_args$units %||% attrs$units       %||% ""
  species     <- user_args$species %||% attrs$species     %||% NA_character_
  citation    <- user_args$citation %||% attrs$citation    %||% attrs$source %||% "pacea object"
  time_desc  <- user_args$time_descriptor %||% attrs$time_descriptor %||% "Not specified"
   user_meta   <- list(...)
  
  # Remove standard arguments from user_args to avoid conflicts
  standard_args <- c("data_type", "region", "time_descriptor", "units", "source_citation")
  additional_meta <- user_args[!names(user_args) %in% standard_args]
  
  # Build the call arguments
  call_args <- list(
    data = df,
    value_col = "value",
    data_type = data_type,
    region = region,
    time_descriptor = time_desc,
    units = units,
    source_citation = citation,
    original_value_col = original_value_col
  )
  
  # Add any additional metadata
  call_args <- c(call_args, additional_meta)
  
  # Call ea_st with the constructed arguments
  do.call(marea::ea_data, call_args)
}