#' @title Coerce a pacea time series object to an ea_data object
#' @description
#' Convert a `pacea_index`, `pacea_biomass`, `pacea_recruitment`, or similar
#' object (as in the pacea package) to a standardized `ea_data` object.
#' All metadata is extracted from attributes, not from a meta slot.
#'
#' @param pacea_obj An object of class `pacea_index`, `pacea_biomass`, `pacea_recruitment`, or similar (data.frame, tibble).
#' @param ...   Additional metadata overrides (named elements).
#' @return An `ea_data` object.
#' @export
as_ea_data <- function(pacea_obj, ...) {
  if (inherits(pacea_obj, "ea_data")) return(pacea_obj)
  df <- as.data.frame(pacea_obj)
  # Guess best value column
  value_col <- intersect(c("median", "val", "anom", "mean", "value"), names(df))[1]
  if (is.na(value_col))
    stop("No recognized value column (median, val, anom, mean, value)", call. = FALSE)
  names(df)[names(df) == value_col] <- "value"
  # Optionally copy over low/high if present
  # (no rename needed; used if present)
  # Metadata: always from attributes!
  attrs <- attributes(pacea_obj)
  data_type   <- attrs$long_name   %||% attrs$axis_name   %||% class(pacea_obj)[1]
  region      <- attrs$region      %||% "Not specified"
  location    <- attrs$stock_name  %||% data_type
  units       <- attrs$units       %||% ""
  species     <- attrs$species     %||% NA_character_
  citation    <- attrs$citation    %||% attrs$source %||% "pacea object (see ?object)"
  # Allow user override
  user_meta <- list(...)
  marea::ea_data(
    data               = df,
    value_col          = "value",
    data_type          = data_type,
    region             = region,
    location_descriptor= location,
    units              = units,
    species            = species,
    source_citation    = citation,
    user_meta
  )
}
# local definition for null-coalesce
`%||%` <- function(x, y) if (is.null(x)) y else x