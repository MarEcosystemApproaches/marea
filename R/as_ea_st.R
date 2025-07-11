#' @title Coerce an sf-like pacea object to an ea_st object
#' @description
#' Convert a spatial/class `pacea_st` or `marea_st` (simple features) object from pacea to generic `ea_st`.
#' All metadata is extracted from attributes, not a meta slot.
#'
#' @param spatial_obj An `sf` or similar object, usually with class `pacea_st` 
#' @param value_col   Name of the column to use as value. If NULL, will auto-pick if only one candidate exists.
#' @param ...         Additional metadata overrides (named elements).
#' @return An `ea_st` object.
#' @export
as_ea_st <- function(spatial_obj, value_col = NULL, ...) {
  if (inherits(spatial_obj, "ea_st")) return(spatial_obj)
  if (!inherits(spatial_obj, "sf")) stop("Object must be of class 'sf' or similar.", call. = FALSE)
  geom_col <- attr(spatial_obj, "sf_column")
  candidate_cols <- setdiff(names(spatial_obj), geom_col)
  if (is.null(value_col)) {
    poss <- candidate_cols
    if (length(poss) != 1)
      stop("Supply value_col (multiple possible data columns in sf object).", call. = FALSE)
    value_col <- poss
  }
  stopifnot(value_col %in% names(spatial_obj))
  # Metadata from attributes
  attrs <- attributes(spatial_obj)
  data_type   <- attrs$long_name   %||% attrs$axis_name   %||% class(spatial_obj)[1]
  region      <- attrs$region      %||% "Not specified"
  time_desc   <- value_col
  units       <- attrs$units       %||% ""
  citation    <- attrs$citation    %||% attrs$source %||% "pacea object (see ?object)"
  user_meta   <- list(...)
  marea::ea_st(
    data            = dplyr::rename(spatial_obj, value = !!value_col),
    value_col       = "value",
    data_type       = data_type,
    region          = region,
    time_descriptor = time_desc,
    units           = units,
    source_citation = citation,
    user_meta
  )
}