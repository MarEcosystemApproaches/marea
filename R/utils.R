#' Helper for NULL values
#'
#' Returns the first argument if it is not NULL, otherwise returns the second.
#'
#' @param x Value to check.
#' @param y Value to return if x is NULL.
#' @return x if not NULL, otherwise y.
#' @keywords internal
#' @name null-coalesce
#' @aliases %||% grapes-or-or-grapes
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x


# Helper function to filter while preserving attributes
filter_preserve_attrs <- function(data, ...) {
  orig_attrs <- attributes(data)
  filtered <- dplyr::filter(data, ...)
  
  # Restore non-standard attributes (keep names, row.names, class from filtered)
  attrs_to_restore <- setdiff(names(orig_attrs), c("names", "row.names", "class"))
  for(attr_name in attrs_to_restore) {
    attr(filtered, attr_name) <- orig_attrs[[attr_name]]
  }
  
  return(filtered)
}