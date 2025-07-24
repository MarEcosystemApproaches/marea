#' Summary method for an ea_data or ea_spatial object
#' @param x an ea_data or ea_spatial object.
#' @param ... further arguments passed to summary method
#' @author Emily O'Grady
#' @export
#' 
ea.summary <- function(x, ...) {
 if (inherits(x, 'ea_data')) {
    summary_stats <- summary(object$data$value)

    cat("--- Summary of ea_data ---\n")
    cat("Metadata:\n")
    cat("  Data Type: ", object$meta$data_type, "\n")
    if (!is.na(object$meta$species) && !is.null(object$meta$species) && object$meta$species != "") {
      cat("  Species: ", object$meta$species, "\n")
    }
    cat("  Location:  ", object$meta$location_descriptor, "\n")
    cat("  Region:  ", object$meta$region, "\n")
    cat("  Source:  ", object$meta$source_citation, "\n\n")

    cat("Data Overview:\n")
    cat("  Time range: ", min(object$data$year, na.rm = TRUE), " to ", max(object$data$year, na.rm = TRUE), "\n")
    cat("  Number of observations: ", nrow(object$data), "\n\n")

    cat("Summary of 'value' column (Units: ", object$meta$units, "):\n", sep = "")
    print(summary_stats)

    invisible(list(meta = object$meta, value_summary = summary_stats))
 } else if (inherits(x, 'ea_spatial')) {
  
     cat("--- Summary of ea_spatial Object ---\n")
     cat("Metadata:\n")
     cat("  Data Type: ", object$meta$data_type, "\n")
     cat("  Region:    ", object$meta$region, "\n")
     cat("  Time:      ", object$meta$time_descriptor, "\n")
     cat("  Source:    ", object$meta$source_citation, "\n\n")

     cat("Spatial Information (from sf):\n")
     # Print the bounding box and CRS from the underlying sf object
     print(sf::st_geometry(object$data))
     cat("\n")

     # If there's a time_descriptor column, show unique values
     if ("time_descriptor" %in% names(object$data)) {
       unique_times <- unique(object$data$time_descriptor)
       cat("Time periods: ", paste(unique_times, collapse = ", "), "\n")
       cat("Number of time periods: ", length(unique_times), "\n\n")
     }

     cat("Summary of 'value' column (Units: ", object$meta$units, "):\n", sep = "")
     print(summary(object$data$value))

     invisible(list(meta = object$meta, value_summary = summary(object$data$value)))
   
 } else{
   stop('Unknown class for summary: ', class(x), '. Please provide an ea_data or ea_spatial object.')
 }
}