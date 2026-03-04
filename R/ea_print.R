#' @title Print an ea_data or ea_spatial Object  
#' @param x an ea_data or ea_spatial object.
#' @param ... further arguments passed to print method
#' @author Emily O'Grady
#' @export
ea.print <- function(x, ...) {
  if (inherits(x, 'ea_data')) {
      cat("--- Ecosystem Approach (EA) Data Object --- \n")
      cat("Class: ", class(x)[1], "\n")
      cat("Data Type: ", x@meta$data_type, "\n")
      if (!is.na(x@meta$species) && !is.null(x@meta$species)) {
        cat("Species:  ", x@meta$species, "\n")
      }
      cat("Location:  ", x@meta$location_Descriptor, " (", x@meta$region, " Region ) \n")
      cat("Time Range:  ", min(x@data$year, na.rm = TRUE), " - ", max(x@data$year, na.rm = TRUE), "\n")
      cat("Units: ", x@meta$units, "\n")
      cat("--------------------------------------------\n")
      cat("Data Preview:\n")
      print.data.frame(head(x@data), ...)
      invisible(x)
  } else if (inherits(x, 'ea_spatial')) {
      cat("--- Ecosystem Approach Spatio-Temporal (ea_spatial) Object ---\n")
      cat("Data Type:      ", x@meta$data_type, "\n")
      cat("Time:           ", x@meta$time_descriptor, "\n")
      cat("Region:         ", x@meta$region, "\n")
      cat("Units:          ", x@meta$units, " (in 'value' column, originally '", x@meta$original_value_col, "')\n", sep = "")
      cat("-----------------------------------------------------------\n")

      # Print the sf data component
      print(as.data.frame(x@data), ...)

      invisible(x)
  } else {
    stop("Unknown class for print: ", class(x), ". Please provide an ea_data or ea_spatial object.", call. = FALSE)
    }
  }
  