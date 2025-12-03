#' Get metadata for all datasets in marea
#'
#' Returns a summary data frame including region, time span, and source information for each dataset.
#'
#' @return A data.frame with columns: Dataset, Region, TimeSpan, Source
#' @export
#'
marea_metadata <- function() {
  # Helper function for NULL coalesce
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  datasets <- data(package = "marea")$results[, "Item"]
  
  metadata <- data.frame(
    Dataset = character(),
    Region = character(),
    TimeSpan = character(),
    Source = character(),
    stringsAsFactors = FALSE
  )
  
  for (dataset_name in datasets) {
    #load all data sets
    utils::data(list = dataset_name, package = "marea", envir = environment())
    # Get the dataset object from the marea namespace
    data_obj <- get(dataset_name)
    
    # Initialize meta extraction
    meta <- NULL
    
    # Try to extract meta list if ea_data or ea_spatial (standard structure)
    if (inherits(data_obj, "ea_data")) {
      meta <- data_obj@meta
      region    <- meta$region                  %||% "Unknown"
      years     <- data_obj@data$year           %||% NA
      timespan  <- if (!all(is.na(years))) paste0(min(years, na.rm=TRUE), "-", max(years, na.rm=TRUE)) else "Unknown"
      source    <- meta$source_citation         %||% "Unknown"
    } else if (inherits(data_obj, "ea_spatial")) {
      meta <- attr(data_obj, "meta")
      region    <- meta$region                  %||% "Unknown"
      # Try for time information
      years <- NULL
      for (col in c("year", "date", "time", "Year")) {
        if (col %in% names(data_obj)) {
          yrs <- data_obj[[col]]
          if (is.numeric(yrs) || inherits(yrs, "Date")) {
            years <- yrs
            break
          }
        }
      }
      timespan  <- if (!is.null(years) && !all(is.na(years)))
        paste0(min(years, na.rm = TRUE), "-", max(years, na.rm = TRUE)) else "Unknown"
      source    <- meta$source_citation         %||% "Unknown"
    } else {
      # Try to infer for base/data.frame or other class
      region    <- attr(data_obj, "region")     %||% "Unknown"
      years     <- NULL
      for (col in c("year", "date", "time", "Year")) {
        if (col %in% names(data_obj)) {
          yrs <- data_obj[[col]]
          if (is.numeric(yrs) || inherits(yrs, "Date")) {
            years <- yrs
            break
          }
        }
      }
      timespan  <- if (!is.null(years) && !all(is.na(years)))
        paste0(min(years, na.rm = TRUE), "-", max(years, na.rm = TRUE)) else "Unknown"
      source    <- attr(data_obj, "source_citation") %||%
        attr(data_obj, "source")          %||% "Unknown"
    }
    
    metadata <- rbind(
      metadata,
      data.frame(
        Dataset = dataset_name,
        Region = as.character(region),
        TimeSpan = as.character(timespan),
        Source = as.character(source),
        stringsAsFactors = FALSE
      )
    )
  }
  
  return(metadata)
}