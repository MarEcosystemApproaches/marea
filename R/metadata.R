#' Get metadata for all datasets in marea
#'
#' @return A data frame with dataset information
#' @export
#' @examples
#' marea_metadata()
marea_metadata <- function() {
  datasets <- data(package = "marea")$results[, "Item"]
  
  metadata <- data.frame(
    Dataset = character(),
    `Temporal Coverage` = character(),
    Source = character(),
    `Update Frequency` = character(),
    stringsAsFactors = FALSE
  )
  
  for (dataset_name in datasets) {
    # Get the dataset
    data_obj <- get(dataset_name, envir = asNamespace("marea"))
    
    # Extract metadata from attributes (you need to add these when creating datasets)
    metadata <- rbind(metadata, data.frame(
      Dataset = dataset_name,
      `Temporal Coverage` = attr(data_obj, "temporal_coverage") %||% 
        paste(range(data_obj$year, na.rm = TRUE), collapse = "-"),
      Source = attr(data_obj, "source") %||% "Unknown",
      `Update Frequency` = attr(data_obj, "update_frequency") %||% "Unknown",
      stringsAsFactors = FALSE
    ))
  }
  
  return(metadata)
}

# Helper for NULL values
`%||%` <- function(x, y) if (is.null(x)) y else x