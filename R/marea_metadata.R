#' Get metadata for all datasets in marea
#'
#' Returns a summary table with information about each dataset in the marea package, including temporal coverage, data source, and update frequency.
#'
#' @return A data frame with columns: Dataset, Temporal Coverage, Source, and Update Frequency.
#' @export
#'
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