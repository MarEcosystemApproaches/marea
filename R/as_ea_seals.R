#' @title Coerce a pacea_harbour_seals object to marea's ea_data
#' @description
#' Adapts a `pacea_harbour_seals` object (from pacea) to the `ea_data` class.
#' Handles standard columns (date, region, mean, low, high) and pulls all metadata from attributes.
#' @param pacea_obj A data.frame/tibble from pacea with class 'pacea_harbour_seals'
#' @return An 'ea_data' object.
#' @export
as_ea_harbour_seals <- function(pacea_obj) {
  if (inherits(pacea_obj, "ea_data")) return(pacea_obj)
  stopifnot(inherits(pacea_obj, "pacea_harbour_seals"))
  df <- as.data.frame(pacea_obj)
  req <- c("date","region","mean","low","high")
  stopifnot(all(req %in% names(df)))
  df$year <- as.integer(format(df$date, "%Y"))
  names(df)[names(df) == "mean"] <- "value"
  attrs <- attributes(pacea_obj)
  data_type   <- attrs$long_name   %||% "Harbour Seal Abundance"
  region      <- attrs$region      %||% "Pacific"
  location    <- "Multiple Regions"
  units       <- "count"
  species     <- attrs$species     %||% "Phoca vitulina richardsi"
  citation    <- attrs$citation    %||% attrs$source %||% "pacea object"
  marea::ea_data(
    data               = df,
    value_col          = "value",
    data_type          = data_type,
    region             = region,
    location_descriptor= location,
    units              = units,
    species            = species,
    source_citation    = citation
  )
}