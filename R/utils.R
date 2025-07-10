##' Add tick marks to a time series plot
##'
##' Adds small, unlabeled tick marks to both axes of a time series plot, making it easier to read. 
##' This function is mainly used internally for plots created by the marea and pacea packages.
##'
##' @param obj_lub A `pacea_index` object with a `date` column (as a lubridate `date`).
##' @param y_tick_by Spacing between y-axis tick marks.
##' @param y_tick_start Where to start y-axis tick marks.
##' @param y_tick_end Where to end y-axis tick marks.
##' @param y_tick_max_number Maximum number of y-axis tick marks.
##' @param x_tick_extra_years Number of extra years to add tick marks for (does not expand the axis).
##' @param start_decade_ticks Year to start decade tick marks (default is 1800).
##'
##' @return Adds tick marks to the current plot. Nothing is returned.
##' @author Andrew Edwards
##' @export
##' @examples
##' \dontrun{
##' plot.pacea_index(oni)
##' }
add_tickmarks <- function(obj_lub,
                          y_tick_by,
                          y_tick_start,
                          y_tick_end,
                          y_tick_max_number = 50,
                          x_tick_extra_years,
                          start_decade_ticks){
  min <- min(lubridate::floor_date(obj_lub$date, unit = "year")) -
    lubridate::years(x_tick_extra_years)

  max <- max(lubridate::ceiling_date(obj_lub$date, unit = "year")) +
    lubridate::years(x_tick_extra_years)

  if(is.null(y_tick_start)){
    y_tick_start <- floor(par("usr")[3])
  }
  if(is.null(y_tick_end)){
    y_tick_end  <- ceiling(par("usr")[4])
  }

  # Small ticks every year
  axis(1,
       seq(min,
           max,
           by = "years"),
       labels = FALSE,
       tcl = -0.2)

  # Slightly larger ticks every decade (since not all get labelled automatically)
  axis(1,
       seq(start_decade_ticks,
           max,
           by = "10 years"),
       labels = FALSE,
       tcl = -0.3)

  # y-axis; not certain these are guaranteed to include 0, may need to add
  # something; see end of plot.pacea_recruiment() for adding in negative
  # tickmarks when starting at 0. Should really be automated here, but
  # plot.pacea_index() code relies on all this, so would take a bit of checking
  # that nothing got messed up. Though if they look funny people should just
  # change y_tick_by, which has default 0.25 for indices, which likely just works
  # as they are standardised.
  # For plot.pacea_biomass() default y_tick_by is 1 which has worked fine,
  # except for age-1 hake since max is about 1200 (x1000 tons). So tweaking
  # here with a new argument y_tick_max_number.

  while(length(seq(y_tick_start,
                   y_tick_end,
                   by = y_tick_by)) > y_tick_max_number){
    y_tick_by <- y_tick_by*10
  }

  axis(2,
       seq(y_tick_start,
           y_tick_end,
           by = y_tick_by),
       labels = FALSE,
       tcl = -0.2)
}

# --- TODOs for developers (not part of user documentation) ---
# TODO: This function was copied over for initial development; consider removing if not needed.

#' Get citation information for marea
#'
#' Returns the recommended citation for the marea package, either as plain text or in BibTeX format.
#'
#' @param bibtex Logical. If TRUE, returns BibTeX format. If FALSE (default), returns plain text.
#' @return Citation information for the marea package.
#' @export
#'
#' @examples
#' # Get text citation
#' cite_marea()
#'
#' # Get BibTeX citation
#' cite_marea(bibtex = TRUE)
cite_marea <- function(bibtex = FALSE) {
  cit <- citation("marea")
  if (bibtex) {
    return(toBibtex(cit))
  } else {
    return(cit)
  }
}

#' Print a reminder to cite marea
#'
#' Prints a message reminding users to cite the marea package in their work.
#'
#' @export
#'
#' @examples
#' marea_citation_reminder()
marea_citation_reminder <- function() {
  message("Thank you for using marea!")
  message("Please cite this package in your work:")
  message("")
  print(citation("marea"), bibtex = FALSE)
}

#' Download Copernicus Marine (CMEMS) data
#'
#' Downloads data from the Copernicus Marine Environment Monitoring Service ([CMEMS](https://marine.copernicus.eu/)) using the Copernicus Marine API and the Python package `copernicusmarine`.
#'
#' @param username Your CMEMS username.
#' @param password Your CMEMS password.
#' @param dataset_id Dataset ID (default is "cmems_mod_glo_phy_my_0.083deg_P1M-m", the global ocean physics product).
#' @param variables Character vector of variable names to download.
#' @param minimum_longitude Minimum longitude for the data bounding box.
#' @param maximum_longitude Maximum longitude for the data bounding box.
#' @param minimum_latitude Minimum latitude for the data bounding box.
#' @param maximum_latitude Maximum latitude for the data bounding box.
#' @param start_datetime Start date and time (e.g., "1993-12-01T00:00:00").
#' @param end_datetime End date and time (e.g., "1994-12-01T00:00:00").
#' @param output_filename Name of the output file (NetCDF format).
#'
#' @return No return value. Downloads a NetCDF file to the specified location.
#' @importFrom reticulate use_virtualenv virtualenv_install virtualenv_create import
#' @export
#'
#' @examples
#' \dontrun{
#' get_CMEMS_ncdf(username = "your_username", password = "your_password", variables = c("thetao"))
#' }
get_CMEMS_ncdf <- function(username = NA, password = NA, dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m", variables, minimum_longitude = -67.74250, maximum_longitude = -54.90132, minimum_latitude = 40.04343, maximum_latitude = 47.83333, start_datetime = "1993-12-01T00:00:00", end_datetime = "1994-12-01T00:00:00", output_filename = tempfile(fileext = ".nc")) {
  # browser()

  pythonenv <- try(reticulate::use_virtualenv("CopernicusMarine", required = TRUE))

  if (inherits(pythonenv, "try-error")) {
    reticulate::virtualenv_create(envname = "CopernicusMarine")
    reticulate::virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))
  }
  reticulate::use_virtualenv("CopernicusMarine", required = TRUE)
  cmt <- try(import("copernicusmarine"))

  # Login function to create your configuration file
  if(!is.na(username)|!is.na(password)){
    cmt$login(username, password)
  }
  tmpdirnc <- tempdir()
  tmpnc <- tempfile(fileext = ".nc")
  cmt$subset(
    dataset_id=dataset_id,
    variables=variables,
    minimum_longitude=minimum_longitude,
    maximum_longitude=maximum_longitude,
    minimum_latitude=minimum_latitude,
    maximum_latitude=maximum_latitude,
    start_datetime=start_datetime,
    end_datetime=end_datetime,
    output_directory = dirname(output_filename),
    output_filename = basename(output_filename)
  )

}

##' Convert an object that represents a time seris (`pacea_index` etc.) to have lubridate
##'  year/month column, and smooth over a year if required TODO copying over for now.
##'
##' Called from `plot.pacea_index()` and others to give a lubrdiate column for
##' easier plotting.
##'
##' @param obj A `pacea_index` or similar object with a `year` column, and optionally a `month` column.
##' @param smooth_over_year Logical. If TRUE, average monthly values over each year.
##' @return original object with month values smoothed (averaged) over the year if
##'   requested, and a new `date` column in lubridate format. Sets a resulting
##'   date to be the first of the month or Jan 1st of that year (as appropriate),
##'   for easy
##'   plotting, but note that these should not be taken as exact dates, which is
##'   why we saved the original objects just as year and/or month, not lubridate
##'   dates.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' lubridate_pacea_series(oni)
##' lubridate_pacea_series(oni, smooth_over_year=TRUE)
##'
##' }
lubridate_pacea_series <- function(obj,
                                   smooth_over_year = FALSE){
  # Store the axis_name attribute
  axis_name <- attr(obj, "axis_name")

  if(smooth_over_year){
    stopifnot("to smooth over year you need monthly data (if you have daily we can adapt the code
               to use that); set smooth_over_year = FALSE" =
              "month" %in% names(obj))

    obj_lub <- dplyr::group_by(obj,
                               year) %>%
      dplyr::summarise(across(-month,
                              mean))  # Replace val, anom, and any other
                                      # non-year non-month column with their
                                      # annual mean

    obj_lub <- dplyr::mutate(obj_lub,
                             date = lubridate::ymd(year,
                                                   truncated = 2))
                             # sets date to 1st Jan of that year to give a valid
                             #  date; could change to middle of year, but a
                             #  little confusing. year column still retained
                             #  (but object not returned so okay).
  } else {
    if("month" %in% names(obj)){

      # TODO extract date-related columns automatically and create the date column correctly
      #  This works for oni, may need to switch (or function, since may want for
      #  pacea_st also) for years-only. And if make function then use for the
      #  obj_lub line above also.

      obj_lub <- dplyr::mutate(obj,
                               date = paste(year,
                                            month,
                                            sep = "-"))
      obj_lub$date <- lubridate::ym(obj_lub$date)

    } else {
      obj_lub <- dplyr::mutate(obj,
                               date = lubridate::ymd(year,
                                                     truncated = 2))
    }
  }
  # Restore the axis_name attribute before returning
  attr(obj_lub, "axis_name") <- axis_name
  
  
  return(obj_lub)
}

# --- TODOs for developers (not part of user documentation) ---
# TODO: This function was copied over for now. Consider refactoring or removing if not needed.
# TODO: Extract date-related columns automatically and create the date column correctly for all supported objects.

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