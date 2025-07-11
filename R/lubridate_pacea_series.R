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
# TODO: rename to marea_lubridate_series or similar, since this is not just for oni.