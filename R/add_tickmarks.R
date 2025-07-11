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
