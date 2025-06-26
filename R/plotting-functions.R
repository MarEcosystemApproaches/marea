#' Plot a marea spatiotemporal data layer
#'
#' Quickly visualize spatiotemporal data from the Maritimes region using `ggplot2`.
#' This function is designed for objects of class `marea_st`, such as `glorys_bottom_temperature`.
#' It uses Maritimes-specific defaults and calls `pacea::plot.pacea_st()` internally.
#'
#' You can specify which months and years to display. For more options and customizable plots, see the package vignette.
#'
#' @param x A `marea_st` object (an `sf` object containing spatial and temporal data).
#' @param months.plot Character or numeric vector indicating which months to include (e.g., `c(1, 2)`, `c("April", "May")`, or `c(1, "April")`).
#' @param years.plot Numeric vector of years to include (from 1993 to 2019).
#' @param ... Additional arguments (currently not used, included for compatibility).
#'
#' @return A plot of the spatial data is displayed. Nothing is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' plot(glorys_bottom_temperature)
#' }
plot.marea_st <- function(x,
                          ...){
  pacea:::plot.pacea_st(x,
                        bc = FALSE,
                        eez = FALSE,
                        ...)
}

# --- TODOs for developers (not part of user documentation) ---
# None for this function

#' Plot a time series for one or all regions
#'
#' Create a time series plot for herring or similar data, either for a single region or all regions.
#' This function is adapted from `plot.pacea_recruitment_herring()`.
#'
#' @param obj A `marea_herring` object.
#' @param region The region(s) to plot. If `NULL`, all regions are plotted.
#' @param x_lab Label for the x-axis (default is "Year").
#' @param y_lab Label for the y-axis (default is taken from `attr(obj, "axis_name")`).
#' @param mar_all_regions Margin settings for `par()` when plotting all regions.
#' @param oma_all_regions Outer margin settings for `par()` when plotting all regions.
#' @param title If `NULL`, no figure title is shown. If `"full"` (default), the full region name is used. If `"short"`, only the acronym is used.
#' @param ... Additional arguments passed to `plot.pacea_biomass()` and `plot.default()`.
#'
#' @inherit plot.pacea_index
#' @return A time series plot is displayed. Nothing is returned.
#' @export
#' @author Jaimie Harbin and Benoit Casault
#' @examples
#' \dontrun{
#' # plot(marea_herring_object)
#' }
plot.marea_trend <- function(obj,
                             region = NULL,
                             x_lab = "Year",
                             y_lab = attr(obj, "axis_name"),
                             mar_all_regions = c(3, 3, 2, 0),
                             oma_all_regions = c(2, 1, 1, 1),
                             y_tick_by = 5,
                             title = "full",
                             ...){

  if (!requireNamespace("pacea", quietly = TRUE)) {
    stop("Package 'pacea' needed for this function to work. Please install it from GitHub:\n",
         "remotes::install_github('pbs-assess/pacea')", 
         call. = FALSE)
  }

  if (!(is.null(region))) {
    if (!(all(region %in% azmp_bottom_temperature$region))) {
      stop("region must be in ", paste0(unique(obj$region), collapse=","))
    }
  }

  regions_all <- unique(obj$region)

  stopifnot("title must be one of full, short, or NULL" =
              title %in% c("full", "short", "NULL"))

  regions_full <- unique(obj$region)
  if(is.null(title)){
    title_text_vec = ""} else
    {
      if(title == "full"){
        title_text_vec = regions_full
      }

      if(title == "short"){
        title_text_vec = regions_all
      }
    }

  # Plot one region
  if(!is.null(region) && length(region) == 1){

    title_text <- title_text_vec[which(regions_all == region)]
    region_choice <- region

    obj_region <- dplyr::filter(obj,
                                region == region_choice)

    pacea:::plot.pacea_biomass(obj_region,
                               y_tick_by = y_tick_by,
                               main = title_text,
                               ...)
  } else {

    # Plot all regions
    par_mfrow_orig <- par()$mfrow
    par_mar_orig <- par()$mar
    par_oma_orig <- par()$oma

    par(mfrow = c(ceiling(length(unique(obj$region))/4), 4),
        mar = mar_all_regions,
        oma = oma_all_regions)
    if (!(is.null(region))) {
      regions_all <- region
    } else {
      regions_all <- unique(obj$region)
    }

    for(i in 1:length(regions_all)){
      pacea:::plot.pacea_biomass(dplyr::filter(obj,
                                               region == regions_all[i]),
                                 main = regions_all[i],
                                 xlab = "",
                                 ylab = "",
                                 y_tick_by = y_tick_by,
                                 ...)
    }
    par(mfrow = par_mfrow_orig,
        mar = par_mar_orig,
        oma = par_oma_orig)
  }

  invisible()
}

# --- TODOs for developers (not part of user documentation) ---
# None for this function

#' Plot a climate or oceanographic index
#'
#' Create a time series plot for a climate or oceanographic index (a `pacea_index` object).
#' You can choose different plot styles and highlight years when specific events occurred (for example, rare animal sightings).
#' See the package vignette for more details and examples.
#'
#' @param obj A `pacea_index` object (a time series).
#' @param value The column of `obj` to plot (default is `"anomaly"`).
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis (default is taken from the object's attributes).
#' @param smooth_over_year If `TRUE`, smooth monthly values over each year (see `?oni` for details).
#' @param type Type of plot (see `plot()`).
#' @param style Plot style. Options are:
#'   \describe{
#'     \item{"red_blue_bar"}{Red bars for positive values, blue bars for negative values.}
#'     \item{"red_blue"}{A line with filled colors: red above zero, blue below zero.}
#'     \item{"goa"}{Gulf of Alaska Ecosystem Report style (not implemented).}
#'     \item{"plain"}{A plain line plot.}
#'   }
#' @param y_tick_by Increment for y-axis ticks.
#' @param y_tick_start Where to start y-axis tick marks (optional).
#' @param y_tick_end Where to end y-axis tick marks (optional).
#' @param y_tick_max_number Maximum number of y-axis tick marks.
#' @param x_tick_extra_years Number of extra years to add tick marks for (does not expand the axis).
#' @param start_decade_ticks Year to start decade tick marks (default is 1800).
#' @param event_years Years when an event occurred (e.g., rare animal sightings) to highlight on the plot.
#' @param event_lub Dates of events as `Date` objects (use either `event_years` or `event_lub`).
#' @param event_pch Plotting character for events.
#' @param event_cex Size of event points.
#' @param event_col Color for event points.
#' @param y_axis_reverse If `TRUE`, reverse the y-axis.
#' @param ... Additional arguments passed to `plot()`.
#'
#' @return A plot of the time series is displayed. Nothing is returned.
#' @export
#' @author Andrew Edwards
#' @examples
#' \dontrun{
#' plot(oni)
#' plot(oni, xlim = c(lubridate::dmy(01011950), lubridate::dmy(01012040)))
#' plot(npi_monthly, value = "value")
#'
#' # Example: Highlight years with rare shark sightings
#' # library(gfiphc)
#' # sp_set_counts <- iphc_get_calc_plot_full("bluntnose sixgill shark")
#' # bluntnose_caught_years <- unique(filter(sp_set_counts$set_counts, N_it > 0, standard == "Y")$year)
#' # plot(oni, event_years = bluntnose_caught_years)
#' }
plot.pacea_index <- function(obj,
                             value = "anomaly",
                             xlab = "Date",
                             ylab = attr(obj, "axis_name"),
                             smooth_over_year = FALSE,
                             type = "l",
                             style = "red_blue_bar",
                             y_tick_by = 0.25,
                             y_tick_start = NULL,
                             y_tick_end = NULL,
                             y_tick_max_number = 50,
                             x_tick_extra_years = 200,
                             start_decade_ticks = lubridate::ymd("1800-01-01",
                                                                 truncated = 2),
                             event_years = NULL,
                             event_lub = NULL,
                             event_pch = 20,
                             event_cex = 3,
                             event_col = "grey",
                             y_axis_reverse = FALSE,
                             ...
                             ){
  stopifnot("value must be a column of the pacea_index object" =
              value %in% names(obj))

  stopifnot("Cannot specify both event_years and event_lub" =
              !(!is.null(event_years) & !is.null(event_lub)))

  stopifnot("event_lub needs to be a Date class (created using lubridate); can use event_years instead for annual events" =
              "Date" %in% class(event_lub) | is.null(event_lub))

  if(y_axis_reverse){
    y_tick_by = - y_tick_by
  }

  obj_lub <- lubridate_pacea_series(obj = obj,
                                    smooth_over_year = smooth_over_year)

  if(!is.null(event_years)){
    event_lub <- lubridate::ymd(event_years, truncated = 2) + months(6)
  }

  if(style == "red_blue"){
    plot_red_blue(obj_lub,
                  value = value,
                  xlab = xlab,
                  ylab = ylab,
                  type = type,
                  ...)
  } else if(style == "red_blue_bar") {
    plot_red_blue_bar(obj_lub,
                      value = value,
                      xlab = xlab,
                      ylab = ylab,
                      type = type,
                      ...)
  } else {
    plot.default(obj_lub$date,
                 obj_lub[[value]],
                 xlab = xlab,
                 ylab = ylab,
                 type = type,
                 ...)
  }

  add_tickmarks(obj_lub,
                y_tick_by = y_tick_by,
                y_tick_start = y_tick_start,
                y_tick_end = y_tick_end,
                y_tick_max_number = y_tick_max_number,
                x_tick_extra_years = x_tick_extra_years,
                start_decade_ticks = start_decade_ticks)

  if(!is.null(event_lub)){
    event_lub <- sort(event_lub)
    if(identical(filter(obj_lub, date %in% event_lub)$date, event_lub)){
      ind <- which(obj_lub$date %in% event_lub)
      points(event_lub,
             obj_lub[ind, ][[value]],
             pch = event_pch,
             cex = event_cex,
             col = event_col)
    } else {
      obj_lub_interp_list <- approx(x = obj_lub$date,
                                    y = obj_lub[[value]],
                                    xout = seq(min(obj_lub$date),
                                               max(obj_lub$date),
                                               "days"))
      obj_lub_interp <- tibble::tibble(date = obj_lub_interp_list$x,
                                       y = obj_lub_interp_list$y)
      names(obj_lub_interp)[2] <- value
      ind_interp <- which(obj_lub_interp$date %in% event_lub)
      points(event_lub,
             obj_lub_interp[ind_interp, ][[value]],
             pch = event_pch,
             cex = event_cex,
             col = event_col)
    }
  }
}

# --- TODOs for developers (not part of user documentation) ---
# TODO: Only checked event plotting with $anomaly column in plot.pacea_index
# TODO: Temporary until figure out how to export from pacea, see https://github.com/pbs-assess/pacea/issues/74

#' Plot a red/blue anomaly time series (smoothed)
#'
#' Internal function to plot a time series with red shading above zero and blue below zero, using linear interpolation for smooth transitions.
#'
#' @param obj_lub A `pacea_index` object with a `date` column (as `Date`).
#' @param value The column to plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param type Type of plot.
#' @param ... Additional arguments passed to `plot()`.
#'
#' @return A plot is displayed. Nothing is returned.
#' @author Andrew Edwards
#' @examples
#' \dontrun{
#' # see plot.pacea_index()
#' }
plot_red_blue <- function(obj_lub,
                          value,
                          xlab,
                          ylab,
                          type,
                          ...){
  # TODO: Check if 0 is within range, or test it works for all positive anomalies

  obj_lub_interp_list <- approx(x = obj_lub$date,
                                y = obj_lub[[value]],
                                xout = seq(min(obj_lub$date),
                                           max(obj_lub$date),
                                           "days"))

  obj_lub_interp <- tibble::tibble(date = obj_lub_interp_list$x,
                                   y = obj_lub_interp_list$y)
  names(obj_lub_interp)[2] <- value

  obj_lub_interp$y_pos <- ifelse(obj_lub_interp[[value]] >= 0,
                                 obj_lub_interp[[value]],
                                 0)
  obj_lub_interp$y_neg <- ifelse(obj_lub_interp[[value]] < 0,
                                 obj_lub_interp[[value]],
                                 0)

  plot(obj_lub_interp$date,
       obj_lub_interp[[value]],
       type = type,
       xlab = xlab,
       ylab = ylab,
       ...)
  abline(h = 0)

  polygon(c(obj_lub_interp$date[1],
            obj_lub_interp$date,
            tail(obj_lub_interp$date, 1)),
          c(0,
            obj_lub_interp$y_pos,
            0),
          col = "red")

  polygon(c(obj_lub_interp$date[1],
            obj_lub_interp$date,
            tail(obj_lub_interp$date, 1)),
          c(0,
            obj_lub_interp$y_neg,
            0),
          col = "blue")
  invisible()
}

# --- TODOs for developers (not part of user documentation) ---
# TODO: Check if 0 is within range, or test it works for all positive anomalies in plot_red_blue

#' Plot a red/blue anomaly time series (bar style)
#'
#' Internal function to plot a time series as red and blue bars, without smoothing.
#'
#' @param obj_lub A `pacea_index` object with a `date` column (as `Date`).
#' @param value The column to plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param type Type of plot.
#' @param ... Additional arguments passed to `plot()`.
#'
#' @return A plot is displayed. Nothing is returned.
#' @author Andrew Edwards
#' @examples
#' \dontrun{
#' # see plot.pacea_index()
#' }
plot_red_blue_bar <- function(obj_lub,
                              value,
                              xlab,
                              ylab,
                              type,
                              ...){
  # TODO: Check if 0 is within range

  obj_lub$y_pos <- ifelse(obj_lub[[value]] >= 0,
                          obj_lub[[value]],
                          0)
  obj_lub$y_neg <- ifelse(obj_lub[[value]] < 0,
                          obj_lub[[value]],
                          0)
  bar_col <- ifelse(obj_lub[[value]] >= 0,
                    "red",
                    "blue")

  plot(obj_lub$date,
       obj_lub[[value]],
       type = "h",
       xlab = xlab,
       ylab = ylab,
       col = bar_col,
       lend = 1,
       ...)
  abline(h = 0)

  invisible()
}

# --- TODOs for developers (not part of user documentation) ---
# TODO: Check if 0 is within range in plot_red_blue_bar