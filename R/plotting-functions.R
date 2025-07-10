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
  .plot_pacea_st(x, bc = FALSE, eez = FALSE, ...)
}

# --- TODOs for developers (not part of user documentation) ---
# Utilize months.plot and years.plot
# Add more options for customizing the plot
# maritimes specific defaults needed?

#' Plot a time series for one or all regions
#'
#' Create a time series plot for herring or similar data, either for a single region or all regions.
#' This function is adapted from `plot.pacea_recruitment_herring()`.
#'
#' @param x A `marea_herring` object.
#' @param region The region(s) to plot. If `NULL`, all regions are plotted.
#' @param x_lab Label for the x-axis (default is "Year").
#' @param y_lab Label for the y-axis (default is taken from `attr(obj, "axis_name")`).
#' @param mar_all_regions Margin settings for `par()` when plotting all regions.
#' @param oma_all_regions Outer margin settings for `par()` when plotting all regions.
#' @param y_tick_by Spacing for y-axis ticks (default is 5).
#' @param title If `NULL`, no figure title is shown. If `"full"` (default), the full region name is used. If `"short"`, only the acronym is used.
#' @param ... Additional arguments passed to `plot.pacea_biomass()` and `plot.default()`.
#'
#' @return A time series plot is displayed. Nothing is returned.
#' @export
#' @author Jaimie Harbin and Benoit Casault
#' @examples
#' \dontrun{
#' data(azmp_bottom_temperature)
#' # plot(azmp_bottom_temperature)
#' }
plot.marea_trend <- function(x,
                             region = NULL,
                             x_lab = "Year",
                             y_lab = attr(x, "axis_name"),
                             mar_all_regions = c(3, 3, 2, 0),
                             oma_all_regions = c(2, 1, 1, 1),
                             y_tick_by = 5,
                             title = "full",
                             ...){
  
  # Check that the required S3 method exists
  if (!exists("plot.pacea_biomass", where = asNamespace("pacea"), mode = "function")) {
    stop("Required S3 method 'plot.pacea_biomass' not found in pacea package")
  }
  
  # Store original attributes
  orig_attrs <- attributes(x)
  
  if (!(is.null(region))) {
    if (!(all(region %in% x$region))) {
      stop("region must be in ", paste0(unique(x$region), collapse=","))
    }
  }
  
  regions_all <- unique(x$region)
  
  stopifnot("title must be one of full, short, or NULL" =
              title %in% c("full", "short", "NULL"))
  
  regions_full <- unique(x$region)
  if(is.null(title)){
    title_text_vec = ""
  } else {
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
    
    x_region <- filter_preserve_attrs(x, region == region_choice)
    if (nrow(x_region) == 0) {
      stop("No data available for the specified region: ", region_choice)
    }
    # Restore important attributes
    attr(x_region, "axis_name") <- orig_attrs$axis_name
    
    # Set the class to ensure proper dispatch
    class(x_region) <- c("pacea_biomass", "data.frame")
    
    
    plot(x_region,
         y_tick_by = y_tick_by,
         main = title_text,
         ...)
  } else {
    # Plot all regions
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    
    par(mfrow = c(ceiling(length(unique(x$region))/4), 4),
        mar = mar_all_regions,
        oma = oma_all_regions)
    
    if (!(is.null(region))) {
      regions_all <- region
    } else {
      regions_all <- unique(x$region)
    }
    
    for(i in 1:length(regions_all)){
      x_filtered <- filter_preserve_attrs(x, region == regions_all[i])
      
      
      # Set the class to ensure proper dispatch
      class(x_filtered) <- c("pacea_biomass", "data.frame")
      cat("DEBUG: axis_name after filter:", attr(x_filtered, "axis_name"), "\n")
      
      plot(x_filtered,
           main = regions_all[i],
           xlab = "",
           ylab = "",
           y_tick_by = y_tick_by,
           ...)
    }
  }
  
  invisible()
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
  # Check that the required S3 method exists
  if (!exists("plot.pacea_index", where = asNamespace("pacea"), mode = "function")) {
    stop("Required S3 method 'plot.pacea_index' not found in pacea package")
  }
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
  # Check that the required S3 method exists
  if (!exists("plot.pacea_index", where = asNamespace("pacea"), mode = "function")) {
    stop("Required S3 method 'plot.pacea_index' not found in pacea package")
  }
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

