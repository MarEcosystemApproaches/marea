
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
    
    .plot_pacea_biomass(x_region,
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
      .plot_pacea_biomass(dplyr::filter(x, region == regions_all[i]),
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
