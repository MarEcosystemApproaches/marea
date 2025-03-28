##' Plot a time series object for one or all regions
##'
##' Adapted slightly from `plot.pacea_recruitment_herring()`.
##'
##' @param obj a `marea_herring` object
##' @param region where the sample took place
##' @param x_lab label for x-axis (default is "Year", only shown for bottom
##'   panel when plotting all five regions)
##' @param y_lab label for y-axis (default is specified by `attr(obj, "axis_name")`.
##' @param mar_all_regions `mar` value for `par()`
##' @param oma_all_regions similar to `mar_all_regions`
##' @param title if `NULL` then no figure title (using `main()`), if `full` (the
##'   default) then spell out the Stock Assessment Region, and if `short` then
##'   just use the acronym.
##' @param ... further options passed onto `plot.pacea_biomass()` that can
##'   also pass onto `plot.default()`
##' @inherit plot.pacea_index
##' @return time series ggplot
##' @export
##' @author Jaimie Harbin and Benoit Casault
##' @examples
##' \dontrun{ TODO
##'
##' }
plot.marea_trend <- function(obj,
                                       region = NULL,
                                       x_lab = "Year",
                                       y_lab = attr(obj, "axis_name"),
                                       mar_all_regions = c(3, 3, 2, 0),
                                       oma_all_regions = c(2, 1, 1, 1),
                                       y_tick_by = 5,
                                       title = "full",
                                       ...){
  
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
  #browser()
  if(!is.null(region) && length(region) == 1){

    title_text <- title_text_vec[which(regions_all == region)] # works for
    # title_text_vec = "" as returns NA
    
    region_choice <- region     # Else region == region in next line does not work
    
    obj_region <- dplyr::filter(obj,
                                region == region_choice)
    
    pacea:::plot.pacea_biomass(obj_region,
                       y_tick_by = y_tick_by,
                       main = title_text,
                       ...)
  } else {

    # Plot all regions
    par_mfrow_orig <- par()$mfrow  # To reset at end
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
    # Not sure these really help
    par(mfrow = par_mfrow_orig,
        mar = par_mar_orig,
        oma = par_oma_orig)
  }
  
  invisible()
}

