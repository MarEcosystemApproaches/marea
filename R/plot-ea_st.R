#' Plot an ea_st object with multiple styles
#'
#' Creates a spatial plot for an `ea_st` object. Styles:
#'   * "fill": geom_sf fill by value
#'   * "contour": contour lines of value
#'   * "bubble": bubble plot (point size ∝ value)
#'   * "anomaly": anomaly plot with specialized color schemes and optional climatology contours
#'
#' @param x An `ea_st` object.
#' @param style Character; one of "fill", "contour", "bubble", or "anomaly".
#' @param months.plot For "anomaly" style: months to plot. Defaults to current month (if available)
#' @param years.plot For "anomaly" style: years to plot. Defaults to most recent year available
#' @param clim.dat For "anomaly" style: climatology data for contour overlay
#' @param coastline logical. Should coastline layer be plotted? (default: TRUE)
#' @param coastline_region Character or numeric vector. Region for coastline:
#'   - "nw_atlantic" (default): Northwest Atlantic
#'   - "pacific": Pacific region
#'   - "gsl": Gulf of St. Lawrence
#'   - "arctic": Arctic region
#'   - c(xmin, xmax, ymin, ymax): Custom bounding box
#' @param coastline_color Color for coastline (default: "darkgrey")
#' @param coastline_fill Fill color for land (default: "grey90")
#' @param coastline_size Line width for coastline (default: 0.3)
#' @param coastline_resolution Resolution for coastline data: "low", "medium", "high" (default: "medium")
#' @param eez logical. Should EEZ layer be plotted? (default: TRUE)
#' @param eez_data sf object with EEZ data (optional)
#' @param resolution For "anomaly" style: resolution for rasterization when creating contours
#' @param ... Additional args passed to the geoms.
#' @return A ggplot object.
#' @export
plot.ea_st <- function(x,
                       style = c("fill", "contour", "bubble", "anomaly"),
                       months.plot = NULL,
                       years.plot = NULL,
                       clim.dat = NULL,
                       coastline = TRUE,
                       coastline_region = "nw_atlantic",
                       coastline_color = "darkgrey",
                       coastline_fill = "grey90",
                       coastline_size = 0.3,
                       coastline_resolution = "medium",
                       eez = TRUE,
                       eez_data = NULL,
                       resolution = 6000,
                       ...) {
  
  style <- match.arg(style)
  meta <- x$meta
  df <- x$data
  geom_col <- attr(x, "sf_column")
  
  # Get data extent for coastline cropping
  data_bbox <- sf::st_bbox(df)
  
  # Get coastline data if requested
  coastline_data <- NULL
  if (coastline) {
    coastline_data <- get_coastline_data(
      region = coastline_region,
      data_bbox = data_bbox,
      resolution = coastline_resolution
    )
  }
  
  # Base plot
  p <- ggplot2::ggplot(df)
  
  # Apply style-specific plotting
  switch(style,
         fill = {
           p <- p +
             ggplot2::geom_sf(ggplot2::aes(fill = .data$value), color = NA, ...) +
             ggplot2::scale_fill_viridis_c(name = meta$units)
         },
         
         contour = {
           p <- create_contour_plot(df, meta, ...)
         },
         
         bubble = {
           # Bubble size proportional to value
           cent <- suppressWarnings(sf::st_centroid(df))
           coords <- sf::st_coordinates(cent)
           df3 <- cbind(data.frame(coords), value = df$value)
           p <- p +
             ggplot2::geom_point(data = df3,
                                 ggplot2::aes(x = X, y = Y, size = .data$value),
                                 fill = "steelblue",
                                 colour = 'black',
                                 pch = 21,
                                 ...) +
             ggplot2::scale_size_continuous(name = meta$units)
         },
         
         anomaly = {
           p <- create_anomaly_plot(df, meta, months.plot, years.plot, clim.dat, resolution, ...)
           
           # Add coastline and EEZ for anomaly style
           if (!is.null(coastline_data)) {
             p <- p + ggplot2::geom_sf(
               data = coastline_data, 
               fill = coastline_fill, 
               color = coastline_color, 
               linewidth = coastline_size,
               inherit.aes = FALSE
             )
           }
           
           if (eez && !is.null(eez_data)) {
             p <- p + ggplot2::geom_sf(
               data = eez_data, 
               fill = NA, 
               color = "black", 
               linetype = "dotted",
               inherit.aes = FALSE
             )
           }
           
           # Return early for anomaly style to avoid duplicate processing
           return(p + ggplot2::labs(
             title = paste(meta$data_type, "anomaly"),
             subtitle = paste(meta$time_descriptor, "| Source:", meta$source_citation),
             x = NULL, y = NULL
           ))
         }
  )
  
  # Add coastline for non-anomaly styles
  if (!is.null(coastline_data)) {
    p <- p + ggplot2::geom_sf(
      data = coastline_data, 
      fill = coastline_fill, 
      color = coastline_color, 
      linewidth = coastline_size,
      inherit.aes = FALSE
    )
  }
  
  # Add EEZ for non-anomaly styles
  if (eez && !is.null(eez_data)) {
    p <- p + ggplot2::geom_sf(
      data = eez_data, 
      fill = NA, 
      color = "black", 
      linetype = "dotted",
      inherit.aes = FALSE
    )
  }
  
  # Add coordinate system
  if ("sf" %in% class(df)) {
    crs <- sf::st_crs(df)
    if (!is.na(crs)) {
      p <- p + ggplot2::coord_sf(crs = crs)
    } else {
      p <- p + ggplot2::coord_fixed()
    }
  } else {
    p <- p + ggplot2::coord_fixed()
  }
  
  # Final styling and labels
  p + ggplot2::labs(
    title = meta$data_type,
    subtitle = paste(meta$time_descriptor, "| Source:", meta$source_citation),
    x = NULL, y = NULL
  ) + ggplot2::theme_bw()
}

# Helper function to get coastline data based on region
get_coastline_data <- function(region, data_bbox, resolution = "medium") {
  
  # Define region bounding boxes
  region_boxes <- list(
    nw_atlantic = c(xmin = -68, xmax = -54, ymin = 40, ymax = 48),
    pacific = c(xmin = -180, xmax = -100, ymin = 30, ymax = 65),
    gsl = c(xmin = -72, xmax = -55, ymin = 45, ymax = 52),
    arctic = c(xmin = -180, xmax = -40, ymin = 65, ymax = 85)
  )
  
  # Determine bounding box
  if (is.character(region) && region %in% names(region_boxes)) {
    bbox <- region_boxes[[region]]
  } else if (is.numeric(region) && length(region) == 4) {
    bbox <- region
    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")
  } else {
    # Default to data extent with buffer
    bbox <- as.numeric(data_bbox)
    bbox[1] <- bbox[1] - 5  # xmin
    bbox[2] <- bbox[2] + 5  # ymin  
    bbox[3] <- bbox[3] + 5  # xmax
    bbox[4] <- bbox[4] + 5  # ymax
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
    bbox <- bbox[c("xmin", "xmax", "ymin", "ymax")]  # Reorder
  }
  # Ensure bbox values are valid
  bbox[1] <- max(bbox[1], -180)  # xmin
  bbox[2] <- min(bbox[2], 180)   # xmax
  bbox[3] <- max(bbox[3], -90)   # ymin
  bbox[4] <- min(bbox[4], 90)    # ymax
  
  
  # Try different data sources
  coastline_data <- NULL
  
  # Option 1: Try rnaturalearth with better error handling
  if (requireNamespace("rnaturalearth", quietly = TRUE)) {
    scale_map <- switch(resolution,
                        "low" = 110,
                        "medium" = 50,
                        "high" = 10,
                        50)
    
    tryCatch({
      # Try to get countries data first
      world <- rnaturalearth::ne_countries(scale = scale_map, returnclass = "sf")
      
      # Check if data is valid
      if (!is.null(world) && nrow(world) > 0) {
        # Create bbox polygon correctly using st_polygon
        bbox_coords <- matrix(c(
          bbox[1], bbox[3],  # xmin, ymin
          bbox[2], bbox[3],  # xmax, ymin
          bbox[2], bbox[4],  # xmax, ymax
          bbox[1], bbox[4],  # xmin, ymax
          bbox[1], bbox[3]   # close polygon
        ), ncol = 2, byrow = TRUE)
        
        bbox_poly <- sf::st_sfc(sf::st_polygon(list(bbox_coords)), crs = 4326)
        
        # Crop to region
        coastline_data <- sf::st_crop(world, bbox_poly)
        
        # Check if result is valid
        if (is.null(coastline_data) || nrow(coastline_data) == 0) {
          coastline_data <- NULL
        }
      }
    }, error = function(e) {
      message("rnaturalearth failed: ", e$message)
      coastline_data <- NULL
    })
  }
  
  # Option 2: Try maps package
  if (is.null(coastline_data) && requireNamespace("maps", quietly = TRUE)) {
    tryCatch({
      world_map <- maps::map("world", 
                             xlim = bbox[c(1,2)], 
                             ylim = bbox[c(3,4)],
                             plot = FALSE, fill = TRUE)
      
      coastline_data <- sf::st_as_sf(world_map)
    }, error = function(e) {
      warning("Could not retrieve coastline data from maps package: ", e$message, call. = FALSE)
    })
  }
  
  # Option 3: Create basic outline based on region
  if (is.null(coastline_data)) {
    warning("No coastline packages available. Install 'rnaturalearth' or 'maps' for coastline data.", 
            call. = FALSE)
    
    # Create basic regional outlines
    coastline_data <- create_basic_coastline(region, bbox)
  }
  
  return(coastline_data)
}

# Helper function to create basic coastline outlines
create_basic_coastline <- function(region, bbox) {
  if (is.character(region)) {
    coords <- switch(region,
                     nw_atlantic = data.frame(
                       x = c(-80, -60, -40, -40, -60, -80, -80),
                       y = c(30, 30, 40, 70, 70, 50, 30)
                     ),
                     pacific = data.frame(
                       x = c(-180, -130, -100, -100, -130, -180, -180),
                       y = c(30, 30, 35, 65, 65, 50, 30)
                     ),
                     gsl = data.frame(
                       x = c(-72, -55, -55, -72, -72),
                       y = c(45, 45, 52, 52, 45)
                     ),
                     arctic = data.frame(
                       x = c(-180, -40, -40, -180, -180),
                       y = c(65, 65, 85, 85, 65)
                     ),
                     # Default rectangle
                     data.frame(
                       x = c(bbox[1], bbox[2], bbox[2], bbox[1], bbox[1]),
                       y = c(bbox[3], bbox[3], bbox[4], bbox[4], bbox[3])
                     )
    )
  } else {
    # Use provided bbox
    coords <- data.frame(
      x = c(bbox[1], bbox[2], bbox[2], bbox[1], bbox[1]),
      y = c(bbox[3], bbox[3], bbox[4], bbox[4], bbox[3])
    )
  }
  
  sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(as.matrix(coords))), crs = 4326)
  )
}

# Helper function for contour plots
create_contour_plot <- function(df, meta, ...) {
  # Validate contour requirements
  if (!all(c("value") %in% names(df))) {
    stop("Contour style requires 'value' column in data.", call. = FALSE)
  }
  
  contour_args <- list(...)
  
  # Extract contour-specific arguments
  breaks <- contour_args$breaks %||% NULL
  bins <- contour_args$bins %||% 10
  binwidth <- contour_args$binwidth %||% NULL
  contour_color <- contour_args$contour_color %||% "black"
  contour_size <- contour_args$contour_size %||% 0.5
  contour_linetype <- contour_args$contour_linetype %||% "solid"
  contour_alpha <- contour_args$contour_alpha %||% 1
  fill_background <- contour_args$fill_background %||% FALSE
  fill_alpha <- contour_args$fill_alpha %||% 0.7
  fill_colors <- contour_args$fill_colors %||% NULL
  show_points <- contour_args$show_points %||% FALSE
  point_size <- contour_args$point_size %||% 1
  point_alpha <- contour_args$point_alpha %||% 0.6
  grid_resolution <- contour_args$grid_resolution %||% 100
  interpolation_method <- contour_args$interpolation_method %||% "linear"
  
  # Get coordinates
  coords <- suppressWarnings(sf::st_coordinates(sf::st_centroid(df)))
  df_coords <- data.frame(x = coords[, 1], y = coords[, 2], value = df$value)
  df_coords <- df_coords[complete.cases(df_coords), ]
  
  # Handle duplicates
  if (anyDuplicated(df_coords[, c("x", "y")])) {
    df_coords <- df_coords %>%
      dplyr::group_by(.data$x, .data$y) %>%
      dplyr::summarise(value = mean(.data$value, na.rm = TRUE), .groups = "drop")
  }
  
  # Interpolation if needed
  if (nrow(df_coords) > 1000 || length(unique(df_coords$x)) * length(unique(df_coords$y)) != nrow(df_coords)) {
    x_seq <- seq(min(df_coords$x), max(df_coords$x), length.out = grid_resolution)
    y_seq <- seq(min(df_coords$y), max(df_coords$y), length.out = grid_resolution)
    
    if (requireNamespace("akima", quietly = TRUE)) {
      interp_result <- akima::interp(
        x = df_coords$x, y = df_coords$y, z = df_coords$value,
        xo = x_seq, yo = y_seq, linear = (interpolation_method == "linear")
      )
      df_grid <- expand.grid(x = interp_result$x, y = interp_result$y)
      df_grid$value <- as.vector(interp_result$z)
      df_grid <- df_grid[complete.cases(df_grid), ]
    } else {
      warning("Package 'akima' not available for interpolation. Using original data points.", call. = FALSE)
      df_grid <- df_coords
    }
  } else {
    df_grid <- df_coords
  }
  
  # Create plot
  p <- ggplot2::ggplot(df_grid, ggplot2::aes(x = .data$x, y = .data$y, z = .data$value))
  
  # Add filled contours if requested
  if (fill_background) {
    fill_args <- list(alpha = fill_alpha)
    if (!is.null(breaks)) fill_args$breaks <- breaks
    if (!is.null(bins)) fill_args$bins <- bins
    if (!is.null(binwidth)) fill_args$binwidth <- binwidth
    
    if (!is.null(fill_colors)) {
      p <- p + 
        do.call(ggplot2::geom_contour_filled, fill_args) +
        ggplot2::scale_fill_manual(values = fill_colors, name = meta$units)
    } else {
      p <- p + 
        do.call(ggplot2::geom_contour_filled, fill_args) +
        ggplot2::scale_fill_viridis_d(name = meta$units)
    }
  }
  
  # Add contour lines
  contour_line_args <- list(
    color = contour_color, linewidth = contour_size,
    linetype = contour_linetype, alpha = contour_alpha
  )
  if (!is.null(breaks)) contour_line_args$breaks <- breaks
  if (!is.null(bins)) contour_line_args$bins <- bins
  if (!is.null(binwidth)) contour_line_args$binwidth <- binwidth
  
  p <- p + do.call(ggplot2::geom_contour, contour_line_args)
  
  # Add points if requested
  if (show_points) {
    p <- p + ggplot2::geom_point(
      data = df_coords,
      ggplot2::aes(x = .data$x, y = .data$y, color = .data$value),
      size = point_size, alpha = point_alpha, inherit.aes = FALSE
    ) + ggplot2::scale_color_viridis_c(name = meta$units)
  }
  
  return(p)
}

# Helper function for anomaly plots
create_anomaly_plot <- function(df, meta, months.plot, years.plot, clim.dat, resolution, ...) {
  # Validate requirements
  if (!requireNamespace("pals", quietly = TRUE)) {
    stop("Package 'pals' is required for anomaly style. Please install it.", call. = FALSE)
  }
  
  # Check temporal structure
  if (!all(c("year", "month") %in% names(df))) {
    if ("date" %in% names(df)) {
      df$year <- lubridate::year(df$date)
      df$month <- lubridate::month(df$date)
    } else {
      stop("Anomaly style requires 'year' and 'month' columns or a 'date' column.", call. = FALSE)
    }
  }
  
  # Month lookup table
  month_table <- data.frame(
    month.name = month.name, month.abb = month.abb, month.num = 1:12
  )
  
  # Set defaults
  available_years <- unique(df$year)
  available_months <- unique(df$month)
  
  if (is.null(years.plot)) {
    years.plot <- max(available_years, na.rm = TRUE)
  }
  if (is.null(months.plot)) {
    months.plot <- lubridate::month(Sys.Date())
    if (!(months.plot %in% available_months)) {
      months.plot <- max(available_months, na.rm = TRUE)
    }
  }
  
  # Validate selections
  if (!all(years.plot %in% available_years)) {
    stop("Invalid 'years.plot' specified", call. = FALSE)
  }
  if (!all(months.plot %in% available_months)) {
    stop("Invalid 'months.plot' specified", call. = FALSE)
  }
  
  # Filter data
  df_filtered <- df %>%
    dplyr::filter(.data$year %in% years.plot, .data$month %in% months.plot) %>%
    dplyr::left_join(month_table, by = c("month" = "month.num")) %>%
    dplyr::mutate(plot.date = paste(.data$year, .data$month.name, sep = " ")) %>%
    dplyr::arrange(.data$year, .data$month)
  
  # Create factors
  df_filtered$month.f <- factor(df_filtered$month.name, levels = unique(df_filtered$month.name))
  df_filtered$plot.date.f <- factor(df_filtered$plot.date, levels = unique(df_filtered$plot.date))
  
  # Get aesthetics
  plot_aesthetics <- get_anomaly_aesthetics(df_filtered$value, meta$data_type, meta$units)
  
  # Create plot
  p <- ggplot2::ggplot(df_filtered) + 
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$value), color = NA, ...) +
    ggplot2::scale_fill_gradientn(
      colours = plot_aesthetics$colors,
      limits = plot_aesthetics$limits,
      breaks = seq(plot_aesthetics$limits[1], plot_aesthetics$limits[2], plot_aesthetics$breaks),
      name = plot_aesthetics$fill_label
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        barheight = 12, ticks.colour = "grey30", ticks.linewidth = 0.5,
        frame.colour = "black", frame.linewidth = 0.5
      )
    )
  
  # Add climatology contours
  if (!is.null(clim.dat)) {
    contour.dat <- process_climatology_contours(df_filtered, clim.dat, months.plot, resolution)
    p <- add_climatology_contours(p, contour.dat, plot_aesthetics$contour_type)
  }
  
  # Add faceting
  if (length(months.plot) > 1 && length(years.plot) > 1) {
    p <- p + ggplot2::facet_grid(year ~ month.f)
  } else if (length(unique(df_filtered$plot.date.f)) > 1) {
    p <- p + ggplot2::facet_wrap(~ plot.date.f)
  }
  
  return(p)
}

# Helper function to determine plot aesthetics (from pacea)
get_anomaly_aesthetics <- function(values, data_type = NULL, units = NULL) {
  # GMT jet color palette
  gmt_jet <- c("#000080", "#0000bf", "#0000FF", "#007fff", "#00FFFF", "#7fffff",
               "#FFFFFF",
               "#FFFF7F", "#FFFF00", "#ff7f00", "#FF0000", "#bf0000", "#820000")
  
  # Default values
  colors <- gmt_jet
  limits <- c(-ceiling(max(abs(values), na.rm = TRUE)), ceiling(max(abs(values), na.rm = TRUE)))
  breaks <- 1
  fill_label <- paste0("Anomaly (", units %||% "units", ")")
  contour_type <- "negative"
  
  # Customize based on data type
  if (!is.null(data_type)) {
    data_type_lower <- tolower(data_type)
    
    if (grepl("temperature", data_type_lower)) {
      colors <- gmt_jet
      limits <- c(-3, 3)
      breaks <- 1
      fill_label <- "Temperature\nanomaly (\u00B0C)"
      contour_type <- "positive"
    } else if (grepl("salinity", data_type_lower)) {
      colors <- pals::brewer.prgn(50)
      fill_label <- "Salinity\nanomaly (ppt)"
    } else if (grepl("oxygen", data_type_lower)) {
      colors <- rev(pals::ocean.curl(50))
      breaks <- 5
      fill_label <- "Dissolved oxygen content\nanomaly (mmol-oxygen m^-3)"
    } else if (grepl("ph", data_type_lower)) {
      colors <- pals::brewer.rdgy(50)
      limits <- c(-0.2, 0.2)
      breaks <- 0.05
      fill_label <- "pH anomaly"
    } else if (grepl("phytoplankton", data_type_lower)) {
      colors <- rev(pals::brewer.brbg(50))
      limits <- c(-30, 30)
      breaks <- 10
      fill_label <- "Phytoplankton anomaly\n(mmol-nitrogen m^-2)"
    } else if (grepl("production", data_type_lower)) {
      colors <- pals::brewer.piyg(50)
      limits <- c(-1, 1)
      breaks <- 0.5
      fill_label <- "Total primary production\nanomaly (gC m^-2 d^-1)"
    }
  }
  
  return(list(
    colors = colors,
    limits = limits,
    breaks = breaks,
    fill_label = fill_label,
    contour_type = contour_type
  ))
}

# Helper function to process climatology contours (from pacea)
process_climatology_contours <- function(df_filtered, clim.dat, months.plot, resolution) {
  # Check required packages
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required for climatology contours. Please install it.", call. = FALSE)
  }
  
  # Filter climatology data for relevant months
  tclim <- clim.dat %>%
    dplyr::filter(.data$month %in% months.plot)
  
  # Add coordinates
  tclim <- tclim %>%
    dplyr::mutate(
      lon = sf::st_coordinates(sf::st_centroid(.))[,1],
      lat = sf::st_coordinates(sf::st_centroid(.))[,2]
    ) %>%
    sf::st_drop_geometry()
  
  # Merge with main data
  tclim.x <- df_filtered %>%
    dplyr::mutate(
      lon = sf::st_coordinates(sf::st_centroid(.))[,1],
      lat = sf::st_coordinates(sf::st_centroid(.))[,2]
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(tclim, by = c("month" = "month", "lon" = "lon", "lat" = "lat")) %>%
    dplyr::mutate(
      sd_1.3_pos = .data$clim_sd * 1.282,
      sd_2.3_pos = .data$clim_sd * 2.326,
      sd_above1.3 = .data$value - .data$sd_1.3_pos,
      sd_above2.3 = .data$value - .data$sd_2.3_pos,
      sd_below1.3 = .data$value + .data$sd_1.3_pos,
      sd_below2.3 = .data$value + .data$sd_2.3_pos
    )
  
  # Create raster grid for contours
  tgrid <- terra::rast(df_filtered, resolution = resolution)
  
  # Generate contour data for each time period
  contour.dat <- data.frame()
  for (plot_date in unique(tclim.x$plot.date.f)) {
    tdat <- tclim.x %>% dplyr::filter(.data$plot.date.f == plot_date)
    
    # Create contours for each percentile
    for (var in c("sd_above1.3", "sd_above2.3", "sd_below1.3", "sd_below2.3")) {
      tclim.x1 <- tdat[, c("lon", "lat", var)]
      names(tclim.x1)[3] <- "z_var"
      
      trast <- terra::rasterize(terra::vect(tclim.x1), tgrid, field = "z_var", fun = mean, na.rm = TRUE)
      tcontour <- data.frame(
        terra::crds(trast, na.rm = FALSE),
        z = as.vector(trast),
        sd_var = var,
        plot.date.f = plot_date
      )
      
      contour.dat <- rbind(contour.dat, tcontour)
    }
  }
  
  return(contour.dat)
}

# Helper function to add climatology contours (from pacea)
add_climatology_contours <- function(p, contour.dat, contour_type) {
  if (contour_type == "positive") {
    # For temperature anomalies, show positive contours
    tcon1 <- contour.dat %>% dplyr::filter(.data$sd_var == "sd_above1.3")
    tcon2 <- contour.dat %>% dplyr::filter(.data$sd_var == "sd_above2.3")
    
    p <- p +
      ggplot2::geom_contour(data = tcon1, ggplot2::aes(x = .data$x, y = .data$y, z = .data$z, colour = "sd_above1.3"), 
                            linewidth = 0.5, breaks = 0) +
      ggplot2::geom_contour(data = tcon2, ggplot2::aes(x = .data$x, y = .data$y, z = .data$z, colour = "sd_above2.3"), 
                            linewidth = 0.5, breaks = 0) +
      ggplot2::scale_colour_manual(
        name = NULL, guide = "legend",
        values = c("sd_above1.3" = "grey60", "sd_above2.3" = "black"),
        labels = c("+90th %-ile", "+99th %-ile")
      )
  } else {
    # For other variables, show negative contours
    tcon1 <- contour.dat %>% dplyr::filter(.data$sd_var == "sd_below1.3")
    tcon2 <- contour.dat %>% dplyr::filter(.data$sd_var == "sd_below2.3")
    
    p <- p +
      ggplot2::geom_contour(data = tcon1, ggplot2::aes(x = .data$x, y = .data$y, z = .data$z, colour = "sd_below1.3"), 
                            linewidth = 0.5, breaks = 0) +
      ggplot2::geom_contour(data = tcon2, ggplot2::aes(x = .data$x, y = .data$y, z = .data$z, colour = "sd_below2.3"), 
                            linewidth = 0.5, breaks = 0) +
      ggplot2::scale_colour_manual(
        name = NULL, guide = "legend",
        values = c("sd_below1.3" = "grey60", "sd_below2.3" = "black"),
        labels = c("-90th %-ile", "-99th %-ile")
      )
  }
  
  return(p)
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x