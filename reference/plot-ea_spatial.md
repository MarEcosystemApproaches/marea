# Plot an ea_spatial object with multiple styles

Creates a spatial plot for an `ea_spatial` object. Styles:

- "fill": geom_sf fill by value

- "contour": contour lines of value

- "bubble": bubble plot (point size ∝ value)

- "anomaly": anomaly plot with specialized color schemes and optional
  climatology contours

## Usage

``` r
# S4 method for class 'ea_spatial,missing'
plot(
  x,
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
  ...
)
```

## Arguments

- x:

  An `ea_spatial` object.

- style:

  Character; one of "fill", "contour", "bubble", or "anomaly".

- months.plot:

  For "anomaly" style: months to plot. Defaults to current month (if
  available)

- years.plot:

  For "anomaly" style: years to plot. Defaults to most recent year
  available

- clim.dat:

  For "anomaly" style: climatology data for contour overlay

- coastline:

  logical. Should coastline layer be plotted? (default: TRUE)

- coastline_region:

  Character or numeric vector. Region for coastline:

  - "nw_atlantic" (default): Northwest Atlantic

  - "pacific": Pacific region

  - "gsl": Gulf of St. Lawrence

  - "arctic": Arctic region

  - c(xmin, xmax, ymin, ymax): Custom bounding box

- coastline_color:

  Color for coastline (default: "darkgrey")

- coastline_fill:

  Fill color for land (default: "grey90")

- coastline_size:

  Line width for coastline (default: 0.3)

- coastline_resolution:

  Resolution for coastline data: "low", "medium", "high" (default:
  "medium")

- eez:

  logical. Should EEZ layer be plotted? (default: TRUE)

- eez_data:

  sf object with EEZ data (optional)

- resolution:

  For "anomaly" style: resolution for rasterization when creating
  contours

- ...:

  Additional args passed to the geoms.

## Value

A ggplot object.#' @export
