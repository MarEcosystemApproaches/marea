# Constructor for `ea_spatial` Objects

Creates an `ea_spatial` object from a spatial data object (`sf`,
`stars`, or `SpatRaster`) and associated metadata.

This function validates the input data, standardizes the primary data
column or layer to be named `value`, and populates the `meta` slot. It
is the primary constructor for the
[`ea_spatial-class`](https://marecosystemapproaches.github.io/marea/reference/ea_spatial-class.md).

## Usage

``` r
ea_spatial(
  data,
  value_col,
  data_type,
  region,
  time_descriptor,
  units,
  source_citation = "No citation provided",
  ...
)

# S4 method for class 'ANY,character'
ea_spatial(
  data,
  value_col,
  data_type,
  region,
  time_descriptor,
  units,
  source_citation = "No citation provided",
  ...
)
```

## Arguments

- data:

  A spatial object of class `sf`, `stars`, or `SpatRaster`.

- value_col:

  `[character(1)]` The name of the column (for `sf`) or layer/attribute
  (for `stars` or `SpatRaster`) that contains the primary numeric
  values. This column/layer will be renamed to `"value"`.

- data_type:

  `[character(1)]` A description of the data (e.g., "sea surface
  temperature", "Chlorophyll-a concentration").

- region:

  `[character(1)]` The geographic region the data represents.

- time_descriptor:

  `[character(1)]` A description of the temporal aspect of the data
  (e.g., "July 2021 climatology", "Annual mean 2020").

- units:

  `[character(1)]` The units of the `value` column/layer.

- source_citation:

  `[character(1)]` Optional. A citation for the data source.

- ...:

  Additional metadata fields to be stored in the `meta` slot.

## Value

An object of class `ea_spatial`.

## Examples

``` r
# Example with an 'sf' object (requires sf package)
if (requireNamespace("sf", quietly = TRUE)) {
  # Create a sample sf object
  pts <- list(
    sf::st_point(c(-63.5, 44.6)),
    sf::st_point(c(-66.1, 44.9)),
    sf::st_point(c(-64.0, 45.1))
  )
  sf_df <- sf::st_sf(
    year = c(2001, 2001, 2002),
    temp = c(12.1, 12.3, 13.0),
    geometry = sf::st_sfc(pts, crs = 4326)
  )

  obj_sf <- ea_spatial(
    data = sf_df,
    value_col = "temp",
    data_type = "Temperature",
    region = "Scotian Shelf",
    time_descriptor = "Point samples",
    units = "degC"
  )
  print(obj_sf)
}
#> An object of class "ea_spatial"
#> Slot "meta":
#> $data_type
#> [1] "Temperature"
#> 
#> $region
#> [1] "Scotian Shelf"
#> 
#> $time_descriptor
#> [1] "Point samples"
#> 
#> $units
#> [1] "degC"
#> 
#> $source_citation
#> [1] "No citation provided"
#> 
#> $original_value_col
#> [1] "temp"
#> 
#> 
#> Slot "data":
#> Simple feature collection with 3 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -66.1 ymin: 44.6 xmax: -63.5 ymax: 45.1
#> Geodetic CRS:  WGS 84
#>   year value           geometry
#> 1 2001  12.1 POINT (-63.5 44.6)
#> 2 2001  12.3 POINT (-66.1 44.9)
#> 3 2002  13.0   POINT (-64 45.1)
#> 
```
