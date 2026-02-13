# Filter an `ea_spatial` Object by Attribute

A helper function to filter the features or cells of an `ea_spatial`
object based on the values in a specified attribute column or layer.

## Usage

``` r
ea.subset.spatial(x, column, value)
```

## Arguments

- x:

  An `ea_spatial` object.

- column:

  `[character(1)]` The name of the attribute column or layer to filter
  by.

- value:

  A value or vector of values to match in the specified column/layer.

## Value

A new `ea_spatial` object containing only the matching features or
cells.

## Details

This masks the `data` slot of the `ea_spatial` object, setting the
attributes to `NA` for features or cells that do not match the specified
value(s). This is useful for subsetting spatial data based on specific
criteria, such as filtering by region or time. It is different than the
ea.subset method for ea_data which removes features that do not match
the criteria.

## Examples

``` r
if (requireNamespace("sf", quietly = TRUE) && requireNamespace("dplyr", quietly = TRUE)) {
  pts <- list(sf::st_point(c(-63, 44)), sf::st_point(c(-66, 45)))
  sf_df <- sf::st_sf(
    temp = c(12, 13),
    region_id = c("A", "B"),
    geometry = sf::st_sfc(pts, crs = 4326)
  )
  obj <- ea_spatial(sf_df, "temp", "temp", "region", "time", "C")

  # Filter for a specific region_id
  ea.subset.spatial(obj, "region_id", "B")
}
#> An object of class "ea_spatial"
#> Slot "meta":
#> $data_type
#> [1] "temp"
#> 
#> $region
#> [1] "region"
#> 
#> $time_descriptor
#> [1] "time"
#> 
#> $units
#> [1] "C"
#> 
#> $source_citation
#> [1] "No citation provided"
#> 
#> $original_value_col
#> [1] "temp"
#> 
#> 
#> Slot "data":
#> Simple feature collection with 2 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -66 ymin: 44 xmax: -63 ymax: 45
#> Geodetic CRS:  WGS 84
#>   value region_id       geometry
#> 1    NA      <NA> POINT (-63 44)
#> 2    13         B POINT (-66 45)
#> 
```
