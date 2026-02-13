# Extract or Access Parts of an ea_spatial Object

Provides access to metadata and data using the `[[` operator. This
allows for intuitive extraction of metadata fields, the full data
object, or specific columns/layers within the data object.

Subsets the `data` slot of an `ea_spatial` object using the `[`
operator, while preserving the object's structure and metadata.

## Usage

``` r
# S4 method for class 'ea_spatial'
x[[i, j, ...]]

# S4 method for class 'ea_spatial,ANY,ANY,ANY'
x[i, j, ..., drop = FALSE]
```

## Arguments

- x:

  An `ea_spatial` object.

- i:

  Row/feature indices (numeric, logical), or a spatial object (`sf`,
  `sfc`) to use for spatial subsetting.

- j:

  Ignored. A warning is issued if `j` is specified for `sf`/`stars`.

- ...:

  Additional arguments passed to the underlying subset method.

- drop:

  Logical. Passed to the underlying subset method.

## Value

The requested element. For `SpatRaster` layers, this returns the raster
values as a vector via
[`terra::values()`](https://rspatial.github.io/terra/reference/values.html).

A new `ea_spatial` object with a subsetted `data` slot.

## Details

This method is primarily intended for spatial or feature-based
subsetting (i.e., by row index or by another spatial object). Column
subsetting (via `j`) is not supported for `sf` or `stars` objects to
prevent accidental removal of the required `value` or geometry columns.

## Examples

``` r
if (requireNamespace("sf", quietly = TRUE)) {
  # Create a sample sf object
  pts <- list(sf::st_point(c(-63, 44)), sf::st_point(c(-66, 45)))
  sf_df <- sf::st_sf(temp = c(12, 13), geometry = sf::st_sfc(pts, crs = 4326))
  obj <- ea_spatial(sf_df, "temp", "temp", "region", "time", "C")

  # Get a metadata field
  obj[["units"]]

  # Get the 'value' vector from the data
  obj[["value"]]
}
#> [1] 12 13
if (requireNamespace("sf", quietly = TRUE)) {
  pts <- list(sf::st_point(c(-63, 44)), sf::st_point(c(-66, 45)))
  sf_df <- sf::st_sf(temp = c(12, 13), geometry = sf::st_sfc(pts, crs = 4326))
  obj <- ea_spatial(sf_df, "temp", "temp", "region", "time", "C")

  # Subset the first feature
  obj[1, ]
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
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -63 ymin: 44 xmax: -63 ymax: 44
#> Geodetic CRS:  WGS 84
#>   value       geometry
#> 1    12 POINT (-63 44)
#> 
```
