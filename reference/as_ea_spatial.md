# Coerce a spatial object to an `ea_spatial` S4 object

Converts a spatial object (`sf`, `stars`, or `SpatRaster`) into a
validated `ea_spatial` S4 object. Metadata is extracted from attributes
and can be overridden via arguments.

## Usage

``` r
as_ea_spatial(x, value_col = NULL, ...)
```

## Arguments

- x:

  A spatial object of class `sf`, `stars`, or `SpatRaster`.

- value_col:

  Name of the column or layer to use as the primary value. If NULL,
  auto-selects if only one candidate exists.

- ...:

  Additional metadata overrides passed to the
  [`ea_spatial()`](https://marecosystemapproaches.github.io/marea/reference/ea_spatial.md)
  constructor.

## Value

A validated `ea_spatial` S4 object.
