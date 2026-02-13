# Spatial Data Validity

The validity method for the `ea_spatial` class ensures that:

1.  The `data` slot contains a supported spatial object (`sf`, `stars`,
    `SpatRaster`).

2.  The spatial object contains a `value` column or layer.

## Arguments

- object:

  An `ea_spatial` object.

## Value

`TRUE` if valid, otherwise a character vector of error messages.
