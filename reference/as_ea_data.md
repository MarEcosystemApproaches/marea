# Coerce a data frame to an `ea_data` S4 object

Converts a data frame or compatible object into an `ea_data` S4 object,
handling multiple potential value columns robustly and validating the
result.

## Usage

``` r
as_ea_data(x, value_col = NULL, ...)
```

## Arguments

- x:

  A data frame or similar object to convert.

- value_col:

  The name of the column to use as the primary "value" column. If not
  specified, the function will attempt to determine the value column
  automatically.

- ...:

  Additional metadata arguments passed to the
  [`ea_data()`](https://marecosystemapproaches.github.io/marea/reference/ea_data.md)
  constructor.

## Value

A validated `ea_data` S4 object.
