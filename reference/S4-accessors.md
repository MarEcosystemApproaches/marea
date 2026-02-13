# Extract or Access Parts of an ea_data Object

Provides access to metadata or data columns using the `[[` operator.
This allows for intuitive extraction of both metadata fields (e.g.,
`obj[["region"]]`) and data vectors (e.g., `obj[["year"]]`).

Subsets the rows of the `data` slot using the `[` operator, while
preserving the object's structure and metadata.

## Usage

``` r
# S4 method for class 'ea_data'
x[[i]]

# S4 method for class 'ea_data,ANY,missing,missing'
x[i, j, ..., drop = FALSE]

# S4 method for class 'ea_data,ANY,ANY,ANY'
x[i, j, ..., drop = FALSE]
```

## Arguments

- x:

  An `ea_data` object.

- i:

  Row indices (numeric, logical, or character) to subset the data.

- j:

  Ignored. A warning is issued if `j` is specified.

- ...:

  Additional arguments (ignored).

- drop:

  Ignored. The method always returns an `ea_data` object.

## Value

The requested element.

A new `ea_data` object with subsetted rows in its `data` slot.

## Details

Column subsetting (via `j`) is not supported and will be ignored with a
warning, as altering the column structure could invalidate the object
(e.g., by removing the `year` or `value` columns).

## Examples

``` r
df <- data.frame(year = 2000:2005, temp_c = rnorm(6))
obj <- ea_data(df,
  value_col = c("temp_c"), data_type = "temperature",
  region = "Scotian Shelf", location_descriptor = "bottom",
  units = "°C"
)

# Get the units
obj[["units"]]
#> [1] "°C"

# Get the year vector
obj[["year"]]
#> [1] 2000 2001 2002 2003 2004 2005

df <- data.frame(year = 2000:2005, temp_c = rnorm(6))
obj <- ea_data(df,
  value_col = "temp_c", data_type = "temperature",
  region = "Scotian Shelf", location_descriptor = "bottom",
  units = "°C"
)

# Subset the first three rows
obj[1:3, ]
#> An object of class "ea_data"
#> Slot "meta":
#> $data_type
#> [1] "temperature"
#> 
#> $region
#> [1] "Scotian Shelf"
#> 
#> $location_descriptor
#> [1] "bottom"
#> 
#> $units
#> [1] "°C"
#> 
#> $species
#> [1] NA
#> 
#> $source_citation
#> [1] "No citation provided"
#> 
#> $original_value_col
#> [1] "temp_c"
#> 
#> 
#> Slot "data":
#> # A tibble: 3 × 2
#>    year temp_c_value
#>   <int>        <dbl>
#> 1  2000       -0.247
#> 2  2001       -0.244
#> 3  2002       -0.283
#> 
```
