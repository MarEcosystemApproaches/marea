# Constructor for `ea_data` Objects

Creates an `ea_data` object from a data frame and associated metadata.
This function serves as the primary constructor for the
[`ea_data-class`](https://marecosystemapproaches.github.io/marea/reference/ea_data-class.md).
It validates the presence and type of required columns, standardizes the
value column name to `value`, and stores metadata in the `meta` slot.

## Usage

``` r
ea_data(
  data,
  value_col,
  data_type,
  region,
  location_descriptor,
  units,
  species = NA_character_,
  source_citation = "No citation provided",
  ...
)

# S4 method for class 'data.frame,character'
ea_data(
  data,
  value_col,
  data_type,
  region,
  location_descriptor,
  units,
  species = NA_character_,
  source_citation = "No citation provided",
  ...
)
```

## Arguments

- data:

  A `data.frame` containing at least a `year` column and a numeric
  column specified by `value_col`.

- value_col:

  A list naming the column(s) in `data` that contains the numeric values
  to be marked as columns with names appended '\_value'

- data_type:

  A character string describing the type of data (e.g., "temperature").

- region:

  A character string indicating the geographic region.

- location_descriptor:

  A character string describing the location (e.g., "bottom",
  "surface").

- units:

  A character string indicating the units of measurement.

- species:

  `[character(1)]` Optional. A character string naming the species
  (default is `NA_character_`).

- source_citation:

  `[character(1)]` Optional. A character string providing the data
  source citation.

- ...:

  Additional metadata fields to be stored in the `meta` slot as a named
  list.

## Value

An object of class `ea_data`.

## Examples

``` r
df <- data.frame(year = 2000:2005, temp_c = rnorm(6))
obj <- ea_data(df,
  value_col = c("temp_c"),
  data_type = "temperature",
  region = "Scotian Shelf",
  location_descriptor = "bottom",
  units = "°C",
  project = "AZMP"
)

# Access metadata
obj[["region"]]
#> [1] "Scotian Shelf"

# Access data
obj[["data"]]
#> # A tibble: 6 × 2
#>    year temp_c_value
#>   <int>        <dbl>
#> 1  2000       1.89  
#> 2  2001      -0.0974
#> 3  2002      -0.936 
#> 4  2003      -0.0160
#> 5  2004      -0.827 
#> 6  2005      -1.51  
```
